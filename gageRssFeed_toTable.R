library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(zoo)
library(tidyr)
library(readr)

# Function to process observedStage and forecastStage
process_stage_data <- function(stage_data) {
  stage_data %>%
    mutate(date = as_date(ymd_hms(validTime))) %>%
    group_by(date) %>%
    summarize(value = mean(primary, na.rm = TRUE)) %>%
    ungroup()
}

apiPrefix = "https://api.water.noaa.gov/nwps/v1/gauges/"

gageAPI_f = function(customerInputTable, writeNewTable_boolean) {
  histAndForecast_ls = list()
  for(thisRow in 1:nrow(customerInputTable)){
    fileLocation = paste0("./Data/", customerInputTable$File_name[thisRow],".csv")
    stageNotFlow = customerInputTable$stageNotFlow[thisRow]
    # read in historical data and check to see if it needs to be refreshed
    historicalStage = data.table::fread(fileLocation, skip = 1, header = TRUE)
    lastSaveDate = as.Date(strsplit(readLines(fileLocation, n = 1), " ")[[1]][2])
    currentDate = Sys.Date()
    
    if(currentDate > lastSaveDate) {
      # process historical data
      if(is.character(historicalStage$date[1])) {
        historicalStage$date = as_date(mdy(sapply(strsplit(historicalStage$date, " "), getElement, 1)))
        historicalStage$value = as.numeric(historicalStage$value)
      } else {
        historicalStage$date = as_date(historicalStage$date)
        historicalStage$value = as.numeric(historicalStage$value)
      }
      
      # Initialize variables to store API data
      forecastStage_daily = NULL
      observedStage_daily = NULL
      
      # Try to get forecast data
      tryCatch({
        forecastStage_daily = paste0(apiPrefix, customerInputTable$NWS_Gage_Name[thisRow], "/stageflow/forecast") %>%
          GET(add_headers(accept = "application/json")) %>%
          content("text", encoding = "UTF-8") %>%
          fromJSON() %>%
          .$data %>%
          select(validTime, primary) %>%
          process_stage_data()
      }, error = function(e) {
        message(paste0("Could not fetch forecast data for row ", thisRow, ": "), e$message)
      })
      
      # Try to get observed data
      tryCatch({
        observedStage_daily = paste0(apiPrefix, customerInputTable$NWS_Gage_Name[thisRow], "/stageflow/observed") %>%
          GET(add_headers(accept = "application/json")) %>%
          content("text", encoding = "UTF-8") %>%
          fromJSON() %>%
          .$data %>%
          select(validTime, primary) %>%
          process_stage_data()
      }, error = function(e) {
        message(paste0("Could not fetch observed data for row ", thisRow, ": "), e$message)
      })
      
      # Merge data based on what was successfully retrieved
      if (!is.null(forecastStage_daily) && !is.null(observedStage_daily)) {
        # Case: Both forecast and observed data available
        allNew = forecastStage_daily %>%
          full_join(observedStage_daily, by="date") %>%
          mutate(value = coalesce(value.y, value.x)) %>%
          select(-value.x, -value.y)
        
        histAndForecast = historicalStage %>%
          full_join(allNew, by='date') %>%
          mutate(value = coalesce(value.y, value.x)) %>%
          select(-value.x, -value.y)
        
      } else if (!is.null(observedStage_daily)) {
        # Case: Only observed data available
        histAndForecast = historicalStage %>%
          full_join(observedStage_daily, by='date') %>%
          mutate(value = coalesce(value.y, value.x)) %>%
          select(-value.x, -value.y)
        
      } else {
        # Case: No API data available
        histAndForecast = historicalStage
      }
      
      # Complete and process the final dataset
      histAndForecast = histAndForecast %>%
        arrange(date) %>%
        complete(date = seq(min(date, na.rm = TRUE), 
                            max(date, na.rm = TRUE), 
                            by = "day")) %>%
        mutate(value = na.approx(value, na.rm = TRUE))
      
      if(writeNewTable_boolean) {
        write_lines(paste0("# ", Sys.Date()), fileLocation)
        write_csv(histAndForecast, fileLocation, append = TRUE, col_names =TRUE)
      }
    } else {
      histAndForecast = historicalStage
    }
    
    histAndForecast$doy = yday(histAndForecast$date)
    histAndForecast$year = year(histAndForecast$date)
    histAndForecast_ls[[thisRow]] = histAndForecast
  }
  return(histAndForecast_ls)
}  

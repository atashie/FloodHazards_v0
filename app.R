library(leaflet)
library(leafpop)
library(sf)
library(raster)
sf_use_s2(FALSE)
library(data.table)
library(hdf5r)
library(viridis)
library(ggplot2)
library(shiny)
library(dataRetrieval)
library(DT)
library(shinycssloaders)
library(shinyalert)
library(shinythemes)
library(aws.s3)

source("helperFunctions.R")
myNotes = fread("Data/myNotes.csv")
s3_bucket_name <- "climate-ai-data-science-shiny-app-data"

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = myNotes$this[1],       # Replace with your Access Key ID
  "AWS_SECRET_ACCESS_KEY" = myNotes$that[1], # Replace with your Secret Access Key
  "AWS_DEFAULT_REGION" = myNotes$theOther[1]
)

# Read local CSV files as before
customerInputTable = data.table::fread("Data/DIU_locations_Nov2024_ed.csv")
customerInputTable_sf = sf::st_as_sf(subset(customerInputTable, !is.na(Lat) & !is.na(Lon)),
                                     coords = c("Lon", "Lat"), crs = 4326)
csvCurrent = fread("Data/Fathom_Depth_Stats_1km2/FLOOD-MAP-1_3ARCSEC-ALL-2020-STATS.csv")
csv126 = fread("Data/Fathom_Depth_Stats_1km2/FLOOD-MAP-1_3ARCSEC-ALL-2050-SSP1_2.6-STATS.csv")
csv245 = fread("Data/Fathom_Depth_Stats_1km2/FLOOD-MAP-1_3ARCSEC-ALL-2050-SSP2_4.5-STATS.csv")
csv585 = fread("Data/Fathom_Depth_Stats_1km2/FLOOD-MAP-1_3ARCSEC-ALL-2050-SSP5_8.5-STATS.csv")

# Read metadata from
myMetadata = read_csv_from_s3_direct(bucket = s3_bucket_name, object_key = "riverForecasts/webdata_metadata.csv")

options(spinner.type = 6)

ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel(title=div(img(src="CAi2.png", height=60, width=200), "    Flood Hazard (beta)")),
                
                sidebarLayout(
                  position='left',
                  sidebarPanel(
#                    'User Interface',
                    width = 2,
                    radioButtons(
                      inputId = "chosenLoc",
                      label = "Location to Plot:",
                      choiceValues = 1:nrow(customerInputTable),
                      choiceNames = customerInputTable$Location_Name
                    )#,
#                    actionButton("rssButton", "Pull Live Data")
                  ),
                  mainPanel(
                    width = 10,
                    tabsetPanel(
                      type = "tabs",
                      # First Tab
                      tabPanel(
                        "General Flood Hazard",
                        fluidRow(
                          column(2,
                                 selectInput("sspScenario", "Climate Scenario:",
                                             c("Low Emissions (SSP126)" = "SSP1_2.6",
                                               "Middle of the Road (SSP245)" = "SSP2_4.5",
                                               "High Emissions (SSP585)" = "SSP5_8.5"))
                          ),
                          column(2,
                                 selectInput("floodType", "Flood Type:",
                                             c("Pluvial" = "PLUVIAL", "Fluvial" = "FLUVIAL", "Coastal" = "COASTAL", "Combined" = "ALL"))
                          ),
                          column(2,
                                 selectInput("recurrenceInterval", "Recurrence Interval:",
                                             rev(c("5yr" = "1in5", "10yr" = "1in10", 
                                               "20yr" = "1in20", "50yr" = "1in50",
                                               "100yr" = "1in100", "200yr" = "1in200",
                                               "500yr" = "1in500", "1000yr" = "1in1000")))
                          ),
                          column(2,
                                 selectInput("defence", "Infrastructure:",
                                             c("Undefended" = "UNDEFENDED", "Defended" = "DEFENDED"))
                          ),
                          column(2,
                                 selectInput("whichDecade", "Decade:",
                                             c("Current"=2020, "2050s"=2050))
#                                 ),
#                          column(2,
#                                 radioButtons(inputId = "depthOrOccurrence",
#                                              label = "Units:",
#                                              choiceValues = c(1,2),
#                                              choiceNames = c("Depth (m)", "Occurrence"))
                          )
                        ),
                        hr(), 
                        fluidRow(
                          column(8,
                                 withSpinner(leafletOutput("myMap_gf", height = "500px"), type=7),
                                 verbatimTextOutput("click_info")  # To display click information
                                 
                          ),
                          column(4,
                                 plotOutput("floodTrends"))
                        )
                      ),
                      # Second Tab
                      tabPanel(
                        "Monitor and Forecast",
                        fluidRow(
                          column(5,
                                 withSpinner(leafletOutput("myMap_gcHist", height = "500px"), type=7)
                          ),
                          column(5,
                                 withSpinner(leafletOutput("myMap_gcNow", height = "500px"), type=7)
                          ),
                          column(2,
                                 plotOutput("relFloodPlot")
                          )
                        ),
                        hr(),
                        hr(),
                        fluidRow(
                          plotOutput("locationPlotter"),
                          downloadButton("downloadPlot", "Download Plot")
                        )
                    )
                  )
              )
  )
)

server <- function(input, output, session) {

  ###################################################
  #!!!! START TAB - GENERAL FLOOD HAZARD
  #!!!! START TAB - GENERAL FLOOD HAZARD

  # Create a reactive value to store click information
  clicked_value <- reactiveVal(NULL)
  rasterData <- reactiveVal()
  
  output$myMap_gf <- renderLeaflet({
    thisDecade <- input$whichDecade
    thisFloodType <- input$floodType
    thisDefence <- input$defence
    
    if (thisFloodType == "PLUVIAL") {
      thisDefence <- "DEFENDED"  # since pluvial only has defended
    }
    
    if (thisFloodType != "ALL") {
      # Construct the object key for S3
      if (thisDecade == "2020") {
        my_object_key <- paste0("floodMonitorAndForecasts/Data/ClimateAI_Sample_geoTiffs/USv3_", input$whichDecade, "/Location_Name_",
                                customerInputTable$Location_Short_Name[as.numeric(input$chosenLoc)], "_geoTiffs/",
                                "FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", input$recurrenceInterval, "-",
                                thisFloodType, "-", thisDefence, "-DEPTH-", thisDecade,
                                "-PERCENTILE50-v3.1.tif")
      }
      if (thisDecade == "2050") {
        my_object_key <- paste0("floodMonitorAndForecasts/Data/ClimateAI_Sample_geoTiffs/USv3_", input$whichDecade, "/Location_Name_",
                                customerInputTable$Location_Short_Name[as.numeric(input$chosenLoc)], "_geoTiffs/",
                                "FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", input$recurrenceInterval, "-",
                                thisFloodType, "-", thisDefence, "-DEPTH-", thisDecade, "-", input$sspScenario,
                                "-PERCENTILE50-v3.1.tif")
      }
    } else {
      # Construct the object key for S3
      if (thisDecade == "2020") {
        my_object_key <- paste0("floodMonitorAndForecasts/Data/ClimateAI_Sample_geoTiffs/USv3_", input$whichDecade, "/Location_Name_",
                                customerInputTable$Location_Short_Name[as.numeric(input$chosenLoc)], "_geoTiffs/",
                                "FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", input$recurrenceInterval, "-",
                                thisFloodType, "-", thisDefence, "-DEPTH-", thisDecade,
                                "-PERCENTILE50-v3.tif")
      }
      if (thisDecade == "2050") {
        my_object_key <- paste0("floodMonitorAndForecasts/Data/ClimateAI_Sample_geoTiffs/USv3_", input$whichDecade, "/Location_Name_",
                                customerInputTable$Location_Short_Name[as.numeric(input$chosenLoc)], "_geoTiffs/",
                                "FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", input$recurrenceInterval, "-",
                                thisFloodType, "-", thisDefence, "-DEPTH-", thisDecade, "-", input$sspScenario,
                                "-PERCENTILE50-v3.tif")
      }
    }
    
    # Specify your S3 bucket name
    s3_bucket_name <- "climate-ai-data-science-shiny-app-data"
    
    # Read the raster from S3
    print(paste("the bucket is: ", s3_bucket_name))
    print(paste("the object key is: ", my_object_key))
    myRaster <- read_tif_from_s3(bucket = s3_bucket_name, object_key = my_object_key)
    
    # Proceed as before with your raster processing and plotting
    print(summary(myRaster))
    myRaster_nad <- reclassify(myRaster, cbind(-Inf, -1, NA), right = FALSE)
    print(summary(myRaster_nad))
    
    rasterData(myRaster_nad)
    
    # Get the actual range of values from your raster
    rasterRange <- range(raster::values(myRaster_nad), na.rm = TRUE)
    if (is.na(rasterRange[1])) { rasterRange[1] <- 0 }
    if (is.na(rasterRange[2])) { rasterRange[2] <- 10 }
    rasterRange[1] <- min(0, rasterRange[1])
    rasterRange[2] <- max(10, rasterRange[2])
    
    # Create color palette with NA handling
    thisPal <- turbo(n = 10, begin = 0, end = 1, direction = 1)
    thatPal <- colorNumeric(
      palette = thisPal,
      domain = rasterRange,
      na.color = "#80808066"  # Hex code for semi-transparent grey
    )
    
    thisCol <- 1
    myLat <- customerInputTable$Lat[as.numeric(input$chosenLoc)]
    myLon <- customerInputTable$Lon[as.numeric(input$chosenLoc)]
    
    # Create a base map
    myMap_gf <- leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addRasterImage(
        myRaster_nad,
        colors = thatPal,
        project = FALSE,
        opacity = 0.8,
        group = "Flood Layer"
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = thatPal,
        values = raster::values(myRaster_nad),
        title = "Flood Depth (cm)",
        opacity = 1
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        overlayGroups = c("Flood Layer"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = myLon, lat = myLat, zoom = 15)
    
    myMap_gf
  })
  
  # Modify the observer to use the reactive raster
  observeEvent(input$myMap_gf_click, {
    click <- input$myMap_gf_click
    lon <- click$lng
    lat <- click$lat
    
    # Get the current raster from reactive value
    current_raster <- rasterData()
    
    # Create SpatialPoints object
    point <- SpatialPoints(cbind(lon, lat), proj4string = crs(current_raster))
    
    # Extract raster value at clicked point
    val <- raster::extract(current_raster, point)
    
    # Update reactive value
    clicked_value(paste0("Lon: ", round(lon, 2), 
                         ", Lat: ", round(lat, 2), 
                         ", Flood Depth: ", round(val, 3), " cm"))
    
    # Add popup to map
    leafletProxy("myMap_gf") %>%
      clearPopups() %>%
      addPopups(lon, lat, paste("Flood Depth:", round(val, 3), "cm"), layerId = "popup")
  })
  
  
  output$floodTrends = renderPlot({
    whichRow = as.numeric(input$chosenLoc) 
    thisDefence = input$defence
    if(input$floodType == "PLUVIAL") {thisDefence = "DEFENDED"}
    colNameCurrent = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", input$recurrenceInterval, "-", 
                            input$floodType, "-", thisDefence, "-DEPTH-2020", 
                            "-PERCENTILE50-v3.1_mean")
    colName126 = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", input$recurrenceInterval, "-", 
                        input$floodType, "-", thisDefence, "-DEPTH-2050", 
                        "-SSP1_2.6-PERCENTILE50-v3.1_mean")
    colName245 = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", input$recurrenceInterval, "-", 
                        input$floodType, "-", thisDefence, "-DEPTH-2050", 
                        "-SSP2_4.5-PERCENTILE50-v3.1_mean")
    colName585 = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", input$recurrenceInterval, "-", 
                        input$floodType, "-", thisDefence, "-DEPTH-2050", 
                        "-SSP5_8.5-PERCENTILE50-v3.1_mean")
    print(colNameCurrent)
    #    whichRow = 1
    #    tt = "FLOOD_MAP-1_3ARCSEC-NW_OFFSET-1in5-PLUVIAL-DEFENDED-DEPTH-2020-PERCENTILE50-v3.1_mean"
    #    avgVals = c(csvCurrent[whichRow,..tt],csvCurrent[whichRow,..tt],csvCurrent[whichRow,..tt])
    avgVals = c(csvCurrent[whichRow,get(colNameCurrent)], csv126[whichRow, get(colName126)],
                csv245[whichRow, get(colName245)], csv585[whichRow, get(colName585)])
    if(any(avgVals < 0)) {avgVals[which(avgVals < 0)] = 0}
    barplot(height=unlist(avgVals), 
            col=c('royalblue', 'orangered1', 'orangered2', 'orangered3'), 
            ylim =c(0,max(100, max(avgVals))), main="Avg Flood Depth (cm)",
            names.arg=c("Current", "SSP126", "SSP245", "SSP585"))
    #            legend.text=c("Current", "SSP126", "SSP245", "SSP585"))
  })
  
  
  #!!!! END TAB - GENERAL FLOOD HAZARD
  #!!!! END TAB - GENERAL FLOOD HAZARD
  ###################################################
  

  ###################################################
  #!!!! START TAB - CURRENT CONDITIONS
  #!!!! START TAB - CURRENT CONDITIONS
  
  output$myMap_gcHist = renderLeaflet({
    locName = customerInputTable$Location_Short_Name[as.numeric(input$chosenLoc)]
    print("#############################################")
    print(locName)
    
    nowTif_hist = read_tif_from_s3(
      bucket = s3_bucket_name,
      object_key = paste0("floodMonitorAndForecasts/Data/Nowcasts/historic_water/", locName, "_historical.tif"))
    print(summary(nowTif_hist))
    print(unique(nowTif_hist))
    
    thisCol = as.numeric(input$varType)   
    myLat = customerInputTable$Lat[as.numeric(input$chosenLoc)]
    myLon = customerInputTable$Lon[as.numeric(input$chosenLoc)]
    
    # Define your categorical colors
    thisPal = c("forestgreen", "blue1")#, 'orangered2')
    
    # Create a factor palette function for categorical data
    pal <- colorFactor(
      palette = thisPal,
      domain = c(0, 1)
      )
   
    # First, define your colors and labels
    colors_legend <- c("forestgreen", "blue1")
    labels_legend <- c("Land", "Permanent Water")
    
    # Then modify your leaflet map
    myMap_gcHist <- leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addRasterImage(
        nowTif_hist,
        colors = pal,
        opacity = 0.7,
        project = FALSE,
        group = "Flood Layer"
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = c(0, 1),
        title = "Categories",
        opacity = 1,
        labFormat = function(type, cuts, p) {
          labels <- c("Land", "Permanent Water")
          return(labels)
        }
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        overlayGroups = c("Flood Layer"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = myLon, lat = myLat, zoom = 15)
    
    myMap_gcHist
  })
  
  
  # Create a reactive expression for the nowcast GeoTIFF data, since it will be used 2x
  nowTif_reactive <- reactive({
    locName = customerInputTable$Location_Short_Name[as.numeric(input$chosenLoc)]
    print("#############################################")
    print(locName)
    
    nowTif_now = read_tif_from_s3(
      bucket = s3_bucket_name,
      object_key = paste0("floodMonitorAndForecasts/Data/Nowcasts/nowcasts/latest/", locName, "_nowcast.tif"))
    print(summary(nowTif_now))
    print(unique(nowTif_now))
    return(nowTif_now)
  })
  
  # NOWCAST MAP
  output$myMap_gcNow = renderLeaflet({
    nowTif_now = nowTif_reactive()

    thisCol = as.numeric(input$varType)   
    myLat = customerInputTable$Lat[as.numeric(input$chosenLoc)]
    myLon = customerInputTable$Lon[as.numeric(input$chosenLoc)]
    
    # categorical colors
    thisPal = c("grey50", "forestgreen", "blue1", 'purple3', "lightsteelblue1")
    
    # palette function for categorical data
    pal <- colorFactor(
      palette = thisPal,
      domain = c( -9999, 0, 1, 2, 3)
    )
    
    # First, define your colors and labels
    colors_legend <- c( "grey50", "forestgreen", "blue1", 'purple3', "lightsteelblue1")
    labels_legend <- c("No Data", "Land", "Permanent Water", "Flood", "Cloud")
    
    
    # Create the map
    myMap_gcNow <- leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addRasterImage(
        nowTif_now,
        colors = pal,
        opacity = 0.7,
        project = FALSE,
        group = "Flood Layer"
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = c(-9999, 0, 1, 2, 3),
        labels = c("No Data", "Land", "Permanent Water", "Flood", "Cloud"),
        title = "Categories",
        opacity = 1,
        labFormat = function(type, cuts, p) {
          labels <- c("No Data", "Land", "Permanent Water", "Flood", "Cloud")
          return(labels)
        }
        
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        overlayGroups = c("Flood Layer"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = myLon, lat = myLat, zoom = 15)
    
    myMap_gcNow
  })
  
 
  
  
  
  output$relFloodPlot = renderPlot({
    nowTif_now = nowTif_reactive()
    raster_values <- as.matrix(nowTif_now)
#    raster_values <- as.vector(raster_values)  # flatten the matrix if needed
    
    # Count values
    land_count = sum(raster_values == 0, na.rm = TRUE)
    permWater_count = sum(raster_values == 1, na.rm = TRUE)
    totWater_count = sum(raster_values %in% c(1,2), na.rm = TRUE)
    totFlood_count = sum(raster_values == 2, na.rm = TRUE)
    total_valid = max(land_count + totWater_count, 1)
 
    # Calculate percentages
    perm_water_pct <- permWater_count / total_valid * 100
    flood_pct <- totFlood_count / total_valid * 100
    
    # Calculate percentages
    perm_water_pct <- permWater_count / total_valid * 100
    flood_pct <- totFlood_count / total_valid * 100
    
    # Create matrix for stacked barplot
    heights_matrix <- matrix(
      c(perm_water_pct,    # First row (permanent water)
        flood_pct),        # Second row (flood only)
      nrow = 2,
      byrow = TRUE
    )
    
    # Create the barplot
    par(mar = c(3, 3, 2, 1),
        cex.main = 1.5,            # Title size
        cex.lab = 1.5,          # Axis labels size
        cex.axis = 1.5)         # Axis numbers size
    
    # Create the barplot
    bp = barplot(heights_matrix,
                 beside = FALSE,
                 col = c("blue1", "purple3"),
#                 main = "Distribution of Land Categories",
                 xlab = NULL,
                 ylab = NULL,
                 names.arg = "Inundation %",
                 ylim = c(0, max(perm_water_pct + flood_pct) * 1.2)) # Increased space for labels
    
    # Add label for permanent water
    text(x = bp,
         y = perm_water_pct/2, # Position label in middle of blue section
         col='grey90',
         labels = paste0("Hist: ", sprintf("%.1f%%", perm_water_pct)),
         cex = 1.8)
    
    # Add label for total
    text(x = bp,
         y = perm_water_pct + flood_pct,
         labels =  paste0("Current: ", sprintf("%.1f%%", perm_water_pct + flood_pct)),
         pos = 3,
         offset = 0.5,
         cex = 1.8)
    })

  
  
  
  # calculating quantiles and previous year values
  historicalQuantiles_ls = list()
  forecastDistributions_ls = list()
  for(thisRow in 1:nrow(customerInputTable)){
    if(customerInputTable$Trained_Model_Exists[thisRow]) {
      thisRiverName = customerInputTable$Location_Name[thisRow] 
      my_object_key_historical = paste0("riverForecasts/historicalFor_", thisRiverName, ".csv")
      my_object_key_forecasts = paste0("riverForecasts/forecastsFor_", thisRiverName, ".h5")
      
      histDat = read_csv_from_s3_direct(bucket = s3_bucket_name, object_key = my_object_key_historical)
      foreDat = read_h5_from_s3(bucket = s3_bucket_name, object_key = my_object_key_forecasts) # dims[1, time, model]
      
      # aggregating the data for plotting and tables
      historicalQuantiles_ls[[thisRow]] = quantileCalculator_f(
        streamflowTable = histDat,
        forecastDaysOut = myMetadata$lookForward[1],
        lastHistDate = as.Date(myMetadata$saveDate[1])
      ) 
      
      forecastDistributions_ls[[thisRow]] = distributionsCalculator_f(
        streamflowArray = foreDat$array, 
        lastHistDate = as.Date(myMetadata$saveDate[1]), 
        modelInitDate = myMetadata$modelInitDate[1]
      )
      
    } else {
      streamflowQuantiles_ls[[thisRow]] = NA
      forecastDistributions_ls[[thisRow]] = NA
    }
  } 
  
  
  output$locationPlotter = renderPlot({
    if(customerInputTable$Trained_Model_Exists[as.numeric(input$chosenLoc)]){
      locationPlotter_f(
        locName = customerInputTable$Location_Name[as.numeric(input$chosenLoc)],
        historicalTable = historicalQuantiles_ls[[as.numeric(input$chosenLoc)]], 
        forecastTable = forecastDistributions_ls[[as.numeric(input$chosenLoc)]], 
        lastHistDate = myMetadata$saveDate[1])#,lookahead = 180)
    } else {
      # Create an empty plot with a message
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
           xlim = c(0, 1), ylim = c(0, 1))
      text(x = 0.5, y = 0.5, labels = " -- Forecast Not Available -- ", 
           cex = 2,  # This controls the font size (2 means 2x normal size)
           font = 2) # This makes the text bold
    }
    
  })
  
  
  
    
  #!!!! END TAB - GENERAL CONDITIONS
  #!!!! END TAB - GENERAL CONDITIONS
  ###################################################
  
}

shinyApp(ui, server)
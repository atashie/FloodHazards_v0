quantileCalculator_f = function(
    streamflowData_ls = streamflowData_ls,
    probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)) {
  
  allQuantiles_ls = list()
  for(thisItem in 1:length(streamflowTable_ls)){
    streamflowTable = as.data.table(streamflowTable_ls[[thisItem]])
    if(any(streamflowTable$value < -99)){
      streamflowTable$value[streamflowTable$value < -99] = NA
      streamflowTable$value[is.na(streamflowTable$value)] = min(streamflowTable$value, na.rm=TRUE)
      
    }
    # remove leap days to simplify plotting
    streamflowTable = subset(streamflowTable, doy < 366)
    currentDoyStart = last(streamflowTable$doy)
    currentYearStart = last(streamflowTable$year)
    
    
    thisYearStartRow = which(streamflowTable$year == (currentYearStart - 1) & streamflowTable$doy == currentDoyStart) 
    thisYearRows = thisYearStartRow:(thisYearStartRow+364)
    lastYearRows = (thisYearStartRow-365):(thisYearStartRow-1)
    last2YearRows = (thisYearStartRow-365*2):(thisYearStartRow-1-365)
    
    quantiles_by_doy = streamflowTable[ , as.list(quantile(value, probs = probs, na.rm = TRUE)), by = doy][streamflowTable$doy[thisYearRows],]
    
    dataTable = data.table(
      date = streamflowTable$date[thisYearRows],
      doy = streamflowTable$doy[thisYearRows],
      value = round(streamflowTable$value[thisYearRows], 1),
      valueLastYear = round(streamflowTable$value[lastYearRows], 1),
      valueLast2Year = round(streamflowTable$value[last2YearRows], 1),
      Q01 = round(unlist(quantiles_by_doy[,2]), 1),
      Q05 = round(unlist(quantiles_by_doy[,3]), 1),
      Q10 = round(unlist(quantiles_by_doy[,4]), 1),
      Q25 = round(unlist(quantiles_by_doy[,5]), 1),
      Q50 = round(unlist(quantiles_by_doy[,6]), 1),
      Q75 = round(unlist(quantiles_by_doy[,7]), 1),
      Q90 = round(unlist(quantiles_by_doy[,8]), 1),
      Q95 = round(unlist(quantiles_by_doy[,9]), 1),
      Q99 = round(unlist(quantiles_by_doy[,10]), 1)
    )
    allQuantiles_ls[[thisItem]] = dataTable
  }
  return(allQuantiles_ls)
}


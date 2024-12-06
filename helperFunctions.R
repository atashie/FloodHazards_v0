

## Translate historic streamflow into quantiles and recent historic
quantileCalculator_f = function(
    streamflowTable,
    probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
    forecastDaysOut = 180,
    lastHistDate) {

  streamflowTable = streamflowTable[which(as.Date(streamflowTable$Date) <= as.Date(lastHistDate)),]
  if(any(streamflowTable$Value < -99)){
    streamflowTable$Value[streamflowTable$Value < -99] = NA
    streamflowTable$Value[is.na(streamflowTable$Value)] = min(streamflowTable$Value, na.rm=TRUE)
    
  }
  
  streamflowTable$doy = yday(streamflowTable$Date)
  streamflowTable$year = year(streamflowTable$Date)

    # remove leap days to simplify plotting
  streamflowTable = subset(streamflowTable, doy < 366)
  
    # identifying the date ranges for different years
  firstRowToIllustrate = which(streamflowTable$Date == (lastHistDate - 365 + forecastDaysOut))
  thisYearRows = firstRowToIllustrate:(firstRowToIllustrate+364)
  lastYearRows = thisYearRows-365
  last2YearRows = thisYearRows-365*2
  
  quantiles_by_doy = streamflowTable[ , as.list(quantile(Value, probs = probs, na.rm = TRUE)), by = doy]
  if(streamflowTable$doy[firstRowToIllustrate] != 1) {
    N = streamflowTable$doy[firstRowToIllustrate]
    quantiles_by_doy = quantiles_by_doy[c(N:365, 1:(N-1)),]
  }
    
  dataTable = data.table(
    date = seq.Date(streamflowTable$Date[firstRowToIllustrate], by=1, length.out=365),
    doy = streamflowTable$doy[lastYearRows],
    Value = round(streamflowTable$Value[thisYearRows], 1),
    ValueLastYear = round(streamflowTable$Value[lastYearRows], 1),
    ValueLast2Year = round(streamflowTable$Value[last2YearRows], 1),
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
  return(dataTable)
}



distributionsCalculator_f = function(
  streamflowArray,# dims[1, time, model]
  probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
  lastHistDate,
  modelInitDate) {

  forecastTable = streamflowArray[1, , , drop = TRUE]  # This removes the first dimension
    # truncating to dates when the forecast begins to diverge from historical
    # - note: era5 is 7 days delayd, so the "date of divergence" is 7 days prior to the forecast initializion
  startRow = which(forecastTable[,1] - forecastTable[,2] !=0)[1]
  forecastTable = forecastTable[-c(1:(startRow-1)),]
  
  percentileTable =  t(apply(forecastTable, 1, function(x) {
    quantile(x, probs = probs)
  }))
  
  dataTable = data.table(
    date = seq.Date(as.Date(lastHistDate)+1, by=1, length.out=nrow(forecastTable)),
    Q05 = loess_smooth(percentileTable[,1]),
    Q10 = loess_smooth(percentileTable[,2]),
    Q25 = loess_smooth(percentileTable[,3]),
    Q50 = loess_smooth(percentileTable[,4]),
    Q75 = loess_smooth(percentileTable[,5]),
    Q90 = loess_smooth(percentileTable[,6]),
    Q95 = loess_smooth(percentileTable[,7])
  )
  
  return(dataTable)
}




# basic loess smoothing function
loess_smooth <- function(x) {
  predict(loess(x ~ seq_along(x), span=0.2))
}


# plotting function for River Forecasts
locationPlotter_f = function(
    locName,
    historicalTable, 
    forecastTable, 
    lastHistDate,
    lookahead = 180) {
   
  # smoothing historical data
  Q01_smooth = loess_smooth(historicalTable$Q01)
  Q05_smooth = loess_smooth(historicalTable$Q05)
  Q10_smooth = loess_smooth(historicalTable$Q10)
  Q25_smooth = loess_smooth(historicalTable$Q25)
  Q75_smooth = loess_smooth(historicalTable$Q75)
  Q90_smooth = loess_smooth(historicalTable$Q90)
  Q95_smooth = loess_smooth(historicalTable$Q95)
  Q99_smooth = loess_smooth(historicalTable$Q99)
  
  # truncating forecast data if necessary
  lastPlotDate = last(historicalTable$date)
  if(last(forecastTable$date) > lastPlotDate){ 
    forecastTable = forecastTable[1:(which(forecastTable$date == lastPlotDate)), ]
  }
  forecastTable$doy = yday(forecastTable$date)
#  historicalTable$Value[is.na(historicalTable$Value)] = forecastTable$Q50
#  lastHistDate = first(which(is.na(historicalTable$Value)))
#  startForecast = which(historicalTable$date == lastHistDate) + 1

  # Update NA values in historicalTable's Value column
  historicalTable[is.na(Value), Value := forecastTable[.SD, Q50, on = "date"]]
    
#  yRange = range(c(historicalTable[,-c(1,2)], customerInputRow[,c(Low_Level_ft, High_Level_ft)]), na.rm=TRUE)
  yRange = range(historicalTable[,-c(1,2)], na.rm=TRUE)
  yNudger = abs(diff(yRange)) * 0.035
  par(mar = c(5.1, 4.1, 4.1, 2.1+2))
#  par(mar = c(1, 1, 1, 1))

  
  # incrementing negatives to allow log transformation for plotting
  numCols = ncol(forecastTable)
  if(any(forecastTable[,c(2:numCols)] < 0, na.rm=TRUE)) {
    forecastTable[,2:numCols] = forecastTable[,2:numCols] + abs(min(forecastTable[,2:numCols], na.rm=TRUE)) + 0.1
  }
  numCols = ncol(historicalTable)
  if(any(historicalTable[ ,3:numCols] < 0, na.rm=TRUE)) {
    historicalTable[,3:numCols] = historicalTable[,3:numCols] + abs(min(historicalTable[,3:numCols], na.rm=TRUE)) + 0.1
  }
  
    
  plot(1:365, NULL,  ylim = yRange ,
       type='l', lwd=1, col='white', 
       log='y',
       xaxt = 'n', 
       ylab="River Stage (ft)", xlab='', main=locName,
       col.lab='#666D74', col.axis='#666D74', col.main='#1A232F')#,
  axis(1, at=seq(1, 365, by=30),  # Show dates every 30 days
       labels=format(historicalTable$date[seq(1, 365, by=30)]),# "%b"),  # Month abbreviation
       col.axis='#666D74')
  polygon(x=c(1:365, 365:1), y=c(Q05_smooth, rev(Q95_smooth)),
          col=adjustcolor('#0098B2', alpha.f=0.04), border=NA)
  polygon(x=c(1:365, 365:1), y=c(Q10_smooth, rev(Q90_smooth)),
          col=adjustcolor('#0098B2', alpha.f=0.04), border=NA)
  polygon(x=c(1:365, 365:1), y=c(Q25_smooth, rev(Q75_smooth)),
          col=adjustcolor('#0098B2', alpha.f=0.04), border=NA)
  polygon(x=c(1:365, 365:1), y=c(Q01_smooth, rev(Q99_smooth)),
          col=adjustcolor('#0098B2', alpha.f=0.04), border=NA)
  #    abline(h=annualData$min, lwd=1, lty =5, col=adjustcolor('#EE6222', alpha.f=1))
  #    abline(h=annualData$Q10, lwd=1, lty =5, col=adjustcolor('#EE6222', alpha.f=.5))
  #    abline(h=annualData$Q25, lwd=1, lty =5, col=adjustcolor('#EE6222', alpha.f=.2))
  #    abline(h=annualData$Q50, lwd=1, lty =5, col=adjustcolor('#EE6222', alpha.f=.1))
  #    abline(h=annualData$Q75, lwd=1, lty =5, col=adjustcolor('#EE6222', alpha.f=.05))
  #    abline(h=annualData$Q90, lwd=1, lty =5, col=adjustcolor('#EE6222', alpha.f=.01))
#  abline(h=customerInputRow$Low_Level_ft, lwd=2, lty =1, col=adjustcolor('red3', alpha.f=.9))
#  text("Low Water", x=330, y=customerInputRow$Low_Level_ft + yNudger, col='red3', lwd=3)
#  abline(h=customerInputRow$High_Level_ft, lwd=2, lty =1, col=adjustcolor('purple4', alpha.f=.9))
#  text("High Water", x=330, y=customerInputRow$High_Level_ft - (yNudger), col='purple4', lwd=3)
  lines(x=1:365, y =  historicalTable$ValueLast2Year, col=adjustcolor('royalblue2', alpha.f=.5), lty=3, lwd=3)
  lines(x=1:365, y = historicalTable$ValueLastYear, col=adjustcolor('royalblue4', alpha.f=.7), lwd=3, lty=2)

  #now adding forecast
  numDaysF = nrow(forecastTable)
  xOrder = c((366-numDaysF):365, 365:(366-numDaysF))
  polygon(x=xOrder, y=c(forecastTable$Q05, rev(forecastTable$Q95)),
          col=adjustcolor('orangered2', alpha.f=0.2), border=NA)
  polygon(x=xOrder, y=c(forecastTable$Q10, rev(forecastTable$Q90)),
          col=adjustcolor('orangered2', alpha.f=0.2), border=NA)
  polygon(x=xOrder, y=c(forecastTable$Q25, rev(forecastTable$Q75)),
          col=adjustcolor('orangered2', alpha.f=0.2), border=NA)
  lines(x=1:365, y = historicalTable$Value, col=adjustcolor('orangered2', alpha.f=1), lwd=3, lty=1)
  legend('topleft', legend = c("Last Year", "2 Yrs Ago", "This Year"), 
         col=c('royalblue2', 'royalblue4', 'orangered2'), lty = 3:1, cex=1.2, box.lty=0)
}

get_aws_signed_url <- function(file, bucket = "my_bucket_name", timeout_seconds = 30, key = "my_jey", secret = "my_secret", region = "us-east-1"){
  # API Implmented according to https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html#query-string-auth-v4-signing-example
  algorithm <- "AWS4-HMAC-SHA256"
  time <- Sys.time()
  date_time <- format(time, "%Y%m%dT%H%M%SZ", tz = "UTC")
  
  # Build query parameters
  date <- glue("/{format(time,'%Y%m%d', tz = 'UTC')}/")
  region_encoded <- glue("{region}/")
  amzn <- "s3/aws4_request"
  
  # Query parameters, this portion is implemented with the help of https://github.com/cloudyr/aws.s3/blob/master/R/s3HTTP.R
  request_body <- ""
  body_hash <- tolower(digest::digest(request_body,
                                      file = is.character(request_body) && file.exists(request_body),
                                      algo = "sha256", serialize = FALSE))
  
  
  signature <- aws.signature::signature_v4_auth(datetime = date_time,
                                                region = region,
                                                service = "s3",
                                                verb = "GET",
                                                action = glue("/{bucket}/{file}"),
                                                key = key,
                                                secret = secret,
                                                request_body = "",
                                                query_args = list(`X-Amz-Algorithm` = algorithm,
                                                                  `X-Amz-Credential` = glue("{key}{date}{region_encoded}{amzn}"),
                                                                  `X-Amz-Date` = date_time,
                                                                  `X-Amz-Expires` = timeout_seconds,
                                                                  `X-Amz-SignedHeaders` = "host",
                                                                  `x-amz-content-sha256` = body_hash),
                                                algorithm = algorithm,
                                                canonical_headers = list(host = glue("s3-{region}.amazonaws.com")))
  
  return(glue("https://s3-{region}.amazonaws.com/{bucket}/{file}?X-Amz-Algorithm={signature$Query$`X-Amz-Algorithm`}&X-Amz-Credential={signature$Query$`X-Amz-Credential`}&X-Amz-Date={signature$Query$`X-Amz-Date`}&X-Amz-Expires={signature$Query$`X-Amz-Expires`}&x-amz-content-sha256={signature$Query$`x-amz-content-sha256`}&X-Amz-SignedHeaders={signature$Query$`X-Amz-SignedHeaders`}&X-Amz-Signature={signature$Signature}"))
}



# Function to read .tif files from S3 with region specification
read_tif_from_s3 <- function(bucket, object_key) {
  # Create a temporary file
  temp_file <- tempfile(fileext = ".tif")
  
  # Download the S3 object to the temporary file
  # Specify use_https = TRUE and region
  success <- aws.s3::save_object(
    object = object_key,
    bucket = bucket,
    file = temp_file,
    region = Sys.getenv("AWS_DEFAULT_REGION"),
    use_https = TRUE
  )
  
  #    if (!success) {
  #      stop("Failed to download the file from S3.")
  #    }
  
  # Read the raster from the temporary file
  raster_obj <- raster(temp_file)
  return(raster_obj)
}

# Function to read .h5 files from S3 with region specification
read_h5_from_s3 <- function(bucket, object_key) {
  # Create a temporary file
  temp_file <- tempfile(fileext = ".h5")
  
  # Download the S3 object to the temporary file
  success <- aws.s3::save_object(
    object = object_key,
    bucket = bucket,
    file = temp_file,
    region = Sys.getenv("AWS_DEFAULT_REGION"),
    use_https = TRUE
  )
  
  # Read the H5 file using rhdf5 package
  require(rhdf5)
  h5_data <- h5read(temp_file, "/") # reads entire file
  
  return(h5_data)
}

# Function to read CSV directly from S3 using data.table::fread
read_csv_from_s3_direct <- function(bucket, object_key) {
  require(aws.s3)
  require(data.table)
  
  # Get the object directly as a raw vector
  s3_object <- aws.s3::get_object(
    object = object_key,
    bucket = bucket,
    region = Sys.getenv("AWS_DEFAULT_REGION")
  )
  
  # Convert raw vector to character
  csv_text <- rawToChar(s3_object)
  
  # Read CSV using fread with text input
  csv_data <- fread(text = csv_text)  # Note the 'text=' parameter
  
  return(csv_data)
}



# Function to read GeoJSON files from S3
read_geojson_from_s3 <- function(bucket, object_key) {
  require(aws.s3)
  require(sf)
  
  # Create a temporary file
  temp_file <- tempfile(fileext = ".geojson")
  
  # Download the S3 object to the temporary file
  success <- aws.s3::save_object(
    object = object_key,
    bucket = bucket,
    file = temp_file,
    region = Sys.getenv("AWS_DEFAULT_REGION"),
    use_https = TRUE
  )
  
  # Read the GeoJSON file using sf
  geojson_data <- sf::st_read(temp_file, quiet = TRUE)
  
  return(geojson_data)
}





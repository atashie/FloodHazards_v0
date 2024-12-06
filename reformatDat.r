library(raster)

customerInputTable = data.table::fread("C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\DIU_locations_Nov2024_ed.csv")
allLocs = customerInputTable$Location_Short_Name

firstPart = "FLOOD_MAP-1_3ARCSEC-NW_OFFSET-"
secondPart = "-DEPTH-"
lastPart = "-PERCENTILE50-v3.1"
theYear = "2020"



allRecurs = c("1in5", "1in10", "1in20", "1in50", "1in100", "1in200", "1in500", "1in1000")
allDefenses = c("DEFENDED", "UNDEFENDED")

	# reading in teh csv's to create new "all" columns
floodDat_csv = data.table::fread(paste0(
	"C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\Fathom_Depth_Stats_1km2\\FLOOD-MAP-1_3ARCSEC-ALL-2020-STATS.csv"))

for(thisLoc in 1:length(allLocs)){
	locName = allLocs[thisLoc] #"AlamoDam"
	fileLoc = paste0("C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\ClimateAI_Sample_geoTiffs\\USv3_", theYear, "\\Location_Name_", locName , "_geoTiffs\\")
	for(thisDefense in 1:length(allDefenses)){
		typeDefense = allDefenses[thisDefense]#"DEFENDED"
		for(thisRecur in 1:length(allRecurs)){
			typeRecur = allRecurs[thisRecur]#"1in5"
		

			coastFlood = raster(paste0(fileLoc, firstPart, typeRecur, "-COASTAL", "-", typeDefense, secondPart, theYear, lastPart, ".tif"))
			pluvialFlood = raster(paste0(fileLoc, firstPart, typeRecur, "-PLUVIAL", "-", "DEFENDED", secondPart, theYear, lastPart, ".tif"))
			fluvialFlood = raster(paste0(fileLoc, firstPart, typeRecur, "-FLUVIAL", "-", typeDefense, secondPart, theYear, lastPart, ".tif"))

			# Method 1: Using stack() and calc()
			rasterStack = stack(coastFlood, pluvialFlood, fluvialFlood)
			maxFlood <- calc(rasterStack, fun = max)

			# add columns to csvs 
			colName = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-ALL", "-", typeDefense, "-DEPTH-", "2020", "-PERCENTILE50-v3.1_mean")
				if(thisLoc == 1){
					floodDat_csv[, (colName) := as.numeric(NA)]
					floodDat_csv[, (colName) := as.numeric(get(colName))]
				}
				# Assign value to specific cell
				floodDat_csv[thisLoc, (colName) := mean(as.matrix(maxFlood), na.rm=TRUE)]

			# Save the output if needed
			writeRaster(maxFlood, 
				filename = paste0(fileLoc, firstPart, typeRecur, "-ALL", "-", typeDefense, secondPart, theYear, lastPart), 
				format = "GTiff", overwrite = TRUE)
		}
	}
}
data.table::fwrite(floodDat_csv, paste0(
  "C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\Fathom_Depth_Stats_1km2\\FLOOD-MAP-1_3ARCSEC-ALL-2020-STATS.csv"))













firstPart = "FLOOD_MAP-1_3ARCSEC-NW_OFFSET-"
secondPart = "-DEPTH-"
lastPart = "-PERCENTILE50-v3.1"
theYear = "2050"


allSsps = c("SSP1_2.6", "SSP2_4.5", "SSP5_8.5")
allRecurs = c("1in5", "1in10", "1in20", "1in50", "1in100", "1in200", "1in500", "1in1000")
allDefenses = c("DEFENDED", "UNDEFENDED")



colCoast = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-COASTAL", "-", typeDefense, "-DEPTH-", "2020", "-PERCENTILE50-v3.1_mean")
colPluvial = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-PLUVIAL", "-", "DEFENDED", "-DEPTH-", "2020", "-PERCENTILE50-v3.1_mean")
colFluvial = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-FLUVIAL", "-", typeDefense, "-DEPTH-", "2020", "-PERCENTILE50-v3.1_mean")




for(thisLoc in 1:length(allLocs)){
	locName = allLocs[thisLoc] #"AlamoDam"
	fileLoc = paste0("C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\ClimateAI_Sample_geoTiffs\\USv3_", theYear, "\\Location_Name_", locName , "_geoTiffs\\")
	for(thisSsp in 1:length(allSsps)) {
		typeSsp = allSsps[thisSsp]
		
			# reading in teh csv's to create new "all" columns
		floodDat_csv = data.table::fread(paste0(
			"C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\Fathom_Depth_Stats_1km2\\FLOOD-MAP-1_3ARCSEC-ALL-2050-", typeSsp, "-STATS.csv"))
		
		for(thisDefense in 1:length(allDefenses)){
			typeDefense = allDefenses[thisDefense]#"DEFENDED"
			for(thisRecur in 1:length(allRecurs)){
				typeRecur = allRecurs[thisRecur]#"1in5"
			

				coastFlood = raster(paste0(fileLoc, firstPart, typeRecur, "-COASTAL", "-", typeDefense, secondPart, theYear, "-", typeSsp, lastPart, ".tif"))
				pluvialFlood = raster(paste0(fileLoc, firstPart, typeRecur, "-PLUVIAL", "-", "DEFENDED", secondPart, theYear, "-", typeSsp, lastPart, ".tif"))
				fluvialFlood = raster(paste0(fileLoc, firstPart, typeRecur, "-FLUVIAL", "-", typeDefense, secondPart, theYear, "-", typeSsp, lastPart, ".tif"))

				# calculate max by pixel
				rasterStack = stack(coastFlood, pluvialFlood, fluvialFlood)
				maxFlood <- calc(rasterStack, fun = max)

				# add columns to csvs 
				colName = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-ALL", "-", typeDefense, "-DEPTH-", "2050", "-", typeSsp, "-PERCENTILE50-v3.1_mean")
				if(thisLoc == 1){
					floodDat_csv[, (colName) := as.numeric(NA)]
					floodDat_csv[, (colName) := as.numeric(get(colName))]
				}
				# Assign value to specific cell
				floodDat_csv[thisLoc, (colName) := mean(as.matrix(maxFlood), na.rm=TRUE)]


				# Save the output if needed
				writeRaster(maxFlood, 
					filename = paste0(fileLoc, firstPart, typeRecur, "-ALL", "-", typeDefense, secondPart, theYear, "-", typeSsp, lastPart), 
					format = "GTiff", overwrite = TRUE)
					
			}
		}
		data.table::fwrite(floodDat_csv, paste0(
		  "C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\Fathom_Depth_Stats_1km2\\FLOOD-MAP-1_3ARCSEC-ALL-2050-", typeSsp, "-STATS.csv"))
	}
}








###################

floodDat2020 = data.table::fread("C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\Fathom_Depth_Stats_1km2\\FLOOD-MAP-1_3ARCSEC-ALL-2020-STATS.csv")
floodDat126 = data.table::fread("C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\Fathom_Depth_Stats_1km2\\FLOOD-MAP-1_3ARCSEC-ALL-2050-SSP1_2.6-STATS.csv")
floodDat245 = data.table::fread("C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\Fathom_Depth_Stats_1km2\\FLOOD-MAP-1_3ARCSEC-ALL-2050-SSP2_4.5-STATS.csv")
floodDat585 = data.table::fread("C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\Fathom_Depth_Stats_1km2\\FLOOD-MAP-1_3ARCSEC-ALL-2050-SSP5_8.5-STATS.csv")


for(thisLoc in 1:length(allLocs)){
	locName = allLocs[thisLoc] #"AlamoDam"
	fileLoc = paste0("C:\\Users\\arik\\Documents\\GitHub\\FloodHazards_v0\\Data\\ClimateAI_Sample_geoTiffs\\USv3_", theYear, "\\Location_Name_", locName , "_geoTiffs\\")
	for(thisSsp in 1:length(allSsps)) {
		typeSsp = allSsps[thisSsp]
		for(thisDefense in 1:length(allDefenses)){
			typeDefense = allDefenses[thisDefense]#"DEFENDED"
			for(thisRecur in 1:length(allRecurs)){


colCoast = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-COASTAL", "-", typeDefense, "-DEPTH-", "2050", "-", typeSsp, "-PERCENTILE50-v3.1_mean")
colPluvial = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-PLUVIAL", "-", "DEFENDED", "-DEPTH-", "2050", "-", typeSsp, "-PERCENTILE50-v3.1_mean")
colFluvial = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-FLUVIAL", "-", typeDefense, "-DEPTH-", "2050", "-", typeSsp, "-PERCENTILE50-v3.1_mean")

colCoast = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-COASTAL", "-", typeDefense, "-DEPTH-", "2020", "-PERCENTILE50-v3.1_mean")
colPluvial = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-PLUVIAL", "-", "DEFENDED", "-DEPTH-", "2020", "-PERCENTILE50-v3.1_mean")
colFluvial = paste0("FLOOD_MAP-1_3ARCSEC-NW_OFFSET-", typeRecur, "-FLUVIAL", "-", typeDefense, "-DEPTH-", "2020", "-PERCENTILE50-v3.1_mean")

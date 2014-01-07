##  		Master file - will run all watersheds at 10:30 am

setwd("/var/opt/hsa-models")
source("Functions/PriestleyTaylorEquations.r")  
# 	also includes SnowMelt - different from function in EcoHydRology
# 	updated in March 2013 - includes Energy, Albedo, and Snow Temp in input and output
source("Functions/StreamFN_UNCALIBRATED.r")					# UPDATE?   StreamFN_UNCALIBRATED.r
source("Functions/NashSutcliffe.r")				
source("Functions/GetPredictions.r")  	# Gets NOAA predictions.  	fn = forcastsNOAA()  	Updated March 2013
source("Functions/GetCoopNOAA_Data.r")  # Gets observed data.	  	fn = Past24hrData()		Updated March 2013

UseCoop = TRUE
	# Put FALSE here when collecting data before 9:30 am

cat(" 10:30 AM Run\n")
print(as.Date(Sys.time()))
cat("\n\n Owasco Lake\n")
setwd("OwascoLake")
source("../WatershedModel1030.r")

# setwd("../SalmonCreek")
# source("../WatershedModel1030.r")
	
# setwd("../FallCreek")
# source("../WatershedModel1030.r")
	
##  Just copy the same lines of code above for adding a new watershed.  
# These need to be initialized.
# In addition to copying the file structure into a new folder with the new watershed's name, 
# also run the model once to get the InputToModel.RData file, and the PrevRun.RData file



##  		Master file - will run all watersheds at 3 am
setwd("/var/opt/hsa-models")
source("Functions/PriestleyTaylorEquations.r")  
# 	also includes SnowMelt - different from function in EcoHydRology
# 	updated in March 2013 - includes Energy, Albedo, and Snow Temp in input and output
source("Functions/StreamFN_UNCALIBRATED.r")					# UPDATE?   StreamFN_UNCALIBRATED.r
source("Functions/NashSutcliffe.r")				
source("Functions/GetPredictions.r")  	# Gets NOAA predictions.  	fn = forcastsNOAA()  	Updated March 2013
source("Functions/GetCoopNOAA_Data.r")  # Gets observed data.	  	fn = Past24hrData()		Updated March 2013

UseCoop = FALSE
	# Put FALSE here when collecting data before 9:30 am

cat("\n 3 AM Run\n")
print(as.Date(Sys.time()))
cat("\n Owasco Lake\n")
setwd("OwascoLake")
source("../WatershedModel_3am.r")

# cat("\n Salmon Creek\n")
# setwd("SalmonCreek")
# source("../WatershedModel_3am.r")

# cat("\n Fall Creek\n")	
# setwd("FallCreek")
# source("../WatershedModel_3am.r")
	
	
##  Just copy the same lines of code above for adding a new watershed.  
# These need to be initialized.
# In addition to copying the file structure into a new folder with the new watershed's name, 
# also run the model once to get the InputToModel.RData file, and the PrevRun.RData file



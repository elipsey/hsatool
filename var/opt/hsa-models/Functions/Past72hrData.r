##	Gets NOAA data for the past three days:

library(XML)

####			 INPUT EXAMPLES and NOTES:
#	NOAAstn = "KITH"  #  Can also input a vector of multiple NOAA station IDs
##  To find NOAA station nearby the watershed: http://weather.uwyo.edu/models/mos/mos.html

################################################################
Past72hrData <- function(NOAAstn = c("KITH", "KSYR")) {

#  NOAA station Precip and temp data	
	ObsTable <- as.data.frame(matrix(nrow=3, ncol=1 + 3 * length(NOAAstn) ) )
	ObsTable[,1] <- c(today-2, today-1, today)

	for (i in 1:length(NOAAstn)){
		CurNOAA <- readHTMLTable(paste("http://w1.weather.gov/data/obhistory/", NOAAstn[i], ".html", sep=""), skip.rows=c(1,2,3,76,77,78), header=FALSE, stringsAsFactors=FALSE)[[4]]  
		colnames(CurNOAA) <- c("day", "time", "wind_mph", "vis_mi", "weath", "Sky", "AirTempF", "DewPt_F", "Temp6hrMax", "Temp6hrMin", "RH", "WindChill_F", "HeatIndex_F", "Press_alt_in", "Press_SL_mb", "Prec_1hr_in", "Prec_3hr_in","Prec_6hr_in")
		ObsTable[,(2+(i-1)*3)] <- c((max(na.omit(as.numeric(c(CurNOAA[49:72,9],CurNOAA[49:72,7])))) - 32)*5/9,
				(max(na.omit(as.numeric(c(CurNOAA[25:48,9],CurNOAA[25:48,7])))) - 32)*5/9,
				(max(na.omit(as.numeric(c(CurNOAA[1:24,9],CurNOAA[1:24,7])))) - 32)*5/9  ) 	# C
		ObsTable[,(3+(i-1)*3)] <- c( (min(na.omit(as.numeric(c(CurNOAA[49:72,10],CurNOAA[49:72,7])))) - 32)*5/9,   # 2 days ago
			(min(na.omit(as.numeric(c(CurNOAA[25:48,10],CurNOAA[25:48,7])))) - 32)*5/9,						# yesterday
			(min(na.omit(as.numeric(c(CurNOAA[1:24,10],CurNOAA[1:24,7])))) - 32)*5/9 )						# today
		ObsTable[,(4+(i-1)*3)] <- c( suppressWarnings(sum(na.omit(as.numeric(CurNOAA$Prec_1hr_in[49:72])))) * 25.4,
			suppressWarnings(sum(na.omit(as.numeric(CurNOAA$Prec_1hr_in[25:48])))) * 25.4,
			suppressWarnings(sum(na.omit(as.numeric(CurNOAA$Prec_1hr_in[1:24])))) * 25.4	)	# mm
	}
	names(ObsTable) <- c("Date", paste(rep(c("MaxTemp24","MinTemp24","precip24"),length(NOAAstn)), rep(NOAAstn,each=3), sep="_") )
	if (length(NOAAstn) > 1) {
	ObsTable$Tx_C <- apply(ObsTable[,grep("Max", names(ObsTable))], 1, mean) 
	ObsTable$Tn_C <- apply(ObsTable[,grep("Min", names(ObsTable))], 1, mean) 
	ObsTable$P_mm <- apply(ObsTable[,grep("precip", names(ObsTable))], 1, mean) 
	} else names(ObsTable)[2:4] <- c("Tx_C", "Tn_C", "P_mm")
	print(ObsTable)
	
## Return the P and temp values in a data frame: 	
	return(ObsTable)
}
	





















##		Owasco Model
# 		February 2012 version

## Must set path to run as a cron job 
setwd("/var/opt/hsa-model")
load("InputToModel.RData")  ##  This gives us the input values from yesterday, we will add to them today.

##################################################################################################################################
###################  Generating predicted values to run
##################   		Ithaca 				########################################################
forcastsI<-read.table("http://www.nws.noaa.gov/cgi-bin/mos/getmav.pl?sta=KITH",skip=8, fill=TRUE)[1:19,]
f1<-apply(forcastsI,1,as.character)
tis<-f1[1,]
tis[which(tis=="X/N" | tis=="N/X")]<-"MxMn"
f2<-f1[-1,]
colnames(f2)<-tis
IF<-suppressWarnings(data.frame(apply(f2,2,as.numeric)))    # We will get warning messages for making NAs here

#  Now we will pull out Max and Min temps, and predicted precipitation
MxI1<-(max(IF$MxMn[1:2])-32)*5/9
MxI2<-(max(IF$MxMn[3:4])-32)*5/9

MnI1<-(min(IF$MxMn[1:2])-32)*5/9
MnI2<-(min(IF$MxMn[3:4])-32)*5/9

PrbIP1<-max(IF$P12[1:2])
PrbIP2<-max(IF$P12[3:4])

MAVcodes<-rbind(c(0,1,2,3,4,5,6), c(0,0.09, 0.24, 0.49, 0.99, 1.99, 2.5)*25.4) 
## mm of predicted precip

# Here we chose to use predictions of precip depth only probability is >30%
if (PrbIP1>30){
	PI1<-MAVcodes[2,which(MAVcodes[1,]==max(IF$Q12[1:2]))]
} else PI1<- 0
if (PrbIP2>30){
	PI2<-MAVcodes[2,which(MAVcodes[1,]==max(IF$Q12[3:4]))]
} else PI2<- 0

###################      Syracuse Predictions 			######################################################
forcastsS<-read.table("http://www.nws.noaa.gov/cgi-bin/mos/getmav.pl?sta=KSYR",skip=8, fill=TRUE)[1:19,]
f1<-apply(forcastsS,1,as.character)
tis<-f1[1,]
tis[which(tis=="X/N"| tis=="N/X")]<-"MxMn"
f2<-f1[-1,]
colnames(f2)<-tis

SF<-suppressWarnings(data.frame(apply(f2,2,as.numeric)))    # Remove warning mess for making NAs here

MxS1<-(max(SF$MxMn[1:2])-32)*5/9
MxS2<-(max(SF$MxMn[3:4])-32)*5/9

MnS1<-(min(SF$MxMn[1:2])-32)*5/9
MnS2<-(min(SF$MxMn[3:4])-32)*5/9

PrbSP1<-max(SF$P12[1:2])
PrbSP2<-max(SF$P12[3:4])

MAVcodes<-rbind(c(0,1,2,3,4,5,6), c(0,0.09, 0.24, 0.49, 0.99, 1.99, 2.5)*25.4)  # mm

# Here we chose to use predictions of precip depth only probability is >30%
if (PrbSP1>30){
	PS1<-MAVcodes[2,which(MAVcodes[1,]==max(IF$Q12[1:2]))]
} else PS1<- 0
if (PrbSP2>30){
	PS2<-MAVcodes[2,which(MAVcodes[1,]==max(IF$Q12[3:4]))]
} else PS2<- 0

#######################  For now we are just averaging these two predictions... In the future we might want to distribute
AvMn1<-mean(MnS1,MnI1)
AvMn2<-mean(MnS2,MnI2)
AvMx1<-mean(MxS1,MxI1)
AvMx2<-mean(MxS2,MxI2)
AvP1<-mean(PS1,PI1)
AvP2<-mean(PS2,PI2)
###################################################################################################################################
source("Functions/PriestleyTaylorEquations.r")  # also include SnowMelt
source("Functions/StreamFN.r")
source("Functions/NashSutcliffe.r")
#  c(TableInfo,AuburnPrecip,CortlandPrecip, IthPrecip, FreevillePrecip, AuroraPrecip, LockePrecip, SyracusePrecip, MoraviaPrecip)
source("Functions/PrecipTempData3.r")

CoopPrecip<-as.numeric(TodaysCoopData)
if (is.na(MoraviaPrecip)){
AvP<-mean(CoopPrecip[-which(is.na(CoopPrecip))])*25.4
} else AvP <- MoraviaPrecip * 25.4  # Moravia precip else av

Tn<-(mean(na.omit(as.numeric(TodaysNOAAData[c(6,8)])))-32)*5/9  # Av of Ithaca and Syracuse Min temps in past 24 hours
Tx<-(mean(na.omit(as.numeric(TodaysNOAAData[c(5,7)])))-32)*5/9  # Ith and Syr data

# Append our input data   ## ONLY RUN THIS ONCE A DAY!  (otherwise we will have duplications...)
today<-as.Date(Sys.time())
DS<-c(DS,today)   # Date Series
tail(DS)  ## print this to make sure we have no duplications...
Pmm<-c(Pmm, AvP) # Precip, mm
TxC<-c(TxC, Tx) # Daily Max Temp, C
TnC<-c(TnC,Tn) # Daily Min Temp, C
save(list=c("DS","Pmm", "TnC", "TxC"), file="InputToModel.RData")

#  Now apply Snow Melt Model: 
SM <- SnowMelt(Date=c(DS,today+1,today+2), precip_mm=c(Pmm,AvP1,AvP2), Tmax_C=c(TxC,AvMx1,AvMx2), Tmin_C=c(TnC,AvMn1,AvMn2), lat_deg=42.38)
rsm <- SM$SnowMelt_mm + SM$Rain_mm  ## new input to stream function

## Streamflow model
TodaysMR<-StreamflowFN(rec_coef=0.08,Se_avg=200, Ia_coef=0.05,SAT_coef=0.02, 
	latitudeDegrees=42.38, albedo=0.2, aspect=0, P=rsm, 
	dateSeries=c(DS,today+1,today+2), PETcap=5, percentImpervious=0,
	Tmax=c(TxC,AvMx1,AvMx2), Tmin=c(TnC,AvMn1,AvMn2), shift=0, no_wet_class=10)

n<-nrow(TodaysMR)

##  Here are the percentage saturated values for today, tomorrow and the next day.
TodaysMR$MaxWetClass[(n-2):n]/10  # as a percentage (assuming 10% wetness classes)	
o1<-paste("var satPercent = ", TodaysMR$MaxWetClass[(n-2)]*10, " ;", sep="")
o2<-paste("var satPercentTomorrow = ", TodaysMR$MaxWetClass[(n-1)]*10, " ;", sep="")
o3<-paste("var satPercentDayAfterTomorrow = ", TodaysMR$MaxWetClass[n]*10, " ;", sep="")
o4 <- paste("var satPercentMax = ", max(TodaysMR$MaxWetClass[n]*10,TodaysMR$MaxWetClass[n-1]*10, TodaysMR$MaxWetClass[n-2]*10), " ;", sep="")
write.table(rbind(o1,o2,o3,o4),file="/var/www/sat_percent.js", quote=FALSE, col.names=FALSE, row.names=FALSE)
	
	
# write out predictions table 
Date <- vector()
Days<-c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun", "Mon")
for(i in 0:2) {
	Date <- append(Date, paste(Days[as.POSIXlt(Sys.time())$wday+1+i], ", ", strftime(Sys.time()+86400*i, format="%b %d, %Y"), sep=""))
}
Label<-c("Rain/Snow melt, mm", "Chance of precip (%)", "% watershed saturated")
Today<-c(round(rsm[(n-2)]), NA, TodaysMR$MaxWetClass[(n-2)]*10)
Tomorrow<-c(round(rsm[(n-1)]), max(PrbSP1, PrbIP1), TodaysMR$MaxWetClass[(n-1)]*10)
DayAfter<-c(round(rsm[n]), max(PrbSP2, PrbIP2), TodaysMR$MaxWetClass[(n)]*10)
library(rjson)
cat(paste("var t = ", toJSON(data.frame(Date,Label,Today, Tomorrow, DayAfter))), file="/var/www/table.jsonp")

write.csv(TodaysMR, file="OwascoModelResults2.csv")
save(TodaysMR, file="PrevRun.RData")

##  To save input
input<-data.frame(DS,Pmm,TxC,TnC)
write.csv(input, file="OwascoInput.csv")  # Back up of input data



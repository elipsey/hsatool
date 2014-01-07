# M.Todd Walter, Cornell University, 2010
# Priestly-Taylor caclulator
# This code follows (approximately): Walter et al. 2005 Journal of Hydrology 300(1-4): 65-75.
#################################################################################################
##	Returns a vector of PET values calculated by the Priestly-Taylor Method, in meters
#	Revised December 2011

####################################################################################################
##		Environmental Energy Functions
####################################################################################################
####### FUNCTIONS FOR CALCULATING SOLAR RADIATION #######

declination<-function(Jday){
# solar declination [rad]
#Jday: Julian date or day of the year [day]
return(0.4102*sin(pi*(Jday-80)/180))
}

solarangle<-function(lat,Jday){
# angle of solar inclination from horizontal at solar noon [rad]
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	# solar declination [rad]
dec<-declination(Jday)
return(asin(sin(lat)*sin(dec)+cos(lat)*cos(dec)*cos(0)))
}

solaraspect<-function(lat,Jday){
# aspect angle of sun from north at solar noon [rad]
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
return(acos((sin(0)*sin(lat) - sin(declination(Jday)))/(cos(0)*cos(lat))))
}

PotentialSolar<-function(lat,Jday){
# potential solar radiation at the edge of the atmosphere [kJ m-2 d-1]
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	# solar declination [rad]
dec<-declination(Jday)
return(117500*(acos(-tan(dec)*tan(lat))*sin(lat)*sin(dec)+cos(lat)*cos(dec)*sin(acos(tan(dec)*tan(lat))))/pi)
}

transmissivity<-function(Tx,Tn){ 	
# fraction of direct solar radiation passing through 
# the atmosphere based on the Bristow-Campbell eqn
	#Tx: maximum daily temperature [C]
	#Tn: minimum daily temperature [C]
len<-length(Tx)
if(len<30){ avDeltaT<-mean(Tx-Tn)
}else {avDeltaT<-vector(length=len)
	avDeltaT[1:14]<-mean(Tx[1:30]-Tn[1:30])
	avDeltaT[(len-14:len)]<-mean(Tx[(len-30:len)]-Tn[(len-30:len)])
	for (i in 15:(len-15)){
		avDeltaT[i]<-mean(Tx[(i-14):(i+15)]-Tn[(i-14):(i+15)])
	}
}
B<-0.036*exp(-0.154*avDeltaT)
return(0.75*(1-exp(-B*(Tx-Tn)^2.4)))
}

slopefactor<-function(lat,Jday,slope,aspect){
# slopefactor: adjusts solar radiation for land slope and aspect relative to the sun, 1=level ground
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	#slope: slope of the ground [rad]
	#aspect: ground aspect [rad from north]
return(cos(slope)-sin(slope)*1/tan(solarangle(lat,Jday))*cos(aspect-solaraspect(lat,Jday)))
}


Solar<-function(lat,Jday,Tx,Tn,albedo,forest,slope,aspect){
# solar radiation at the ground surface [kJ m-2 d-1]
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	#Tx: maximum daily temperature [C]
	#Tn: minimum daily temperature [C]
	#albedo: surface albedo or reflectivity [-]
	#forest: forest or vegeation cover [-]
	#slope: slope of the ground [rad]
	#aspect: ground aspect [rad from north]
return((1-albedo)*(1-forest)*transmissivity(Tx,Tn)*PotentialSolar(lat,Jday)*slopefactor(lat,Jday,slope,aspect))
}



####### FUNCTIONS FOR CALCULATING LONGWAVE RADIATION #######

AtmosphericEmissivity<-function(airtemp,cloudiness){
# the emissivity of the atmsophere [-]
	#airtemp: air temperature [C]
	#cloudiness: fraction of the sky covered in clouds [-]
return((0.72+0.005*airtemp)*(1-0.84*cloudiness)+0.84*cloudiness)
}


EstCloudiness<-function(Tx,Tn){
# estimates the cloudiness of the atmosphere by scaling to atmospheric transmissivity
	#Tx: maximum daily temperature [C]
	#Tn: minimum daily temperature [C]
trans<-transmissivity(Tx,Tn)
cloudiness<-1-(trans-0.15)/(0.75-0.15)
cloudiness[which(cloudiness>1)]<-1
cloudiness[which(cloudiness<0)]<-0
return(cloudiness)		
}


Longwave<-function(emissivity,temp){
# daily longwave radiation based on the Sephan-Boltzman equation [kJ m-2 d-1]
	#emissivity: [-]
	#temp: temperature of the emitting body [C]
SBconstant<-0.00000489 #[kJ m-2 K-4 d-1]
tempK<-temp+273.3 #[degrees K]
return(emissivity*SBconstant*tempK^4)
}


####### FUNCTION FOR CALCULATING NET RADIATION #######

NetRad<-function(lat, Jday, Tx, Tn, albedo, forest, slope, aspect, airtemp, cloudiness, surfemissivity, surftemp){
# daily net radiation [kJ m-2 d-1]
return(Solar(lat,Jday,Tx,Tn,albedo,forest,slope,aspect)+Longwave(AtmosphericEmissivity(airtemp,cloudiness),airtemp)-Longwave(surfemissivity,surftemp))
}


####### FUNCTION FOR CALCULATING SENSIBLE HEAT EXCHANGE #######

SensibleHeat<-function(surftemp,airtemp,wind){
# sensible heat exchange between a surface and the surrounding air [kJ m-2 d-1]
	#surftemp: surface temperature [C]
	#airtemp: average dailiy air temperature [C]
	#wind: average daily windspeed [m/s]
latentht<-2500 #latent heat of vaporization [kJ kg-1]
heatcapacity<-1.25 #approx. heat capacity of air [kJ m-3 C-1]
windfunction<-5.3*(1+wind)
return(86400*heatcapacity*(surftemp-airtemp)*windfunction/latentht)
}


####### FUNCTIONS FOR CALCULATING EVAPORATIVE HEAT EXCHANGE #######

SatVaporDensity<-function(temp){
# saturated vapor density at a given temperature
	#temp: temperature [C]
return(exp((16.78*temp-116.9)/(temp+273.3))*1/((273.15+temp)*0.4615))
}


EvapHeat<-function(surftemp,airtemp,relativehumidity,Tn,wind){
# evaporative heat exchange between a surface and the surrounding air; usually cooling [kJ m-2 d-1]
# this function is only intended for wet surfaces, i.e., it assumes the vapor density at the surface is the saturation vapor density
	#surftemp: surface temperature [C]
	#airtemp: average dailiy air temperature [C]
	#relativehumidity: relative humidity [-]
	#Tn: minimum dailiy air temperature, assumed to be the dewpoint temperature [C]
	#wind: average daily windspeed [m/s]
#NOTE: this function will use the relative humidity to esimate air vapor density if the value passed is greater than zero (0)
# If the relative humidity is less than one, we will assume the minimum daily air temperature is approsimate the dew point temp.
windfunction<-5.3*(1+wind)
if(relativehumidity>0){airvapordensity<-relativehumidity*SatVaporDensity(airtemp)}
else{airvapordensity<-SatVaporDensity(Tn)}
surfacevapordensity<-SatVaporDensity(surftemp)
return(86400*windfunction*(surfacevapordensity-airvapordensity))
}



####### FUNCTION FOR CALCULATING HEAT FROM RAIN #######

RainHeat<-function(airtemp,rain){
# temperature added to the land from heat exchange with rain (usually in the context of snowmelt) [kJ m-2 d-1]
	#airtemp: average dailiy air temperature [C]
	#rain: depth of rainfall [m]
heatcapacity<-4.2  #heat capacity of water [kJ kg-1 C-1]
waterdensity<-1000 #density of water [kg m-3]
return(heatcapacity*waterdensity*rain*(airtemp-0))
}



####### FUNCTION FOR CALCULATING HEAT FROM GROUND CONDUCTION #######

GroundHeat<-function(){
# the heat conducted to the bottom of a snowpack, assumed constant [kJ m-2 d-1]
return(173)
}



####### FUNCTIONS FOR CALCULATING ENERGY INPUTS #######

EnvirEnergy<-function(lat,Jday,Tx,Tn,wind,relativehumidity,cloudiness,albedo,forest,slope,aspect,surftemp,surfemissivity){
# the total energy exchange between the surface and the surrounding air
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	#Tx: maximum daily temperature [C]
	#Tn: minimum daily temperature [C]
	#wind: average daily windspeed [m/s]
	#relativehumidity: relative humidity; if negative, air vapor density will be approximated [-]
	#cloudiness: fraction of the sky covered in clouds,if negative, cloudiness will be approximated [-]
	#albedo: surface albedo or reflectivity [-]
	#forest: forest or vegeation cover [-]
	#slope: slope of the ground [rad]
	#aspect: ground aspect [rad from north]
	#surftemp: surface temperature [C]
	#surfemissivity: [-]
if(cloudiness<0){cloudiness<-EstCloudiness(Tx,Tn)}
airtemp<-(Tx+Tn)/2 #average daily air temperature [C]
return(Solar(lat,Jday,Tx,Tn,albedo,forest,slope,aspect)+Longwave(AtmosphericEmissivity(airtemp,cloudiness),airtemp)-Longwave(surfemissivity,surftemp)+SensibleHeat(surftemp,airtemp,wind)+EvapHeat(surftemp,airtemp,relativehumidity,Tn,wind)+RainHeat(airtemp,rain)+GroundHeat())
}


####### Psychrometric Chart Slope (delta) #######
#Delta: slope of the saturation vapor pressure vs. T curve [kPa/K]
SatVapPresSlope<-function(temp_C){
(2508.3/(temp_C+237.3)^2)*exp(17.3*temp_C/(temp_C+237.3))
}


PETfromTemp<-function(Jday, Tmax_C, Tmin_C, lat_radians, AvgT=(Tmax_C+Tmin_C)/2, albedo=0.18, TerrestEmiss=0.97, aspect=0, 
	slope=0, forest=0){
	##	forest = Forest cover, but will always be left at zero for watershed-wide processes.  Only need to put
	## 	actual percent forest when calculating eg. PET under canopy.
#	lat: in radians (degLat*pi/180)
#	albedo can be a vector or single value

if (length(Jday)!=length(Tmax_C) | length(Jday)!=length(Tmin_C)){ 
	cat("Warning, input vectors unequal length: Longer data sets truncated.\n")
	length(Jday)<-min(length(Jday), length(Tmax_C), length(Tmin_C))
	length(Tmax_C)<-min(length(Jday), length(Tmax_C), length(Tmin_C))
	length(Tmin_C)<-min(length(Jday), length(Tmax_C), length(Tmin_C))
	}

cloudiness<-EstCloudiness(Tmax_C,Tmin_C)
DailyRad<-NetRad(lat_radians,Jday,Tmax_C,Tmin_C,albedo,forest,slope,aspect,AvgT,cloudiness,TerrestEmiss,AvgT)

#####	Constants
PTconstant<-1.26 		# 	[-] Generic Priestly-Taylor constant
LatentHtEvap<-2500 		# 	[kJ/kg]
DensityWater<-1000 		# 	[kg/m3]
PsychConstant<-0.066	#	[kPa/K]

potentialET<-PTconstant*SatVapPresSlope(AvgT)*DailyRad/((SatVapPresSlope(AvgT)+PsychConstant)*(LatentHtEvap*DensityWater))
potentialET[which(potentialET<0)]<-0
potentialET[which(Tmax_C==-999 | Tmin_C==-999)]<-(-999)
return(potentialET)
}


##  SNOW MELT FUNCTION:
SnowMelt <- function (Date, precip_mm, Tmax_C, Tmin_C, lat_deg, slope = 0, aspect = 0, tempHt = 1, windHt = 2, groundAlbedo = 0.25, SurfEmissiv = 0.95, windSp = 2, forest = 0, startingSnowDepth_m = 0) 
{
    WaterDens <- 1000
    lambda <- 3.35 * 10^5
    lambdaV <- 2500
    SnowHeatCap <- 2.1
    LatHeatFreez <- 333.3
    Tav <- (Tmax_C + Tmin_C)/2
    precip_m <- precip_mm * 0.001
    R_m <- precip_m
    R_m[which(Tav < 0)] <- 0
    NewSnowDensity <- 50 + 3.4 * (Tav + 15)
    NewSnowDensity[which(NewSnowDensity < 50)] <- 50
    NewSnowWatEq <- precip_m
    NewSnowWatEq[which(Tav > 0)] <- 0
    NewSnow <- NewSnowWatEq * WaterDens/NewSnowDensity
    JDay <- strptime(Date, format = "%Y-%m-%d")$yday + 1
    lat <- lat_deg * pi/180
    rh <- log((windHt + 0.001)/0.001) * log((tempHt + 2e-04)/2e-04)/(0.41 * 0.41 * windSp * 86400)
    if (length(windSp) == 1) 
        rh <- rep(rh, length(precip_mm))
    cloudiness <- vector(length = length(precip_mm))
    cloudiness <- EstCloudiness(Tmax_C, Tmin_C)
    AE <- AtmosphericEmissivity(Tav, cloudiness)
    SnowTemp <- rep(0, length(precip_m))
    rhos <- SatVaporDensity(SnowTemp)
    rhoa <- SatVaporDensity(Tmin_C)
    SnowWaterEq <- vector(length = length(precip_mm))
    SnowWaterEq[1] <- startingSnowDepth_m/10
    TE <- rep(SurfEmissiv, length(precip_mm))
    DCoef <- rep(0, length(precip_mm))
    SnowDensity <- rep(450, length(precip_mm))
    SnowDepth <- vector(length = length(precip_mm))
    SnowDepth[1] <- startingSnowDepth_m
    SnowMelt <- rep(0, length(precip_mm))
    Albedo <- rep(groundAlbedo, length(precip_mm))
    H <- vector(length = length(precip_mm))
    E <- vector(length = length(precip_mm))
    S <- vector(length = length(precip_mm))
    La <- Longwave(AE, Tav)
    Lt <- vector(length = length(precip_mm))
    G <- 173
    P <- 4.2 * 10^3 * R_m * Tav
    Energy <- vector(length = length(precip_mm))
    Energy[1] <- 0
    for (i in 2:length(precip_m)) {
        if (NewSnow[i] > 0) {
            Albedo[i] <- 0.98 - (0.98 - Albedo[i - 1]) * exp(-4 * 
                NewSnow[i] * 10)
        }
        else if (SnowDepth[i - 1] < 0.1) {
            Albedo[i] <- max(0.25, Albedo[i - 1] + (groundAlbedo - 
                0.85)/10)
        }
        else Albedo[i] <- 0.35 - (0.35 - 0.98) * exp(-1 * (0.177 + 
            (log((-0.3 + 0.98)/(Albedo[i - 1] - 0.3)))^2.16)^0.46)
        S[i] <- Solar(lat = lat, Jday = JDay[i], Tx = Tmax_C[i], 
            Tn = Tmin_C[i], albedo = Albedo[i - 1], forest = forest, 
            aspect = aspect, slope = slope)
        if (SnowDepth[i - 1] > 0) 
            TE[i] <- 0.97
        if (SnowWaterEq[i - 1] > 0 | NewSnowWatEq[i] > 0) {
            DCoef[i] <- 6.2
            if (SnowMelt[i - 1] == 0) {
                SnowTemp[i] <- max(min(0, Tmin_C[i]), min(0, 
                  (SnowTemp[i - 1] + min(-SnowTemp[i - 1], Energy[i - 
                    1]/((SnowDensity[i - 1] * SnowDepth[i - 1] + 
                    NewSnow[i] * NewSnowDensity[i]) * SnowHeatCap * 
                    1000)))))
            }
        }
        rhos[i] <- SatVaporDensity(SnowTemp[i])
        H[i] <- 1.29 * (Tav[i] - SnowTemp[i])/rh[i]
        E[i] <- lambdaV * (rhoa[i] - rhos[i])/rh[i]
        Lt[i] <- Longwave(TE[i], SnowTemp[i])
        Energy[i] <- S[i] + La[i] - Lt[i] + H[i] + E[i] + G + 
            P[i]
        if (Energy[i] > 0) 
            k <- 2
        else k <- 1
        SnowDensity[i] <- ifelse((SnowDepth[i - 1] + NewSnow[i]) > 
            0, min(450, ((SnowDensity[i - 1] + k * 30 * (450 - 
            SnowDensity[i - 1]) * exp(-DCoef[i])) * SnowDepth[i - 
            1] + NewSnowDensity[i] * NewSnow[i])/(SnowDepth[i - 
            1] + NewSnow[i])), 450)
        SnowMelt[i] <- max(0, min((SnowWaterEq[i - 1] + NewSnowWatEq[i]), 
            (Energy[i] - SnowHeatCap * (SnowWaterEq[i - 1] + 
                NewSnowWatEq[i]) * WaterDens * (0 - SnowTemp[i]))/(LatHeatFreez * 
                WaterDens)))
        SnowDepth[i] <- (SnowWaterEq[i - 1] + NewSnowWatEq[i] - SnowMelt[i]) * WaterDens/SnowDensity[i]
        SnowWaterEq[i] <- max(0, SnowWaterEq[i - 1] - SnowMelt[i] + NewSnowWatEq[i])
    }
    Results <- data.frame(Date, Tmax_C, Tmin_C, R_m * 1000, NewSnowWatEq * 1000, SnowMelt * 1000, NewSnow, SnowDepth)
    colnames(Results) <- c("Date", "MaxT_C", "MinT_C", "Rain_mm", 
        "SnowfallWatEq_mm", "SnowMelt_mm", "NewSnow_m", "SnowDepth_m")
    return(Results)
}







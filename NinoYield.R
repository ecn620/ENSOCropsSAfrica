#This code will import the data we need for Lesotho and South Africa

#Import rain by itself ####
require(raster)
require(lubridate)
require(dplyr)

#Extract the country shapefiles first
lesotho<-getData('GADM', country='LSO', level=1)
safrica<-getData("GADM",country='ZAF',level=1)

plot(lesotho)

#Also need to define this function to 
#Extract the district of interest and take spatial average
extractaverage<-function(climvar,districtshp){
  cropvar<-raster::mask(climvar,districtshp)
  meanvar<-apply(as.array(cropvar),MARGIN=3,FUN=mean,na.rm=T)
}

#Now make a wrapper function that opens the precip file and averages
#by admin 1 region
extractprecipdata<-function(filenameprecip,countryshape){
  # erapreciples<-brick(paste("/cluster/tufts/coughlanlab/ecough02/ERA5/Precip/",
  #                           "les_entire_dailyeraTP_1980_2022.nc",sep=""))
  erapreciples<-brick(paste("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/CropsinR/",
                            filenameprecip,sep=""))
  #Convert from m to mm
  erapreciples<-erapreciples*1000
  
  #Need to clip the first and last value because are incomplete seasons
  erapreciples<-erapreciples[[2:42]]
  
  #Each district can be pulled out itself
  districtshapes<-lapply(countryshape$NAME_1,
                                FUN=function(x){subset(countryshape,NAME_1==x)})
  names(districtshapes)<-countryshape$NAME_1
  
  #Extract dates from the brick
  dateseraland<-as.Date(paste(substr(names(erapreciples),2,5),
                              substr(names(erapreciples),6,7),
                              substr(names(erapreciples),8,9),sep="-"))
  
  #Subset by the district shape and then average result by district
  districtprecip<-sapply(districtshapes,
                         FUN=function(x){extractaverage(erapreciples,x)})
  colnames(districtprecip)<-countryshape$NAME_1
  districtprecip<-as.data.frame(districtprecip)
  districtprecip$yearindex<-year(dateseraland)
  districtprecip$Date<-dateseraland
  return(districtprecip)
}


erapreciples<-brick(paste("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/CropsinR/",
                          "les_entire_dailyeraTP_1980_2022_DJFsum.nc",sep=""))
lesotho$NAME_1
plot(lesotho$NAME_1)

dim(erapreciples)
plot(erapreciples[[1]])
maps::map("world",add=T)


#Get total precip Lesotho for NDJ season
onlyNDJtotalprecip_lesotho<-extractprecipdata("les_entire_dailyeraTP_1980_2022_DJFsum.nc",
                                              lesotho)
head(onlyNDJtotalprecip_lesotho)

#South Africa
onlyNDJtotalprecip_saf<-extractprecipdata("saf_entire_dailyeraTP_1980_2022_DJFsum.nc",
                                          safrica)
head(onlyNDJtotalprecip_saf)

#Get Lesotho ON only precip
#would need to adjust function hang on for this

#Get ON precip for Lesotho country itself
erapreciples<-brick(paste("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/CropsinR/",
                          "saf_entire_dailyeraTP_1980_2022_ONsum.nc",sep=""))
#Convert from m to mm
erapreciples<-erapreciples*1000

#Get year of data
eraprecipyear<-as.numeric(substr(names(erapreciples),2,5))
eraprecipyear

#Add 1 to match year of harvest because ON date is year before
eraprecipyear<-eraprecipyear+1

onlyONtotalprecip_lescountry<-extractaverage(erapreciples,lesotho)
onlyONtotalprecip_lescountry<-
  data.frame(year=eraprecipyear,ONrain=onlyONtotalprecip_lescountry)
head(onlyONtotalprecip_lescountry)
tail(onlyONtotalprecip_lescountry)

#Let's get rid of 2023
onlyONtotalprecip_lescountry<-onlyONtotalprecip_lescountry[1:41,]

#Add a column for whether the ON season was unusually dry
onlyONtotalprecip_lescountry$drystart<-
  onlyONtotalprecip_lescountry$ONrain < 
  quantile(onlyONtotalprecip_lescountry$ONrain,0.33)
head(onlyONtotalprecip_lescountry)

#Repeat this for South Africa
extractprecipdataON<-function(filenameprecip,countryshape){
  erapreciples<-brick(paste("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/CropsinR/",
                            filenameprecip,sep=""))
  #Convert from m to mm
  erapreciples<-erapreciples*1000
  
  #Get year of data
  eraprecipyear<-as.numeric(substr(names(erapreciples),2,5))
  eraprecipyear
  
  #Add 1 to match year of harvest because ON date is year before
  eraprecipyear<-eraprecipyear+1
  
  #Need to clip the last value because not needed
  erapreciples<-erapreciples[[1:41]]
  
  #Each district can be pulled out itself
  districtshapes<-lapply(countryshape$NAME_1,
                         FUN=function(x){subset(countryshape,NAME_1==x)})
  names(districtshapes)<-countryshape$NAME_1
  
  #Subset by the district shape and then average result by district
  districtprecip<-sapply(districtshapes,
                         FUN=function(x){extractaverage(erapreciples,x)})
  colnames(districtprecip)<-countryshape$NAME_1
  districtprecip<-as.data.frame(districtprecip)
  districtprecip$yearindex<-eraprecipyear[1:41]
  return(districtprecip)
}
#South Africa
onlyONtotalprecip_saf<-extractprecipdataON(
  "saf_entire_dailyeraTP_1980_2022_ONsum.nc",safrica)
head(onlyONtotalprecip_saf)
tail(onlyONtotalprecip_saf)


#Now, open the rain for the entire growing season of the crop
#Nov to Jun
erapreciples<-brick(paste("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/CropsinR/",
                          "saf_entire_dailyeraTP_1980_2022_NovtoJunsum.nc",sep=""))
#Convert from m to mm
erapreciples<-erapreciples*1000

#Get year of data
eraprecipyear<-as.numeric(substr(names(erapreciples),2,5))
eraprecipyear
#don't need to add 1 because year in year of harvest

#Extract the data
onlyNtoJtotalprecip_lescountry<-extractaverage(erapreciples,lesotho)
onlyNtoJtotalprecip_lescountry<-data.frame(year=eraprecipyear,croprain=onlyNtoJtotalprecip_lescountry)
head(onlyNtoJtotalprecip_lescountry)
tail(onlyNtoJtotalprecip_lescountry)

#Let's get rid of last date because partial season
onlyNtoJtotalprecip_lescountry<-onlyNtoJtotalprecip_lescountry[1:41,]

#add columns for BN
onlyNtoJtotalprecip_lescountry$BN<-
  onlyNtoJtotalprecip_lescountry$croprain<
  quantile(onlyNtoJtotalprecip_lescountry$croprain,0.33)
onlyNtoJtotalprecip_lescountry$ABN<-
  onlyNtoJtotalprecip_lescountry$croprain>
  quantile(onlyNtoJtotalprecip_lescountry$croprain,0.66)
onlyNtoJtotalprecip_lescountry$ABN[
  which(onlyNtoJtotalprecip_lescountry$BN==TRUE)]<- -1
head(onlyNtoJtotalprecip_lescountry)

plot(erapreciples[[1]])

#Repeat for each South Africa district

#Each district can be pulled out itself
districtshapes_saf<-lapply(safrica$NAME_1,
                       FUN=function(x){subset(safrica,NAME_1==x)})
names(districtshapes_saf)<-safrica$NAME_1
districtshapes_saf

#Subset by the district shape and then average result by district
onlyNtoJtotalprecip_saf<-sapply(districtshapes_saf,
                       FUN=function(x){extractaverage(erapreciples,x)})
colnames(onlyNtoJtotalprecip_saf)<-safrica$NAME_1


#Here repeat by district
onlyNtoJtotalprecip_saf<-
  data.frame(year=eraprecipyear,onlyNtoJtotalprecip_saf)
  head(onlyNtoJtotalprecip_saf)
  tail(onlyNtoJtotalprecip_saf)
  
  #Let's get rid of last date because partial season
  onlyNtoJtotalprecip_saf<-onlyNtoJtotalprecip_saf[1:41,]
  
#Make it into long format
require(tidyr)
  onlyNtoJtotalprecip_saf %>% gather(key="year",value="precip")
  onlyNtoJtotalprecip_saf_long<-onlyNtoJtotalprecip_saf %>% 
    pivot_longer(!year,cols_vary="slowest")
  names(onlyNtoJtotalprecip_saf_long)<-c("year","admin_1","croprain")
names(onlyNtoJtotalprecip_saf_long)

#Now add columns for BN
onlyNtoJtotalprecip_saf_long$BN<-NA
onlyNtoJtotalprecip_saf_long$ABN<-NA
#
onlyNtoJtotalprecip_saf_long<-as.data.frame(onlyNtoJtotalprecip_saf_long)
#Make BN column for all data using full 40 years
unique(onlyNtoJtotalprecip_saf_long$admin_1)
for (i in unique(onlyNtoJtotalprecip_saf_long$admin_1)){
  districtname<-i
  subset<-onlyNtoJtotalprecip_saf_long[
    which(onlyNtoJtotalprecip_saf_long$admin_1==districtname),3]
  onlyNtoJtotalprecip_saf_long[
    which(onlyNtoJtotalprecip_saf_long$admin_1==districtname),4]<-
    (subset < quantile(subset,0.33))
#Now do the above-below column
  onlyNtoJtotalprecip_saf_long[
    which(onlyNtoJtotalprecip_saf_long$admin_1==districtname),5]<-
    (subset > quantile(subset,0.66)) #add 1 for above normal
}
#Now add the -1 for below-normal into the ABN column
onlyNtoJtotalprecip_saf_long[which(
  onlyNtoJtotalprecip_saf_long$BN==T),5]<- -1

head(onlyNtoJtotalprecip_saf_long)  
tail(onlyNtoJtotalprecip_saf_long)
dim(onlyNtoJtotalprecip_saf_long)
sum(onlyNtoJtotalprecip_saf_long$BN) #Cheks out to be 1/3

#Now, check it's looking like the Lesotho one - good
head(onlyNtoJtotalprecip_lescountry)



#What about ENSO?###########
require(readxl)
NOAA_relativeONI <- read_excel("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/NOAA_relativeONI.xlsx")
#NOAA_relativeONI <- read_excel("/cluster/tufts/coughlanlab/ecough02/CropModel/NOAA_relativeONI.xlsx")
NOAA_relativeONI<-as.data.frame(NOAA_relativeONI)
NOAA_relativeONI
hist(NOAA_relativeONI$ANOM)
ensowinter_DJF<-NOAA_relativeONI[which(NOAA_relativeONI=="DJF"),]
ensowinter_DJF
ensowinter_NDJ<-NOAA_relativeONI[which(NOAA_relativeONI=="NDJ"),]
ensowinter_OND<-NOAA_relativeONI[which(NOAA_relativeONI=="OND"),]
ensowinter_NDJ$YR 
#need to add 1 because of harvest in next year
ensowinter_NDJ$YR<-ensowinter_NDJ$YR+1
ensowinter_OND$YR<-ensowinter_OND$YR+1
ensowinter_NDJ$YR
ensowinter_OND$YR

# #Do the correlation
# cor.test(ensowinter_DJF$ANOM,
#          filter(model.SorLeribe,Crop=="Sorghum" & Location=="Leribe")$Yield)
# cor.test(ensooverlap$ANOM,
#          filter(model.SorLeribe,Crop=="Maize" & Location=="Leribe")$Yield)
# plot(ensooverlap$ANOM,filter(model.SorLeribe,Crop=="Sorghum")$Yield)
# text(ensooverlap$ANOM,filter(model.SorLeribe,Crop=="Sorghum")$Yield,
#      labels=filter(model.SorLeribe,Crop=="Sorghum")$Year)
# 
# #Quthing
# cor.test(ensooverlap$ANOM,
#          filter(model.SorLeribe,Crop=="Sorghum" & Location=="Quthing")$Yield)
# cor.test(ensooverlap$ANOM,
#          filter(model.SorLeribe,Crop=="Maize" & Location=="Quthing")$Yield)
# 
# #Great to see this - 1998 was big ENSO year but didn't result in drought, so 
# #wouldn't expect to see low yields that year - checks out perfectly


#IRI Forecast Open#########
#Now use IRI seasonal forecast: prob of below-normal rainfall
require(ncdf4)
#a<-nc_open("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/CropsinR/IRIworldoldSepLead1.nc")
#a<-nc_open("/cluster/tufts/coughlanlab/ecough02/CropModel/IRIworldoldSepLead1.nc")
a<-nc_open("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/CropsinR/IRIworldoldOctLead1.nc")

irihist<-ncvar_get(a)
#The problem is that the fcst from Oct is actually Sep 
#In the first few years ugh, so need ot figure out timeline
timeoldfcst<-ncvar_get(a,"F")
#Round this down because the half month is irrelevant
timeoldfcst<-floor(timeoldfcst)
timeoldfcst
#This is months since 1960-01-01
origin<-as.Date("1960-01-01")

dim(irihist)
image(irihist[1,,,1])
#Divide up the three forecast types so we have maps by time of issue (3 dims)
irihist_below<-irihist[1,,,]
irihist_near<-irihist[2,,,]
irihist_above<-irihist[3,,,]
#image(irihist_below[,,1])

#Make a function that converts an array into a brick (maps layered by time)
convertfcsttobrick<-function(irifcstdata,oldnew){
  irifcstdata<-aperm(irifcstdata,c(2,1,3))
  # Create raster brick
  if(oldnew=="old"){
    # Define extent
    extent <- extent(c(-180,180, -90, 90))
    brick_data<-brick(irifcstdata, xmn = 0, xmx = 360, ymn = -90, ymx = 90)
  }else if (oldnew=="new"){
    # Define extent
    extent <- extent(c(0, 360, -90, 90))
    brick_data <- brick(irifcstdata[181:0,,], xmn = 0, xmx = 360, ymn = -90, ymx = 90)
  }
  # Set extent
  extent(brick_data) <- extent
  return(brick_data)
}

#Convert to raster brick
irihist_below_brick<-convertfcsttobrick(irihist_below,"old")
plot(irihist_below_brick[[1]])

irihist_near_brick<-convertfcsttobrick(irihist_near,"old")
irihist_above_brick<-convertfcsttobrick(irihist_above,"old")

#And increase the number of cells so the overlay with small districts will work
irihist_below_brick<-disaggregate(irihist_below_brick,fact=10)
irihist_near_brick<-disaggregate(irihist_near_brick,fact=10)
irihist_above_brick<-disaggregate(irihist_above_brick,fact=10)


#open more recent years data
#In IRIDL, selected lead 1 and month of issue = Sep
b<-nc_open("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/CropsinR/IRIworldrecentSepLead1.nc")
#b<-nc_open("/cluster/tufts/coughlanlab/ecough02/CropModel/IRIworldrecentSepLead1.nc")
irirecent<-ncvar_get(b)
irirecent_below<-irirecent[,,1,]
irirecent_near<-irirecent[,,2,]
irirecent_above<-irirecent[,,3,]

irirecent_below_brick<-convertfcsttobrick(irirecent_below,"new")
irirecent_near_brick<-convertfcsttobrick(irirecent_near,"new")
irirecent_above_brick<-convertfcsttobrick(irirecent_above,"new")

#And increase the number of cells so the overlay with small districts will work
irirecent_below_brick<-disaggregate(irirecent_below_brick,fact=10)
irirecent_near_brick<-disaggregate(irirecent_near_brick,fact=10)
irirecent_above_brick<-disaggregate(irirecent_above_brick,fact=10)

plot(irihist_below_brick[[1]])
plot(irirecent_below_brick[[1]])


#Now mask for region of interest and take average
maskregionfcstaverage<-function(districtshp){
  hist_below<-extractaverage(irihist_below_brick,districtshp)
  hist_near<-extractaverage(irihist_near_brick,districtshp)
  hist_above<-extractaverage(irihist_above_brick,districtshp)
  recent_below<-extractaverage(irirecent_below_brick,districtshp)
  recent_near<-extractaverage(irirecent_near_brick,districtshp)
  recent_above<-extractaverage(irirecent_above_brick,districtshp)
  below<-c(hist_below,recent_below)
  near<-c(hist_near,recent_near)
  above<-c(hist_above,recent_above)
  irifcsts<-data.frame(below,near,above)
  return(irifcsts)
}

#Make a list of shapefiles per country in global environment
#Each district can be pulled out itself
districtshp_les<-lapply(lesotho$NAME_1,
                    FUN=function(x){subset(lesotho,NAME_1==x)})
names(districtshp_les)<-lesotho$NAME_1
districtshp_saf<-lapply(safrica$NAME_1,
                        FUN=function(x){subset(safrica,NAME_1==x)})
names(districtshp_saf)<-safrica$NAME_1


#Extract forecast for each district
#This is the problem code that causes issues with memory
irifcstperdistrict_lesotho<-lapply(districtshp_les,maskregionfcstaverage)
irifcstperdistrict_saf<-lapply(districtshp_saf,maskregionfcstaverage)

#Checking out the data to make sure it is what I expected
class(irifcstperdistrict_saf[[1]])
length(irifcstperdistrict_saf)
dim(irifcstperdistrict_saf[[1]])

#Now add a vector for the year of harvest, which is year of issue + 1
addyearofharvest<-function(irifcstperdistrict){
  lapply(c(1:length(irifcstperdistrict)),
         FUN = function(x){
           #Add years
           irifcstperdistrict[[x]]$year=c(1998:2024)
           #Get rid of last year because no 2024 harvest yet
           irifcstperdistrict[[x]]<-irifcstperdistrict[[x]][1:25,]
           return(irifcstperdistrict[[x]])})
}

irifcstperdistrict_lesotho<-addyearofharvest(irifcstperdistrict_lesotho)
irifcstperdistrict_moz<-addyearofharvest(irifcstperdistrict_moz)
irifcstperdistrict_zim<-addyearofharvest(irifcstperdistrict_zim)
irifcstperdistrict_zam<-addyearofharvest(irifcstperdistrict_zam)
irifcstperdistrict_saf<-addyearofharvest(irifcstperdistrict_saf)
names(irifcstperdistrict_lesotho)<-lesotho$NAME_1
names(irifcstperdistrict_zim)<-zimbabwe$NAME_1
names(irifcstperdistrict_zam)<-zambia$NAME_1
#Typo in Moz names!
mozambique$NAME_1
mozambique$NAME_1[8]<-"Niassa"
names(irifcstperdistrict_moz)<-mozambique$NAME_1
names(irifcstperdistrict_saf)<-safrica$NAME_1
names(irifcstperdistrict_lesotho)
head(irifcstperdistrict_saf)

#Now, get obs data of whether below/near/above normal
head(onlyNDJtotalprecip_lesotho)
onlyNDJtotalprecip_lesotho$yearindex
onlyNDJtotalprecip_saf$yearindex

#Function to classify years into below-normal and above-normal thresholds
getobspercentiles<-function(precipinput,onlyNDJtotalprecip){
  percentilethresh<-quantile(precipinput,c(0.33, 0.66))
  obspercents<-as.data.frame(precipinput<percentilethresh[1])
  names(obspercents)<-"below"
  obspercents$near<-precipinput>percentilethresh[1] &
    precipinput<percentilethresh[2]
  obspercents$above<-precipinput>percentilethresh[2]
  obspercents$yearindex<-onlyNDJtotalprecip$yearindex
  return(obspercents)
}


#Then, do briar score for below-normal rainfall each year
#We have all the forecasts
irifcstperdistrict_lesotho
#And all the verifications
typeof(districtobspercentiles)

districtnumber<-1
#Do this for each district - here is function to make the skillscores
calculateskill<-function(districtnumber,irifcstperdistrict,
                         districtobspercentiles,onlyNDJtotalprecip){
  irifcst.frame<-irifcstperdistrict[[districtnumber]]
  obspercentiles<-districtobspercentiles[[districtnumber]]

  #First make timeseries match
  irifcst.frame$actbelow<-obspercentiles$below[which(obspercentiles$yearindex
                                                     %in% irifcst.frame$year)]
  irifcst.frame$actnear<-obspercentiles$near[which(obspercentiles$yearindex
                                                   %in% irifcst.frame$year)]
  irifcst.frame$actabove<-obspercentiles$above[which(obspercentiles$yearindex
                                                     %in% irifcst.frame$year)]
  
  #Now calculate briar score
  #Score = (probability of belownormal - binary yes/no was it below normal) ^2
  irifcst.frame$briarbelow<-(irifcst.frame$below/100-irifcst.frame$actbelow)^2
  irifcst.frame$briarnear<-(irifcst.frame$near/100-irifcst.frame$actnear)^2
  irifcst.frame$briarabove<-(irifcst.frame$above/100-irifcst.frame$actabove)^2
  
  #Now, sum the briar scores for each forecast!!
  irifcst.frame$briartotal<-irifcst.frame$briarbelow+
    irifcst.frame$briarnear+
    irifcst.frame$briarabove
  
  #And add observed precip
  irifcst.frame$prec<-onlyNDJtotalprecip[[districtnumber]][which(onlyNDJtotalprecip$yearindex
                                                                   %in% irifcst.frame$year)]
  return(irifcst.frame)
}



#Make a wrapper function for all this
makeiriframething<-function(onlyNDJtotalprecip,countryname,irifcstperdistrict){
  districtobspercentiles<-lapply(c(1:(dim(onlyNDJtotalprecip)[2]-2)),
                                 FUN=function(x){
                                   getobspercentiles(onlyNDJtotalprecip[[x]],
                                                     onlyNDJtotalprecip)})
  names(districtobspercentiles)<-countryname$NAME_1
  
  #Now apply the skill calc function
  districtskillscores<-lapply(c(1:length(irifcstperdistrict)),calculateskill,
                              irifcstperdistrict=irifcstperdistrict,
                              districtobspercentiles=districtobspercentiles,
                              onlyNDJtotalprecip=onlyNDJtotalprecip)
  names(districtskillscores)<-countryname$NAME_1
  return(districtskillscores)
}

#Apply the wrapper function to each country
districtskillscores_les<-makeiriframething(onlyNDJtotalprecip_lesotho,
                                           lesotho,irifcstperdistrict_lesotho)
districtskillscores_les

districtskillscores_saf<-makeiriframething(onlyNDJtotalprecip_saf,
                                           safrica,
                                           irifcstperdistrict_saf)
districtskillscores_saf
head(onlyNDJtotalprecip_lesotho)

#ECMWF Forecast Open ###### 
#__________________________________________________________________________
ecmwffcst<-brick("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/CropsinR/seas5southernafricainit10.ensmean.NDJ.nc")
dim(ecmwffcst)
plot(ecmwffcst[[which(yearsecmwf==1998)]])
#Convert from m/s to mm/day: multiply by seconds in day, then 1000 to go from m to mm
ecmwffcst<-ecmwffcst*86400*1000
names(ecmwffcst)
yearsecmwf<-substr(names(ecmwffcst),2,5)
yearsecmwf
#Add 1 for harvest year
yearsecmwf<-as.numeric(yearsecmwf)+1

#Make higher resolution so the shapefiles can be located
ecmwffcst_large<-disaggregate(ecmwffcst,fact=10)


#get all the ecmwf forecast totals for each country
test<-extractaverage(ecmwffcst_large,districtshp_les[[1]])
test
#Extract forecast for each district
ecmwffcstperdistrict_les<-lapply(districtshp_les,FUN=function(x){
  extractaverage(ecmwffcst_large,x)})
names(ecmwffcstperdistrict_les)<-names(districtshp_les)
ecmwffcstperdistrict_saf<-lapply(districtshp_saf,FUN=function(x){
  extractaverage(ecmwffcst_large,x)})
ecmwffcstperdistrict_saf
names(ecmwffcstperdistrict_saf)<-names(districtshp_saf)
names(ecmwffcstperdistrict_saf)

#Create anomalies and calculate error
ecmwfverification<-function(districtnum,fcstin,precipin){
  obsprecip<-precipin[,districtnum]
  obsyears<-precipin$yearindex
  fcstprecip<-fcstin[[districtnum]]
  fcstyears<-yearsecmwf
  #Shorten the fcst data because one year too long
  fcstprecip<-fcstprecip[which(obsyears %in% yearsecmwf)]
  #Make standardized anomalies of both
  fcstprecipanom<-(fcstprecip-mean(fcstprecip))/sd(fcstprecip)
  obsprecipanom<-(obsprecip-mean(obsprecip))/sd(obsprecip)
  #Calculate errors
  error<-fcstprecipanom-obsprecipanom
  sqerror<-error^2
  return(data.frame(obsyears,error,sqerror))
}

#apply this to all districts
ecmwfverification_saf<-lapply(c(1:length(safrica$NAME_1)),ecmwfverification,
                              ecmwffcstperdistrict_saf,onlyNDJtotalprecip_saf)
names(ecmwfverification_saf)<-safrica$NAME_1
ecmwfverification_les<-lapply(c(1:length(lesotho$NAME_1)),ecmwfverification,
                              ecmwffcstperdistrict_les,onlyNDJtotalprecip_lesotho)
names(ecmwfverification_les)<-lesotho$NAME_1
#Add ECMWF verification info to the district skill scores
for (i in c(1:length(safrica$NAME_1))){
  districtskillscores_saf[[i]]$error<-
    ecmwfverification_saf[[i]]$error[which(ecmwfverification_saf[[i]]$obsyears 
                                           %in% districtskillscores_saf[[i]]$year)]
}
for (i in c(1:length(lesotho$NAME_1))){
  districtskillscores_les[[i]]$error<-
    ecmwfverification_les[[i]]$error[which(ecmwfverification_les[[i]]$obsyears 
                                           %in% districtskillscores_les[[i]]$year)]
}


#Area planted ####
#__________________________________________________________________________

#Make a wrapper function for this too...
openplantingyielddata<-function(filenamefews){
  #For Mozambique and other countries:
  allcropdata<-read.csv(paste("C:/Users/ecough02/Box/AAAA/06 Research content/Lesotho/",
                              filenamefews,sep=""))
  allcropdata<-as.data.frame(allcropdata)
  return(allcropdata)
}

allcropdata_les<-openplantingyielddata("FEWS Lesotho Area Yield Maize Sorghum.csv")
allcropdata_saf<-openplantingyielddata("FEWS South Africa Area Yield Maize Sorghum.csv")

head(allcropdata_saf)

#Fix the name of maize to be consistent
fixmaizefunction<-function(dataset){
  dataset$product[which(dataset$product=="Maize Grain (White)")]<-
    "Maize"
  dataset$product[which(dataset$product=="Maize Grain (Yellow)")]<-
    "Maize"
  dataset$product[which(dataset$product=="Maize (Corn)")]<-
    "Maize"
  dataset$product[which(dataset$product=="Maize, white")]<-
    "Maize"
  return(dataset)
}

allcropdata_les<-fixmaizefunction(allcropdata_les)
allcropdata_saf<-fixmaizefunction(allcropdata_saf)


#Select columns of interest and add a column for year, get rid of winter
#In Lesotho data - not needed in Mozambique
areaplanted.df_les<- allcropdata_les %>%
   filter(season_name == "Summer")
areaplanted.df_les<-allcropdata_les %>%
  filter(indicator == "Area Planted")#for Mozambique

#In SAfrica dataset is labeled differently
allcropdata_saf$indicator[which(allcropdata_saf$indicator=="area")]<-
  "Area Planted"
areaplanted.df_saf<-allcropdata_saf %>%
  filter(indicator == "Area Planted")
areaplanted.df_saf

#Add year and select only data we want
areaplanted.df_les$year<-year(as.Date(areaplanted.df_les$period_date))
areaplanted.df_les<-areaplanted.df_les %>%
  select(admin_1,crop_production_system,value,value_per_capita,product,
         year)
head(areaplanted.df_les)
head(areaplanted.df_saf)
areaplanted.df_saf<-areaplanted.df_saf %>%
  select(admin_1,crop_production_system,value,product,
         harvest_year)
head(areaplanted.df_les)
head(areaplanted.df_saf)
#rename "harvest year" to year
names(areaplanted.df_saf)[5]<-"year"
#add a blank column for value_per_capita
areaplanted.df_saf$value_per_capita<-NA

#And in SAfrica - all commercial I think
unique(areaplanted.df_saf$crop_production_system)
areaplanted.sum_saf<-areaplanted.df_saf
names(areaplanted.sum_saf)[c(3,6)]<-c("area","areapercapita")
head(areaplanted.sum_saf)

#Repeat for yield
#Lesotho Zambia Zimbabwe
 yield.df_les<- allcropdata_les %>%
   filter(season_name == "Summer")
 yield.df_les<-yield.df_les %>%
   filter(indicator == "Yield")
 head(yield.df_les)
 yield.df_les$year<-year(as.Date(yield.df_les$period_date))
 #Select only columns we want
 yield.df_les<-yield.df_les %>%
   select(admin_1,crop_production_system,value,product,
          year)
#For South Africa
#Change case of letters
allcropdata_saf$indicator[which(allcropdata_saf$indicator=="yield")]<-
  "Yield"
yield.df_saf<-allcropdata_saf %>%
  filter(indicator == "Yield")#for South Africa
yield.df_saf<-yield.df_saf %>%
  select(admin_1,crop_production_system,value,product,
         harvest_year)
names(yield.df_saf)[5]<-"year"
head(yield.df_saf)
unique(yield.df_saf$year)


#Because no need to average production regions, then do this
names(yield.df_saf)
names(yield.df_saf)[3]<-"yield"
yieldaverage_saf<-yield.df_saf

#Now sum the production system regions - needed for Lesotho & others
names(areaplanted.df_les)
areaplanted.sum_les<-areaplanted.df_les %>%
  group_by(admin_1,year,product) %>%
  summarize(area=sum(value,na.rm=T),areapercapita=sum(value_per_capita, na.rm=T))
tail(areaplanted.sum_les)

#For yield, average the production system regions - Lesotho Zim and Zam
yieldaverage_les<-yield.df_les %>%
  group_by(admin_1,year,product) %>%
  summarize(yield=mean(value,na.rm=T))
tail(yieldaverage_les)


###Correlations #######
#___________________________________________________________________

#Correlate Sorghum and Maize to above and below per region
#Here is a list of all districts and maize sorghum
admin.product_les<-unique(areaplanted.sum_les %>% 
  dplyr::ungroup() %>% 
  select(admin_1,product))
admin.product_les

#For South Africa
admin.product_saf<-unique(areaplanted.sum_saf %>% 
                            dplyr::ungroup() %>% 
                            select(admin_1,product))
admin.product_saf


#Now let's plot the data over time
head(areaplanted.sum_les)
areaplanted.sum_les

plotareaovertimecountry<-function(datacountry,title){
  ggplot(datacountry,# %>% filter(product=="Maize"), 
         aes(x=year, y=area,group=admin_1,color=admin_1)) +
    geom_line()+
    theme_minimal()+
    labs(title=title,
         x ="", y = "Area",color="Admin 1 Region")+
    facet_grid(. ~ product)
}
plotareapcovertimecountry<-function(datacountry,title){
  ggplot(datacountry,# %>% filter(product=="Maize"), 
         aes(x=year, y=areapercapita,group=admin_1,color=admin_1)) +
    geom_line()+
    theme_minimal()+
    labs(title=title,
         x ="", y = "Area",color="Admin 1 Region")+
    facet_grid(. ~ product)
}

require(ggplot2)
require(ggpubr)

#Arrange in a three-tier plot
#Publication plot!
a<-
  plotareaovertimecountry(areaplanted.sum_les,"Planted Area Lesotho")
b<-
  ggplot(areaplanted.sum_saf %>% filter(admin_1 %in% c(
  "Free State", "Mpumalanga", "North West", "Gauteng", "Limpopo")), 
       aes(x=year, y=area,group=admin_1,color=admin_1)) +
  geom_line()+
  theme_minimal()+
  labs(title="Planted Area South Africa",
       x ="", y = "Area",color="")+
  facet_grid(. ~ product)

c<-ggplot(yieldaverage_les,# %>% filter(product=="Maize"), 
       aes(x=year, y=yield,group=admin_1,color=admin_1)) +
  geom_line()+
  theme_minimal()+
  labs(title="Yield Lesotho",
       x ="", y = "Yield",color="")+
  facet_grid(. ~ product)
d<-ggplot(yieldaverage_saf%>% filter(admin_1 %in% c(
  "Free State", "Mpumalanga", "North West", "Gauteng", "Limpopo")), 
          aes(x=year, y=yield,group=admin_1,color=admin_1)) +
  geom_line()+
  theme_minimal()+
  labs(title="Yield South Africa",
       x ="", y = "Yield",color="")+
  facet_grid(. ~ product)

ggarrange(a,b,c,d,labels="auto",ncol=1,nrow=4)



#What happens if we detrend the data?
head(areaplanted.sum_les)
require(pracma)
detrend(areaplanted.sum_les$area)
test<-areaplanted.sum_les %>%
  filter(product=="Maize")%>%
  filter(admin_1=="Berea")
testdetrend<-detrend(test$area)
plot(test$area,type="l")
par(new=T)
plot(testdetrend,col="red",type="l")
head(districtskillscores_les)

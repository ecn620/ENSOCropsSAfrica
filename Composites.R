#File to run the correlations and composites

#Move correlations here at some point

#Here are the datasets we're working with
areaplanted.sum_les
yieldaverage_les
ensowinter_OND
onlyNDJtotalprecip_lesotho
head(districtskillscores_les)

#Question: does area planted reduce when people know
#ENSO is happening, or only if season starts dry?
#Lesotho maize planting is OND
#Lesotho sorghum planting is ND
#South Africa maize planting (in east, where it mostly is?) =ON
#South Africa sorghum is DJ
#Everything harvests approximately in June

#Composites for area planted for Lesotho: based on ENSO state
#and whether or not it was dry at the time of planting
head(ensowinter_OND)
#put 1 for El Nino
hist(ensowinter_OND$ANOM)
ensowinter_OND$state<-ensowinter_OND$ANOM > 1
#Put -1 for La Nina
ensowinter_OND$state[which(ensowinter_OND$ANOM < -1)]<- -1
head(ensowinter_OND)

#Yield ENSO
head(areaplanted.sum_les)
plantedyearsles<-unique(areaplanted.sum_les$year)
head(onlyNDJtotalprecip_lesotho)
unique(onlyNDJtotalprecip_lesotho$yearindex)
plantedyearsles

#function(districtname,cropname,areaplanted,precip,ensodata){
cropname<-"Maize"
districtname<-"Berea"  
#Let's make sum total of area planted in Lesotho
head(areaplanted.sum_les)
areaplanted_countryles<-areaplanted.sum_les %>%
  group_by(year,product) %>%
  summarize(areatotal=sum(area))
areaplanted_countryles
areaplanted_countryles_wide<-areaplanted_countryles %>%
  filter(product=="Maize")
names(areaplanted_countryles_wide)[3]<-"Maize"
areaplanted_countryles_wide$Sorghum<-(areaplanted_countryles %>%
  filter(product=="Sorghum"))$areatotal
head(areaplanted_countryles_wide)
areaplanted_countryles_wide<-select(areaplanted_countryles_wide,-2)

#Repeat South Africa
areaplanted_countrysaf_wide<-areaplanted.sum_saf %>%
  pivot_wider(names_from=product, values_from=area)
#Get rid of the production system column
areaplanted_countrysaf_wide<-select(areaplanted_countrysaf_wide,-c(2,4))
head(areaplanted_countrysaf_wide)

#Make detrended area planted data
head(areaplanted_countryles_wide)
areaplanted_countryles_wide$detrMaize<-
  as.numeric(detrend(areaplanted_countryles_wide$Maize))/
  sd(areaplanted_countryles_wide$Maize)
areaplanted_countryles_wide$detrSorghum<-
  as.numeric(detrend(areaplanted_countryles_wide$Sorghum))/
  sd(areaplanted_countryles_wide$Sorghum)

#Detrend each of the provinces of South Africa
#Make the columns for this
areaplanted_countrysaf_wide$detrMaize<-NA
areaplanted_countrysaf_wide$detrSorghum<-NA
#Now fill them for each province with detrended and standardized data
for (i in c(1:length(unique(areaplanted_countrysaf_wide$admin_1)))){
  #Get the district in question
  districtname<-unique(yield_countrysaf_wide$admin_1)[i]
  #select the rows that correspond to that district
  areaplanted_countrysaf_wide$detrMaize[
    which(areaplanted_countrysaf_wide$admin_1==districtname)]<-
    #And fill them with the detrended data
    as.numeric(detrend(
      areaplanted_countrysaf_wide$Maize[
        which(areaplanted_countrysaf_wide$admin_1==districtname)]))/
      #Which is also standardized
    sd(areaplanted_countrysaf_wide$Maize[
      which(areaplanted_countrysaf_wide$admin_1==districtname)])
  
  areaplanted_countrysaf_wide$detrSorghum[
    which(areaplanted_countrysaf_wide$admin_1==districtname)]<-
    as.numeric(detrend(
      areaplanted_countrysaf_wide$Sorghum[
        which(areaplanted_countrysaf_wide$admin_1==districtname)]))/
    #Which is also standardized
    sd(areaplanted_countrysaf_wide$Sorghum[
      which(areaplanted_countrysaf_wide$admin_1==districtname)])
  
}
head(areaplanted_countrysaf_wide)

#Now, add ENSO state at time of planting
head(ensowinter_OND)
areaplanted_countryles_wide$ensoOND<-
  ensowinter_OND$state[which(ensowinter_OND$YR %in% 
                               areaplanted_countryles_wide$year)]
head(areaplanted_countryles_wide)

#Now, add the ENSO state to the SAfrica data
areaplanted_countrysaf_wide<-left_join(areaplanted_countrysaf_wide, ensowinter_OND, 
          by = join_by(year == YR))

#Here is the actual rainfall at the time of planting
#whether or not people thought it was already starting to be dry
#October-November
areaplanted_countryles_wide$ONrain<-onlyONtotalprecip_lescountry$ONrain
areaplanted_countryles_wide$drystart<-
  onlyONtotalprecip_lescountry$drystart
head(areaplanted_countryles_wide)


#Now need to repeat this for SAfrica
#The areaplanted_countrysaf_wide has the ENSO data and detrended area planted
#But now need to add the ON rain season start
#Which is in wide format
#And it doesn't have the "drystart" column - need to add that
head(onlyONtotalprecip_saf)
#sapply on a data frame applies the function to each column
#so this calculates the drystart by column
onlyONtotalprecip_saf_drystart<-sapply(onlyONtotalprecip_saf,function(x){x<quantile(x,0.33)})
#except shouldn't have done that to the yearindex haha
onlyONtotalprecip_saf_drystart<-as.data.frame(onlyONtotalprecip_saf_drystart)
onlyONtotalprecip_saf_drystart$yearindex<-onlyONtotalprecip_saf$yearindex
head(onlyONtotalprecip_saf_drystart)

#Convert to long
onlyONtotalprecip_saf_long<-onlyONtotalprecip_saf %>%
  pivot_longer(!yearindex, names_to = "admin_1", values_to = "ONrain")
onlyONtotalprecip_saf_drystart_long<-onlyONtotalprecip_saf_drystart %>%
  pivot_longer(!yearindex, names_to = "admin_1", values_to = "ONraindrystart")


#Then do a left join with the areaplanted data
areaplanted_countrysaf_wide<-left_join(areaplanted_countrysaf_wide, onlyONtotalprecip_saf_long, 
                                       by = join_by(year == yearindex,
                                                    admin_1))
head(areaplanted_countrysaf_wide)
areaplanted_countrysaf_wide<-left_join(areaplanted_countrysaf_wide, 
                                       onlyONtotalprecip_saf_drystart_long, 
                                       by = join_by(year == yearindex,
                                                    admin_1))




# areaplantedselect<-areaplanted.sum_les %>%
#     filter(product==cropname)%>%
#     filter(admin_1==districtname)
# ensosmall<-ensowinter_OND %>%
#   filter(YR %in% areaplanted.sum_les$year)
# datatogether<-data.frame(areaplantedselect,
#                          onlyNDJtotalprecip_lesotho$Berea,
#                          ensosmall$state)
# head(datatogether)
# datatogether$drought<-
#   datatogether$onlyNDJtotalprecip_lesotho.Berea<
#   quantile(datatogether$onlyNDJtotalprecip_lesotho.Berea,0.33)



#Plot the results for Lesotho for both crops
#Figure plot 2
a<-ggplot(areaplanted_countryles_wide, 
       aes(x=drystart, y=detrMaize, fill=as.factor(ensoOND))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Oct-Nov Rainfall", y = "Area Planted",
       fill="ENSO state",title="Lesotho: Maize")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

b<-ggplot(areaplanted_countryles_wide, 
       aes(x=drystart, y=detrSorghum, fill=as.factor(ensoOND))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Oct-Nov Rainfall", y = "Area Planted",
       fill="ENSO state",title="Lesotho: Sorghum")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

ggarrange(a,b,labels="auto",common.legend = T)


#Provinces of interest are:
#Free State, Mpumalanga, North West, and Gauteng

#Repeat for South Africa paper plot area planted oct-nov rain and enso state
c<-
  ggplot(areaplanted_countrysaf_wide %>% filter(admin_1=="Free State"), 
          aes(x=ONraindrystart, y=detrMaize, fill=as.factor(state))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Oct-Nov Rainfall", y = "Area Planted (anom)",
       fill="ENSO state",title="Free State: Maize")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

d<-
  ggplot((areaplanted_countrysaf_wide %>% filter(admin_1=="North West")), 
          aes(x=ONraindrystart, y=detrSorghum, fill=as.factor(state))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Oct-Nov Rainfall", y = "Area Planted (anom)",
       fill="ENSO state",title="North West: Sorghum")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

ggarrange(a,b,c,d,labels="auto",common.legend = T)


#Supplementary Figure Plots of the others that were not significant
#Maize: Free State (done in paper), Mpumalanga, North West, Gauteng 
#Sorghum: Limpopo, Mpumalanga, Free State, and North West (done in paper)
supfigureplotsorghum<-function(location){
  a<-ggplot((areaplanted_countrysaf_wide %>% filter(admin_1==location)), 
         aes(x=ONraindrystart, y=detrSorghum, fill=as.factor(state))) + 
    geom_boxplot()+
    theme_minimal()+
    scale_fill_discrete(labels = c("La Nina","Neutral","El Nino"))+
    labs(x ="Oct-Nov Rainfall", y = "Area Planted (anom)",
         fill="ENSO state",title=paste0(location,": Sorghum"))+
    scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))
  return(a)
}
supfigureplotmaize<-function(location){
  a<-ggplot((areaplanted_countrysaf_wide %>% filter(admin_1==location)), 
            aes(x=ONraindrystart, y=detrMaize, fill=as.factor(state))) + 
    geom_boxplot()+
    theme_minimal()+
    scale_fill_discrete(labels = c("La Nina","Neutral","El Nino"))+
    labs(x ="Oct-Nov Rainfall", y = "Area Planted (anom)",
         fill="ENSO state",title=paste0(location,": Maize"))+
    scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))
  return(a)
}

ggarrange(
  supfigureplotmaize("Mpumalanga"),
  supfigureplotmaize("North West"),
  supfigureplotmaize("Gauteng"),
  
  supfigureplotsorghum("Mpumalanga"),
  supfigureplotsorghum("Limpopo"),
  supfigureplotsorghum("Free State"),
  labels="auto",common.legend = T
)



#Repeat this for Lesotho using yield
#Got this variable way down later in the code and am bringing it up here
head(lesotho_only_yield)
dim(lesotho_only_yield)
dim(areaplanted_countryles_wide)
areaplanted_countryles_wide$detrMaizeyield<-lesotho_only_yield$detrMaize
areaplanted_countryles_wide$detrSorghumyield<-lesotho_only_yield$detrSorghum
areaplanted_countryles_wide$NtoJrain<-lesotho_only_yield$croprain
head(areaplanted_countryles_wide)

plot(lesotho_only_yield$detrMaize,
     areaplanted_countryles_wide$detrMaizeyield)
as.factor(lesotho_only_yield$BN)
as.numeric(areaplanted_countryles_wide$NtoJrain<
  quantile(areaplanted_countryles_wide$NtoJrain,0.33))

plot(areaplanted_countryles_wide$ensoOND,
     (lesotho_only_yield$ENSOcat))


require(ggplot2)
require(ggpubr)
head(areaplanted_countryles_wide)

#Make a column that includes both above and below normal rain
areaplanted_countryles_wide$ABN<-
  areaplanted_countryles_wide$NtoJrain > 
  quantile(areaplanted_countryles_wide$NtoJrain,0.66)
areaplanted_countryles_wide$ABN[
  which(areaplanted_countryles_wide$NtoJrain <
          quantile(areaplanted_countryles_wide$NtoJrain,0.33))]<- -1
   
areaplanted_countryles_wide

#a<-
  ggplot(areaplanted_countryles_wide, 
          aes(as.factor(ABN),
            #x=(NtoJrain<quantile(NtoJrain,0.33)), 
                 y=detrMaizeyield, fill=as.factor(ensoOND))) + 
  geom_boxplot()+
  theme_minimal()
  scale_fill_discrete(labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Nov-Jan Rainfall", y = "Yield (standardized anomalies)",
       fill="ENSO state",title="Maize")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

#b<-
  ggplot(areaplanted_countryles_wide, 
          aes(x=as.factor(ABN),
            #x=(NtoJrain<quantile(NtoJrain,0.33)), 
              y=detrSorghumyield, fill=as.factor(ensoOND))) + 
  geom_boxplot()+
  theme_minimal()
  scale_fill_discrete(labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Nov-Jan Rainfall", y = "Yield (standardized anomalies",
       fill="ENSO state",title="Sorghum")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

ggarrange(a,b,labels="auto",common.legend = T)

#Calculate regression: area planted in Lesotho controlling for 
#how dry the start of the season was
head(areaplanted_countryles_wide)
reg.detrmaize.areales.ONrain<-lm(detrMaize~ensoOND+ONrain+ensoOND*ONrain,
                                 data=areaplanted_countryles_wide)
summary(reg.detrmaize.areales.ONrain)
reg.detrmaize.areales.ONrain$coefficients

#Bootstrap this result
require(boot)
# function to obtain R-Squared from the data
coef <- function(formula, data, indices){
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  #return(summary(fit)$r.square)
  return(fit$coefficients)
}
boot.detrmaize.areales.ONrain<-boot(data=areaplanted_countryles_wide, statistic=coef,
                R=1000, formula=detrMaize~ensoOND+ONrain+ensoOND*ONrain)
boot.detrmaize.areales.ONrain
plot(boot.detrmaize.areales.ONrain)
boot.ci(boot.detrmaize.areales.ONrain, type="bca",index=1)
boot.ci(boot.detrmaize.areales.ONrain, type="bca",index=2)
boot.ci(boot.detrmaize.areales.ONrain, type="bca",index=3)
boot.ci(boot.detrmaize.areales.ONrain, type="bca",index=4)

#repeat bootstrap for sorghum in Lesotho
boot.detrsorg.areales.ONrain<-boot(data=areaplanted_countryles_wide, statistic=coef,
                                    R=1000, formula=detrSorghum~ensoOND+ONrain+ensoOND*ONrain)
boot.detrsorg.areales.ONrain
plot(boot.detrsorg.areales.ONrain)
boot.ci(boot.detrsorg.areales.ONrain, type="bca",index=1)
boot.ci(boot.detrsorg.areales.ONrain, type="bca",index=2)
boot.ci(boot.detrsorg.areales.ONrain, type="bca",index=3)
boot.ci(boot.detrsorg.areales.ONrain, type="bca",index=4)


head(areaplanted_countryles_wide)


#Test for multicollinearity using VIF
require(car)
vifvalues<-vif(reg.detrmaize.areales.ONrain)
vifvalues
#It seems that the multicollinearity might not be a huge problem
cor.test(areaplanted_countryles_wide$ensoOND,
         areaplanted_countryles_wide$ONrain)

#Interaction term significant for sorghum
reg.detrsorg.areales.ONrain<-lm(detrSorghum~ensoOND+ONrain+ensoOND*ONrain,
                                 data=areaplanted_countryles_wide)
summary(reg.detrsorg.areales.ONrain)

#Now repeat for safrica
namesplanted<-unique(areaplanted_countrysaf_wide$admin_1)
unique(areaplanted_countrysaf_wide$admin_1)[c(4,6,8,9)] #Sorghum
unique(areaplanted_countrysaf_wide$admin_1)[c(4,6:8)] #Maize
head(areaplanted_countrysaf_wide)
names
#Print the regression results for each district
#for (i in c(4,6,8,9)){
for (i in c(4,6:8)){
    subset<-(areaplanted_countrysaf_wide %>%
             filter(admin_1==namesplanted[i]))
  print(namesplanted[i])
  #Regress rain and error on Maize
  #Regress rain and abs value error
  #Regress rain and briar score
  #print(summary(lm(detrSorghum~ONrain+ANOM+ONrain*ANOM,data=subset)))
  print(summary(lm(detrMaize~ONrain+ANOM+ONrain*ANOM,data=subset)))
  #print(cor.test(subset$detrMaize,subset$error))
}
head(subset)

#Look at length of both datasets
unique(areaplanted_countryles_wide$year)
unique(areaplanted_countrysaf_wide$year)


#Do the bootstrapping for SAfrica Free State Maize and
#SAfrica North West Sorghum
namesplanted
areaplanted_saf_freestate<-(areaplanted_countrysaf_wide %>%
           filter(admin_1==namesplanted[4]))
areaplanted_saf_freestate
boot.detrmaize.areasaf.ONrain<-boot(data=areaplanted_saf_freestate, statistic=coef,
                                   R=1000, formula=detrMaize~ANOM+ONrain+ANOM*ONrain)
boot.detrmaize.areasaf.ONrain
plot(boot.detrmaize.areasaf.ONrain,index=1)
boot.ci(boot.detrmaize.areasaf.ONrain, type="bca",index=1)
boot.ci(boot.detrmaize.areasaf.ONrain, type="bca",index=2)
boot.ci(boot.detrmaize.areasaf.ONrain, type="bca",index=3)
boot.ci(boot.detrmaize.areasaf.ONrain, type="bca",index=4)

#And now for sorghum in NW
areaplanted_saf_northwest<-(areaplanted_countrysaf_wide %>%
                              filter(admin_1==namesplanted[6]))
areaplanted_saf_northwest
boot.detrsorg.areasaf.ONrain<-boot(data=areaplanted_saf_northwest, statistic=coef,
                                   R=1000, formula=detrSorghum~ANOM+ONrain+ANOM*ONrain)
boot.detrsorg.areasaf.ONrain
plot(boot.detrsorg.areasaf.ONrain,index=4)
boot.ci(boot.detrsorg.areasaf.ONrain, type="bca",index=1)
boot.ci(boot.detrsorg.areasaf.ONrain, type="bca",index=2)
boot.ci(boot.detrsorg.areasaf.ONrain, type="bca",index=3)
boot.ci(boot.detrsorg.areasaf.ONrain, type="bca",index=4)


#Ok, now do something similar for Lesotho and each
#Southern African district for yield and total rain
lesothocountry_all<-areaplanted_countryles_wide
head(lesothocountry_all)
head(yieldaverage_les)
yield.df_countryles<-yieldaverage_les %>%
  group_by(year,product) %>%
  summarize(yield=mean(yield,na.rm=T))
yield.df_countryles
yield_countryles_wide<-yield.df_countryles %>%
  filter(product=="Maize")
head(yield_countryles_wide)
#Get rid of extra column now
yield_countryles_wide<-select(yield_countryles_wide,-2)
names(yield_countryles_wide)[2]<-"Maize"
yield_countryles_wide$Sorghum<-(yield.df_countryles %>%
                                  filter(product=="Sorghum"))$yield
head(yield_countryles_wide)
#Combine with precip data 
yield_countryles_wide<-cbind(yield_countryles_wide,
      select(onlyNtoJtotalprecip_lescountry,-1)) #deselect year column
head(yield_countryles_wide)

#Take anomalies
yield_countryles_wide$detrMaize<-
  as.numeric(detrend(yield_countryles_wide$Maize)/sd(yield_countryles_wide$Maize))
yield_countryles_wide$detrSorghum<-
  as.numeric(detrend(yield_countryles_wide$Sorghum)/sd(yield_countryles_wide$Sorghum))

#Make the dataset shorter and merge with fcst verification data
head(districtskillscores_les)
head(districtskillscores_saf)
#For now just take Berea
#FIX to get truely accurate entire Lesotho error

#Save a version as a fulltime version
yield_countryles_fulltime<-yield_countryles_wide

#Making it shorter
yield_countryles_wide<-yield_countryles_wide %>%
  filter(year %in% districtskillscores_les$Berea$year)

#Now add error data from ECMWF
yield_countryles_wide$error<-districtskillscores_les$Berea$error
#And briar score from IRI
yield_countryles_wide$briar<-districtskillscores_les$Berea$briartotal

#Make categorical error
hist(yield_countryles_wide$error)
yield_countryles_wide$caterror<-yield_countryles_wide$error > 0.5
yield_countryles_wide$caterror[which(
  yield_countryles_wide$error < -0.5)]<- -1
yield_countryles_wide$caterror
head(yield_countryles_wide)

#Boxpot to show relationship between detrended yield and total rain
#with categories for forecast accuracy
a<-ggplot(yield_countryles_wide, 
       aes(x=BN, y=detrMaize, fill=as.factor(caterror))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(labels = c("Too low","Accurate","Too high"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (anom)",
       fill="Fcst error",title="Maize")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

b<-ggplot(yield_countryles_wide, 
       aes(x=BN, y=detrSorghum, fill=as.factor(caterror))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(labels = c("Too low","Accurate","Too high"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (anom)",
       fill="Fcst error",title="Sorghum")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

ggarrange(a,b,labels="auto",common.legend = T)

dim(yield_countryles_wide)
head(yield_countryles_wide)

#Repeat for South Africa data
head(yieldaverage_saf)
#make the Maize and Sorgum yields into their own columns
require(tidyr)
yieldaverage_saf_wide<- yieldaverage_saf %>% spread(key = product, value = yield)
#Wow that was easy
#Remove the production system not needed
yieldaverage_saf_wide<-select(yieldaverage_saf_wide,-2)
head(yieldaverage_saf_wide)

# #Now turn into a list by district
# safrica$NAME_1
# head(yieldaverage_saf_wide)
# yieldaverage_saf_list<-lapply(c(1:length(safrica$NAME_1)),function(x){
#   yieldaverage_saf_wide %>% filter(admin_1==safrica$NAME_1[x])
# })
# names(yieldaverage_saf_list)<-safrica$NAME_1
# yieldaverage_saf_list$`Eastern Cape`



#Combine with precip data 
yield_countryles_wide<-cbind(yield_countryles_wide,
                             select(onlyNtoJtotalprecip_lescountry,-1)) #deselect year column
head(yield_countryles_wide)

#SAfrica combine with precip data
dim(onlyNtoJtotalprecip_saf_long)
dim(yieldaverage_saf_wide)
table(yieldaverage_saf_wide$year) #This starts in 1987
table(onlyNtoJtotalprecip_saf_long$year) #This starts in 1982
dim(onlyNtoJtotalprecip_saf_long %>% filter(year>=1987)) #Now that checks out
head(onlyNtoJtotalprecip_saf_long)
yield_countrysaf_wide<-cbind(yieldaverage_saf_wide,
                             onlyNtoJtotalprecip_saf_long %>% filter(year>=1987))
head(yield_countrysaf_wide) #looks perfect
tail(yield_countrysaf_wide)
#Let's get rid of the extra columns
yield_countrysaf_wide<-yield_countrysaf_wide %>% select(-c(5,6))

#Take anomalies
require(pracma)
yield_countryles_wide$detrMaize<-
  as.numeric(detrend(yield_countryles_wide$Maize)/sd(yield_countryles_wide$Maize))
yield_countryles_wide$detrSorghum<-
  as.numeric(detrend(yield_countryles_wide$Sorghum)/sd(yield_countryles_wide$Sorghum))

#Anomalies for SAfrica
yield_countrysaf_wide$detrMaize<-NA
yield_countrysaf_wide$detrSorghum<-NA
head(yield_countrysaf_wide)
for (i in unique(yield_countrysaf_wide$admin_1)){
  districtname<-i
  #Maize
  subset<-yield_countrysaf_wide[
    which(yield_countrysaf_wide$admin_1==districtname),3]
  yield_countrysaf_wide[
    which(yield_countrysaf_wide$admin_1==districtname),7]<-
    (detrend(subset)/sd(subset))
  #Sorghum
  subset<-yield_countrysaf_wide[
    which(yield_countrysaf_wide$admin_1==districtname),4]
  yield_countrysaf_wide[
    which(yield_countrysaf_wide$admin_1==districtname),8]<-
    (detrend(subset)/sd(subset))
}

#Look at results to confirm the detrending worked well - looks good
require(ggplot2)
ggplot(yield_countrysaf_wide,# %>% filter(product=="Maize"), 
       aes(x=year, y=Maize,group=admin_1,color=admin_1)) +
  geom_line()+
  theme_minimal()
ggplot(yield_countrysaf_wide,# %>% filter(product=="Maize"), 
       aes(x=year, y=detrMaize,group=admin_1,color=admin_1)) +
  geom_line()+
  theme_minimal()

#Save a version as the long version
yield_countrysaf_fulltime<-yield_countrysaf_wide
yield_countryles_fulltime
yield_countrysaf_fulltime
View(yield_countrysaf_fulltime)

#Make the dataset shorter and merge with fcst verification data
head(districtskillscores_les)
head(districtskillscores_saf)
names(districtskillscores_saf)
#For now just take Berea
#FIX to get truely accurate entire Lesotho error

#Making it shorter
yield_countryles_wide<-yield_countryles_wide %>%
  filter(year %in% districtskillscores_les$Berea$year)
yield_countrysaf_wide<-yield_countrysaf_wide %>%
  filter(year %in% districtskillscores_les$Berea$year)
head(yield_countrysaf_wide)

#Now add error data from ECMWF
yield_countryles_wide$error<-districtskillscores_les$Berea$error
#And briar score from IRI
yield_countryles_wide$briar<-districtskillscores_les$Berea$briartotal

#Make categorical error
hist(yield_countryles_wide$error)
yield_countryles_wide$caterror<-yield_countryles_wide$error > 0.5
yield_countryles_wide$caterror[which(
  yield_countryles_wide$error < -0.5)]<- -1
yield_countryles_wide$caterror
head(yield_countryles_wide)

#Repeat for each district in SAfrica
yield_countrysaf_wide$error<-NA
yield_countrysaf_wide$briar<-NA
yield_countrysaf_wide$caterror<-NA
head(yield_countrysaf_wide)
for (i in c(1:length(unique(yield_countrysaf_wide$admin_1)))){
  districtname<-unique(yield_countrysaf_wide$admin_1)[i]
  #ECMWF Error
  yield_countrysaf_wide[
    which(yield_countrysaf_wide$admin_1==districtname),9]<-
    (districtskillscores_saf[[i]]$error)
  #Briar total
  yield_countrysaf_wide[
    which(yield_countrysaf_wide$admin_1==districtname),10]<-
    (districtskillscores_saf[[i]]$briartotal)
}

#Make categorical error
hist(yield_countrysaf_wide$error)
yield_countrysaf_wide$caterror<-yield_countrysaf_wide$error > 0.5
yield_countrysaf_wide$caterror[which(
  yield_countrysaf_wide$error < -0.5)]<- -1
yield_countrysaf_wide$caterror
head(yield_countrysaf_wide)

#Combine Lesotho data as if Lesotho was an admin_1 region of SAfrica
head(yield_countryles_wide)
yield_countryles_wide<-as.data.frame(yield_countryles_wide)
#Need to add location name to Lesotho data
yield_countryles_wide$admin_1<-"Lesotho"
#Now, re-arratnge so it is first
yield_countryles_wide <- yield_countryles_wide %>%
  select(admin_1, everything())
dim(yield_countryles_wide)
dim(yield_countrysaf_wide)

#Combine the two!
yield_regionsaf<-rbind(yield_countrysaf_wide,yield_countryles_wide)
head(yield_regionsaf)
tail(yield_regionsaf)
hist(yield_regionsaf$detrSorghum)

#Now, can make plots of how the yield varies, using all the data
#Boxpot to show relationship between detrended yield and total rain
#with categories for forecast accuracy
(yield_regionsaf$BN)
(yield_countryles_wide$BN)
unique(yield_regionsaf$admin_1)
View(yield_regionsaf)

a<-ggplot(yield_regionsaf %>%
            filter(admin_1 %in% c("Free State","North West","Gauteng",
                                  "Mpumalanga","Lesotho")), 
          aes(x=as.factor(BN), y=detrMaize, fill=as.factor(caterror))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(labels = c("Too low","Accurate","Too high"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (anom)",
       fill="Fcst error",title="Maize")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

a
b<-ggplot(yield_regionsaf %>%
            filter(admin_1 %in% c("Free State","North West","Limpopo",
                                  "Mpumalanga","Lesotho")), 
          aes(x=as.factor(BN), y=detrSorghum, fill=as.factor(caterror))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(labels = c("Too low","Accurate","Too high"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (anom)",
       fill="Fcst error",title="Sorghum")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

ggarrange(a,b,labels="auto",common.legend = T)

#What about the IRI fcst?
head(yield_regionsaf)
#Add a column saying whether the highest fcst probability was A N B?
head(districtskillscores_saf)


yield_regionsaf$domtercile<-NA
head(yield_regionsaf)
dim(yield_regionsaf)

for (i in c(1:length(unique(yield_countrysaf_wide$admin_1)))){
  districtname<-unique(yield_countrysaf_wide$admin_1)[i]
  #Figure out which column was the max
  results<-districtskillscores_saf[[i]][,1:3] %>%
    mutate(domtercile = names(.)[max.col(.)])
  #And label the ones that were just climatology
  results[which((results$below + results$near)==66),4]<-"climatology"
  
  #Assign the answer
  yield_regionsaf[
    which(yield_regionsaf$admin_1==districtname),12]<-
    (results[,4])
  }
head(yield_regionsaf)
tail(yield_regionsaf)

#Now do the same for Lesotho only
results<-districtskillscores_les$Berea[,1:3] %>%
  mutate(domtercile = names(.)[max.col(.)])
#And label the ones that were just climatology
results[which((results$below + results$near)==66),4]<-"climatology"
results

#Assign the answer
yield_regionsaf[
  which(yield_regionsaf$admin_1=="Lesotho"),12]<-
  (results[,4])

#Put the forecast types in order
yield_regionsaf$domtercile<-factor(yield_regionsaf$domtercile,
                                   levels=c("below","near","above","climatology"))
#Now can plot the results
a<-ggplot(yield_regionsaf %>%
            filter(admin_1 %in% c("Free State","North West","Gauteng",
                                  "Mpumalanga","Lesotho")), 
       aes(x=as.factor(BN), y=detrMaize, fill=as.factor(domtercile))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(
    labels = c("Below Normal","Near Normal","Above Normal","Climatology"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (anom)",
       fill="OND Forecast tercile",title="Maize")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

b<-ggplot(yield_regionsaf %>%
       filter(admin_1 %in% c("Free State","North West","Limpopo",
                             "Mpumalanga","Lesotho")), 
       aes(x=as.factor(BN), y=detrSorghum, fill=as.factor(domtercile))) + 
  geom_boxplot()+
  theme_minimal()+
    scale_fill_discrete(drop=F,
      labels = c("Below Normal","Near Normal","Above Normal","Climatology"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (anom)",
       fill="OND Forecast tercile",title="Sorghum")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

ggarrange(a,b,labels="auto",common.legend = T)

#Simple correlation between detrended area planted
head(areaplanted_countryles_wide)
cor.test(areaplanted_countryles_wide$detrMaize,
         areaplanted_countryles_wide$detrSorghum)
unique(areaplanted_countrysaf_wide$admin_1)
cor.test(
  (areaplanted_countrysaf_wide %>% filter(admin_1=="Free State"))$detrMaize,
  (areaplanted_countrysaf_wide %>% filter(admin_1=="Free State"))$detrSorghum
)  
cor.test(
  (areaplanted_countrysaf_wide %>% filter(admin_1=="North West"))$detrMaize,
  (areaplanted_countrysaf_wide %>% filter(admin_1=="North West"))$detrSorghum
)  
cor.test(
  (areaplanted_countrysaf_wide %>% filter(admin_1=="Mpumalanga"))$detrMaize,
  (areaplanted_countrysaf_wide %>% filter(admin_1=="Mpumalanga"))$detrSorghum
)  


#Do a regression for a few districts
names<-unique(yield_regionsaf$admin_1)
length(names)
#See what they all say
for (i in c(1:10)){
  subset<-(yield_regionsaf %>%
             filter(admin_1==names[i]))
  print(names[i])
  #Regress rain and error on Maize
  #Regress rain and abs value error
  #Regress rain and briar score
  print(summary(lm(detrMaize~croprain+error+croprain*error,data=subset)))
  #print(cor.test(subset$detrMaize,subset$error))
  }
head(subset)


#What about knowledge of El Nino vs. yields?
yield_countryles_fulltime
yield_countrysaf_fulltime
yield_countryles_fulltime$admin_1<-"Lesotho"
yield_countryles_fulltime<-yield_countryles_fulltime %>% select(admin_1,everything())
yield_all_fulltime<-rbind(yield_countrysaf_fulltime,yield_countryles_fulltime)
head(yield_all_fulltime)
tail(yield_all_fulltime)


#Add El Nino column
yield_all_fulltime$ensoOND<-NA
yield_all_fulltime$ensocatOND<-NA
yield_all_fulltime$ensoNDJ<-NA
yield_all_fulltime$ensocatNDJ<-NA

names(yield_all_fulltime)
ensowinter_NDJ
head(yield_all_fulltime)
dim(yield_all_fulltime)
unique(yield_all_fulltime$year)
for (i in unique(yield_all_fulltime$year)){
  #Add ENSO values for the correct years for OND
  yield_all_fulltime[which(yield_all_fulltime$year==i),9]<-
    ensowinter_OND[which(ensowinter_OND$YR==i),3]
  #Then repeat for NDJ
  yield_all_fulltime[which(yield_all_fulltime$year==i),11]<-
    ensowinter_NDJ[which(ensowinter_NDJ$YR==i),3]
}

i<-1
for (i in c(1:10)){
  subset<-(yield_all_fulltime %>%
             filter(admin_1==names[i]))
  print(names[i])
  #Regress rain and error on Maize
  #Regress rain and abs value error
  #Regress rain and briar score
  modelout<-lm(detrMaize~croprain+ensoNDJ+croprain*ensoNDJ,data=subset)
  (summary(modelout))[4]
  print(summary(modelout))
  names(summary(modelout))
  lmp(modelout)
  summary(modelout)$adj.r.squared
  summary(modelout)$coefficients
  print(cor.test(subset$ensoNDJ,subset$croprain))
  print(vif(modelout,type="predictor"))
}

#Here is a function to pull out the p-value of the overall model
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#Write a function to save the model out for all locations
allregsrainENSOmaize<-lapply(c(1:10),function(x){
  subset<-(yield_all_fulltime %>%
             filter(admin_1==names[x]))
  modelout<-lm(detrMaize~croprain+ensoNDJ+croprain*ensoNDJ,data=subset)
  return(modelout)
  })

length(allregsrainENSOmaize)
allregsrainENSOmaize
names(allregsrainENSOmaize)<-names
names(allregsrainENSOmaize)

#Here are the p-values to see which models are statistically significant
sigmodelsENSOmaize<-lapply(allregsrainENSOmaize,function(x){
  lmp(x)
})<0.05
sigmodelsENSOmaize

#Take a look at the coefficients of ENSO state for sig models
for (i in which(sigmodelsENSOmaize==T)){
  print(summary(allregsrainENSOmaize[[i]])$coefficients)
}

#Extract only the ensoNDJ coefficients
sigmodelsENSOmaize_ensocoeff<-sapply(which(sigmodelsENSOmaize==T),function(x){
  summary(allregsrainENSOmaize[[x]])$coefficients[3,1]
})

#Which locations are these?
unique(yield_all_fulltime$admin_1)[which(sigmodelsENSOmaize==T)]
names(sigmodelsENSOmaize_ensocoeff)<-unique(yield_all_fulltime$admin_1)[which(sigmodelsENSOmaize==T)]
sigmodelsENSOmaize_ensocoeff

#Repeat for sorghum
#Write a function to save the model out for all locations
names
#select only sorghum planting locations: "Free State","North West","Limpopo",
#"Mpumalanga","Lesotho"
#numbers 2,5,6,7,10 in the names
allregsrainENSOsorghum<-lapply(c(2,5,6,7,10),function(x){
  subset<-(yield_all_fulltime %>%
             filter(admin_1==names[x]))
  modelout<-lm(detrSorghum~croprain+ensoNDJ+croprain*ensoNDJ,data=subset)
  return(modelout)
})
summary(allregsrainENSOsorghum[[1]])
length(allregsrainENSOsorghum)
names(allregsrainENSOsorghum)<-names[c(2,5,6,7,10)]
names(allregsrainENSOsorghum)

#Here are the p-values to see which models are statistically significant
sigmodelsENSOsorghum<-lapply(allregsrainENSOsorghum,function(x){
  lmp(x)
})<0.05
sigmodelsENSOsorghum

#Take a look at the coefficients of ENSO state for sig models
for (i in which(sigmodelsENSOsorghum==T)){
  print(summary(allregsrainENSOsorghum[[i]])$coefficients)
}

#Extract only the ensoNDJ coefficients
sigmodelsENSOsorghum_ensocoeff<-sapply(which(sigmodelsENSOsorghum==T),function(x){
  summary(allregsrainENSOsorghum[[x]])$coefficients[3,1]
})
sigmodelsENSOsorghum_ensocoeff
sorghumnames<-names[c(2,5,6,7,10)]
sorghumnames[which(sigmodelsENSOsorghum==T)]
names(sigmodelsENSOsorghum_ensocoeff)<-sorghumnames[which(sigmodelsENSOsorghum==T)]
#Those are the only significant locations
sigmodelsENSOmaize_ensocoeff
sigmodelsENSOsorghum_ensocoeff

#Manually combine these
sigmodelsENSOmaize_ensocoeff<-as.data.frame(sigmodelsENSOmaize_ensocoeff)
sigmodelsENSOmaize_ensocoeff$crop<-"Maize"
sigmodelsENSOmaize_ensocoeff
names(sigmodelsENSOmaize_ensocoeff)[1]<-"ensocoeff"
sigmodelsENSOmaize_ensocoeff$province<-row.names(sigmodelsENSOmaize_ensocoeff)
sigmodelsENSOsorghum_ensocoeff<-as.data.frame(sigmodelsENSOsorghum_ensocoeff)
sigmodelsENSOsorghum_ensocoeff$crop<-"Sorghum"
names(sigmodelsENSOsorghum_ensocoeff)[1]<-"ensocoeff"
sigmodelsENSOsorghum_ensocoeff$province<-row.names(sigmodelsENSOsorghum_ensocoeff)
sigmodelsENSOall<-rbind(sigmodelsENSOsorghum_ensocoeff,
                        sigmodelsENSOmaize_ensocoeff)
names(sigmodelsENSOall)
sigmodelsENSOall

#Let's select only the provinces we wanted
names(allregsrainENSOmaize)
names(allregsrainENSOsorghum)
allregsrainENSOmaize_select<-allregsrainENSOmaize[c(2,3,6,7,10)]
names(allregsrainENSOmaize_select)

#For each one, get the beta coefficient for ENSO and sterror
#This is the coeff
summary(allregsrainENSOmaize_select[[1]])$coefficients[3,1]
#this is the sterror
summary(allregsrainENSOmaize_select[[1]])$coefficients[3,2]

#Maizeresults beta and sterror
maizecoeffsterror<-
  sapply(c(1:length(allregsrainENSOmaize_select)),
       function(x){
         #Get coeff
         a<-summary(allregsrainENSOmaize_select[[x]])$coefficients[3,1]
         #get sterror
         b<-summary(allregsrainENSOmaize_select[[x]])$coefficients[3,2]
         return(c(a,b))
       })
maizecoeffsterror<-as.data.frame(t(maizecoeffsterror))
names(maizecoeffsterror)<-c("coeff","sterror")
maizecoeffsterror$admin_1<-names(allregsrainENSOmaize_select)
maizecoeffsterror$crop<-"Maize"
maizecoeffsterror

#Repeat for sorghum
sorghumcoeffsterror<-
  sapply(c(1:length(allregsrainENSOsorghum)),
         function(x){
           #Get coeff
           a<-summary(allregsrainENSOsorghum[[x]])$coefficients[3,1]
           #get sterror
           b<-summary(allregsrainENSOsorghum[[x]])$coefficients[3,2]
           return(c(a,b))
         })
sorghumcoeffsterror<-as.data.frame(t(sorghumcoeffsterror))
names(sorghumcoeffsterror)<-c("coeff","sterror")
sorghumcoeffsterror$admin_1<-names(allregsrainENSOsorghum)
sorghumcoeffsterror$crop<-"Sorghum"
sorghumcoeffsterror

#Stick them together
elmodelsENSOall<-rbind(
  sorghumcoeffsterror,maizecoeffsterror
)
elmodelsENSOall


#Plot the results
#Old paper figure from first submission
ggplot(elmodelsENSOall, aes(x = admin_1, y=coeff, 
                             fill=crop)) + 
  geom_col(
    position = position_dodge(.5),
    width = .5
  ) +
  geom_errorbar(aes(
    ymin = coeff - sterror,
    ymax = coeff + sterror
  ),
  position = position_dodge(.5),
  width = .2
  )+
  theme_classic()+
  labs(x ="", y = expression(paste(beta," coefficient")),
                        fill="Crop")+
  scale_fill_brewer(palette="YlOrRd")



##Repeat this analysis using the boostrapping approach

#Function to do all the bootstrapping and return the original
#of the coeff for the ensoNDJ as well as the low and high cis
getbootstrapsyield<-function(location,crop){
  subset<-(yield_all_fulltime %>%
             filter(admin_1==names[location]))
  if(crop=="Maize"){
    boot.out<-boot(data=subset, statistic=coef,
                   R=1000, formula=detrMaize~ensoNDJ+croprain+croprain*ensoNDJ)
  }else if(crop=="Sorghum"){
    boot.out<-boot(data=subset, statistic=coef,
                   R=1000, formula=detrSorghum~ensoNDJ+croprain+croprain*ensoNDJ)
  }
  a<-boot.ci(boot.out,type="bca",index=2)
  results<-c(boot.out$t0[2],a$bca[4:5])
  names(results)<-c("orig","lowci","highci")
  return(results)
}

#For Maize
#Maize
names[c(2,3,6,7,10)]
bootstrapallmaize<-sapply(c(2,3,6,7,10),getbootstrapsyield,crop="Maize")
bootstrapallmaize<-as.data.frame(t(bootstrapallmaize))
bootstrapallmaize$admin1<-names[c(2,3,6,7,10)]
bootstrapallmaize$crop<-"Maize"


#For Sorghum
names[c(2,5,6,7,10)]
bootstrapallsorghum<-sapply(c(2,5,6,7,10),getbootstrapsyield,crop="Sorghum")
bootstrapallsorghum
bootstrapallsorghum<-as.data.frame(t(bootstrapallsorghum))
bootstrapallsorghum$admin1<-names[c(2,5,6,7,10)]
bootstrapallsorghum$crop<-"Sorghum"
bootstrapallsorghum

#Merge
bootstrapalltogether<-rbind(bootstrapallsorghum,bootstrapallmaize)
bootstrapalltogether

#New Paper Figure!!
ggplot(bootstrapalltogether, aes(x = admin1, y=orig, 
                                 color=crop)) + 
  geom_pointrange(aes(ymin=lowci, ymax=highci),
#                  linewidth=1,size=0.90,
                      position = position_dodge(.5))+
  theme_classic()+
  labs(x ="", y = expression(paste(beta," coefficient")),
       color="Crop")+
  geom_hline(yintercept=0)


a







head(subset)

lesotho_only_yield<-(yield_all_fulltime %>%
           filter(admin_1=="Lesotho"))
subset
#Add enso categroy
yield_all_fulltime$ensocatOND<-yield_all_fulltime$ensoOND>1
yield_all_fulltime$ensocatOND[which(yield_all_fulltime$ensoOND< -1)]<- -1
head(yield_all_fulltime)
yield_all_fulltime$ensocatNDJ<-yield_all_fulltime$ensoNDJ>1
yield_all_fulltime$ensocatNDJ[which(yield_all_fulltime$ensoNDJ< -1)]<- -1


lesotho_only_yield$detrMaize
as.factor(lesotho_only_yield$BN)
as.factor(lesotho_only_yield$ENSOcat)



head(yield_all_fulltime)
dim(yield_all_fulltime)
#Add a column that's not just BN also including AN
yield_all_fulltime$ABN<-NA
for (i in c(1:10)){
  subset<-(yield_all_fulltime %>%
             filter(admin_1==names[i]))
  print(names[i])
  #Now add the column for ABN
  subset$ABN<-subset$croprain>quantile(subset$croprain,0.66)
  subset$ABN[which(subset$BN==T)]<- -1
  yield_all_fulltime[which(
    yield_all_fulltime$admin_1==names[i]),13]<-subset$ABN
}


#Plot yield vs. total crop rainfall disaggregated by ENSO state
#For all locations pooled
#But need to only keep the selected maize locatoins
#Paper plot!
a<-
  ggplot(yield_all_fulltime %>%
           filter(admin_1 %in% c("Free State","North West",
                                "Mpumalanga","Lesotho","Gauteng")),
       aes(x=as.factor(BN), y=detrMaize, fill=as.factor(ensocatNDJ))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(
    labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (st anom)",
       fill="ENSO state NDJ",title="Maize")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

b<-
  ggplot(yield_all_fulltime %>%
           filter(admin_1 %in% c("Free State","North West","Limpopo",
                                 "Mpumalanga","Lesotho")),
       aes(x=as.factor(BN), y=detrSorghum, fill=as.factor(ensocatNDJ))) + 
  geom_boxplot()+
  theme_minimal()+
  scale_fill_discrete(
    labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (st anom)",
       fill="ENSO state NDJ",title="Sorghum")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))

ggarrange(a,b,labels="auto",common.legend = T)
#FIGURE FOR PAPER

head(yield_all_fulltime)
#What about plotting this as points with the color being ENSO state?
a<-ggplot(yield_all_fulltime,# %>% filter(admin_1=="Lesotho"),
       aes(x=croprain, y=detrMaize, color=as.factor(ensocatNDJ))) + 
  geom_point()+
  theme_minimal()+
  scale_color_discrete(
    labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (st anom)",
       color="ENSO state NDJ",title="Maize")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))
b<-ggplot(yield_all_fulltime,# %>% filter(admin_1=="Lesotho"),
          aes(x=croprain, y=detrSorghum, color=as.factor(ensocatNDJ))) + 
  geom_point()+
  theme_minimal()+
  scale_color_discrete(
    labels = c("La Nina","Neutral","El Nino"))+
  labs(x ="Nov-Jun Rainfall", y = "Yield (st anom)",
       color="ENSO state NDJ",title="Sorghum")+
  scale_x_discrete(labels=c("Upper two terciles","Lower tercile"))


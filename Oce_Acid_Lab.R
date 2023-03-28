#######################
#Can we quantify ocean acidification in the subtropical North Atlantic Ocean?
#1) Is surface ocean pco2 increasing
#2) Is surface ocean seawater ph decreasing? 
#3) Is surface ocean seawater saturation state with respect to aragonite decreasing? 
#Load Required library 
library(tidyverse)
library(reader)
library(gsw)
library('seacarb')
library(ggplot2)
library(plotly)

###############################################################################Intro>methods>result>discussion mino-lab report
#Import Data from BATS
bats_bottle <- read_delim("bats_bottle.txt", 
                          delim = "\t", escape_double = FALSE, 
                          col_names = FALSE, trim_ws = TRUE, skip = 60)
View(bats_bottle) #DATA Without names 

bats_bottle <- read_csv("bats_bottle.txt", 
                        skip = 59)
View(bats_bottle) #NAMES ONLY

#Import column names from Bats and assign to Data joining Data and names 
colnames(bats_bottle)<- colnames(read_csv("bats_bottle.txt", 
                                          skip = 59))
#Check that its correct
View(bats_bottle)
#Now I have a data frame of MATS data with column names for variables
###############################################################################
#Variables
#yyyymmdd = Year Month Day   
#decy   = Decimal Year     
#time   = Time (hhmm)      
#latN   = Latitude (Deg N) 
#lonW   = Longitude (Deg W)
#Depth  = Depth (m)                  
#Temp   = Temperature ITS-90 (C)    
#CTD_S  = CTD Salinity (PSS-78)      
#Sal1   = Salinity-1 (PSS-78)        
#Sig-th = Sigma-Theta (kg/m^3)       
#O2(1)  = Oxygen-1 (umol/kg)          
#OxFixT = Oxygen Fix Temp (C)        
#Anom1  = Oxy Anomaly-1 (umol/kg)    
#CO2    = dissolved inorganic carbon (umol/kg)              
#Alk    = Alkalinity (uequiv)        
#NO31   = Nitrate+Nitrite-1 (umol/kg)
#NO21   = Nitrite-1 (umol/kg)        
#PO41   = Phosphate-1 (umol/kg)      
#Si1    = Silicate-1 (umol/kg)       
#POC    = POC (ug/kg)                
#PON    = PON (ug/kg)                
#TOC    = TOC (umol/kg)                
#TN     = TN (umol/kg)  
#Bact   = Bacteria enumeration (cells*10^8/kg)   
#POP    = POP (umol/kg)
#TDP    = Total dissolved Phosphorus (nmol/kg)
#SRP    = Low-level phosphorus (nmol/kg)
#BSi    = Particulate biogenic silica (umol/kg)
#LSi    = Particulate lithogenic silica  (umol/kg)
#Pro    = Prochlorococcus (cells/ml)
#Syn    = Synechococcus (cells/ml)
#Piceu  = Picoeukaryotes (cells/ml)
#Naneu  = Nanoeukaryotes (cells/ml)

#/Quality flags
#-999 = Missing or bad data
#0 = Less than detection limit
###############################################################################
#Calculate Co2 chemistry parameter
?carb

#carb(flag, var1, var2, S=35, T=25, Patm=1, P=0, Pt=0, Sit=0,
    # k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential", 
    # warn="y", eos="eos80", long=1.e20, lat=1.e20)

#We Have TA, DIC, S T, Pt, Sit, but we don't have pressure (dbar)

#Firts we need to calculate pressure using TEOS-10
?gsw
# p = gsw_p_from_z(z,lat)
bats_co2=bats_bottle %>%
  mutate(P_dbar=gsw_p_from_z(Depth*-1,latN)) #-1 cause is negative in the ocean
#we now have all of the variables that we need to calculate the surface seawater chemistry at the BATS station, but that we need to be very careful about our units 

View(bats_co2)

#############################################################################
#Intro on BATS and ocean acidification

#Here we will be using The Gibbs SeaWater (GSW) Oceanographic Toolbox of TEOS-10 to helps us quantify ocean acidification over the past 40 years in the subtropical North Atlantic Ocean using BATS Time-series records. This with the intend to find different ways to predict changes going further into the future to help us understand what to expect in the future and analyze how this will affect the oceanic ecosystem. 

#Ocean acidification due to climate change is changing the carbon cycle in the ocean. Trying to understand the metabolic processes on the ocean ecosystem and the variability between net ecosystem production and net ecosystem calcification...   (expand on ocean acidification with references?)
############################################################################


#W now have TA, DIC, S, T, Pt, Sit, Pressure
#What are the units of these and what does CO2sys need?

#TA is in uequiv = umol/kg and we need mol/kg
  #Alk*10^-6

#DIC is in umol/ kg and we need mol/kg
  #DIC*10^-6

#S is in PSS and we will use Eos80
  #Sal1

#T is in degrees C and we need degree C
  #Temp

#Pt is in umol/kg and we need mol/kg
  #PO41*10^-6

#siT is in umol/kg an we need mol/kg
  #Si1*10^-6

#P_dbar is in dbar and we need bar 
  #P_dbar/10 or P_dbar*10^-1

#We will need to convert Units scaling when using CO2sys

?carb #copy code from here
#we have TA y DIC What fag should we use? flag = 15 ALK and DIC given for option 1 and 2

#Option 1 on How to do with summarize
test=
  bats_co2 %>%
  filter(Alk!=-999, CO2!=-999, Sal1!=-999, Temp!=-999, P_dbar!=-999, PO41!=-999, Si1!=-999) %>%
    summarise(carb(flag=15, var1=Alk*10^-6, var2=CO2*10^-6, 
              S=Sal1, T=Temp, 
              Patm=1, P= P_dbar*10^-1, 
              Pt=PO41*10^-6, Sit=Si1*10^-6,
              k1k2="l", kf="pf", ks="d", pHscale="T", 
              b="u74", gas="potential", 
              warn="y", eos="eos80", 
              long=360-lonW, lat=latN))
view(test)
  
#Option 2 on How o do with mutate, use this one better to have the year and depth to calculate ocean acidification 
bats_co2sys=
 bats_co2 %>%
  filter(Alk!=-999, CO2!=-999, Sal1!=-999, Temp!=-999, P_dbar!=-999, PO41!=-999, Si1!=-999) %>%
  rename(DIC=CO2)%>% 
    mutate(carb(flag=15, var1=Alk*10^-6, var2=DIC*10^-6, 
       S=Sal1, T=Temp, 
       Patm=1, P= P_dbar*10^-1, 
       Pt=PO41*10^-6, Sit=Si1*10^-6,
       k1k2="l", kf="pf", ks="d", pHscale="T", 
       b="u74", gas="potential", 
       warn="y", eos="eos80", 
       long=360-lonW, lat=latN))
view(bats_co2sys)

plot(bats_co2sys$DIC,test$TA)

#filter for only the surface ocean
bats_co2sys_surf = bats_co2sys %>%
  filter(Depth<100) #we are selecting only the upper 100m
view(bats_co2sys_surf)

#1) Is surface ocean pco2 increasing
bats_co2sys_surf%>%
  ggplot(mapping = aes(x=decy,y=pCO2insitu))+
  geom_point()+
  geom_smooth()

check=bats_co2sys_surf%>%
  filter(pCO2<200)

pCO2_model = lm(pCO2insitu~decy, data=bats_co2sys_surf)
summary(pCO2_model)

#For homework
#Make decy vs pco2 plot pretty
#could include methods text for co2sys in lab report

#For Thursday:
#How do we check our model performance
#How do we plot our model predictions?


#2) Is surface ocean seawater ph decreasing? 
#3) Is surface ocean seawater saturation state with respect to aragonite decreasing?
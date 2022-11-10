# Package ID: edi.200.12 Cataloging System:https://pasta.edirepository.org.
# Data set title: Time series of high-frequency profiles of depth, temperature, dissolved oxygen, conductivity, specific conductance, chlorophyll a, turbidity, pH, oxidation-reduction potential, photosynthetic active radiation, and descent rate for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in Southwestern Virginia, USA 2013-2021.
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Abigail Lewis - Virginia Tech 
# Data set creator:  Ryan McClure - Virginia Tech 
# Data set creator:  Alexandra Gerling - Virginia Tech 
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech 
# Data set creator:  Arpita Das - Virginia Tech 
# Contact:  Cayelan Carey -  Virginia Tech  - cayelan@vt.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/12/0a62d1946e8d9a511bc1404e69e59b8c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt7 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "Date",     
                 "Depth_m",     
                 "Temp_C",     
                 "DO_mgL",     
                 "DO_pSat",     
                 "Cond_uScm",     
                 "Spec_Cond_uScm",     
                 "Chla_ugL",     
                 "Turb_NTU",     
                 "pH",     
                 "ORP_mV",     
                 "PAR_umolm2s",     
                 "Desc_rate",     
                 "Flag_Temp",     
                 "Flag_DO",     
                 "Flag_Cond",     
                 "Flag_SpecCond",     
                 "Flag_Chla",     
                 "Flag_Turb",     
                 "Flag_pH",     
                 "Flag_ORP",     
                 "Flag_PAR",     
                 "Flag_DescRate"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt7$Reservoir)!="factor") dt7$Reservoir<- as.factor(dt7$Reservoir)
if (class(dt7$Site)=="factor") dt7$Site <-as.numeric(levels(dt7$Site))[as.integer(dt7$Site) ]               
if (class(dt7$Site)=="character") dt7$Site <-as.numeric(dt7$Site)                                   
# attempting to convert dt7$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1Date<-as.POSIXct(dt7$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt7$Date <- tmp1Date } else {print("Date conversion failed for dt7$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt7$Depth_m)=="factor") dt7$Depth_m <-as.numeric(levels(dt7$Depth_m))[as.integer(dt7$Depth_m) ]               
if (class(dt7$Depth_m)=="character") dt7$Depth_m <-as.numeric(dt7$Depth_m)
if (class(dt7$Temp_C)=="factor") dt7$Temp_C <-as.numeric(levels(dt7$Temp_C))[as.integer(dt7$Temp_C) ]               
if (class(dt7$Temp_C)=="character") dt7$Temp_C <-as.numeric(dt7$Temp_C)
if (class(dt7$DO_mgL)=="factor") dt7$DO_mgL <-as.numeric(levels(dt7$DO_mgL))[as.integer(dt7$DO_mgL) ]               
if (class(dt7$DO_mgL)=="character") dt7$DO_mgL <-as.numeric(dt7$DO_mgL)
if (class(dt7$DO_pSat)=="factor") dt7$DO_pSat <-as.numeric(levels(dt7$DO_pSat))[as.integer(dt7$DO_pSat) ]               
if (class(dt7$DO_pSat)=="character") dt7$DO_pSat <-as.numeric(dt7$DO_pSat)
if (class(dt7$Cond_uScm)=="factor") dt7$Cond_uScm <-as.numeric(levels(dt7$Cond_uScm))[as.integer(dt7$Cond_uScm) ]               
if (class(dt7$Cond_uScm)=="character") dt7$Cond_uScm <-as.numeric(dt7$Cond_uScm)
if (class(dt7$Spec_Cond_uScm)=="factor") dt7$Spec_Cond_uScm <-as.numeric(levels(dt7$Spec_Cond_uScm))[as.integer(dt7$Spec_Cond_uScm) ]               
if (class(dt7$Spec_Cond_uScm)=="character") dt7$Spec_Cond_uScm <-as.numeric(dt7$Spec_Cond_uScm)
if (class(dt7$Chla_ugL)=="factor") dt7$Chla_ugL <-as.numeric(levels(dt7$Chla_ugL))[as.integer(dt7$Chla_ugL) ]               
if (class(dt7$Chla_ugL)=="character") dt7$Chla_ugL <-as.numeric(dt7$Chla_ugL)
if (class(dt7$Turb_NTU)=="factor") dt7$Turb_NTU <-as.numeric(levels(dt7$Turb_NTU))[as.integer(dt7$Turb_NTU) ]               
if (class(dt7$Turb_NTU)=="character") dt7$Turb_NTU <-as.numeric(dt7$Turb_NTU)
if (class(dt7$pH)=="factor") dt7$pH <-as.numeric(levels(dt7$pH))[as.integer(dt7$pH) ]               
if (class(dt7$pH)=="character") dt7$pH <-as.numeric(dt7$pH)
if (class(dt7$ORP_mV)=="factor") dt7$ORP_mV <-as.numeric(levels(dt7$ORP_mV))[as.integer(dt7$ORP_mV) ]               
if (class(dt7$ORP_mV)=="character") dt7$ORP_mV <-as.numeric(dt7$ORP_mV)
if (class(dt7$PAR_umolm2s)=="factor") dt7$PAR_umolm2s <-as.numeric(levels(dt7$PAR_umolm2s))[as.integer(dt7$PAR_umolm2s) ]               
if (class(dt7$PAR_umolm2s)=="character") dt7$PAR_umolm2s <-as.numeric(dt7$PAR_umolm2s)
if (class(dt7$Desc_rate)=="factor") dt7$Desc_rate <-as.numeric(levels(dt7$Desc_rate))[as.integer(dt7$Desc_rate) ]               
if (class(dt7$Desc_rate)=="character") dt7$Desc_rate <-as.numeric(dt7$Desc_rate)
if (class(dt7$Flag_Temp)=="factor") dt7$Flag_Temp <-as.numeric(levels(dt7$Flag_Temp))[as.integer(dt7$Flag_Temp) ]               
if (class(dt7$Flag_Temp)=="character") dt7$Flag_Temp <-as.numeric(dt7$Flag_Temp)
if (class(dt7$Flag_DO)=="factor") dt7$Flag_DO <-as.numeric(levels(dt7$Flag_DO))[as.integer(dt7$Flag_DO) ]               
if (class(dt7$Flag_DO)=="character") dt7$Flag_DO <-as.numeric(dt7$Flag_DO)
if (class(dt7$Flag_Cond)=="factor") dt7$Flag_Cond <-as.numeric(levels(dt7$Flag_Cond))[as.integer(dt7$Flag_Cond) ]               
if (class(dt7$Flag_Cond)=="character") dt7$Flag_Cond <-as.numeric(dt7$Flag_Cond)
if (class(dt7$Flag_SpecCond)=="factor") dt7$Flag_SpecCond <-as.numeric(levels(dt7$Flag_SpecCond))[as.integer(dt7$Flag_SpecCond) ]               
if (class(dt7$Flag_SpecCond)=="character") dt7$Flag_SpecCond <-as.numeric(dt7$Flag_SpecCond)
if (class(dt7$Flag_Chla)=="factor") dt7$Flag_Chla <-as.numeric(levels(dt7$Flag_Chla))[as.integer(dt7$Flag_Chla) ]               
if (class(dt7$Flag_Chla)=="character") dt7$Flag_Chla <-as.numeric(dt7$Flag_Chla)
if (class(dt7$Flag_Turb)=="factor") dt7$Flag_Turb <-as.numeric(levels(dt7$Flag_Turb))[as.integer(dt7$Flag_Turb) ]               
if (class(dt7$Flag_Turb)=="character") dt7$Flag_Turb <-as.numeric(dt7$Flag_Turb)
if (class(dt7$Flag_pH)=="factor") dt7$Flag_pH <-as.numeric(levels(dt7$Flag_pH))[as.integer(dt7$Flag_pH) ]               
if (class(dt7$Flag_pH)=="character") dt7$Flag_pH <-as.numeric(dt7$Flag_pH)
if (class(dt7$Flag_ORP)=="factor") dt7$Flag_ORP <-as.numeric(levels(dt7$Flag_ORP))[as.integer(dt7$Flag_ORP) ]               
if (class(dt7$Flag_ORP)=="character") dt7$Flag_ORP <-as.numeric(dt7$Flag_ORP)
if (class(dt7$Flag_PAR)=="factor") dt7$Flag_PAR <-as.numeric(levels(dt7$Flag_PAR))[as.integer(dt7$Flag_PAR) ]               
if (class(dt7$Flag_PAR)=="character") dt7$Flag_PAR <-as.numeric(dt7$Flag_PAR)
if (class(dt7$Flag_DescRate)=="factor") dt7$Flag_DescRate <-as.numeric(levels(dt7$Flag_DescRate))[as.integer(dt7$Flag_DescRate) ]               
if (class(dt7$Flag_DescRate)=="character") dt7$Flag_DescRate <-as.numeric(dt7$Flag_DescRate)

# Convert Missing Values to NA for non-dates

dt7$Depth_m <- ifelse((trimws(as.character(dt7$Depth_m))==trimws("NA")),NA,dt7$Depth_m)               
suppressWarnings(dt7$Depth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$Depth_m))==as.character(as.numeric("NA"))),NA,dt7$Depth_m))
dt7$Temp_C <- ifelse((trimws(as.character(dt7$Temp_C))==trimws("NA")),NA,dt7$Temp_C)               
suppressWarnings(dt7$Temp_C <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$Temp_C))==as.character(as.numeric("NA"))),NA,dt7$Temp_C))
dt7$DO_mgL <- ifelse((trimws(as.character(dt7$DO_mgL))==trimws("NA")),NA,dt7$DO_mgL)               
suppressWarnings(dt7$DO_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$DO_mgL))==as.character(as.numeric("NA"))),NA,dt7$DO_mgL))
dt7$DO_pSat <- ifelse((trimws(as.character(dt7$DO_pSat))==trimws("NA")),NA,dt7$DO_pSat)               
suppressWarnings(dt7$DO_pSat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$DO_pSat))==as.character(as.numeric("NA"))),NA,dt7$DO_pSat))
dt7$Cond_uScm <- ifelse((trimws(as.character(dt7$Cond_uScm))==trimws("NA")),NA,dt7$Cond_uScm)               
suppressWarnings(dt7$Cond_uScm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$Cond_uScm))==as.character(as.numeric("NA"))),NA,dt7$Cond_uScm))
dt7$Spec_Cond_uScm <- ifelse((trimws(as.character(dt7$Spec_Cond_uScm))==trimws("NA")),NA,dt7$Spec_Cond_uScm)               
suppressWarnings(dt7$Spec_Cond_uScm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$Spec_Cond_uScm))==as.character(as.numeric("NA"))),NA,dt7$Spec_Cond_uScm))
dt7$Chla_ugL <- ifelse((trimws(as.character(dt7$Chla_ugL))==trimws("NA")),NA,dt7$Chla_ugL)               
suppressWarnings(dt7$Chla_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$Chla_ugL))==as.character(as.numeric("NA"))),NA,dt7$Chla_ugL))
dt7$Turb_NTU <- ifelse((trimws(as.character(dt7$Turb_NTU))==trimws("NA")),NA,dt7$Turb_NTU)               
suppressWarnings(dt7$Turb_NTU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$Turb_NTU))==as.character(as.numeric("NA"))),NA,dt7$Turb_NTU))
dt7$pH <- ifelse((trimws(as.character(dt7$pH))==trimws("NA")),NA,dt7$pH)               
suppressWarnings(dt7$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$pH))==as.character(as.numeric("NA"))),NA,dt7$pH))
dt7$ORP_mV <- ifelse((trimws(as.character(dt7$ORP_mV))==trimws("NA")),NA,dt7$ORP_mV)               
suppressWarnings(dt7$ORP_mV <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$ORP_mV))==as.character(as.numeric("NA"))),NA,dt7$ORP_mV))
dt7$PAR_umolm2s <- ifelse((trimws(as.character(dt7$PAR_umolm2s))==trimws("NA")),NA,dt7$PAR_umolm2s)               
suppressWarnings(dt7$PAR_umolm2s <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$PAR_umolm2s))==as.character(as.numeric("NA"))),NA,dt7$PAR_umolm2s))
dt7$Desc_rate <- ifelse((trimws(as.character(dt7$Desc_rate))==trimws("NA")),NA,dt7$Desc_rate)               
suppressWarnings(dt7$Desc_rate <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt7$Desc_rate))==as.character(as.numeric("NA"))),NA,dt7$Desc_rate))


# Here is the structure of the input data frame:
str(dt7)                            
attach(dt7)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Reservoir)
summary(Site)
summary(Date)
summary(Depth_m)
summary(Temp_C)
summary(DO_mgL)
summary(DO_pSat)
summary(Cond_uScm)
summary(Spec_Cond_uScm)
summary(Chla_ugL)
summary(Turb_NTU)
summary(pH)
summary(ORP_mV)
summary(PAR_umolm2s)
summary(Desc_rate)
summary(Flag_Temp)
summary(Flag_DO)
summary(Flag_Cond)
summary(Flag_SpecCond)
summary(Flag_Chla)
summary(Flag_Turb)
summary(Flag_pH)
summary(Flag_ORP)
summary(Flag_PAR)
summary(Flag_DescRate) 
# Get more details on character variables

summary(as.factor(dt7$Reservoir))
detach(dt7)               






# Package ID: edi.198.10 Cataloging System:https://pasta.edirepository.org.
# Data set title: Secchi depth data and discrete depth profiles of photosynthetically active radiation, temperature, dissolved oxygen, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2021.
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Heather Wander - Virginia Tech 
# Data set creator:  Ryan McClure - Virginia Tech 
# Data set creator:  Mary Lofton - Virginia Tech 
# Data set creator:  Kathleen Hamre - Virginia Tech 
# Data set creator:  Jonathan Doubek - Virginia Tech 
# Data set creator:  Alexandra Gerling - Virginia Tech 
# Data set creator:  Abigail Lewis - Virginia Tech 
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech 
# Contact:  Cayelan Carey -  Virginia Tech  - cayelan@vt.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/198/10/375f87747001e1681b0e805d00cc1341" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt5 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "Secchi_m",     
                 "Flag_DateTime",     
                 "Flag_Secchi"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$Reservoir)!="factor") dt5$Reservoir<- as.factor(dt5$Reservoir)
if (class(dt5$Site)=="factor") dt5$Site <-as.numeric(levels(dt5$Site))[as.integer(dt5$Site) ]               
if (class(dt5$Site)=="character") dt5$Site <-as.numeric(dt5$Site)                                   
# attempting to convert dt5$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1DateTime<-as.POSIXct(dt5$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DateTime) == length(tmp1DateTime[!is.na(tmp1DateTime)])){dt5$DateTime <- tmp1DateTime } else {print("Date conversion failed for dt5$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DateTime) 
if (class(dt5$Secchi_m)=="factor") dt5$Secchi_m <-as.numeric(levels(dt5$Secchi_m))[as.integer(dt5$Secchi_m) ]               
if (class(dt5$Secchi_m)=="character") dt5$Secchi_m <-as.numeric(dt5$Secchi_m)
if (class(dt5$Flag_DateTime)=="factor") dt5$Flag_DateTime <-as.numeric(levels(dt5$Flag_DateTime))[as.integer(dt5$Flag_DateTime) ]               
if (class(dt5$Flag_DateTime)=="character") dt5$Flag_DateTime <-as.numeric(dt5$Flag_DateTime)
if (class(dt5$Flag_Secchi)=="factor") dt5$Flag_Secchi <-as.numeric(levels(dt5$Flag_Secchi))[as.integer(dt5$Flag_Secchi) ]               
if (class(dt5$Flag_Secchi)=="character") dt5$Flag_Secchi <-as.numeric(dt5$Flag_Secchi)

# Convert Missing Values to NA for non-dates

dt5$Reservoir <- as.factor(ifelse((trimws(as.character(dt5$Reservoir))==trimws("NA")),NA,as.character(dt5$Reservoir)))
dt5$Site <- ifelse((trimws(as.character(dt5$Site))==trimws("NA")),NA,dt5$Site)               
suppressWarnings(dt5$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$Site))==as.character(as.numeric("NA"))),NA,dt5$Site))
dt5$Secchi_m <- ifelse((trimws(as.character(dt5$Secchi_m))==trimws("NA")),NA,dt5$Secchi_m)               
suppressWarnings(dt5$Secchi_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$Secchi_m))==as.character(as.numeric("NA"))),NA,dt5$Secchi_m))


# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Reservoir)
summary(Site)
summary(DateTime)
summary(Secchi_m)
summary(Flag_DateTime)
summary(Flag_Secchi) 
# Get more details on character variables

summary(as.factor(dt5$Reservoir))
detach(dt5)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/198/10/b3bd353312f9e37ca392e2a5315cc9da" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt6 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "Depth_m",     
                 "Temp_C",     
                 "DO_mgL",     
                 "DOSat",     
                 "Cond_uScm",     
                 "Sp_cond_uScm",     
                 "PAR_umolm2s",     
                 "ORP_mV",     
                 "pH",     
                 "Flag_DateTime",     
                 "Flag_Temp",     
                 "Flag_DO",     
                 "Flag_DOSat",     
                 "Flag_Cond",     
                 "Flag_Sp_Cond",     
                 "Flag_PAR",     
                 "Flag_ORP",     
                 "Flag_pH"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt6$Reservoir)!="factor") dt6$Reservoir<- as.factor(dt6$Reservoir)
if (class(dt6$Site)=="factor") dt6$Site <-as.numeric(levels(dt6$Site))[as.integer(dt6$Site) ]               
if (class(dt6$Site)=="character") dt6$Site <-as.numeric(dt6$Site)                                   
# attempting to convert dt6$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp2DateTime<-as.POSIXct(dt6$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2DateTime) == length(tmp2DateTime[!is.na(tmp2DateTime)])){dt6$DateTime <- tmp2DateTime } else {print("Date conversion failed for dt6$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2DateTime) 
if (class(dt6$Depth_m)=="factor") dt6$Depth_m <-as.numeric(levels(dt6$Depth_m))[as.integer(dt6$Depth_m) ]               
if (class(dt6$Depth_m)=="character") dt6$Depth_m <-as.numeric(dt6$Depth_m)
if (class(dt6$Temp_C)=="factor") dt6$Temp_C <-as.numeric(levels(dt6$Temp_C))[as.integer(dt6$Temp_C) ]               
if (class(dt6$Temp_C)=="character") dt6$Temp_C <-as.numeric(dt6$Temp_C)
if (class(dt6$DO_mgL)=="factor") dt6$DO_mgL <-as.numeric(levels(dt6$DO_mgL))[as.integer(dt6$DO_mgL) ]               
if (class(dt6$DO_mgL)=="character") dt6$DO_mgL <-as.numeric(dt6$DO_mgL)
if (class(dt6$DOSat)=="factor") dt6$DOSat <-as.numeric(levels(dt6$DOSat))[as.integer(dt6$DOSat) ]               
if (class(dt6$DOSat)=="character") dt6$DOSat <-as.numeric(dt6$DOSat)
if (class(dt6$Cond_uScm)=="factor") dt6$Cond_uScm <-as.numeric(levels(dt6$Cond_uScm))[as.integer(dt6$Cond_uScm) ]               
if (class(dt6$Cond_uScm)=="character") dt6$Cond_uScm <-as.numeric(dt6$Cond_uScm)
if (class(dt6$Sp_cond_uScm)=="factor") dt6$Sp_cond_uScm <-as.numeric(levels(dt6$Sp_cond_uScm))[as.integer(dt6$Sp_cond_uScm) ]               
if (class(dt6$Sp_cond_uScm)=="character") dt6$Sp_cond_uScm <-as.numeric(dt6$Sp_cond_uScm)
if (class(dt6$PAR_umolm2s)=="factor") dt6$PAR_umolm2s <-as.numeric(levels(dt6$PAR_umolm2s))[as.integer(dt6$PAR_umolm2s) ]               
if (class(dt6$PAR_umolm2s)=="character") dt6$PAR_umolm2s <-as.numeric(dt6$PAR_umolm2s)
if (class(dt6$ORP_mV)=="factor") dt6$ORP_mV <-as.numeric(levels(dt6$ORP_mV))[as.integer(dt6$ORP_mV) ]               
if (class(dt6$ORP_mV)=="character") dt6$ORP_mV <-as.numeric(dt6$ORP_mV)
if (class(dt6$pH)=="factor") dt6$pH <-as.numeric(levels(dt6$pH))[as.integer(dt6$pH) ]               
if (class(dt6$pH)=="character") dt6$pH <-as.numeric(dt6$pH)
if (class(dt6$Flag_DateTime)=="factor") dt6$Flag_DateTime <-as.numeric(levels(dt6$Flag_DateTime))[as.integer(dt6$Flag_DateTime) ]               
if (class(dt6$Flag_DateTime)=="character") dt6$Flag_DateTime <-as.numeric(dt6$Flag_DateTime)
if (class(dt6$Flag_Temp)=="factor") dt6$Flag_Temp <-as.numeric(levels(dt6$Flag_Temp))[as.integer(dt6$Flag_Temp) ]               
if (class(dt6$Flag_Temp)=="character") dt6$Flag_Temp <-as.numeric(dt6$Flag_Temp)
if (class(dt6$Flag_DO)=="factor") dt6$Flag_DO <-as.numeric(levels(dt6$Flag_DO))[as.integer(dt6$Flag_DO) ]               
if (class(dt6$Flag_DO)=="character") dt6$Flag_DO <-as.numeric(dt6$Flag_DO)
if (class(dt6$Flag_DOSat)=="factor") dt6$Flag_DOSat <-as.numeric(levels(dt6$Flag_DOSat))[as.integer(dt6$Flag_DOSat) ]               
if (class(dt6$Flag_DOSat)=="character") dt6$Flag_DOSat <-as.numeric(dt6$Flag_DOSat)
if (class(dt6$Flag_Cond)=="factor") dt6$Flag_Cond <-as.numeric(levels(dt6$Flag_Cond))[as.integer(dt6$Flag_Cond) ]               
if (class(dt6$Flag_Cond)=="character") dt6$Flag_Cond <-as.numeric(dt6$Flag_Cond)
if (class(dt6$Flag_Sp_Cond)=="factor") dt6$Flag_Sp_Cond <-as.numeric(levels(dt6$Flag_Sp_Cond))[as.integer(dt6$Flag_Sp_Cond) ]               
if (class(dt6$Flag_Sp_Cond)=="character") dt6$Flag_Sp_Cond <-as.numeric(dt6$Flag_Sp_Cond)
if (class(dt6$Flag_PAR)=="factor") dt6$Flag_PAR <-as.numeric(levels(dt6$Flag_PAR))[as.integer(dt6$Flag_PAR) ]               
if (class(dt6$Flag_PAR)=="character") dt6$Flag_PAR <-as.numeric(dt6$Flag_PAR)
if (class(dt6$Flag_ORP)=="factor") dt6$Flag_ORP <-as.numeric(levels(dt6$Flag_ORP))[as.integer(dt6$Flag_ORP) ]               
if (class(dt6$Flag_ORP)=="character") dt6$Flag_ORP <-as.numeric(dt6$Flag_ORP)
if (class(dt6$Flag_pH)=="factor") dt6$Flag_pH <-as.numeric(levels(dt6$Flag_pH))[as.integer(dt6$Flag_pH) ]               
if (class(dt6$Flag_pH)=="character") dt6$Flag_pH <-as.numeric(dt6$Flag_pH)

# Convert Missing Values to NA for non-dates

dt6$Reservoir <- as.factor(ifelse((trimws(as.character(dt6$Reservoir))==trimws("NA")),NA,as.character(dt6$Reservoir)))
dt6$Site <- ifelse((trimws(as.character(dt6$Site))==trimws("NA")),NA,dt6$Site)               
suppressWarnings(dt6$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Site))==as.character(as.numeric("NA"))),NA,dt6$Site))
dt6$Depth_m <- ifelse((trimws(as.character(dt6$Depth_m))==trimws("NA")),NA,dt6$Depth_m)               
suppressWarnings(dt6$Depth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Depth_m))==as.character(as.numeric("NA"))),NA,dt6$Depth_m))
dt6$Temp_C <- ifelse((trimws(as.character(dt6$Temp_C))==trimws("NA")),NA,dt6$Temp_C)               
suppressWarnings(dt6$Temp_C <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Temp_C))==as.character(as.numeric("NA"))),NA,dt6$Temp_C))
dt6$DO_mgL <- ifelse((trimws(as.character(dt6$DO_mgL))==trimws("NA")),NA,dt6$DO_mgL)               
suppressWarnings(dt6$DO_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$DO_mgL))==as.character(as.numeric("NA"))),NA,dt6$DO_mgL))
dt6$DOSat <- ifelse((trimws(as.character(dt6$DOSat))==trimws("NA")),NA,dt6$DOSat)               
suppressWarnings(dt6$DOSat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$DOSat))==as.character(as.numeric("NA"))),NA,dt6$DOSat))
dt6$Cond_uScm <- ifelse((trimws(as.character(dt6$Cond_uScm))==trimws("NA")),NA,dt6$Cond_uScm)               
suppressWarnings(dt6$Cond_uScm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Cond_uScm))==as.character(as.numeric("NA"))),NA,dt6$Cond_uScm))
dt6$Sp_cond_uScm <- ifelse((trimws(as.character(dt6$Sp_cond_uScm))==trimws("NA")),NA,dt6$Sp_cond_uScm)               
suppressWarnings(dt6$Sp_cond_uScm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Sp_cond_uScm))==as.character(as.numeric("NA"))),NA,dt6$Sp_cond_uScm))
dt6$PAR_umolm2s <- ifelse((trimws(as.character(dt6$PAR_umolm2s))==trimws("NA")),NA,dt6$PAR_umolm2s)               
suppressWarnings(dt6$PAR_umolm2s <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$PAR_umolm2s))==as.character(as.numeric("NA"))),NA,dt6$PAR_umolm2s))
dt6$ORP_mV <- ifelse((trimws(as.character(dt6$ORP_mV))==trimws("NA")),NA,dt6$ORP_mV)               
suppressWarnings(dt6$ORP_mV <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$ORP_mV))==as.character(as.numeric("NA"))),NA,dt6$ORP_mV))
dt6$pH <- ifelse((trimws(as.character(dt6$pH))==trimws("NA")),NA,dt6$pH)               
suppressWarnings(dt6$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$pH))==as.character(as.numeric("NA"))),NA,dt6$pH))
dt6$Flag_DateTime <- ifelse((trimws(as.character(dt6$Flag_DateTime))==trimws("NA")),NA,dt6$Flag_DateTime)               
suppressWarnings(dt6$Flag_DateTime <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Flag_DateTime))==as.character(as.numeric("NA"))),NA,dt6$Flag_DateTime))
dt6$Flag_Temp <- ifelse((trimws(as.character(dt6$Flag_Temp))==trimws("NA")),NA,dt6$Flag_Temp)               
suppressWarnings(dt6$Flag_Temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Flag_Temp))==as.character(as.numeric("NA"))),NA,dt6$Flag_Temp))
dt6$Flag_DO <- ifelse((trimws(as.character(dt6$Flag_DO))==trimws("NA")),NA,dt6$Flag_DO)               
suppressWarnings(dt6$Flag_DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Flag_DO))==as.character(as.numeric("NA"))),NA,dt6$Flag_DO))
dt6$Flag_DOSat <- ifelse((trimws(as.character(dt6$Flag_DOSat))==trimws("NA")),NA,dt6$Flag_DOSat)               
suppressWarnings(dt6$Flag_DOSat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Flag_DOSat))==as.character(as.numeric("NA"))),NA,dt6$Flag_DOSat))
dt6$Flag_Cond <- ifelse((trimws(as.character(dt6$Flag_Cond))==trimws("NA")),NA,dt6$Flag_Cond)               
suppressWarnings(dt6$Flag_Cond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Flag_Cond))==as.character(as.numeric("NA"))),NA,dt6$Flag_Cond))
dt6$Flag_Sp_Cond <- ifelse((trimws(as.character(dt6$Flag_Sp_Cond))==trimws("NA")),NA,dt6$Flag_Sp_Cond)               
suppressWarnings(dt6$Flag_Sp_Cond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Flag_Sp_Cond))==as.character(as.numeric("NA"))),NA,dt6$Flag_Sp_Cond))
dt6$Flag_PAR <- ifelse((trimws(as.character(dt6$Flag_PAR))==trimws("NA")),NA,dt6$Flag_PAR)               
suppressWarnings(dt6$Flag_PAR <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Flag_PAR))==as.character(as.numeric("NA"))),NA,dt6$Flag_PAR))
dt6$Flag_ORP <- ifelse((trimws(as.character(dt6$Flag_ORP))==trimws("NA")),NA,dt6$Flag_ORP)               
suppressWarnings(dt6$Flag_ORP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Flag_ORP))==as.character(as.numeric("NA"))),NA,dt6$Flag_ORP))
dt6$Flag_pH <- ifelse((trimws(as.character(dt6$Flag_pH))==trimws("NA")),NA,dt6$Flag_pH)               
suppressWarnings(dt6$Flag_pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Flag_pH))==as.character(as.numeric("NA"))),NA,dt6$Flag_pH))


# Here is the structure of the input data frame:
str(dt6)                            
attach(dt6)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Reservoir)
summary(Site)
summary(DateTime)
summary(Depth_m)
summary(Temp_C)
summary(DO_mgL)
summary(DOSat)
summary(Cond_uScm)
summary(Sp_cond_uScm)
summary(PAR_umolm2s)
summary(ORP_mV)
summary(pH)
summary(Flag_DateTime)
summary(Flag_Temp)
summary(Flag_DO)
summary(Flag_DOSat)
summary(Flag_Cond)
summary(Flag_Sp_Cond)
summary(Flag_PAR)
summary(Flag_ORP)
summary(Flag_pH) 
# Get more details on character variables

summary(as.factor(dt6$Reservoir))
detach(dt6)               






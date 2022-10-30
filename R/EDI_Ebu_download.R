# Package ID: edi.440.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Methane ebullition and diffusion rates, turbulence, water temperature, and water depth data from Falling Creek Reservoir (Virginia, USA) in the ice-free period during 2016-2019.
# Data set creator:  Ryan McClure - Virginia Tech 
# Data set creator:  Mary Lofton - Virginia Tech 
# Data set creator:  Shengyang Chen - UNSW Water Research Laboratory 
# Data set creator:  Kathryn Krueger - Virginia Tech 
# Data set creator:  John Little - Virginia Tech 
# Data set creator:  Cayelan Carey - Virginia Tech 
# Contact:  Ryan McClure -  Virginia Tech  - ryan333@vt.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/440/2/64ddedbed8338b628f0ec88d0746bb32" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "Transect",     
                 "DateTime",     
                 "Depth_m",     
                 "Ebu_rate",     
                 "Diff_rate",     
                 "Flag_depth",     
                 "Flag_ebu",     
                 "Flag_diff"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Reservoir)!="factor") dt1$Reservoir<- as.factor(dt1$Reservoir)
if (class(dt1$Site)!="factor") dt1$Site<- as.factor(dt1$Site)
if (class(dt1$Transect)!="factor") dt1$Transect<- as.factor(dt1$Transect)                                   
# attempting to convert dt1$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DateTime<-as.Date(dt1$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DateTime) == length(tmp1DateTime[!is.na(tmp1DateTime)])){dt1$DateTime <- tmp1DateTime } else {print("Date conversion failed for dt1$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DateTime) 
if (class(dt1$Depth_m)=="factor") dt1$Depth_m <-as.numeric(levels(dt1$Depth_m))[as.integer(dt1$Depth_m) ]               
if (class(dt1$Depth_m)=="character") dt1$Depth_m <-as.numeric(dt1$Depth_m)
if (class(dt1$Ebu_rate)=="factor") dt1$Ebu_rate <-as.numeric(levels(dt1$Ebu_rate))[as.integer(dt1$Ebu_rate) ]               
if (class(dt1$Ebu_rate)=="character") dt1$Ebu_rate <-as.numeric(dt1$Ebu_rate)
if (class(dt1$Diff_rate)=="factor") dt1$Diff_rate <-as.numeric(levels(dt1$Diff_rate))[as.integer(dt1$Diff_rate) ]               
if (class(dt1$Diff_rate)=="character") dt1$Diff_rate <-as.numeric(dt1$Diff_rate)
if (class(dt1$Flag_depth)=="factor") dt1$Flag_depth <-as.numeric(levels(dt1$Flag_depth))[as.integer(dt1$Flag_depth) ]               
if (class(dt1$Flag_depth)=="character") dt1$Flag_depth <-as.numeric(dt1$Flag_depth)
if (class(dt1$Flag_ebu)=="factor") dt1$Flag_ebu <-as.numeric(levels(dt1$Flag_ebu))[as.integer(dt1$Flag_ebu) ]               
if (class(dt1$Flag_ebu)=="character") dt1$Flag_ebu <-as.numeric(dt1$Flag_ebu)
if (class(dt1$Flag_diff)=="factor") dt1$Flag_diff <-as.numeric(levels(dt1$Flag_diff))[as.integer(dt1$Flag_diff) ]               
if (class(dt1$Flag_diff)=="character") dt1$Flag_diff <-as.numeric(dt1$Flag_diff)

# Convert Missing Values to NA for non-dates

dt1$Reservoir <- as.factor(ifelse((trimws(as.character(dt1$Reservoir))==trimws("NA")),NA,as.character(dt1$Reservoir)))
dt1$Site <- as.factor(ifelse((trimws(as.character(dt1$Site))==trimws("NA")),NA,as.character(dt1$Site)))
dt1$Transect <- as.factor(ifelse((trimws(as.character(dt1$Transect))==trimws("NA")),NA,as.character(dt1$Transect)))
dt1$Depth_m <- ifelse((trimws(as.character(dt1$Depth_m))==trimws("NA")),NA,dt1$Depth_m)               
suppressWarnings(dt1$Depth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Depth_m))==as.character(as.numeric("NA"))),NA,dt1$Depth_m))
dt1$Ebu_rate <- ifelse((trimws(as.character(dt1$Ebu_rate))==trimws("NA")),NA,dt1$Ebu_rate)               
suppressWarnings(dt1$Ebu_rate <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Ebu_rate))==as.character(as.numeric("NA"))),NA,dt1$Ebu_rate))
dt1$Diff_rate <- ifelse((trimws(as.character(dt1$Diff_rate))==trimws("NA")),NA,dt1$Diff_rate)               
suppressWarnings(dt1$Diff_rate <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Diff_rate))==as.character(as.numeric("NA"))),NA,dt1$Diff_rate))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Reservoir)
summary(Site)
summary(Transect)
summary(DateTime)
summary(Depth_m)
summary(Ebu_rate)
summary(Diff_rate)
summary(Flag_depth)
summary(Flag_ebu)
summary(Flag_diff) 
# Get more details on character variables

summary(as.factor(dt1$Reservoir)) 
summary(as.factor(dt1$Site)) 
summary(as.factor(dt1$Transect))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/440/2/ee7a8e262285ed49ea1b4e4ae11525bd" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "Transect",     
                 "DateTime",     
                 "Sed_temp",     
                 "Surf_temp",     
                 "Flag_sed_temp",     
                 "Flag_surf_temp"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Reservoir)!="factor") dt2$Reservoir<- as.factor(dt2$Reservoir)
if (class(dt2$Site)!="factor") dt2$Site<- as.factor(dt2$Site)
if (class(dt2$Transect)!="factor") dt2$Transect<- as.factor(dt2$Transect)                                   
# attempting to convert dt2$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%m:SS" 
tmp2DateTime<-as.POSIXct(dt2$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2DateTime) == length(tmp2DateTime[!is.na(tmp2DateTime)])){dt2$DateTime <- tmp2DateTime } else {print("Date conversion failed for dt2$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2DateTime) 
if (class(dt2$Sed_temp)=="factor") dt2$Sed_temp <-as.numeric(levels(dt2$Sed_temp))[as.integer(dt2$Sed_temp) ]               
if (class(dt2$Sed_temp)=="character") dt2$Sed_temp <-as.numeric(dt2$Sed_temp)
if (class(dt2$Surf_temp)=="factor") dt2$Surf_temp <-as.numeric(levels(dt2$Surf_temp))[as.integer(dt2$Surf_temp) ]               
if (class(dt2$Surf_temp)=="character") dt2$Surf_temp <-as.numeric(dt2$Surf_temp)
if (class(dt2$Flag_sed_temp)=="factor") dt2$Flag_sed_temp <-as.numeric(levels(dt2$Flag_sed_temp))[as.integer(dt2$Flag_sed_temp) ]               
if (class(dt2$Flag_sed_temp)=="character") dt2$Flag_sed_temp <-as.numeric(dt2$Flag_sed_temp)
if (class(dt2$Flag_surf_temp)=="factor") dt2$Flag_surf_temp <-as.numeric(levels(dt2$Flag_surf_temp))[as.integer(dt2$Flag_surf_temp) ]               
if (class(dt2$Flag_surf_temp)=="character") dt2$Flag_surf_temp <-as.numeric(dt2$Flag_surf_temp)

# Convert Missing Values to NA for non-dates

dt2$Transect <- as.factor(ifelse((trimws(as.character(dt2$Transect))==trimws("NA")),NA,as.character(dt2$Transect)))
dt2$Sed_temp <- ifelse((trimws(as.character(dt2$Sed_temp))==trimws("NA")),NA,dt2$Sed_temp)               
suppressWarnings(dt2$Sed_temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Sed_temp))==as.character(as.numeric("NA"))),NA,dt2$Sed_temp))
dt2$Surf_temp <- ifelse((trimws(as.character(dt2$Surf_temp))==trimws("NA")),NA,dt2$Surf_temp)               
suppressWarnings(dt2$Surf_temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Surf_temp))==as.character(as.numeric("NA"))),NA,dt2$Surf_temp))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Reservoir)
summary(Site)
summary(Transect)
summary(DateTime)
summary(Sed_temp)
summary(Surf_temp)
summary(Flag_sed_temp)
summary(Flag_surf_temp) 
# Get more details on character variables

summary(as.factor(dt2$Reservoir)) 
summary(as.factor(dt2$Site)) 
summary(as.factor(dt2$Transect))
detach(dt2)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/440/2/7ae66c0543f4c17f03319682a4b137a1" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Transect",     
                 "DateTime",     
                 "Sed_turb",     
                 "Surf_turb",     
                 "Flag_sed_turb",     
                 "Flag_surf_turb"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Reservoir)!="factor") dt3$Reservoir<- as.factor(dt3$Reservoir)
if (class(dt3$Transect)!="factor") dt3$Transect<- as.factor(dt3$Transect)                                   
# attempting to convert dt3$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3DateTime<-as.Date(dt3$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3DateTime) == length(tmp3DateTime[!is.na(tmp3DateTime)])){dt3$DateTime <- tmp3DateTime } else {print("Date conversion failed for dt3$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3DateTime) 
if (class(dt3$Sed_turb)=="factor") dt3$Sed_turb <-as.numeric(levels(dt3$Sed_turb))[as.integer(dt3$Sed_turb) ]               
if (class(dt3$Sed_turb)=="character") dt3$Sed_turb <-as.numeric(dt3$Sed_turb)
if (class(dt3$Surf_turb)=="factor") dt3$Surf_turb <-as.numeric(levels(dt3$Surf_turb))[as.integer(dt3$Surf_turb) ]               
if (class(dt3$Surf_turb)=="character") dt3$Surf_turb <-as.numeric(dt3$Surf_turb)
if (class(dt3$Flag_sed_turb)=="factor") dt3$Flag_sed_turb <-as.numeric(levels(dt3$Flag_sed_turb))[as.integer(dt3$Flag_sed_turb) ]               
if (class(dt3$Flag_sed_turb)=="character") dt3$Flag_sed_turb <-as.numeric(dt3$Flag_sed_turb)
if (class(dt3$Flag_surf_turb)=="factor") dt3$Flag_surf_turb <-as.numeric(levels(dt3$Flag_surf_turb))[as.integer(dt3$Flag_surf_turb) ]               
if (class(dt3$Flag_surf_turb)=="character") dt3$Flag_surf_turb <-as.numeric(dt3$Flag_surf_turb)

# Convert Missing Values to NA for non-dates

dt3$Reservoir <- as.factor(ifelse((trimws(as.character(dt3$Reservoir))==trimws("NA")),NA,as.character(dt3$Reservoir)))
dt3$Transect <- as.factor(ifelse((trimws(as.character(dt3$Transect))==trimws("NA")),NA,as.character(dt3$Transect)))
dt3$Sed_turb <- ifelse((trimws(as.character(dt3$Sed_turb))==trimws("NA")),NA,dt3$Sed_turb)               
suppressWarnings(dt3$Sed_turb <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Sed_turb))==as.character(as.numeric("NA"))),NA,dt3$Sed_turb))
dt3$Surf_turb <- ifelse((trimws(as.character(dt3$Surf_turb))==trimws("NA")),NA,dt3$Surf_turb)               
suppressWarnings(dt3$Surf_turb <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Surf_turb))==as.character(as.numeric("NA"))),NA,dt3$Surf_turb))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Reservoir)
summary(Transect)
summary(DateTime)
summary(Sed_turb)
summary(Surf_turb)
summary(Flag_sed_turb)
summary(Flag_surf_turb) 
# Get more details on character variables

summary(as.factor(dt3$Reservoir)) 
summary(as.factor(dt3$Transect))
detach(dt3)               


inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/440/2/000aa2c878541fed86fc5358f27ae5fb" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "Transect",     
                 "Latitude",     
                 "Longitude"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$Reservoir)!="factor") dt4$Reservoir<- as.factor(dt4$Reservoir)
if (class(dt4$Site)!="factor") dt4$Site<- as.factor(dt4$Site)
if (class(dt4$Transect)!="factor") dt4$Transect<- as.factor(dt4$Transect)
if (class(dt4$Latitude)=="factor") dt4$Latitude <-as.numeric(levels(dt4$Latitude))[as.integer(dt4$Latitude) ]               
if (class(dt4$Latitude)=="character") dt4$Latitude <-as.numeric(dt4$Latitude)
if (class(dt4$Longitude)=="factor") dt4$Longitude <-as.numeric(levels(dt4$Longitude))[as.integer(dt4$Longitude) ]               
if (class(dt4$Longitude)=="character") dt4$Longitude <-as.numeric(dt4$Longitude)

# Convert Missing Values to NA for non-dates

dt4$Reservoir <- as.factor(ifelse((trimws(as.character(dt4$Reservoir))==trimws("NA")),NA,as.character(dt4$Reservoir)))
dt4$Site <- as.factor(ifelse((trimws(as.character(dt4$Site))==trimws("NA")),NA,as.character(dt4$Site)))
dt4$Transect <- as.factor(ifelse((trimws(as.character(dt4$Transect))==trimws("NA")),NA,as.character(dt4$Transect)))
dt4$Latitude <- ifelse((trimws(as.character(dt4$Latitude))==trimws("NA")),NA,dt4$Latitude)               
suppressWarnings(dt4$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$Latitude))==as.character(as.numeric("NA"))),NA,dt4$Latitude))
dt4$Longitude <- ifelse((trimws(as.character(dt4$Longitude))==trimws("NA")),NA,dt4$Longitude)               
suppressWarnings(dt4$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$Longitude))==as.character(as.numeric("NA"))),NA,dt4$Longitude))


# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Reservoir)
summary(Site)
summary(Transect)
summary(Latitude)
summary(Longitude) 
# Get more details on character variables

summary(as.factor(dt4$Reservoir)) 
summary(as.factor(dt4$Site)) 
summary(as.factor(dt4$Transect))
detach(dt4)               







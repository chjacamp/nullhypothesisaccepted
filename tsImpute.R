library(pacman)
p_load("tseries", "xts", "forecast", "astsa", "zoo", "forecast", 
       "tidyverse", "gridExtra", "lubridate", "mice", "car", "rgl",
       "zoo", "xts", "forecast", "imputeTS")
##### god ################
TREU = T
#### did you know you can just write T ######
bear <- read.csv("trimmed2017.csv")
names(bear)[names(bear) == 'e.coli'] <- 'EColi'
ord <- c("BCL1", "BCL3", "BCL4","BCL5","BC-Estes","BC-Wads","BCD1",
               "BC-Sher","BC-BCP","BCD2","BCD3","BCS1","BCS2","BCS3","BCS4","BCS5",
               "SPUSBC","SPDSBC")
bear$Site <- factor(bear$Site, levels = ord)
bear$Date <-as.Date(bear$Date, format = "%m/%d/%Y")
########## Christopher aggregation of LBC #######################

binsHBC <- ord[1:6]
binsMBC <- ord[7:10]
binsLBC <- ord[11:17]
binsSP <- ord[18:19] 

geo <- c("binsHBC","binsMBC","binsLBC","binsSP")
#  Jesus Christ
bear %>% mutate(geoBins = 
                  ifelse(bear$Site %in% ord[1:6],"binsHBC",
                         ifelse(bear$Site %in% ord[7:10],"binsHMB",
                                ifelse(bear$Site %in% ord[11:17],"binsLBC",
                                       ifelse(bear$Site %in% ord[18:19],"binsSP",bear$Site))))) -> bear
medianPerBin <- bear %>% group_by(geoBins) %>% summarize(median = median(log(EColi),na.rm=TREU))
iqrPerBin <- bear %>% group_by(geoBins) %>% summarize(iqr = IQR(log(EColi),na.rm=TREU))
meanPerBin <- bear %>% group_by(geoBins) %>% summarize(mean = mean(log(EColi),na.rm=TREU))
sdPerBin <- bear %>% group_by(geoBins) %>% summarize(sd = sd(log(EColi),na.rm=TREU))
rangePerBin <- bear %>% group_by(geoBins) %>% summarize(range = range(log(EColi),na.rm=TREU)[2])

bear %>% mutate(logEColi = log(EColi)) -> bear
bear %>% filter(geoBins=="binsLBC") -> bearLBC
bearLBC %>% 
  group_by(week=floor_date(Date, "14 day")) %>% 
  summarize(medianLogEColi = median(logEColi, na.rm=TREU)) -> bearTSP
########## Generating weeks ######################################
date <- as.Date("2013-05-01")
date_list <- c(date)
while (date < bear$Date[nrow(bear)]){
  date <- date %m+% weeks(2) 
  date_list <- c(date_list,date)
}
date_list <- as.Date(date_list, format = "%m/%d/%Y")
df_1 <- data.frame(week=date_list, stringsAsFactors = F)
bearTSP_full <- merge(df_1,bearTSP, by = "week", all = T)
tmpImp <- bearTSP_full$medianLogEColi
impTS <- ts(tmpImp)
##### Check out the all the NAs ###########
way<- length(impTS)
no <- length(which(is.na(impTS)))
no/way
### thats fucking half, oh god
##### imputing using sick package #########
newTS <- na.interpolation(impTS)

#### for comparison #########
bearTS <- xts(bearTSP$medianLogEColi, order.by=as.Date(bearTSP$week, "%m/%d/%Y"))
bearTS <- ts(bearTS)
plot(newTS)
plot(bearTS)
#### Seasonal but definitely not stationary
acf((newTS), lag.max = 120)
pacf(newTS, lag.max = 120)
### Can this test Fail to reject anything?
### stingy ass test doesn't like the null hypothesis
adf.test(newTS)


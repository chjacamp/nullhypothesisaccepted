library(ggplot2)
library(dplyr)
library(lubridate)

# IMPORTANT IMPORTANT IMPORTANT
TREU = TRUE
# IMPORTANT IMPORTANT IMPORTANT

bear <- read.csv("trimmed2017.csv",header=TRUE)
names(bear)[names(bear) == 'e.coli'] <- 'EColi'
ord <-c("BCL1","BCL3","BCL4","WEC","BCL5","BC-Estes","BC-Wads","BCD1",
        "BC-Sher","BC-BCP","BCD2","BCD3","BCS1","BCS2","BCS3","BCS4",
        "BCS5","SPUSBC","SPDSBC")

bear$Date <- mdy(bear$Date)

bear$daysFromOrigin <- as.duration(interval(bear$Date[1],bear$Date))
bear$daysFromOrigin <- as.numeric(bear$daysFromOrigin, "days")

bear<- transform(bear,Site=factor(Site,levels=ord))


ggplot(bear,aes(x=bear$Date,y=bear$EColi)) +
  geom_point(alpha = 0.3) + facet_wrap(~Site, ncol=4) + theme_bw()

ggplot(data=bear[1:1665,],aes(x=bear$daysFromOrigin[1:1665], y=diff(bear$EColi))) + geom_line() + 
         geom_point(alpha=.3) + facet_wrap(~Site,ncol=4) + theme_bw()

##Create new factors for geographical binning


binsHBC <- ord[1:6]
binsMBC <- ord[7:10]
binsLBC <- ord[11:17]
binsSP <- ord[18:19] 

geo <- c("binsHBC","binsMBC","binsLBC","binsSP")
## Nesting like mama bird but all of her little bird children were horrible
## Cronenberg monsters.

bear %>% mutate(geoBins = 
      ifelse(bear$Site %in% ord[1:6],"binsHBC",
             ifelse(bear$Site %in% ord[7:10],"binsHMB",
                    ifelse(bear$Site %in% ord[11:17],"binsLBC",
                           ifelse(bear$Site %in% ord[18:19],"binsSP",bear$Site))))) -> bear 


## Table for values per site bin above 3 standard deviations
with(bear,table(geoBins))

medianPerBin <- bear %>% group_by(geoBins) %>% summarize(median = median(EColi,na.rm=TREU))
iqrPerBin <- bear %>% group_by(geoBins) %>% summarize(iqr = IQR((EColi),na.rm=TREU))
meanPerBin <- bear %>% group_by(geoBins) %>% summarize(mean = mean(log(EColi),na.rm=TREU))
sdPerBin <- bear %>% group_by(geoBins) %>% summarize(sd = sd(log(EColi),na.rm=TREU))
ggplot(bear, aes(log(EColi))) + geom_histogram() + facet_grid(.~geoBins)
ggplot(bear, aes(EColi)) + geom_histogram() + facet_grid(.~geoBins)

valuesAbove3SD <- c()

valuesAbove3SD[1] <- bear[bear$geoBins==geo[1],] %>%
  filter(EColi >= meanPerBin$mean[1]+3*sdPerBin$sd[1]) %>% 
  summarize(HBC = n())
valuesAbove3SD[2] <- bear[bear$geoBins==geo[2],] %>%
  filter(EColi >= meanPerBin$mean[2]+3*sdPerBin$sd[2]) %>% 
  summarize(MBC = n())
valuesAbove3SD[3] <- bear[bear$geoBins==geo[3],] %>%
  filter(EColi >= meanPerBin$mean[3]+3*sdPerBin$sd[3]) %>% 
  summarize(LBC = n())
valuesAbove3SD[4] <- bear[bear$geoBins==geo[4],] %>%
  filter(EColi >= meanPerBin$mean[4]+3*sdPerBin$sd[4]) %>% 
  summarize(Confluence = n())

valuesAbove3SD[2] <- bear[bear$geoBins==geo[2],] %>%
  filter(EColi >= meanPerBin$mean[2]+3*sdPerBin$sd[2]) %>% 
  summarize(MBC = n())


valuesAbove3SD[3] <- bear[bear$geoBins==geo[3],] %>%
  filter((EColi) >= medianPerBin$median[3]+3*iqrPerBin$iqr[3]) %>% 
  summarize(LBC = n())

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

ggplot(data=bear[1:1665,],aes(x=bear$daysFromOrigin[1:1665], y=diff(log(bear$EColi)))) + 
         geom_point(alpha=.3) + facet_wrap(~Site,ncol=4) + theme_bw()

ggplot(data=bear,aes(x=bear$daysFromOrigin, y=(log(bear$EColi)))) + 
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

medianPerBin <- bear %>% group_by(geoBins) %>% summarize(median = median(log(EColi),na.rm=TREU))
iqrPerBin <- bear %>% group_by(geoBins) %>% summarize(iqr = IQR(log(EColi),na.rm=TREU))
meanPerBin <- bear %>% group_by(geoBins) %>% summarize(mean = mean(log(EColi),na.rm=TREU))
sdPerBin <- bear %>% group_by(geoBins) %>% summarize(sd = sd(log(EColi),na.rm=TREU))
rangePerBin <- bear %>% group_by(geoBins) %>% summarize(range = range(log(EColi),na.rm=TREU)[2])

ggplot(bear, aes(log(EColi))) + geom_histogram() + facet_grid(.~geoBins)
ggplot(bear, aes(EColi)) + geom_histogram() + facet_grid(.~geoBins)


ggplot(data=bear,aes(x=bear$daysFromOrigin, y=log(bear$EColi), col=Site)) + 
  geom_point(alpha=.7) + facet_wrap(~geoBins,nrow=4) + theme_bw()

valuesAbove3SD <- c()

valuesAbove3SD[1] <- bear[bear$geoBins==geo[1],] %>%
  filter(log(EColi) >= meanPerBin$mean[1]+3*sdPerBin$sd[1]) %>% 
  summarize(HBC = n())
valuesAbove3SD[2] <- bear[bear$geoBins==geo[2],] %>%
  filter(log(EColi) >= meanPerBin$mean[2]+3*sdPerBin$sd[2]) %>% 
  summarize(MBC = n())
valuesAbove3SD[3] <- bear[bear$geoBins==geo[3],] %>%
  filter(log(EColi) >= meanPerBin$mean[3]+3*sdPerBin$sd[3]) %>% 
  summarize(LBC = n())
valuesAbove3SD[4] <- bear[bear$geoBins==geo[4],] %>%
  filter(log(EColi) >= meanPerBin$mean[4]+3*sdPerBin$sd[4]) %>% 
  summarize(Confluence = n())

valuesAbove3SD[2] <- bear[bear$geoBins==geo[2],] %>%
  filter(EColi >= meanPerBin$mean[2]+3*sdPerBin$sd[2]) %>% 
  summarize(MBC = n())


valuesAbove3SD[3] <- bear[bear$geoBins==geo[3],] %>%
  filter((EColi) >= medianPerBin$median[3]+3*iqrPerBin$iqr[3]) %>% 
  summarize(LBC = n())




######## Packages ########################################################################
require(pacman)
p_load("plotly", "tidyverse", "lubridate", "gridExtra", "Hmisc")


######## Read data and mutate ###########################################################
bear <- read.csv("trimmed2017.csv")
siteOrder <- c("BCL1", "BCL3", "BCL4","BCL5","BC-Estes","BC-Wads","BCD1",
               "BC-Sher","BC-BCP","BCD2","BCD3","BCS1","BCS2","BCS3","BCS4","BCS5",
               "SPUSBC","SPDSBC")
bear$Site <- factor(bear$Site, levels = siteOrder)
bear$Date <-as.Date(bear$Date, format = "%m/%d/%Y")
bear$monthYear <- format(bear$Date, "%m/%Y")
weather <- read.csv("weather.csv")
weather$DATE <- as.Date(weather$DATE, format = "%m/%d/%Y")
weather$monthYear <- format(weather$DATE, "%m/%Y")
weather <- weather %>% filter(DATE <= as.Date("2017-9-13"))
str(weather)

######## Plot weather ############################
bearPlot <- ggplot(bear,aes(x=Date, y=e.coli))
rainPlot <- ggplot(weather,aes(x=DATE, y = PRCP))
p2 <- rainPlot + geom_point() 
p3 <- bearPlot + geom_point()
grid.arrange(p2,p3,nrow=2)

######### Lossless merge #########################################
wthr1 <- weather %>% group_by(Date = floor_date(DATE, "day"))
bear1 <- bear %>% group_by(Date = floor_date(Date, "day"))
merged1 <- merge(wthr1, bear1, by="Date", all = T)
p47 <- ggplot(merged1,aes(x=Date)) + 
  geom_point(aes(y=PRCP*400), col = "blue", alpha = .2) +
  geom_point(aes(y=SNOW*150), col = "violet", alpha = .2) +
  geom_point(aes(y=e.coli), col = "green", alpha = .2)
p47
#### This plot tends to crash R ##################################
ggplotly(p47)
##### Try these instead ##########################################
merged1 %>% plot_ly(x=~Date) %>% add_trace( y=~e.coli, mode = "markers") %>%
  add_trace(y=~PRCP*400) %>% add_trace(y=~SNOW*100)
merged1 %>% plot_ly(x=~Date) %>% add_trace( y=~log(e.coli), mode = "markers") %>%
  add_trace(y=~log(PRCP*400)) %>% add_trace(y=~log(SNOW*200))
##### Dplyr meets base R #################
weather %>% group_by(weather$DATE) %>% 
  summarize(precipy = mean(PRCP)) -> wthr
names(wthr) <- c("Date", "precipy")
bear<- merge(bear, wthr, by = "Date")

############################################
lm(data=bear, log(e.coli)~Date+precipy)-> fit
summary(fit)
plot(log(bear$e.coli),bear$precipy)
bear %>% plot_ly(x=~Date, y=~precipy, z=~e.coli, color = ~Site)
#############################################
confint(fit)

library(plotly)
library(pacman)
p_load("tseries", "xts", "forecast", "astsa", "zoo", "forecast", 
       "tidyverse", "gridExtra", "lubridate", "mice", "car", "rgl",
       "zoo", "xts", "forecast","astsa","pracma","extrafont","RColorBrewer","wesanderson")

TREU = TRUE

#Only run once
#font_import()
#loadfonts(device = "win")

## Data Imputation, I dislike this method after looking at some of its choices of imputation

to_imput <- read.csv("trimmed2017.csv")
tmpImp <- data.frame(to_imput$e.coli, to_imput$tempC)
imputed <- mice(tmpImp ,m = 1 ,maxit = 1 ,meth = 'pmm' ,seed = 500)
imputed <- complete(imputed)
to_imput$e.coli <- imputed$to_imput.e.coli
to_imput$tempC <- imputed$to_imput.tempC

bear <- to_imput

## Some cleaning - better names, ordered factors by geography, better dates
names(bear)[names(bear) == 'e.coli'] <- 'EColi'
bear %>% mutate(logEColi = log(EColi)) -> bear

ord <-c("BCL1","BCL3","BCL4","WEC","BCL5","BC-Estes","BC-Wads","BCD1",
        "BC-Sher","BC-BCP","BCD2","BCD3","BCS1","BCS2","BCS3","BCS4",
        "BCS5","SPUSBC","SPDSBC")
bear<- transform(bear,Site=factor(Site,levels=ord))

bear$Date <- mdy(bear$Date)
bear$daysFromOrigin <- as.duration(interval(bear$Date[1],bear$Date))
bear$daysFromOrigin <- as.numeric(bear$daysFromOrigin, "days")

## Here is what we are looking at - a highly seasonal trend, especially in the Lower Bear Creek (LBC) area
ggplot(data=bear,aes(x=bear$daysFromOrigin, y=(log(bear$EColi)))) + 
  geom_point(alpha=.3) + facet_wrap(~Site,ncol=4) + theme_bw()

##Create new factors for geographical binning
binsHBC <- ord[1:6]
binsMBC <- ord[7:10]
binsLBC <- ord[11:17]
binsSP <- ord[18:19] 

geo <- c("binsHBC","binsMBC","binsLBC","binsSP")

## Nesting like mama bird but all of her little bird children were horrible
## Cronenberg monsters. Is there a better way to do this?

bear %>% mutate(geoBins = 
                  ifelse(bear$Site %in% ord[1:6],"binsHBC",
                         ifelse(bear$Site %in% ord[7:10],"binsHMB",
                                ifelse(bear$Site %in% ord[11:17],"binsLBC",
                                       ifelse(bear$Site %in% ord[18:19],"binsSP",bear$Site))))) -> bear 


## Some descriptive statistics
with(bear,table(geoBins))
medianPerBin <- bear %>% group_by(geoBins) %>% summarize(median = median(log(EColi),na.rm=TREU))
iqrPerBin <- bear %>% group_by(geoBins) %>% summarize(iqr = IQR(log(EColi),na.rm=TREU))
meanPerBin <- bear %>% group_by(geoBins) %>% summarize(mean = mean(log(EColi),na.rm=TREU))
sdPerBin <- bear %>% group_by(geoBins) %>% summarize(sd = sd(log(EColi),na.rm=TREU))
rangePerBin <- bear %>% group_by(geoBins) %>% summarize(range = range(log(EColi),na.rm=TREU)[2])

which(log(bear$EColi) >= medianPerBin[,2] + 2.5*iqrPerBin[,2])
which(log(bear$EColi) <= medianPerBin[,2] - 2.5*iqrPerBin[,2])

## Histogram looks relatively normal
ggplot(bear, aes(log(EColi))) + geom_histogram() + facet_grid(.~geoBins)

## Some plots indicating time series structure

ggplot(data=bear,aes(x=bear$daysFromOrigin, y=log(bear$EColi), col=Site)) + 
  geom_jitter(alpha=.7) + facet_wrap(~geoBins,nrow=4) + 
  theme_bw()

ggplot(data=bear[bear$geoBins!="binsHBC",],aes(x=Date, y=logEColi, col=Site)) + 
  geom_jitter(alpha=.7) + facet_wrap(~geoBins,nrow=4) + 
  geom_line(aes(y=bear[bear$geoBins!="binsHBC",]$tempC/3,x=Date)) +
  theme_bw()

ggplot(data=bear[bear$geoBins=="binsLBC",],aes(x=Date, y=logEColi)) + 
  geom_point(alpha=.7) + facet_wrap(~geoBins,nrow=4) + 
  theme_bw()

## We want to see if our trend persists after averaging over two week periods, focusing on 
## the lower break creek area.

bear %>% filter(geoBins=="binsLBC") -> bearLBC

## Notice here that the median smooths out the measurement and decreases the peaks - this 
## is explored in more detail below after the creation of a time series.

bearLBC %>% 
  group_by(week=floor_date(Date, "14 day")) %>% 
  summarize(medianLogEColi = median(logEColi, na.rm=TREU)) %>% plot(type='l')

bearLBC %>% 
  group_by(week=floor_date(Date, "14 day")) %>% 
  summarize(medianLogEColi = median(logEColi, na.rm=TREU)) ->bearTSP

## Create a time series object from data.frame
bearTS <- xts(bearTSP$medianLogEColi, order.by=as.Date(bearTSP$week, "%m/%d/%Y"))
bearTS <- ts(bearTS)

## The following creates a linear interpolation of points between values in bearTSP so that 
## we can plot the median values on top of our existing plots. 
graph_data <- data.frame(graphit=c(rep(1,828)),Dates=c(rep(as.Date("01-01-2001"),828)))

# Spreads out the 92 cell data frame to 828 cells
for(i in 1:92) {
  bearTS[i] -> graph_data[(9*i-8),1]
  rep(NA,8) -> graph_data[(9*i-7):(9*i),1]
}

for(i in 1:92) {
  bearTSP[i,1] -> graph_data[(9*i-8),2]
  rep(NA,8) -> graph_data[(9*i-7):(9*i),2]
}

# If you ever need to interpolate something in the future...here's the code:
#graph_data$graphit <- with(graph_data, interp1(1:828, graphit, 1:828, "linear"))
#graph_data$daysFromOrigin <- with(graph_data, interp1(1:828, daysFromOrigin, 1:828, "linear"))
#graph_data$daysFromOrigin <- as.Date(graph_data$daysFromOrigin,origin="2013-05-01")

## Maroon is the median interpolation, green is the log EColi raw data

ggplot(data=bear[bear$geoBins %in% c("binsLBC"),], aes(x=Date, y=logEColi)) + 
  geom_jitter(alpha=.4,aes(col=Site)) + facet_wrap(~geoBins,nrow=2) +
  geom_line(col="aquamarine3",size=1,alpha=1) + 
  geom_point(col="coral3",aes(y=graph_data$graphit,x=graph_data$Dates)) + 
  geom_line(col="coral4", size=1.25, alpha=.7,aes(y=graph_data$graphit,x=graph_data$Dates))+
  theme_bw()

ggplot(data=bear[bear$geoBins %in% c("binsLBC"),], aes(x=Date, y=logEColi)) + 
  geom_jitter(alpha=.4,aes(col=Site)) + facet_wrap(~geoBins,nrow=2) +
  #geom_line(col="aquamarine3",size=1,alpha=1) + 
  geom_point(col="coral3",aes(y=graph_data$graphit,x=graph_data$Dates)) + 
  geom_line(col="coral4", size=1.25, alpha=.7,aes(y=graph_data$graphit,x=graph_data$Dates))+
  theme_bw()

## At this point, if we wanted to see how other statistics match up we basically have to rerun all the code above
## By mean, go ahead and flick back and forth between the plots to see how they compare,
## the mean really smooths things out

bearLBC %>% 
  group_by(week=floor_date(Date, "14 day")) %>% 
  summarize(meanLogEColi = mean(logEColi, na.rm=TREU)) ->bearTSPMean
bearTSMean <- xts(bearTSPMean$meanLogEColi, order.by=as.Date(bearTSPMean$week, "%m/%d/%Y"))
bearTSMean <- ts(bearTSMean)
graph_data <- data.frame(graphit=c(rep(1,828)),Dates=c(rep(as.Date("01-01-2001"),828)))
for(i in 1:92) {
  bearTSMean[i] -> graph_data[(9*i-8),1]
  rep(NA,8) -> graph_data[(9*i-7):(9*i),1]
}
for(i in 1:92) {
  bearTSPMean[i,1] -> graph_data[(9*i-8),2]
  rep(NA,8) -> graph_data[(9*i-7):(9*i),2]
}

ggplot(data=bearLBC, aes(x=Date, y=logEColi)) + 
  geom_jitter(alpha=.4,aes(col=Site)) + facet_wrap(~geoBins,nrow=2) +
  geom_line(col="aquamarine3",size=1,alpha=1) + 
  geom_point(col="coral3",aes(y=graph_data$graphit,x=graph_data$Dates)) + 
  geom_line(col="coral4", size=1.25, alpha=.7,aes(y=graph_data$graphit,x=graph_data$Dates))+
  theme_bw()



## It would be nice to have an idea how linearity affects the median/mean 

## We need to convert each day of each 2 week period into a numeric value 1:14

# Some code that I thought may work but is actually pretty difficult

# Here's the basic idea:
as.POSIXlt(bearLBC$Date)[1]$wday

# Vectorizing the above operation
bearLBC$daysOfBiWeek <- as.POSIXlt(bearLBC$Date)$wday

# Adding 7 to the second week - hard if not undoable, maybe ifelse loop?


bearLBC %>% group_by(floor_date(Date,"14 days")) %>% 
  mutate(timediff = difftime(ceiling_date(Date,"1 days"), 
                             floor_date(Date, "14 days"), units="days")) -> bearLBC

bearLBC$daysOfBiWeekA <-ifelse(bearLBC$timediff > 7, bearLBC$daysOfBiWeekA <- bearLBC$daysOfBiWeek + 7, 
       bearLBC$daysOfBiWeekA <- bearLBC$daysOfBiWeek)



# This is unrealistic but works for x: rep(1:14,60)[1:828], otherwise it is a choice 
# as to whether we bin monthly or weekly

p <- ggplot(data = bearLBC, aes(x = daysOfBiWeekA, y = logEColi, color = Site)) + 
  scale_color_manual(values=c(wes_palette(n=4,"FantasticFox"),
                             wes_palette(n=3,"GrandBudapest2"))) +
  geom_point(size=3) +
  geom_line() +
  facet_wrap(~factor(floor_date(bearLBC$Date,"14 day"))) + 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text=element_text(size=18,  family="Georgia"),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("BiWeekly Readings of E. coli in the Lower Bear Creek Area") +
  xlab("BiWeekly Periods") + ylab("Log E. coli")

p

# We can retrieve the group row numbers, then create a vector using this info, difference it,
# and then use the rep function to repeat the median values an appropriate number of times.

bearpos <- bearLBC %>% arrange(Date) %>%
  group_by(floor_date(Date,"14 day")) %>%
  mutate(positionInCategory=1:n())
bearpos$positionInCategory

cuts <- which(bearpos$positionInCategory==1)
diffcuts <- diff(cuts)

medianValues <- rep(bearTSP$medianLogEColi,c(diffcuts,14))

p+geom_point(aes(x=rep(0,828),y=medianValues), col="blue",size=2,shape=25,
             fill =wes_palette(n=1,name="Moonrise3"))


## On to modeling as a time series

## Note the seasonality in acf, suggesting MA 1 or 2, AR 1 or 2
acf(bearTS, lag.max = 120)
pacf(bearTS, lag.max = 120)

# But they look much better with a first difference, suggests AR 1 and MA 0 or 1

acf(diff(bearTS), lag.max = 120)
pacf(diff(bearTS), lag.max=120)

## We take a look at differencing wrt to the year (since we chose bi-weekly, 2*26=52 weeks or 1 year),
## we see promising results, and may not need seasonal differencing:



acf(diff(bearTS, 26), lag.max = 120)
pacf(diff(bearTS, 26), lag.max = 120)

acf(diff(diff(bearTS, 26)), lag.max = 120)
pacf(diff(diff(bearTS, 26)), lag.max = 120)

# This indicates 2 MA/0 AR or a seasonal differencing of 1 with AR 1 and MA 0 or 1

sarima(bearTS, 2, 0, 2, 0, 1, 2, 26)

## Without a seasonal component...
sarima(bearTS, 1, 0, 4, 0, 0, 1, 26)
sarima(bearTS, 1, 0, 4, 0, 0, 2, 26)

## The only issue is that the last two models are not great in forecasting...
newFit <- arima(bearTS, order = c(1,0,4),
                seasonal = list(order = c(0,0,1), period = 26 ))
autoplot(forecast(newFit))


# This is Christopher's new "mental" model
fit <- arima(bearTS, 
             c(1, 1, 1),seasonal = list(order = c(1, 1, 0), period = 26))
pred <- predict(fit,n.ahead=52)
ts.plot(bearTS,pred$pred,log='y', lty=c(1,3))
autoplot(forecast(fit))
sarima(bearTS, 1,0,1,1,1,0,26)




#Some alternative models
sarima(bearTS, 1,1,0,1,1,0,26)
fit <- arima(bearTS, 
             c(1, 1, 0),seasonal = list(order = c(1, 1, 0), period = 26))
pred <- predict(fit,n.ahead=50)
ts.plot(bearTS,pred$pred,log='y', lty=c(1,3))
autoplot(forecast(fit))

sarima(bearTS, 1,1,0,1,1,0,26)
fit <- arima(bearTS, 
             c(1, 1, 0),seasonal = list(order = c(1, 1, 0), period = 26))
pred <- predict(fit,n.ahead=52)
ts.plot(bearTS,pred$pred,log='y', lty=c(1,3))



sarima(bearTS, 1,0,1,1,1,0,26)



## Diagonstics  -seems relatively normal
hist(fit$residuals[24:92])
lines(seq(-3,3,.1), 70*dnorm(seq(-3,3,.1),0, sd(fit$residuals[24:92])), col = 2)
qqnorm(fit$residuals[24:92])

# Fitted vs residuals do not look great
qplot(y=fit$residuals[24:92],x=fitted.values(fit)[24:92])

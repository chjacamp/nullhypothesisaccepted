library(plotly)
library(pacman)
p_load("tseries", "xts", "forecast", "astsa", "zoo", "forecast", 
       "tidyverse", "gridExtra", "lubridate", "mice", "car", "rgl",
       "zoo", "xts", "forecast")
# IMPORTANT IMPORTANT IMPORTANT
TREU = TRUE
# IMPORTANT IMPORTANT IMPORTANT
#### impute ############################
to_imput <- read.csv("trimmed2017.csv")
tmpImp <- data.frame(to_imput$e.coli, to_imput$tempC)
imputed <- mice(tmpImp ,m = 1 ,maxit = 1 ,meth = 'pmm' ,seed = 500)
imputed <- complete(imputed)
to_imput$e.coli <- imputed$to_imput.e.coli
to_imput$tempC <- imputed$to_imput.tempC
########################################
bear <- to_imput
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

bear %>% mutate(logEColi = log(EColi)) -> bear

#### Really this is the start of a time series analysis after viewing the plot below

ggplot(data=bear,aes(x=bear$daysFromOrigin, y=log(bear$EColi), col=Site)) + 
  geom_jitter(alpha=.7) + facet_wrap(~geoBins,nrow=4) + 
  theme_bw()

p <- ggplot(data=bear[bear$geoBins!="binsHBC",],aes(x=Date, y=logEColi, col=Site)) + 
  geom_jitter(alpha=.7) + facet_wrap(~geoBins,nrow=4) + 
  geom_line(aes(y=bear[bear$geoBins!="binsHBC",]$tempC/3,x=Date)) +
  theme_bw()
p
ggplot(data=bear[bear$geoBins=="binsLBC",],aes(x=Date, y=logEColi, col=Site)) + 
  geom_jitter(alpha=.7) + facet_wrap(~geoBins,nrow=4) + 
  theme_bw()

#### We want to see if our trend persists after averaging over two week periods


bear %>% filter(geoBins=="binsLBC") -> bearLBC

bearLBC %>% 
  group_by(week=floor_date(Date, "14 day")) %>% 
  summarize(medianLogEColi = median(logEColi, na.rm=TREU)) %>% plot(type='l')

bearLBC %>% 
  group_by(week=floor_date(Date, "14 day")) %>% 
  summarize(medianLogEColi = median(logEColi, na.rm=TREU)) ->bearTSP


bearTS <- xts(bearTSP$medianLogEColi[-70], order.by=as.Date(bearTSP$week, "%m/%d/%Y")[-70])
bearTS <- ts(bearTS)

adf.test(bearTS, alternative="stationary", k=0)


## Note the seasonality in acf
acf(bearTS, lag.max = 120)
pacf(bearTS, lag.max = 120)


### these didnt really help
acf(diff(bearTS, 20), lag.max = 120)
pacf(diff(bearTS, 20), lag.max = 120)

### trying w/o seasonal difference
sarima(bearTS, 0, 0, 2, 0, 0, 2, 21)
### trying a bunch of stuff
sarima(bearTS, 0, 0, 2, 1, 1, 2, 21)
sarima(bearTS, 0, 0, 5, 0, 0, 2, 21)
sarima(bearTS, 1, 1, 1, 1, 1, 1, 21)
#### these twoo look pretty yeah?
sarima(bearTS, 1, 0, 4, 0, 0, 1, 21)
sarima(bearTS, 1, 0, 4, 0, 0, 2, 21)

newFit <- arima(bearTS, order = c(1,0,4),
                seasonal = list(order = c(0,0,2), period = 21 ))
##### oh yeah
x <- seq(-3 ,3, .1)
newRes <- newFit$residuals
hist(newRes)
lines(x, 43*dnorm(x, 0, sd(newRes)), col = 2)

bear$EColi %>% na.omit() %>% log() %>% abs() %>% diff() %>% acf()


fit <- arima(bearTS, 
             c(8, 1, 1),seasonal = list(order = c(0, 1, 1), period = 24))
pred <- predict(fit,n.ahead=100)
ts.plot(bearTS,pred$pred,log='y', lty=c(1,3))

pred <- predict(fitter, n.ahead = 100)
ts.plot(bearTS,pred$pred, log = "y", lty = c(1,3))


fit <- Arima(bearTS, order=c(2,0,1), xreg=fourier(bearTS, K = 2))

heyo <- sarima(bearTS, 8,1,1,0,1,1,24)

preds <- sarima.for(bearTS, n.ahead=3, 2,0,1,1,2,1,12)

autoplot(forecast(fit))
autoplot(fit$residuals)
hist(fit$residuals)
lines(seq(-3,3,.1), 50*dnorm(seq(-3,3,.1),0, sd(fit$residuals)), col = 2)
qqnorm(fit$residuals)
## What if we ran the same model but with the sewer break elimnated?

bearTSQ <- bearTSP
bearTSQ[33,2] <- 4

bearTS2 <- xts(bearTSQ$medianLogEColi, order.by=as.Date(bearTSQ$week, "%m/%d/%Y"))
bearTS2 <- ts(bearTS2)

# This is my "mental" model
fit <- arima(bearTS2, 
             c(4, 1, 1),seasonal = list(order = c(1, 1, 0), period = 24))
pred <- predict(fit,n.ahead=52)
ts.plot(bearTS2,pred$pred,log='y', lty=c(1,3))

heyo <- sarima(bearTS2, 4,0,1,1,1,0,24)

autoplot(forecast(fit))
autoplot(fit$residuals)
hist(fit$residuals)
lines(seq(-3,3,.1), 50*dnorm(seq(-3,3,.1),0, sd(fit$residuals)), col = 2)
qqnorm(fit$residuals)
# The model looks better in diagnostics, however:

fit <- arima(bearTS2, 
             c(5, 0, 1),seasonal = list(order = c(1, 1, 0), period = 24))
pred <- predict(fit,n.ahead=140)
ts.plot(bearTS2,pred$pred,log='y', lty=c(1,3))

heyo <- sarima(bearTS2, 4,0,1,1,1,0,24)


autoplot(forecast(fit))
autoplot(fit$residuals)
hist(fit$residuals)
lines(seq(-3,3,.1), 50*dnorm(seq(-3,3,.1),0, sd(fit$residuals)), col = 2)
qqnorm(fit$residuals)

preds <- sarima.for(bearTS, n.ahead=14, 1,0,1,0,1,1,12)

ggplot(data=bearTSP,aes(x=bearTSP$week, y=bearTSP$medianLogEColi)) + 
  geom_jitter(alpha=.7) + geom_line(data=bearTS2, aes(y=bearTS2[1:88]))
theme_bw()


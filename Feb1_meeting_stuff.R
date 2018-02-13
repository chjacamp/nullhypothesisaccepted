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
weather <- read.csv("Weather Data.csv")
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

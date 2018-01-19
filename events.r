bear <- read.csv("removenas.csv")
names(bear)[names(bear) == 'E.coli_CFUper100ml'] <- 'EColi'


bear$date <- mdy(bear$date)

bear$date <- as.duration(interval(bear$date[1],bear$date))
bear$date <- as.numeric(bear$date, "days")

ggplot(bear,aes(x=bear$date,y=bear$EColi)) +
  geom_point(alpha = 0.3) + facet_wrap(~site, ncol=4) + theme_bw()


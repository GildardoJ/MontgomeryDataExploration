dat = read.csv("file:///C:/Users/gilda/Documents/CSUMBSpring2019/CST383/Crash_Reporting_-_Drivers_Data.csv")
dat1 = dat
name = names(dat$Route.Type)

# plot of rout frequency in the data. 
par(las=2) # make label text perpendicular to axis
par(mar=c(8,8,2,2))
barplot(table(dat$Route.Type))

dat$Cross.Street.Type

plot(dat$Cross.Street.Type)
plot(dat$Route.Type)
names(dat)

summary(dat)
str(dat)

table(dat$Route.Type)


sum(dat$Cross.Street.Type == "Unknown")
sum(dat$Cross.Street.Type == "")

#plot of time of day 

#dat$dtime = strptime(as.character(dat$Crash.Date.Time),"%m/%d/%Y %I:%M:%S %p")
times = strptime(as.character(dat$Crash.Date.Time),"%m/%d/%Y %I:%M:%S %p")
hour = times$hour
barplot(table(hour))

#aggregate by date
times = strptime(as.character(dat$Crash.Date.Time),"%m/%d/%Y %I:%M:%S %p")
hour = times$hour
dat1$dates = times
day = times$yday[times$yday == 73]
par(las =2)

dates = as.Date(times)
sort(table(dates))
plot(table(dates),type = ".")

plot(table(day))
plot(table(times$mi)) # shows to what minute was the time stated


year = times$year
unique(year)

#plot month onver month
times = strptime(as.character(dat$Crash.Date.Time),"%m/%d/%Y %I:%M:%S %p")
jan = times[times$mon == 0,]
feb = times[times$mon == 1,]
mar = times[times$mon == 2,]
nov = times[times$mon == 10,]
year2015 = times[times$year == 115]
year2016 = times[times$year == 116]
year2017 = times[times$year == 117]
year2018 = times[times$year == 118]
year2015
summary(times)
times
is.vector(times)
is.table(times)
is.character(times)
is.complex(times)
is.data.frame(times)
is.data.frame(dat)
head(dat$Crash.Date.Time)

par(las =2)
plot(table(month))
barplot(table(year2015$yday))
plot(table(times$yday), type = "line") #keep

plot(table(jan$yday), type = "line") #keep  
points(table(feb$yday - 31), type = "line", col = "blue") #keep  
points(table(mar$yday - 59), type = "line", col = "red") #keep 
points(table(nov$yday - 304), type = "line", col = "purple") #keep 

plot(table(year2015$mon), main ="Rates of accidetnts per Month", type = "line",xaxt = "n" ,xlab = "Month", ylab = "Number of accidents", ylim = c(1000,2500))
axis(1, at=0:11, labels=c("Jan","Feb","Mar", "April","May","Jun","July","Aug","Sep","Oct","Nov", "Dec"))
points(table(year2016$mon), type = "line", col = "blue")
points(table(year2017$mon), type = "line", col = "red")
points(table(year2018$mon), type = "line", col = "purple")
grid()
legend(0, 2500, legend=c("2017","2016","2018","2015"),col=c("red", "blue","purple","black"), lty=c(1,1,1,1), cex=0.8, title ="Year")


plot(table(year2015$wday), type = "p")
points(table(year2016$wday), type = "p", col = "blue")
points(table(year2017$wday), type = "p", col = "red")
points(table(year2018$wday), type = "p", col = "purple")

abline(h=mean(table(year2015$mon)),lty=2)  
abline(h=mean(table(year2016$mon)),col='blue',lty=2)
abline(h=mean(table(year2017$mon)),col='red',lty=2)
abline(h=mean(table(year2018$mon)),col='purple',lty=2)

abline(h=median(table(year2015$mon)),lty=4)  
abline(h=median(table(year2016$mon)),col='blue',lty=4)
abline(h=median(table(year2017$mon)),col='red',lty=4)
abline(h=median(table(year2018$mon)),col='purple',lty=4) #keep

barplot(table(times$yday), type = "p") #keep
aggregate(mon ~ year,times, FUN = sum )

dat$Crash.Date.Time
dat1$dates

# seach by date

head(dat1$Crash.Date.Time)
par(mar=c(3,6,2,2))
par(las =2)
weth = dat1[dat1$Weather == "RAINING" ]

plot(weth$Weather ~ weth$dates$year, data = dat1)

unique(dat1$Weather)
rained = dat1[dat1$Weather == "RAINING" & dat1$dates$year == 117,] # months, how many accidents when it rainned. 
plot(table(rained$dates$mon ))
            # surface condition 
unique(dat1$Surface.Condition)
rained = dat1[dat1$Surface.Condition == "WET" & dat1$dates$year == 117,] # months, how many accidents when it rainned. 
plot(table(rained$dates$mon ))


summary(rained )
nrow(rained)
nRain = dat1[dat1$Weather == "CLEAR",]
nrow(nRain)
summary(nRain)
rainedT = table(rained$dates$yday)
summary(rainedT)
rainedT = sort(rainedT$Weather)
plot(rainedT)
#weather of may and last three months of the year
par(mar(c(8,12,6,6)))
mayW = dat1[dat1$dates$mon == 4 & dat1$dates$year == 116, ]
octW = dat1[dat1$dates$mon == 9,]
plot(table(mayW$Weather))
plot(table(mayW$Surface.Condition))
plot(table(octW$Weather))
plot(table(octW$Surface.Condition))
summary(mayW)
nrow(mayW)
nrow(octW)
sum(unique(mayW$dates$yday[mayW$Weather == "RAINING"]))
unique(mayW$dates$yday[mayW$Weather == "RAINING" ])

sum(unique(mayW$dates$yday[mayW$Weather == "CLEAR"]))
unique(mayW$dates$yday[mayW$Weather == "CLEAR" & mayW$Weather != "WET"])

plot(mayW$Weather ~ mayW$Surface.Condition, data = mayW)

#light 
unique(dat$Light)
sum(dat$Light == "N/A" | dat$Light == "UNKNOWN")/length((dat$Light)) # ONLY ONE PERCENT of the data
par(las = 2)
barplot(table(dat$Light))

#pie chart explore

pie(table(dat$Light), main = "All data")
sum(dat$Light)


#parking lot

found = grep("parking",dat$Off.Road.Description,ignore.case = TRUE, value = FALSE)
found

dat[found,]
length(found)/nrow(dat)

#parking lot incident what type of vehicle movement
par(mar=c(12,8,2,2))
barplot(sort(table(dat[found,]$Vehicle.Movement), decreasing = TRUE))

barplot( c(length(found),nrow(dat)-length(found)),names.arg = c("Parking","Road"))

#pie(table(dat[found,]$Light), main = "Parking lot")

par(mfrow = c(2,1))

parkingLot = c(1:nrow(dat))
parkingLot = ifelse(dat[found,]$light == dat$Light , "TRUE","FALSE")
length(parkingLot)
parkingLot
head(found)
head(dat)
nrow(parkingLot)


tbl = table(dat[found,]$Light,parkingLot)

barplot(table(dat[found,]$Light), main = "Parking lot")

#surface conditions

barplot(sort(table(dat$Light), decreasing = TRUE))

 namesWeather = unique(dat$Weather)
 namesSurface = unique(dat$Surface.Condition)
 
par(mar=c(12,12,2,2))
par(las =2)
weather = sort(dat[found,]$Weather,decreasing = TRUE)
weather
plot(dat[found,]$Light ~ weather)
plot(dat[found,]$Light ~ dat[found,]$Weather)



plot(dat$Light ~ dat$Weather)

rainDat = dat[dat$Weather == "RAINING",]
rainDat

lightDat = dat[dat$Light]

nrow(rainDat)
nrow(dat)


  # light data frame

daylight = dat[dat$Light == "DAYLIGHT" & dat$Weather == "CLEAR",]
nrow(daylight) #39730 ROWS



daylight = dat[dat$Light == "DAYLIGHT" & dat$Weather == "RAINING",]
nrow(daylight) #6362 rows

plot(daylight$Weather ~ daylight$Light)

plot(rainDat$Light ~ rainDat$Weather)

  #weather data grame 
  weatherDat = dat[dat$Weather == "CLEAR" & dat$Weather == "",]


plot(sort(table(dat$Weather), decreasing = TRUE), type = "p", col = "red4", ylab = "# incidents")
barplot(sort(table(dat$Surface.Condition), decreasing = TRUE))

barplot(sort(table(dat$Surface.Condition),decreasing = FALSE), horiz = TRUE)

# what ratio of accidents during wet condition was it raining. 

plot(dat$Surface.Condition ~ dat$Weather)


# Correlation plot

#parking lot damage 
barplot(sort(table(dat[found,]$Vehicle.Damage.Extent), decreasing = TRUE))
barplot(sort(table(dat$Vehicle.Damage.Extent), decreasing = TRUE))


#Montgomery county weather for 2010

dat2 = read.csv("file:///C:/Users/gilda/Documents/CSUMBSpring2019/CST383/Mont2010.csv")
summary(dat2)
names(dat2)
plot(dat2$MTD.PRCP.NORMAL)
plot(dat2$DATE)
summary(dat2$DATE)

rain = aggregate(dat2$MLY.PRCP.50PCTL ~ dat2$DATE, FUN = mean)
plot(rain, type = "l")                              # mean rain
plot(dat2$MLY.PRCP.50PCTL ~ dat2$DATE)
plot(dat2$MLY.SNOW.50PCTL ~ dat2$DATE, type = "l") # snow



points(dat2$MLY.SNOW.25PCTL ~ dat2$DATE, col = "red", pch = 20)
plot(dat2$MLY.TMIN.NORMAL ~ dat2$DATE, col = "green", pch = 20)


head(dat2$DATE)

layout(title = 'Title',
       xaxis = list(title = "X-axis title"),
       yaxis2 = list(side = 'right', overlaying = "y", title = 'secondary y axis', showgrid = FALSE, zeroline = FALSE))


#montgomery county rain for 2015 - 18

dat3 = read.csv("file:///C:/Users/gilda/Documents/CSUMBSpring2019/CST383/Mont2015-18.csv")
times = strptime(as.character(dat$Crash.Date.Time),"%m/%d/%Y %I:%M:%S %p")
summary(dat3)
names(dat3)
dat3 = sort(dat3$DATE, decreasing = FALSE)

#rain2 = aggregate(dat3$PRCP ~ dat3$DATE, FUN = mean)
plot(rain2, type = "l")
plot(dat3$PRCP ~ dat3$DATE, type = "l")
points(dat3$PRCP ~ dat3$DATE)

nrow(dat3)/48
unique(dat3$DATE)

sum(is.na(dat3$DATE))
summary(dat3)
dat3[dat3$PRCP > 7.5 & dat3$PRCP < 8.1 & dat3$DATE == "2015-09",]

plot(dat3$PRCP ~ dat3$DATE)
grid()

#injury severity
dat1$dates
hour = times$hour
injuries = aggregate(Injury.Severity~ hour, data=injury, table)
injuries
injury = dat1[dat1$Injury.Severity == "FATAL INJURY",]
injury$dates$hour
plot(injury ~ injury$dates$hour)


#
dat2 = read.csv("file:///C:/Users/gilda/Documents/CSUMBSpring2019/CST383/Mont2010.csv") #weather data from NOAA ncdc.noaa.gov
times = strptime(as.character(dat$Crash.Date.Time),"%m/%d/%Y %I:%M:%S %p")
year2015 = times[times$year == 115]
year2016 = times[times$year == 116]
year2017 = times[times$year == 117]
year2018 = times[times$year == 118]
rain = aggregate(dat2$MLY.PRCP.50PCTL ~ dat2$DATE, FUN = mean)
#rain = scale(rain)

snow = dat2$MLY.SNOW.NORMAL ~ dat2$DATE
#snow = scale(snow)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

rain <- as.data.frame(lapply(rain, normalize))

xmonth = c("Jan","Feb","Mar", "April","May","Jun","July","Aug","Sep","Oct","Nov", "Dec")

par(mar= c(5,4,4,6)+0.1)
plot(table(year2015$mon), main ="Rates of accidents per Month", type = "l",xaxt = "n" ,xlab = "Month", ylab = "Number of accidents", ylim = c(1300,2100))
axis(1, at=0:11, labels= xmonth)
points(table(year2016$mon), type = "l", col = "blue")
points(table(year2017$mon),  type = "l", col = "red")
points(table(year2018$mon),  type = "l", col = "purple")
grid()
legend(0, 2200, legend=c("2015","2016","2017","2018"),col=c("black", "blue","red","purple"), lty=c(1,1,1,1), cex=0.8, title ="Year")



par(new=TRUE)
plot( rain, type = "b",col = "green", axes = F, xlab= NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 3, 'Scaled rain fall, snow')
par(new = TRUE)

plot( snow, type = "b", col = "brown", axes = F, xlab= NA, ylab = NA)



#plot(rain, type = "l")
with(table(year2015$mon), plot(rain, type = "l", axes=F, xlab=NA, ylab=NA))
#
legend("topright",
       legend=c(expression(-log[10](italic(p))), "N genes"),
       lty=c(1,0), pch=c(NA, 16), col=c("red3", "black"))

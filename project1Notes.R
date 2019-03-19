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
plot(table(times$mi))


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

plot(table(year2015$mon), type = "line")
points(table(year2016$mon), type = "line", col = "blue")
points(table(year2017$mon), type = "line", col = "red")
points(table(year2018$mon), type = "line", col = "purple")


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
weth = dat1[dat1$Weather == "RAINING" | dat1$Weather == "CLEAR",]

plot(weth$Weather ~ weth$dates$year, data = dat1)

rained = dat1[dat1$Weather == "RAINING",]

plot(table(rained$dates$yday ))
summary(rained )
nrow(rained)
nRain = dat1[dat1$Weather == "CLEAR",]
nrow(nRain)
summary(nRain)
rainedT = table(rained$dates$yday)
summary(rainedT)
rainedT = sort(rainedT$Weather)
plot(rainedT)


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


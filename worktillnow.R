packages <- c("dplyr", "lubridate", "ggplot2", "hydroGOF", "e1071", "forecast", "tseries")
if(length(setdiff(packages, rownames(installed.packages()))) > 0){
        install.packages(setdiff(packages, rownames(installed.packages())))
}


lapply(packages, require, character.only = TRUE)

setwd("C:/Users/hp/Desktop")
dir.create("Kanpur_Vansh")
setwd("Kanpur_Vansh")

df <- read.csv("C:/Users/hp/Downloads/aqi-data-kanpur.csv", header = FALSE, na.strings = " None", col.names = c("Site_Code", "Place", "Pollutant", "Time", "pollution_Level"))

df1 <- df[!is.na(df$pollution_Level),]
df1$Time <- as.POSIXct(strptime(df1$Time, " %A, %d %b %Y, %H:%M"))
df1 <- df1[df1$Time < as.POSIXct("2018-01-01"),]
df1 <- df1[,-2:-1]
df1$Pollutant <- as.factor(trimws(df1$Pollutant))
# df1$Pollutant <- (trimws(df1$Pollutant)) #character type output

dev.off()


g <- ggplot(df1, aes(Time, (pollution_Level)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + geom_line()

g <- ggplot(df1, aes(Time, log(pollution_Level)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + geom_line() +
        geom_abline(col = "blue")

df2 <- df1 %>% mutate(Year = as.integer(format(Time, "%Y")), Month = as.integer(format(Time, "%m")), Date = as.integer(format(Time, "%d")), Hour = as.integer(format(Time, "%H")), Minute = as.integer(format(Time, "%M"))) %>% group_by(Pollutant) %>% arrange(Time)

# df2 <- df1 %>% mutate(Year = as.integer(format(Time, "%Y")), Month = as.integer(format(Time, "%m")), Date = as.integer(format(Time, "%d")), Hour = as.integer(format(Time, "%H")), Minute = as.integer(format(Time, "%M"))) %>% group_by(Pollutant, Month, Hour, Date, Year) %>% arrange(Time) %>% summarise(monthly_pollution_mean = mean(pollution_Level))

df3 <- df1 %>% mutate(Year = as.integer(format(Time, "%Y")), Month = as.integer(format(Time, "%m")), Date = as.integer(format(Time, "%d")), Hour = as.integer(format(Time, "%H")), Minute = as.integer(format(Time, "%M"))) %>% group_by(Pollutant, Time) %>% summarise(pollution_mean = mean(pollution_Level)) %>% arrange(Time)

g <- ggplot(df3, aes(Time, (pollution_mean)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + geom_line()

yearly_grouped_analysis <- df3 %>% group_by(Pollutant, Year = as.integer(format(Time, "%Y"))) %>% summarise(meanYearly = mean(pollution_mean))
g <- ggplot(yearly_grouped_analysis, aes(Year, (meanYearly)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + geom_smooth()

yearly_monthly_grouped_analysis <- df3 %>% group_by(Pollutant, Year = as.integer(format(Time, "%Y")), Month = as.integer(format(Time, "%m"))) %>% summarise(meanYearly = mean(pollution_mean))
g <- ggplot(yearly_monthly_grouped_analysis, aes(Month, (meanYearly)))
g + geom_point(alpha = 1/3) + facet_wrap(Pollutant~Year) + geom_smooth()

dfsplit <- split(df3, df3$Pollutant)

count_ts = ts(dfsplit$CO[,c('pollution_mean')])
dfsplit$CO$clean_pollution_mean = tsclean(count_ts)

count_ts = ts(dfsplit$NO2[,c('pollution_mean')])
dfsplit$NO2$clean_pollution_mean = tsclean(count_ts)

count_ts = ts(dfsplit$OZONE[,c('pollution_mean')])
dfsplit$OZONE$clean_pollution_mean = tsclean(count_ts)

count_ts = ts(dfsplit$PM2.5[,c('pollution_mean')])
dfsplit$PM2.5$clean_pollution_mean = tsclean(count_ts)

count_ts = ts(dfsplit$SO2[,c('pollution_mean')])
dfsplit$SO2$clean_pollution_mean = tsclean(count_ts)

lapply(dfsplit, head)


ggplot() +
        geom_line(data = dfsplit$CO, aes(x = Time, y = clean_pollution_mean)) + ylab('Cleaned Pollutant') + ggtitle('CO')

ggplot() +
        geom_line(data = dfsplit$SO2, aes(x = Time, y = clean_pollution_mean)) + ylab('Cleaned Pollutant') + ggtitle('SO2')

ggplot() +
        geom_line(data = dfsplit$NO2, aes(x = Time, y = clean_pollution_mean)) + ylab('Cleaned Pollutant') + ggtitle('NO2')

ggplot() +
        geom_line(data = dfsplit$OZONE, aes(x = Time, y = clean_pollution_mean)) + ylab('Cleaned Pollutant') + ggtitle('OZONE')

ggplot() +
        geom_line(data = dfsplit$PM2.5, aes(x = Time, y = clean_pollution_mean)) + ylab('Cleaned Pollutant') + ggtitle('PM2.5')


dfsplit$CO$clean_pollution_mean_ma = ma(dfsplit$CO$clean_pollution_mean, order=7) # using the clean count with no outliers
dfsplit$CO$clean_pollution_mean_ma30 = ma(dfsplit$CO$clean_pollution_mean, order=30)

str(dfsplit$CO)

ggplot() +
        geom_line(data = dfsplit$CO, aes(x = Time, y = clean_pollution_mean, colour = "Counts")) +
        geom_line(data = dfsplit$CO, aes(x = Time, y = clean_pollution_mean_ma,   colour = "Weekly Moving Average"))  +
        geom_line(data = dfsplit$CO, aes(x = Time, y = clean_pollution_mean_ma30, colour = "Monthly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('CO')

dfsplit$NO2$clean_pollution_mean_ma = ma(dfsplit$NO2$clean_pollution_mean, order=7) # using the clean NO2unt with no outliers
dfsplit$NO2$clean_pollution_mean_ma30 = ma(dfsplit$NO2$clean_pollution_mean, order=30)

str(dfsplit$NO2)

ggplot() +
        geom_line(data = dfsplit$NO2, aes(x = Time, y = clean_pollution_mean, NO2lour = "NO2unts")) +
        geom_line(data = dfsplit$NO2, aes(x = Time, y = clean_pollution_mean_ma,   NO2lour = "Weekly Moving Average"))  +
        geom_line(data = dfsplit$NO2, aes(x = Time, y = clean_pollution_mean_ma30, NO2lour = "Monthly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('NO2')

dfsplit$SO2$clean_pollution_mean_ma = ma(dfsplit$SO2$clean_pollution_mean, order=7) # using the clean SO2unt with no outliers
dfsplit$SO2$clean_pollution_mean_ma30 = ma(dfsplit$SO2$clean_pollution_mean, order=30)

str(dfsplit$SO2)

ggplot() +
        geom_line(data = dfsplit$SO2, aes(x = Time, y = clean_pollution_mean, SO2lour = "SO2unts")) +
        geom_line(data = dfsplit$SO2, aes(x = Time, y = clean_pollution_mean_ma,   SO2lour = "Weekly Moving Average"))  +
        geom_line(data = dfsplit$SO2, aes(x = Time, y = clean_pollution_mean_ma30, SO2lour = "Monthly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('SO2')

dfsplit$OZONE$clean_pollution_mean_ma = ma(dfsplit$OZONE$clean_pollution_mean, order=7) # using the clean OZONEunt with no outliers
dfsplit$OZONE$clean_pollution_mean_ma30 = ma(dfsplit$OZONE$clean_pollution_mean, order=30)

str(dfsplit$OZONE)

ggplot() +
        geom_line(data = dfsplit$OZONE, aes(x = Time, y = clean_pollution_mean, OZONElour = "OZONEunts")) +
        geom_line(data = dfsplit$OZONE, aes(x = Time, y = clean_pollution_mean_ma,   OZONElour = "Weekly Moving Average"))  +
        geom_line(data = dfsplit$OZONE, aes(x = Time, y = clean_pollution_mean_ma30, OZONElour = "Monthly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('OZONE')

dfsplit$PM2.5$clean_pollution_mean_ma = ma(dfsplit$PM2.5$clean_pollution_mean, order=7) # using the clean PM2.5unt with no outliers
dfsplit$PM2.5$clean_pollution_mean_ma30 = ma(dfsplit$PM2.5$clean_pollution_mean, order=30)

str(dfsplit$PM2.5)

ggplot() +
        geom_line(data = dfsplit$PM2.5, aes(x = Time, y = clean_pollution_mean, PM2.5lour = "PM2.5unts")) +
        geom_line(data = dfsplit$PM2.5, aes(x = Time, y = clean_pollution_mean_ma,   PM2.5lour = "Weekly Moving Average"))  +
        geom_line(data = dfsplit$PM2.5, aes(x = Time, y = clean_pollution_mean_ma30, PM2.5lour = "Monthly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('PM2.5')


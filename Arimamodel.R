packages <- c("dplyr", "lubridate", "ggplot2", "hydroGOF", "e1071", "forecast", "tseries", "padr")
if(length(setdiff(packages, rownames(installed.packages()))) > 0){
        install.packages(setdiff(packages, rownames(installed.packages())))
}


lapply(packages, require, character.only = TRUE)

setwd("C:/Users/Jhingalala/Desktop")
dir.create("Kanpur_Vansh")
setwd("Kanpur_Vansh")

df <- read.csv("C:/Users/Jhingalala/Downloads/aqi-data-kanpur.csv", header = FALSE, na.strings = " None", col.names = c("Site_Code", "Place", "Pollutant", "Time", "pollution_Level"))
df <- df[,-2:-1]
ggplot(df, aes(Time, pollution_Level)) + geom_line() + ylab("Pollution Level") +
        xlab("Time") 
df$Time <- dmy_hm(df$Time)
minute(df$Time) <- 0
df1<- df[df$Time %within% interval(ymd("2012-04-02"), ymd_hm("2017-12-31 23:00")),]
df1$Pollutant <- as.factor(trimws(df1$Pollutant))
# df$Pollutant <- (trimws(df$Pollutant)) #character type output

dfsplit <- split(df1, df1$Pollutant)
#lapply(dfsplit, summary)
#lapply(dfsplit, str)

ggplot(dfsplit$CO, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("CO")

ggplot(dfsplit$SO2, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("SO2")

ggplot(dfsplit$NO2, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("NO2")

ggplot(dfsplit$PM2.5, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("PM2.5")

ggplot(dfsplit$OZONE, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("OZONE")
dfsplit <- lapply(dfsplit, 
                          function(x){
                                  if(hour(min(x$Time)) != 0){
                                          x <- x[x$Time %within% interval(ymd(paste(year(min(x$Time)),"-",month(min(x$Time)),"-",day(min(x$Time)) + 1)), ymd_hm("2017-12-31 23:00")),]
                                  }else{
                                          x<-x
                                  }
                                  x <- x[-1]
                          }
                )


# lapply(dfsplit,dim)

first <- dfsplit$OZONE
first <- pad(first, interval = "hour")
l <- c()
k <- c()
for(i in 1:(length(first$Time)-1)){
        if(as.integer(difftime(first$Time[i+1], first$Time[i], units = "hours")) != 1){
                l <- c(l,i-1,i,i+1)
                k <- c(k,i) 
        }
}
 #first[l,]
for(i in k){
        if(!is.na(first[i,2]) && !is.na(first[i+1,2])){
                first[i,2] <- (first[i,2]+first[i+1,2])/2
                first[i+1,2] <- first[i,2]
        }else if(is.na(first[i+1,2])){
                first[i+1,2] <- first[i,2]
        }else{
                first[i,2] <- first[i+1,2]
        }
        first <- first[-1 * i,]
}
# first[l,]

dfsplit$OZONE <- first

first <- dfsplit$CO
first <- pad(first, interval = "hour")
l <- c()
k <- c()
for(i in 1:(length(first$Time)-1)){
        if(as.integer(difftime(first$Time[i+1], first$Time[i], units = "hours")) != 1){
                l <- c(l,i-1,i,i+1)
                k <- c(k,i) 
        }
}
# first[l,]
for(i in k){
        if(!is.na(first[i,2]) && !is.na(first[i+1,2])){
                first[i,2] <- (first[i,2]+first[i+1,2])/2
                first[i+1,2] <- first[i,2]
        }else if(is.na(first[i+1,2])){
                first[i+1,2] <- first[i,2]
        }else{
                first[i,2] <- first[i+1,2]
        }
        first <- first[-1 * i,]
}
# first[l,]

dfsplit$CO <- first

first <- dfsplit$NO2
first <- pad(first, interval = "hour")
l <- c()
k <- c()
for(i in 1:(length(first$Time)-1)){
        if(as.integer(difftime(first$Time[i+1], first$Time[i], units = "hours")) != 1){
                l <- c(l,i-1,i,i+1)
                k <- c(k,i) 
        }
}
# first[l,]
for(i in k){
        if(!is.na(first[i,2]) && !is.na(first[i+1,2])){
                first[i,2] <- (first[i,2]+first[i+1,2])/2
                first[i+1,2] <- first[i,2]
        }else if(is.na(first[i+1,2])){
                first[i+1,2] <- first[i,2]
        }else{
                first[i,2] <- first[i+1,2]
        }
        first <- first[-1 * i,]
}
# first[l,]

dfsplit$NO2 <- first


first <- dfsplit$PM2.5
first <- pad(first, interval = "hour")
l <- c()
k <- c()
for(i in 1:(length(first$Time)-1)){
        if(as.integer(difftime(first$Time[i+1], first$Time[i], units = "hours")) != 1){
                l <- c(l,i-1,i,i+1)
                k <- c(k,i) 
        }
}
# first[l,]
for(i in k){
        if(!is.na(first[i,2]) && !is.na(first[i+1,2])){
                first[i,2] <- (first[i,2]+first[i+1,2])/2
                first[i+1,2] <- first[i,2]
        }else if(is.na(first[i+1,2])){
                first[i+1,2] <- first[i,2]
        }else{
                first[i,2] <- first[i+1,2]
        }
        first <- first[-1 * i,]
}
# first[l,]

dfsplit$PM2.5 <- first


first <- dfsplit$SO2
first <- pad(first, interval = "hour")
l <- c()
k <- c()
for(i in 1:(length(first$Time)-1)){
        if(as.integer(difftime(first$Time[i+1], first$Time[i], units = "hours")) != 1){
                l <- c(l,i-1,i,i+1)
                k <- c(k,i) 
        }
}
# first[l,]
for(i in k){
        if(!is.na(first[i,2]) && !is.na(first[i+1,2])){
                first[i,2] <- (first[i,2]+first[i+1,2])/2
                first[i+1,2] <- first[i,2]
        }else if(is.na(first[i+1,2])){
                first[i+1,2] <- first[i,2]
        }else{
                first[i,2] <- first[i+1,2]
        }
        first <- first[-1 * i,]
}
# first[l,]

dfsplit$SO2 <- first


insertRow2 <- function(existingDF, newrow, r) {
        existingDF <- rbind(existingDF,newrow)
        existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
        row.names(existingDF) <- 1:nrow(existingDF)
        return(existingDF)  
}


ggplot(dfsplit$CO, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("CO")

ggplot(dfsplit$SO2, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("SO2")

ggplot(dfsplit$NO2, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("NO2")

ggplot(dfsplit$PM2.5, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("PM2.5")

ggplot(dfsplit$OZONE, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("OZONE Data Wrangled")

if(FALSE){
dfsplit <- lapply(dfsplit, 
                  function(x){
                         for(i in seq(length(x$Time)-1)){
                                 if(as.integer(difftime(x$Time[i+1], x$Time[i], units = "hours")) != 1){
                                         z <- seq.POSIXt((x$Time[i] + hours(1)), (x$Time[i+1] - hours(1)), by = "hours")
                                         for(j in 1:length(z)){
                                                 x <- insertRow2(x, list(x$Pollution[i], z[j], NA), i + j)
                                         }
                                }
                        }
                }
        )
}

# lapply(dfsplit, summary)

dfsplit1 <- lapply(dfsplit, 
                  function(x){
                          
                          count_ts = ts(x[,c('pollution_Level')])
                          x$clean_pollution_mean = tsclean(count_ts)
                          x$clean_pollution_mean_ma = ma(x$clean_pollution_mean, order = 24) # using the clean count with no outliers
                          # x$clean_pollution_mean_ma_weekly = ma(x$clean_pollution_mean, order=24 * 7)
                          # x$clean_pollution_mean_ma_monthly = ma(x$clean_pollution_mean, order = 24 * 30)
                          # considering month of 30 days
                          return(x)
                  }
        )


# lapply(dfsplit, head)
# lapply(dfsplit, summary)
# lapply(dfsplit, dim)

# lapply(dfsplit1, head)
# lapply(dfsplit1, summary)
# lapply(dfsplit1, dim)

if(FALSE){
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
        
ggplot(dfsplit1$CO, aes(Time, clean_pollution_mean)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("CO")

ggplot(dfsplit1$SO2, aes(Time, clean_pollution_mean)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("SO2")

ggplot(dfsplit1$NO2, aes(Time, clean_pollution_mean)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("NO2")

ggplot(dfsplit1$PM2.5, aes(Time, clean_pollution_mean)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("PM2.5")

ggplot(dfsplit1$OZONE, aes(Time, clean_pollution_mean)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("OZONE")

dfsplit1$CO$clean_pollution_mean_ma = ma(dfsplit1$CO$clean_pollution_mean, order=24) # using the clean count with no outliers
dfsplit1$SO2$clean_pollution_mean_ma = ma(dfsplit1$SO2$clean_pollution_mean, order=24) # using the clean count with no outliers
dfsplit1$NO2$clean_pollution_mean_ma = ma(dfsplit1$NO2$clean_pollution_mean, order=24) # using the clean count with no outliers
dfsplit1$OZONE$clean_pollution_mean_ma = ma(dfsplit1$OZONE$clean_pollution_mean, order=24) # using the clean count with no outliers
dfsplit1$PM2.5$clean_pollution_mean_ma = ma(dfsplit1$PM2.5$clean_pollution_mean, order=24) # using the clean count with no outliers

dfsplit1$CO$clean_pollution_mean_ma_weekly = ma(dfsplit1$CO$clean_pollution_mean, order=24) # using the clean count with no outliers
dfsplit1$SO2$clean_pollution_mean_ma_weekly = ma(dfsplit1$SO2$clean_pollution_mean, order=24) # using the clean count with no outliers
dfsplit1$NO2$clean_pollution_mean_ma_weekly = ma(dfsplit1$NO2$clean_pollution_mean, order=24) # using the clean count with no outliers
dfsplit1$OZONE$clean_pollution_mean_ma_weekly = ma(dfsplit1$OZONE$clean_pollution_mean, order=24) # using the clean count with no outliers
dfsplit1$PM2.5$clean_pollution_mean_ma_weekly = ma(dfsplit1$PM2.5$clean_pollution_mean, order=24) # using the clean count with no outliers


ggplot() +
        geom_line(data = dfsplit1$CO, aes(x = Time, y = clean_pollution_mean, colour = "Counts")) +
        geom_line(data = dfsplit1$CO, aes(x = Time, y = clean_pollution_mean_ma,   colour = "Hourly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('CO')



ggplot() +
        geom_line(data = dfsplit1$PM2.5, aes(x = Time, y = clean_pollution_mean, colour = "Counts")) +
        geom_line(data = dfsplit1$PM2.5, aes(x = Time, y = clean_pollution_mean_ma,   colour = "Hourly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('PM2.5')


ggplot() +
        geom_line(data = dfsplit1$NO2, aes(x = Time, y = clean_pollution_mean, colour = "Counts")) +
        geom_line(data = dfsplit1$NO2, aes(x = Time, y = clean_pollution_mean_ma,   colour = "Hourly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('NO2')


ggplot() +
        geom_line(data = dfsplit1$OZONE, aes(x = Time, y = clean_pollution_mean, colour = "Counts")) +
        geom_line(data = dfsplit1$OZONE, aes(x = Time, y = clean_pollution_mean_ma,   colour = "Hourly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('OZONE')


ggplot() +
        geom_line(data = dfsplit1$SO2, aes(x = Time, y = clean_pollution_mean, colour = "Counts")) +
        geom_line(data = dfsplit1$SO2, aes(x = Time, y = clean_pollution_mean_ma,   colour = "Hourly Moving Average"))  +
        ylab('Cleaned Pollutant') + ggtitle('SO2')
}

############### Evaluating for OZONE ##################################
count_ma = ts(na.omit(dfsplit1$OZONE$clean_pollution_mean_ma), frequency = 30 * 24) # time series
decomp = stl(count_ma, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# Augmented Dickey-Fuller Test

adf.test(count_ma, alternative = "stationary") # t-test

Acf(count_ma, main='')

Pacf(count_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=200, main='(1,1,2) Model Residuals')

fit2 = arima(deseasonal_cnt, order=c(1,1,24))

fit2

tsdisplay(residuals(fit2), lag.max=200, main='Seasonal Model Residuals')

fcast <- forecast(fit2, h=24 * 30)
plot(fcast)


###################### Seasonal Changes #####################################
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality
tsdisplay(residuals(fit_w_seasonality), lag.max=200, main='(1,1,2) Model Residuals')

seas_fcast <- forecast(fit_w_seasonality, h=60 * 24)
plot(seas_fcast)


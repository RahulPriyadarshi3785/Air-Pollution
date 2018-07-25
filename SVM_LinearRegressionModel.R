packages <- c("dplyr", "lubridate", "ggplot2", "hydroGOF", "e1071", "forecast", "tseries", "padr")
if(length(setdiff(packages, rownames(installed.packages()))) > 0){
        install.packages(setdiff(packages, rownames(installed.packages())))
}


lapply(packages, require, character.only = TRUE)

setwd("C:/Users/hp/Desktop")
dir.create("Kanpur_Vansh")
setwd("Kanpur_Vansh")

df <- read.csv("C:/Users/hp/Downloads/aqi-data-kanpur.csv", header = FALSE, na.strings = " None", col.names = c("Site_Code", "Place", "Pollutant", "Time", "pollution_Level"))
df <- df[,-2:-1]
ggplot(df, aes(Time, pollution_Level)) + geom_line() + ylab("Pollution Level") +
        xlab("Time") 
df$Time <- dmy_hm(df$Time)
minute(df$Time) <- 0
df2<- df[!(df$Time %within% interval(ymd("2012-04-02"), ymd_hm("2017-12-31 23:00"))),]
df2$Pollutant <- as.factor(trimws(df2$Pollutant))
# df$Pollutant <- (trimws(df$Pollutant)) #character type output

dfsplit2 <- split(df2, df2$Pollutant)
#lapply(dfsplit1, summary)
#lapply(dfsplit1, str)

ggplot(dfsplit2$CO, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
  xlab("Time") + ggtitle("CO")

ggplot(dfsplit2$SO2, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
  xlab("Time") + ggtitle("SO2")

ggplot(dfsplit2$NO2, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
  xlab("Time") + ggtitle("NO2")

ggplot(dfsplit2$PM2.5, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
  xlab("Time") + ggtitle("PM2.5")

ggplot(dfsplit2$OZONE, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
  xlab("Time") + ggtitle("OZONE")

dfsplit2 <- lapply(dfsplit2, 
                  function(x){
                    if(hour(min(x$Time)) != 0){
                      x <- x[x$Time %within% interval(ymd(paste(year(min(x$Time)),"-",month(min(x$Time)),"-",day(min(x$Time)) + 1)), ymd_hm("2017-12-31 23:00")),]
                    }else{
                      x<-x
                    }
                    x <- x[-1]
                  }
)


#lapply(dfsplit2,dim)

first <- dfsplit2$OZONE
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
  first <- first[-1 * i,] #removing repeated row
}
#first[l,]

dfsplit2$OZONE <- first

first <- dfsplit2$CO
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

dfsplit2$CO <- first

first <- dfsplit2$NO2
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

dfsplit2$NO2 <- first


first <- dfsplit2$PM2.5
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

dfsplit2$PM2.5 <- first


first <- dfsplit2$SO2
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

dfsplit2$SO2 <- first



#################################################################################################




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
#lapply(dfsplit,dim)


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
        first <- first[-1 * i,] #removing repeated row
}
#first[l,]

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
        xlab("Time") + ggtitle("CO Data Wrangled")

ggplot(dfsplit$SO2, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("SO2 Data Wrangled")

ggplot(dfsplit$NO2, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("NO2 Data Wrangled")

ggplot(dfsplit$PM2.5, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("PM2.5 Data Wrangled")

ggplot(dfsplit$OZONE, aes(Time, pollution_Level)) + geom_line() + ylab("pollution Level") +
        xlab("Time") + ggtitle("OZONE Data Wrangled")


#lapply(dfsplit, head)
################################################ OVerfitting DataSet ################################

model <- lm(pollution_Level ~ Time, dfsplit$OZONE) #using linear regression
predictedY <- predict(model, dfsplit$OZONE)


if(FALSE){
        rmse <- function(error)
        {
                sqrt(mean(error^2))
        }}
# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse <- hydroGOF::rmse(predictedY, dfsplit$OZONE$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse)

modelsvm = svm(pollution_Level ~ Time, data = dfsplit$OZONE, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit$OZONE)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm <- hydroGOF::rmse(predictYsvm, dfsplit$OZONE$pollution_Level[!is.na(dfsplit$OZONE$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm)

g <- ggplot(dfsplit$OZONE[!is.na(dfsplit$OZONE$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
        geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best = Predicted_rmse_svm
cost_best = 1
epsilon_best = 0.1
gamma_best = NA # 1 / dimensions

for(c in 2^(1:9)){
        for(e in seq(0,1,0.1)){
                for(g in c(10^(-5:-1), 0.5, 1, 2)){
                        tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$OZONE, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
                        predictYsvm = predict(tunermodelsvm, dfsplit$OZONE)
                        
                        # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
                        Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit$OZONE$pollution_Level[!is.na(dfsplit$OZONE$pollution_Level)])
                        
                        if(Predicted_rmse_svm_new < Predicted_rmse_svm_best){
                                epsilon_best = e
                                cost_best = c
                                gamma_best = g
                        }
                }
        }
}

finalmodelsvm <- svm(pollution_Level ~ Time, data = dfsplit$OZONE, epsilon = epsilon_best, gamma = gamma_best, cost = cost_best, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm, dfsplit$OZONE)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final <- hydroGOF::rmse(predictYsvm, dfsplit$OZONE$pollution_Level[!is.na(dfsplit$OZONE$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final)

dev.new(width=5, height=5)
g <- ggplot(dfsplit$OZONE[!is.na(dfsplit$OZONE$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
        geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W_best = t(modelsvm$coefs) %*% modelsvm$SV
b_best = modelsvm$rho

# tune.svm not working
if(FALSE){
        tunedSVMResult1 <- tune(svm, monthly_pollution_mean ~ month_Year,  data = data,
                                ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))# gamma = 2^(-1:1)
        )
        plot(tunedSVMResult1)
        
        tunedModel1 <- tunedSVMResult1$best.model
        tunedPredictedY <- predict(tunedModel1, df2) 
        
        tunedModelRMSE1 <- hydroGOF::rmse(tunedPredictedY, df2$monthly_pollution_mean)
        paste("Root Mean Error on monthly data of tuned Support Vector Regression MOdel using RBF : ", tunedModelRMSE1)
}
#********************************************************************
        
        
model <- lm(pollution_Level ~ Time, dfsplit$CO) #using linear regression
predictedY <- predict(model, dfsplit$CO)



# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse <- hydroGOF::rmse(predictedY, dfsplit$CO$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse)

modelsvm = svm(pollution_Level ~ Time, data = dfsplit$CO, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit$CO)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm <- hydroGOF::rmse(predictYsvm, dfsplit$CO$pollution_Level[!is.na(dfsplit$CO$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm)

g <- ggplot(dfsplit$CO[!is.na(dfsplit$CO$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
  geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best = Predicted_rmse_svm
cost_best = 1
epsilon_best = 0.1
gamma_best = NA # 1 / dimensions

for(c in 2^(1:9)){
  for(e in seq(0,1,0.1)){
    for(g in c(10^(-5:-1), 0.5, 1, 2)){
      tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$CO, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
      predictYsvm = predict(tunermodelsvm, dfsplit$CO)
      
      # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
      Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit$CO$pollution_Level[!is.na(dfsplit$CO$pollution_Level)])
      
      if(Predicted_rmse_svm_new < Predicted_rmse_svm_best){
        epsilon_best = e
        cost_best = c
        gamma_best = g
      }
    }
  }
}

finalmodelsvm <- svm(pollution_Level ~ Time, data = dfsplit$CO, epsilon = epsilon_best, gamma = gamma_best, cost = cost_best, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm, dfsplit$CO)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final <- hydroGOF::rmse(predictYsvm, dfsplit$CO$pollution_Level[!is.na(dfsplit$CO$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final)

dev.new(width=5, height=5)
g <- ggplot(dfsplit$CO[!is.na(dfsplit$CO$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
  geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W_best = t(modelsvm$coefs) %*% modelsvm$SV
b_best = modelsvm$rho
##############################################################################################
model <- lm(pollution_Level ~ Time, dfsplit$NO2) #using linear regression
predictedY <- predict(model, dfsplit$NO2)



# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse <- hydroGOF::rmse(predictedY, dfsplit$NO2$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse)

modelsvm = svm(pollution_Level ~ Time, data = dfsplit$NO2, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit$NO2)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm <- hydroGOF::rmse(predictYsvm, dfsplit$NO2$pollution_Level[!is.na(dfsplit$NO2$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm)

g <- ggplot(dfsplit$NO2[!is.na(dfsplit$NO2$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
  geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best = Predicted_rmse_svm
cost_best = 1
epsilon_best = 0.1
gamma_best = NA # 1 / dimensions

for(c in 2^(1:9)){
  for(e in seq(0,1,0.1)){
    for(g in c(10^(-5:-1), 0.5, 1, 2)){
      tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$NO2, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
      predictYsvm = predict(tunermodelsvm, dfsplit$NO2)
      
      # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
      Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit$NO2$pollution_Level[!is.na(dfsplit$NO2$pollution_Level)])
      
      if(Predicted_rmse_svm_new < Predicted_rmse_svm_best){
        epsilon_best = e
        cost_best = c
        gamma_best = g
      }
    }
  }
}

finalmodelsvm <- svm(pollution_Level ~ Time, data = dfsplit$NO2, epsilon = epsilon_best, gamma = gamma_best, cost = cost_best, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm, dfsplit$NO2)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final <- hydroGOF::rmse(predictYsvm, dfsplit$NO2$pollution_Level[!is.na(dfsplit$NO2$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final)

dev.new(width=5, height=5)
g <- ggplot(dfsplit$NO2[!is.na(dfsplit$NO2$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
  geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W_best = t(modelsvm$coefs) %*% modelsvm$SV
b_best = modelsvm$rho

##############################################################################################
model <- lm(pollution_Level ~ Time, dfsplit$PM2.5) #using linear regression
predictedY <- predict(model, dfsplit$PM2.5)



# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse <- hydroGOF::rmse(predictedY, dfsplit$PM2.5$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse)
.
modelsvm = svm(pollution_Level ~ Time, data = dfsplit$PM2.5, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit$PM2.5)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm <- hydroGOF::rmse(predictYsvm, dfsplit$PM2.5$pollution_Level[!is.na(dfsplit$PM2.5$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm)

g <- ggplot(dfsplit$PM2.5[!is.na(dfsplit$PM2.5$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
  geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best = Predicted_rmse_svm
cost_best = 1
epsilon_best = 0.1
gamma_best = NA # 1 / dimensions

for(c in 2^(1:9)){
  for(e in seq(0,1,0.1)){
    for(g in c(10^(-5:-1), 0.5, 1, 2)){
      tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$PM2.5, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
      predictYsvm = predict(tunermodelsvm, dfsplit$PM2.5)
      
      # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
      Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit$PM2.5$pollution_Level[!is.na(dfsplit$PM2.5$pollution_Level)])
      
      if(Predicted_rmse_svm_new < Predicted_rmse_svm_best){
        epsilon_best = e
        cost_best = c
        gamma_best = g
      }
    }
  }
}

finalmodelsvm <- svm(pollution_Level ~ Time, data = dfsplit$PM2.5, epsilon = epsilon_best, gamma = gamma_best, cost = cost_best, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm, dfsplit$PM2.5)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final <- hydroGOF::rmse(predictYsvm, dfsplit$PM2.5$pollution_Level[!is.na(dfsplit$PM2.5$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final)

dev.new(width=5, height=5)
g <- ggplot(dfsplit$PM2.5[!is.na(dfsplit$PM2.5$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
  geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W_best = t(modelsvm$coefs) %*% modelsvm$SV
b_best = modelsvm$rho

##############################################################################################
model <- lm(pollution_Level ~ Time, dfsplit$SO2) #using linear regression
predictedY <- predict(model, dfsplit$SO2)



# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse <- hydroGOF::rmse(predictedY, dfsplit$SO2$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse)

modelsvm = svm(pollution_Level ~ Time, data = dfsplit$SO2, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit$SO2)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm <- hydroGOF::rmse(predictYsvm, dfsplit$SO2$pollution_Level[!is.na(dfsplit$SO2$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm)

g <- ggplot(dfsplit$SO2[!is.na(dfsplit$SO2$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
  geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best = Predicted_rmse_svm
cost_best = 1
epsilon_best = 0.1
gamma_best = NA # 1 / dimensions

for(c in 2^(1:9)){
  for(e in seq(0,1,0.1)){
    for(g in c(10^(-5:-1), 0.5, 1, 2)){
      tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$SO2, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
      predictYsvm = predict(tunermodelsvm, dfsplit$SO2)
      
      # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
      Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit$SO2$pollution_Level[!is.na(dfsplit$SO2$pollution_Level)])
      
      if(Predicted_rmse_svm_new < Predicted_rmse_svm_best){
        epsilon_best = e
        cost_best = c
        gamma_best = g
      }
    }
  }
}

finalmodelsvm <- svm(pollution_Level ~ Time, data = dfsplit$SO2, epsilon = epsilon_best, gamma = gamma_best, cost = cost_best, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm, dfsplit$SO2)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final <- hydroGOF::rmse(predictYsvm, dfsplit$SO2$pollution_Level[!is.na(dfsplit$SO2$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final)

dev.new(width=5, height=5)
g <- ggplot(dfsplit$SO2[!is.na(dfsplit$SO2$pollution_Level),], aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + 
  geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)

W_best = t(modelsvm$coefs) %*% modelsvm$SV
b_best = modelsvm$rho

########################## Test on test set #########################################

model <- lm(pollution_Level ~ Time, dfsplit$OZONE) #using linear regression
predictedY <- predict(model, dfsplit2$OZONE)


if(FALSE){
  rmse <- function(error)
  {
    sqrt(mean(error^2))
  }}
# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse2 <- hydroGOF::rmse(predictedY, dfsplit2$OZONE$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse2)

modelsvm = svm(pollution_Level ~ Time, data = dfsplit$OZONE, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit2$OZONE)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm2 <- hydroGOF::rmse(predictYsvm, dfsplit2$OZONE$pollution_Level[!is.na(dfsplit2$OZONE$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm2)


W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best2 = Predicted_rmse_svm2
cost_best2 = 1
epsilon_best2 = 0.1
gamma_best2 = NA # 1 / dimensions

for(c in 2^(1:9)){
  for(e in seq(0,1,0.1)){
    for(g in c(10^(-5:-1), 0.5, 1, 2)){
      tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$OZONE, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
      predictYsvm = predict(tunermodelsvm, dfsplit2$OZONE)
      
      # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
      Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit2$OZONE$pollution_Level[!is.na(dfsplit2$OZONE$pollution_Level)])
      
      if(Predicted_rmse_svm_new < Predicted_rmse_svm_best2){
        epsilon_best2 = e
        cost_best2 = c
        gamma_best2 = g
      }
    }
  }
}

finalmodelsvm2 <- svm(pollution_Level ~ Time, data = dfsplit$OZONE, epsilon = epsilon_best2, gamma = gamma_best2, cost = cost_best2, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm2, dfsplit2$OZONE)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final2 <- hydroGOF::rmse(predictYsvm, dfsplit2$OZONE$pollution_Level[!is.na(dfsplit2$OZONE$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final2)

W_best2 = t(modelsvm$coefs) %*% modelsvm$SV
b_best2 = modelsvm$rho


######################## Change here ############################################

########################## Test on test set #########################################

model <- lm(pollution_Level ~ Time, dfsplit$CO) #using linear regression
predictedY <- predict(model, dfsplit2$CO)


if(FALSE){
        rmse <- function(error)
        {
                sqrt(mean(error^2))
        }}
# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse2 <- hydroGOF::rmse(predictedY, dfsplit2$CO$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse2)

modelsvm = svm(pollution_Level ~ Time, data = dfsplit$CO, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit2$CO)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm2 <- hydroGOF::rmse(predictYsvm, dfsplit2$CO$pollution_Level[!is.na(dfsplit2$CO$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm2)


W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best2 = Predicted_rmse_svm2
cost_best2 = 1
epsilon_best2 = 0.1
gamma_best2 = NA # 1 / dimensions

# 7.3 * 9 * 11 * 7
for(c in 2^(1:9)){# 9
        for(e in seq(0,1,0.1)){ # 11
                for(g in c(10^(-5:-1), 0.5, 1, 2)){ # 7
                        tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$CO, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
                        predictYsvm = predict(tunermodelsvm, dfsplit2$CO)
                        
                        # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
                        Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit2$CO$pollution_Level[!is.na(dfsplit2$CO$pollution_Level)])
                        
                        if(Predicted_rmse_svm_new < Predicted_rmse_svm_best2){
                                epsilon_best2 = e
                                cost_best2 = c
                                gamma_best2 = g
                        }
                }
        }
}

finalmodelsvm2 <- svm(pollution_Level ~ Time, data = dfsplit$CO, epsilon = epsilon_best2, gamma = gamma_best2, cost = cost_best2, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm2, dfsplit2$CO)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final2 <- hydroGOF::rmse(predictYsvm, dfsplit2$CO$pollution_Level[!is.na(dfsplit2$CO$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final2)

W_best2 = t(modelsvm$coefs) %*% modelsvm$SV
b_best2 = modelsvm$rho

########################## Test on test set #########################################

model <- lm(pollution_Level ~ Time, dfsplit$NO2) #using linear regression
predictedY <- predict(model, dfsplit2$NO2)


if(FALSE){
        rmse <- function(error)
        {
                sqrt(mean(error^2))
        }}
# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse2 <- hydroGOF::rmse(predictedY, dfsplit2$NO2$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse2)

modelsvm = svm(pollution_Level ~ Time, data = dfsplit$NO2, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit2$NO2)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm2 <- hydroGOF::rmse(predictYsvm, dfsplit2$NO2$pollution_Level[!is.na(dfsplit2$NO2$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm2)


W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best2 = Predicted_rmse_svm2
cost_best2 = 1
epsilon_best2 = 0.1
gamma_best2 = NA # 1 / dimensions

for(c in 2^(1:9)){
        for(e in seq(0,1,0.1)){
                for(g in c(10^(-5:-1), 0.5, 1, 2)){
                        tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$NO2, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
                        predictYsvm = predict(tunermodelsvm, dfsplit2$NO2)
                        
                        # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
                        Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit2$NO2$pollution_Level[!is.na(dfsplit2$NO2$pollution_Level)])
                        
                        if(Predicted_rmse_svm_new < Predicted_rmse_svm_best2){
                                epsilon_best2 = e
                                cost_best2 = c
                                gamma_best2 = g
                        }
                }
        }
}

finalmodelsvm2 <- svm(pollution_Level ~ Time, data = dfsplit$NO2, epsilon = epsilon_best2, gamma = gamma_best2, cost = cost_best2, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm2, dfsplit2$NO2)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final2 <- hydroGOF::rmse(predictYsvm, dfsplit2$NO2$pollution_Level[!is.na(dfsplit2$NO2$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final2)

W_best2 = t(modelsvm$coefs) %*% modelsvm$SV
b_best2 = modelsvm$rho

########################## Test on test set #########################################

model <- lm(pollution_Level ~ Time, dfsplit$SO2) #using linear regression
predictedY <- predict(model, dfsplit2$SO2)


if(FALSE){
        rmse <- function(error)
        {
                sqrt(mean(error^2))
        }}
# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse2 <- hydroGOF::rmse(predictedY, dfsplit2$SO2$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse2)

modelsvm = svm(pollution_Level ~ Time, data = dfsplit$SO2, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit2$SO2)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm2 <- hydroGOF::rmse(predictYsvm, dfsplit2$SO2$pollution_Level[!is.na(dfsplit2$SO2$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm2)


W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best2 = Predicted_rmse_svm2
cost_best2 = 1
epsilon_best2 = 0.1
gamma_best2 = NA # 1 / dimensions

for(c in 2^(1:9)){
        for(e in seq(0,1,0.1)){
                for(g in c(10^(-5:-1), 0.5, 1, 2)){
                        tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$SO2, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
                        predictYsvm = predict(tunermodelsvm, dfsplit2$SO2)
                        
                        # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
                        Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit2$SO2$pollution_Level[!is.na(dfsplit2$SO2$pollution_Level)])
                        
                        if(Predicted_rmse_svm_new < Predicted_rmse_svm_best2){
                                epsilon_best2 = e
                                cost_best2 = c
                                gamma_best2 = g
                        }
                }
        }
}

finalmodelsvm2 <- svm(pollution_Level ~ Time, data = dfsplit$SO2, epsilon = epsilon_best2, gamma = gamma_best2, cost = cost_best2, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm2, dfsplit2$SO2)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final2 <- hydroGOF::rmse(predictYsvm, dfsplit2$SO2$pollution_Level[!is.na(dfsplit2$SO2$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final2)

W_best2 = t(modelsvm$coefs) %*% modelsvm$SV
b_best2 = modelsvm$rho

########################## Test on test set #########################################

model <- lm(pollution_Level ~ Time, dfsplit$PM2.5) #using linear regression
predictedY <- predict(model, dfsplit2$PM2.5)


if(FALSE){
        rmse <- function(error)
        {
                sqrt(mean(error^2))
        }}
# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse2 <- hydroGOF::rmse(predictedY, dfsplit2$PM2.5$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse2)

modelsvm = svm(pollution_Level ~ Time, data = dfsplit$PM2.5, scale=FALSE, probability=TRUE) #svg w/o normalising
predictYsvm = predict(modelsvm, dfsplit2$PM2.5)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm2 <- hydroGOF::rmse(predictYsvm, dfsplit2$PM2.5$pollution_Level[!is.na(dfsplit2$PM2.5$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm2)


W = t(modelsvm$coefs) %*% modelsvm$SV # coefficient
b = modelsvm$rho # unit

Predicted_rmse_svm_best2 = Predicted_rmse_svm2
cost_best2 = 1
epsilon_best2 = 0.1
gamma_best2 = NA # 1 / dimensions

for(c in 2^(1:9)){#9
        for(e in seq(0,1,0.1)){#11
                for(g in c(10^(-5:-1), 0.5, 1, 2)){#7
                        tunermodelsvm = svm(pollution_Level ~ Time, data = dfsplit$PM2.5, epsilon = e, gamma = g, cost = c,  scale=FALSE, probability=TRUE)
                        predictYsvm = predict(tunermodelsvm, dfsplit2$PM2.5)
                        
                        # Predicted_rmse_svm_new <- rmse(modelsvm$residuals)
                        Predicted_rmse_svm_new <- hydroGOF::rmse(predictYsvm, dfsplit2$PM2.5$pollution_Level[!is.na(dfsplit2$PM2.5$pollution_Level)])
                        
                        if(Predicted_rmse_svm_new < Predicted_rmse_svm_best2){
                                epsilon_best2 = e
                                cost_best2 = c
                                gamma_best2 = g
                        }
                }
        }
}

finalmodelsvm2 <- svm(pollution_Level ~ Time, data = dfsplit$PM2.5, epsilon = epsilon_best2, gamma = gamma_best2, cost = cost_best2, scale=FALSE, probability=TRUE)
predictYsvm = predict(finalmodelsvm2, dfsplit2$PM2.5)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm_final2 <- hydroGOF::rmse(predictYsvm, dfsplit2$PM2.5$pollution_Level[!is.na(dfsplit2$PM2.5$pollution_Level)])
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm_final2)

W_best2 = t(modelsvm$coefs) %*% modelsvm$SV
b_best2 = modelsvm$rho


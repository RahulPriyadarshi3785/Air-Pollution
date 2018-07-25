pollutionLevels <- function(){
        
packages <- c("dplyr", "lubridate", "ggplot2", "hydroGOF", "e1071")
if(length(setdiff(packages, rownames(installed.packages()))) > 0){
        install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, require, character.only = TRUE)

setwd("C:/Users/hp/Desktop")
dir.create("Kanpur_Vansh1")
setwd("Kanpur_Vansh1")
# Place data in Downloads folders
df <- read.csv("C:/Users/hp/Downloads/aqi-data-kanpur.csv", header = FALSE, na.strings = " None", col.names = c("Site_Code", "Place", "Pollutant", "Time", "pollution_Level"))
# dim(df)
# head(df)
# str(df)



df1 <- df[!is.na(df$pollution_Level),]
df1$Time <- as.POSIXct(strptime(df1$Time, " %A, %d %b %Y, %H:%M"))
df1 <- df1[df1$Time < as.POSIXct("2018-01-01"),]
# str(df1)
# dim(df1)

# Resetting  DRivers 
dev.off()

png("timeSeries_Daily.png", width=840, height=484)
g <- ggplot(df1, aes(Time, (pollution_Level)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + geom_line()
dev.off()

png("log_timeSeries_Daily.png", width=840, height=484)
g <- ggplot(df1, aes(Time, log(pollution_Level)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + geom_line() +
        geom_abline(col = "blue")
dev.off()

model <- lm(pollution_Level ~ Time, df1)
predictedY <- predict(model, df1)

if(FALSE){
        rmse <- function(error)
        {
                sqrt(mean(error^2))
        }}
# Predicted_rmse <- rmse(model$residuals)
Predicted_rmse <- hydroGOF::rmse(predictedY, df1$pollution_Level)
paste("Root Mean Error of Linear Regression MOdel : ", Predicted_rmse)

png("linear_Regression_Model.png", width=840, height=484)
g <- ggplot(df1, aes(Time, (pollution_Level)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + 
        geom_point(aes(x=Time, y=predictedY), colour="blue", pch = 4)
dev.off()
if(FALSE){
modelsvm = svm(pollution_Level ~ Time, data = df1)
predictYsvm = predict(modelsvm, df1)

# Predicted_rmse_svm <- rmse(modelsvm$residuals)
Predicted_rmse_svm <- hydroGOF::rmse(predictYsvm, df1$pollution_Level)
paste("Root Mean Error of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svm)

png("SVM_Model.png", width=840, height=484)

g <- ggplot(df1, aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + 
        geom_point(aes(x=Time, y=predictYsvm), colour="red", pch = 4)
dev.off()

tunedSVMResult <- tune(svm, pollution_Level ~ Time,  data = df1,
                       ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))# gamma = 2^(-1:1)
)
plot(tunedSVMResult)

tunedModel <- tunedSVMResult$best.model
tunedPredictedY <- predict(tunedModel, df1) 

png("SVM_Model_tuned.png", width=840, height=484)

g <- ggplot(df1, aes(Time, pollution_Level))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + 
        geom_point(aes(x=Time, y=tunedPredictedY), colour="red", pch = 4)
dev.off()

tunedModelRMSE <- hydroGOF::rmse(tunedPredictedY, df1$pollution_Level)
paste("Root Mean Error of tuned Support Vector Regression MOdel using RBF : ", tunedModelRMSE)


W = t(tunedModel$coefs) %*% tunedModel$SV
b = tunedModel$rho
}
df2 <- df1 %>% mutate(month_Year = as.integer(format(Time, "%Y%m"))) %>% group_by(month_Year, Pollutant) %>% summarise(monthly_pollution_mean = mean(pollution_Level))
# str(df2)
# tail(df2)


png("log_Monthly_Classified_TimeSeries.png", width=840, height=484)
g <- ggplot(df2, aes(month_Year, log(monthly_pollution_mean)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) +
        geom_smooth(method = "lm", se = FALSE, col = "steelblue") +
        geom_line() +
        geom_abline(col = "blue")
dev.off()
# g <- ggplot(df2, aes(month_Year, (monthly_pollution_mean)))
# g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) +geom_smooth(method = "lm", se = FALSE, col = "steelblue") +geom_line()


model1 <- lm(monthly_pollution_mean ~ month_Year, df2)
predictedY <- predict(model1, df2)

# Predicted_rmse1 <- rmse(model1$residuals)
Predicted_rmse1 <- hydroGOF::rmse(predictedY, df2$monthly_pollution_mean)

paste("Root Mean Error on monthly data of Linear Regression MOdel using RBF : ", Predicted_rmse1)


png("linear_Regression_Model_Monthly.png", width=840, height=484)
g <- ggplot(df2, aes(month_Year, (monthly_pollution_mean)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + 
        geom_point(aes(x=month_Year, y=predictedY), colour="blue", pch = 4)
dev.off()


if(FALSE){
model1svm = svm(monthly_pollution_mean ~ month_Year, df2)
predictedYsvr = predict(model1svm, df2)

# Predicted_rmse_svm1 <- rmse(model1svm$residuals)
Predicted_rmse_svr1 <- hydroGOF::rmse(predictedYsvr, df2$monthly_pollution_mean)

paste("Root Mean Error on monthly data of Support Vector Regression MOdel using RBF : ", Predicted_rmse_svr1)


png("SVM_Model_Monthly.png", width=840, height=484)
g <- ggplot(df2, aes(month_Year, (monthly_pollution_mean)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + 
        geom_point(aes(x=month_Year, y=predictedYsvm), colour="red", pch = 4)
dev.off()


tunedSVMResult1 <- tune(svm, monthly_pollution_mean ~ month_Year,  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))# gamma = 2^(-1:1)
)
plot(tunedSVMResult1)

tunedModel1 <- tunedSVMResult1$best.model
tunedPredictedY <- predict(tunedModel1, df2) 

png("SVM_Model_Monthly_tuned.png", width=840, height=484)
g <- ggplot(df2, aes(month_Year, (monthly_pollution_mean)))
g + geom_point(alpha = 1/3) + facet_grid(Pollutant~.) + 
        geom_point(aes(x=month_Year, y=tunedPredictedY), colour="red", pch = 4)
dev.off()

tunedModelRMSE1 <- hydroGOF::rmse(tunedPredictedY, df2$monthly_pollution_mean)
paste("Root Mean Error on monthly data of tuned Support Vector Regression MOdel using RBF : ", tunedModelRMSE1)

W1 = t(tunedModel1$coefs) %*% tunedModel1$SV
b1 = tunedModel1$rho
}
# Use ARIMA Model instead....
}
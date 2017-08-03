library(quantmod)
library(fBasics)
library(corrplot)
library(caret)
library(tseries)
library(kernlab)

# load US Oil (USO) price from Yahoo
getSymbols('USO', from = "2014-12-09", to = "2015-12-09")
USO_Adj <- USO$USO.Adjusted

# load Exxon Mobil (XOM) price from Yahoo
getSymbols('XOM', from = "2014-12-09", to = "2015-12-09")
XOM_Adj <- XOM$XOM.Adjusted

# load Chevron (CVX) price from Yahoo
getSymbols('CVX', from = "2014-12-09", to = "2015-12-09")
CVX_Adj <- CVX$CVX.Adjusted

# load 	Conocoz Phillip (COP) price from Yahoo
getSymbols('COP', from = "2014-12-09", to = "2015-12-09")
COP_Adj <- COP$COP.Adjusted

# load 	Occidental Petroleum (OXY) price from Yahoo
getSymbols('OXY', from = "2014-12-09", to = "2015-12-09")
OXY_Adj <- OXY$OXY.Adjusted

# load 	Eog Resource (EOG) price from Yahoo
getSymbols('EOG', from = "2014-12-09", to = "2015-12-09")
EOG_Adj <- EOG$EOG.Adjusted

# load Anadarko Petroleu (APC) price from Yahoo
getSymbols('APC', from = "2014-12-09", to = "2015-12-09")
APC_Adj <- APC$APC.Adjusted

# load Phillips 66 (PSX) price from Yahoo
getSymbols('PSX', from = "2014-12-09", to = "2015-12-09")
PSX_Adj <- PSX$PSX.Adjusted

# load Valero Energy (VLO) price from Yahoo
getSymbols('VLO', from = "2014-12-09", to = "2015-12-09")
VLO_Adj <- VLO$VLO.Adjusted

# load Marathon Petroleu (MPC) price from Yahoo
getSymbols('MPC', from = "2014-12-09", to = "2015-12-09")
MPC_Adj <- MPC$MPC.Adjusted

# load Devon Energy (DVN) price from Yahoo
getSymbols('DVN', from = "2014-12-09", to = "2015-12-09")
DVN_Adj <- DVN$DVN.Adjusted

#Plotting all the variables
par(mfrow = c(2,2))
plot(XOM_Adj)
plot(CVX_Adj)
plot(COP_Adj)
plot(OXY_Adj)
plot(EOG_Adj)
plot(APC_Adj)
plot(PSX_Adj)
plot(VLO_Adj)
plot(MPC_Adj)
plot(DVN_Adj)

# Creating a data frame of all data
data.set <- data.frame(cbind(USO_Adj,XOM_Adj,CVX_Adj,COP_Adj,OXY_Adj,EOG_Adj,APC_Adj,PSX_Adj,VLO_Adj,MPC_Adj,DVN_Adj))

#Shapiro Test for NOrmality

shapiro.test(data.set$USO.Adjusted)

shapiro.test(data.set$XOM.Adjusted)

shapiro.test(data.set$CVX.Adjusted)

shapiro.test(data.set$COP.Adjusted)

shapiro.test(data.set$OXY.Adjusted)

shapiro.test(data.set$EOG.Adjusted)

shapiro.test(data.set$APC.Adjusted)

shapiro.test(data.set$PSX.Adjusted)

shapiro.test(data.set$VLO.Adjusted)

shapiro.test(data.set$MPC.Adjusted)

shapiro.test(data.set$DVN.Adjusted)

#Box Plot
boxplot(data.set)

#LOgnormal Transformation

USO_Adj<-diff(log(USO_Adj))

XOM_Adj<-diff(log(XOM_Adj))

CVX_Adj<-diff(log(CVX_Adj))

COP_Adj<-diff(log(COP_Adj))

OXY_Adj<-diff(log(OXY_Adj))

EOG_Adj<-diff(log(EOG_Adj))

APC_Adj<-diff(log(APC_Adj))

PSX_Adj<-diff(log(PSX_Adj))

VLO_Adj<-diff(log(VLO_Adj))

MPC_Adj<-diff(log(MPC_Adj))

DVN_Adj<-diff(log(DVN_Adj))

# Creating new data frame of all data
data.set <- data.frame(cbind(USO_Adj,XOM_Adj,CVX_Adj,COP_Adj,OXY_Adj,EOG_Adj,APC_Adj,PSX_Adj,VLO_Adj,MPC_Adj,DVN_Adj))

#Removing the NA's
data.set<-data.set[-1,]


#For Removing Date Writing and Re-Reading the Dataset
write.csv(data.set, file="data.set.csv")
data.set1<-read.csv("data.set.csv")
data.set<-data.set1[,-1]

#Checking the new dataset for names and Class
data.set
names(data.set)
class(data.set)

#Boxplot After Transformation
boxplot(data.set)

#Plotting the new transformed variables
par(mfrow = c(2,2))
plot(XOM_Adj)
plot(CVX_Adj)
plot(COP_Adj)
plot(OXY_Adj)
plot(EOG_Adj)
plot(APC_Adj)
plot(PSX_Adj)
plot(VLO_Adj)
plot(MPC_Adj)
plot(DVN_Adj)

#Finding corelations
A<-cor(data.set,use="complete")
head(A)
dataCorr <- cor(data.set)
dim(dataCorr)
corrplot(dataCorr, order = "hclust", tl.cex = .75)

#Data Splitting

data.set_Split <- sample(2, nrow(data.set), replace = TRUE, prob = c(0.6,0.4))
data.Train <- data.set[data.set_Split==1,]
data.Test <- data.set[data.set_Split==2,]

## Applying Linear Regression MOdel
set.seed(500)
lm <- lm(TC~,data.Train)
summary(lm)

predicted_Values <- predict(lm,data.Test)
predicted_Values


#plot model
plot(lm)

#Calculating AIC & BIC Values
AIC(lm)
BIC(lm)

#ANOVA
anova(lm)



## Applying Support vector machine model (SVM)
set.seed(500)
svmFit <- train(USO.Adjusted ~ ., data = data.set, method = "svmRadial", preProc = c("center", "scale"), tuneLength = 10)
trControl = trainControl(method = "repeatedcv", repeats = 5)
svmFit
predict(svmFit)
plot(svmFit)s

## Applying Generalized linear model (glm) - logistic regression 
set.seed(500)
logisticReg <- train(USO.Adjusted ~ ., data = data.set, method = "glm", trControl = trainControl(method = "repeatedcv", repeats = 5))
logisticReg
predict(logisticReg)


# Appling Time Series ARIMA Models

install.packages("tseries")
library(tseries)
mydata<- read.csv("data.set.csv")
attach(mydata)
mydata = mydata[-1,]
#mydata = forecastArima(oil)
mydata=data.set
View(mydata)

# Defining variables
Y <- mydata$USO.Adjusted
d.Y <- diff(Y)
t <- mydata$X

# Descriptive statistics and plotting the data
summary(Y)
summary(d.Y)

plot(t,Y, type="l")
plot(d.Y, type="l")

# Dickey-Fuller test for variable
adf.test(Y, alternative="stationary", k=0)
adf.test(Y, alternative="explosive", k=0)

# Augmented Dickey-Fuller test
adf.test(Y, alternative="stationary")

# DF and ADF tests for differenced variable
adf.test(d.Y, k=0)
adf.test(d.Y)

# ACF and PACF
acf(Y)
pacf(Y)

acf(d.Y)
pacf(d.Y)

# ARIMA(1,0,0) or AR(1)
arima(Y, order = c(1,0,0))

# ARIMA(2,0,0) or AR(2)
arima(Y, order = c(2,0,0))

# ARIMA(0,0,1) or MA(1)
arima(Y, order = c(0,0,1))

# ARIMA(1,0,1) or AR(1) MA(1)
arima(Y, order = c(1,0,1))

# ARIMA on differenced variable 
#ARIMA(1,1,0)
arima(d.Y, order = c(1,0,0))

# ARIMA(0,1,1)
arima(d.Y, order = c(0,0,1))

# ARIMA(1,1,1)
arima(d.Y, order = c(1,1,1))

# ARIMA(1,1,3)
arima(d.Y, order = c(1,0,3))

# ARIMA(2,1,3)
arima(d.Y, order = c(2,0,3))


# ARIMA(1,0,1) forecasting
mydata.arima101 <- arima(Y, order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima101, n.ahead=100)
plot (Y, type ="l")
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")
predict(mydata.arima101,1)


# ARIMA(1,1,1) forecasting
mydata.arima111 <- arima(d.Y, order = c(1,1,1))
mydata.pred1 <- predict(mydata.arima203, n.ahead=100)
plot (d.Y, type ="l")
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")
predict(mydata.arima111,2)
View(mydata)
************************************END*********************************
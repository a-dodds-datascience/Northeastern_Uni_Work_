#Andrew Dodds
#Assignment 3
# 4/21/20

####### Part I ############################################################################################################################
#Perform	exponential	smoothing	forecasts	on	the	Alphabet stock	prices	to	forecast	
#the	price	for	1/15/2020 Use	successive	values	of	0.15,	0.35,	0.55,	and	0.75	for	the	
#smoothing	parameter	??.	Calculate	the	MSE	of	each	forecast,	Use	the	MSEs	of	your	
#forecasts	to	determine	the	value	of	??	that	has	provided	the	most	accurate	forecast.	
library(forecast)
library(readxl)
library(ggplot2)
library(data.table)
library(lattice)
library(broom)
library(dplyr)
library(corrplot)
library(plotly)
library(hrbrthemes)
#install.packages("ggfortify")
library("ggfortify")
#Import the alphabet dataset
bet <- read_xlsx("C:/Users/thech/Desktop/Decision Support/Alphabet.xlsx")

#view the dataset
str(bet)
head(bet)

#Make sure the date column is recognized as a date
bet$Date <- as.Date(bet$Date)
str(bet)


#Plot the time series graph of the share price for Google for the close price for each time period
p <- bet %>%
  ggplot( aes(x=Date, y=Close))        +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2")           +
  ylab("Share Price GOOGL (NASDAQ)")   +
  theme_ipsum()

#Make it interactive with ggplotly command - lit
p <- ggplotly(p)
p

#Perform	exponential	smoothing	forecasts	on	the	Alphabet stock	prices	to	forecast	
#the	price	for	1/15/2020 Use	successive	values	of	0.15,	0.35,	0.55,	and	0.75
# Perform exponential smoothing on Alphabet stock prices
bet <- ts(bet[,3])
#
#sm <- c(.15, .35, .55, .75)
#
#n  <- length(sm)
#
# for( i in 1:n){
#   lit <- ses(bet, h = 10, alpha = i)
#   
#   }
#obj[lit]
#
## Part 1.1 ##
#########loess#############################
bet15 <- ses(bet, h = 10, alpha = 0.15) #
bet35 <- ses(bet, h = 10, alpha = 0.35) #
bet55 <- ses(bet, h = 10, alpha = 0.55) #
bet75 <- ses(bet, h = 10, alpha = 0.75) #
#########################################


## Part 1.2 ##
#Calculate the Mean Squared Error of each of the exponentially smoothed forecasts
MSE_15 <- mean(bet15$residuals^2)
MSE_35 <- mean(bet35$residuals^2)
MSE_55 <- mean(bet55$residuals^2)
MSE_75 <- mean(bet75$residuals^2)
MSE_15
MSE_35
MSE_55
MSE_75
### Problem 2 ###########################################################################################################################
#Use	your	exponential	smoothing	forecast	with	alpha =.75,	and	perform	adjusted	
#exponential	smoothing	forecasts	on	the	Alphabet stock	prices	to	forecast	the	price	
#for	01/15/2020

## Part 2.1
#Find the fit of ses75
fit_75 <- bet75[['mean']][1]

## Part 2.2
#Add the fitted data to the original vector
fitted_2 <- ts(rbind(bet, fit_75))
fitted_2

## Part 2.3
#plot the fitted 2 just for S&G
plot(fitted_2,
     ylab = "Share Price GOOGL (NASDAQ)",
     col  = "darkorchid"                ,
     main = "Price of GOOGL - Smoothed (ses75)")

## Perform adjusted exponential smoothing forecast with an alpha of 0.75 to predict the stock price for 1/15/2020 (Since this date has 
# passed, we can test the accuracy of our test compared to the actual value) Use	successive	values	of	0.15,	0.25,	0.45,	and	0.85
#for	the	trend	parameter	??

## Part 2.4
bet_adj_15 <- holt(fitted_2, alpha = 0.75, beta = .15) 
bet_adj_25 <- holt(fitted_2, alpha = 0.75, beta = .25)
bet_adj_45 <- holt(fitted_2, alpha = 0.75, beta = .45)
bet_adj_15
bet_adj_25
bet_adj_45
#bet_adj_85 <- holt(fitted_2, alpha = 0.75, beta = .85) #This one dont work too good


#beta <- c(.15,.25,.45,85)
#RMSE <- NA
#for(i in seq_along(beta)){
#fit <- holt(bet, alpha = .75 , beta = beta[i], h = 100,
#            seasonal = c("additive", "multiplicative"),
#            start.periods = 2, l.start = NULL, b.start = NULL,
#            s.start = NULL,
#            optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
#            optim.control = list())
#RMSE[i] <-accuracy(fit, bet)[2,2]
#}



## Part 2.5
# Calculate the Mean Squared Error of each forecast
adj_mse_15 <- mean(bet_adj_15$residuals^2)
adj_mse_25 <- mean(bet_adj_25$residuals^2)
adj_mse_45 <- mean(bet_adj_45$residuals^2)
adj_mse_15
adj_mse_25
adj_mse_45
#adj_mse_85 <- mean(bet_adj_85$residuals^2)


### Problem 3 ##########################################################################################################################
# Perform	a	simple	regression	analysis	of	Alphabet stock	prices versus	periods	(i.e.,	1,	
# 2,	3,.)	to	forecast	the	Alphabet stock	value	for	1/15/2020

## Part 3.1 #####
bet <- read_xlsx("C:/Users/thech/Desktop/Decision Support/Alphabet.xlsx")

plot(bet$Period, bet$Close,
     ylab = "Share Price GOOGL (NASDAQ)"    ,
     xlab = "Time in #Weeks from 7/19/2019" ,
     main = "Prediction of Share Price"     ,
     col  = "darkblue"                      ,
     pch  = 20,
)
regbet <- lm(Close ~ Period, data = bet)
abline(regbet,
       col = "darkorchid2")

#View summary information of the regression
sumregbet <- summary(regbet)


## Part 3.2 #####
# Predict value for Period = 125 (01/15/2020)
indipendant_value <- data.frame(Period = 125)
predicted_value <- predict(regbet, indipendant_value)
print(predicted_value)

## 3A  #####
# Coefficient of correlation
CoeffRegBet <- sumregbet$coefficients
CoeffRegBet


# Coefficient of determination
coefdetbet <- sumregbet$r.squared
coefdetbet

## 3B ######
#Create A	histogram	of	the	regression	residuals,	and	the	interpretation	of	its	shape
resbet <- sumregbet$residuals 

hist(resbet,
          main = "Histogram of Regression Residual for Alphabet",
          xlab = "Regression Residuals",
          ylab = "Frequency Percentage of Total",
          col  = "cyan")


### 3-C ###
# Sample normal probability distribution
normalprobdist <- rnorm(length(bet$Close), 0, 1)


# Chi-square normality test of residuals
chisq.test(resbet, normalprobdist)

### 3-D ###
# Normal Probability Plot (QQPlot)
qqplot(resbet, normalprobdist,
       main = "QQ Plot of Regression Residual and Sample Normal Distribution",
       xlab = "GOOGL (NASDAQ) Regression Residuals",
       ylab = "Sample of Normal Distribution")
qqline(resbet,normalprobdist,
       col = "red",
       lty = 2)

### 3-E ###
# scatter	plot	of	residuals	versus	time	to	study	their	independency,	and	the	
# interpretation	of	the	shape	of	the	scatter	plot

checkresiduals(regbet)

### 3-F ###
# A	scatter	plot	of	residuals	versus	the	predicted	stock	values	to	study	their	
#homoscedasticity,	and	the	interpretation	of	the	shape	of	the scatter	plot

autoplot(regbet)



















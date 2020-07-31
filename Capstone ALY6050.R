## Andrew Dodds 
## ALY6050 Captstone Project - Hodrick Prescott Decomposition
## 5/17/20 

#Import the necessary packages
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(corrplot)
library(forecast)
library(tseries)
library(lubridate)
library(data.table)
library(reprex)
#install.packages("mFilter")
#install.packages("quantmod")
library(mFilter)
library(quantmod)

#Import the dataset and View it
ggl <- read_xlsx("C:/Users/thech/Desktop/Week 6 Project-part 2-Data.xlsx")
#View(ggl)

#Insert the column names because its essentially useless right now
colnames(ggl) <- c("Date", "Open", "High", "Low", "Close", "Adj Close", "Vol")

#Drop the unnecessary columns using the subset function
ggl <- subset(ggl, select = -c(2,3,4,5,7))

#Create the log of the adjusted closing price for google stock
ggl$log_close <- log(ggl$`Adj Close`)


#Create inputs for the HP Filter
period <- 365

lambda <- 100*(period)^2

#Use HP Filter to create the decomposition for the log of ggl adjusted adjusted closing price
hpggl <- hpfilter(ggl$log_close ,
                freq = lambda   , 
                type = "lambda"
                 )

#Assign the decomposed columns into somthing a bit more readable
ggl_logprice <- hpggl$x
ggl_trend    <- hpggl$trend
ggl_cycle    <- hpggl$cycle


# Plotting the log(Alphabet) and Trend component
par(bg = "black")
plot(ggl_logprice       , 
     type = "o"         ,
     col  = "grey97"    , 
     col.axis = "white",
     xlab = "Time"      ,
     ylab = "Log Price" ,
     lwd  = 2           ,
     main = "HP Filter - Alphabet Stock Price",
     col.main = "white"
     )
lines(ggl_trend, col = "lightskyblue")


# Plot alphabet price against trend component
ggl_hod_pres_price <- data.frame(exp(1)^(ggl_trend))
names(ggl_hod_pres_price) <- "GOOG USD decomposed"



#Create the Plot
par (bg = "grey94")
plot(ggl_logprice               ,
     type = "o"                 ,
     col  = "darkorchid4"       , 
     xlab = "Time"              ,
     ylab = "Price USD"         ,
     main = "Google Stock Price Analysis"
     )
lines(ggl_trend, col = "springgreen4", lwd=2)


#Plot the cyclical component of the Google stock time series decomposition
par(bg = "grey94")
plot(ggl_cycle                                          ,
     t    ='n'                                          ,
     main = "Cyclical Component of Alphabet Stock Price",
     xlab = "Time - Days"                               ,
     ylab = "Variation in Trend vs. Log Price")
lines(ggl_cycle, 
      col = "springgreen3"
      )






















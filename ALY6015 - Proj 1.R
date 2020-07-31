### Assignment 1 Andrew Dodds
### 4/12/20
# Professor Vladimir Shapiro 

#Load the necessary packages
library(normalr)

#Create Pop Out Window GUI
windows()

#use the trees dataset and save it as an object 
trees <- trees

#View the datset trees
View(trees)

#view the 5 number summary of "trees"
summary(trees)

#Create histogram of dataset trees
x <- (trees$Girth)
hat <-trees$Height
h <- hist(trees$Girth, trees$Height,
     breaks = 7                    ,
     col    = "darkorchid2"        ,
     ylab   = "Height"             ,
     xlab   = "Girth"              ,
     main   = "Histogram of Tree Height by Girth")

#Plot a density plot over the histogram
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)

#Boxplot over histogram
# Layout to split the screen
tap <- data.frame(x, hat)
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(tap , horizontal=TRUE , ylim=c(0,25), xaxt="n" , col=rgb(0.8,0.8,0,0.6) , frame=F)

par(mar=c(4, 3.1, 1.1, 2.1))
hist(x , breaks=7 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="B.Plot on Hist" , xlab="Girth", ylab = "Height", xlim=c(5,25))
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

################################################################################################################################################
#Repeat the above commands with a normalized dataset
#Create histogram of dataset trees
#Fix the plotting area with par(mar())
windows()
par(mar=c(1,5,1,1))
x   <- normalise(trees$Girth)
hat <- normalise(trees$Height)
h   <- hist(x, hat                        ,
            breaks = 31                   ,
            col    = "darkorchid2"        ,
            ylab   = "Height"             ,
            xlab   = "Girth"              ,
            main   = " Normalized Histogram of Tree Height by Girth")


#Plot a density plot over the histogram
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)

#Boxplot over histogram
# Layout to split the screen
tap <- data.frame(x, hat)
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(tap , horizontal=TRUE  , 
        ylim=c(0,25)           ,
        xaxt="n"               ,
        col=rgb(0.8,0.8,0,0.5) ,
        frame=F)

par(mar=c(4, 3.1, 1.1, 2.1))
hist(x , breaks=7 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="B.Plot on Hist" , xlab="Girth", ylab = "Height", xlim=c(5,25))
xfit <- seq(min(x), max(x)       , length=40)
yfit <- dnorm(xfit, mean=mean(x) , sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)



browseURL("https://www.youtube.com/watch?v=-kFUSgR7ujI")


#########################################################################################################################################
#Question 2
# Calculate the standardized height (z-score) of a U.S. woman who is 71 inches tall
# Z-score = (x - u)/sigma

z <- ((71 - 63.8)/2.7)
z

#Body weight and Height dataset from Kaggle
#Import the bodyweight dataset and save as FAT
fat <- read.csv("C:/Users/thech/Downloads/500-person-gender-height-weight-bodymassindex/500_Person_Gender_Height_Weight_Index.csv")

#Create dataframe for average height of men and women
AvgHeight <- fat %>% group_by(fat$Gender) %>% summarise(average = mean(fat$Height))
AvgHeight

#Create a dataframe for average weight of men and women
AvgWeight <- fat %>% group_by(fat$Gender) %>% summarise(average = mean(fat$Weight))
AvgWeight

#Caclulate the average weight and hight for both men and women together
mean(fat$Height)
mean(fat$Weight)


#Calculate the standard deviation in for weight and height by gender
SDWeight <- fat %>% group_by(fat$Gender) %>% summarise(stdev = sd(fat$Weight))
SDWeight <- data.frame(SDWeight)

SDHeight <- fat %>% group_by(fat$Gender) %>% summarise(stdev = sd(fat$Height))
SDHeight
browseURL("https://www.youtube.com/watch?v=-kFUSgR7ujI")
############################################################################################################################################
#Question 3
#Import Kaggle dataset for height and weight
wut <- read.csv("C:/Users/thech/Downloads/weight-height/weight-height.csv")
library(ggplot2)

#View the dataset
View(wut)

#Split the dataset according to gender and make a new dataframe
#wut_split <- split(wut, wut$Gender)

#wutmen   <- wut_split$Male
#wutwomen <- wut_split$Female

ggplot(wut, aes(Height, fill = Gender)) + geom_density(alpha = 0.2) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

browseURL("https://www.youtube.com/watch?v=-kFUSgR7ujI")
###########################################################################################
#Question number 4
#Create z score data using the data from the question above 

zscore <- data.frame(scale(wut$Height, center = TRUE, scale = TRUE))
browseURL("https://www.youtube.com/watch?v=-kFUSgR7ujI")



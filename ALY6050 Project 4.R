#ALY6050 Project 4
#Andrew Dodds
# 5/3/2020

#Load necessary packages
library(EnvStats)
library(lattice)
library(data.table)
library(ggplot2)
library(datetime)

############################# Problem 1 #####################################
#Annual	demand	has	a	triangular	probability	distribution	between	15000	and	25000	units	
#with	a	peak	of	20000	units. If	using	R,	perform	a	simulation	consisting	of	1000	
#occurrences,	and	calculate	the	minimum	total	cost	for	each	occurrence.

#establish the fixed values as objects
CPU  <- 90        #(CPU = Cost to purchase each unit - fixed, not dependent on volume)
CPO  <- 195       #(CPO = Cost to the businesse to process an order)
COI  <- 0.15      #(COI = Cost of Inventory - this value is set at 15%)
CCPU <- COI * CPU #(CCPU = Carrying Cost per Unit - this value is 13.50 for this problem but is variable)

#Create the triangular distribution for annual sales volume
An_dmnd <- rtri(1000    ,
           min  = 15000 ,
           max  = 25000 ,
           mode = 20000
           )

#Createa a vector for order quantities from 1 - 25000
Orderqty <- c(1:25000)
View(Orderqty)



############################## Problem 3 #####################################
# Determine	the	probability	distribution	that	best	fits	the	order	quantity.
# Calculating economic order quantity (EOQ)
EOQ <- ((2 * An_dmnd * CPO) / (CPU * CCPU))^0.5
head(EOQ)
f<- hist(EOQ,
     main = "Economic Order Quantity  Histogram (EOQ)",
     xlab = "Economic Order Quantity"           ,
     col  = "darkorchid3")
xfit<-seq(min(EOQ),max(EOQ),length=40)
yfit<-dnorm(xfit,mean=mean(EOQ),sd=sd(EOQ))
yfit <- yfit*diff(f$mids[1:2])*length(EOQ)
lines(xfit, yfit, col="darkgoldenrod1", lwd=2)

head(EOQ)

#Calculate the Annual Orders
An_orders <- (An_dmnd / EOQ)

#Calculate the annual Ordering costs (AOC)
AOC <- (CPO * An_orders)

#Calculate the annual holding costs (AHC)
AHC <- (CCPU * (An_dmnd))



############################## Problem 2  #####################################
#Determine	the	probability	distribution	that	best	fits	the	minimum	total	cost.

minTot_Cost <- AOC + AHC
f<-  hist(minTot_Cost,
          main = "Minimum Total Cost  Histogram",
          ylab = "Frequency"                    ,
          xlab = "Minimum Total Cost"           ,
          col  = "darkorchid3")
x <- minTot_Cost
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(f$mids[1:2])*length(x)
lines(xfit, yfit, col="cyan", lwd=2)


#Create 1000 iterations to insert orders to Data Frame and calculate the minimum total cost for each occurance (1000)
set.seed(123)
for (i in 1:1000){
  total_orders <- ( An_dmnd / Orderqty[i])
}

head(total_orders)
tail(total_orders)


##Calculate the total cost for each order in total_orders
#set.seed(123)
#for (i in 1:1000){
#minTot_Cost <- ((CPO * total_orders[i]) + CCPU * Orderqty)
#}




############################## Problem 4   #####################################
# Determine	the	probability	distribution	that	best	fits	the	order	quantity.
#Generate a histogram and probability distribution that best fits the annual number of orders
h <- hist(An_orders,
      main = "Annual Number of Orders",
      xlab = "Number of Orders in a Year",
      col  = "cyan")
x<-An_orders
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)





############################### Problem 2 #####################################
## Determine	the	probability	distribution	that	best	fits	the	minimum	total	cost
#
#CostMatrix <- as.data.frame(0,0,0)
#CostMatrix <- cbind(total_orders, minTot_Cost, Orderqty)
#View(CostMatrix)
#
#
#plot(Orderqty, minTot_Cost)
#
#
#d <- density(An_dmnd)
#plot(d,
#     col="blue4",
#     main = "Density Plot Annual Orders")
#
#hist(An_dmnd               ,
#     col  = "cyan"         ,
#     xlab = "Annual Demand",
#     main = "Hist. of Annual Dmnd")




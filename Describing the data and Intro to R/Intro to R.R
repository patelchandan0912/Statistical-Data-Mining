# Introduction to R

rm(list=ls())
# Arithmetic Operations with Constants
5/4
5\4                                     # This should give you an error
5^4
5-4^2
5+2/3
# Functions
sqrt(5)
log(5)
exp(5)

# Using Variables
x<-5                                    # R does not require predefined data types
x
x^4
sqrt(x)
Sqrt(x)                                 # R is case-sensitive
y<-sqrt(x)                              # Storing values in another variable

# Loops
for (year in 2010:2018) {
  print(year)
}

for (i in 5:10) {                       # What does this for loop do?
  if (i %% 2){
    print (i)
  }
}

for (i in 10:5) {
  if (i %% 2) { 
    next
  }
  print (i^2)
}

# Vectors
x <- c(1, 2, 3, 4, 5)                   # c is the concatenation operator
x
x[3]
x[3] + 10
sum(x)
sqrt(x)
y <- x^2; y

# Sequence and Replication
x <- seq(1,10)
x
x^2
sum(x)
y <- exp(x); y
x <- seq (1, 10, by=2); x
y <- c(1, 1, NA, seq(5)) ; y            # R is agnostic to data types

x <- rep(1,10); x
y <- rep(c(1, 2), 5); y
length(x)                               # Number of values in the vector
?rep                                    # Help on replication operator

z <- rnorm(10)                          # What does this command do?
z <- rnorm(10, mean=0, sd=1);z

# Vector Algebra
x <- seq(1,10); x
y <- rep(1,10); y
z <- x^2 + y; z

x <- seq(1, 10, by=2); x                # What happens when you add vectors of different sizes?
x+y
x <- seq(1, 10, by=3); x        
x+y

# Question: Compute the sum of the sequence (1, 1.1, 1.2, 1.3, …, 9.9, 10)
x<- seq(1,10, by =0.1);x
sum(x)

#Dataframe 
a = data.frame(x=1:3, y=c("NY", "CA", "IL"));a
# Matrices
x <- matrix(c(1, 2, 3, 11, 12, 13), nrow=2, ncol=3);x
x <- matrix(rnorm(30), nrow=6, ncol=5); x
x <- matrix(rnorm(30), 30, 1); x
x <- matrix(rnorm(30), 30); x
x <- matrix(rnorm(30), 6, 5); x

x[4,1]                                 # What does this command do?
x[4,]
x[,3]
x[4,3] <- 50                           # What does this command do?
x[4, ] <- 100; x

x <- matrix(round(rnorm(20), 2), nrow=5, ncol=4); x
y <- seq(1,5); y
z <- rbind(x, y); z                    # rbind: row bind
z <- cbind(x, y); z                    # cbind: column bind

z <- data.frame(z); z                  # z is now a data frame
# Data frame: a struture for storing a table of data in  structured format

# Working with Data Files
setwd("c:/Users/abhatt/Desktop/Data")  # setwd: set working directory (for Windows users)
setwd("/Users/anol/Desktop/Data")      # For Mac users
read.csv("HousePrices.csv")		# Read data from an external csv file

rm(list = ls())
library(rio)
d <- import("C:/Users/CHANDAN PATEL/Desktop/Study USF/Material/Swayam/Study Material/Study Material/Data Sets/HousePrices.csv")       # Data stored in a data frame
names(d)                               # List names of all variables in a data frame
dim(d)
head(d)
View(d)                                # Note: the "V" is caps
nrow(d)
d[1,7]
d[1,]
d[,7]
d[1:5,]

d$Price
d$Neighborhood
unique(d$Neighborhood)
length(unique(d$Neighborhood))

# Question: What is the difference between a matrix and a data frame?
# Question: How is the data frame structure different from similar structures in other programming languages?

# Subsetting data from data frames
mydata <- data.frame(price=d$Price, sqft=d$SqFt)
names(mydata)
mydata
mydata$price

d[d$Offers>=4,]                        # Conditional logic
d[d$Offers>=4 & d$SqFt>2000,]
new.data <- d[d$Offers>=4 & d$SqFt>2000,]    
new.data

new.data <- subset(d, Bedrooms>=4); new.data
new.data <- subset(d, Bedrooms>=4 & Bathrooms>=2); new.data
new.data <- subset(d, Brick=="Yes"); new.data
new.data <- subset(d, Neighborhood %in% c("East", "North")); new.data
new.data <- subset(d, Bedrooms>=4, select=c(Price, SqFt, Bedrooms, Brick)); new.data

attach(d)                             # Stores data frame d to memory
Price
detach(d)
Price

# Descriptive Data Analysis
d$Price                               # Numeric variables
range(d$Price)
mean(d$Price)
sd(d$Price)
quantile(d$Price)
summary(d$Price)

levels(d$Brick)                        # Categorial variables
levels(d$Neighborhood)                 
table(d$Neighborhood)
table(d$Brick, d$Neighborhood)         # What does this command do?  This is contingency table
barplot(table(d$Brick, d$Neighborhood),col=c("red", "blue"))
# Question 1: What is the mean price of houses with 4+ bedrooms and 2+ bathrooms?
# Question 2: Calculate the mean price of brick house by neighborhood (East/North/West).
# Question 3: Calculate the mean price of houses by bedrooms.
# Homework: Create a for loop to answer Question 3.

# Univariate Plots
hist(d$Price)
?hist
hist(d$Price, breaks=10, col="green")
hist(d$Price, main="Histogram of Price", xlab="Price", ylab="Count", col="red")
# Question: What does this histogram say about the relationship between mean and median house prices?

boxplot(d$Price)
boxplot(d$Price ~ d$Brick)
boxplot(d$Price ~ d$Neighborhood)
boxplot(d$Price ~ d$Brick*d$Neighborhood)
boxplot(d$Price ~ d$Brick*d$Neighborhood, col=c("red", "blue"))

barplot(table(d$Neighborhood))
barplot(table(d$Brick, d$Neighborhood))

# Overlaying histograms with Kernel Density Functions
# Kernel density function is a non-parametric probability density function of a random variable
den<-density(d$Price)                  # Compute the density function of the variable of interest
den

plot(den)
plot(den, main="Kernel Density of Price", col="red")
hist(d$Price, col="green", main="Histogram of Price", prob=T)
lines(den, col="red")

# prob="T" rescales the y-axis of a histogram to a 0-1 range, to match with the density function
# Question: Draw two separate histograms for house prices with and without a brick exterior. 
# These are called conditional histograms. Are the distributions for brick and non-brick houses symmetric?
is.factor(d$Brick)
d$Brick=as.factor(d$Brick)
# Bivariate Plots
plot (d$SqFt, d$Price)                 # Option 1: plot (x,y)
plot(d$Price ~ d$SqFt)                 # Option 2: plot (y~x)
plot(d$SqFt , d$Price, xlab="SqFt", ylab="Price", main="Price vs. SqFt")
plot(d$SqFt, d$Price, col=c("red","blue")[d$Brick])  ## brick should be factor variable here otherwise it wont work
legend(x="topright", legend=levels(d$Brick), col=c("red", "blue"), pch=1)
# Question: Any pattern you see in the conditional plot of Price vs. SqFt?

plot(d$SqFt, d$Price, col=as.numeric(d$Brick), cex=d$Offers) # What are we doing here?

plot(d$Price)                          # What happens when you use plot with one variable
plot(d$Neighborhood)
plot(table(d$Brick, d$Neighborhood))
?plot

# Using R Graphics Libraries
install.packages("lattice")            # Install R library
library(lattice)                       # Load library into memory
xyplot(d$Price~d$SqFt | d$Brick, col=as.numeric(d$Bedrooms), cex=d$Offers)
# Check out the lattice library; it mas many interesting plot options
?lattice
?cloud
?plot
install.packages("tabplot")
library(tabplot)
tableplot(d[,1:7])

# Bivariate correlations
cor(d$Price, d$SqFt)
cor(d[1:5])                           # Correlations work on numeric variables only
install.packages("corrplot")
library(corrplot)
m <- cor(d[1:5])
corrplot(m, method="circle")

# Dynamic Visualization of Time Series Data
install.packages("googleVis")
library(googleVis)
install.packages("rio")
library (rio)
d <- import("C:/Users/CHANDAN PATEL/Desktop/Study USF/Material/Swayam/Study Material/Study Material/Data Sets/DrunkDriving2.csv")
head(d)
new.data <- subset(, state<=10, select=c(state, year, fatalityrate, unemprate))
new.data
m <- gvisMotionChart(data=mydata, idvar="Brick", timevar="A_Time_Series_var")
plot (m)
# For googleVis example, see https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html



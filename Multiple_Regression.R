#### Install Packages ####
if(require("caret")==FALSE){install.packages("caret")}
if(require("corrplot")==FALSE){install.packages("corrplot")}
if(require("Amelia")==FALSE){install.packages("Amelia")}
if(require("binr")==FALSE){install.packages("binr")}
if(require("pandas")==FALSE){install.packages("pandas")}

#### Load Data ####
setwd("/Users/PROUD/Dropbox/ubiqum/Blackwell/Multiple_Regression")
library(caret)
library(corrplot)
library(Amelia)
library(binr)
library(pandas)
Existing<-read.csv("ChristianProudExisting.csv")

#### Explore Data ####

#### Preprocess Data ####

# change variable names #
names(Existing)<-c("Product_type","Product_ID","Prices","x5Stars","x4Stars","x3Stars","x2Stars",
                   "x1Star","Positive","Negative","Recommend","Best","Weight","Depth","Profit_margin",
                   "Volume","Competitors","Professional","Age")

# remove product weight/depth/length/height #
Existing$Weight <- NULL
Existing$Depth <- NULL
Existing$Width <- NULL
Existing$Heigth <- NULL

# change variable types #
Existing$Product_type <-as.factor(Existing$Product_type)
Existing$Prices <-as.numeric(Existing$Prices)
Existing$Best_seller_rank <-as.numeric(Existing$Best_seller_rank)
Existing$Profit_margin <- as.numeric(Existing$Profit_margin)

# replace missing values #
Existing$Best_seller_rank[is.na(Existing$Best_seller_rank)] <- median(Existing$Best_seller_rank,na.rm=T)
is.na(Existing$Best_seller_rank)
sum(is.na(Existing$Best_seller_rank))

summary(Existing)

Existing$x5Stars[Existing$x5Stars=="0"]<-mean(Existing$x5Stars)

# bin Best_seller_rank #
str(Existing$Best_seller_rank)
summary(Existing$Best_seller_rank)
intervals <- bins(Existing$Best_seller_rank,5, max.breaks = 6, exact.groups = FALSE, verbose = FALSE, errthresh = 0.1, minpts = NA)
intervals

# dummify the data #
newdataframe <- dummyVars(" ~ .", data = Existing)
readydata <- data.frame(predict(newdataframe, newdata = Existing))

# check data types #
str(readydata)

# looking at all missing values #

missmap(readydata, main = "Missing Values")

is.na(readydata)
sum(is.na(readydata))

readydata$attributeWithMissingData <- NULL

#### Find Correlations ####

corrdata <- cor(readydata) 
corrdata

# correlation matrix #
corrplot(corrdata)

####master####

###change Christian###

#### Install Packages ####
if(require("caret")==FALSE){install.packages("caret")}
if(require("corrplot")==FALSE){install.packages("corrplot")}
if(require("Amelia")==FALSE){install.packages("Amelia")}
if(require("binr")==FALSE){install.packages("binr")}
if(require("ggcorrplot")==FALSE){install.packages("ggcorrplot")}
if(require("party")==FALSE){install.packages("party")}
if(require("tidyr")==FALSE){install.packages("tidyr")}
if(require("ggpubr")==FALSE){install.packages("ggpubr")}
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

#### Load Data ####
setwd("/Users/PROUD/Dropbox/ubiqum/Blackwell/Multiple_Regression")
library(caret)
library(corrplot)
library(Amelia)
library(binr)
library(ggcorrplot)
library(party)
library(tidyr)
library(ggpubr)
Existing<-read.csv("ChristianProudExisting.csv")

#### Preprocess Data ####

# change variable names #
names(Existing)<-c("Num","Product_type","Product_ID","Prices","x5Stars","x4Stars","x3Stars","x2Stars",
                   "x1Star","Positive","Negative","Recommend","Best_seller","Weight","Depth",
                   "Width","Height","Profit_margin","Volume","Competitors","Profession","Age")

# removing identifier #
Existing$Num<-NULL
Existing$Product_ID<-NULL

# look at missing values #
is.na(Existing)
sum(is.na(Existing))
is.na(Existing$Best_seller)
sum(is.na(Existing$Best_seller))

str(Existing)

# change variable types #
Existing$Prices<-as.numeric(Existing$Prices)
Existing$Best_seller<-as.numeric(Existing$Best_seller)
Existing$Profit_margin<-as.numeric(Existing$Profit_margin)

# list attribute types #
sapply(Existing, class)
str(Existing)

#### Explore Data ####

attributes(Existing)
summary(Existing)
str(Existing)

ggscatter(Existing, x = "x4Stars", y = "Volume", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Product Type", ylab = "Volume")

# plots / histograms #
cor(Existing[,unlist(lapply(Existing, is.numeric))])

ggplot(Existing, aes(x=Volume)) + 
  geom_histogram(aes(y=..density..), bindwidth =0.5, colour = "black", fill = "white") + 
  geom_density(alpha =0.2, fill = "coral")+
  geom_vline(aes(xintercept=median(Volume)), linetype ="dashed", size = 0.5)

# replace missing values #

# remove Volume outliers > 6000 #
Existing<-Existing[!Existing$Volume>6000,]

# remove x5Stars because of overfit #
Existing$x5Stars<-NULL

# remove product weight/depth/length/height #
Existing$Best_seller<-NULL
Existing$Weight<-NULL
Existing$Depth<-NULL
Existing$Width<-NULL
Existing$Height<-NULL

ct<-ctree(Volume~., data=Existing, controls = ctree_control(maxdepth=3))
plot(ct)

#### Find Correlations ####
# Existing$Product_type<-NULL #
# corr<-round(cor(Existing),2) #

#correlation matrix #
cor(Existing[,unlist(lapply(Existing, is.numeric))])

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE)

#### Decision Tree ####

ct<-ctree(Volume~., data=Existing, controls = ctree_control(maxdepth=3))
plot(ct)

#### Preprocess Data for Classifiers ####

# dummify the data #
Existing_dummy <- dummyVars(" ~ .", data = Existing)

Existing_ready <- data.frame(predict(Existing_dummy, newdata = Existing))

# check data types #
str(Existing_ready)

#### Find Correlations ####

corrdata <- cor(Existing_ready) 
corrdata

# correlation matrix #
corrplot(corrdata)

#### Data Partition ####

# set.seed and create a 75/25 train/test split of data set #
set.seed(147)

inTraining <- createDataPartition(Existing$Volume, p = .75, list = FALSE) ## using data dist. $brand ##
training <- Existing[inTraining,]
testing <- Existing[-inTraining,]

# ten-fold cross-validation #
fitControl <- trainControl (## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# creating a predictive model k-NN #
#### k-NN Fit 1 - all variables ####
knnFit1 <- train(Volume~.,data = training, method = "knn", trControl=fitControl,
                 preProcess=c("center", "scale"),tuneLength=5)
summary(knnFit1)
knnFit1

#### k-NN Fit 2 - using 

##
#### k-NN predictor variables ####
predictors(knnFit1)
#### Make Predictions for k-NN ####
testPredknn1 <- predict(knnFit1, testing)
# measure performance #
postResample(testPredknn1, testing$Existing)
# predicted vs. actual plot #
plot(testPredknn1, testing$Existing)
plot(knnFit1)




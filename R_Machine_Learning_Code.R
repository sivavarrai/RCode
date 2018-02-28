#This Algorithm showcases the supervised machine learning using logistic regression model  
#to predict whether the flight will be delayed or not based on various predictors>

#<downloaded file 501332214_T_ONTIME.csv from the US Transportation Dept website for a particular time period>

#checking the current working directory
getwd()

#setting up current working directory 
setwd("/Users/Desktop/R-Programming/R Working Directory/")


#importing the file 501332214_T_ONTIME.csv to flightorigdata Dataframe by using base function read.csv2 and 
#to make arguments header=TRUE to consider first row as column names and argument stringAsFactors as FALSE to 
# NOT consider categorical values as Factors.
flightorigdata<-read.csv2("501332214_T_ONTIME.csv",sep=",",header = TRUE, stringsAsFactors = FALSE)

#Examing the structure and first 5 observations of the dataframe
str(flightorigdata)
head(flightorigdata,5)

#Examing the no of rows and columns using base function nrow and ncol
nrow(flightorigdata)
ncol(flightorigdata)

#creating a vector airports to specifically filter popular airports in the large data set.
airports<-c('JFK','SFO','CLT','ATL','LAX','ORD','DFW','LAS','PHX')

#filter dataframe flightorigdata using airports in airports vector
flightorigdata<-subset(flightorigdata,DEST %in% airports & ORIGIN %in% airports)

#removing dummy columns which are not required as part of analysis by assigning NULL value:
flightorigdata$X<-NULL

# checking correlation between various predictors inorder to remove if there is high co-relation 
# between the variables and consider any one of the predictor .
#from below example we can see that the correlation is 1 between the mentioned columns so we are removing 
#columns with redundant information which is already considered in the model
cor(flightorigdata$ORIGIN_AIRPORT_ID,flightorigdata$ORIGIN_AIRPORT_SEQ_ID)
cor(flightorigdata$DEST_AIRPORT_ID,flightorigdata$DEST_AIRPORT_SEQ_ID)
flightorigdata$ORIGIN_AIRPORT_SEQ_ID<-NULL
flightorigdata$DEST_AIRPORT_SEQ_ID<-NULL

#Lets drop unique carrier since its already duplicated in the column carrier
flightorigdata$UNIQUE_CARRIER<-NULL

#Filtering the observations from  df with arrival time and departure time not null and without blankspaces
onTimeData <- flightorigdata[!is.na(flightorigdata$ARR_DEL15) & flightorigdata$ARR_DEL15!="" & 
                         !is.na(flightorigdata$DEP_DEL15) & flightorigdata$DEP_DEL15!="",]

#checking the number of observations
nrow(onTimeData)

#changing the column datatypes 
onTimeData$DISTANCE<- as.integer(onTimeData$DISTANCE)
onTimeData$DIVERTED<- as.integer(onTimeData$DIVERTED)
onTimeData$CANCELLED<- as.integer(onTimeData$CANCELLED)
onTimeData$DEP_DEL15 <- as.factor(onTimeData$DEP_DEL15)
onTimeData$ARR_DEL15 <- as.factor(onTimeData$ARR_DEL15)
onTimeData$DEST_AIRPORT_ID <- as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID <- as.factor(onTimeData$ORIGIN_AIRPORT_ID)
onTimeData$DAY_OF_WEEK <- as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$DEST <- as.factor(onTimeData$DEST)
onTimeData$ORIGIN <- as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK <- as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$CARRIER <- as.factor(onTimeData$CARRIER)

#checking the summary of the df:
summary(onTimeData$ARR_DEL15)

#install the "caret" package for building the regression model using the training data set:
install.packages("caret")
library("caret")

#set the random seed
set.seed(12345)

#considering only the main attributes out of dataframe
maincolumns <- c("ORIGIN","DEST","CARRIER","DAY_OF_WEEK","DEP_TIME_BLK","ARR_DEL15")

#filtering only main data columns from the data set
ontimedatafiltered<-onTimeData[,maincolumns]

#observing the top 10 observations
head(ontimedatafiltered,10)

#considering row no partition based on Arrvial delay for 15 minutes 
intrainobs<-createDataPartition(ontimedatafiltered$ARR_DEL15,p=0.70,list=FALSE)

#dividing train and test data sets 
traindatafiltered<-ontimedatafiltered[intrainobs,]
testdatafiltered<-ontimedatafiltered[-intrainobs,]

#checking the percentage of test data and train data 
nrow(traindatafiltered)/(nrow(traindatafiltered)+nrow(testdatafiltered))

nrow(testdatafiltered)/(nrow(traindatafiltered)+nrow(testdatafiltered))

#Building logisitic regression model from the train data set.
logisticregmode<- train(ARR_DEL15 ~.,data = traindatafiltered,model="glm",family='binomial',
                        trControl=trainControl(method="cv", number=10, repeats=10))

#Testing the logistic regression model on Test data
logistictestmodel<-predict(logisticregmode,testdatafiltered)

#creating the confusion matrix to see the performance of the model by check the misclassification.
confMatrix<-confusionMatrix(logistictestmodel, testDataFiltered[,"ARR_DEL15"])




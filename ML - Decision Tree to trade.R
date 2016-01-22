# Article URL https://inovancetech.com/blogML3.html 

install.packages("quantmod")
library("quantmod")
#Allows us to import the data we need and calculate the technical indicators

install.packages("rpart") 
library("rpart")
#Gives us access to the decision trees we will be using. (I had to update my version of R in order to install this one.)
install.packages("rpart.plot")
library("rpart.plot")
#Let's us easily create good looking diagrams of the trees.

# Next, let's grab all the data we will need and calculate the indicators.
startDate = as.Date("2013-05-01")     #The beginning of the date range we want to look at
endDate = as.Date("2015-12-08")       #The end of the date range we want to look at
getSymbols("RDUS", src = "google", from = startDate, to = endDate) #Retrieving the daily OHLCV of Bank of America’s stock from Yahoo Finance
RSI3<-RSI(Op(BAC), n= 3)              #Calculate a 3-period relative strength index (RSI) off the open price
EMA5<-EMA(Op(BAC),n=5)                #Calculate a 5-period exponential moving average (EMA)
EMAcross<- Op(BAC)-EMA5               #Let's explore the difference between the open price and our 5-period EMA
MACD<-MACD(Op(BAC),fast = 12, slow = 26, signal = 9)    #Calculate a MACD with standard parameters
MACDsignal<-MACD[,2]                  #Grab just the signal line to use as our indicator.
SMI<-SMI(Op(BAC),n=13,slow=25,fast=2,signal=9)  #Stochastic Oscillator with standard parameters
SMI<-SMI[,1]                          #Grab just the oscillator to use as our indicator

# Then calculate the variable we are looking to predict and build our data sets.
PriceChange<- Cl(BAC) - Op(BAC)             #Calculate the difference between the close price and open price
Class<-ifelse(PriceChange>0,"UP","DOWN")    #Create a binary classification variable, the variable we are trying to predict.
DataSet<-data.frame(RSI3,EMAcross,MACDsignal,SMI,Class)                   #Create our data set
colnames(DataSet)<-c("RSI3","EMAcross","MACDsignal","Stochastic","Class") #Name the columns
DataSet<-DataSet[-c(1:33),]                 #Get rid of the data where the indicators are being calculated
TrainingSet<-DataSet[1:250,]                #Use 2/3 of the data to build the tree
TestSet<-DataSet[251:348,]                  #And leave out 1/3 data to test our strategy
# Now that we have everything we need, let's build that tree!

DecisionTree<-rpart(Class~RSI3+EMAcross+MACDsignal+Stochastic,data=TrainingSet, cp=.001)
#Specifying the indicators to we want to use to predict the class and controlling the growth of the tree by 
# setting the minimum amount of information gained (cp) needed to justify a split.

# Great! We have our first decision tree, now let's see what we built.
prp(DecisionTree,type=2,extra=8)

#Nice plotting tool with a couple parameters to make it look good. If you want to play around with the visualization 
# yourself, "http://www.milbo.org/rpart-plot/prp.pdf" is a great resource.

printcp(DecisionTree)                 #shows the minimal cp for each trees of each size.
plotcp(DecisionTree,upper="splits")   #plots the average geometric mean for trees of each size.


PrunedDecisionTree<-prune(DecisionTree,cp=0.0272109)
#I am selecting the complexity parameter (cp) that has the lowest cross-validated error (xerror)

prp(PrunedDecisionTree, type=2, extra=8)

# Time to see how well it does over our test set.
table(predict(PrunedDecisionTree,TestSet,type="class"),TestSet[,5],dnn=list('predicted','actual'))

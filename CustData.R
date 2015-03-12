require(ggplot2)
#### Base Setup ####
setwd('/Users/rajeshmalpani/workspace/Data Science/Practical Data Science in R/Custdata')
custdata <- read.table('custdata.tsv', sep='\t', header = TRUE )
dim(custdata); head(custdata)
custTrain <- custdata[1:600,]         # create customer training data set
custCal <- custdata[601:800,]         # create customer calibaration data set
custTest <- custdata[801:1000,]       # create customer testing data set
custvars <- colnames(custdata)        # vector of column names
outcomes <- "health.ins"
variables <- setdiff(colnames(custTrain), c(outcomes))
categoricVars <- variables[sapply(custTrain[,variables],class) %in% c('factor','character', 'logical')]   # Note: 9 
numericalVars <- variables[sapply(custTrain[,variables],class) %in% c('numeric','integer')]   # Note: 10 
class(custdata)

plot(custTrain) 
ggplot(custTrain) + geom_histogram(aes(y=health.ins, y=marital.stat)) 

### Information Gain + Entropy for field selection ####
require(FSelector)

weights < information.gain(health.ins ~ income, custdata)




### brute force field selection ####


### GLM  ####
custFormula <- as.formula(paste('health.ins=="health.ins"', paste(custvars,collapse=' + '),sep=' ~ '))  
custModel <- glm(custFormula,family=binomial(link='logit'), data=custTrain)   # glm model with a bionomial distribution
custTrain$pred <- predict(custModel,newdata=custTrain, type='response')       # add glm model prediction 'response' to spamTrain
custTest$pred <- predict(custModel,newdata=custTest, type='response')         # add glm model prediction 'response' to spamTest
print(with(custTrain,table(y=health.ins,glmPred=pred>0.5)))                   # print confusion matrix with custTest, health.ins vs pred > 0.5 
summary(custTrain$pred)
mean(is.na(custTrain$pred))
rm(categoricVars, numericalVars)

table(custdata$is.employed, custdata$health.ins, useNA='ifany')
ggplot(custdata, aes(y=income/1000, x=age)) + geom_point() + geom_smooth(method="glm")
mod <- lm(custdata$health.ins ~ custdata$income+custdata$age)
summary(mod)
ggplot

### Tree  + kfold ####


### Clustering #####

data(iris)
class(iris)
head(iris)
rm(iris)

install.packages("ggplot2")
require(ggplot2)
#### Base Setup ####
setwd('/Users/rajeshmalpani/workspace/Data Science/Practical Data Science in R/Custdata')
custdata <- read.table('custdata.tsv', sep='\t', header = TRUE )
dim(custdata); head(custdata)


### Cleanup data  ####
summary(custdata[is.na(custdata$housing.type), c("recent.move", "num.vehicles")])
summary(custdata[c("housing.type", "recent.move", "num.vehicles")])   # look at selective summary for specific features
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "not active in work force",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))

cd = custdata

names(custdata)
summary(as.factor(custdata$is.employed.fix))
summary(custdata$income)
meanIncome <- mean(custdata$income)
Income.fix <- ifelse(is.na(custdata$income), meanIncome, custdata$income)
summary(Income.fix)

breaks <-c(0, 10000, 50000, 100000, 250000, 1000000)    # define income grouping ranges
Income.groups <- cut(custdata$income, breaks=breaks, include.lowest=T)  
summary(Income.groups)
Income.groups <- as.character(Income.groups)
Income.groups <- ifelse(is.na(Income.groups), "no income", Income.groups)
summary(as.factor(Income.groups))
missingIncome <- is.na(custdata$income)
Income.fix <- ifelse(is.na(custdata$income), 0, custdata$income)


load("exampleData.rData") 
summary(medianincome)
custdata <- merge(custdata, medianincome, by.x="state.of.res", by.y="State")    # Note: 2 
head(custdata)
summary(custdata$Median.Income.x); summary(custdata$Median.Income.y)
names(custdata)
summary(custdata[,c("state.of.res", "income", "Median.Income.x")]) 	# Note: 3 


custdata$income.norm <- with(custdata, income/Median.Income.x)   # Note: 4 
summary(custdata$income.norm)

custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)

brks <- c(0, 25, 65, Inf)    # Note: 1 
custdata$age.range <- cut(custdata$age, breaks=brks, include.lowest=T) 	# Note: 2 
summary(custdata$age.range)   # Note: 3 

summary(custdata$age)
meanage <- mean(custdata$age)
custdata$age.normalized <- custdata$age/meanage
summary(custdata$age.normalized)

stdage <- sd(custdata$age)     	# Note: 2 
meanage; stdage
custdata$age.normalized <- (custdata$age-meanage)/stdage 	# Note: 3 
summary(custdata$age.normalized)

signedlog10 <- function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

### Preparing Test, Train & Calliberation data #### 

custdata$gp <- runif(dim(custdata)[1])    # Note: 1 
testSet <- subset(custdata, custdata$gp < 0.1) 	                        # create customer testing data set
calSet <- subset(custdata, custdata$gp >= 0.1 & custdata$gp <= 0.3)     # create customer caliberation data set
trainingSet <- subset(custdata, custdata$gp > 0.3) 	                    # create customer training data set
dim(testSet)[1]
dim(calSet)[1]
dim(trainingSet)[1]

hh <- unique(hhdata$household_id)   # Note: 1 
households <- data.frame(household_id = hh, gp = runif(length(hh))) 	# Note: 2 
hhdata <- merge(hhdata, households, by="household_id") 	# Note: 3


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


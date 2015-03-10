
library('rpart')
# Chapter 1.3
setwd('/Users/rajeshmalpani/workspace/Data Science/Practical Data Science in R/Statlog')
load('GCDData.RData')
summary(creditdata)
head(creditdata)
model <- rpart(Good.Loan ~
                 Duration.in.month +
                 Installment.rate.in.percentage.of.disposable.income +
                 Credit.amount  +
                 Other.installment.plans,
               data=d,
               control=rpart.control(maxdepth=4),
               method="class")    # Building a decision tree of Good.Loan wiht 4 attributes and max depth of 4
model
resultframe <- data.frame(Good.Loan=creditdata$Good.Loan, pred=predict(model, type="class"))  # get Actual Good.Loan from data & Prediction
rtab <- table(resultframe)  # convert to a truth table / confusion matrix
rtab 
sum(diag(rtab)/sum(rtab))       # Model's accuracy 73% of predictions were accurate
sum(rtab[1,1] / sum(rtab[,1]))  # Model's precision : 76% of the applicants predicted as really did defualt
sum(rtab[1,1] / sum(rtab[1,]))  # Model's recall - ie. the model found 14% of defaulting loans
sum(rtab[2,1] / sum(rtab[2,]))  # Model's false positive 2% of good applicants were mistakenly identified as bad

# Chap 2.1
setwd('/Users/rajeshmalpani/workspace/Data Science/Practical Data Science in R/UCICar')
uciCar <- read.table('http://www.win-vector.com/dfiles/car.data.csv', sep=',', header = TRUE)
head(uciCar)
summary(uciCar)
class(uciCar)
dim(uciCar)
table(d$Purpose, d$Good.Loan)

# Chap 3.1 Exploring Data
setwd('/Users/rajeshmalpani/workspace/Data Science/Practical Data Science in R/Custdata')
custdata <- read.table('custdata.tsv', sep='\t', header = TRUE )
summary(custdata)
head(custdata)
summary(custdata$health.ins)
summary(custdata$marital.stat)
custdata[, custdata$income <= 0]
require(ggplot2)
require(scales)
ggplot(custdata) + geom_histogram(aes(x=age), binwidth=5, fill='gray')  # Histogram Plot of age distribution
ggplot(custdata) + geom_density(aes(x=age))                             # Line Plot of age distribution
ggplot(custdata) + geom_density(aes(x=income)) + scale_x_continuous(label=dollar) # plot line graph for income 
ggplot(custdata) + geom_density(aes(x=income)) 
    + scale_x_log10(breaks=c(100, 1000, 10000, 100000), label=dollar)    # plot line graph for income in logarithmic scale 
    + annotation_logticks(sides='bt')                                    # with log scaled tick marks on top & bottom of graph

ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")            # Bar chart of Marital status - Categorical variable distribution
          # Bar chart of State of Residence with flipped co-ordinates, and y - coord text scaled down to 80%
ggplot(custdata) + geom_bar(aes(x=state.of.res), fill="gray") + coord_flip() + theme(axis.text.y=element_text(size=rel(0.8)))
statesums <- table(custdata$state.of.res) # extract state of residence to a table
summary(statesums)
statef <- as.data.frame(statesums)        # conver table to a data frame
colnames(statef) <- c('state.of.res', 'count')  # rename column names
summary(statef)
statef <- transform(statef, state.of.res=reorder(state.of.res, count))  # order data by count
ggplot(statef) + geom_bar(aes(x=state.of.res, y=count), stat="identity", fill="gray") + coord_flip() + theme(axis.text.y=element_text(size=rel(0.8)))

x <- runif(100); y <- x^2 + 0.2*x;    # x = random distribution 0 thru 1, and y is quadratic equation of x
ggplot(data.frame(x=x,y=y), aes(x=x, y=y)) + geom_line()
# subset of data with reasonable age and income vales
custdata2 <- subset(custdata, (custdata$age > 0 & custdata$age < 100 & custdata$income > 0 ))
cor(custdata2$age, custdata2$income)    # get correlation of age to income
ggplot(custdata2, aes(y=income, x=age)) + geom_point() + ylim(0,200000)   # scatter plot with income limit of 200000, gives a scatter without regression
ggplot(custdata2, aes(y=income, x=age)) + geom_point() + geom_smooth(method="lm") + ylim(0,200000)   # scatter plot with income limit of 200000, gives a scatter with line regression lm applied
ggplot(custdata2, aes(y=income, x=age)) + geom_point() + geom_smooth() + ylim(0,200000)   # scatter plot with income limit of 200000, gives a scatter with smooth curve
ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) + geom_point(position = position_jitter(0.05, h=0.05)) + geom_smooth() # scatter plot of age to ins, with jitter dots
library(hexbin)   # hexbin plots  
ggplot(custdata2, aes(x=age, y=income)) + geom_hex(binwidth=c(5,10000)) + geom_smooth(color="white", se=F) + ylim(0,20000) # hex plot with smoothing curge in white suppressing standard error ribbon
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins)) # bar chart with marital status & health insurance stacked
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins), position="dodge") # bar chart with marital status & health insurance side by side
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins), position="fill") # bar chart with marital status & health insurance filled
# bar chart with marital status & health insurance filled, with points set below y axis made slightly transparent with alpha parameter, 
# and a rug of slight jitter points for legibility
ggplot(custdata, aes(x=marital.stat)) + geom_bar(aes(fill=health.ins,), position="fill") + geom_point(aes(y=-0.05), size=0.75, alpha=0.3, position=position_jitter(h=0.01))
# bar chart with marital status & house type side by side, with x label angled for better visibility
ggplot(custdata) + geom_bar(aes(x=housing.type, fill=marital.stat), position="dodge") + theme(axis.text.x = element_text(angle=45, hjust=1))
# bar chart of marital status & house type tiled, with x label angled for better visibility
ggplot(custdata) + geom_bar(aes(x=marital.stat), position="dodge", fill="darkgray") + facet_wrap(~ housing.type, scales="free_y") + theme(axis.text.x = element_text(angle=45, hjust=1))

# Chap 4 Managing Data
# 4.1 Cleaning Data
summary(custdata[is.na(custdata$housing.type), c("recent.move", "num.vehicles")])
summary(custdata[c("housing.type", "recent.move", "num.vehicles")])   # look at selective summary for specific features
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "not active in work force",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))
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

load("exampleData.rData") # load prepped data
summary(medianincome)
names(medianincome)
names(custdata)
custdata <- merge(custdata, medianincome,
                  by.x="state.of.res", by.y="State")   # merge both data frames
head(medianincome); head(custdata)
summary(custdata[,c("state.of.res", "income", "Median.Income.x", "Median.Income.y")])
custdata$income.norm <- with(custdata, income/Median.Income.x)
summary(custdata$income.norm)

custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)

brks <- c(0, 25, 65, Inf)       # define age ranges, lower and upper bounds
custdata$age.range <- cut(custdata$age, breaks=brks, include.lowest=T)  # add a new derived feature based on ranges for bucketing
summary(custdata$age.range)

summary(custdata$age)
meanage <- mean(custdata$age)   # get Mean of age
stdage <- sd(custdata$age)      # get SD of age
custdata$age.normalized <- (custdata$age-meanage)/stdage    # Use the mean value as the origin (or reference point) and rescale the distance from the mean by the standard deviation
summary(custdata$age.normalized)    

signedlog10 <- function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))   #  R function to calculate signed Log base 10
}

ggplot(custdata) + geom_density(aes(x=income))
ggplot(custdata) + geom_density(aes(x=log10(income)))

# 4.2 Sampling for modelling and validation
custdata$gp <- runif(dim(custdata)[1])
head(custdata$gp)
testSet <- subset(custdata, custdata$gp <= 0.1)     # 10 % data for test
trainingSet <- subset(custdata, custdata$gp > 0.1)  # 90 % data for training
dim(testSet)[1]
dim(trainingSet)[1]


# Part 2 - Chap 5 Modeling methods
summary(custdata$income)
custweight <- information.gain(health.ins~., custdata)
custweight*100
model <- lm(health.ins~income, custdata)
summary(model)
coef(model)
require(ggplot2)
ggplot(custdata, aes(x=income, y=age)) + geom_point(aes(x=custdata$health.ins)) + coord_flip() + geom_smooth(method="lm")
install.packages("FSelector")
require(FSelector)

data(iris)
head(iris)
weights <- information.gain(Species~., iris)
subset <- cutoff.k(weights, 2)
f <- as.simple.formula(subset, "Species")

print(f)

weights <- gain.ratio(Species~., iris)
print(weights)
subset <- cutoff.k(weights, 2)
f <- as.simple.formula(subset, "Species")
print(f)

as.dataframe(custdata$is.employed)

install.packages('rpart')
install.packages('ROCR')
install.packages('tree')

require(class)

var <= 'X'
paste(var, '>')

# Chap 7.5 plyr Package
require(plyr)               # dplyr is a newer version of plyr 
head(baseball)
baseball$sf[baseball$year < 1954] <- 0    # assign value 0 to sf for any that are before 1954
any(is.na(baseball$sf))                   # are there any na's 
baseball$hbp[is.na(baseball$hbp)] <- 0     # where baseball$hbp is na
baselball <- baseball[baseball$ab >= 50,]
baseball$OBP <- with(baseball, (h + bb + hbp)/ (ab + bb + hbp + sf)) # create new attribute for each row with the formula
tail(baseball)
obp <- function(data) {
  c(OBP = with(data, sum(h + bb + hbp) / sum(ab + bb + hbp + sf)))  # = gives the variable name as OBP, where as <- will crete a generic vector
}

careerOBP <- ddply(baseball, .variables="id", obp)    # split apply combine paradigm, all in a combination
head(careerOBP)
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing = TRUE),] 
# llply
theList <- list(A=matrix(1:9, 3), B=1:5, C=matrix(1:4,2), D=2)
theList 
lapply(theList, sum)    # base R function to apply function to lists, will provide sum of each sub list
llply(theList, sum)     # llply from plyr package applys function for given input list and produces a list output
identical(lapply(theList, sum), llply(theList, sum))  # compare if the two outputs produced are identical 

sapply(theList, sum)    # base R function sapply will apply function and produce a vector output
laply(theList, sum)     # plyr function laply takes list as input and produces an array output without array name
identical(sapply(theList, sum), laply(theList, sum)) # although the result is same output is different, i.e. w/o vector names

# Aggregate function
require(ggplot2)
head(diamonds)
aggregate(price ~ cut, diamonds, each(mean, median))  # produces price.mean and price.median by each cut
numcolwise(sum, na.rm=TRUE)(diamonds)                 # plyr way of applying colwise function to only numeric columns to data
sapply(diamonds[, sapply(diamonds, is.numeric)], sum) # base R way to get sum of numeric columns 

# 7.6 Combine datasets

sport <- c("Hockey", "Baseball", "Football")
league <- c("NHL", "MLB", "NFL")
trophy <- c("Stanley Cup", "Commissioner's cup", "Vince Lombardi Trophy")
sports1 <- cbind(sport, league, trophy) # column binding of 3 arrays 
sports2 <- data.frame(sport=c("Baseketball", "Golf"), league=c("NBA", "PGA"), trophy=c("Larry O'Brien Championship", "Wanamaker Trophy"))
sports2
sports <- rbind(sports1, sports2)   # rbind will bind two data sets as long as they have the same columns names
sports

# 7.7 Join datasets
setwd("/Users/rajeshmalpani/workspace/Data Science/JaredLander/")
codes <- read.table("./data/countryCodes.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")
countries<- read.table("./data/GovType.csv", stringsAsFactors = FALSE, header = TRUE, sep=",")
countrymerged <- merge(x=codes, y=countries, by.x="Country.name", by.y="Country")   # produce a merged data set by joining on col x & y
head(countrymerged)
require(plyr)
codes <- rename(codes, c(Country.name="Country"))   # rename column to new value
countryJoined <- join(x=codes, y=countries, by="Country")  # Join has a limitation where the column names for match have to be the same
head(countryJoined)

# Chapter 7.8 Switch storage paradigms
require(reshape2)
data(airquality)
head(airquality)
airmelt <- melt(airquality, id=c("Month", "Day"),             # pivot the data by id as Month an day and add rows to represent
                value.name="Value", variable.name="Metric")   # remaining columns as metric and value in a LONG format
head(airmelt, 10)
dim(airquality); dim(airmelt)
aircast <- dcast(airmelt, Month+Day ~ Metric, value.var="Value")  # to get data into wide format, by Month+Day against Metric with Value as values
head(aircast, 10)
aircast <- aircast[,c("Ozone", "Solar.R", "Wind", "Temp", "Month", "Day")]  # rearrange columns to original sequence
# end of Lesson 7
# Lesson 8 Manipulate Strings
# Chap 8.1 Combine strings together
paste("a", "b", "Hello", sep="/")   # stitch variables together with an optional separator
paste(c("Hello", "Hey", "Howdy"), c("Jared", "Bob", "David")) # add's vectors by element / position
paste("Hello", c("Jared", "Bob", "David"))  # adds first var to each element of the array
vectorOfText <- c("Hello", "Everyone", "out there", ".")
paste(vectorOfText, collapse=" ")
person <- "Jared"; waitTime<- 25; partySize <- 8;
paste("Hello ", person,  ", your party of ", partySize , " will be seated in " , waitTime , " minutes.", sep="")  # can achive the result, but is prone errors due to complexity
sprintf("Hello %s, your party of %d, will be seated in %d miinutes.", person, partySize, waitTime);               # produces same result with less complexity

# Chap 8.2 Extract text
require(XML)
theURL <- "http://www.loc.gov/rr/print/list/057_chron.html"
presidents <- readHTMLTable(theURL, which=3, as.data.frame=TRUE, skip.rows=1, header=TRUE, stringsAsFactors=FALSE )
tail(presidents)
tail(presidents$YEAR)
presidents <- presidents[1:64,]
require(stringr)    # hadley wickam's string manipulation package
yearList <- str_split(presidents$YEAR, pattern = "-")     # stringr function to split a string using a pattern
head(yearList)
class(yearList); class(yearMatrix)
yearMatrix <- data.frame(Reduce(rbind, yearList)) # reduce uses a binary function to successively combine elements of a vector
head(yearMatrix)
names(yearMatrix) <- c("Start", "Stop")
presidents <- cbind(presidents, yearMatrix) # bind the generated yearMatrix as added columns to president data set
str_sub(presidents$PRESIDENT, start=1, end=3) 
str_sub(presidents$PRESIDENT, start=4, end=8) # extract string 4 through 8 of President$PRESIDENT
presidents[str_sub(presidents$Start, start=4, end=4) == 1, c("YEAR", "PRESIDENT", "Start", "Stop")] # get a subset of presidents whose 4th digit of Start year = 1
str_detect(presidents$PRESIDENT, ignore.case("John"))    # stringr function to detect a specific string, returns a TRUE / FALSE array
presidents[str_detect(presidents$PRESIDENT, ignore.case("John")), ] # str_detect used to select matching records

con <- url("http://www.jaredlander.com/data/warTimes.rdata")  # reference a url
load(con) # load the data 
close(con)
head(warTimes, 10)
warTimes[str_detect(warTimes, pattern = "-") ]
theTimes <- str_split(string=warTimes, pattern="(ACAEA)|-", n=2)    # split the data
head(theTimes)

theStart <- sapply(theTimes, FUN=function(x) x[1])      # impromptu function that returns first element of the vector
head(theStart)
theStart <- str_trim(theStart)                         # trim leading and trailing spaces of a string

str_extract(string=theStart, pattern = "January")       # search for the pattern in given string
theStart[str_detect(string = theStart, pattern = "January")]  # extract data that hast the matching patern

head(str_extract(string = theStart, pattern = "[0-9][0-9][0-9][0-9]"), 20)  # reg ex to extract any data that has 4 integers in sequence
head(str_extract(string = theStart, pattern = "[0-9]{4}"), 20)  # works as previus expression but repeats pattern 4 times
head(str_extract(string = theStart, pattern = "\\d{4}"), 20)    # works similar i.e. looks for 4 digits in a row
head(str_extract(string = theStart, pattern = "\\d{1,3}"), 20)    # find any 1, 2 or 3 digits  in a row
head(str_extract(theStart, "^\\d{4}"), 20)    # look for 4 digit year at begining of the line 
head(str_extract(theStart, "^\\d{4}$"), 20)   # look for 4 digit year at begining of the line with nothing else on the line.

head(str_replace(theStart, pattern = "\\d", replacement = "x"))     # replace first instance of matching pattern
head(str_replace_all(theStart, pattern = "\\d", replacement = "x")) # replace all matching instances
head(str_replace_all(theStart, pattern = "\\d{1,4}", replacement = "x")) # replace an digit 1 through 4 times matching instances with replacement

commands <- c("<a href=index.html>The link is here.</a>", 
              "<b>This is a bold text</b>")
commands
str_replace(commands, pattern="<.+?>(.+?)<.+>", replacement="\\1")
# end of Lesson 8

### Lesson 9 Basic Statistics ###
# 9.1: Draw numbers from probability distributions
rnorm(10)                   # draw 10 random numbers by default the draw from standard normal the is mean of 0 and sd of 1
rnorm(10, mean=100, sd=20)  # draws 10 number with mean of 100 ad sd of 20
randNorm10 <- rnorm(10)   
dnorm(randNorm10)           # find probability of the numbers occuring provided in the input
dnorm(c(-1, 0, 1))

require(ggplot2)
randNorm <- rnorm(30000)    # randomg 30000 numbers with mean of 0 and sd of 1
randDensity <- dnorm(randNorm)  # their probability
ggplot(data.frame(x=randNorm, y=randDensity)) + aes(x=x, y=y) + geom_point() + labs(x="Random Variables", y="Density")

randNorm10
pnorm(randNorm10)   # probability distribution of a random number is the cumulative probability of that number or smaller
pnorm(c(-3,0,3))    # > 0.001349898 0.500000000 0.998650102   # i.e. probability of values less than -3 is very low, 0 is 50% and 3 is 99.86%
pnorm(1) - pnorm(0) # to find probability between numbers 0 and 1

# qnorm()             # given a certain probability it tells you what a certain # was
randNorm10
pnorm(randNorm10)         # probability distribution of each value in randNorm10
qnorm(pnorm(randNorm10))  # quantile norm on probability gives the original vector randNorm10
identical(randNorm10, qnorm(pnorm(randNorm10))) # will return false as there are some positional & rounding errors

# binomial distribution : series of childs where the answer can be yes / no, live / die, true / false, some binary result
rbinom(n=1, size=10, prob = 0.4)    # one draw out of 10 tests / trials with a probability of .4, 
rbinom(n=5, size=10, prob = 0.4)    
rbinom(n=10, size=10, prob = 0.4)    
rbinom(n=1, size=1, prob = 0.4)    # becomes a bernoli random variable

binomdata <- data.frame(Success=rbinom(n=10000, size=10, prob = 0.3))
ggplot(binomdata, aes(x=Success)) + geom_histogram(binwidth=1)
binom5 <- data.frame(Success=rbinom(n=10000, size=5, prob = 0.3), size=5)
binom10 <- data.frame(Success=rbinom(n=10000, size=10, prob = 0.3), size=10)
binom100 <- data.frame(Success=rbinom(n=10000, size=100, prob = 0.3), size=100)
binom1000 <- data.frame(Success=rbinom(n=10000, size=1000, prob = 0.3), size=1000)
binomAll <- rbind(binom5, binom10, binom100, binom1000)
head(binomAll, 10)
tail(binomAll, 10)
ggplot(binomAll, aes(x=Success)) + geom_histogram() + facet_wrap(~size, scale="free")

#dbinom : distribution of a bionomial random variable
dbinom(x=3, size = 10, prob = 0.3)  # what are the chances of 3 success out of 10 with a probability of 0.3
pbinom(q = 3, size = 10, prob = 0.3) # probability of 3 or less successes out of 10
qbinom(p=0.3, size = 10, prob = 0.3) # quantile given a probability the chances of successes
qbinom(p = c(0.3, 0.05, 0.35, 0.5, 0.6),size = 10 ,prob = 0.3) # returns the quantiles of the vectors probability

# poisson   # of home runs / # of successes 
pois1 <- rpois(n = 10000, lambda = 1)
pois2 <- rpois(n = 10000, lambda = 2)
pois5 <- rpois(n = 10000, lambda = 5)
pois10 <- rpois(n = 10000, lambda = 10)
pois20 <- rpois(n = 10000, lambda = 20)
pois <- data.frame(Lambda.1=pois1, Lambda.2=pois2, Lambda.5=pois5, Lambda.10=pois10, Lambda.20=pois20)
head(pois); tail(pois)
require(reshape2)
pois <- melt(data=pois, variable.name="Lambda", value="x")  # will convert Lambda.1, Lamdba.2 ... with their values and convert into 2 column vector with column name Lambda & value
require(stringr)
pois$Lambda <- str_extract(string=pois$Lambda, pattern = "\\d+") # search pattern is to pull any digit and after
class(pois$Lambda)  # ensure pois$Lambda is a character
pois$Lambda <- as.factor(as.numeric(pois$Lambda))
ggplot(pois, aes(x=x)) + 
  geom_density(aes(group=Lambda, color=Lambda, fill=Lambda), adjust=4, alpha=1/2) + 
  scale_color_discrete() + scale_fill_discrete() + ggtitle("Probability Mass Function")
  # alpha of 1/2 means for everyposition of plot, it takes 2 dots to make it solid

dpois
qpois
### 9.2: Calculate averages, standard deviations and correlations ####
x <- sample(x=1:100, size=100, replace=TRUE)
mean(x)
y <- x
y[sample(x=1:100, size=20, replace=FALSE)] <- NA
y
mean(y, na.rm = TRUE)
grades <- c(95,72,87, 66)
weights <- c(1/2, 1/4, 1/8, 1/8)
mean(grades)
weighted.mean(x=grades, w=weights)  # weighted mean grades

# Variance of a vector: is the sum of each observation minus the average value squared over # of observation -1
var(x);     # variation is a measure of how spread out the data is. roughly how far each number is from average
sum((x - mean(x))^2)/(length(x)-1)  # variance computed

#SD   : a more common understandable version of variance is sqrt(var(x))
sqrt(var(x))
sd(x)
sd(y); sd(y, na.rm = TRUE)
sum(x); min(x); max(x); mean(x); quantile(x, c(0.25, 0.75)); median(x)     # 
summary(x)

require(ggplot2)
head(economics)
# corelation formula :  sum of (x - mean(x)) (y-mean(y))  over (n-1)*sd(x)*sd(y)
cor(economics$pce, economics$psavert)   # to get co-relation between 2 variables
cor(economics[, c(2,4:6)] )             # corelates a matrix subset
econCor <- cor(economics[, c(2,4:6)] )
require(reshape2)
econMelt <- melt(econCor, varnames=c("x", "y"), value.name="Correlation")   # produces an output of relation between variables
econMelt <- econMelt[order(econMelt$Correlation), ]   # change order of the data
head(econMelt)

# produces a heat map of variables and their co-relation, with a guide bar on the right
ggplot(econMelt, aes(x=x, y=y)) + geom_tile(aes(fill=Correlation)) + 
  scale_fill_gradient2(low = "red", mid="white", high="steel blue", guide=guide_colorbar(ticks = FALSE, barheight = 10), limits=c(-1, 1)) +
  theme_minimal() + labs(x=NULL, y=NULL)

    # test vectors
m <- c(9, 9, NA, 3, NA, 5, 8, 1, 10, 4)
n <- c(2, NA, 1, 6, 6, 4, 1, 1, 6, 7)
p <- c(8, 4, 3, 9, 10, NA, 3, NA, 9, 9)
q <- c(10, 10, 7, 8, 4, 2, 8, 5, 5, 2)
r <- c(1, 9, 7, 6, 5, 6, 2, 7, 9, 10)
theMat <- cbind(m, n, p, q, r)
cor(theMat, use="complete.obs") # use data where there are complete observations

### 9.3: Compare samples with t-tests and analysis of variance ####
data(tips, package="reshape2")
head(tips)
unique(tips$day)
unique(tips$sex)

# t.test  : T statistic is the difference between average and the average error of the average. P value tells whether or not you should reject the test. 
# In the above example the P value is very low indicating the null hypothesis of tip being 2.5 is very very low. 
# Test also provides a confidence interval
t.test(tips$tip, alternative = "two.sided", mu = 2.5)  # tests if the value is two sided equal to 2.5, 2.5 is the null hypothesis 

require(ggplot2)
randT <- rt(30000, df=NROW(tips)-1); # random variable of 30K size 
tipTTest <- t.test(tips$tip, alternative = "two.sided", mu = 2.5)
tipTTest
ggplot(data.frame(x=randT)) + geom_density(aes(x=x), fill="grey", color="grey") + 
  geom_vline(xintercept=tipTTest$statistic) + geom_vline(xintercept=mean(randT) + c(-2,2)*sd(randT), linetype=2)
t.test(tips$tip, alternative = "greater", mu=2.5)

aggregate(tip~ sex, data=tips, mean)  # compare tips of female workers to make workers
aggregate(tip~ sex, data=tips, var)   # variance of tips between male and female
aggregate(tip~ sex, data=tips, sd)

shapiro.test(tips$tip) # to test if value is normally distributed - hint high p value means normally distributed
shapiro.test(tips$tip[tips$sex=="Male"]) # to test if value is normally distributed for "Males"
shapiro.test(tips$tip[tips$sex=="Female"]) # to test if value is normally distributed for "Females"
ggplot(tips, aes(x=tip, fill=sex)) + geom_histogram(binwidth=.5, alpha=1/2) # build a plot to check the distribution of tips by sex

ansari.test(tip ~ sex, tips) # to check if value of tip is equally distributed between sex, high p value indicates it is
t.test(tip~sex, data=tips, var=TRUE) # to check true difference in tip between sex's
require(plyr)

ggplot(data.frame(x=randNorm,y=randDensity)) + aes(x=x, y=y) + geom_point() + labs(x="Random Variables", y="Density")
binomData <- data.frame(Sucess=rbinom(n=1000, size = 10,prob = 0.4))
ggplot(binomData, aes(x=Success)) + geom_histogram(binwidth=1)
aggregate(tip~ sex, data=tips, mean)
aggregate(tip~ sex, data=tips, var)
ansari.test(tip ~ sex, tips)
ansari.test(tip ~ sex, tips)
t.test(tip~sex, data=tips, var=TRUE)
# to create a custom summary object with tips, tip.mean, tip.sd, Lower, Upper, supply ddply the function "summarize"
  # "tip.mean - 2*tip.sd/sqrt(NROW(tip))" - provides 2 standard error in lower direction
tipSummary <- ddply(tips, "sex", summarize, tip.mean=mean(tip), tip.sd=sd(tip),
                    Lower=tip.mean - 2*tip.sd/sqrt(NROW(tip)),
                    Upper=tip.mean + 2*tip.sd/sqrt(NROW(tip)))

# plot to show lower, mean, upper for each sex 
summary(tipSummary)
ggplot(tipSummary, aes(x=tip.mean, y=sex)) + geom_point() + geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.2)

require(UsingR)
head(father.son)
# check variance between 2 variables, if low p value, then there is not much diference
t.test(father.son$fheight , father.son$sheight, paired=TRUE) 
# when you require a comparison of more than 2 variables use Anova
tipAnova <- aov(tip ~ day -1, tips)   # do tips vary by day? "-1" is required to not have a generic intercept, but a specific day
tipIntercept <- aov(tip ~ day, tips)  # produces a result with generic intercept 
tipAnova$coefficients; summary(tipAnova)
tipIntercept$coefficients
# create a summary data table that contains, day, mean, sd, # of days, 90th percentile, Lower and upper
tipsByDay <- ddply(tips, "day", summarise, tip.mean=mean(tip), tip.sd= sd(tip), Length=NROW(tip), 
                    tfrac=qt(p=.90, df=Length-1),  # get 90th percentile to get exact error rate to use
                    Lower = tip.mean - tfrac*tip.sd/sqrt(Length),
                    Upper = tip.mean + tfrac*tip.sd/sqrt(Length))
# plot tip by day, with Lower, mean & Upper error bars  
ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() + geom_errorbarh(aes(xmin=Lower, xmax=Upper, height=.3))

### Chap 10.1 - Fit Simple Linear models ####
require(UsingR)
head(father.son)

# build ggplot, using linear model "lm" as param for geom_smooth. For the best fit line
ggplot(father.son, aes(x=fheight, y=sheight)) + geom_point() + geom_smooth(method="lm") + labs(x="Father", y="Son")
heightsLM <- lm(sheight ~ fheight, data=father.son)
heightsLM; 
summary(heightsLM)

data(tips, package="reshape2")
tipAnova <- aov(tip ~ day -1, tips)   # get Tips by Day using Anova model
tipLM <- lm(tip ~ day -1, tips)       # get Tips by Day using Linear model, this provides same high level info as Anova but more details
summary(tipAnova)
summary(tipLM)
### Chap 10.2 - Explore Data ####

housing <- read.table("data/housing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE )
head(housing)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt", "Income", "IncomePerSqFt", "Expense", "ExpensePerSqFt", "NetIncome", "Value", "ValuePerSqFt", "Boro")
  
  # Explore ValuePerSqFt
require(ggplot2)
ggplot(housing, aes(x=ValuePerSqFt)) + geom_histogram(binwidth=10) +labs(x="Values Per Square Foot")
# breakup by Boro, use fill
ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) + geom_histogram(binwidth=10) +labs(x="Values Per Square Foot") 
# Breakup by Boro as faceted / tiled graphs
ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) + geom_histogram(binwidth=10) +labs(x="Values Per Square Foot") + facet_wrap(~Boro) 
ggplot(housing, aes(x=SqFt)) + geom_histogram() + labs(x="Square Foot")   # does not yield an informative visual
ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point() # try a scatterplot to see if this gives a better visual
ggplot(housing[housing$Units < 1000, ], aes(x=SqFt)) + geom_histogram() +labs(x="Square Foot") # Use data as qualified for units less than 1000
ggplot(housing[housing$Units < 1000, ], aes(x=SqFt, y=ValuePerSqFt)) + geom_point() # to plot a scatterplot
sum(housing$Units > 1000) # to get count of data that meets the criteria
housing <- housing[housing$Units < 1000, ] # Remove data for units greater than 1000

### Chap 10.3 - Fit Multiple Regression Models  ####
  #  Y = XB(beta) + E(Epsilon), the goal is the find the value of beta
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing)  # linear model of ValuePerSqFt against Units + SqFt + Boro
# notice 1 of the boro got dropped due to multi colinearity
summary(house1)
head(model.matrix(ValuePerSqFt ~ Boro, housing))
house1$coefficients # or use $coef(house1) or $coefficients(house1) to get the co-efficients of house1
require(coefplot)
update.packages("coefplot")
coefplot(house1)  # produces SD, 1 error away from Sd as solid bar, and 2 errors away from Sd as thin bar
house2 <- lm(ValuePerSqFt ~ Units*SqFt + Boro, housing) # provides interaction of ValuePerSqFt wr.t. Units*SqFt
coefplot(house2); summary(house2);
head(model.matrix(ValuePerSqFt ~ Units*SqFt, housing)) # gives interaction of variables and the variables i.e. Units, SqFt & Units:SqFt
head(model.matrix(ValuePerSqFt ~ Units:SqFt, housing)) # Gives interaction only without including the variables 
house3 <- lm(ValuePerSqFt ~ Units:SqFt + Boro, housing) # provides interaction of ValuePerSqFt wr.t. Units:SqFt
coefplot(house3)
house4 <- lm(ValuePerSqFt ~ SqFt*Units*Income, housing) # provides interaction of ValuePerSqFt wr.t. SqFt, Units, Income
coefplot(house4)
house4$coefficients
house5 <- lm(ValuePerSqFt ~ Class*Boro, housing) # provides interaction of ValuePerSqFt wr.t. Class*Boro, i.e. categorical against categorical data
house5$coefficients
house6 <- lm(ValuePerSqFt ~ I(SqFt/Units) + Boro, housing)  # I : Identity function, creates the param as new variable
house6$coefficients
house7 <- lm(ValuePerSqFt ~ (Units+SqFt)^2, housing)
house7$coefficients
house8 <- lm(ValuePerSqFt ~ Units*SqFt, housing)  #  house7 and house8 produce same results
house8$coefficients
house9 <- lm(ValuePerSqFt ~ I(Units+SqFt)^2, housing)
house9$coefficients
multiplot(house1, house2, house3)   # plots multiple model's co-ef plot
housingNew <- read.table("data/housingNew.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
head(housingNew)
# Predict function, to get standard errors give "se.fit=TRUE", and an interval of prediction / confidence, with a 95% confidence
housePredict <- predict(house1, newdata=housingNew, se.fit=TRUE, interval="prediction", level=.95)
head(housePredict$fit)

# Tune the models to get a nice prediction, size of coefficient tells the effect of the variable on the response
# highly +ve means a positive effect and highly -ve means a negative effect on the response

### Chap 10. 4 - Fit Logistic regression - Good fit when data is boolean outcome ####
  # GLM - fits a regression & adjusts the weights, fits the regression again & adjusts the weights, until it converges and does not need to adjust weights
acs <- read.table("data/acs_ny.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
head(acs)

acs$Income <- with(acs, FamilyIncome >= 150000)
require(useful); require(ggplot2)
ggplot(acs, aes(x=FamilyIncome)) + geom_density(fill="grey", color="grey") + 
  geom_vline(x=150000) + scale_x_continuous(label=multiple.dollar, limits=c(0,1000000))
# fiting a Logistic regression ,with generalized linear model. Family of type binomial (o.e. yes / no data) LINK Logit translates linear data to yes/no
# multi variable linear model Income against HouseCosts+NumWorkers+OwnRent+NumBedrooms+FamilyType

income1 <- glm(Income ~ HouseCosts+NumWorkers+OwnRent+NumBedrooms+FamilyType, acs, family=binomial(link="logit")) # general linear model
summary(income1)
# measure of error for GLM is deviance as compared to MSE for LM
coefplot(income1)
income1$coefficients
invlogit <- function(x) { 1 /( 1 + exp(-x))}
invlogit(income1$coefficients)  # converts co-efficients to original format

### Chap 10.5 - Poisson regression - Good fit when the data is counted ####
  # num of child, num of accidenets, 
head(acs);  require(ggplot2); require(coefplot)
ggplot(acs, aes(x=NumChildren)) + geom_histogram(binwidth=1)  # 
# family type of Poisson as this is a quantitative , log is to transform back to original scale
children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, data=acs, family = poisson(link="log"))
summary(children1)
coefplot(children1)

z <- (acs$NumChildren - children1$fitted.values) / sqrt(children1$fitted.values)  # dispersion formula
sum(z^2) / children1$df.residual        # overdispersion factor, any thing over 1 is overdispersion
pchisq(sum(z^2), df=children1$df.residual)  # prob function for chi distribution, p value of 1 confirms overdispersion
# family quasipoisson uses a -ve bionomial distribution
children2 <- glm(NumChildren ~ FamilyIncome+FamilyType + OwnRent, data=acs, family=quasipoisson(link="log"))
summary(children2)
multiplot(children1, children2)

###  Chap 10.6 - Analyze Survival Data  ####
require(survival)
head(bladder)
?bladder        # to get help documentation information on the library & data
survObject <- with(bladder[100:105,], Surv(stop, event))    # Surv creates a survival object with columns provided
survObject
# coxph : fits a proportional hazards regression model
cox1 <- coxph(Surv(stop, event)~ rx + number + size + enum, data=bladder) 
summary(cox1)
coefplot(cox1)
plot(survfit(cox1) ,xlab="Days", ylab="Survival Rate", conf.int=TRUE) # survial fit 
cox2<- coxph(Surv(stop, event) ~ strata(rx) + number + size + enum, data=bladder)
summary(cox2)
plot(survfit(cox2) ,xlab="Days", ylab="Survival Rate", conf.int=TRUE, col=1:2) # survial fit 
legend("bottomleft", legend=c(1,2), lty=1, col=1:2, text.col=1:2, title="rx")

cox.zph(cox1); cox.zph(cox2)
ag1 <- coxph(Surv(start, stop, event) ~ rx + number + size + enum + cluster(id), data=bladder2)
ag2 <- coxph(Surv(start, stop, event) ~ strata(rx) + number + size + enum + cluster(id), data=bladder2)
plot(survfit(ag1), conf.int=TRUE, col=1:2) 
plot(survfit(ag2), conf.int=TRUE, col=1:2)
legend("topright", legend=c(1,2), lty=1, col=1:2, text.col=1:2, title="rx")


###  Chap 10.7 - Assess model quality with residuals  ####
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1); coef(house1); coefplot(house1)
head(fortify(house1))   # adds hat, cooksStd Dev, fitted values, residuals, studentised residuals, 

h1 <- ggplot(aes(x=.fitted, y=.resid), data=house1) +  # will automatically fortify the data and then ploot the columns
  geom_point() + geom_hline(yintercept=0) + geom_smooth(se=FALSE) + labs(x="Fitted", y="Residuals")
h1      # paints a scatter plot that may not give clarity
h1 + geom_point(aes(color=Boro))    # coloring by Boro gives clarity on why we see clusters
# plots a qqplot that shows Quantiles of standardized residualsvs Theoretical Quantiles
plot(house1, which=2)     # as there are values that are not in line with linear line, at start and end there may be some missing assumptions
ggplot(house1, aes(sample=.stdresid)) + stat_qq() + geom_abline()  # produces a similar graph as qqplot
ggplot(house1, aes(x=.stdresid)) + geom_histogram()


### 10.8 - Compare models ####
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data = housing)
house3 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + Class, data = housing)
house4 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt * Class, data = housing)
house5 <- lm(ValuePerSqFt ~ Boro + Class, data = housing)

multiplot(house1, house2, house3, house4, house5, pointSize = 2)
anova(house1, house2, house3, house4, house5)   # ideally you want the lowest residual sum of squares 
AIC(house1, house2, house3, house4, house5)     # takes into account lowest RSS but also factors a penalty for higher complexity when comparing models
BIC(house1, house2, house3, house4, house5)     # takes into account lowest RSS but also factors a penalty for higher complexity when comparing models
# all three, anova, AIC & BIC show a lower RSS for house4, thereby agreeing that model4 is the better of the lot

housing$HighValue <- housing$ValuePerSqFt >= 150        # new boolean derived value if priced above $150
high1 <- glm(HighValue ~ Units + SqFt + Boro, data = housing, family=binomial(link="logit"))
high2 <- glm(HighValue ~ Units * SqFt + Boro, data = housing, family=binomial(link="logit"))
high3 <- glm(HighValue ~ Units + SqFt * Boro + Class, data = housing, family=binomial(link="logit"))
high4 <- glm(HighValue ~ Units + SqFt * Boro + SqFt * Class, data = housing, family=binomial(link="logit"))
high5 <- glm(HighValue ~ Boro + Class, data = housing, family=binomial(link="logit"))
# anova & AIC agree that high4 is better, but BIC returns lowest deviance value for high5
anova(high1, high2, high3, high4, high5)    # reports back the deviance and the lowest deviance is better
AIC(high1, high2, high3, high4, high5)
BIC(high1, high2, high3, high4, high5)

### 10.9 - Judge accuracy using cross validation ####
  # K fold cross validations, i.e. fit model on k-1 fold, and predict the fold that was excluded, and keep track of errors using cv
require(boot)
houseG1 <- glm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing, family = gaussian(link="identity"))  # by default GLM fits gaussian model, but included here for completeness
summary(houseG1)
houseCV1 <- cv.glm(data = housing, glmfit = houseG1, K = 5) # 5 fold validation, per research this is an optimal # where you get good results without going into computation overload
houseCV1$delta      # returns cross validated error or mean squared error (average of residuals is (1/n)(sum(y^-y)2)
# build the different models
houseG2 <- glm(ValuePerSqFt ~ Units * SqFt + Boro, data = housing, family = gaussian(link="identity"))
houseG3 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + Class, data = housing, family = gaussian(link="identity"))
houseG4 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt * Class, data = housing, family = gaussian(link="identity"))
houseG5 <- glm(ValuePerSqFt ~ Boro + Class, data = housing, family = gaussian(link="identity"))
# apply cross validations for each model
houseCV2 <- cv.glm(data = housing, glmfit = houseG2, K = 5)
houseCV3 <- cv.glm(data = housing, glmfit = houseG3, K = 5)
houseCV4 <- cv.glm(data = housing, glmfit = houseG4, K = 5)
houseCV5 <- cv.glm(data = housing, glmfit = houseG5, K = 5)
# get delta values
houseCV1$delta; houseCV2$delta; houseCV3$delta; houseCV4$delta; houseCV5$delta
multiplot(houseCV1, houseCV2, houseCV3, houseCV4, houseCV5)

cvResults <- as.data.frame(rbind(houseCV1$delta, houseCV2$delta, houseCV3$delta, houseCV4$delta, houseCV5$delta))
cvResults
names(cvResults) <- c("Error", "Adjusted Error")
cvResults$Model <- sprintf("houseG%d", 1:5)   # Add model name
# compare models and reports back their deviance : hint lower deviance is better
cvAnova <- anova(houseG1, houseG2, houseG3, houseG4, houseG5)    
cvAIC <- AIC(houseG1, houseG2, houseG3, houseG4, houseG5)

cvResults$Anova <- cvAnova$`Resid. Dev` # use backtick when vector name has space
cvResults$AIC <- cvAIC$AIC
# the combined cross validation model assessments confirm model4 is the better of the lot.

### 10.10 - Estimate uncertainty with the bootstrap ####
require(plyr)
head(baseball)
baseball <- baseball[baseball$year >= 1990,]    # subset data after 1990 as this is good data

bat.avg <- function(data, indices=1:NROW(data), hits="h", at.bats="ab")
{
  sum(data[indices, hits], na.rm=TRUE) / sum(data[indices, at.bats], na.rm=TRUE)
}
bat.avg(baseball)

require(boot)
# it has been observed that 1200 boot strap samples give, stype = i passess indices
# boot strap samples random 1200 samples, which may result in some data being drawn multiple times, and some never
avgBoot <- boot(data = baseball, statistic= bat.avg, R = 1200, stype = "i")
avgBoot     # provides original average, bias and standard error (measure of uncertainity)

# bootci : confidence interval calculation of a 95% confidence interval and normal confidence interval quantiles
boot.ci(boot.out = avgBoot, conf=.95, type = "norm")    
quantile(avgBoot$t, c(0.25, 0.5, 0.8, 0.95))  # extract quantiles of averages from bootstrap

require(ggplot2)
# produces a histogram of computed averages in the sample, with each vertical lines representing confidence interval 
ggplot() + geom_histogram(aes(x=avgBoot$t), fill = "grey", color="grey")   +  # avgBoot$t is the 1200 replicates
  geom_vline(xintercept=avgBoot$t0 + c(-1,1)*2*sqrt(var(avgBoot$t)), linetype=2)

###   10.11 - Choose variables using stepwise selection  #### 
  # step wise selection, is one brute force way to choose variables for the model
head(housing)

nullModel <- lm(ValuePerSqFt ~ 1, data = housing)   # Base model comparing to intercept
fullModel <- lm(ValuePerSqFt ~ Units + SqFt * Boro + Boro * Class, data = housing)  # a complex model

houseStep <- step(nullModel, 
                  scope = list(lower=nullModel, upper=fullModel),  # provide lower and upper limit for model comparisons
                  direction = "both")  # give traverse direction, in this case it will travel both ways

houseStep   # results in the best algorithm calculated through brute force, which in this case is
#  lm(formula = ValuePerSqFt ~ Boro + Class + SqFt + Units + Boro:SqFt + Boro:Class, data = housing)

### Chap 11 - Other Models ########
  # beyond regression there are many other models that can be fit to data, models covered include 
      # 1) regularization with the elastic net
      # 2) baigean shrinkage
      # 3) non linear models such as "non-linear least squares"
      # 4) splines
      # 5) GAMS : generalized additive models
      # 6) random forests and decision trees


### Chap 11.1 - Select variables and improve predictions with the elastic net ####
# Elastic net deals with high dimension data
#ridge regression penalty(L2) takes a number of variables that are highly corelated and divides the coefficients amongst them
  # is really good at smoothing out prediction and make sure you have a well fitted predictor matrix
#lasso regression penalty(L1) takes a number of variables that are highly corelated and gives the coefficient and get's rid of all but one of them and gives the entier coefficient to one
  # is really good to use for variable selection

install.packages("glmnet")
require(glmnet)     # needs to be fed a predictor matrix and a response matrix as it is optimized for best speed, can handle a large set of variables upto 50K
# cannot process categorical data, you need to have dummy variable
require(useful)
head(acs)

acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits + NumVehicles + NumWorkers +
                OwnRent + YearBuilt + ElectricBill + FoodStamp + HeatingFuel + Insurance + Language-1, 
                data=acs, contrasts=FALSE)
# above command created dummy variables to flatten out categorical data and added columns to go from 19 to 44
dim(acs); class(acsX) ; dim(acsX); topleft(acsX, c = 6); topright(acsX, c = 6)

acsY <- build.y(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits + NumVehicles + NumWorkers +
                  OwnRent + YearBuilt + ElectricBill + FoodStamp + HeatingFuel + Insurance + Language-1, data=acs )
class(acsY) ; dim(acsY); head(acsY); tail(acsY)
  # an issue with elastic net is you have to choose the lambda parameter, the amount of penalty you apply
  # so if you have more penalty you shrink everything down, and if you have less penalty you don't shrink as much 
  # the best method to choose this param is through cross validation, in this case it is built into glmnet
set.seed(1863561)  # set a random seed
acsCV1 <- cv.glmnet(x=acsX, y=acsY, family="binomial", nfold = 5)   # logistic regression, with 5 folds cross validation and returns two optimal lambda
acsCV1$lambda;        # vector of all lambda's
acsCV1$lambda.min;    # lambda that minimizes mean squared error    
acsCV1$lambda.1se     # largest lambda that is within 1 squared error of the minimum
plot(acsCV1)          # plots binomial deviance ~ log(Lambda), and at top the # of variables used. 
# Pick a lambda that has highest deviance but is still within 1 sd but with lower # of variables

coef(acsCV1, s="lambda.1se")
plot(acsCV1$glmnet.fit, xvar="lambda")    
# plots a visual of variables getting included as lambda gets reduced, and how the variables get to 0 and become irrelevant
# plot looks more like a branch
abline(v=log(c(acsCV1$lambda.min, acsCV1$lambda.1se)), lty=2)  # adds vertical lines for lambda.in and lambda.1se

# glmnet parameter : alpha = 1 produces a lasso regression, alpha=0 produces a ridge regression, somewhere in between is a combination of two
 # elastic net with a ridge regression
acsCV0 <- cv.glmnet(x=acsX, y=acsY, family="binomial", nfold = 5, alpha=0)   
acsCV0$lambda.min; acsCV0$lambda.1se

plot(acsCV0$glmnet.fit, xvar="lambda") ; 
# plot looks more line a funnel, as variables gradually hover around 0, as ridge variables never get singled out but squoosh down
abline(v=log(c(acsCV0$lambda.min, acsCV0$lambda.1se)), lty=2)

### Chap 11.2 - Decrease uncertainty with weakly informative priors ####
load("data/ideo.rdata")
head(ideo)
theYears <- unique(ideo$Year)
results <- vector(mode = "list", length = length(theYears))
names(results) <- theYears
results

for (i in theYears)
{
  results[[as.character(i)]] <- glm(Vote ~ Race + Income + Gender + Education, 
                                    data=ideo, subset=Year==i, family=binomial(link="logit"))
}

require(coefplot)
voteInfo <- multiplot(results, coefficients="Raceblack", plot=FALSE)
head(voteInfo)
multiplot(results, coefficients="Raceblack", secret.weapon = TRUE, outerCI = 2, innerCI = 1)

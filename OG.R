setwd("~/workspace/Data Science/Otto Group/data")
### Requires #####
require("e1071");     # used for Naive Bayes 
require("ROCR");      # used for perforance function
require("caret");     # used for confusion matrix & train functions
require("data.table")
require("dplyr")      # used for groupby function

## Read data ####
prod <- read.table("train.csv", header = TRUE, sep = ",")
# prod <- read.table("train.csv", header = TRUE, sep = ",", colClasses = c("numeric", rep("character", 93), "factor"))

prod <- read.table("train.csv", header = TRUE, sep = ",", colClasses = c("numeric", rep("factor", 94)))


prod$id <- NULL
prodnotarget <- prod
prodnotarget$target <- NULL

c(names(prodnotarget))
name <- names(prodnotarget)
name[1:92, ]

### check for duplicate records by all features ####

x<- prodnotarget %>% group_by(feat_1, feat_2, feat_3, feat_4, feat_5, feat_6, feat_7, feat_8, feat_9, feat_10, feat_11, feat_12, feat_13, feat_14,
                          feat_15, feat_16, feat_17, feat_18, feat_19, feat_20, feat_21, feat_22, feat_23, feat_24, feat_25, feat_26, feat_27, feat_28,
                          feat_29, feat_30, feat_31, feat_32, feat_33, feat_34, feat_35, feat_36, feat_37, feat_38, feat_39, feat_40, feat_41, feat_42, 
                          feat_43, feat_44, feat_45, feat_46, feat_47, feat_48, feat_49, feat_50, feat_51, feat_52, feat_53, feat_54, feat_55, feat_56, 
                          feat_57, feat_58, feat_59, feat_60, feat_61, feat_62, feat_63, feat_64, feat_65, feat_66, feat_67, feat_68, feat_69, feat_70, 
                          feat_71, feat_72, feat_73, feat_74, feat_75, feat_76, feat_77, feat_78, feat_79, feat_80, feat_81, feat_82, feat_83, feat_84, 
                          feat_85, feat_86, feat_87, feat_88, feat_89, feat_90, feat_91, feat_92, feat_93
                          ) %>% tally
x[x$n>1,  ]  # get data where group by count > 1

paste(as.character(names(prodnotarget)), sep="' '", collapse=", ")  # produce a character array of all columns

# use dplyr package to pipe and group_by all columns (characterized array generated using as.char ...)
x <- prodnotarget %>% group_by(paste(as.character(names(prodnotarget)), sep="' '", collapse=", ")) %>% tally

summary(prod)
head(prod, 1)
unique(prod$feat_1)

NROW(prod)
train <- prod[1:60000, ]
test <- prod[60001:61868,]

validation <- prod
validation$id <- NULL; validation$target <- NULL
summary(validation)

tprior <- table(prod$target)
tprior <- tprior/sum(tprior)
tprior

### Model generated without Laplace correction ####
model <- naiveBayes(target ~ . , data = prod, )
model

validation$pred <- predict(model, validation)
summary(validation$pred)
head(validation, 10)

validationraw <- validation; 
validationraw$pred <- NULL

validationraw$pred <- predict(model, validationraw, type = "raw")
summary(validationraw$pred)
validationraw[1, c("pred")]; validation[1, c("pred")]; prod[1, c("target")]

summary(prod$target)
confMatrix <- table(prod$target, validation$pred)
confusionMatrix(prod$target, validation$pred) # Accuracy : 0.6619 with 95% CI : (0.6581, 0.6656)

rowSums(confMatrix)
colSums(confMatrix)
rowSums(confMatrix) / colSums(confMatrix)

### Model generated with Laplace correction ####
require(caret)
modelLap <- naiveBayes(target ~ . , data = prod, laplace = 0.1)
modelLap

validationLap <- prod
validationLap$id <- NULL; validationLap$target <- NULL
validationLap$pred <- predict(modelLap, validationLap)
table(prod$target, validationLap$pred)
confMatrixKFold <- confusionMatrix(prod$target, validationLap$pred)  # Increases Accuracy to 0.6987 with 95% CI : (0.6951, 0.7023)

### Model with k fold cross validation ####
require(caret)

train_control <- trainControl(method = "cv", number = 5)
modelKFold <- train(prod$target ~ ., data = prod, trControl = train_control, method = "nb")

validationKFold <- prod
validationKFold$id <- NULL; validationKFold$target <- NULL
validationKFold$pred <- predict(modelKFold, validationKFold)
table(prod$target, validationKFold$pred)
confMatrixKFold <- confusionMatrix(prod$target, validationKFold$pred)  # Increases Accuracy 

### try perfcurve for Naive ROC Curve ####

# http://www.mathworks.com/help/toolbox/stats/perfcurve.html
# 
# Here is a small example of how to plot an ROC curve without cross-validation:
#   
#   load fisheriris
# x = meas(51:end,1:2);
# y = species(51:end);
# nb = NaiveBayes.fit(x,y);
# p = posterior(nb,x);
# [X,Y] = perfcurve(y,p(:,1),'versicolor');
# plot(X,Y)
# xlabel('False positive rate'); ylabel('True positive rate')
# title('ROC for classification by naïve Bayes')



### Clustering to compute centroids  ####

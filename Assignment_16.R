1. Use the below given data setData Set
2. Perform the below given activities:
   a. Predict the no of comments in next H hrs
Note:-
1. Use LASSO, Elastic Net and Ridge and other regression techniques that are covered in thmodule
2. Report the training accuracy and test accuracy
3. compare with linear models and report the accuracy
4. create a graph displaying the accuracy of all models


setwd("F:/AcadGild/workings")

lib=c("bigmemory", "readr", "Hmisc", "dplyr", "MASS", "ggplot2", "lattice", "caret", "rpart", 
      "randomForest", "rpart.plot","lattice", "rattle", "data.table","RColorBrewer", "reshape2",
      "InformationValue","stringr", "VIF", "Information", "Amelia", "gdata", "party","car", 
      "lubridate","zoo", "sqldf", "fuzzyjoin", "party", "mice", "mlbench")
sapply(lib, require, character.only=TRUE, quietly=TRUE)

# import train data set
Variant_1<-fread("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_1.csv", header = FALSE)
Variant_2<-fread("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_2.csv", header = FALSE)
Variant_3<-fread("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_3.csv", header = FALSE)
Variant_4<-fread("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_4.csv", header = FALSE)
Variant_5<-fread("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_5.csv", header = FALSE)

fbtrain<-rbind(Variant_1,Variant_2,Variant_3,Variant_4,Variant_5)
dim(fbtrain)


#import test data set
test1<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_1.csv", header = FALSE)
test2<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_2.csv", header = FALSE)
test3<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_3.csv", header = FALSE)
test4<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_4.csv", header = FALSE)
test5<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_5.csv", header = FALSE)
test6<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_6.csv", header = FALSE)
test7<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_7.csv", header = FALSE)
test8<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_8.csv", header = FALSE)
test9<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_9.csv", header = FALSE)
test10<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_10.csv", header = FALSE)

fbtest<-rbind(test1,test2,test3,test4,test5,test6,test7,test8,test9,test10)
dim(fbtest)

# Assign variable names to the train and test data set
colnames(fbtrain) <- c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
                       "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
                       "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
                       "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
                       "basetue","basewed","basethu","basefri","basesat","target")

colnames(fbtest) <- c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
                      "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
                      "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
                      "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
                      "basetue","basewed","basethu","basefri","basesat","target")

# exploratory analysis 
dim(fbtrain); dim(fbtest)
str(fbtrain); str(fbtest)
View(fbtrain); View(fbtest)
summary(fbtrain); summary(fbtest)
describe(fbtrain); describe(fbtest)
Amelia::missmap(fbtrain); Amelia::missmap(fbtest) # no missing values 

train<-(fbtrain); test<-(fbtest)
head(train); head(test)

# removing overlapping observations if any
distinct(train)
dim(train)
distinct(test)
dim(test)

# list the levels for the class
sapply(train, class)


# missing values count
sapply(train, function(x) sum(is.na(x))) # no missing values

# model building
library(lars)
x<-as.matrix(train[,c(30:34, 36:38)])
y<-as.matrix(train[,35])
fittrain<-lars(x,y,type = "lasso")

summary(fittrain)
plot(fittrain)

#result fit
fittrain

# select a step with a minimum error
best_step <- fittrain$df[which.min(fittrain$RSS)]
best_step
summary(best_step)

# predictions
predictions <- predict(fittrain, x, s=best_step, type="fit")$fittrain

# summarize accuracy
mse_train <- mean((y - predictions)^2)
print(mse_train)

# Elastic Net
library(elasticnet)

elasticnet::
ENreg.fit(x_train,y_train)

pred_cv = ENreg.predict(x_cv)

#calculating mse

mse = np.mean((pred_cv - y_cv)*2)

mse 1773750.73

ENreg.score(x_cv,y_cv)

# leaner model
LM<-lm(target~ x+y, data = train)
summary(LM)
attributes(LM)
plot(LM)
coef(LM)

plot(train$target, data=train, main="Scatterplot")




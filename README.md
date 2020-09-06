# R-programming

# import the college_admission.csv


college <- read.csv('College_admission.csv')

head(college,5)


str(college)


as.factor(college$ses)

str(college)

college <- read.csv('C:/R/dataset/College_admission.csv')


str(college)

# we need to convert the ses into a factor

college$ses <-  as.factor(college$ses)

# we need to convert the gender_name and race into a factor


college$Gender_Male <- as.factor(college$Gender_Male)

college$Race <- as.factor(college$Race)

str(college)


summary(college)

a <- c(1,2,3,4,5,NA)

sum(is.na(a))


#get the missing values column wise


sapply(college,function(x) sum(is.na(x)))



# missing value treatment

# general rule

# if there is a var X, if missing value ~50% ,then remove x for analysis


#  if NA is about 30% , we can replace the missing values with the mean or median


# if we have age - median age

# you can take the  x1 = lm(x2,x3,x4) etc we can model for the missing values

# for outliers

# boxplot

boxplot(college$gre)


 # can check the histogram

hist(college$gre)

summary(college$gre)


quantile()

quantile(college$gre,c(0.95,0.01,0.05,0.99))


# normailty

# for gre score

plot(density(college$gre))



qqnorm(college$gre)

View(college)

# for normalizing ,we have the scale function

#scale - x-mean/std dev


gre_norm <- scale(college$gre)


# for checking if a variable has been normalized,

mean(gre_norm)


# standard dev for a normalized


sd(gre_norm)

# run a logistic regression model

# generalized linear model
# 2 categories - 0 /1 for admit , so we binomial family of curves

glm(dependent var ~ x1+x2+x3 ,data=,family='binomial')


selec <- sample.split(college$ses,SplitRatio = 0.7)

college_train <- college[selec==TRUE,]

college_test <- college[!selec,]

admit_model <- glm(admit~.,college,family='binomial')


# h0 that the coefficient is 0

# remove the variables which have a p value >0.05


admit_model <- glm(admit ~gre+gpa+rank,data=college_train,family='binomial')

summary(admit_model)

dim(college)

# how to predict the dependent var in the test data


predict(model_name, newdata=, type='response')


# model_name = the name of the model which was created on the training data
# newdata=apply the model on the test data
# type ='response' , we want the prob of occurence of the dependent var

# predict will give the values on the test dataset.

pred_admit <- predict(admit_model,newdata=college_test,type='response')

View(pred_admit)

View(college_test)

# add the predicted var on the test dataset

college_final <- cbind(college_test,pred_admit)

View(college_final)



# convert the pred_admit to a categorical value of pass or fail.
library('dplyr')

mutate(college_final,pred_class=ifelse(pred_admit>=0.3,1,0))

View(college_final)



table(college_final$admit,college_final$pred_class)

# how to create the confusion matrix

# classification and regression tools 

install.packages('caret')


library('caret')

# positive = 1 means that the person got admit p(admit)=1 

confusionMatrix(table(college_final$admit,college_final$pred_class), positive='1')



# receiver operator characteristics package creates the ROC curve


install.packages('ROCR',dependencies = TRUE)


library(ROCR)

search()


# create a prediction object for graph

#prediction(predict,actual)


pred <- prediction(college_final$pred_class,college_final$admit)

# perform the evaluation of roc curve

# tpr - TRue positive rate on the y axis

perf <- performance(pred,'tpr','fpr')


# fpr - on the x axis

# plot 


plot(perf)


# choose the threshold which will give us the best accuracy

# look at the accuracy , sensitivity and specificity -


# what is our requirement - accuracy, sensitivity, specificity? we have to check


college$admit <- as.factor(college$admit)

ggplot(data=college) +geom_point(mapping=aes(x=gre,y=gpa,color=admit))+geom_smooth(mapping=aes(x=gre,y=gpa))

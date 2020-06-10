# rm(list=ls())
# setwd("~/Dropbox/0.0 Data")
## ----
options(scipen = 999,stringsAsFactors = F)
getwd()
setwd('C:/Karan/Project/Project_5_Banking')
rg_train = read.csv("./bank-full_train.csv", stringsAsFactors = FALSE)

rg_test = read.csv("./bank-full_test.csv", stringsAsFactors = FALSE)

head(rg_train)
setdiff(names(rg_train), names(rg_test))

table(rg_train$y)
table(rg_test$y)

library(dplyr)
glimpse(rg_train)

rg_train = rg_train %>%
  mutate(y = ifelse(y == "yes", 1, 0),
         y = as.numeric(y))


CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var]) ## getting the table for the variable(any categorical variable)
  t=t[t>freq_cutoff] ## cutoff is the frequency of occurance of a variable default is 0 , but ideally it should be atleast 15-20% of actual data,
  ## so here whatever categories which are less than that cut off frequencies are dropped(no dummes are created for them)
  t=sort(t) ## sort the data
  categories=names(t)[-1] ## pick everything but exclude the first as it has lowest frequency: REmember its n-1
  
  for( cat in categories){
    name=paste(var,cat,sep="_") ## Inside the for loop create a name separated by name of variable and category separeted by "_" underscore
    name=gsub(" ","",name) ## replace any spaces if there is found in categoreis of variables
    name=gsub("-","_",name) ## replace any dash if found in categories to underscropes: e.g. 'Cat-1', 'Cat-2' will be 'Cat_1', 'Cat_2'
    name=gsub("\\?","Q",name) ## any question mark is converted to 'Q'
    name=gsub("<","LT_",name) ## Less than sign is converted to LT_
    name=gsub("\\+","",name) ## + sign is removed
    name=gsub("\\/","_",name) ## "/" is replaced with "_"
    name=gsub(">","GT_",name) ## ">" is replaced with 'GT_'
    name=gsub("=","EQ_",name) ## '=' is replaced with 'EQ_'
    name=gsub(",","",name) ##  ',' is replaced with ''
    name=gsub("default","On_LOAD",name) 
    data[,name]=as.numeric(data[,var]==cat) ## changing to numeric type
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(rg_train)


rg_test$y=NA

rg_train$data='train'
rg_test$data='test'
## Appending the train test data for data processing
rg=rbind(rg_train,rg_test)
mean(rg$age)








library(tidyverse)
library(ggplot2)

# 
# view(rg_train$balance)
# 
# IQR(rg_train$balance)
# quantile(rg_train$balance)
# 
# 72-(1.5*1342)
# 1414 + (1.5*1342)
# 
# tr <- rg_train %>% 
#   mutate(less_val=rg_train$balance < -1941 ,
#          great_val=rg_train$balance > 3427)
# 
# tr
# sum(tr$less_val)
# sum(tr$great_val)
# 
# 
# 
# 14+3315
# 
# 
# var(rg_train$balance)
# 
# 
# 
# view(tr)
# 



dim(rg)
dim(rg_train)
dim(rg_test)
## ----

library(tidyverse)



# we'll exclude column named data as it simply represent which dataset the observation is from


## the above code can be made using lapply or sapply , Try it !!!

glimpse(rg)
table(rg$duration)

rg=rg %>% 
  mutate(age_18_28=as.numeric(age>=18 & age <=28),
         age_28_38=as.numeric(age>28 & age <=38),
         age_38_48=as.numeric(age>38 & age <=48),
         age_48_58=as.numeric(age>48 & age <=58),
         age_58_68=as.numeric(age>58 & age <=68),
         age_68_78=as.numeric(age>68 & age <=78),
         age_78_88=as.numeric(age>78 & age <=88),
         age_GT_88=as.numeric(duration>88)) %>% 
  select(-age) 


rg=rg %>% 
  mutate( duration_LT_100=as.numeric(duration < 100),
          duration_100_200=as.numeric(duration>100 & duration <=200),
          duration_200_300=as.numeric(duration>200 & duration <=300),
          duration_300_400=as.numeric(duration>300 & duration <=400),
          duration_400_500=as.numeric(duration>400 & duration <=500),
          duration_500_600=as.numeric(duration>500 & duration <=600),
          duration_600_700=as.numeric(duration>600 & duration <=700),
          duration_700_800=as.numeric(duration>700 & duration <=800),
          duration_800_900=as.numeric(duration>800 & duration <=900),
          duration_GT_900=as.numeric(duration>900)) %>% 
  select(-duration) 



cat_cols = c(
  "job",
  "marital",
  "education",
  "default",
  "housing",
  "loan",
  "contact",
  "poutcome",
  "month","campaign","age_18_28",
  "age_28_38",
  "age_38_48",
  "age_48_58",
  "age_58_68",
  "age_68_78",
  "age_78_88",
  "age_GT_88",
  "duration_LT_100",
  "duration_100_200",
  "duration_200_300",
  "duration_300_400",
  "duration_400_500",
  "duration_500_600",
  "duration_600_700",
  "duration_700_800",
  "duration_800_900",
  "duration_GT_900"
)

## picking all the character columns and creating dummies
for(cat in cat_cols){
  rg=CreateDummies(rg,cat,200)
}



## ------------------------------------------------------------------------


# aggregate( Price ~ Type, data = dt, mean)


glimpse(rg)


## again splitting for the modeling
rg_train=rg %>% filter(data=='train') %>% select(-data)
rg_test=rg %>% filter(data=='test') %>% select (-data,-y)
## ------------------------------------------------------------------------


## splitting the data 80%-20%
set.seed(2)
s=sample(1:nrow(rg_train),0.8*nrow(rg_train))
rg_train1=rg_train[s,]
rg_train2=rg_train[-s,]
glimpse(rg_train1)
dim(rg_train)
dim(rg_train1)
dim(rg_train2)

## ----
library(car)

## creating the formula for whole model
## taking all the value as independent variable except Revenue.Grid and REF_NO
form <-
  as.formula(paste0('y ~ ', paste0(setdiff(
    names(rg_train1), c('y', 'ID')
  ), collapse = ' + ')))


## For VIF calculation; we create a basic linear model
for_vif=lm(form,data=rg_train1)
## find the first value of vif, if you found it to be greater than 4 then use the below code
sort(vif(for_vif),decreasing = T)



alias(lm(form,data=rg_train1))$Complete

rg_train1$ID




## k is the vif value initialized to a very high value
k <- 100000000
## appended_dropped is a sentinal value which will change with k so that we don't run into infinite loop
appended_dropped <- c('y')

## loop will run untill all the values have vif lower than 4
while(k > 4){
  for_vif=lm(form,data=rg_train1) ## first a linear model for understanding the linear combination
  k <- sort(vif(for_vif),decreasing = T)[1] ## get the value of vif for highest value
  if (k <= 4){
    break
  }
  var_dropped <- names(k) ## get the name of the variable which has highest value
  print(k)
  appended_dropped <- c(var_dropped, appended_dropped) ## update the sentinal value with the new variable which needs to be dropped
  form <-
    as.formula(paste0('y ~ ', paste0(setdiff(
      names(rg_train1), c('y', 'ID', appended_dropped)
    ), collapse = ' + '))) ## update the formula everytime
}

## Always remember VIF is an iterative process for variable removal. Never ever use it at once
## VIF doesn't depend on dependent variable, Even if you change the dependent variable the information will remain the same 

## chanign the dependent variable to factor
rg_train1$y <- as.factor(rg_train1$y)

nchar(form)

## creating the logistic model
log_fit=glm(form,data=rg_train1,family = "binomial")

## run the stepwise
log_fit=step(log_fit)
# this might take 5-6 minutes to finish 
formula(log_fit)


glm(y ~ balance + day + pdays + previous + job_student + job_housemaid + 
      job_retired + job_admin. + job_technician + job_management + 
      marital_married + education_primary + education_tertiary + 
      On_LOAD_no + housing_yes + loan_no + contact_cellular + poutcome_other + 
      poutcome_failure + month_mar + month_sep + month_oct + month_jan + 
      month_feb + month_apr + month_jun + month_jul + campaign_9 + 
      campaign_8 + campaign_7 + campaign_6 + campaign_5 + campaign_4 + 
      campaign_3 + campaign_2 + age_18_28_0 + age_38_48_0 + age_58_68_0 + 
      age_68_78_0 + age_GT_88_1 + duration_200_300_0 + duration_300_400_0 + 
      duration_400_500_0 + duration_500_600_0 + duration_600_700_0 + 
      duration_700_800_0 + duration_800_900_0 + duration_GT_900_0, data= rg_train1, family = 'binomial')


## get all the variables required using the last step of stepwise


form <-
  as.formula("
   y ~ balance + day + pdays + previous + job_student + job_housemaid + 
      job_retired + job_admin. + job_technician + job_management + 
      marital_married + education_primary + education_tertiary + 
      On_LOAD_no + housing_yes + loan_no + contact_cellular + poutcome_other + 
      poutcome_failure + month_mar + month_sep + month_oct + month_jan + 
      month_feb + month_apr + month_jun + month_jul + campaign_9 + 
      campaign_8 + campaign_7 + campaign_6 + campaign_5 + campaign_4 + 
      campaign_3 + campaign_2 + age_18_28_0 + age_38_48_0 + age_58_68_0 + 
      age_68_78_0 + age_GT_88_1 + duration_200_300_0 + duration_300_400_0 + 
      duration_400_500_0 + duration_500_600_0 + duration_600_700_0 + 
      duration_700_800_0 + duration_800_900_0 + duration_GT_900_0")

log_fit=glm(form,data=rg_train1,family='binomial')

## Run the logistic
## if you find any warning of 'fitted probabilites of 1/0' 
## It is due to complete/quasi separation
## you need to remove the variable which is behaving similar to your dependent variable
## there is no shortcut, but a basic relationship using table can be gathered to remove them
## There is another way to find out(mentioned below):
## alternatively, with the given set of variable run it by picking one variable at a time and adding it to the equation,and running the logistic regression
## so an example would be y ~ x1 in first step, then y ~ x1 + x2 in second step, then y ~ x1 + x2 + x3, if adding x3 results to 1/0 fitted probabilites then x3 
## is not right variable, hence you drop it.
## drop that variable. Carry the above approach untill you are finished with all the variables


saveRDS(file='log_fit_class.RDS', log_fit)
summary(log_fit)
log_fit <- readRDS(file='log_fit_class.RDS')

## caTools to get the ROC:
## Run it to determine the ROC Plot
## Install caTools library
caTools::colAUC(predict(log_fit, rg_train1, type = 'response'), 
                rg_train1$y, plotROC = TRUE)

caTools::colAUC(predict(log_fit, rg_train2, type = 'response'), 
                rg_train2$y, plotROC = TRUE)


#### performance of score model on validation data
library(pROC)
# install.packages('pROC')

## scoring the test(validation) data
val.score=predict(log_fit,newdata = rg_train2,type='response')

## scoring the train
train.score <- predict(log_fit, newdata=rg_train1, type='response')

#comparing the auc for train and test
auc(roc(rg_train2$y,val.score))
auc(roc(rg_train1$y,train.score))


# so the tentative score performance of logistic regression is going to be around 0.95
# now lets build the model on entire training data

# code given below is result of multiple iterations
## final model for glm with given set of variables,
## the final variables are result of step wise
## bivariate analysis of data(relationship b/w dependent variable an indepndent variable)

# now if we needed to submit probability scores for the test data we can do at this point

##final score to be submitted
test.prob.score= predict(log_fit,newdata = rg_test,type='response')
write.csv(test.prob.score,"proper_submission_file_name.csv",row.names = F)

# however if we need to submit hard classes, we'll need to determine cutoff score

## scoring the train2 and train1
rg_train1$score <- predict(log_fit, newdata=rg_train1,type = 'response')
rg_train2$score <- predict(log_fit, newdata=rg_train2, type = 'response')


library(ggplot2)

ggplot(data=rg_train1,aes(x = score, y = y, col=y)) + geom_point() + geom_jitter()


head(rg_train1)

## dplyr::ntile
## Decile wise information for doing decile wise analysis
rg_train1$decile <- ntile(rg_train1$score, 10)



x <- rg_train1 %>% 
  group_by(decile) %>% 
  summarise(counts = n(), event_sum = sum(as.numeric(as.character(y))),'min'= min(score),'max' = max(score)) %>%
  arrange(desc(decile)) %>% 
  data.frame()
x

## This output to be pasted on excel sheet shared earlier, only two columns are required here 
## the counts and event_sum
## you need to reverse sort the data using decile and copy and paste these columns
## in the given excel sheet
write.csv(file='X.csv',x, row.names = F)

## For K-S we use below code
## determine the score using predict
train.score = predict(log_fit, newdata = rg_train1, type = 'response')

## get the real value using Revenue.Grid of rg_train1
real=rg_train1$y
length(real)
## get 999 values of probabilities score for which you want to test TP, FP, FN and TN
cutoffs=seq(0.0001,0.9999,0.0001)
length(cutoffs)

## Create a data frame with initialised garbage values
cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

## iterating the loop for all the 999 probabilities
for(cutoff in cutoffs){
  ## determine the prediction for each cut off here
  predicted=as.numeric(train.score>cutoff) ## Here hard classess of 0 and 1 are generated for every iteration
  
  ## fill the value of TP, FP, FN and TN
  TP=sum(real==1 & predicted==1) #True Positive Rate 
  TN=sum(real==0 & predicted==0) # True Negative Rate
  
  FP=sum(real==0 & predicted==1) # False Positive Rate
  FN=sum(real==1 & predicted==0) # False Negative Rate
  
  P=TP+FN # Total Positives
  N=TN+FP # Total Negatives
  
  Sn=TP/P # True Positive Rate : Sensitivity
  Sp=TN/N # True Negative Rate : Specificity
  precision=TP/(TP+FP) ## Precision
  recall=Sn ## Sensitivity and REcall is same, also called as hit
  ## KS is the cutoff
  KS=(TP/P)-(FP/N) ## KS cutoff 
  
  F5=(26*precision*recall)/((25*precision)+recall)
  ## F.1 score is maximum at 1 and min at 0
  ## A value of F.1 closer to 1 is good
  ## In case of low event rate model, F.1 closer to 1 is great
  ## F.1 score captures both precision and recall hence it is very useful in case of low event rate model
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  ## Binding the data
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

View(cutoff_data)

## removing the garbage column
cutoff_data=cutoff_data[-1,]

## getting the row where maximum value of KS is there
cutoff_data[cutoff_data$KS == max(cutoff_data$KS),]
View(cutoff_data)


## use the cut off to get the hard class value
rg_train1$predicted_Class <- (rg_train1$score > 0.1161)+0
table(rg_train1$predicted_Class)
install.packages('e1071')
library(caret)

## Draw the confusion matrix:
## Draw it for both train and test, compare them to see if they both behave similarly
confusionMatrix(factor(rg_train1$predicted_Class),
                rg_train1$y,
                positive = '1')



# #### visualise how these measures move across cutoffs
# library(ggplot2)
# ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()
# 
# library(tidyr)
# 
# cutoff_long=cutoff_data %>% 
#   gather(Measure,Value,Sn:M)
# 
# ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()
# 
# 
 my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
# 
# my_cutoff

# now that we have our cutoff we can convert score to hard classes
#my_cutoff=0.17
## In case you want to submit hard class probability
test.predicted=as.numeric(test.prob.score>my_cutoff)
write.csv(test.predicted,"proper_submission_file_name.csv",row.names = F)

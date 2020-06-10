#bank
library(plyr)
library(dplyr)

bank_train= read.csv('D:/R_dir/project/bank/bank-full_train.csv', stringsAsFactors = FALSE)

bank_test= read.csv('D:/R_dir/project/bank/bank-full_test.csv', stringsAsFactors = FALSE)

bank_train <- bank_train %>% mutate(target= as.numeric(y=='yes')) %>% select(-y) 

bank_test$target <- NA

glimpse(bank_train)

bank_train$data= 'train'
bank_test$data= 'test'

bank_all=rbind(bank_train,bank_test)

#finding outliers
#plotting in linear regression
# plot(
#   bank_data$age,
#   bank_data$y,
#   xlim = c(18, 100),
#   ylim = c(10,100),
#   main = "With Outliers",
#   xlab = "age",
#   ylab = "target",
#   pch = "*",
#   col = "red",
#   cex = 2
# )
# abline(lm(y ~ age, data=bank_data), col="blue", lwd=3, lty=2)
glimpse(bank_all)

boxplot(bank_all$age)

lapply(bank_all, function(x) sum(is.na(x)))

nrow((bank_all[abs((bank_all$age - mean(bank_all$age))) > 3 * sd(bank_all$age),]))

bank_all$age <- ifelse(abs((bank_all$age - mean(bank_all$age))) >= 3 * sd(bank_all$age),
       mean(bank_all$age),
       bank_all$age)
#sweep(bank_data, 2, mean(bank_data$age),'-')
# quantile(bank_all$age)
#bank_all$age  <-  ifelse(bank_all$age <= 18, 1, ifelse(bank_all$age <= 33, 2, ifelse(bank_all$age <= 39, 3, ifelse(bank_all$age <= 48, 4 , 5))))

# bank_all <- bank_all %>% 
#   mutate(age_18_24=ifelse(age >=18 & age<=24,1,0),
#          age_25_29=ifelse(age >24 & age <= 30,1,0),
#          age_30_36=ifelse(age >30 & age <= 36,1,0),
#          age_37_41=ifelse(age >36 & age <= 42,1,0),
#          age_42_48=ifelse(age >42 & age <= 48,1,0),
#          age_49_60=ifelse(age >48 & age <= 60,1,0)) %>% 
#   select( -age)

bank_all <- bank_all %>% 
  mutate(job_1=as.numeric(job %in% c('entrepreneur','self-employed')),
         job_2=as.numeric(job %in% 'admin.'),
         job_3=as.numeric(job %in% 'blue-collar'),
         job_4=as.numeric(job %in% 'housemaid'),
         job_5=as.numeric(job %in% 'management'),
         job_6=as.numeric(job %in% 'retired'),
         job_7=as.numeric(job %in% 'services'),
         job_8=as.numeric(job %in% 'student'),
         job_9=as.numeric(job %in% 'technician'),
         job_10=as.numeric(job %in% 'unemployed')
         ) %>% select(-job)

bank_all <- bank_all %>% 
  mutate(education_1=as.numeric(education %in% 'primary'),
         education_2=as.numeric(education %in% 'secondary'),
         education_3=as.numeric(education %in% 'tertiary'),
         ) %>% select (-education)

bank_all <- bank_all %>% 
  mutate(marital_status_1= as.numeric(marital %in% 'divorced'),
         marital_status_2= as.numeric(marital %in% 'married')) %>% 
  select(-marital)

bank_all$default_1 <- (bank_all$default %in% 'yes')+0

bank_all$balance_gt_0 <- (bank_all$balance > 0)+0

bank_all$housing_yes <- (bank_all$housing %in% 'yes')+0

bank_all$personal_loan_yes <- (bank_all$loan %in% 'yes')+0

bank_all <- bank_all %>% 
  mutate(contact_cell= as.numeric(contact %in% 'cellular'),
         contact_tel= as.numeric(contact %in% 'telephone')) %>% 
  select (-contact, -loan, -housing, -balance, -default)

# bank_all <- bank_all %>% 
#   mutate(duration_gt_60= (duration > 60)+0) %>% select(-duration)

# bank_all$pdays_first_contact <- (bank_all$pdays < 0)+0

bank_all <- bank_all %>% 
  mutate(poutcome_failure= as.numeric(poutcome %in% 'failure'),
         poutcome_success= as.numeric(poutcome %in% 'success'),
         poutcome_other= as.numeric(poutcome %in% 'other'),
         ) %>% select(-poutcome, -pdays, -ID)

# bank_all <- bank_all %>% 
#   mutate(mon1 = as.numeric(month == "jan"),
#        mon2 = as.numeric(month== "feb"),
#        mon3 = as.numeric(month== "mar"),
#        mon4 = as.numeric(month== "apr"),
#        mon5 = as.numeric(month== "may"),
#        mon6 = as.numeric(month== "jun"),
#        mon7 = as.numeric(month== "jul"),
#        mon8 = as.numeric(month== "aug"),
#        mon9 = as.numeric(month== "sep"),
#        mon10 = as.numeric(month== "oct"),
#        mon11 = as.numeric(month== "nov")) %>% 
#   select(-month)


bank_all <- bank_all %>% select(-month)
#standardizing
# bank_all$previous_stdize <-
#   (bank_all$previous - mean(bank_all$previous)) / sd(bank_all$previous)

glimpse(bank_all)

#seperating train and test data
bank_train_p <- bank_all %>% filter(data=='train') %>% select(-data)
bank_test_p <- bank_all %>% filter(data=='test') %>% select(-data, -target)

#scoring model
library(car)

 s=sample(1:nrow(bank_train_p),0.8*nrow(bank_train_p))
 bank_train_p1 <- bank_train_p[s,] 
 bank_train_p2 <- bank_train_p[-s,]

form <-
  as.formula(paste0('target ~', paste0(setdiff(
    names(bank_train_p1), 'target'
  ), collapse = '+')))

variable_drop <- c()
repeat {
  vif_calc= lm(form, data=bank_train_p1)
  k <- sort(vif(vif_calc), decreasing = TRUE)[1]
  if(k < 4) {
    break
  }
  print(k)
  variable_drop <- c(variable_drop, names(k))

  form <-
    as.formula(paste0('target ~ ', paste0(setdiff(
      names(bank_train_p1), c('target', variable_drop)
    ), collapse = ' + ')))  
}

bank_train_p1$target <-  as.factor(bank_train_p1$target)

log_fit=glm(form,data=bank_train_p1,family = "binomial")

log_fit_new <- step(log_fit)

formula(log_fit_new)
summary(log_fit_new)

form_new <-
  as.formula(
    "target ~ duration + campaign + job_2 + job_5 + 
    job_6 + job_8 + education_1 + 
    marital_status_2 + balance_gt_0 + housing_yes + 
    personal_loan_yes + contact_cell + contact_tel + poutcome_failure + 
    poutcome_success + poutcome_other"
  )

log_fit_new=glm(form_new,data=bank_train_p1,family='binomial')

summary(log_fit_new)

# form_new <- as.formula("target ~ age_18_24 + age_25_29 + age_42_48 + age_49_60 +  
#   job_6 + education_1 + education_3 + 
#   marital_status_2 + balance_gt_0 + housing_yes + personal_loan_yes + 
#   contact_cell + contact_tel + duration_gt_60 + poutcome_failure + 
#   poutcome_success + poutcome_other")

#Verifying model

caTools::colAUC(predict(log_fit_new, bank_train_p1, type = 'response'), 
                bank_train_p1$target, plotROC = TRUE)

caTools::colAUC(predict(log_fit_new, bank_train_p2, type = 'response'), 
                bank_train_p2$target, plotROC = TRUE)
library(car)
#predicting
bank_train_p1$train1_score <- predict(log_fit_new, data=bank_train_p1, type='response')
bank_train_p2$train2_score <- predict(log_fit_new, data=bank_train_p2, type='response')

#comparing AUC
library(pROC)
auc(roc(bank_train_p1$target, bank_train_p1$train1_score))
auc(roc(bank_train_p2$target,  bank_train_p1$train2_score))


#submission of file
glimpse(bank_test_p)
test.probs = predict(log_fit_new, data=bank_test_p, type='response')
write.csv(test.probs,'Nitesh_Bhosle_P5_part2.csv',row.names = F)

#calculating KS cutt-off
#train_score = predict(log_fit_new, newdata = bank_train_p, type = 'response')
real=bank_train_p$target
cutoffs=seq(0.001,0.999,0.001)

## Create a data frame with initialised garbage values
cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)
for(cutoff in cutoffs){
  ## determine the prediction for each cut off here
  predicted=as.numeric(train_score>cutoff)
  
  ## fill the value of TP, FP, FN and TN
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  ## KS is the cutoff
  KS=(TP/P)-(FP/N)
  
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

#to remove garbage that we used to declare empty dataframe
cutoff_data=cutoff_data[-1,]

#row having max cuttoff
cutoff_data[cutoff_data$KS == max(cutoff_data$KS),]

# KS --> 0.63394
#cutt of --> 0.109

##########
#submission for hard class
test.class=as.numeric(predict(log_fit_new,newdata=bank_test_p,type='response')>0.109)
#test.class=ifelse(test.class==1,'Yes','No')
write.csv(test.class,'Bank_Predict_hard_class.csv',row.names = F)




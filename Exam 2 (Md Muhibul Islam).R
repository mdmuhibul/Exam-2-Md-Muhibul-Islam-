
#Exam 2; Fall 2023
#Md Muhibul Islam


# Question 1
#I am going to generate a hypothesis test about whether men and women are equally likely to live in the same state that
#they were born or not.

#summary(live_same_state_born[as.logical(female)])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0.0000  0.0000  1.0000  0.5557  1.0000  1.0000
#> summary(live_same_state_born[!as.logical(female)])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0.0000  0.0000  1.0000  0.5384  1.0000  1.0000
#> summary(as.logical(female))
#Mode   FALSE    TRUE
#logical   50579   45087


#Initially, I am going to extract the necessary proportions and sample sizes from mentioned data:
# Proportions
prop_men = 0.5384
prop_women = 0.5557

# Sample sizes
n_men = 50579
n_women = 45087

# Perform the test statistic
test_result = prop.test(x = c(prop_men * n_men, prop_women * n_women),
                        n = c(n_men, n_women),
                        correct = FALSE)
print(test_result)
#X-squared = 28.787, df = 1, p-value = 8.079e-08
#alternative hypothesis: two.sided
#95 percent confidence interval:
#-0.02361754 -0.01098246
#sample estimates:
#prop 1 prop 2
#0.5384 0.5557

#Here, I got test ststistic is 28.787, p-value is 8.079e-08, degree of freedom 1 as well as 95 percent confidence interval is -0.02361754 and -0.01098246
#This P value is extremely small which is less than chosen significance level 0.05, the null hypothesis is rejected. A p value of 8.079e-08 means that there is
#strong evidence against null hypothesis which leads to reject.
#Based on the avove analysis, it demonstrates that they are not equally likely to live in the same state that they were born.



#Question 2: Create a subgroup of the sample, that makes sense as you focus on the decision of whether to move away. Perhaps look at a
#certain age range or education or occupation or degree field. Explain your rationale

library(tidyverse)
attach(acs2021)
View(acs2021)

summary(acs2021)
summary(acs2021$AGE)
obj1 <- (acs2021$AGE >= 25)
summary(obj1)

#Here I have chosen the variable AGE where their age is 25 or more than 25. It is interestng to me because it shows that 2350139 
#people made decision to move away.



#Question 3:

summary(acs2021)
summary(acs2021$AGE)
obj1 <- (acs2021$AGE >= 25)
dat_use <- subset(acs2021, obj1)
summary(dat_use)

model1 <- lm(live_same_state_born ~ EDUC + female + MARST + RACE, data = dat_use)
summary(model1)

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4833 on 2350125 degrees of freedom
#Multiple R-squared:  0.06539,	Adjusted R-squared:  0.06539 
#F-statistic: 1.265e+04 on 13 and 2350125 DF,  p-value: < 2.2e-16

# simplify the education dummy
dat_use$live_same_state_born <- dat_use$female + dat_use$educ_advdeg

# whole dataset
model1 <- lm(live_same_state_born ~ female + EDUC + AGE + I(female*educ_college) + I(AGE * female), data = dat_use)
summary(model1)

#Residual standard error: 1.16e-11 on 2350124 degrees of freedom
#Multiple R-squared:      1,	Adjusted R-squared:      1 
#F-statistic: 4.712e+26 on 14 and 2350124 DF,  p-value: < 2.2e-16

dat_use_female <- subset(dat_use,as.logical(dat_use$female))
dat_use_male <- subset(dat_use,!(dat_use$female))

#Here, I am going to split into 2 parts

model1 <- lm(live_same_state_born ~ EDUC + AGE, data = dat_use_female)
summary(model1)
#Residual standard error: 4.241e-12 on 1215684 degrees of freedom
#Multiple R-squared:      1,	Adjusted R-squared:      1 
#F-statistic: 7.794e+26 on 11 and 1215684 DF,  p-value: < 2.2e-16


model1 <- lm(live_same_state_born ~ EDUC + AGE, data = dat_use_male)
summary(model1)

#Residual standard error: 1.181e-13 on 1134431 degrees of freedom
#Multiple R-squared:      1,	Adjusted R-squared:      1 
#F-statistic: 8.887e+29 on 11 and 1134431 DF,  p-value: < 2.2e-16




#Question 4: Estimate a better OLS model for whether they live in the same state as born, within your subsample. You should include more than just gender and education now.


#Here, I am going to add three more variables named as Hispanic, PERWT & EMPSTAT to run the OLS model.
model1 <- lm(live_same_state_born ~ EDUC + female + MARST + RACE + Hispanic + PERWT + EMPSTAT, data = dat_use)
summary(model1)

#Residual standard error: 6.647e-12 on 2350123 degrees of freedom
#Multiple R-squared:      1,	Adjusted R-squared:      1 
#F-statistic: 1.341e+27 on 15 and 2350123 DF,  p-value: < 2.2e-16


to_be_predicted2 <- data.frame(AGE = 20:55, SEX = "Female", EMPSTAT = "Employed - at work") 
to_be_predicted2$dhaka <- predict(model1, newdata = to_be_predicted2, type= "response")
summary(to_be_predicted2$dhaka)


#How amny type 1 and type 2 errors

index<-sample(x=2,size=nrow(live_same_state_born),replace=TRUE,prob=c(0.7,0.3))
train<-kids[index==1,]
test<-kids[index==2,]
trainmodel<-lm(live_same_state_born ~ AGE + SEX + MARST + EMPSTAT , data = dat_use)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$live_same_state_born,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum


#Here, it shows that the type 1 error is 193  and the type two error is 783 based on the output.  It can predict the train data 
#accurately.It correctly predicts 57% of the train data.



#Question 5: Estimate a simple logit model, for the outcome variable live_same_state_born, within your subsample

model1 <- glm(live_same_state_born ~ EDUC + female + MARST + RACE + Hispanic + PERWT + EMPSTAT, data = dat_use, family = binomial)
summary(model1)

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 2124118  on 2350138  degrees of freedom
#Residual deviance: 1618367  on 2350122  degrees of freedom
#AIC: 1618401

pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_use$female =="HIGH SCHOOL")

set.seed(12345)
index<-sample(x=2,size=nrow(AGE),replace=TRUE,prob=c(0.7,0.3))
train<-kids[index==1,]
test<-kids[index==2,]
trainmodel<-glm(live_same_state_born > 1 ~ AGE + SEX + EMPSTAT, data = dat_use, family= binomial)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$live_same_state_born,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum




#Question 6

#estimate one or more additional models:

library(randomForest)
library(tidyverse)
set.seed(54321)
model_randFor <- randomForest(as.factor(live_same_state_born) ~ SEX + EMPSTAT, data = dat_use, importance=TRUE, proximity=TRUE)
round(importance(model_randFor),2)
varImpPlot(model_randFor)

# look at confusion matrix for this too
pred_model1 <- predict(model_randFor,  data = dat_use)
table(pred = pred_model1, true = dat_test$live_same_state_born)

#Using anothe vector machine

install.packages("spikeslab")
set.seed(54321)
model1_spikeslab <- spikeslab(acs2021$EDUC, data = dat_use)
summary(model1_spikeslab)
print(model1_spikeslab)
plot(model1_spikeslab)


#Using another model

install.packages("glmnet")
model1_elasticnet <-  glmnet(as.matrix(acs2021$EDUC[,-1]),sobj$data$live_same_state_born) 
# default is alpha = 1, lasso

cvmodel1_elasticnet = cv.glmnet(data.matrix(acs2021$female[,-1]),data.matrix(acs2021$live_same_state_born)) 
cvmodel1_elasticnet$lambda.min
log(cvmodel1_elasticnet$lambda.min)
coef(cvmodel1_elasticnet, s = "lambda.min")

pred1_elasnet <- predict(model1_elasticnet, newx = data.matrix(s_dat_test), s = cvmodel1_elasticnet$lambda.min)
pred_model1_elasnet <- (pred1_elasnet < mean(pred1_elasnet)) 
table(pred = pred_model1_elasnet, true = acs2021$live_same_state_born)


model2_elasticnet <-  glmnet(as.matrix(acs2021$male[,-1]),sobj$data$pub_work, alpha = 1) 
print(model2_elasticnet)
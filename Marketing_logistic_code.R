getwd()
interim<-read.csv("interimfile.csv")
library(ggplot2)
scatter.smooth(interim$age,interim$last_contact_duration)
abline(lm(interim$age ~ interim$last_contact_duration))

hist(interim$last_contact_duration)
median(interim$last_contact_duration)

hist(interim$yearly_balance)

hist(interim$days_since_last_contact)


edu_target <-  ggplot() + geom_bar(aes(y = target_subscribed, x = education_level, fill = target_subscribed), data = interim,
                                stat="identity")
edu_target

marital_target <-  ggplot() + geom_bar(aes(y = target_subscribed, x = marital_status, fill = target_subscribed), data = interim,
                                   stat="identity")
marital_target

last_communication_target <-  ggplot() + geom_bar(aes(y = target_subscribed, x = last_communcation_type, fill = target_subscribed), data = interim,
                                   stat="identity")
last_communication_target


previous_campaign_target <-  ggplot() + geom_bar(aes(y = target_subscribed, x = previous_campaign_outcome, fill = target_subscribed), data = interim,
                                                  stat="identity")
previous_campaign_target

personal_loan_target <-  ggplot() + geom_bar(aes(y = target_subscribed, x = personal_loan, fill = target_subscribed), data = interim,
                                                 stat="identity")
personal_loan_target

## logistic regression model

install.packages("caTools")
library(caTools)

set.seed(100)
library(dplyr)

#logistic_regression classification
train <- sample_frac(interim,0.8)
sid <- as.numeric(rownames(train))
test <- interim[-sid,]

#checking for frequency of the two classes for both training and test dataset
freq_train <- table(train$target_subscribed)
freq_train

freq_test <- table(test$target_subscribed)
freq_test

interim$target_subscribed.1

df2 <- interim[, -which(names(interim) %in% c("housing_loan","target_subscribed","default_status","personal_loan","job_type","last_communcation_type","education_level","last_contact_month","previous_campaign_outcome","marital_status",'job_type_unknown','last_communcation_type_unknown','education_level_unknown','previous_campaign_outcome_unknown','last_contact_month_dec','marital_status_divorced'))]

train <- sample_frac(df2,0.8)
sid <- as.numeric(rownames(train))
test <- df2[-sid,]

#checking for frequency of the two classes for both training and test dataset
freq_train <- table(train$target_subscribed)
freq_train

freq_test <- table(test$target_subscribed)
freq_test

install.packages('clusterGeneration')
require(MASS)
require(clusterGeneration)


#removing multicollinearity using vif

fit <- lm(target_subscribed.1~., data=df2)
s <-vif(fit)
View(s)

df2_remove_precious_contact_failure <- df2[, -which(names(df2) %in% c("previous_campaign_outcome_failure"))]
fit1<- lm(target_subscribed.1~., data= df2_remove_precious_contact_failure)
u <- vif(fit1)
View(u)


df2_total_contact <- df2_remove_precious_contact_failure[, -which(names(df2_remove_precious_contact_failure) %in% c("total_contacts"))]
fit2<- lm(target_subscribed.1~., data= df2_total_contact)
u <- vif(fit2)
View(u)


df2_june_july <- df2_total_contact[, -which(names(df2_total_contact) %in% c("last_contact_month_jun","last_contact_month_jul"))]
fit3<- lm(target_subscribed.1~., data= df2_june_july)
v <- vif(fit3)
View(v)


df2_days_since_last_contact <- df2_june_july[, -which(names(df2_june_july) %in% c("days_since_last_contact"))]
fit4<- lm(target_subscribed.1~., data= df2_days_since_last_contact)
w <- vif(fit4)
View(w)

df2_jobs <- df2_days_since_last_contact[, -which(names(df2_days_since_last_contact) %in% c("job_type_admin.","job_type_self.employed","job_type_housemaid","job_type_services","job_type_technician"))]
fit5<- lm(target_subscribed.1~., data= df2_jobs )
y <- vif(fit5)
View(y)



df2_not_contacted <- df2_jobs[, -which(names(df2_jobs) %in% c("not_contacted","X"))]
fit6<- lm(target_subscribed.1~., data= df2_not_contacted)
z <- vif(fit6)
View(z)

logistic_dataset <- df2_not_contacted

train <- sample_frac(logistic_dataset,0.8)
sid <- as.numeric(rownames(train))
test <- logistic_dataset[-sid,]

#checking for frequency of the two classes for both training and test dataset
freq_train <- table(train$target_subscribed)
freq_train

freq_test <- table(test$target_subscribed)
freq_test


##logistic regression model

new_data <- train[, -which(names(train) %in% c("last_contact_month_mar","age","job_type_management","education_level_secondary","log_days_since_last_contact","default_status.1","job_type_unemployed","last_contact_month_feb","contacts_before_current_campaign","last_contact_month_day","marital_status_single","job_type_entrepreneur","job_type_blue.collar","last_contact_month_apr","previous_campaign_outcome_other","yearly_balance"))]
#new_data1 <- train[, -which(names(train) %in% c("last_contact_month_mar","age","last_contact_month_day","contacts_before_current_campaign","job_type_management","default_status.1","job_type_unemployed","education_level_secondary","last_contact_month_feb","log_days_since_last_contact","marital_status_single"))]
model <- glm (new_data$target_subscribed.1~. , data = new_data, family = binomial(), control = list(maxit=1000, trace = TRUE))
summary(model)

new_data1 <- test[, -which(names(test) %in% c("last_contact_month_mar","age","job_type_management","education_level_secondary","log_days_since_last_contact","default_status.1","job_type_unemployed","last_contact_month_feb","contacts_before_current_campaign","last_contact_month_day","marital_status_single","job_type_entrepreneur","job_type_blue.collar","last_contact_month_apr","previous_campaign_outcome_other","yearly_balance"))]
#new_data1 <- train[, -which(names(train) %in% c("last_contact_month_mar","age","last_contact_month_day","contacts_before_current_campaign","job_type_management","default_status.1","job_type_unemployed","education_level_secondary","last_contact_month_feb","log_days_since_last_contact","marital_status_single"))]
model1 <- glm (new_data1$target_subscribed.1~. , data = new_data1, family = binomial(), control = list(maxit=1000, trace = TRUE))
summary(model1)



install.packages("caret")
library(caret)
library(rlang)
library(e1071)
pdata <- predict(model1, newdata = new_data1, type = "response")

View(pdata)

log_result <- cbind(new_data1,pdata)

confusionMatrix(data = as.factor(as.numeric(pdata>0.5)), reference = as.factor(new_data1$target_subscribed.1))
write.csv(log_result,"logistic_regression_test_result.csv")



######################################################

# STEP 1: START #

install.packages("gmodels")
install.packages("HH")
install.packages("Hmisc")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("car")
install.packages("caret")
install.packages("dplyr")
library(HH)
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)
install.packages("InformationValue")
library(InformationValue)

cat("\014") # Clearing the screen

# STEP 1: END #

# STEP 2: START #

# Setting the working directory - 
getwd()
setwd("C:/Users/WAQUEE/Desktop/Studies/AMMA Assignment")

# reading client datasets
df.client <- read.csv('bank_client.csv')
str(df.client)

# reading other attributes
df.attr <- read.csv('bank_other_attributes.csv')
str(df.attr)

# reading campaign data
df.campaign <- read.csv('latest_campaign.csv')
str(df.campaign)

# reading campaign outcome
df.campOutcome <- read.csv('campaign_outcome.csv')
str(df.campOutcome)

# Create campaign data by joining all tables together
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)
length(unique(df.data$Cust_id)) == nrow(df.data) #checking for any duplicate customer ID

# clearing out temporary tables
rm(df.temp1,df.temp2)

# see few observations of merged dataset
head(df.data)

# STEP 2: END #

# STEP 3: START #

###### Rajneesh's Code Start ######

# see a quick summary view of the dataset
summary(df.data)

# see the tables structure
str(df.data)

# check the response rate
CrossTable(df.data$y)

# split the data into training and test
set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
nrow(df.train)
nrow(df.test)

# how the categorical variables are distributed and are related with target outcome
CrossTable(df.train$job, df.train$y)
CrossTable(df.train$marital, df.train$y)
CrossTable(df.train$education, df.train$y)
CrossTable(df.train$default, df.train$y)
CrossTable(df.train$housing, df.train$y)
CrossTable(df.train$loan, df.train$y)
CrossTable(df.train$poutcome, df.train$y)

# let see how the numerical variables are distributed
hist(df.train$age)
hist(df.train$balance)
hist(df.train$duration)
hist(df.train$campaign)
hist(df.train$pdays)
hist(df.train$previous)
describe(df.train[c("age", "balance", "duration", "campaign", "pdays", "previous")])

# running a full model  #

df.train$yact = ifelse(df.train$y == 'yes',1,0)
full.model <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                    job + marital + education + default + housing + loan + poutcome, 
                  data=df.train, family = binomial)
summary(full.model)

# check for vif
fit <- lm(formula <- yact ~ age + balance + duration + campaign + pdays + previous +
            job + marital + education + default + housing + loan + poutcome, 
          data=df.train)
vif(fit)

# automated variable selection - Backward
backward <- step(full.model, direction = 'backward')
summary(backward)



# training probabilities and roc
df.train$prob = predict(full.model, type=c("response"))
class(df.train)
nrow(df.train)
q <- roc(y ~ prob, data = df.train)
plot(q)
auc(q)

# variable importance
varImp(full.model, scale = FALSE)

# confusion matrix on training set
df.train$ypred = ifelse(df.train$prob>=.5,'pred_yes','pred_no')
table(df.train$ypred,df.train$y)

#probabilities on test set
df.test$prob = predict(full.model, newdata = df.test, type=c("response"))

#confusion matrix on test set
df.test$ypred = ifelse(df.test$prob>=.5,'pred_yes','pred_no')
table(df.test$ypred,df.test$y)


#ks plot
ks_plot(actuals=df.train$y, predictedScores=df.train$ypred)

###### Rajneesh's Code End ######

# STEP 3: END #

########## Waquee - code Start ##############

# STEP 4: START #

View(df.data)

df.data_final <- df.data
df.data_final$yact = ifelse(df.data$y == 'yes',1,0) #Loading 1s for 'yes' and 0s for 'no'
nrow(df.data_final)

df.data_final <- df.data_final[!apply(df.data_final[,c("age", "balance", "duration", "campaign", "pdays", "previous", "job","marital", "education", "default", "housing", "loan", "poutcome")], 1, anyNA),]
nrow(df.data_final)
View(df.data_final)

set.seed(1234)
df.data_final$rand <- runif(nrow(df.data_final))

df.train_Waquee_model <- df.data_final[df.data_final$rand <= 0.9,]
df.test_Waquee_model <- df.data_final[df.data_final$rand > 0.9,]
nrow(df.train_Waquee_model)

gc()

result_tentative_trainWaquee_model <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                                      job + marital + education + default + housing + loan + poutcome, 
                                    data=df.train_Waquee_model, family = binomial)
summary(result_tentative_trainWaquee_model)

df.train_Waquee_model_onlysig <- df.train_Waquee_model[df.train_Waquee_model$job!="unknown",]
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                                           job + marital + education + default + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)

df.test_Waquee_model_onlysig <- df.test_Waquee_model[df.test_Waquee_model$job!="unknown",]

summary(result_tentative_trainWaquee_model_sig1)

df.train_Waquee_model_onlysig$pdays <-NULL
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + default + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)


df.test_Waquee_model_onlysig$pdays <-NULL

summary(result_tentative_trainWaquee_model_sig1)

df.train_Waquee_model_onlysig <- df.train_Waquee_model_onlysig[df.train_Waquee_model_onlysig$marital!="single",]
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + default + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)

df.test_Waquee_model_onlysig <- df.test_Waquee_model_onlysig[df.test_Waquee_model_onlysig$marital!="single",]

summary(result_tentative_trainWaquee_model_sig1)

df.train_Waquee_model_onlysig <- df.train_Waquee_model_onlysig[df.train_Waquee_model_onlysig$marital!="yes",]
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)

df.test_Waquee_model_onlysig <- df.test_Waquee_model_onlysig[df.test_Waquee_model_onlysig$marital!="yes",]

summary(result_tentative_trainWaquee_model_sig1)

df.train_Waquee_model_onlysig <- df.train_Waquee_model_onlysig[df.train_Waquee_model_onlysig$job!="management",]
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)

df.test_Waquee_model_onlysig <- df.test_Waquee_model_onlysig[df.test_Waquee_model_onlysig$job!="management",]

summary(result_tentative_trainWaquee_model_sig1)

df.train_Waquee_model_onlysig <- df.train_Waquee_model_onlysig[df.train_Waquee_model_onlysig$poutcome!="other",]
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)

df.test_Waquee_model_onlysig <- df.test_Waquee_model_onlysig[df.test_Waquee_model_onlysig$poutcome!="other",]

summary(result_tentative_trainWaquee_model_sig1)

df.train_Waquee_model_onlysig <- df.train_Waquee_model_onlysig[df.train_Waquee_model_onlysig$job!="entrepreneur",]
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)

df.test_Waquee_model_onlysig <- df.test_Waquee_model_onlysig[df.test_Waquee_model_onlysig$job!="entrepreneur",]

summary(result_tentative_trainWaquee_model_sig1)

df.train_Waquee_model_onlysig <- df.train_Waquee_model_onlysig[df.train_Waquee_model_onlysig$education!="unknown",]
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)

df.test_Waquee_model_onlysig <- df.test_Waquee_model_onlysig[df.test_Waquee_model_onlysig$education!="unknown",]

summary(result_tentative_trainWaquee_model_sig1)

df.train_Waquee_model_onlysig <- df.train_Waquee_model_onlysig[df.train_Waquee_model_onlysig$job!="student",]
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)

df.test_Waquee_model_onlysig <- df.test_Waquee_model_onlysig[df.test_Waquee_model_onlysig$job!="student",]

summary(result_tentative_trainWaquee_model_sig1)

df.train_Waquee_model_onlysig <- df.train_Waquee_model_onlysig[df.train_Waquee_model_onlysig$job!="unemployed",]
result_tentative_trainWaquee_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Waquee_model_onlysig, family = binomial)

df.test_Waquee_model_onlysig <- df.test_Waquee_model_onlysig[df.test_Waquee_model_onlysig$job!="unemployed",]

summary(result_tentative_trainWaquee_model_sig1)

result_Waquee_model_sig1 <- result_tentative_trainWaquee_model_sig1
class(result_Waquee_model_sig1)
print(result_Waquee_model_sig1)
plot(result_Waquee_model_sig1)

plot(result_Waquee_model_sig1)
varImp(result_Waquee_model_sig1, scale = FALSE)

fit_Waquee_model <- lm(formula <- yact ~ age + balance + duration + campaign + previous +
                   job + marital + education + housing + loan + poutcome, 
                 data=df.train_Waquee_model_onlysig)
vif(fit_Waquee_model)

backward_Waquee_model <- step(result_Waquee_model_sig1, direction = 'backward')
summary(backward_Waquee_model)

result_Waquee_model_probs <- df.train_Waquee_model_onlysig
nrow(result_Waquee_model_probs)
class(result_Waquee_model_probs)
result_Waquee_model_probs$prob = predict(result_Waquee_model_sig1, type=c("response"))
q_Waquee_model <- roc(y ~ prob, data = result_Waquee_model_probs)
plot(q_Waquee_model)
auc(q_Waquee_model)

CrossTable(df.train_Waquee_model_onlysig$job, df.train_Waquee_model_onlysig$y)
CrossTable(df.train_Waquee_model_onlysig$marital, df.train_Waquee_model_onlysig$y)
CrossTable(df.train_Waquee_model_onlysig$education, df.train_Waquee_model_onlysig$y)
CrossTable(df.train_Waquee_model_onlysig$default, df.train_Waquee_model_onlysig$y)
CrossTable(df.train_Waquee_model_onlysig$housing, df.train_Waquee_model_onlysig$y)
CrossTable(df.train_Waquee_model_onlysig$loan, df.train_Waquee_model_onlysig$y)
CrossTable(df.train_Waquee_model_onlysig$poutcome, df.train_Waquee_model_onlysig$y)

hist(df.train_Waquee_model_onlysig$age)
hist(df.train_Waquee_model_onlysig$balance)
hist(df.train_Waquee_model_onlysig$duration)
hist(df.train_Waquee_model_onlysig$campaign)
hist(df.train_Waquee_model_onlysig$previous)

result_Waquee_model_probs$ypred = ifelse(result_Waquee_model_probs$prob>=.5,'pred_yes','pred_no')
table(result_Waquee_model_probs$ypred,result_Waquee_model_probs$y)

df.test_Waquee_model_onlysig$prob = predict(result_Waquee_model_sig1, newdata = df.test_Waquee_model_onlysig, type=c("response"))

df.test_Waquee_model_onlysig$ypred = ifelse(df.test_Waquee_model_onlysig$prob>=.5,'pred_yes','pred_no')
table(df.test_Waquee_model_onlysig$ypred,df.test_Waquee_model_onlysig$y)

ks_plot(actuals=result_Waquee_model_probs$y, predictedScores=result_Waquee_model_probs$ypred)

# STEP 4: END

############### Waquee - code End #############


# DO IT YOURSELF ------------------------------------------------------------------
# Improve your model by removing insignificant variables
# Use automated variable selection to improve models
# check performance of test data
#-------------------------------------------------------------------------------------

#setwd("C:/Users/WAQUEE/Desktop/Studies/AMMA Assignment")

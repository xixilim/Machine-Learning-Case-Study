##### Machine Learning Case Study 4 #####
# Team 13: Chenlin Cheng / Megan Brunick / Sebastian Rincon
# 12/02/2021

####################
## Data & Library ##
####################
rm(list = ls())
library(caret)
library(dplyr)
library(ggplot2)
library(rattle)
library(MASS)

case4 <- read.csv('Case4.csv', stringsAsFactors = TRUE)
summary(case4)
str(case4)
attach(case4)

#######################
## Logistic Analysis ##
#######################
shortlogmodel <- glm(intubated ~ age + Gender, family = binomial, data = case4)
summary(shortlogmodel)

exp(coef(shortlogmodel))

### Interpreting ###

# Based on our simple logistic model, both age and gender are significant predictors of the 
# probability of being intubated.

# For every one-year increase in a person's age, the odds of the person being intubated increases 
# by a factor of 1.0144. This makes logical sense because as someone ages, the immune system weakens 
# and a person may suffer from more health issues, which leads to a higher probability of intubation.

# When a person is a woman, the odds of being intubated increases by a factor of .7407. Since this 
# factor is less than 1, a person's odds of being intubated actually decreases as a woman. 
# Throughout the pandemic, many health organizations and news outlets reported that men were being
# hit harder by the effects of COVID-19 due to a mix of biological factors and social habits.
# With this knowledge, our prediction makes sense that the female gender is less likely to be intubated.


########################
## Divide the dataset ##
########################
set.seed(100)
divideData <- createDataPartition(case4$intubated, p = 0.4, list = FALSE)
train <- case4[divideData, ]
test <- case4[-divideData, ]

#########################
## Logistic Regression ##
#########################

logisticmodel <- glm(intubated ~ ., family = binomial, data = train)
summary(logisticmodel)

log_prob <- predict(logisticmodel, newdata = test, type = "response")
log_pred <- ifelse(log_prob > 0.5, "Yes", "No")

### Interpreting ###

table(log_pred, test$intubated)
# The logistic model produced 30946 true positives (Not intubated) and 6882 false positives.
# A false positive occurred when we predicted a No response, but the person was actually intubated.
# Due to the high prevalence of No's in our training data set, the model only predicted 1 Yes, which
# was a false negative and 0 true negatives. 
# A negative response refers to a person who was intubated, which is important to know for hospital
# planning purposes and monitoring different types of COVID-19 patients.
# This model was not very useful in predicting intubation.

logsensitivity <- 30946 / (30946+1) ; logsensitivity  # 0.9999
# We predicted true positives 99.99% of the time.
logspecificity <- 0 / (6882+0) ; logspecificity  # 0
# We did not predict any true negatives, so the model did very poorly in terms of specificity.


######################
## Center and Scale ##
######################

preprocessing <- train %>% preProcess(method = c('center', 'scale'))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

###############
## LDA model ##
###############

ldamodel <- lda(intubated ~ ., data = traintransformed)
ldamodel

ldapredictions <- ldamodel %>% predict(testtransformed)

### Interpreting ###

table(ldapredictions$class, testtransformed$intubated)
# The LDA model produced 30945 true positives (Not intubated) and 6882 false positives.
# This model had similar results as the logistic model with only 2 false negatives and 0 true negatives.
# These few predictions of false negatives and true negatives occurs because of the high quantity 
# of No's that we have in the data. Overall, this model shows a lack of viability to classify intubated,
# even though it correctly predicts not intubated almost every time (sensitivity).

ldasensitivity <- 30945 / (30945+2) ; ldasensitivity  # 0.9999
# We predicted true positives 99.99% of the time.
ldaspecificity <- 0 / (6882+0) ; ldaspecificity  # 0
# We predicted true negatives 0% of the time.


###############
## QDA model ##
###############

# When running the QDA model with all predictors, we got the following error message:
# "Error in qda.default(x, grouping, ...) : rank deficiency in group No"
# which means there could be collinearity or a relationship among some of our predictor variables.
# Since most of the variables in this data set are categorical, we decided to conduct a 
# Chi-square Test for Independence on a few of the concerning pairs.

# Chi-square Test for Independence between categorical variables
chisq.test(case4$Gender, case4$pregnant)
chisq.test(case4$labResults, case4$finalClassification)
# Both p-values in these tests are less than 0.05, so we reject the null hypothesis and conclude 
# that these two categorical variable pairs are correlated. The problem with the pregnant variable
# is that NotApplicable is always associated with gender Man. For labResults and finalClassification,
# their values are almost always parallel (e.g. "Yes" and "ConfirmedCase").

# Additionally, we noticed in our logistic model summary that pregnantNotApplicable and 
# finalClassificationNegativeCase have NA/undefined estimates and p-values, which also could
# be related to this issue.

# Therefore, we decided to remove pregnant and finalClassification from our model in order to 
# evaluate the QDA model results.

qdamodel <- qda(intubated ~ .-pregnant - finalClassification, data = traintransformed)
qdamodel

qdapredictions <- qdamodel %>% predict(testtransformed)


### Interpreting ###

table(qdapredictions$class, testtransformed$intubated)
# The QDA model is a more advanced technique, which assumes a quadratic decision boundary.
# This model produced better results than the Logistic Regression and LDA models in predicting
# 1657 true negatives (intubated), but it performed worse in predicting 26949 true positives (not intubated).
# The model incorrectly predicted 3998 people as being intubated, who were actually not intubated, 
# and 5225 people as not intubated, who were actually intubated.

qdasensitivity <- 26949 / (26949+3998) ; qdasensitivity  # 0.8708
# The model was able to successfully predict 87.08% of true positives.
qdaspecificity <- 1657 / (5225+1657) ; qdaspecificity  # 0.2407
# The model was able to successfully predict 24.07% of the true negatives.


###############
## KNN model ##
###############
knnmodel <- train(intubated~., data = train, method = 'knn', preProcess = c('center', 'scale'))
set.seed(100)
knnmodel$bestTune  # k=9
plot(knnmodel)

knnpredictions <- predict(knnmodel, newdata = test)

### Interpreting ###

confusionMatrix(knnpredictions, test$intubated)
# The KNN model produced 30297 true positives (Not intubated) and 6609 false positives.
# The model also predicted 650 false negatives and 273 true negatives (intubated).
# This means that we predicted 650 people as intubated who were actually not intubated, and 
# 6609 people as not intubated who were actually intubated.
# The model performed slightly better than Logistic and LDA in identifying patients who were 
# intubated, but maintained a high level of accuracy in predicting people who were not intubated.

knnsensitivity <- 30297/(30297+650) ; knnsensitivity # 0.9790
# We predicted true positives 97.90% of the time.
knnspecificity <- 273/(6609+273) ; knnspecificity # 0.0397
# We predicted true negatives 3.97% of the time.


####################
## Accuracy Rates ##
####################

### Logistic Regression ###
mean(log_pred == test$intubated)
# Given a proportion of 0.4 and a set seed of 100, the accuracy rate of the Logistic model is 0.8180 or 81.80%

### LDA model ###
mean(ldapredictions$class == testtransformed$intubated)
# Given a proportion of 0.4 and a set seed of 100, the accuracy rate of the LDA model is 0.8180 or 81.80%

### QDA model ###
mean(qdapredictions$class == testtransformed$intubated)
# Given a proportion of 0.4 and a set seed of 100, the accuracy rate of the QDA model is 0.7561 or 75.61%

### KNN model ###
mean(knnpredictions == test$intubated)
# Given a proportion of 0.4 and a set seed of 100, the accuracy rate of the KNN model is 0.8081 or 80.81%


################
## Best Model ##
################
# After interpreting and comparing our four different analyses, we chose the K-Nearest Neighbors (KNN) 
# as the best model for classifying intubated patients. The accuracy rate of this model was 80.81%, which
# was almost equal to the accuracy rates of our Logistic Regression and LDA models. We eliminated the 
# QDA model because it had a lower accuracy rate than the other three models. Looking at further qualities 
# like the interpretability of each model and the confusion matrices, we decided the KNN model achieved 
# the best balance of predicting not intubated and intubated patients, having an almost perfect 
# sensitivity rate and a 4% specificity rate, which was the highest of the three contending models.
# The problem with the Logistic and LDA models is that they achieved the highest accuracy rates by
# almost always predicting No because of the high proportion of No's in the data. However, this is not the
# most valid classification method in trying to differentiate between intubated and not intubated patients,
# which is why our team selected KNN as the best model.
# The KNN model is a nonparametric method, meaning that the data is likely highly non-linear, and we cannot
# make assumptions about its shape. Since K = 9 in our model, there is likely low bias with some variance.




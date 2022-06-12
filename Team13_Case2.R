##Team 13 Case Study 2
##Team Members: Cam Doyle / Chenlin Cheng / Megan Brunick / Sebastian Rincon

library(lmtest)

#########################
#  Loading the dataset  #
#########################

#Clear out any variables in your environment
rm(list=ls())

#Read in the CSV data file and save to a variable called case2
case2 <- read.csv('Case2.csv')

#Examine the structure of the data - all variables are integers, except institutionName
str(case2)

#Use summary to produce descriptive statistics for all variables
#Take note of high number of NAs for PTRetentionRate, TenureTrack, and Tenured
summary(case2)

#View the first six rows of data to get a general understanding
head(case2)

#Use scatterplots to visualize relationships between different variables
#Our team looked at 5 independent variables that might be good predictors of FTRetentionRate
pairs(~ FTRetentionRate + PTRetentionRate + TotalEnrollment + FTEnrollment + TuitionAndFees + S2FRatio, data = case2)

#Attach the data set, so you don't have to keep typing it out
attach(case2)



#########################
#  Linear Regression 1  #
#########################

#Relationship between FTRetentionRate and PTRetentionRate
lmPTR <- lm(FTRetentionRate ~ PTRetentionRate)

#Produce a summary of the regression model
summary(lmPTR)
  #very small p-value = 2.2e-16, which means model is significant
  #Adj R-squared = .09106, which means 9.1% of variation in FTRetentionRate can be explained by PTRetentionRate
  #Residual standard error = 11.19, which is very small. This suggests that the model is a good fit.
  #Note: 1052 observations were deleted due to missing values

par(mfrow = c(1,1))
#Plot the relationship between the two variables in the model
plot(FTRetentionRate ~ PTRetentionRate)

#Plot the regression line
abline(lmPTR, col = "red")
  #appears to be a weak positive relationship with a slope of 0.147

#Save model coefficients to a new variable
coef_lmPTR <- coef(lmPTR)

#Print the regression equation
cat('Linear Regression 1: ', "FTRetentionRate = ", coef_lmPTR[1], "+ PTRetentionRate *", coef_lmPTR[2])


##Checking Assumptions

#Normality of Errors
hist(lmPTR$residuals)
mean(lmPTR$residuals)     
#Histogram of residuals is slightly left-skewed, with a very small residual mean of 1.431243e-15
#A mean of almost 0 and a relatively normal distribution suggests that the normality assumption is met.

#Test for Homoskedasticity
lmtest::bptest(lmPTR)
#p-value = 0.3533 > .05, so the assumption of homoskedasticity is met.

par(mfrow = c(2,2))
plot(lmPTR)
#Residuals vs Fitted plot: Since there is no obvious pattern in the residuals, the assumption of linearity is met.
#Normal Q-Q: The standardized residuals mostly fall along the line with a few potential outliers.
#Overall, these residuals plots do not indicate that any assumptions have been violated.


## Reasons why this variable matters:
#Part-time enrollment at a university is typically smaller than full-time enrollment,
  #so these students represent a subset of the total school population.
#Therefore, part-time retention rate could be a good predictor of full-time retention rate because
  #it is an indication of how well the school is operating to keep all its students engaged and enrolled.
#One might also expect full-time retention rate to be slightly higher than part-time retention rate
  #because part-time students may have jobs or other life commitments that prohibit them from continuing school.
  #This may contribute to the weak positive slope in the model.
#Part-time students may also become full-time students the next year, thus influencing future full-time retention rates.
  #Although this data only looks at the year 2020, it is important to consider how these two variables are connected over time.




#########################
#  Linear Regression 2  #
#########################

#Relationship between FTRetentionRate and FTEnrollment
lmFTE <- lm(FTRetentionRate ~ FTEnrollment)

#Produce a summary of the regression model
summary(lmFTE)
  #very small p-value = 2.2e-16, which means the model is significant
  #Adj R-squared = .07622, which means 7.62% of variation in FTRetentionRate can be explained by FTEnrollment (not very high)
  #Residual standard error = 12, which is small. This suggests that the model is a good fit.
  #Note: 354 observations were deleted due to missing values

par(mfrow = c(1,1))
#Plot the relationship between the two variables in the model
plot(FTRetentionRate ~ FTEnrollment)

#Plot the regression line
abline(lmFTE, col = "red")
  #appears to be a very weak positive relationship with a slope of 3.795e-04
  #the majority of universities have an enrollment of less than 50,000
  #most likely affected by multiple few outliers

#Save model coefficients to a new variable
coef_lmFTE <- coef(lmFTE)

#Print the regression equation
cat('Linear Regression 2: ', "FTRetentionRate = ", coef_lmFTE[1], "+ FTEnrollment *", coef_lmFTE[2])


##Checking Assumptions

#Normality of Errors
hist(lmFTE$residuals)
mean(lmFTE$residuals)
#Histogram of residuals is left skewed, which means the normality of errors may be violated.
#However, the mean of residuals is -2.806007e-15, which is desired for the normality assumption.

#Test for Homoskedasticity
lmtest::bptest(lmFTE)
#p-value = 1.214e-05 < .05, so it violates the homoskedasticity assumptions

par(mfrow = c(2,2))
plot(lmFTE)
#Residuals vs Fitted plot: Funnel-shaped plot of residual, so it violates the assumption of normality of errors.
#Normal Q-Q: The middle of residual points fits the line very well, but there are multiple significant outliers. 
#Based on these plots, the model likely has problems in normality and linearity. 


## Reasons why this variable matters:
#This relationship is weak and appears to have multiple concerning outliers, so it would be advantageous to 
  #remove these data points before making strong conclusions about the model.
#Still, this model provides some value because we know that the number of enrolled full-time students directly relates 
  #to the number of students that are eligible to return next year, which factors into full-time retention rate.
#It is important for a university to monitor how many students are continuing enrollment year over year
  #to ensure a high graduation rate.
#Additionally, universities with greater enrollment often have more resources, class options, and 
  #other opportunities, which could improve retention over smaller schools.
#All of these are probable explanations for the observed positive relationship between these two variables, but due to the 
  #violation of multiple assumptions, this model should be adjusted before any reliable predictions can be made with the data.




#########################
#  Linear Regression 3  #
#########################

#Relationship between FTRetentionRate and TuitionAndFees
lmTAF <- lm(FTRetentionRate ~ TuitionAndFees)

#Produce a summary of the regression model
summary(lmTAF)
  #p-value = 2.2e-16, which means model is significant
  #Adj R-squared = .09546, which means 9.546% of variation in FTRetentionRate can be explained by TuitionAndFees
  #Residual standard error = ?, which is very small. This suggests that the model is a good fit.
  #Note: 374 observations were deleted due to missing values

par(mfrow = c(1,1))
#Plot the relationship between the two variables in the model
plot(FTRetentionRate ~ TuitionAndFees)

#Plot the regression line
abline(lmTAF, col = "red")
  #appears to be a moderate positive relationship with a slope of 2.33e-04

#Save model coefficients to a new variable
coef_lmTAF <- coef(lmTAF)

#Print the regression equation
cat('Linear Regression 3: ', "FTRetentionRate = ", coef_lmTAF[1], "+ TuitionAndFees *", coef_lmTAF[2])


##Checking Assumptions

#Normality of Errors
hist(lmTAF$residuals)
mean(lmTAF$residuals)
#Histogram of residuals is slightly left skewed, and mean of residual is -1.324e-15

#Test for Homoskedasticity
lmtest::bptest(lmTAF)
#The p-value is 1.386e-12, so it violates the assumptions of homoskedasticity.

par(mfrow = c(2,2))
plot(lmTAF)
#Residuals vs Fitted plot: The residual points don't show obvious pattern and distributed randomly. So the model is linear. 
#Normal Q-Q: It appears normal with some outliers that do not appear to strongly influence the fitted values. 

## Reasons why this variable matters:
# Tuition & fees being moderately positively correlated with full-time retention rates is logical.
# If a higher number of students are retained, in conjunction with incoming students being
# added to the overall student body, the amount of money earned by the university will increase.
# Having more students being retained/enrolled could lead to increasing tuition & fees because more
# resources need to be made available for more students. 
# This variable is important for universities to monitor because higher tuition & fees can allow
# a university to have more resources, but a sharp increase could also impact retention or enrollment
# rates.



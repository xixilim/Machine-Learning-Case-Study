##################
#  Case Study 3  #
##################

#Team 13: Cam Doyle / Chenlin Cheng / Megan Brunick / Sebastian Rincon

####################
#  Load Libraries  #
####################
rm(list = ls())
library(tidyverse)
library(lmtest)

####################
# Load the Dataset #
####################

case3 <- read.csv("Case3.csv", stringsAsFactors = TRUE)
attach(case3)

str(case3)
summary(case3)
head(case3)

case3 <- na.omit(case3)  #remove NAs from data set

###################
#  Linear Model   #
###################

# Use step function to choose a model by AIC in a stepwise algorithm
lmcase3_step <- step(lm(SalePrice ~ ., data = case3), direction = 'both')
summary(lmcase3_step)
car::vif(lmcase3_step)

# The step function reduced the number of variables in the model from 65 to 34.
# Based on the summary and vif test, we deleted additional variables one-by-one 
# due to insignificance and/or multicollinearity.


# After removing some variables, this was our Linear Model 
lmcase3 <- lm(SalePrice ~ MSSubClass + LotArea + OverallQual + OverallCond + 
                YearBuilt + BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + 
                KitchenAbvGr + GarageCars + GarageArea + Fireplaces + LandContour + 
                LotConfig + LandSlope + RoofMatl + BsmtQual + BsmtExposure + 
                KitchenQual + SaleCondition, data = case3)

summary(lmcase3)
# The model has 21 independent variables (including category variables)
# Adjusted R-square = 0.8687, which means that 86.87% of variance in Sale Price can be
# explained by these 21 predictors.
# Next, we check assumptions to see if this model holds.


######################################
# Check Assumptions for Linear Model #
######################################

# Multicollinearity
car::vif(lmcase3)
# Looking at the last column, none of the variables exhibit multicollinearity.
# All GVIF^(1/(2*Df)) scores are less than 5.

# Normality of Errors
par(mfrow = c(1,1))
hist(lmcase3$residuals, breaks = 30)
# The residuals have a very normal distribution with a few strong outliers.
mean(lmcase3$residuals)
# Mean = -1.178511e-13 which is extremely close to 0, supporting the assumption of normality.

# Linearity Assumption
par(mfrow = c(2,2))
plot(lmcase3)
# Residuals vs Fitted plot: Doesn't show obvious pattern, mostly random cluster
# of residuals around 0 with a few outliers (points 430, 740, 967)

# Normal Q-Q plot: Residuals closely follow the line with slight curves at the extremes. Appears normal.
# Cook's distance: Point 430 is outside one of the dashed red threshold lines

# Homoskedasticity Assumption
lmtest::bptest(lmcase3)
# p-value < 2.2e-16, which is significantly less than .05
# Therefore, we reject the null hypothesis of homoskedasticity.


# Homoskedasticity assumption is violated, and multiple outliers are identified in the
# plots of the residuals.
# Start by transforming y in the model and removing any concerning observations 
# that are still present in the transformed model.


###################
# Transform Model #
###################

hist(case3$SalePrice)
# High right skewness in histogram of Sale Price
# Transforming SalePrice with a log(Y) model will make the most difference to 
# improve the model by linearizing the y values.

#Reload dataset - remove outliers - remove NAs
case3 <- read.csv("Case3.csv", stringsAsFactors = TRUE)
case3_log <- case3[-c(61, 77, 116, 145, 155, 213, 224, 258, 299, 340, 365, 377, 380, 384, 268, 
                      392, 430, 477, 543, 547, 552, 566, 562, 570, 590, 599, 615, 639, 663, 
                      670, 740, 747, 754, 719, 728, 731, 853, 868, 873, 880, 966, 967, 964, 
                      544, 568, 667, 447, 951, 993, 1043, 1066, 1082, 1102, 1168, 1131, 938), ]

summary(case3_log)
case3_log <- na.omit(case3_log)


## Final Model - Take the log of Sale Price ##
lmcase3_logy <- lm(log(SalePrice) ~ LotArea + OverallQual + OverallCond + 
                           YearBuilt + BsmtUnfSF + X1stFlrSF + X2ndFlrSF +
                           KitchenAbvGr + GarageArea + Fireplaces + LandSlope +
                           RoofMatl + BsmtQual + KitchenQual, data = case3_log)

options(scipen = 999)
summary(lmcase3_logy)

# After trial and error of removing extreme observations and removing variables,
# this model has 14 significant variables and satisfies all assumptions.
# p-value < 2.2e-16 means the entire model is significant.
# Adjusted R-squared =  0.9354, but cannot be interpreted in the same way since we
# took the log of y.


#################################################
# Check Assumptions for Final Transformed Model #
#################################################

# Multicollinearity - Satisfied
car::vif(lmcase3_logy)
# None of the variables show multicollinearity. All GVIF^(1/(2*Df)) scores are less than 5.
# Which means that the predictor variables are not linearly related.

# Normality of Errors - Satisfied
par(mfrow = c(1,1))
hist(lmcase3_logy$residuals, breaks = 40)
# Residuals have a very normal distribution, centered at 0
mean(lmcase3_logy$residuals)
# Mean = 4.224433e-18, which is essentially 0, which confirms normality.

# Linearity Assumption - Satisfied
par(mfrow = c(2,2))
plot(lmcase3_logy)

# The residual plot has a random pattern and the points are scattered around 0.
# The standardized residuals follow the line closely in the Normal Q-Q plot, which
# supports normality and linearity.

# Homoskedasticity Assumption - Satisfied
lmtest::bptest(lmcase3_logy)
# We removed about 50 observations that were noted as outliers in order to fix heteroskedasticity.
# p-value = 0.05633, which is greater than 0.05. The null hypothesis should not 
# be rejected, and we can conclude that the model is homoscedastic.


#############################
# Final Regression Equation #
#############################

# log(SalePrice) = 0.5805787643 + 0.0000075391LotArea + 0.0608648639OverallQual + 0.0557770810OverallCond + 0.0037104265YearBuilt - 
#               0.0000542148BsmtUnfSF + 0.0004510288X1stFlrSF +  0.0002756338X2ndFlrSF - 0.1089781116KitchenAbvGr + 
#               0.0001799793GarageArea + 0.0278603669Fireplaces + 0.0170033261LandSlopeMod - 0.2961978209LandSlopeSev +
#               2.9695670805RoofMatlCompShg + 3.2924913785RoofMatlMembran + 3.0405512725RoofMatlRoll +
#               2.7278960858RoofMatlTar_Grv + 2.9823078278RoofMatlWdShngl - 0.1117525050BsmtQualFa -
#               0.0663875932BsmtQualGd - 0.0850429953BsmtQualTA - 0.1079271716KitchenQualFa - 
#               0.0788803969KitchenQualGd - 0.1126374663KitchenQualTA


####################################
# Correlation of Exponential model #
####################################

attach(case3_log)

## Coefficients of model
Sig <- sigma(lmcase3_logy); Sig ##rse
b0 <- lmcase3_logy$coefficients[1]; b0 ## intercept 
b1 <- lmcase3_logy$coefficients[2]; b1 ## LotArea: Lot size in square feet
b2 <- lmcase3_logy$coefficients[3]; b2 ## OverallQual: Rates the overall material and finish of the house
b3 <- lmcase3_logy$coefficients[4]; b3 ## OverallCond: Rates the overall condition of the house
b4 <- lmcase3_logy$coefficients[5]; b4 ## YearBuilt: Original construction date
b5 <- lmcase3_logy$coefficients[6]; b5 ## BsmtUnfSF: Unfinished square feet of basement area
b6 <- lmcase3_logy$coefficients[7]; b6 ## X1stFLrSF: First Floor square feet
b7 <- lmcase3_logy$coefficients[8]; b7 ## X2ndFlrSF: Second floor square feet
b8 <- lmcase3_logy$coefficients[9]; b8 ## KitchenAbvGr: Kitchens above grade
b9 <- lmcase3_logy$coefficients[10]; b9 ## GarageArea: Size of garage in square feet
b10 <- lmcase3_logy$coefficients[11]; b10 ## Fireplaces: Number of fireplaces
b11 <- lmcase3_logy$coefficients[12]; b11 ## LandSlope: Slope of property - Mod
b12 <- lmcase3_logy$coefficients[13]; b12 ## LandSlope: Slope of property - Sev
b13 <- lmcase3_logy$coefficients[14]; b13 ## RoofMatl - CompShg
b14 <- lmcase3_logy$coefficients[15]; b14 ## RoofMatl - Membran
b15 <- lmcase3_logy$coefficients[16]; b15 ## RoofMatl - Roll
b16 <- lmcase3_logy$coefficients[17]; b16 ## RoofMatl - Tar&Grv
b17 <- lmcase3_logy$coefficients[18]; b17 ## RoofMatl - WdShngl
b18 <- lmcase3_logy$coefficients[19]; b18 ## BsmtQual - Fa
b19 <- lmcase3_logy$coefficients[20]; b19 ## BsmtQual - Gd
b20 <- lmcase3_logy$coefficients[21]; b20 ## BsmtQual - TA
b21 <- lmcase3_logy$coefficients[22]; b21 ## KitchenQual - Fa
b22 <- lmcase3_logy$coefficients[23]; b22 ## KitchenQual - Gd
b23 <- lmcase3_logy$coefficients[24]; b23 ## KitchenQual - TA


## Convert categorical variables to numeric variables.
LandSlopeMod <- ifelse(case3_log$LandSlope =="Mod", 1,0)
LandSlopeSev <- ifelse(case3_log$LandSlope =="Sev",1,0)
RoofMatlCompShg <- ifelse(case3_log$RoofMatl =="CompShg",1,0)
RoofMatlMembran <- ifelse(case3_log$RoofMatl =="Membran", 1,0)
RoofMatlRoll  <- ifelse(case3_log$RoofMatl =="Roll", 1,0)
RoofMatlTar_Grv <- ifelse(case3_log$RoofMatl =="Tar&Grv",1,0)
RoofMatlWdShngl <- ifelse(case3_log$RoofMatl =="WdShngl",1,0)
BsmtQualFa <- ifelse(case3_log$BsmtQual =="Fa", 1,0)
BsmtQualGd <- ifelse(case3_log$BsmtQual =="Gd", 1,0)
BsmtQualTA <- ifelse(case3_log$BsmtQual =="TA", 1,0)
KitchenQualFa <- ifelse(case3_log$KitchenQual =="Fa", 1,0)
KitchenQualGd <- ifelse(case3_log$KitchenQual =="Gd", 1,0)
KitchenQualTA <- ifelse(case3_log$KitchenQual =="TA", 1,0)


## Calculating y-hats because we took the log of Y. 
yhat <- exp(b0 + b1 * LotArea + b2 * OverallQual + b3 * OverallCond + b4 * YearBuilt + 
              b5 * BsmtUnfSF + b6 * X1stFlrSF + b7 * X2ndFlrSF + b8 * KitchenAbvGr + 
              b9 * GarageArea + b10 * Fireplaces + b11 * LandSlopeMod + b12 * LandSlopeSev +
              b13 * RoofMatlCompShg + b14 * RoofMatlMembran + b15 * RoofMatlRoll +
              b16 * RoofMatlTar_Grv + b17 * RoofMatlWdShngl + b18 * BsmtQualFa +
              b19 * BsmtQualGd + b20 * BsmtQualTA + b21 * KitchenQualFa + 
              b22 * KitchenQualGd + b23 * KitchenQualTA + Sig^2/2)
              

# correlation of log(SalePrice) and quality variables
cor(yhat, case3_log$SalePrice)^2
## Calculated R-Squared = 0.9396, which means that 93.96% of variation in Sale Price can
## be explained by these 14 predictors.

## Adjusted R-squared of Log(y) Model =  0.9354


##################
# Interpretation #
##################
lmcase3_logy$coefficients*100


# Quantitative Predictors
# 
# When Lot Area increases by 1 square foot, Sale Price increases by 0.000754%
# When Overall Quality increases by 1 rating, Sale Price increases by 6.0865%
# When Overall Condition increases by 1 rating, Sale Price increases by 5.5777%
# When Year Built increases by 1 year, Sale Price increases by 0.3710%
# When Unfinished Area of Basement increases by 1 square foot, Sale Price decreases by 0.00542%
# When Area of 1st Floor increases by 1 square foot, Sale Price increases by 0.0451%
# When Area of 2nd Floor increases by 1 square foot, Sale Price increases by 0.0276%
# When Kitchen Above Grade increases by 1 unit, Sale Price decreases by 10.8978%
# When GarageArea increases by 1 square foot, Sale Price increases by 0.0179%
# When the number of Fireplaces increases by 1, Sale Price increases by 2.7860%
# 
# Categorical Predictors
# When LandSlope changes from Gentle to Moderate, Sale Price increases by 1.7003%
# When LandSlope changes from Gentle to Severe, Sale Price decreases by 29.6198%
# When Roof Material changes from Clay/Tile to Composite Shingle, Sale Price increases by 296.9567%
# When Roof Material changes from Clay/Tile to Membrane, Sale Price increases by 329.2491%
# When Roof Material changes from Clay/Tile to Roll, Sale Price increases by 304.0551%
# When Roof Material changes from Clay/Tile to Gravel and Tar, Sale Price increases by 272.7896%
# When Roof Material changes from Clay/Tile to Wood Shingles, Sale Price increases by 298.2308%
# When height of Basement changes from Excellent to Fair, Sale Price decreases by 11.1753%
# When height of Basement changes from Excellent to Good, Sale Price decreases by 6.6388%
# When height of Basement changes from Excellent to Typical/Average, Sale Price decreases by 8.5043%
# When Kitchen Quality changes from Excellent to Fair, Sale Price decreases by 10.7927%
# When Kitchen Quality changes from Excellent to Good, Sale Price decreases by 7.8880%
# When Kitchen Quality changes from Excellent to Typical/Average, Sale Price decreases by 11.2638%
# 
# 
# Our model predicts the sale price of a house, considering fourteen (14) 
# important variables when choosing the right property. These features have a 
# significant impact on final house price since they are considered 
# fundamental for most buyers according to our study and personal experiences.
# 
# When developing our model, we focused on including the variables that statistically influenced
# sale price and also logically seemed relevant to American consumers purchasing a house.
# In developing our model, we used a linear regression model, conducted multiple tests for our
# assumptions, experimented with adding and dropping different variables, and transformed our
# model. All of this helped us to choose those variables that are statistically significant and 
# affect home sale price from a common-sense perspective. This model is important to home buyers,
# sellers, and other stakeholders in the real estate market because it gives us a sense of what 
# aspects of a home provide the most monetary value.
# 
# We have divided our interpretation of the model into three main categories:
# 
# 1 - The size of the land/property results in a higher sale price. 
# 
# In this category, we included those features that are related to how spacious 
# the house is in terms of construction and land plot: Lot size, first-floor size, 
# second-floor size, garage size, the height of the basement, and the unfinished basement area.  
# All these variables are positively correlated to sale price, except for the unfinished basement 
# area, which will decrease the final price since a buyer would have to spend extra money to finish
# the basement.
# 
# 2- Higher quality features result in higher sale price. 
# 
# In this category, we focused on features related to the quality of materials 
# used to build the house and its current condition. The variables we included 
# in this category are overall quality of materials and finish of the house, roof 
# material, kitchen quality, and the condition of the house at the time of sale. Also, we 
# decided to include fireplaces since it is an additional feature that increases the price. 
# So, as the overall condition of the house and the quality of materials goes up, the price 
# will increase as well. We wanted to make an important note related to the Roof Material 
# variable. Based on our interpretation of the coefficients, when Roof Material changes from Clay/Tile 
# to another material, Sale Price increases by 200-300%. Conceptually, this seems unrealistic, 
# so we explored the data and realized that R chose Clay/Tile as our referent category, and there is 
# only one observation in the case3_log dataset that has Clay/Tile roof material. This clearly skewed
# the coefficients for our Roof Material categorical variables. We decided to keep Roof Material
# in our model because they are still all significant and our personal assumptions tell us
# that the quality and structure of roof material matters to the value of a house.
# Nonetheless, we wanted to mention this to proceed with caution when only looking at
# this variable. We may need more observations for all the Roof Material categories that 
# are not Composite Shingle.
# 
# 3- Others Predictors 
# 
# The remaining predictors are the year the house was built and the slope of the land on which
# the property was built. The more recent a house was built, the higher the selling price because
# a newer house typically has less problems and more updated features. A Moderate Land Slope has a 
# small positive effect on prices. However, when Land Slope changes from Gentle to Severe, there 
# is a strong negative effect on Sale Price, likely because a very hilly property is difficult to 
# park on, do yard work, for kids to play, among other reasons.
# 
# Overall, we believe all of the variables in our model are strong predictors of home sale price.

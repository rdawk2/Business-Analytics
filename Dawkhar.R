#Read the data file
library(readr)
df <- read_csv("/Users/rujutadawhkar/Desktop/Tutorial R dataset/LOGISTIC_4.csv")

#View the loaded data frame
View(df)

#check datatypes of columns
sapply(df, class)

#change datatype of Donor, Married to Factor (Categorical)
df$DONOR <- as.factor(df$DONOR)
df$MARRIED <- as.factor(df$MARRIED)

#recheck if datatype changed
sapply(df, class)

# Convert categorical variables Gender and Event Attended in binary values
df$GENDER <- factor(df$GENDER, levels = c("Female","Male"), 
                             labels = c(0,1))
df$EVENT_ATTENDED <- factor(df$EVENT_ATTENDED, levels = c("Yes","No"), 
                    labels = c(1,0))
View(df)


#model w/o test train
model1 <- glm(DONOR ~  AGE + GENDER + MARRIED + EVENT_ATTENDED,
              data = df,
              family = binomial())

summary(model1)

# Deviance Statistic

# The deviance statistics determine the overall fit of a logistic regression model.
# Larger values of the deviance means models are badly fitted.
# Deviance is a measure of goodness of fit of a model. 
# Higher numbers always indicates bad fit.

# The Null deviance which is 2772.6, is the deviance of the model with no predictors.
# There should be a decrease in size in the difference between the NULL and residual deviance. 

# In our case the Residual Deviance is 2449.0 The decrease in size indicates that 
# the model is better at predicitng whether someone was a donor after we add the predictor
# variables (the model is predicting the outcome variable more accurately.)


# Model Chi-Square Statistic

# The chi-Square model can be used to determine how much better model is at predicting an
# outcome with the predictors added than with only the constant. 

# To get the chi-square statistic subtract the deviance from the null deviance

model1_chi <- model1$null.deviance - model1$deviance

model1_chi

#-----X2 = 323.5876

# Get the difference in the degrees of freedom between the model and the null model:

model1_chi_df <- model1$df.null - model1$df.residual 

model1_chi_df

#This value comes out to be 4 and is equal to the number of variables in the model

#Calculate the probability associated with the Chi-Square Statistic:

model1_chisq.prob <- 1 - pchisq(model1_chi, model1_chi_df)

model1_chisq.prob

# We get a value of 0 that means the p value = 0 ; This probability is < 0.05
# we can reject the null hypothesis that the model is not better than chance at 
# predicting the outcome.

# It means that adding the predictor variables produced a significant 
# improvement in the fit of the model.


# Check Coefficients

# Check the estimate 'Estimate' column. 

# In this case Age, Gender and Married variables are significant as the p value 
# for them is < 0.05 and make a contribution to the model
# Gender has p value of 0.5121 > 0.05 therefore it is not significant

# AGE              0.012471   0.002634   4.735 2.19e-06 ***
# GENDER1          0.077789   0.118655   0.656   0.5121    
# MARRIED1         1.379134   0.099198  13.903  < 2e-16 ***
# EVENT_ATTENDED0 -1.031602   0.137780  -7.487 7.03e-14 ***


# Pseudo-R2

logistic_r2 <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev /nullDev
  R.cs <- 1 - exp(-(nullDev - dev)/modelN)
  R.n <- R.cs / (1 - (exp(-(nullDev/modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l,3), "\n")
  cat("Cox and Snell R^2       ", round(R.cs,3), "\n")
  cat("Nagelkerke R^2          ", round(R.n,3), "\n")
}

logistic_r2(model1) 

# Pseudo R^2 for logistic regression
# Hosmer and Lemeshow R^2  0.117 
# Cox and Snell R^2        0.149 
# Nagelkerke R^2           0.199 
# All three measures show that the effect size of the model is small - 
# which means that we can't explain much of the variability in the outcome with just the
# variables we have. 
# There can be factors other than these which will affect if a person becomes a donor or not.


# The Odds Ratio

# Odds ratio greater than 1 - means that as the predictor increases, the odds of the 
# outcome occurence increase.

# Odds ratio value less than 1 - means that as the predictor increases, 
# the odds of the outcome occuring decrease.

exp(model1$coefficients)

#(Intercept)             AGE         GENDER1        MARRIED1  EVENT_ATTENDED0 
#0.6487178       1.0125492       1.0808945       3.9714608       0.3564356 

# So, for every 1 yr increase in a Age, the odds of their being a donor increase
# by 1.0125492 times.

# when the odds ratio is greater than 1, it describes a positive relationship. 

# The positive relationship means that as gender “increases,” 
# the odds of being a donor increases. 
# Based on our coding, an “increase” in gender means a gender of 1 instead of 0
# in other words, being male
# This can be interpreted to mean that being in the (1) group, or being male, 
# puts you at 1.0808945 times greater odds of being a donor.

# An odds ratio less than 1 implies a negative relationship. 
# This means that people who did not attend event were at lesser odds of being a donor, 
# as the odds ratio is less than 1.

# Confidence Intervals

exp(confint(model1))

#2.5 %    97.5 %
#  (Intercept)     0.4336011 0.9687872
# AGE             1.0073751 1.0178372
# GENDER1         0.8565408 1.3640919
# MARRIED1        3.2732224 4.8294094
# EVENT_ATTENDED0 0.2711492 0.4655612

# The fact that both the lower
# and upper limits of our confidence interval are above 1 for Age and Married variable
# gives us confidence that the
# direction of the relationship(positive) that we have observed is true in the population. 

# Values less than 1 mean the opposite: as the predictor
# variable increases, the odds of being donor decrease which is the case when a person does
# not attend an event (EVENT_ATTENDED0)

# The lower limit is below 1 for Gender variable it tells us that there is a chance
# that in the population the direction of the relationship is the opposite to what we
# have observed. This would mean that we could not trust that 
# being male, 
# puts you at 1.0808945 times greater odds of being a donor.


 -------------

# Checking Residuals

# It is important to check for outliers in residuals

library(car)
library(mlogit)
library(readr)
  
df$predicted_probabilities <- fitted(model1)  

df$standardized_residuals <- rstandard(model1)
df$studentized_residuals <- rstudent(model1)
df$dfbeta <- dfbeta(model1)
df$dffit <- dffits(model1)
df$leverage <- hatvalues(model1)


View(df)


# We need 95% of our cases to have standardized residuals within plus or minus 2, 
# in a set of 2000 cases we would expect 
# roughly 1900 to be around 2, or roughly 100 cases outside the limit.

df$standardized_residuals > 2 | df$standardized_residuals < -2

# Store this data in dataframe:

df$large_residual <- df$standardized_residuals > 2 | df$standardized_residuals < -2

View(df)

sum(df$large_residual)

# We have 8 residuals/outliers here

df[df$large_residual,c("AGE","GENDER","MARRIED","EVENT_ATTENDED","standardized_residuals")]

# Here standardized_residuals value for all outliers is nearly = -2 and we have only 8 residuals
# out of 2000 cases
# So this is good.

# DFBeta should be less than 1.
# Here going to df and sorting it, the higest entry found was 0.02830551
# So this value is < 1 and is good
# -------------

# Testing for multicollinearity

# If the largest VIF is greater than 10 then there is a problem.
# If the average VIF is substantially greater than 1 then there may be bias in the model.
# If the the tolerence is below .1 that indicates a serious problem.
# Tolerence below .2 indicates a potential problem.

vif(model1)
1/vif(model1) # Tolerence
mean(vif(model1)) # Average

#> vif(model1)
#AGE            GENDER        MARRIED        EVENT_ATTENDED 
#1.012496       1.009898       1.000323       1.003367 

#> 1/vif(model1) # Tolerence
#AGE            GENDER        MARRIED        EVENT_ATTENDED 
#0.9876583      0.9901991      0.9996769      0.9966440 

#> mean(vif(model1)) # Average
#[1] 1.006521

# Average VIF is 1.006521 which is good and in limit
# Tolearnce is also not below 0.1 or 0.2 so this is good.
# Largest VIF is also not greater than 1 so this is good.

# Hence we can say that it doesn't look like there is any collinearity in our model.

# Linearity of the Logit

# First we need to create the variables which we will use for the test:
# We include Age as it is not a factor.

df$logAGE <- log(df$AGE)*df$AGE

View(df)

model1_test <- glm(DONOR ~ AGE  + logAGE,
                   data = df,
                   family = binomial())

summary(model1_test)

#Coefficients:
#             Estimate   Std. Error z value Pr(>|z|)
# (Intercept)  0.15942    1.00061   0.159    0.873
# AGE         -0.11494    0.08775  -1.310    0.190
# logAGE       0.02733    0.01731   1.579    0.114

# In other words we don't want interactions to be significant (p value > 0.05).
# p value for Age and logAge are >0.05 so it looks good.

# Additional Part
# Build model using test train and find out Accuracy and Confusion Matrix
#drop constituent ID column
library(readr)
df <- read_csv("/Users/rujutadawhkar/Desktop/Tutorial R dataset/LOGISTIC_4.csv")
View(df)

# Repeat steps to create factor variables
#check datatypes of columns
sapply(df, class)

#change datatype of Donor, Married to Factor (Categorical)
df$DONOR <- as.factor(df$DONOR)
df$MARRIED <- as.factor(df$MARRIED)

#recheck if datatype changed
sapply(df, class)

# Convert categorical variables Gender and Event Attended in binary values
df$GENDER <- factor(df$GENDER, levels = c("Female","Male"), 
                    labels = c(0,1))
df$EVENT_ATTENDED <- factor(df$EVENT_ATTENDED, levels = c("Yes","No"), 
                            labels = c(1,0))
View(df)

#create test train data
#Train and test set creation with a split ratio of 2/3
library(caTools)
set.seed(123)
split<-sample.split(df$DONOR, SplitRatio = 2/3) #returns true or false
split
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

#Feature Scaling for Age variable (exclude cat variables)
training_set[,1] = scale(training_set[,1])
test_set[,1] = scale(test_set[,1])

#Fit model to train set
model1 <- glm(formula = DONOR ~ ., 
              data = training_set,
              family = binomial())

#Predicting test set results 
prob_pred = predict(model1, type= 'response', newdata=test_set[-5])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#confusion matrix
cm = table(unlist(test_set[, 5]), y_pred)
cm

# y_pred
#     0   1
# 0 236  97
# 1 117 216

#Accuracy
accuracy <- sum(diag(cm))/sum(cm)
accuracy
# [1] 0.6786787
# Accuracy = 67.86%
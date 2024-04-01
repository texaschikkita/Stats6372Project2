# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)
library(car) # For VIF
library(effects) # For effect plots
library(lmtest) # For coeftest
library(sandwich) # For robust standard errors


# Quick data overview
str(glow_bonemed)
summary(glow_bonemed)

# Check for missing values
sum(is.na(glow_bonemed))

# Exploratory Data Analysis (EDA) with visualizations
# Age vs Fracture
ggplot(glow_bonemed, aes(x = age, y = fracture, color = fracture)) + 
  geom_jitter() + 
  theme_minimal() + 
  ggtitle("Age vs Fracture")

# BMI vs Fracture
ggplot(glow_bonemed, aes(x = bmi, y = fracture, color = fracture)) + 
  geom_jitter() + 
  theme_minimal() + 
  ggtitle("BMI vs Fracture")

# Summary of numeric variables
summary(select(glow_bonemed, age, weight, height, bmi, fracscore))

# Summary of categorical variables
summary(select(glow_bonemed, priorfrac, premeno, momfrac, armassist, smoke, raterisk, fracture, bonemed, bonemed_fu, bonetreat))

# Logistic Regression Modeling
# Simple Model for Interpretability
model <- glm(fracture ~ age + bmi, data = glow_bonemed, family = binomial())
summary(model)

# Model with Additional Predictors
model2 <- glm(fracture ~ age + bmi + priorfrac + smoke + raterisk, data = glow_bonemed, family = binomial())
summary(model2)

# Odds Ratios and Confidence Intervals
exp(coef(model))
exp(confint(model))

# Odds Ratios and Confidence Intervals model 2
exp(coef(model2))
exp(confint(model2))

# Model Performance Evaluation
roc_curve <- roc(response = glow_bonemed$fracture, predictor = fitted(model))
plot(roc_curve, main="ROC Curve for Logistic Regression Model")
auc(roc_curve)

# Model Validation
set.seed(123) # For reproducibility
training_index <- createDataPartition(glow_bonemed$fracture, p = 0.8, list = FALSE)
training_data <- glow_bonemed[training_index, ]
testing_data <- glow_bonemed[-training_index, ]

# Model Training
model_train <- glm(fracture ~ age + bmi, data = training_data, family = binomial())
summary(model_train)

# Model Testing
predictions <- predict(model_train, newdata = testing_data, type = "response")
roc_curve_test <- roc(response = testing_data$fracture, predictor = predictions)
plot(roc_curve_test, main="ROC Curve for Logistic Regression on Test Data")
auc(roc_curve_test)

# Deeper EDA
ggplot(glow_bonemed, aes(x = age, y = fracture, color = bonemed)) + 
  geom_jitter() + 
  theme_minimal() + 
  ggtitle("Age vs Fracture with Treatment Overlay")

# Univariate Feature Selection
model_age <- glm(fracture ~ age, data = glow_bonemed, family = binomial())
summary(model_age)

# Diagnostic Plots for Model Assumptions
# Residuals vs Fitted
plot(fitted(model_train), residuals(model_train), xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

# Normal Q-Q Plot
qqnorm(residuals(model_train))
qqline(residuals(model_train), col="red")

# Scale-Location Plot
plot(fitted(model_train), sqrt(abs(residuals(model_train))), xlab="Fitted Values", ylab="Sqrt(|Residuals|)", main="Scale-Location Plot")
abline(h = 0, col="red")

# Variance Inflation Factor (VIF) Check
vif(model)

# Effect Plots
plot(allEffects(model))

# Robust Standard Errors
model_robust <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
print(model_robust)

# Feature Engineering
# Creating a new feature based on existing data
glow_bonemed$age_group <- cut(glow_bonemed$age, breaks=c(50,60,70,80,90), include.lowest=TRUE, right=FALSE)

# Additional Model with Age Group
model_with_age_group <- glm(fracture ~ age_group + bmi + priorfrac + smoke + raterisk, data = glow_bonemed, family = binomial())
summary(model_with_age_group)

# Hypothesis Testing and Confidence Intervals for Each Coefficient
hypothesis_test_results <- coef_table[, "Pr(>|z|)"] < 0.05
print(hypothesis_test_results)
exp_confint <- exp(confint(model))
print(exp_confint)

# Comment on the practical significance of statistically significant factors


# Compare Model 1 to model 2:
# AIC comparison
aic_model1 <- AIC(model)
aic_model2 <- AIC(model2)
print(paste("AIC for Model 1:", aic_model1))
print(paste("AIC for Model 2:", aic_model2))

# BIC comparison
bic_model1 <- BIC(model)
bic_model2 <- BIC(model2)
print(paste("BIC for Model 1:", bic_model1))
print(paste("BIC for Model 2:", bic_model2))

# Likelihood Ratio Test
lrt <- anova(model, model2, test="Chisq")
print(lrt)

# AUC comparison
# Assuming 'roc_curve' is for Model 1 and 'roc_curve_test' is for Model 2
auc_model1 <- auc(roc(response = glow_bonemed$fracture, predictor = fitted(model)))
auc_model2 <- auc(roc(response = glow_bonemed$fracture, predictor = fitted(model2)))

print(paste("AUC for Model 1:", auc_model1))
print(paste("AUC for Model 2:", auc_model2))

# Comment on which model performs better
# 2   - though it has slightly higher BIC than mofel 1, it's got a better (lower aic ands a higher roc)

# Comment on the practical significance of statistically significant factors

# working with model 2:
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)
library(car) # For VIF
library(effects) # For effect plots
library(lmtest) # For coeftest
library(sandwich) # For robust standard errors

# Quick data overview
str(glow_bonemed)
summary(glow_bonemed)

# Check for missing values
sum(is.na(glow_bonemed))

# Exploratory Data Analysis (EDA) with visualizations
# Age vs Fracture
ggplot(glow_bonemed, aes(x = age, y = fracture, color = fracture)) + 
  geom_jitter() + 
  theme_minimal() + 
  ggtitle("Age vs Fracture")

# BMI vs Fracture
ggplot(glow_bonemed, aes(x = bmi, y = fracture, color = fracture)) + 
  geom_jitter() + 
  theme_minimal() + 
  ggtitle("BMI vs Fracture")

# Summary of numeric variables
summary(select(glow_bonemed, age, weight, height, bmi, fracscore))

# Summary of categorical variables
summary(select(glow_bonemed, priorfrac, premeno, momfrac, armassist, smoke, raterisk, fracture, bonemed, bonemed_fu, bonetreat))

# Logistic Regression Modeling with Additional Predictors
model2 <- glm(fracture ~ age + bmi + priorfrac + smoke + raterisk, data = glow_bonemed, family = binomial())
summary(model2)

# Odds Ratios and Confidence Intervals
exp(coef(model2))
exp(confint(model2))

# Model Performance Evaluation
roc_curve2 <- roc(response = glow_bonemed$fracture, predictor = fitted(model2))
plot(roc_curve2, main="ROC Curve for Logistic Regression Model 2")
auc_model2 <- auc(roc_curve2)
print(paste("AUC for Model 2:", auc_model2))

# Model Validation using Training and Testing sets
set.seed(123) # For reproducibility
training_index <- createDataPartition(glow_bonemed$fracture, p = 0.8, list = FALSE)
training_data <- glow_bonemed[training_index, ]
testing_data <- glow_bonemed[-training_index, ]

# Rebuilding Model 2 with Training Data
model2_train <- glm(fracture ~ age + bmi + priorfrac + smoke + raterisk, data = training_data, family = binomial())
summary(model2_train)

# Testing Model 2 with Testing Data
predictions2 <- predict(model2_train, newdata = testing_data, type = "response")
roc_curve_test2 <- roc(response = testing_data$fracture, predictor = predictions2)
plot(roc_curve_test2, main="ROC Curve for Logistic Regression Model 2 on Test Data")
auc_model2_test <- auc(roc_curve_test2)
print(paste("AUC for Model 2 on Testing Data:", auc_model2_test))

# Diagnostic Plots for Model 2 Assumptions
plot(fitted(model2_train), residuals(model2_train), xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

qqnorm(residuals(model2_train))
qqline(residuals(model2_train), col="red")

plot(fitted(model2_train), sqrt(abs(residuals(model2_train))), xlab="Fitted Values", ylab="Sqrt(|Residuals|)", main="Scale-Location Plot")
abline(h = 0, col="red")

# Variance Inflation Factor (VIF) Check for Model 2
vif(model2)

# Effect Plots for Model 2
plot(allEffects(model2))

# Robust Standard Errors for Model 2
model2_robust <- coeftest(model2, vcov = vcovHC(model2, type = "HC1"))
print(model2_robust)

# Hypothesis Testing and Confidence Intervals for Each Coefficient of Model 2
hypothesis_test_results2 <- summary(model2)$coefficients[, "Pr(>|z|)"] < 0.05
print(hypothesis_test_results2)
exp_confint2 <- exp(confint(model2))
print(exp_confint2)

# Comment on the practical significance of statistically significant factors from Model 2
# ( interpretation here)

# AIC and BIC for Model 2
aic_model2 <- AIC(model2)
bic_model2 <- BIC(model2)
print(paste("AIC for Model 2:", aic_model2))
print(paste("BIC for Model 2:", bic_model2))

# Comment on Model 2 performance and compare with results of previous models if necessary
# (interpretation here)

# Comment on which model performs better based on AUC and other metrics
# model 2 auc drops alot when applied to rtesting data  indicating overfitting - it may nopt play nice w new unforseen data

# further model testing

# AUC Comparison for Generalizability
# AUC for Model 1 on Training Data
roc_curve_train1 <- roc(response = training_data$fracture, predictor = fitted(model_train))
auc_train1 <- auc(roc_curve_train1)

# AUC for Model 2 on Training Data
roc_curve_train2 <- roc(response = training_data$fracture, predictor = fitted(model2_train))
auc_train2 <- auc(roc_curve_train2)

# Comparing AUCs to evaluate generalizability
print(paste("Training AUC Model 1:", auc_train1, "Testing AUC Model 1:", auc_model1))
print(paste("Training AUC Model 2:", auc_train2, "Testing AUC Model 2:", auc_model2_test))

# stability and robustness evaluation: # VIF for Model 2 (Already done in your code)
print(vif(model2))

# Robust Standard Errors for Model 2 (Already done in your code)
model2_robust <- coeftest(model2, vcov = vcovHC(model2, type = "HC1"))
print(model2_robust)


# model comparison on AIC and BIC:
# Already done in your code
print(paste("AIC Model 1:", aic_model1, "vs. AIC Model 2:", aic_model2))
print(paste("BIC Model 1:", bic_model1, "vs. BIC Model 2:", bic_model2))

# model 1` is better given all factors - unless efforts are nade to mitgate overfitting`


# retesting
library(boot)

# Generalizability Test: AUC on Testing Data for both models
auc_model1_test <- auc(roc(response = testing_data$fracture, predictor = predict(model, newdata = testing_data, type = "response")))
auc_model2_test <- auc(roc(response = testing_data$fracture, predictor = predict(model2, newdata = testing_data, type = "response")))

print(paste("AUC for Model 1 on Testing Data:", auc_model1_test))
print(paste("AUC for Model 2 on Testing Data:", auc_model2_test))

# Simplicity Assessment: Number of Predictors
num_predictors_model1 <- length(coef(model))
num_predictors_model2 <- length(coef(model2))

print(paste("Number of predictors in Model 1:", num_predictors_model1))
print(paste("Number of predictors in Model 2:", num_predictors_model2))

# Clinical or Practical Significance: This would be more of a discussion based on the predictors used in each model.

# Stability and Robustness: Could use cross-validation as an example
cv_model1 <- cv.glm(glow_bonemed, model, K = 10) # Requires boot package for cv.glm
cv_model2 <- cv.glm(glow_bonemed, model2, K = 10)

print(paste("CV Error for Model 1:", cv_model1$delta[1]))
print(paste("CV Error for Model 2:", cv_model2$delta[1]))

# Direct Comparison of Performance Metrics
# AIC and BIC have already been calculated. For the Likelihood Ratio Test:
lrt_result <- anova(model, model2, test = "Chisq")

# Display the results
print(lrt_result)


# Cross-validation for Model 1
cv_model1 <- cv.glm(glow_bonemed, model, K = 10)

# Cross-validation for Model 2
cv_model2 <- cv.glm(glow_bonemed, model2, K = 10)

# Print the results
print(paste("CV Error for Model 1:", cv_model1$delta[1]))
print(paste("CV Error for Model 2:", cv_model2$delta[1]))


# Commenting on model comparison based on new insights
# model 2 is more complex, requireing regularizaztion techniques - Caleb, Rafia what do you think?


# provide narrative insights based on the outputs of the above comparisons, focusing on generalizability, simplicity, clinical significance, stability, and direct metric comparison.

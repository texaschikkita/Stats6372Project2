# INITIAL WORKING/EDA/OBJECTIVE 1:



# Load libraries and data
library(aplore3)
library(boot)
library(car)
library(caret)
library(caretEnsemble)
library(caTools)
library(class)
library(ComplexHeatmap)
library(DescTools)
library(dplyr)
library(doParallel)
library(effects)
library(fmsb)
library(ggplot2)
library(ggvis)
library(GGally)
library(ggcorrplot)
library(glmnet)
library(gt)
library(htmlwidgets)
library(kknn)
library(knitr)
library(lattice)
library(lime)
library(lmtest)
library(MASS)
library(memoise)
library(patchwork)
library(pheatmap)
library(plotly)
library(pROC)
library(PRROC)
library(randomForest)
library(ranger)
library(reshape2)
library(RColorBrewer)
library(rgl)
library(ROSE)
library(rmarkdown)
library(shapr)
library(sandwich)
library(smotefamily)
library(stats)
library(tibble)
library(tidyr)
library(tidyverse)
library(xgboost)

data("glow_bonemed")
head(glow_bonemed)
summary(glow_bonemed)
sum(is.na(glow_bonemed))





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






# RAFIA


# Histograms for numeric variables
variable_list <- c("age", "weight", "height", "bmi", "fracscore", "phy_id")
color_list <- c("red", "orange", "yellow", "green", "blue", "purple")
for (i in seq_along(variable_list)) {
  p <- ggplot(glow_bonemed, aes_string(x = variable_list[i])) +
    geom_histogram(binwidth = 5, fill = color_list[i], color = "black") +
    theme_minimal() +
    ggtitle(paste("Distribution of", variable_list[i]))
  print(p)
}

# Bar Charts for Categorical Variables
categorical_vars <- c("priorfrac", "premeno", "momfrac", "armassist", "smoke", "fracture", "raterisk")
cat_data_long <- glow_bonemed %>%
  select(all_of(categorical_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
ggplot(cat_data_long, aes(x = value, fill = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Variables", x = NULL, y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation matrix for non-categorical variables
cor_matrix <- glow_bonemed %>%
  select(age, height, weight) %>%
  na.omit() %>%
  cor()
ggcorrplot(corr = cor_matrix)

# Multicollinearity check
set.seed(123)
trainIndex <- createDataPartition(glow_bonemed$fracture, p = .8, list = FALSE)
trainData <- glow_bonemed[trainIndex, ]
validationData <- glow_bonemed[-trainIndex, ]
vif_model <- glm(fracture ~ age + weight + height + bmi + premeno + momfrac + armassist + smoke + raterisk + fracscore + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
vif(vif_model)
vif_model_updated <- glm(fracture ~ age + height + weight + premeno + momfrac + armassist + smoke + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
vif(vif_model_updated)

# Model building: Logistic Regression
log.model <- glm(fracture ~ age + height + weight + premeno + momfrac + armassist + smoke + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
evaluate_model_glm <- function(model, data, actual) {
  predicted_probs <- predict(model, newdata = data, type = "response")
  predicted_class <- ifelse(predicted_probs > 0.5, "Yes", "No")
  predicted_class <- factor(predicted_class, levels = c("No", "Yes"))
  actual <- factor(actual, levels = c("No", "Yes"))
  if(all(levels(predicted_class) %in% levels(actual))) {
    cm <- confusionMatrix(predicted_class, actual)
  } else {
    cm <- list(Error = "Not all factor levels are present in both predicted and actual data.")
  }
  return(cm)
}
actual_outcomes <- trainData$fracture
cm <- evaluate_model_glm(log.model, trainData, actual_outcomes)
print(cm)
summary(log.model)
exp(coef(log.model))
confint(log.model, level = 0.95)
vif_model_1 <- glm(fracture ~ age + height + weight + premeno + momfrac + armassist + smoke + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
vif(vif_model_1)

# Advanced modeling: Ridge, LASSO, and Elastic Net
x_var <- model.matrix(~ bonemed + bonemed_fu + bonetreat + bmi + fracscore, trainData)
y_var <- trainData$fracture
ridge_model <- glmnet(x_var, y_var, family = "binomial", alpha = 0)
print(ridge_model)
lasso_model <- glmnet(x_var, y_var, family = "binomial", alpha = 1)
print(lasso_model)
elastic_model <- cv.glmnet(x_var, y_var, family = "binomial", alpha = 0.5)
print(elastic_model)
plot(elastic_model)










# CALEB

# Load and inspect the data
data("glow_bonemed")
head(glow_bonemed)
summary(glow_bonemed)
sum(is.na(glow_bonemed))

# Histograms for numeric variables
ggplot(glow_bonemed, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  theme_minimal()
ggplot(glow_bonemed, aes(x = weight)) + 
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  theme_minimal()
ggplot(glow_bonemed, aes(x = height)) + 
  geom_histogram(binwidth = 5, fill = "yellow", color = "black") +
  theme_minimal()
ggplot(glow_bonemed, aes(x = bmi)) + 
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  theme_minimal()
ggplot(glow_bonemed, aes(x = fracscore)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal()
ggplot(glow_bonemed, aes(x = phy_id)) + 
  geom_histogram(binwidth = 5, fill = "purple", color = "black") +
  theme_minimal()

# Bar plots for categorical variables
ggplot(glow_bonemed, aes(x = priorfrac)) + geom_bar(fill = "red") + theme_minimal()
ggplot(glow_bonemed, aes(x = premeno)) + geom_bar(fill = "orange") + theme_minimal()
ggplot(glow_bonemed, aes(x = momfrac)) + geom_bar(fill = "yellow") + theme_minimal()
ggplot(glow_bonemed, aes(x = armassist)) + geom_bar(fill = "green") + theme_minimal()
ggplot(glow_bonemed, aes(x = smoke)) + geom_bar(fill = "blue") + theme_minimal()
ggplot(glow_bonemed, aes(x = fracture)) + geom_bar(fill = "purple") + theme_minimal()
ggplot(glow_bonemed, aes(x = raterisk)) + geom_bar(fill = "black") + theme_minimal()

# Data partitioning
set.seed(123)
trainIndex <- createDataPartition(glow_bonemed$fracture, p = .8, list = FALSE, times = 1)
trainData <- glow_bonemed[trainIndex, ]
validationData <- glow_bonemed[-trainIndex, ]

# Data summary
summary(trainData)
str(trainData)
table(trainData$fracture)

# EDA: VIF calculation
vif_model <- glm(fracture ~ age + weight + height + bmi + premeno + momfrac + armassist + smoke + raterisk + fracscore + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
vif(vif_model)
vif_model <- glm(fracture ~ age + height + weight + premeno + momfrac + armassist + smoke + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
vif(vif_model)

# Logistic regression models
log.model <- glm(fracture ~ age + height + weight + premeno + momfrac + armassist + smoke + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
cat("Log Model Summary:\n")
summary(log.model)
cat("Log Model Odds Ratios:\n")
exp(coef(log.model))
cat("\nLog Model Confidence Interval:\n")
confint(log.model)
log.model2 <- glm(fracture ~ age + height + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
cat("Log Model 2 Summary:\n")
summary(log.model2)
cat("Log Model 2 Odds Ratio:\n")
exp(coef(log.model2))
cat("\nLog Model 2 Confidence Interval:\n")
confint(log.model2)

# Advanced modeling: Ridge, LASSO, and Elastic Net
x_var <- data.matrix(trainData[, c("bonemed", "bonemed_fu", "bonetreat", "bmi", "fracscore")])
y_var <- trainData[, "fracture"]
ridge_model <- glmnet(x_var, y_var, family = "binomial", alpha = 0)
cat("\nRidge Regression Model Summary:\n")
print(ridge_model)
lasso_model <- glmnet(x_var, y_var, family = "binomial", alpha = 1)
cat("\nLASSO Regression Model Summary:\n")
print(lasso_model)
Elastic_model <- cv.glmnet(x_var, y_var, family="binomial", alpha=.5)
cat("\nElastic Net Regression Model Summary:\n")
print(Elastic_model)
plot(Elastic_model)









# JESSICA

# Load the dataset
data("glow_bonemed")
# Drop the specified columns
glow_bonemed <- glow_bonemed[, !(names(glow_bonemed) %in% c("sub_id", "site_id", "phy_id"))]
# Check the structure of the modified dataset
str(glow_bonemed)


# work with numeric predictors when calculating VIF
numeric_data <- data.frame(lapply(glow_bonemed, function(x) if(is.factor(x)) as.numeric(as.factor(x)) else x))

# Fit a linear model using all predictors
model <- lm(fracture ~ ., data = numeric_data)

# Calculate VIF
vif_values <- vif(model)
print(vif_values)

# Filter high VIF values, if any, to identify multicollinear variables
high_vif <- vif_values[vif_values > 5]  # Change 5 to 10 if you prefer a higher threshold
print(high_vif)

# standardize to address multicollinearity

# Standardize numeric variables
glow_bonemed_scaled <- as.data.frame(scale(glow_bonemed[, sapply(glow_bonemed, is.numeric)]))

# Combine with factor variables
glow_bonemed_pre_pca <- cbind(glow_bonemed_scaled, glow_bonemed[, sapply(glow_bonemed, is.factor)])

# Check the structure of the new dataset
str(glow_bonemed_pre_pca)

# Apply PCA
pca_result <- prcomp(glow_bonemed_pre_pca[, sapply(glow_bonemed_pre_pca, is.numeric)], center = TRUE, scale. = TRUE)

# Summarize PCA results
summary(pca_result)

# Plot Scree plot to visualize the importance of each principal component
screeplot(pca_result, type = "lines")

# Select the first few principal components, for example, the first 5
final_data <- pca_result$x[, 1:5]

# Add back the target variable if necessary
final_data <- cbind(final_data, glow_bonemed$fracture)

# Check the final prepared data
head(final_data)

#  Get PCA variables
loadings <- pca_result$rotation
print(loadings)



# Use only the first three principal components
final_data <- pca_result$x[, 1:3]

# Add back the target variable
final_data <- cbind(final_data, Fracture = glow_bonemed$fracture)

# Check the final prepared data
head(final_data)

# Convert 'final_data' from matrix to data frame
final_data <- as.data.frame(final_data)

# Check balance of the target variable
table(final_data$Fracture)

# If imbalance is present, apply SMOTE
# Note that for SMOTE, the target variable must be a factor
if (any(table(final_data$Fracture) < 10)) { # This threshold is arbitrary, adjust as necessary
  library(smotefamily)
  final_data$Fracture <- as.factor(final_data$Fracture) # Ensure Fracture is a factor
  smote_data <- SMOTE(Fracture ~ ., final_data, perc.over = 100, k = 5) # Adjust perc.over and k as needed
  final_data_balanced <- smote_data$data
  table(final_data_balanced$Fracture)
}

# Balance Dataset with smote using Rose

# Load the ROSE library
library(ROSE)

# Apply SMOTE using the ROSE package
set.seed(123) # for reproducibility
smote_data <- ovun.sample(Fracture ~ ., data = final_data, method = "over", N = 375*2)$data

# Check the balance of the target variable after SMOTE
table(smote_data$Fracture)

# Re-encode the target variable to be binary 0 and 1
smote_data$Fracture <- ifelse(smote_data$Fracture == 1, 0, 1)

# Prepare matrix for xgboost
train_matrix <- xgb.DMatrix(data = as.matrix(smote_data[, -ncol(smote_data)]), label = smote_data$Fracture)

# Set xgboost parameters
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc"
)

# Train the xgboost model
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 100, # Number of boosting rounds. Adjust based on model performance.
  verbose = 0 # Change to 1 if you want to see the training progress
)

# Predictions
xgb_pred <- predict(xgb_model, train_matrix)

# Convert probabilities to binary class prediction
xgb_pred_binary <- ifelse(xgb_pred > 0.5, 1, 0)

# Confusion matrix
confusionMatrix(factor(xgb_pred_binary), factor(smote_data$Fracture))

# Cross-validation with xgboost
cv_results <- xgb.cv(
  params = params,
  data = train_matrix,
  nrounds = 100,
  nfold = 5, # Number of folds in CV. Adjust based on your data.
  showsd = TRUE, # Show standard deviation of CV metrics
  stratified = TRUE, # Perform stratified sampling
  print.every.n = 10, # Print every n iterations
  early_stopping_rounds = 10 # Stop if performance doesn't improve for 10 rounds
)

# Print the CV results
print(cv_results)

# Initialize rf_data as a copy of smote_data if not already done
rf_data <- smote_data  # This assumes smote_data is correctly created with balanced classes

# Now ensure Fracture is a factor with the right levels
rf_data$Fracture <- factor(rf_data$Fracture, levels = c(0, 1))

# Check the levels of Fracture in rf_data to ensure it's ready for classification
table(rf_data$Fracture)

# Random Forest
## Ensure 'Fracture' is a factor with appropriate levels
rf_data$Fracture <- factor(rf_data$Fracture, levels = c(0, 1))

# Train the Random Forest model as a classification model
rf_model <- randomForest(Fracture ~ ., data = rf_data, ntree = 500, mtry = 2, importance = TRUE, method = "class")

# Print model summary
print(rf_model)

# Predict using the random forest model
rf_pred <- predict(rf_model, rf_data, type = "class")

# Confusion matrix
confusionMatrix(rf_pred, rf_data$Fracture)

# Review the results
print(rf_train)
```
```{r}
# Check the class of rf_model to ensure it's the expected type
print(class(rf_model))

# Assuming the model is of the correct type, retrieve variable importance
var_importance <- randomForest::importance(rf_model)

# Create a data frame for plotting
importance_data <- data.frame(
  Variable = rownames(var_importance),
  Importance = var_importance[, "MeanDecreaseGini"]  # or "MeanDecreaseAccuracy" based on preference
)

# Use ggplot2 to create the variable importance plot
library(ggplot2)
ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the coordinates to make the plot horizontal
  labs(title = "Variable Importance", x = "Variables", y = "Importance") +
  theme_minimal()

```

# ensemble


# Predict probabilities for the test set
xgb_probs <- predict(xgb_model, xgb.DMatrix(data = as.matrix(test_data[, -which(names(test_data) == "Fracture")])), type = "prob")
rf_probs <- predict(rf_model, test_data, type = "prob")[, "Yes"]  # assuming 'Yes' is the positive class

# Ensemble predictions: averaging probabilities
ensemble_probs <- (xgb_probs + rf_probs) / 2

# Convert probabilities to binary outcome based on threshold
ensemble_preds <- ifelse(ensemble_probs > 0.5, "Yes", "No")
ensemble_preds <- factor(ensemble_preds, levels = c("No", "Yes"))

# Evaluate the ensemble model
conf_matrix <- confusionMatrix(ensemble_preds, test_data$Fracture)
print(conf_matrix)

# XGBoost probabilities (directly output as probabilities for the positive class)
xgb_prob <- predict(xgb_model, train_matrix)

# Ensure Random Forest is outputting probabilities
# Assuming 'rf_model' has been trained as a classification model
rf_prob <- predict(rf_model, rf_data, type = "prob")[, 2]  # probabilities of the positive class

# Ensemble predictions (averaging probabilities)
ensemble_prob <- (xgb_prob + rf_prob) / 2

# Convert averaged probabilities to class predictions based on a new threshold
threshold = 0.5  # Adjust this threshold based on evaluation
ensemble_class_pred <- ifelse(ensemble_prob > threshold, 1, 0)

# Evaluate ensemble predictions
ensemble_conf_matrix <- confusionMatrix(factor(ensemble_class_pred, levels = c(0, 1)), factor(rf_data$Fracture, levels = c(0, 1)))

# Print ensemble model confusion matrix
print(ensemble_conf_matrix)

# Assuming 'Fracture' is currently a numeric 0 and 1
# Convert 'Fracture' to a factor and set levels as valid R names
#rf_data$Fracture <- factor(rf_data$Fracture, levels = c(0, 1), labels = c("Class0", "Class1"))

# Check changes
#str(rf_data$Fracture)

rf_data_complete <- na.omit(rf_data)  # This will remove all rows with any NA values

# Set more meaningful factor level names
levels(rf_data$Fracture) <- c("No", "Yes")  # Assuming '0' was 'No', '1' was 'Yes'

# Now check the updated levels to ensure they are as expected
levels(rf_data$Fracture)

library(caret)
set.seed(123)  # for reproducibility

# Define training control
train_control <- trainControl(
  method = "cv",    # type of cross-validation
  number = 10,      # number of folds
  savePredictions = "final",
  classProbs = TRUE,  # save class probabilities for ensemble
  summaryFunction = twoClassSummary  # use AUC and other binary classification metrics
)

# Train the XGBoost model using cross-validation
xgb_train <- train(
  Fracture ~ ., data = rf_data, 
  method = "xgbTree",
  trControl = train_control,
  metric = "ROC"  # optimizing for ROC AUC
)

# Review the results
print(xgb_train)

# Repeat for Random Forest
rf_train <- train(
  Fracture ~ ., data = rf_data, 
  method = "rf",
  trControl = train_control,
  metric = "ROC"  # optimizing for ROC AUC
)

# Review the results
print(rf_train)

print(class(rf_model))

# Assuming the model is of the correct type, retrieve variable importance
var_importance <- randomForest::importance(rf_model)

# Create a data frame for plotting
importance_data <- data.frame(
  Variable = rownames(var_importance),
  Importance = var_importance[, "MeanDecreaseGini"]  # or "MeanDecreaseAccuracy" based on preference
)

# Use ggplot2 to create the variable importance plot
library(ggplot2)
ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the coordinates to make the plot horizontal
  labs(title = "Variable Importance", x = "Variables", y = "Importance") +
  theme_minimal()


# Assume rf_data is your full dataset
set.seed(123)  # for reproducibility

# Partitioning the data
partition <- createDataPartition(rf_data$Fracture, p = 0.8, list = FALSE)
training_data <- rf_data[partition, ]
test_data <- rf_data[-partition, ]

# Check the size of the datasets
nrow(training_data)
nrow(test_data)

# Ensure Fracture is a factor with consistent levels in both training and test datasets
training_data$Fracture <- factor(training_data$Fracture, levels = c("Class0", "Class1"))
test_data$Fracture <- factor(test_data$Fracture, levels = c("Class0", "Class1"))

# Train XGBoost
xgb_train <- train(
  Fracture ~ ., data = training_data, 
  method = "xgbTree",
  trControl = train_control,
  metric = "ROC"
)

# Train Random Forest
rf_train <- train(
  Fracture ~ ., data = training_data, 
  method = "rf",
  trControl = train_control,
  metric = "ROC"
)

# Predictions - Convert predictions to factor with the same levels as Fracture
test_ensemble_preds <- ifelse(test_ensemble_probs > 0.5, "Class1", "Class0")
test_ensemble_preds <- factor(test_ensemble_preds, levels = c("Class0", "Class1"))
# Evaluate performance
test_conf_matrix <- confusionMatrix(test_ensemble_preds, test_data$Fracture)
print(test_conf_matrix)

# Assuming you've already split data and trained models, now get probabilities:
# XGBoost probabilities
test_xgb_probs <- predict(xgb_train, newdata = test_data, type = "prob")[, "Class1"]

# Random Forest probabilities
test_rf_probs <- predict(rf_train, newdata = test_data, type = "prob")[, "Class1"]

# Ensemble probabilities (simple average of XGBoost and Random Forest)
test_ensemble_probs <- (test_xgb_probs + test_rf_probs) / 2

library(ROCR)

# Create prediction objects for ROC plotting
pred_xgb <- prediction(test_xgb_probs, test_data$Fracture)
pred_rf <- prediction(test_rf_probs, test_data$Fracture)
pred_ensemble <- prediction(test_ensemble_probs, test_data$Fracture)

# Calculate performance
perf_xgb <- performance(pred_xgb, "tpr", "fpr")
perf_rf <- performance(pred_rf, "tpr", "fpr")
perf_ensemble <- performance(pred_ensemble, "tpr", "fpr")

# Plot
plot(perf_xgb, col = "red", main = "ROC Curves Comparison", lwd = 2)
plot(perf_rf, col = "blue", add = TRUE, lwd = 2)
plot(perf_ensemble, col = "green", add = TRUE, lwd = 2)
legend("bottomright", legend = c("XGBoost", "Random Forest", "Ensemble"), col = c("red", "blue", "green"), lwd = 2)

# AUC calculation
auc_xgb <- performance(pred_xgb, "auc")@y.values[[1]]
auc_rf <- performance(pred_rf, "auc")@y.values[[1]]
auc_ensemble <- performance(pred_ensemble, "auc")@y.values[[1]]

# Print AUC values
cat("AUC for XGBoost: ", auc_xgb, "\n")
cat("AUC for Random Forest: ", auc_rf, "\n")
cat("AUC for Ensemble: ", auc_ensemble, "\n")

# Assuming test_xgb_probs are the predicted probabilities from the XGBoost model
xgb_preds <- ifelse(test_xgb_probs > 0.5, "Class1", "Class0")
xgb_preds <- factor(xgb_preds, levels = c("Class0", "Class1"))

# Calculate the confusion matrix and related statistics
xgb_conf_matrix <- confusionMatrix(xgb_preds, test_data$Fracture)

# Display the metrics
xgb_conf_matrix

# Assuming test_rf_probs are the predicted probabilities from the Random Forest model
rf_preds <- ifelse(test_rf_probs > 0.5, "Class1", "Class0")
rf_preds <- factor(rf_preds, levels = c("Class0", "Class1"))

# Calculate the confusion matrix and related statistics
rf_conf_matrix <- confusionMatrix(rf_preds, test_data$Fracture)

# Display the metrics
rf_conf_matrix

library(ggplot2)
library(dplyr)

# Creating a data frame with model performance metrics
model_comparison <- data.frame(
  Model = c("XGBoost", "Random Forest", "Ensemble"),
  Accuracy = c(0.88, 0.8667, 0.8867),
  Sensitivity = c(0.8267, 0.7867, 0.8267),
  Specificity = c(0.9333, 0.9467, 0.9467),
  Precision = c(0.9254, 0.9365, 0.9394)
)

# Melting the data for plotting with ggplot2
model_comparison_long <- model_comparison %>%
  gather(key = "Metric", value = "Value", -Model)

# Plotting the comparison
ggplot(model_comparison_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Model Performance Comparison", x = "Model", y = "Metric Value") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.title = element_blank())

# Radar Chart

# Create a data frame in the format expected by the radar chart function
radar_data <- as.data.frame(rbind(
  c(1, 1, 1, 1),  # Max values for normalization
  model_comparison[1, -1],  # Ensemble
  model_comparison[2, -1],  # Random Forest
  model_comparison[3, -1]   # XGBoost
))
rownames(radar_data) <- c("Max", "Ensemble", "Random Forest", "XGBoost")

# Normalize data (since radar chart requires all values to be on the same scale)
radar_data_norm <- radar_data
radar_data_norm[-1, ] <- as.data.frame(lapply(radar_data[-1, ], function(x) x / radar_data["Max",]))

# Radar chart plot
radarchart(radar_data_norm, axistype = 1,
           pcol = c("red", "blue", "green"),
           pfcol = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0,1,0,0.5)),
           plwd = 2, 
           cglcol = "grey", cglty = 1, axislabcol = "grey", 
           vlcex = 0.8)
legend(x = 0.8, y = 1, legend = rownames(radar_data)[-1], bty = "n", pch = 20, col = c("red", "blue", "green"))

# Composite Indices
# Example weights
weights <- c(Accuracy = 0.25, Precision = 0.25, Sensitivity = 0.25, Specificity = 0.25)

# Data frame with performance metrics for each model
performance_metrics <- data.frame(
  Model = c("Ensemble", "Random Forest", "XGBoost"),
  Accuracy = c(0.8867, 0.8667, 0.88),
  Precision = c(0.9394, 0.9365, 0.9254),
  Sensitivity = c(0.8267, 0.7867, 0.8267),
  Specificity = c(0.9467, 0.9467, 0.9333)
)

# Calculate composite index for each model
performance_metrics$CompositeIndex <- rowSums(performance_metrics[, -1] * weights)

# Order models by composite index
ordered_models <- performance_metrics[order(-performance_metrics$CompositeIndex), ]
ordered_models

# Weights based on assumed importance
weights <- c(Accuracy = 0.20, Precision = 0.30, Sensitivity = 0.30, Specificity = 0.20)

# Data frame with performance metrics for each model
performance_metrics <- data.frame(
  Model = c("Ensemble", "Random Forest", "XGBoost"),
  Accuracy = c(0.8867, 0.8667, 0.88),
  Precision = c(0.9394, 0.9365, 0.9254),
  Sensitivity = c(0.8267, 0.7867, 0.8267),
  Specificity = c(0.9467, 0.9467, 0.9333)
)

# Calculating the composite index for each model
performance_metrics$CompositeIndex <- (
  performance_metrics$Accuracy * weights['Accuracy'] +
  performance_metrics$Precision * weights['Precision'] +
  performance_metrics$Sensitivity * weights['Sensitivity'] +
  performance_metrics$Specificity * weights['Specificity']
)

# View the performance_metrics data frame with composite indices
performance_metrics

# Create a data frame with the model names and their composite index values
composite_data <- data.frame(
  Model = c("Ensemble", "XGBoost", "Random Forest"),
  CompositeIndex = c(0.89651, 0.88829, 0.87964)
)

# Create the bar chart
ggplot(composite_data, aes(x = Model, y = CompositeIndex, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(CompositeIndex, 4)), vjust = -0.3, size = 3.5) +
  labs(title = "Composite Index of ML Models", x = "Model", y = "Composite Index") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("red", "green", "blue"))

# Load necessary libraries
library(plotly)
library(ROCR)
library(dplyr)

# Create prediction objects for each model
pred_xgb <- prediction(test_xgb_probs, test_data$Fracture)
pred_rf <- prediction(test_rf_probs, test_data$Fracture)
pred_ensemble <- prediction(test_ensemble_probs, test_data$Fracture)

# Calculate performance for each model's ROC
perf_xgb <- performance(pred_xgb, "tpr", "fpr")
perf_rf <- performance(pred_rf, "tpr", "fpr")
perf_ensemble <- performance(pred_ensemble, "tpr", "fpr")

# Create a unified data frame for plotly
roc_data <- data.frame(
    fpr = c(perf_xgb@x.values[[1]], perf_rf@x.values[[1]], perf_ensemble@x.values[[1]]),
    tpr = c(perf_xgb@y.values[[1]], perf_rf@y.values[[1]], perf_ensemble@y.values[[1]]),
    model = factor(rep(c('XGBoost', 'Random Forest', 'Ensemble'), 
                       times = c(length(perf_xgb@x.values[[1]]), 
                                 length(perf_rf@x.values[[1]]), 
                                 length(perf_ensemble@x.values[[1]])))
    )
)

# Generate the interactive ROC plot
roc_plot <- plot_ly(roc_data, x = ~fpr, y = ~tpr, color = ~model, type = 'scatter', mode = 'lines') %>%
    layout(title = 'ROC Curve Comparison',
           xaxis = list(title = 'False Positive Rate'),
           yaxis = list(title = 'True Positive Rate'),
           hovermode = 'closest')

# Print the plot
roc_plot 


# Final check on xbg

# Ensure 'FRACTURE' is a binary factor with correct levels
train_data$FRACTURE <- factor(train_data$FRACTURE, levels = c("No", "Yes"))

# Convert y_train to numeric, ensuring "No" = 0 and "Yes" = 1
y_train <- as.numeric(train_data$FRACTURE) - 1

# Handling potential NA values in y_train
y_train[is.na(y_train)] <- 0  # Assuming NA should be the majority class for the example

# Prepare the training data matrix, excluding the target variable
x_train <- as.matrix(train_data[, !names(train_data) %in% "FRACTURE"])

# Create the DMatrix for XGBoost
dtrain <- xgb.DMatrix(data = x_train, label = y_train)

# Define parameters for XGBoost training
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 1,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 1,
  subsample = 0.5
)

# Train the XGBoost model with correct parameter naming
xgb_model <- xgb.train(
  params = params,
  data = dtrain,  # Ensure to use 'data = dtrain'
  nrounds = 50
)

# Print the model to see a summary
print(xgb_model)




















# MODEL COMPARISON
# Set global chunk options
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)


# Set seed for reproducibility
set.seed(123)
split <- sample.split(glow_bonemed$fracture, SplitRatio = 0.7)
trainData <- subset(glow_bonemed, split == TRUE)
testData <- subset(glow_bonemed, split == FALSE)

# Logistic Regression Models
log.model1 <- glm(fracture ~ age + height + weight + premeno + momfrac + armassist + smoke + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
log.model2 <- glm(fracture ~ age + height + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
lda.model <- lda(fracture ~ age + height + weight + raterisk + bonemed + bonemed_fu + bonetreat, data = trainData)

# Predictions
pred_log1 <- predict(log.model1, testData, type = "response")
pred_log2 <- predict(log.model2, testData, type = "response")
pred_lda <- predict(lda.model, testData)
pred_log1_class <- ifelse(pred_log1 > 0.5, "Yes", "No")
pred_log2_class <- ifelse(pred_log2 > 0.5, "Yes", "No")
pred_lda_class <- pred_lda$class

# Confusion Matrices
conf_matrix_log1 <- confusionMatrix(as.factor(pred_log1_class), as.factor(testData$fracture))
conf_matrix_log2 <- confusionMatrix(as.factor(pred_log2_class), as.factor(testData$fracture))
conf_matrix_lda <- confusionMatrix(as.factor(pred_lda_class), as.factor(testData$fracture))
print("Confusion Matrix for Logistic Regression Model 1:")
print(conf_matrix_log1)
print("Confusion Matrix for Logistic Regression Model 2:")
print(conf_matrix_log2)
print("Confusion Matrix for LDA Model:")
print(conf_matrix_lda)

# Data for plotting performance metrics
model_data <- data.frame(
  Model = c("Logistic Regression 1", "Logistic Regression 2", "LDA"),
  Accuracy = c(0.7867, 0.78, 0.78),
  Sensitivity = c(0.9823, 0.9823, 0.9735),
  Specificity = c(0.1892, 0.1622, 0.1892)
)
melted_data <- melt(model_data, id.vars = "Model")
ggplot(melted_data, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of Model Metrics", x = "Model", y = "Value", fill = "Metric") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Additional logistic regression models
log.model3 <- glm(fracture ~ age + height + weight + premeno + momfrac + armassist + smoke + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
log.model4 <- glm(fracture ~ age + height + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
pred_log3 <- predict(log.model3, testData, type = "response")
pred_log4 <- predict(log.model4, testData, type = "response")
pred_log3_class <- ifelse(pred_log3 > 0.5, "Yes", "No")
pred_log4_class <- ifelse(pred_log4 > 0.5, "Yes", "No")
conf_matrix_log3 <- confusionMatrix(as.factor(pred_log3_class), as.factor(testData$fracture))
conf_matrix_log4 <- confusionMatrix(as.factor(pred_log4_class), as.factor(testData$fracture))
print("Confusion Matrix for Logistic Regression Model 3:")
print(conf_matrix_log3)
print("Confusion Matrix for Logistic Regression Model 4:")
print(conf_matrix_log4)

# Model fitting and ROC curve plotting
set.seed(123)
log.model <- glm(fracture ~ age + height + weight + premeno + momfrac + armassist + smoke + raterisk + bonemed + bonemed_fu + bonetreat, family = binomial, data = trainData)
pred_probs <- predict(log.model, testData, type = "response")
pred_class <- ifelse(pred_probs > 0.5, "Yes", "No")
actual_numeric <- as.numeric(testData$fracture == "Yes")
roc_curve <- roc(response = actual_numeric, predictor = pred_probs)
plot(roc_curve, main = "ROC Curve", col = "#1c61b6")
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC:", round(auc_value, 3)), col = "#1c61b6", lwd = 2, cex = 0.8, xpd = TRUE, horiz = FALSE)
conf_matrix <- confusionMatrix(as.factor(pred_class), testData$fracture)
print(conf_matrix)


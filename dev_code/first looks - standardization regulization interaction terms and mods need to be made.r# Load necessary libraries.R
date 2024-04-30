# very basic exploration into data.   looks like standardization and regualization will need to be done, multicollinarity is a huge isse, factors need to be made into interactive tersm and then feature selection.   who wants to jump on a zoom and and spend a couple hours with meon this?


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)
library(car)
library(effects)
library(lmtest)
library(sandwich)
library(glmnet)
library(MASS)
library(broom)
library(tidyr)
library(kableExtra)
library(aplore3)
library(tibble)
library(ranger)
library(pheatmap)
library(boot)

# Set seed for reproducibility
set.seed(123)

# Data Overview
str(glow_bonemed)
summary(glow_bonemed)

# Checking for Missing Values
missing_values <- sum(is.na(glow_bonemed))
print(paste("Total Missing Values:", missing_values))

# Data Summarization and Transformation
numeric_summary <- glow_bonemed %>%
  summarise(
    Age_Min = min(age, na.rm = TRUE),
    Age_Max = max(age, na.rm = TRUE),
    Weight_Mean = mean(weight, na.rm = TRUE),
    BMI_Median = median(bmi, na.rm = TRUE)
  )

print(numeric_summary)

categorical_summary <- glow_bonemed %>%
  count(priorfrac)
print(categorical_summary)
print(table(glow_bonemed$priorfrac))

# Exploratory Data Analysis (EDA) with Visualizations
ggplot(glow_bonemed, aes(x = age, y = fracture, color = fracture)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = c("#6A0DAD", "lightpink")) +
  theme_minimal() +
  labs(title = "Age vs Fracture")

# Modeling
model <- glm(fracture ~ age + bmi, data = glow_bonemed, family = binomial())
summary(model)

# Model Validation using Training and Testing sets
splitIndex <- createDataPartition(glow_bonemed$fracture, p = 0.8, list = FALSE)
training_data <- glow_bonemed[splitIndex, ]
test_data <- glow_bonemed[-splitIndex, ]

predictions <- predict(model, newdata = test_data, type = "response")
roc_obj <- roc(test_data$fracture, predictions)
auc_value <- auc(roc_obj)
print(paste("AUC for Model:", auc_value))

# Advanced Modeling Techniques
poly_model <- lm(weight ~ poly(age, 2), data = glow_bonemed)
summary(poly_model)

# Fit Ridge and Lasso Regression Models
x_matrix <- model.matrix(~ age + height + bmi, data = glow_bonemed)[, -1]
y_vector <- glow_bonemed$weight
cv_ridge <- cv.glmnet(x_matrix, y_vector, alpha = 0)
optimal_lambda_ridge <- cv_ridge$lambda.min
ridge_model <- glmnet(x_matrix, y_vector, alpha = 0, lambda = optimal_lambda_ridge)

cv_lasso <- cv.glmnet(x_matrix, y_vector, alpha = 1)
optimal_lambda_lasso <- cv_lasso$lambda.min
lasso_model <- glmnet(x_matrix, y_vector, alpha = 1, lambda = optimal_lambda_lasso)

predictions_ridge <- predict(ridge_model, s = optimal_lambda_ridge, newx = x_matrix)
predictions_lasso <- predict(lasso_model, s = optimal_lambda_lasso, newx = x_matrix)

# Performance Metrics
ridge_performance <- postResample(predictions_ridge, y_vector)
lasso_performance <- postResample(predictions_lasso, y_vector)
print(list(Ridge = ridge_performance, Lasso = lasso_performance))

# Random Forest Model
rf_model <- ranger(fracture ~ age + bmi, data = training_data, probability = TRUE)
rf_predictions <- predict(rf_model, test_data)$predictions[, "Yes"]
rf_roc_curve <- roc(test_data$fracture, rf_predictions)
auc_rf <- auc(rf_roc_curve)
print(paste("AUC for Random Forest Model:", auc_rf))

# Cross-validation Results
cv_model1 <- cv.glm(glow_bonemed, model, K = 10)
cv_model2 <- cv.glm(glow_bonemed, model2, K = 10)
print(paste("CV Error for Model 1:", cv_model1$delta[1]))
print(paste("CV Error for Model 2:", cv_model2$delta[1]))

# Final Model Evaluation and Selection
print("Cross-validation Results for Model Robustness")
print(cross_validation_results)







































Comprehensive Data Analysis and Modeling for Glow_Bonemed Dataset
Authors: Jessica McPhaul, Rafia Mirza, Caleb Thornsbury
Date: 2024-04-06

Introduction
This document presents a detailed exploration and insights from an initial analysis of the Glow_Bonemed dataset. For further details, refer to the full report on RPubs.

Preliminaries
Libraries Loaded
The following R libraries are necessary for the analysis:


set.seed(123)
Data Overview
Structure and Summary
Structure: The dataset is a data.frame with 500 observations and 18 variables.
Summary: Variables include patient ID, site ID, physician ID, fracture history, age, weight, height, BMI, menopausal status, and medication details.
Missing Values Check
No missing values were detected in the dataset.


missing_values <- sum(is.na(glow_bonemed))
print(paste("Total Missing Values:", missing_values))
Exploratory Data Analysis (EDA)
Visualizations
Age vs. Fracture: Visualization shows jitter plots with fracture status.
BMI vs. Age: Scatter plot and density plot for detailed distribution analysis.
Data Summarization
Numeric Summary: Min, Max, Mean, and Median for age, weight, BMI, and other numerical variables.
Categorical Summary: Count of levels for factors like fracture history and medication status.
Correlations and Interactions
Detailed correlation matrix visualizations and interpretations for pairs of variables such as Age and BMI.
Modeling
Logistic Regression
Simple logistic regression models were used to predict fracture risk based on age and BMI. Model performance was evaluated using AUC from ROC curves.

Advanced Techniques
Random Forests: Used for better prediction accuracy.
Polynomial and Ridge/Lasso Regression: Explored for handling non-linear relationships and regularization.
Model Validation
Training/Testing Split: Standard approach using 80-20% split.
ROC Curve Analysis: For logistic and random forest models to compare prediction accuracy.
Conclusions
Model Performance
Best models based on AUC scores and ROC curves.
Insights from model coefficients and feature importance.
Recommendations
Suggest potential clinical interventions based on predictive factors.
Recommendations for further data collection and research to improve model robustness and accuracy.
Future Work
Explore additional modeling techniques such as SVMs and neural networks.
Extend the analysis to include more diverse datasets for external validation.






























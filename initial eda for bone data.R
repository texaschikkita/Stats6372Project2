sink("stats2project2.txt")
# Load the aplore3 package and the dataset
library(aplore3)
data("glow_bonemed")

# View the first few rows to understand the data structure
head(glow_bonemed)

# Summary statistics to get an overview (mean, median, min, max, etc.)
summary(glow_bonemed)

# Check for missing values
sum(is.na(glow_bonemed))

# Visualize distributions of numeric variables
library(ggplot2)
# Histogram for the 'age' variable
ggplot(glow_bonemed, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal()


# For categorical variables, use a bar plot to see the distribution
# Bar plot for the 'priorfrac' variable
ggplot(glow_bonemed, aes(x = priorfrac)) + 
  geom_bar(fill = "coral") +
  theme_minimal()




# Fit a GLM - Example with a binary outcome
# Fitting a GLM with fracture as the response and age, bmi, fracscore as predictors
glm_model <- glm(fracture ~ age + bmi + fracscore, family = binomial, data = glow_bonemed)

# Summary of the GLM model
summary(glm_model)


# is 'fracture' is the binary outcome variable I want to use?
# 'age', 'bmi', and 'fracscore' as predictor variables?


# logistic regression model
logistic_model <- glm(fracture ~ age + bmi + fracscore, family = binomial(link = "logit"), data = glow_bonemed)

# Summary of the logistic regression model
summary(logistic_model)

# Run the linear regression model using GLM with the gaussian family for a continuous outcome
glm_model_continuous <- glm(bmi ~ age + fracscore, family = gaussian, data = glow_bonemed)

# Summary of the linear regression model
summary(glm_model_continuous)


# Objective 1
# step 1: train  / validation split

# Load necessary libraries
library(caret)

# dataset is named `glow_bonemed`
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(glow_bonemed$fracture, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- glow_bonemed[trainIndex, ]
validationData <- glow_bonemed[-trainIndex, ]

# STEP 2: EXPLORATORY EDA
# Summarizing the dataset
summary(trainData)
str(trainData)
table(trainData$fracture)

# Visualizing distributions of numeric variables
hist(trainData$age, main="Histogram of Age", xlab="Age")
hist(trainData$bmi, main="Histogram of BMI", xlab="BMI")

# Relationship between predictor and outcome
boxplot(age ~ fracture, data = trainData, main="Age by Fracture Status")


# STEP 3 - feature selection - PENDING PENDING PENDING
# a: assess variable distributions and relationships


# step 3.5 - moving along 
# Check correlation between continuous predictors
cor(trainData[, c("age", "bmi", "fracscore")])

# Univariate analysis for a single predictor
summary(glm(fracture ~ age, family = binomial, data = trainData))

# Compute Variance Inflation Factor (VIF) to check for multicollinearity
# Note that you need to fit a model with all your predictors for VIF
library(car)
vif_model <- glm(fracture ~ age + bmi + fracscore, family = binomial, data = trainData)
vif(vif_model)

# Final decision on features to include
# This is conceptual; you would include variables based on the outcomes of your analyses
selected_features <- c("age", "bmi", "fracscore")


# STEP 4:  MLR ANALYSIS:
# Fitting the logistic regression model
logitModel <- glm(fracture ~ age + bmi + fracscore, family = binomial, data = trainData)

# Summary of the model
summary(logitModel)

# STEP 5: INTERPRETATION OF THE REGRESSION COEFFICENTS:
# Calculating odds ratios and confidence intervals
exp(coef(logitModel))
confint(logitModel)


# STEP 6: COMMENT ON PRACTICAL VERSUS STATISTICAL SIGNIFICANCE: PENDING - PENDING - PENDING

# STEP 7 VALIDATION
# Predicting on the validation set
validationData$predictedProb <- predict(logitModel, newdata = validationData, type = "response")

# Evaluate the model's performance
library(pROC)
roc(response = validationData$fracture, predictor = validationData$predictedProb)

# STEP 8: CONCLUSION - summarize findings including eda insights, model interpretation, model performance on the validation set, recomendations and conclusions that can be drawn and observed from analysis
# PENDING PENDING PENDING

######  stepping back for a minute: model simplification:

# Model with age and bmi
model_age_bmi <- glm(fracture ~ age + bmi, family = binomial, data = trainData)
summary(model_age_bmi)

# Model with fracscore and bmi
model_fracscore_bmi <- glm(fracture ~ fracscore + bmi, family = binomial, data = trainData)
summary(model_fracscore_bmi)

# Compare AIC values
AIC(model_age_bmi)
AIC(model_fracscore_bmi)


# advanced techniques even though not requested here - just for me:
# Perform PCA on the predictors
library(FactoMineR)
pca_res <- PCA(trainData[, c("age", "bmi", "fracscore")], scale.unit = TRUE, ncp = 2, graph = FALSE)

# Build the model using the principal components
model_pca <- glm(fracture ~ pca_res$ind$coord[,1] + pca_res$ind$coord[,2], family = binomial, data = trainData)
summary(model_pca)

# evaluate model performace:
# Predicting on the validation set with the chosen model
validationData$predictedProb <- predict(model_age_bmi, newdata = validationData, type = "response") # Change to model_fracscore_bmi if you choose it

# Evaluate the model's performance using ROC curve
library(pROC)
roc_result <- roc(response = validationData$fracture, predictor = validationData$predictedProb)
auc(roc_result)


# selecting final features:
# This should reflect the outcome of your analyses
selected_features <- c("age", "bmi") # or c("fracscore", "bmi")


# Building final model:
# Final model with selected features
final_model <- glm(fracture ~ age + bmi, family = binomial, data = trainData) # Update as necessary
summary(final_model)



# improving model's predicitve performance: - PENDING


# INTERPRETING MODEL COEFFICIENTS
# Calculate odds ratios
odds_ratios <- exp(coef(model_age_bmi))
odds_ratios

# Calculate confidence intervals for the odds ratios
conf_int <- exp(confint(model_age_bmi))
conf_int


# Feature Engineering: Adding Polynomial Features:
# Example of adding a squared term for age
trainData$age_squared <- trainData$age^2

# Fit a new model with the additional polynomial term
model_poly <- glm(fracture ~ age + I(age^2) + bmi, family = binomial, data = trainData)
summary(model_poly)

# Model Complexity: Exploring Non-Linear Models
# Fit a model using a spline for age
library(splines)
model_spline <- glm(fracture ~ ns(age, df = 3) + bmi, family = binomial, data = trainData)
summary(model_spline)

# Additional Data: Including More Predictors
# if 'new_variable' is an additional predictor available in dataset include the new variable in the model
#model_additional_var <- glm(fracture ~ age + bmi + new_variable, family = binomial, data = trainData)
#summary(model_additional_var)
  
# Alternative Models: Decision Tree
# Fit a decision tree model
library(rpart)
tree_model <- rpart(fracture ~ age + bmi, data = trainData, method = "class")

# Regularization: Ridge or Lasso Regression
# Fit a model using lasso (L1) regularization
library(glmnet)
x <- as.matrix(trainData[, c("age", "bmi")])
y <- trainData$fracture

# cv.glmnet will do cross-validation to find the optimal lambda
cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1) # alpha = 1 for lasso; alpha = 0 for ridge
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = cv_fit$lambda.min)


# Evaluating Model Performance
# Predicting on the validation set with the new model
validationData$predictedProb <- predict(model_poly, newdata = validationData, type = "response")

# Evaluate the model's performance using ROC curve - for each new model evaluate performace using the AUC metric:
library(pROC)
roc_result <- roc(response = validationData$fracture, predictor = validationData$predictedProb)
auc(roc_result)
sink()
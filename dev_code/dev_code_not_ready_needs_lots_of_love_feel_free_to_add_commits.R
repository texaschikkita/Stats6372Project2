R Notebook
# Load libraries
library(aplore3) 
library(caret)   
## Loading required package: ggplot2
## Loading required package: lattice
library(pROC)    
## Type 'citation("pROC")' for a citation.
## 
## Attaching package: 'pROC'
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
library(randomForest)
## randomForest 4.7-1.1
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## The following object is masked from 'package:ggplot2':
## 
##     margin
library(class)
library(rpart)
library(xgboost)
library(ROSE)   
## Loaded ROSE 0.0-4
library(smotefamily) 
library(memoise)
library(doParallel)
## Loading required package: foreach
## Loading required package: iterators
## Loading required package: parallel
# Load data
data("glow_bonemed") 
glow_bonemed$fracture <- as.factor(glow_bonemed$fracture)

library(aplore3) 
library(ggplot2)


data(glow_bonemed)

# Simple logistic regression model 
model <- glm(fracture ~ bonetreat + age, data = glow_bonemed, family = binomial)
#model <- glm(outcome ~ bonetreat + age, data = glow_bonemed, family = binomial) - no outcome found
summary(model) 


# Boxplot of fracture by bone medication use
ggplot(glow_bonemed, aes(x = bonetreat, y = fracture)) + 
  geom_boxplot()

# Scatterplot: age vs fracture
ggplot(glow_bonemed, aes(x = age, y = fracture, color = fracture)) +
  geom_point()

# Distribution of fracture
ggplot(glow_bonemed, aes(x = fracture)) + geom_bar()

# Age distribution by fracture status
ggplot(glow_bonemed, aes(x = age, fill = fracture)) + geom_violin()

# Grouped bar chart of medication use by age group
glow_bonemed %>% 
  mutate(age_group = cut(age, breaks = c(0, 50, 65, Inf))) %>% 
  ggplot(aes(x = age_group, fill = bonetreat)) + 
  geom_bar(position = "dodge")


# Distribution of fracture
ggplot(glow_bonemed, aes(x = fracture)) + geom_bar()

# Age distribution by fracture status
ggplot(glow_bonemed, aes(x = age, fill = fracture)) + geom_violin()

# Grouped bar chart of medication use by age group
glow_bonemed %>% 
  mutate(age_group = cut(age, breaks = c(0, 50, 65, Inf))) %>% 
  ggplot(aes(x = age_group, fill = bonetreat)) + 
  geom_bar(position = "dodge")

glow_bonemed$fracture <- as.numeric(glow_bonemed$fracture)  

hist(glow_bonemed$fracture)

ggplot(glow_bonemed, aes(x = fracture)) + 
  geom_bar(stat = "count") 


ggplot(glow_bonemed, aes(x = fracture, y = age, fill = fracture)) + 
  geom_violin()

# Boxplot of fracture by bone medication use
ggplot(glow_bonemed, aes(x = bonetreat, y = fracture)) + 
  +     geom_boxplot()
 
# Scatterplot: age vs fracture
ggplot(glow_bonemed, aes(x = age, y = fracture, color = fracture)) +
+     geom_point()
 
# Distribution of fracture
ggplot(glow_bonemed, aes(x = fracture)) + geom_bar()
# Age distribution by fracture status
ggplot(glow_bonemed, aes(x = age, fill = fracture)) + geom_violin()


ggplot(glow_bonemed, aes(x = fracture)) + geom_bar()
 
ggplot(glow_bonemed, aes(x = fracture, y = age, fill = fracture)) + geom_violin() 
glow_bonemed$fracture <- as.numeric(glow_bonemed$fracture)  
 
hist(glow_bonemed$fracture)
ggplot(glow_bonemed, aes(x = fracture)) + 
  +     geom_bar(stat = "count") 
 
# Bar Chart is better for categorical data
ggplot(glow_bonemed, aes(x = fracture)) + geom_bar()
ggplot(glow_bonemed, aes(x = fracture, y = age, fill = fracture)) + geom_violin() 
glow_bonemed$fracture <- as.numeric(glow_bonemed$fracture)  
hist(glow_bonemed$fracture)

ggplot(glow_bonemed, aes(x = fracture)) + 
 +     geom_bar(stat = "count") 


# Bar Chart is better for categorical data
ggplot(glow_bonemed, aes(x = fracture)) + geom_bar()


save(data, file = "bonedata.Rdata")


library(writexl)
write_xlxs(data, path = "bonedata.xlxs")


write.csv(data, file = "bonedata.csv")

saveRDS(data, file = "data.Rds")



write.table(data, file = "bone_data.txt", sep = "\t", row.names = FALSE)# Remove less predictive identifiers
trimmed_data <- glow_bonemed[, !(names(glow_bonemed) %in% c("sub_id", "site_id", "phy_id"))]

# Handling class imbalance with ROSE
set.seed(123)
rose_data <- ROSE(fracture ~ ., data = trimmed_data, seed = 1)$data

# Splitting data into training, validation, and test sets
set.seed(123)
# Split into temporary training and a test set
temp_train_indices <- createDataPartition(rose_data$fracture, p = 0.8, list = FALSE)
train_temp <- rose_data[temp_train_indices, ]
test_set <- rose_data[-temp_train_indices, ]

# Further split the temporary training set into actual training and validation sets
index <- createDataPartition(train_temp$fracture, p = 0.8, list = FALSE)
train_set <- train_temp[index, ]
validation_set <- train_temp[-index, ]

# Set up parallel processing
cl <- makeCluster(detectCores() - 1)  # Use one less than the total number of cores
registerDoParallel(cl)

# Feature selection using recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10, returnResamp = "all", saveDetails = TRUE)
results <- rfe(train_set[, -ncol(train_set)], train_set$fracture, sizes = c(1:5), rfeControl = control)

# Set control for training models
fit_control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE, verboseIter = TRUE, allowParallel = TRUE)
list(train_set = train_set, validation_set = validation_set, test_set=test_set, results = results, control=control)
## $train_set
##     priorfrac      age    weight   height      bmi premeno momfrac armassist
## 1          No 76.57131  63.88782 157.0700 23.17194      No      No        No
## 4          No 73.27055  51.91283 155.7938 24.26608      No      No        No

## 495        No 71.57434  66.60855 156.2115 18.39726      No      No        No
## 496        No 79.26159  79.16509 149.9979 36.64621      No      No        No
## 497        No 76.43735  61.25051 157.5315 24.29828      No      No       Yes
##     smoke raterisk   fracscore fracture bonemed bonemed_fu bonetreat
## 1     Yes     Less  3.83759409       No      No         No        No
## 4     Yes     Less  3.61050088       No     
## 
## $validation_set

##     smoke raterisk  fracscore fracture bonemed bonemed_fu bonetreat

## $results
## 
## Recursive feature selection
## 
## Outer resampling method: Cross-Validated (10 fold) 
## 
## Resampling performance over subset size:
## 
##  Variables Accuracy Kappa AccuracySD KappaSD Selected
##          1        1     1          0       0        *
##          2        1     1          0       0         
##          3        1     1          0       0         
##          4        1     1          0       0         
##          5        1     1          0       0         
##         14        1     1          0       0         
## 
## The top 1 variables (out of 1):
##    fracture
## 
## 
## $control
## $control$functions
## $control$functions$summary
## function (data, lev = NULL, model = NULL) 
## {
##     if (is.character(data$obs)) 
##         data$obs <- factor(data$obs, levels = lev)
##     postResample(data[, "pred"], data[, "obs"])
## }
## <bytecode: 0x000002190a4e0df8>
## <environment: namespace:caret>
## 
## $control$functions$fit
## function (x, y, first, last, ...) 
## {
##     loadNamespace("randomForest")
##     randomForest::randomForest(x, y, importance = TRUE, ...)
## }
## <bytecode: 0x000002190a4e00d8>
## <environment: namespace:caret>
## 
## $control$functions$pred
## function (object, x) 
## {
##     tmp <- predict(object, x)
##     if (is.factor(object$y)) {
##         out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
##             x, type = "prob"), stringsAsFactors = TRUE))
##     }
##     else out <- tmp
##     out
## }
## <bytecode: 0x000002190a4dfc08>
## <environment: namespace:caret>
## 
## $control$functions$rank
## function (object, x, y) 
## {
##     vimp <- varImp(object)
##     if (is.factor(y)) {
##         if (all(levels(y) %in% colnames(vimp))) {
##             avImp <- apply(vimp[, levels(y), drop = TRUE], 1, 
##                 mean)
##             vimp$Overall <- avImp
##         }
##     }
##     vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
##     if (ncol(x) == 1) {
##         vimp$var <- colnames(x)
##     }
##     else vimp$var <- rownames(vimp)
##     vimp
## }
## <bytecode: 0x000002190a4e2d60>
## <environment: namespace:caret>
## 
## $control$functions$selectSize
## function (x, metric, maximize) 
## {
##     best <- if (maximize) 
##         which.max(x[, metric])
##     else which.min(x[, metric])
##     min(x[best, "Variables"])
## }
## <bytecode: 0x000002190a4e4980>
## <environment: namespace:caret>
## 
## $control$functions$selectVar
## function (y, size) 
## {
##     finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
##         na.rm = TRUE))
##     names(finalImp)[2] <- "Overall"
##     finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
##         ]
##     as.character(finalImp$var[1:size])
## }
## <bytecode: 0x000002190a4e4248>
## <environment: namespace:caret>
## 
## 
## $control$rerank
## [1] FALSE
## 
## $control$method
## [1] "cv"
## 
## $control$saveDetails
## [1] TRUE
## 
## $control$number
## [1] 10
## 
## $control$repeats
## [1] 1
## 
## $control$returnResamp
## [1] "all"
## 
## $control$verbose
## [1] FALSE
## 
## $control$p
## [1] 0.75
## 
## $control$index
## NULL
## 
## $control$indexOut
## NULL
## 
## $control$timingSamps
## [1] 0
## 
## $control$seeds
## [1] NA
## 
## $control$allowParallel
## [1] TRUE
# Set up parallel processing
library(doParallel)
cl <- makeCluster(detectCores() - 1)  # Use one less than the total number of cores
registerDoParallel(cl)

# Suppress warnings to clean up model training output
options(warn = -1)

# Train models
rf_model <- train(fracture ~ ., data = train_set, method = "rf", trControl = fit_control)
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 2 on full training set
knn_model <- train(fracture ~ ., data = train_set, method = "knn", trControl = fit_control)
## Aggregating results
## Selecting tuning parameters
## Fitting k = 7 on full training set
tree_model <- train(fracture ~ ., data = train_set, method = "rpart", trControl = fit_control)
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0473 on full training set
# Train XGBoost model with a comprehensive tuning grid
xgb_model <- train(
    fracture ~ ., 
    data = train_set, 
    method = "xgbTree", 
    trControl = fit_control,
    tuneGrid = expand.grid(
        nrounds = 100,
        max_depth = c(3, 6, 9),
        eta = c(0.01, 0.1, 0.3),
        gamma = c(0, 0.1, 0.2),
        colsample_bytree = c(0.5, 0.75, 1),
        min_child_weight = c(1, 3, 5),
        subsample = c(0.5, 0.75, 1)
    ),
    verbose = FALSE
)
## Aggregating results
## Selecting tuning parameters
## Fitting nrounds = 100, max_depth = 9, eta = 0.01, gamma = 0.1, colsample_bytree = 0.75, min_child_weight = 1, subsample = 1 on full training set
# Stop parallel processing and reset options
stopCluster(cl)
registerDoSEQ()
options(warn = 0)  # Reset warning level
# Define function to extract and print model metrics
extract_metrics <- function(model, data, outcome_col) {
  predictions <- predict(model, newdata = data)
  prob_predictions <- predict(model, newdata = data, type = "prob")
  confusion <- confusionMatrix(predictions, data[[outcome_col]])
  roc_result <- roc(response = data[[outcome_col]], predictor = prob_predictions[,2])
  
  list(
    Sensitivity = confusion$byClass['Sensitivity'],
    Specificity = confusion$byClass['Specificity'],
    PPV = confusion$byClass['Pos Pred Value'],
    NPV = confusion$byClass['Neg Pred Value'],
    Accuracy = confusion$overall['Accuracy'],
    AUROC = auc(roc_result)
  )
}

# Evaluate models
rf_metrics <- extract_metrics(rf_model, validation_set, "fracture")
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
knn_metrics <- extract_metrics(knn_model, validation_set, "fracture")
## Setting levels: control = No, case = Yes
## Setting direction: controls > cases
tree_metrics <- extract_metrics(tree_model, validation_set, "fracture")
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
xgb_metrics <- extract_metrics(xgb_model, validation_set, "fracture")
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
# Print metrics
print("Random Forest Metrics:")
## [1] "Random Forest Metrics:"
print(rf_metrics)
## $Sensitivity
## Sensitivity 
##   0.7209302 
## 
## $Specificity
## Specificity 
##   0.6388889 
## 
## $PPV
## Pos Pred Value 
##      0.7045455 
## 
## $NPV
## Neg Pred Value 
##      0.6571429 
## 
## $Accuracy
##  Accuracy 
## 0.6835443 
## 
## $AUROC
## Area under the curve: 0.7574
print("KNN Metrics:")
## [1] "KNN Metrics:"
print(knn_metrics)
## $Sensitivity
## Sensitivity 
##   0.6744186 
## 
## $Specificity
## Specificity 
##   0.2222222 
## 
## $PPV
## Pos Pred Value 
##      0.5087719 
## 
## $NPV
## Neg Pred Value 
##      0.3636364 
## 
## $Accuracy
##  Accuracy 
## 0.4683544 
## 
## $AUROC
## Area under the curve: 0.5252
print("Decision Tree Metrics:")
## [1] "Decision Tree Metrics:"
print(tree_metrics)
## $Sensitivity
## Sensitivity 
##   0.6744186 
## 
## $Specificity
## Specificity 
##   0.6111111 
## 
## $PPV
## Pos Pred Value 
##      0.6744186 
## 
## $NPV
## Neg Pred Value 
##      0.6111111 
## 
## $Accuracy
##  Accuracy 
## 0.6455696 
## 
## $AUROC
## Area under the curve: 0.6376
print("XGBoost Metrics:")
## [1] "XGBoost Metrics:"
print(xgb_metrics)
## $Sensitivity
## Sensitivity 
##   0.7209302 
## 
## $Specificity
## Specificity 
##   0.5277778 
## 
## $PPV
## Pos Pred Value 
##      0.6458333 
## 
## $NPV
## Neg Pred Value 
##      0.6129032 
## 
## $Accuracy
##  Accuracy 
## 0.6329114 
## 
## $AUROC
## Area under the curve: 0.6789
# Feature Importance Plot for Random Forest
importance <- varImp(rf_model, scale = FALSE)
plot(importance)


# Correlation matrix of the model predictions to compare model agreement
predictions_rf <- predict(rf_model, validation_set, type = "prob")
predictions_knn <- predict(knn_model, validation_set, type = "prob")
predictions_tree <- predict(tree_model, validation_set, type = "prob")
predictions_xgb <- predict(xgb_model, validation_set, type = "prob")

# Assuming binary classification and interested in positive class probabilities
cor_matrix <- cor(cbind(predictions_rf[,2], predictions_knn[,2], predictions_tree[,2], predictions_xgb[,2]),
                  method = "pearson")
print(cor_matrix)
##           [,1]      [,2]      [,3]      [,4]
## [1,] 1.0000000 0.4698662 0.8070576 0.8628819
## [2,] 0.4698662 1.0000000 0.5212532 0.4023063
## [3,] 0.8070576 0.5212532 1.0000000 0.6969978
## [4,] 0.8628819 0.4023063 0.6969978 1.0000000
# Evaluate models on the test set using the metrics already defined
test_metrics_rf <- extract_metrics(rf_model, test_set, "fracture")
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
test_metrics_knn <- extract_metrics(knn_model, test_set, "fracture")
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
test_metrics_tree <- extract_metrics(tree_model, test_set, "fracture")
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
test_metrics_xgb <- extract_metrics(xgb_model, test_set, "fracture")
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
# Print test metrics for each model
print("Test Metrics - Random Forest:")
## [1] "Test Metrics - Random Forest:"
print(test_metrics_rf)
## $Sensitivity
## Sensitivity 
##   0.6851852 
## 
## $Specificity
## Specificity 
##   0.5652174 
## 
## $PPV
## Pos Pred Value 
##      0.6491228 
## 
## $NPV
## Neg Pred Value 
##      0.6046512 
## 
## $Accuracy
## Accuracy 
##     0.63 
## 
## $AUROC
## Area under the curve: 0.7206
print("Test Metrics - KNN:")
## [1] "Test Metrics - KNN:"
print(test_metrics_knn)
## $Sensitivity
## Sensitivity 
##   0.7037037 
## 
## $Specificity
## Specificity 
##   0.3043478 
## 
## $PPV
## Pos Pred Value 
##      0.5428571 
## 
## $NPV
## Neg Pred Value 
##      0.4666667 
## 
## $Accuracy
## Accuracy 
##     0.52 
## 
## $AUROC
## Area under the curve: 0.5215
print("Test Metrics - Decision Tree:")
## [1] "Test Metrics - Decision Tree:"
print(test_metrics_tree)
## $Sensitivity
## Sensitivity 
##   0.6851852 
## 
## $Specificity
## Specificity 
##   0.4565217 
## 
## $PPV
## Pos Pred Value 
##      0.5967742 
## 
## $NPV
## Neg Pred Value 
##      0.5526316 
## 
## $Accuracy
## Accuracy 
##     0.58 
## 
## $AUROC
## Area under the curve: 0.5773
print("Test Metrics - XGBoost:")
## [1] "Test Metrics - XGBoost:"
print(test_metrics_xgb)
## $Sensitivity
## Sensitivity 
##   0.6666667 
## 
## $Specificity
## Specificity 
##   0.4565217 
## 
## $PPV
## Pos Pred Value 
##      0.5901639 
## 
## $NPV
## Neg Pred Value 
##      0.5384615 
## 
## $Accuracy
## Accuracy 
##     0.57 
## 
## $AUROC
## Area under the curve: 0.6135
# Plot ROC curves for each model using the test set predictions
library(pROC)
roc_rf <- roc(response = test_set$fracture, predictor = predict(rf_model, test_set, type = "prob")[,2])
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
roc_knn <- roc(response = test_set$fracture, predictor = predict(knn_model, test_set, type = "prob")[,2])
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
roc_tree <- roc(response = test_set$fracture, predictor = predict(tree_model, test_set, type = "prob")[,2])
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
roc_xgb <- roc(response = test_set$fracture, predictor = predict(xgb_model, test_set, type = "prob")[,2])
## Setting levels: control = No, case = Yes
## Setting direction: controls < cases
plot(roc_rf, col="#1F77B4", lwd=2, main="ROC Curves for Models")
plot(roc_knn, col="#FF7F0E", lwd=2, add=TRUE)
plot(roc_tree, col="#2CA02C", lwd=2, add=TRUE)
plot(roc_xgb, col="#D62728", lwd=2, add=TRUE)
legend("bottomright", legend=c("Random Forest", "KNN", "Decision Tree", "XGBoost"),
       col=c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728"), lwd=2)


```{compare-and-analyze, cache=TRUE} # Comparison visualization and analysis # Combine AUC and other metrics into a single data frame for comparison aucs <- data.frame( Model = c(“Random Forest”, “KNN”, “Decision Tree”, “XGBoost”), AUC = c(auc(roc_rf), auc(roc_knn), auc(roc_tree), auc(roc_xgb)), Accuracy = c(test_metrics_rfAccuracy,testmetricsknn
Accuracy, test_metrics_treeAccuracy,testmetricsxgb
Accuracy) )

Bar plot of AUCs using ggplot2
library(ggplot2) ggplot(aucs, aes(x=Model, y=AUC, fill=Model)) + geom_bar(stat=“identity”, color=“black”) + theme_minimal() + labs(title=“Comparison of Model AUCs”, x=“Model”, y=“AUC Value”) + scale_fill_brewer(palette=“Set1”)

```
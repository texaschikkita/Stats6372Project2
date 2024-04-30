# Load libraries
library(aplore3)
library(caret)
library(pROC)
library(randomForest)
library(class)
library(rpart)
library(xgboost)
library(ROSE)
library(smotefamily)
library(memoise)
library(doParallel)

# Load data
data("glow_bonemed")
glow_bonemed$fracture <- as.factor(glow_bonemed$fracture)

# Remove less predictive identifiers
trimmed_data <- glow_bonemed[, !(names(glow_bonemed) %in% c("sub_id", "site_id", "phy_id"))]

# Handling class imbalance with ROSE
set.seed(123)
rose_data <- ROSE(fracture ~ ., data = trimmed_data, seed = 1)$data

# Splitting data into training, validation, and test sets
set.seed(123)
temp_train_indices <- createDataPartition(rose_data$fracture, p = 0.8, list = FALSE)
train_temp <- rose_data[temp_train_indices, ]
test_set <- rose_data[-temp_train_indices, ]
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

# Training various models
rf_model <- train(fracture ~ ., data = train_set, method = "rf", trControl = fit_control)
knn_model <- train(fracture ~ ., data = train_set, method = "knn", trControl = fit_control)
tree_model <- train(fracture ~ ., data = train_set, method = "rpart", trControl = fit_control)
xgb_model <- train(fracture ~ ., data = train_set, method = "xgbTree", trControl = fit_control)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Extracting and reporting results
list(train_set = train_set, validation_set = validation_set, test_set = test_set, results = results)

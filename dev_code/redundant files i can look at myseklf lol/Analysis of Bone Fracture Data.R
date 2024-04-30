Analysis of Bone Fracture Data

# Load Libraries
library(caret)
library(randomForest)
library(rpart)
library(pROC)
library(ROSE)  # for SMOTE

# Load Data
data("glow_bonemed")

# Pre-processing
glow_bonemed <- glow_bonemed %>%
  mutate(FRACTURE = as.numeric(FRACTURE == "Yes"),
         PRIORFRAC = as.numeric(PRIORFRAC == "Yes"),
         MOMFRAC = as.numeric(MOMFRAC == "Yes"),
         ARMASSIST = as.numeric(ARMASSIST == "Yes"),
         SMOKE = as.numeric(SMOKE == "Yes"),
         AGE_STDZ = scale(AGE),
         PRIORFRACxAGE = AGE_STDZ * PRIORFRAC,
         NOPRIORFRACxAGE = AGE_STDZ * (1 - PRIORFRAC))

# Model Building

# Split Data
set.seed(123)
split <- createDataPartition(glow_bonemed$FRACTURE, p = 0.75, list = FALSE)
train_data <- glow_bonemed[split, ]
test_data <- glow_bonemed[-split, ]

# Balance Data Using SMOTE
train_data_balanced <- ovun.sample(FRACTURE ~ ., data = train_data, method = "over", N = 2*table(train_data$FRACTURE)[2])$data

# Train Model
model_rf <- randomForest(FRACTURE ~ ., data = train_data_balanced, importance = TRUE, ntree = 500)

# Model Evaluation
predictions <- predict(model_rf, test_data)
conf_mat <- confusionMatrix(predictions, test_data$FRACTURE)

# Print Model Results
print(conf_mat)
importance(model_rf, type = 1)  # Type 1 for mean decrease accuracy

# ROC Curve
prob_predictions <- predict(model_rf, test_data, type = "prob")
roc_result <- roc(test_data$FRACTURE, prob_predictions[,2])
plot(roc_result, main = "ROC Curve")
auc(roc_result)

# Optional: Variable Importance Plot
varImpPlot(model_rf)































































































Libraries
## PART 2
library(xgboost)
library(shapper)
## Warning: package 'shapper' was built under R version 4.3.3
library(ROCR)
## Warning: package 'ROCR' was built under R version 4.3.3
library(ROSE)
## Warning: package 'ROSE' was built under R version 4.3.3
library(DMwR2)
## Warning: package 'DMwR2' was built under R version 4.3.3
library(smotefamily)
## Warning: package 'smotefamily' was built under R version 4.3.3
library(randomForest)
library(readxl)
library(dplyr)
library(car)
library(caret)
## Warning: package 'ggplot2' was built under R version 4.3.3
## Warning: package 'lattice' was built under R version 4.3.3
library(car)
library(pROC)
library(dplyr)
library(glmnet)
## Warning: package 'glmnet' was built under R version 4.3.3
library(FactoMineR)
## Warning: package 'FactoMineR' was built under R version 4.3.3
library(rpart)
library(rpart)
library(rpart.plot)
## Warning: package 'rpart.plot' was built under R version 4.3.3
library(aplore3)
## Warning: package 'aplore3' was built under R version 4.3.3
# Load and summarize the dataset
data("glow_bonemed")  # Corrected dataset name
summary(glow_bonemed)
##      sub_id         site_id          phy_id       priorfrac      age       
##  Min.   :  1.0   Min.   :1.000   Min.   :  1.00   No :374   Min.   :55.00  
##  1st Qu.:125.8   1st Qu.:2.000   1st Qu.: 57.75   Yes:126   1st Qu.:61.00  
##  Median :250.5   Median :3.000   Median :182.50             Median :67.00  
##  Mean   :250.5   Mean   :3.436   Mean   :178.55             Mean   :68.56  
##  3rd Qu.:375.2   3rd Qu.:5.000   3rd Qu.:298.00             3rd Qu.:76.00  
##  Max.   :500.0   Max.   :6.000   Max.   :325.00             Max.   :90.00  
##      weight           height           bmi        premeno   momfrac   armassist
##  Min.   : 39.90   Min.   :134.0   Min.   :14.88   No :403   No :435   No :312  
##  1st Qu.: 59.90   1st Qu.:157.0   1st Qu.:23.27   Yes: 97   Yes: 65   Yes:188  
##  Median : 68.00   Median :161.5   Median :26.42                                
##  Mean   : 71.82   Mean   :161.4   Mean   :27.55                                
##  3rd Qu.: 81.30   3rd Qu.:165.0   3rd Qu.:30.79                                
##  Max.   :127.00   Max.   :199.0   Max.   :49.08                                
##  smoke        raterisk     fracscore      fracture  bonemed   bonemed_fu
##  No :465   Less   :167   Min.   : 0.000   No :375   No :371   No :361   
##  Yes: 35   Same   :186   1st Qu.: 2.000   Yes:125   Yes:129   Yes:139   
##            Greater:147   Median : 3.000                                 
##                          Mean   : 3.698                                 
##                          3rd Qu.: 5.000                                 
##                          Max.   :11.000                                 
##  bonetreat
##  No :382  
##  Yes:118  
##           
##           
##           
## 
# Rename Columns and convert factors where needed
glow_bonemed_NEW <- glow_bonemed %>%
  rename(
    FRACTURE = fracture,
    AGE = age,
    HEIGHT = height,
    WEIGHT = weight,
    PREMENO = premeno,
    MOMFRAC = momfrac,
    RATERISK = raterisk,
    PRIORFRAC = priorfrac,
    ARMASSIST = armassist,
    SMOKE = smoke,
    BMI = bmi,
    SUB_ID = sub_id,
    SITE_ID = site_id,
    PHY_ID = phy_id,
    BONEMED = bonemed,
    FRACSCORE =fracscore,
    BONEMED_FU = bonemed_fu,
    BONETREAT = bonetreat
  ) %>%
  mutate(
    PRIORFRAC = as.numeric(PRIORFRAC == "Yes"),
    ARMASSIST = as.numeric(ARMASSIST == "Yes"),
    MOMFRAC = as.numeric(MOMFRAC == "Yes"),
    SMOKE = as.numeric(SMOKE == "Yes"),
    FRACTURE = as.numeric(FRACTURE == "Yes"),
    RATERISK_EQ_3 = as.numeric(RATERISK == "Greater"),
    RATERISK_num = as.numeric(factor(RATERISK))
  )




# INTERACTION AND STANDARDIZATION TERMS

# age
glow_bonemed_NEW <- glow_bonemed_NEW %>%
  mutate(AGE_STDZ = scale(AGE, center = TRUE, scale = TRUE))


# Standardize AGE and create interaction terms
glow_bonemed_NEW <- glow_bonemed_NEW %>%
  mutate(
    AGE_STDZ = scale(AGE, center = TRUE, scale = TRUE), # Standardize AGE
    AGExPRIORFRAC = AGE_STDZ * PRIORFRAC, # Interaction term: Standardized AGE * PRIORFRAC
    MOMFRACxARMASSIST = MOMFRAC * ARMASSIST, # Interaction term: MOMFRAC * ARMASSIST
    PRIORFRACxAGE_STDZ = PRIORFRAC * AGE_STDZ,
    NOPRIORFRACxAGE_STDZ = (1 - PRIORFRAC) * AGE_STDZ
    #AGE_STDZxNOPRIOR =(1 - PRIORFRAC)  * AGE_STDZ #(same as above but used in code)
    
  )

# Create Interaction Terms
glow_bonemed_NEW <- glow_bonemed_NEW %>%
  mutate(
    PRIORFRACxAGE_STDZ = PRIORFRAC * AGE_STDZ,
    NOPRIORFRACxAGE_STDZ = (1 - PRIORFRAC) * AGE_STDZ
  )


# Save the new dataframe to a CSV file
#write.csv(glow_bonemed_NEW, "glow_bonemed_NEW.csv", row.names = FALSE)

# Drop Useless Columns
glow_bonemedNEW <- glow_bonemed_NEW[, !(names(glow_bonemed_NEW) %in% c("SUB_ID", "SITE_ID", "PHY_ID"))]

# Rename Dataset to work with
GLOW_data <- glow_bonemed_NEW

glow <- GLOW_data
glows <- glow

colnames(GLOW_data)
##  [1] "SUB_ID"               "SITE_ID"              "PHY_ID"              
##  [4] "PRIORFRAC"            "AGE"                  "WEIGHT"              
##  [7] "HEIGHT"               "BMI"                  "PREMENO"             
## [10] "MOMFRAC"              "ARMASSIST"            "SMOKE"               
## [13] "RATERISK"             "FRACSCORE"            "FRACTURE"            
## [16] "BONEMED"              "BONEMED_FU"           "BONETREAT"           
## [19] "RATERISK_EQ_3"        "RATERISK_num"         "AGE_STDZ"            
## [22] "AGExPRIORFRAC"        "MOMFRACxARMASSIST"    "PRIORFRACxAGE_STDZ"  
## [25] "NOPRIORFRACxAGE_STDZ"
colnames(glow)
##  [1] "SUB_ID"               "SITE_ID"              "PHY_ID"              
##  [4] "PRIORFRAC"            "AGE"                  "WEIGHT"              
##  [7] "HEIGHT"               "BMI"                  "PREMENO"             
## [10] "MOMFRAC"              "ARMASSIST"            "SMOKE"               
## [13] "RATERISK"             "FRACSCORE"            "FRACTURE"            
## [16] "BONEMED"              "BONEMED_FU"           "BONETREAT"           
## [19] "RATERISK_EQ_3"        "RATERISK_num"         "AGE_STDZ"            
## [22] "AGExPRIORFRAC"        "MOMFRACxARMASSIST"    "PRIORFRACxAGE_STDZ"  
## [25] "NOPRIORFRACxAGE_STDZ"
colnames(glows)
##  [1] "SUB_ID"               "SITE_ID"              "PHY_ID"              
##  [4] "PRIORFRAC"            "AGE"                  "WEIGHT"              
##  [7] "HEIGHT"               "BMI"                  "PREMENO"             
## [10] "MOMFRAC"              "ARMASSIST"            "SMOKE"               
## [13] "RATERISK"             "FRACSCORE"            "FRACTURE"            
## [16] "BONEMED"              "BONEMED_FU"           "BONETREAT"           
## [19] "RATERISK_EQ_3"        "RATERISK_num"         "AGE_STDZ"            
## [22] "AGExPRIORFRAC"        "MOMFRACxARMASSIST"    "PRIORFRACxAGE_STDZ"  
## [25] "NOPRIORFRACxAGE_STDZ"
Model Building

GLM
Prepare the Logistic Regression Model
model1 <- glm(FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + ARMASSIST + RATERISK_EQ_3 + PRIORFRACxAGE_STDZ + NOPRIORFRACxAGE_STDZ, data = GLOW_data, family = binomial())

# Check Model Sumary & Diagnostics
summary(model1)
## 
## Call:
## glm(formula = FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + 
##     ARMASSIST + RATERISK_EQ_3 + PRIORFRACxAGE_STDZ + NOPRIORFRACxAGE_STDZ, 
##     family = binomial(), data = GLOW_data)
## 
## Coefficients: (1 not defined because of singularities)
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           5.18600    2.90210   1.787 0.073941 .  
## AGE_STDZ              0.49416    0.14671   3.368 0.000756 ***
## HEIGHT               -0.04329    0.01813  -2.388 0.016951 *  
## PRIORFRAC             0.85315    0.25473   3.349 0.000811 ***
## MOMFRAC               0.71225    0.30707   2.319 0.020368 *  
## ARMASSIST             0.44757    0.23238   1.926 0.054106 .  
## RATERISK_EQ_3         0.46265    0.23961   1.931 0.053495 .  
## PRIORFRACxAGE_STDZ   -0.51953    0.23153  -2.244 0.024839 *  
## NOPRIORFRACxAGE_STDZ       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 562.34  on 499  degrees of freedom
## Residual deviance: 504.78  on 492  degrees of freedom
## AIC: 520.78
## 
## Number of Fisher Scoring iterations: 4
#car::vif(model)

# Refit the model without the problematic interaction term
model_refit <- glm(FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + ARMASSIST + RATERISK_EQ_3 + PRIORFRACxAGE_STDZ, data = GLOW_data, family = binomial())

# Check the new model summary
summary(model_refit)
## 
## Call:
## glm(formula = FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + 
##     ARMASSIST + RATERISK_EQ_3 + PRIORFRACxAGE_STDZ, family = binomial(), 
##     data = GLOW_data)
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         5.18600    2.90210   1.787 0.073941 .  
## AGE_STDZ            0.49416    0.14671   3.368 0.000756 ***
## HEIGHT             -0.04329    0.01813  -2.388 0.016951 *  
## PRIORFRAC           0.85315    0.25473   3.349 0.000811 ***
## MOMFRAC             0.71225    0.30707   2.319 0.020368 *  
## ARMASSIST           0.44757    0.23238   1.926 0.054106 .  
## RATERISK_EQ_3       0.46265    0.23961   1.931 0.053495 .  
## PRIORFRACxAGE_STDZ -0.51953    0.23153  -2.244 0.024839 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 562.34  on 499  degrees of freedom
## Residual deviance: 504.78  on 492  degrees of freedom
## AIC: 520.78
## 
## Number of Fisher Scoring iterations: 4
# Attempt VIF calculation again
vif(model_refit)
##           AGE_STDZ             HEIGHT          PRIORFRAC            MOMFRAC 
##           1.804248           1.069318           1.218999           1.029081 
##          ARMASSIST      RATERISK_EQ_3 PRIORFRACxAGE_STDZ 
##           1.106067           1.069982           1.881434
# Original Model
# Fit the original logistic regression model
original_model <- glm(FRACTURE ~ AGE + HEIGHT + PRIORFRAC + MOMFRAC + ARMASSIST + RATERISK_EQ_3 + AGExPRIORFRAC, 
                      family = binomial(link = "logit"), 
                      data = GLOW_data)

summary(original_model)
## 
## Call:
## glm(formula = FRACTURE ~ AGE + HEIGHT + PRIORFRAC + MOMFRAC + 
##     ARMASSIST + RATERISK_EQ_3 + AGExPRIORFRAC, family = binomial(link = "logit"), 
##     data = GLOW_data)
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    1.41714    3.29734   0.430 0.667353    
## AGE            0.05497    0.01632   3.368 0.000756 ***
## HEIGHT        -0.04329    0.01813  -2.388 0.016951 *  
## PRIORFRAC      0.85315    0.25473   3.349 0.000811 ***
## MOMFRAC        0.71225    0.30707   2.319 0.020368 *  
## ARMASSIST      0.44757    0.23238   1.926 0.054106 .  
## RATERISK_EQ_3  0.46265    0.23961   1.931 0.053495 .  
## AGExPRIORFRAC -0.51953    0.23153  -2.244 0.024839 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 562.34  on 499  degrees of freedom
## Residual deviance: 504.78  on 492  degrees of freedom
## AIC: 520.78
## 
## Number of Fisher Scoring iterations: 4
car::vif(original_model)
##           AGE        HEIGHT     PRIORFRAC       MOMFRAC     ARMASSIST 
##      1.804248      1.069318      1.218999      1.029081      1.106067 
## RATERISK_EQ_3 AGExPRIORFRAC 
##      1.069982      1.881434
Logistic Regression Model

model2 <- glm(FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + ARMASSIST, data = GLOW_data, family = binomial())
summary(model2)
## 
## Call:
## glm(formula = FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + 
##     ARMASSIST, family = binomial(), data = GLOW_data)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept)  5.78083    2.90433   1.990  0.04654 * 
## AGE_STDZ     0.26748    0.11464   2.333  0.01964 * 
## HEIGHT      -0.04635    0.01816  -2.552  0.01070 * 
## PRIORFRAC    0.75259    0.23959   3.141  0.00168 **
## MOMFRAC      0.72263    0.30235   2.390  0.01684 * 
## ARMASSIST    0.52372    0.22829   2.294  0.02179 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 562.34  on 499  degrees of freedom
## Residual deviance: 513.46  on 494  degrees of freedom
## AIC: 525.46
## 
## Number of Fisher Scoring iterations: 4
Check Model Summary and Diagnostics

car::vif(model2)
##  AGE_STDZ    HEIGHT PRIORFRAC   MOMFRAC ARMASSIST 
##  1.140680  1.066260  1.080805  1.012421  1.085556
Validation Split Data and Validate Model

set.seed(123)
trainIndex <- createDataPartition(GLOW_data$FRACTURE, p = 0.8, list = FALSE, times = 1)
trainData <- GLOW_data[trainIndex, ]
validationData <- GLOW_data[-trainIndex, ]

fitModel <- glm(FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC, data = trainData, family = binomial())
validationData$predicted_probs <- predict(fitModel, newdata = validationData, type = "response")
validationData$predicted_class <- ifelse(validationData$predicted_probs > 0.5, 1, 0)

conf_matrix <- caret::confusionMatrix(as.factor(validationData$predicted_class), as.factor(validationData$FRACTURE))
print(conf_matrix)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  0  1
##          0 65 22
##          1 10  3
##                                           
##                Accuracy : 0.68            
##                  95% CI : (0.5792, 0.7698)
##     No Information Rate : 0.75            
##     P-Value [Acc > NIR] : 0.95540         
##                                           
##                   Kappa : -0.0159         
##                                           
##  Mcnemar's Test P-Value : 0.05183         
##                                           
##             Sensitivity : 0.8667          
##             Specificity : 0.1200          
##          Pos Pred Value : 0.7471          
##          Neg Pred Value : 0.2308          
##              Prevalence : 0.7500          
##          Detection Rate : 0.6500          
##    Detection Prevalence : 0.8700          
##       Balanced Accuracy : 0.4933          
##                                           
##        'Positive' Class : 0               
## 
ROC Curve & AUC

roc_result <- roc(response = validationData$FRACTURE, predictor = validationData$predicted_probs)
## Setting levels: control = 0, case = 1
## Setting direction: controls < cases
plot(roc_result, main="ROC Curve")


auc(roc_result)
## Area under the curve: 0.5464
# Improved Model:
# Standardize AGE and create new interaction terms
GLOW_data <- GLOW_data %>%
  mutate(
    AGE_STDZ = scale(AGE, center = TRUE, scale = TRUE), # Standardize AGE
    PRIORFRACxAGE_STDZ = PRIORFRAC * AGE_STDZ, # Interaction term: PRIORFRAC * Standardized AGE
    NOPRIORFRACxAGE_STDZ = (1 - PRIORFRAC) * AGE_STDZ # Interaction term: (1 - PRIORFRAC) * Standardized AGE
  )

# Fit the improved logistic regression model
improved_model <- glm(FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + ARMASSIST + RATERISK_EQ_3 + PRIORFRACxAGE_STDZ + NOPRIORFRACxAGE_STDZ, 
                      family = binomial(link = "logit"), 
                      data = GLOW_data)

# car::vif(improved_model) # Too Much Multicolinearity
summary(improved_model)
## 
## Call:
## glm(formula = FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + 
##     ARMASSIST + RATERISK_EQ_3 + PRIORFRACxAGE_STDZ + NOPRIORFRACxAGE_STDZ, 
##     family = binomial(link = "logit"), data = GLOW_data)
## 
## Coefficients: (1 not defined because of singularities)
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           5.18600    2.90210   1.787 0.073941 .  
## AGE_STDZ              0.49416    0.14671   3.368 0.000756 ***
## HEIGHT               -0.04329    0.01813  -2.388 0.016951 *  
## PRIORFRAC             0.85315    0.25473   3.349 0.000811 ***
## MOMFRAC               0.71225    0.30707   2.319 0.020368 *  
## ARMASSIST             0.44757    0.23238   1.926 0.054106 .  
## RATERISK_EQ_3         0.46265    0.23961   1.931 0.053495 .  
## PRIORFRACxAGE_STDZ   -0.51953    0.23153  -2.244 0.024839 *  
## NOPRIORFRACxAGE_STDZ       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 562.34  on 499  degrees of freedom
## Residual deviance: 504.78  on 492  degrees of freedom
## AIC: 520.78
## 
## Number of Fisher Scoring iterations: 4
# Fit the improved logistic regression model without the problematic term
improved_model2 <- glm(FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + ARMASSIST + RATERISK_EQ_3 + PRIORFRACxAGE_STDZ, 
                      family = binomial(link = "logit"), 
                      data = GLOW_data)

summary(improved_model2)
## 
## Call:
## glm(formula = FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + 
##     ARMASSIST + RATERISK_EQ_3 + PRIORFRACxAGE_STDZ, family = binomial(link = "logit"), 
##     data = GLOW_data)
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         5.18600    2.90210   1.787 0.073941 .  
## AGE_STDZ            0.49416    0.14671   3.368 0.000756 ***
## HEIGHT             -0.04329    0.01813  -2.388 0.016951 *  
## PRIORFRAC           0.85315    0.25473   3.349 0.000811 ***
## MOMFRAC             0.71225    0.30707   2.319 0.020368 *  
## ARMASSIST           0.44757    0.23238   1.926 0.054106 .  
## RATERISK_EQ_3       0.46265    0.23961   1.931 0.053495 .  
## PRIORFRACxAGE_STDZ -0.51953    0.23153  -2.244 0.024839 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 562.34  on 499  degrees of freedom
## Residual deviance: 504.78  on 492  degrees of freedom
## AIC: 520.78
## 
## Number of Fisher Scoring iterations: 4
# check the VIF for the improved model again
car::vif(improved_model2)
##           AGE_STDZ             HEIGHT          PRIORFRAC            MOMFRAC 
##           1.804248           1.069318           1.218999           1.029081 
##          ARMASSIST      RATERISK_EQ_3 PRIORFRACxAGE_STDZ 
##           1.106067           1.069982           1.881434
# Test Model
# Split into training and validation

set.seed(123)  # for reproducibility
trainIndex <- createDataPartition(GLOW_data$FRACTURE, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- GLOW_data[trainIndex, ]
validationData <- GLOW_data[-trainIndex, ]

# Fit Model on Training Data
improved_model <- glm(FRACTURE ~ AGE_STDZ + HEIGHT + PRIORFRAC + MOMFRAC + ARMASSIST + RATERISK_EQ_3 + PRIORFRACxAGE_STDZ, 
                      family = binomial(link = "logit"), 
                      data = trainData)


# Make Predictions on Validation Data
# Predicting probabilities
validationData$predicted_probs <- predict(improved_model, newdata = validationData, type = "response")

# Convert probabilities to a binary outcome (0 or 1) based on a threshold of 0.5
validationData$predicted_class <- ifelse(validationData$predicted_probs > 0.5, 1, 0)


# Evaluate Model Performance
# Creating a confusion matrix to compare actual and predicted classifications
conf_matrix <- confusionMatrix(as.factor(validationData$predicted_class), as.factor(validationData$FRACTURE))
print(conf_matrix)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  0  1
##          0 67 21
##          1  8  4
##                                           
##                Accuracy : 0.71            
##                  95% CI : (0.6107, 0.7964)
##     No Information Rate : 0.75            
##     P-Value [Acc > NIR] : 0.85046         
##                                           
##                   Kappa : 0.0645          
##                                           
##  Mcnemar's Test P-Value : 0.02586         
##                                           
##             Sensitivity : 0.8933          
##             Specificity : 0.1600          
##          Pos Pred Value : 0.7614          
##          Neg Pred Value : 0.3333          
##              Prevalence : 0.7500          
##          Detection Rate : 0.6700          
##    Detection Prevalence : 0.8800          
##       Balanced Accuracy : 0.5267          
##                                           
##        'Positive' Class : 0               
## 
# ROC Curve & AUC
# ROC curve
roc_result <- roc(response = validationData$FRACTURE, predictor = validationData$predicted_probs)
## Setting levels: control = 0, case = 1
## Setting direction: controls < cases
plot(roc_result, main="ROC Curve")


auc(roc_result)
## Area under the curve: 0.6149
# REFINING FURTHER
# Pairwise
pairwise_interactions <- GLOW_data %>%
  mutate(
    AGExWEIGHT = AGE * WEIGHT,
    AGExHEIGHT = AGE * HEIGHT,
    WEIGHTxHEIGHT = WEIGHT * HEIGHT
  )

# Total Pairwise
selected_vars <- c("AGE", "WEIGHT", "HEIGHT", "PRIORFRAC", "AGExPRIORFRAC", "AGE_STDZ",  "AGE_STDZxPRIOR", "AGE_STDZxNOPRIOR", "BMI", "PREMENO", "MOMFRAC", "ARMASSIST", "MOMFRACxARMASSIST", "SMOKE", "RATERISK", "RATERISK_EQ_1", "RATERISK_EQ_2", "RATERISK_EQ_3", "FRACSCORE", "PRIORFRACxAGE_STDZ", "NOPRIORFRACxAGE_STDZ")  # List the variables to combine

# Ensure to use the correct variable names as they exist in your dataframe
combinations <- combn(selected_vars, 2, simplify = FALSE) # Get all combinations of these variables

# Iterate over the combinations and create interaction terms
for(comb in combinations) {
  if(all(comb %in% names(GLOW_data))) {
    var_name <- paste(comb, collapse = "TOTAL_PAIRWISE")  # Create a name for the new variable
    pairwise_interactions[[var_name]] <- GLOW_data[[comb[1]]] * GLOW_data[[comb[2]]]
  } else {
    warning("Variable combination does not exist in the dataset: ", paste(comb, collapse = " and "))
  }
}
## Warning: Variable combination does not exist in the dataset: AGE and
## AGE_STDZxPRIOR
## Warning: Variable combination does not exist in the dataset: AGE and
## AGE_STDZxNOPRIOR
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: AGE and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: AGE and
## RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: WEIGHT and
## AGE_STDZxPRIOR
## Warning: Variable combination does not exist in the dataset: WEIGHT and
## AGE_STDZxNOPRIOR
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: WEIGHT and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: WEIGHT and
## RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: HEIGHT and
## AGE_STDZxPRIOR
## Warning: Variable combination does not exist in the dataset: HEIGHT and
## AGE_STDZxNOPRIOR
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: HEIGHT and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: HEIGHT and
## RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: PRIORFRAC and
## AGE_STDZxPRIOR
## Warning: Variable combination does not exist in the dataset: PRIORFRAC and
## AGE_STDZxNOPRIOR
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: PRIORFRAC and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: PRIORFRAC and
## RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: AGExPRIORFRAC and
## AGE_STDZxPRIOR
## Warning: Variable combination does not exist in the dataset: AGExPRIORFRAC and
## AGE_STDZxNOPRIOR
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: AGExPRIORFRAC and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: AGExPRIORFRAC and
## RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: AGE_STDZ and
## AGE_STDZxPRIOR
## Warning: Variable combination does not exist in the dataset: AGE_STDZ and
## AGE_STDZxNOPRIOR
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: AGE_STDZ and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: AGE_STDZ and
## RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## AGE_STDZxNOPRIOR
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## BMI
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## PREMENO
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## MOMFRAC
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## ARMASSIST
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## MOMFRACxARMASSIST
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## SMOKE
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## RATERISK
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## RATERISK_EQ_3
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## FRACSCORE
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## PRIORFRACxAGE_STDZ
## Warning: Variable combination does not exist in the dataset: AGE_STDZxPRIOR and
## NOPRIORFRACxAGE_STDZ
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and BMI
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and PREMENO
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and MOMFRAC
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and ARMASSIST
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and MOMFRACxARMASSIST
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and SMOKE
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and RATERISK
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and RATERISK_EQ_3
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and FRACSCORE
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and PRIORFRACxAGE_STDZ
## Warning: Variable combination does not exist in the dataset: AGE_STDZxNOPRIOR
## and NOPRIORFRACxAGE_STDZ
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: BMI and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: BMI and
## RATERISK_EQ_2
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: PREMENO and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: PREMENO and
## RATERISK_EQ_2
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: MOMFRAC and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: MOMFRAC and
## RATERISK_EQ_2
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: ARMASSIST and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: ARMASSIST and
## RATERISK_EQ_2
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: MOMFRACxARMASSIST
## and RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: MOMFRACxARMASSIST
## and RATERISK_EQ_2
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: SMOKE and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: SMOKE and
## RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: RATERISK and
## RATERISK_EQ_1
## Warning: Variable combination does not exist in the dataset: RATERISK and
## RATERISK_EQ_2
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning in Ops.factor(GLOW_data[[comb[1]]], GLOW_data[[comb[2]]]): '*' not
## meaningful for factors
## Warning: Variable combination does not exist in the dataset: RATERISK_EQ_1 and
## RATERISK_EQ_2
## Warning: Variable combination does not exist in the dataset: RATERISK_EQ_1 and
## RATERISK_EQ_3
## Warning: Variable combination does not exist in the dataset: RATERISK_EQ_1 and
## FRACSCORE
## Warning: Variable combination does not exist in the dataset: RATERISK_EQ_1 and
## PRIORFRACxAGE_STDZ
## Warning: Variable combination does not exist in the dataset: RATERISK_EQ_1 and
## NOPRIORFRACxAGE_STDZ
## Warning: Variable combination does not exist in the dataset: RATERISK_EQ_2 and
## RATERISK_EQ_3
## Warning: Variable combination does not exist in the dataset: RATERISK_EQ_2 and
## FRACSCORE
## Warning: Variable combination does not exist in the dataset: RATERISK_EQ_2 and
## PRIORFRACxAGE_STDZ
## Warning: Variable combination does not exist in the dataset: RATERISK_EQ_2 and
## NOPRIORFRACxAGE_STDZ
## MORE ADVANCED MODELING
# Refining Further
# Pairwise
pairwise_interactions <- GLOW_data %>%
  mutate(
    AGExWEIGHT = AGE * WEIGHT,
    AGExHEIGHT = AGE * HEIGHT,
    WEIGHTxHEIGHT = WEIGHT * HEIGHT
  )

# Total Pairwise
selected_vars <- c("AGE", "WEIGHT", "HEIGHT") 
combinations <- combn(selected_vars, 2, simplify = FALSE)  # Get all combinations of these variables

# Check the structure of the new dataframe with interaction terms
str(pairwise_interactions)
## 'data.frame':    500 obs. of  28 variables:
##  $ SUB_ID              : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ SITE_ID             : int  1 4 6 6 1 5 5 1 1 4 ...
##  $ PHY_ID              : int  14 284 305 309 37 299 302 36 8 282 ...
##  $ PRIORFRAC           : num  0 0 1 0 0 1 0 1 1 0 ...
##  $ AGE                 : int  62 65 88 82 61 67 84 82 86 58 ...
##  $ WEIGHT              : num  70.3 87.1 50.8 62.1 68 68 50.8 40.8 62.6 63.5 ...
##  $ HEIGHT              : int  158 160 157 160 152 161 150 153 156 166 ...
##  $ BMI                 : num  28.2 34 20.6 24.3 29.4 ...
##  $ PREMENO             : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ MOMFRAC             : num  0 0 1 0 0 0 0 0 0 0 ...
##  $ ARMASSIST           : num  0 0 1 0 0 0 0 0 0 0 ...
##  $ SMOKE               : num  0 0 0 0 0 1 0 0 0 0 ...
##  $ RATERISK            : Factor w/ 3 levels "Less","Same",..: 2 2 1 1 2 2 1 2 2 1 ...
##  $ FRACSCORE           : int  1 2 11 5 1 4 6 7 7 0 ...
##  $ FRACTURE            : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BONEMED             : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 2 1 1 ...
##  $ BONEMED_FU          : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 2 1 1 ...
##  $ BONETREAT           : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 2 1 1 ...
##  $ RATERISK_EQ_3       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ RATERISK_num        : num  2 2 1 1 2 2 1 2 2 1 ...
##  $ AGE_STDZ            : num [1:500, 1] -0.73 -0.396 2.162 1.495 -0.841 ...
##   ..- attr(*, "scaled:center")= num 68.6
##   ..- attr(*, "scaled:scale")= num 8.99
##  $ AGExPRIORFRAC       : num [1:500, 1] 0 0 2.16 0 0 ...
##   ..- attr(*, "scaled:center")= num 68.6
##   ..- attr(*, "scaled:scale")= num 8.99
##  $ MOMFRACxARMASSIST   : num  0 0 1 0 0 0 0 0 0 0 ...
##  $ PRIORFRACxAGE_STDZ  : num [1:500, 1] 0 0 2.16 0 0 ...
##   ..- attr(*, "scaled:center")= num 68.6
##   ..- attr(*, "scaled:scale")= num 8.99
##  $ NOPRIORFRACxAGE_STDZ: num [1:500, 1] -0.73 -0.396 0 1.495 -0.841 ...
##   ..- attr(*, "scaled:center")= num 68.6
##   ..- attr(*, "scaled:scale")= num 8.99
##  $ AGExWEIGHT          : num  4359 5662 4470 5092 4148 ...
##  $ AGExHEIGHT          : int  9796 10400 13816 13120 9272 10787 12600 12546 13416 9628 ...
##  $ WEIGHTxHEIGHT       : num  11107 13936 7976 9936 10336 ...
# View the first few rows to confirm the new columns were added
head(pairwise_interactions)
##   SUB_ID SITE_ID PHY_ID PRIORFRAC AGE WEIGHT HEIGHT      BMI PREMENO MOMFRAC
## 1      1       1     14         0  62   70.3    158 28.16055      No       0
## 2      2       4    284         0  65   87.1    160 34.02344      No       0
## 3      3       6    305         1  88   50.8    157 20.60936      No       1
## 4      4       6    309         0  82   62.1    160 24.25781      No       0
## 5      5       1     37         0  61   68.0    152 29.43213      No       0
## 6      6       5    299         1  67   68.0    161 26.23356      No       0
##   ARMASSIST SMOKE RATERISK FRACSCORE FRACTURE BONEMED BONEMED_FU BONETREAT
## 1         0     0     Same         1        0      No         No        No
## 2         0     0     Same         2        0      No         No        No
## 3         1     0     Less        11        0      No         No        No
## 4         0     0     Less         5        0      No         No        No
## 5         0     0     Same         1        0      No         No        No
## 6         0     1     Same         4        0      No         No        No
##   RATERISK_EQ_3 RATERISK_num   AGE_STDZ AGExPRIORFRAC MOMFRACxARMASSIST
## 1             0            2 -0.7299597     0.0000000                 0
## 2             0            2 -0.3962384     0.0000000                 0
## 3             0            1  2.1622915     2.1622915                 1
## 4             0            1  1.4948489     0.0000000                 0
## 5             0            2 -0.8412001     0.0000000                 0
## 6             0            2 -0.1737576    -0.1737576                 0
##   PRIORFRACxAGE_STDZ NOPRIORFRACxAGE_STDZ AGExWEIGHT AGExHEIGHT WEIGHTxHEIGHT
## 1          0.0000000           -0.7299597     4358.6       9796       11107.4
## 2          0.0000000           -0.3962384     5661.5      10400       13936.0
## 3          2.1622915            0.0000000     4470.4      13816        7975.6
## 4          0.0000000            1.4948489     5092.2      13120        9936.0
## 5          0.0000000           -0.8412001     4148.0       9272       10336.0
## 6         -0.1737576            0.0000000     4556.0      10787       10948.0
# Find Target Column "FRACTURE"
# Print all column names in the dataset
print(names(GLOW_data))
##  [1] "SUB_ID"               "SITE_ID"              "PHY_ID"              
##  [4] "PRIORFRAC"            "AGE"                  "WEIGHT"              
##  [7] "HEIGHT"               "BMI"                  "PREMENO"             
## [10] "MOMFRAC"              "ARMASSIST"            "SMOKE"               
## [13] "RATERISK"             "FRACSCORE"            "FRACTURE"            
## [16] "BONEMED"              "BONEMED_FU"           "BONETREAT"           
## [19] "RATERISK_EQ_3"        "RATERISK_num"         "AGE_STDZ"            
## [22] "AGExPRIORFRAC"        "MOMFRACxARMASSIST"    "PRIORFRACxAGE_STDZ"  
## [25] "NOPRIORFRACxAGE_STDZ"
# Or use the which function to find the index of the 'FRACTURE' column
fracture_column_index <- which(names(GLOW_data) == "FRACTURE")
print(paste("The 'FRACTURE' column is at index:", fracture_column_index))
## [1] "The 'FRACTURE' column is at index: 15"
# *********************************************
# ADD FRACTURE COLLMN BACK IN
# GLOW_data <- GLOW_data %>% 
#  mutate(
#    FRACTURE = as.numeric(FRACTURE == "Yes")
#  )
# **********************************************
# Ensure y is just the FRACTURE column as a factor if it's categorical
y <- as.factor(GLOW_data$FRACTURE)

# Ensure x excludes the FRACTURE column
x <- GLOW_data[, -which(names(GLOW_data) == "FRACTURE")]

# Setup RFE control
control <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10, repeats=3)

# Run RFE
results <- rfe(x, y, sizes=c(1:5), rfeControl=control)

# Print results
print(results)
## 
## Recursive feature selection
## 
## Outer resampling method: Cross-Validated (10 fold, repeated 3 times) 
## 
## Resampling performance over subset size:
## 
##  Variables Accuracy  Kappa AccuracySD  KappaSD Selected
##          1   0.9987 0.9964   0.005074 0.013524         
##          2   0.9993 0.9982   0.003651 0.009732        *
##          3   0.9987 0.9964   0.005074 0.013524         
##          4   0.9987 0.9964   0.005074 0.013524         
##          5   0.9987 0.9964   0.005074 0.013524         
##         24   0.9987 0.9964   0.005074 0.013524         
## 
## The top 2 variables (out of 2):
##    SUB_ID, FRACSCORE
# Print the results
print(results)
## 
## Recursive feature selection
## 
## Outer resampling method: Cross-Validated (10 fold, repeated 3 times) 
## 
## Resampling performance over subset size:
## 
##  Variables Accuracy  Kappa AccuracySD  KappaSD Selected
##          1   0.9987 0.9964   0.005074 0.013524         
##          2   0.9993 0.9982   0.003651 0.009732        *
##          3   0.9987 0.9964   0.005074 0.013524         
##          4   0.9987 0.9964   0.005074 0.013524         
##          5   0.9987 0.9964   0.005074 0.013524         
##         24   0.9987 0.9964   0.005074 0.013524         
## 
## The top 2 variables (out of 2):
##    SUB_ID, FRACSCORE
# Summary RFE
summary(results)
##              Length Class        Mode     
## pred          0     -none-       NULL     
## variables     6     data.frame   list     
## results       5     data.frame   list     
## bestSubset    1     -none-       numeric  
## fit          18     randomForest list     
## optVariables  2     -none-       character
## optsize       1     -none-       numeric  
## call          5     -none-       call     
## control      14     -none-       list     
## resample      8     data.frame   list     
## metric        1     -none-       character
## maximize      1     -none-       logical  
## perfNames     2     -none-       character
## times         3     -none-       list     
## resampledCM   0     -none-       NULL     
## obsLevels     2     -none-       character
## dots          0     -none-       list
# Plotting RFE
plot(results, type = c("g", "c"))


# Review Selected Features
print(results$optsize)   # Prints the optimal size of features
## [1] 2
print(results$variables) # Prints the names of the selected variables at the optimal size
# 1170 Fold10.Rep3
# The optimal number of features determined by the RFE process is 5.
# The top 5 variables selected are FRACSCORE, WEIGHT, BMI, HEIGHT, and AGE_STDZxNOPRIOR
# RANDOM FOREST
# Ensure FRACTURE is a factor if it's categorical
GLOW_data$FRACTURE <- as.factor(GLOW_data$FRACTURE)

# Build the random forest model
rf_model <- randomForest(FRACTURE ~ ., data=GLOW_data, ntree=500, importance=TRUE)

# Print the importance of each variable
print(importance(rf_model))
##                               0            1 MeanDecreaseAccuracy
## SUB_ID               62.6121879 65.715482904          67.08370002
## SITE_ID               2.4985095 -0.212001626           2.38049773
## PHY_ID                1.1306586  0.176230208           1.12326494
## PRIORFRAC             3.5586719  1.291933074           3.61322142
## AGE                   4.5245942  0.341649995           4.31086931
## WEIGHT                5.9583739 -0.757895657           5.04256662
## HEIGHT                2.4831513  1.898654477           3.11310891
## BMI                   8.4760980 -0.008161530           7.46166891
## PREMENO              -1.2633360  0.325914944          -0.67325726
## MOMFRAC               0.0380801 -0.916736366          -0.44918254
## ARMASSIST             1.8319557 -0.082007382           1.48800959
## SMOKE                 0.5829811 -1.077956978          -0.02888994
## RATERISK              1.0425834  1.995687899           2.01243508
## FRACSCORE             6.8392742  3.494309715           7.46335355
## BONEMED               4.8554062 -2.010361346           4.13443523
## BONEMED_FU            6.0979212  0.273828229           5.97286350
## BONETREAT             4.8335809 -0.509607481           4.52005198
## RATERISK_EQ_3         2.4538964  0.137860152           2.20026871
## RATERISK_num         -0.8066306  2.090266963           0.69317951
## AGE_STDZ              5.6714141 -0.295374932           4.88828644
## AGExPRIORFRAC         0.6799395  0.962445643           1.06704087
## MOMFRACxARMASSIST     0.9806671  0.005158267           0.87793266
## PRIORFRACxAGE_STDZ    0.8544333 -1.799052771          -0.40288044
## NOPRIORFRACxAGE_STDZ  4.8100556  2.189911868           5.37778120
##                      MeanDecreaseGini
## SUB_ID                    122.7330419
## SITE_ID                     2.4933762
## PHY_ID                      5.6309217
## PRIORFRAC                   1.7654977
## AGE                         4.5232876
## WEIGHT                      5.1066637
## HEIGHT                      6.0252334
## BMI                         6.4377834
## PREMENO                     0.7015662
## MOMFRAC                     1.2943913
## ARMASSIST                   1.2210545
## SMOKE                       0.4642600
## RATERISK                    1.5981293
## FRACSCORE                   5.1039134
## BONEMED                     0.9710159
## BONEMED_FU                  1.6896958
## BONETREAT                   0.8693418
## RATERISK_EQ_3               0.9241034
## RATERISK_num                1.5116333
## AGE_STDZ                    4.6902943
## AGExPRIORFRAC               2.6647241
## MOMFRACxARMASSIST           0.3369723
## PRIORFRACxAGE_STDZ          2.8592081
## NOPRIORFRACxAGE_STDZ        4.8683982
# Plot variable importance
varImpPlot(rf_model)


# RANDOM FOREST
# Ensure FRACTURE is a factor if it's categorical
GLOW_data$FRACTURE <- as.factor(GLOW_data$FRACTURE)

# Build the random forest model
rf_model <- randomForest(FRACTURE ~ ., data=GLOW_data, ntree=500, importance=TRUE)

# Print the importance of each variable
print(importance(rf_model))
##                                0          1 MeanDecreaseAccuracy
## SUB_ID               60.16663720 67.4610174          65.34765401
## SITE_ID               1.30569171 -1.6626086           0.11624490
## PHY_ID                2.89723929 -1.8107504           1.56372457
## PRIORFRAC             3.05775685 -0.8151036           2.04340106
## AGE                   6.10992332  0.1221043           5.43553008
## WEIGHT                5.64673538 -1.5585010           4.36418179
## HEIGHT                1.63782194  0.7700751           1.67873246
## BMI                   8.18352552 -0.7434904           7.34504388
## PREMENO              -0.78279524 -0.3512526          -0.82418082
## MOMFRAC               0.82064084 -0.4710652           0.44331323
## ARMASSIST             2.65380873 -0.5908957           1.85284397
## SMOKE                 1.22911986  0.3556638           1.23176302
## RATERISK             -0.08129908  0.7318728           0.29254444
## FRACSCORE             5.38718149  4.0868331           7.00321557
## BONEMED               6.10597708  0.3166537           5.46981026
## BONEMED_FU            5.63340861  0.7353121           5.36272155
## BONETREAT             3.06225394 -1.0757157           2.13524203
## RATERISK_EQ_3         2.00763766  0.5269715           2.09121189
## RATERISK_num         -0.92198682  1.7590908           0.52276713
## AGE_STDZ              4.86638369  0.3492855           4.29208421
## AGExPRIORFRAC         3.27763723 -1.7006615           1.67031301
## MOMFRACxARMASSIST    -0.41999782  0.8433497           0.04198686
## PRIORFRACxAGE_STDZ    1.63446093 -2.2570610           0.10056339
## NOPRIORFRACxAGE_STDZ  3.52678142  4.0267914           5.45518575
##                      MeanDecreaseGini
## SUB_ID                    122.1622138
## SITE_ID                     2.2593862
## PHY_ID                      5.2316616
## PRIORFRAC                   1.6909397
## AGE                         4.5117954
## WEIGHT                      5.4508460
## HEIGHT                      5.8765275
## BMI                         6.3249037
## PREMENO                     0.7445076
## MOMFRAC                     1.3813428
## ARMASSIST                   1.0533234
## SMOKE                       0.4489627
## RATERISK                    1.6999218
## FRACSCORE                   5.4248189
## BONEMED                     0.9983017
## BONEMED_FU                  1.7777501
## BONETREAT                   0.8504035
## RATERISK_EQ_3               0.9447167
## RATERISK_num                1.5866790
## AGE_STDZ                    4.5215574
## AGExPRIORFRAC               2.8864676
## MOMFRACxARMASSIST           0.4285930
## PRIORFRACxAGE_STDZ          2.8623326
## NOPRIORFRACxAGE_STDZ        4.5978523
# Plot variable importance
varImpPlot(rf_model)


# Random Forest W SEED
GLOW_data$FRACTURE <- as.factor(GLOW_data$FRACTURE)
set.seed(123)  # For reproducibility
rf_model <- randomForest(FRACTURE ~ ., data=GLOW_data, ntree=500, importance=TRUE)
importance(rf_model)  # Shows importance score for each variable
##                               0          1 MeanDecreaseAccuracy
## SUB_ID               57.7788117 64.7201775          63.22439311
## SITE_ID               3.0265127 -1.2136723           2.13673684
## PHY_ID                1.5228799 -0.7425820           0.80350194
## PRIORFRAC             2.3205249  0.7059509           2.36535767
## AGE                   5.1009957  0.5552545           4.89401905
## WEIGHT                6.2717122 -1.0568195           5.26060721
## HEIGHT                4.1288809  2.3824576           4.72345294
## BMI                   7.2333438 -1.5298134           5.91426141
## PREMENO              -0.9159542  0.7758452          -0.02627726
## MOMFRAC               1.9913930 -1.3713291           0.85247675
## ARMASSIST             1.1244073 -0.1765833           0.74740612
## SMOKE                 0.5666124 -0.4811641           0.26609402
## RATERISK             -1.2362910  1.4672427          -0.07975750
## FRACSCORE             6.1613400  2.0099281           6.82616729
## BONEMED               5.3839874 -0.9626865           4.82202599
## BONEMED_FU            5.0975261  1.8355321           5.11289900
## BONETREAT             2.6006868  1.2425631           2.81217249
## RATERISK_EQ_3         2.3659117 -1.5811427           1.07816317
## RATERISK_num          0.2026713 -1.4906767          -0.90332043
## AGE_STDZ              3.5561482  0.6940959           3.81292352
## AGExPRIORFRAC         3.1599722 -0.8063485           2.26311639
## MOMFRACxARMASSIST    -1.4565590 -0.2946979          -1.26243862
## PRIORFRACxAGE_STDZ    2.3032139 -1.7357001           1.07800326
## NOPRIORFRACxAGE_STDZ  4.7749760  1.2349391           5.08783678
##                      MeanDecreaseGini
## SUB_ID                    121.6988191
## SITE_ID                     2.3330145
## PHY_ID                      5.6066219
## PRIORFRAC                   1.6651829
## AGE                         4.8114544
## WEIGHT                      5.5597058
## HEIGHT                      5.7925347
## BMI                         6.5129065
## PREMENO                     0.7395337
## MOMFRAC                     1.3454730
## ARMASSIST                   1.0115536
## SMOKE                       0.4689098
## RATERISK                    1.6297359
## FRACSCORE                   5.4296917
## BONEMED                     1.1499490
## BONEMED_FU                  1.8871480
## BONETREAT                   0.7417384
## RATERISK_EQ_3               0.9478571
## RATERISK_num                1.5842613
## AGE_STDZ                    4.8099313
## AGExPRIORFRAC               2.8208520
## MOMFRACxARMASSIST           0.4396781
## PRIORFRACxAGE_STDZ          2.8700394
## NOPRIORFRACxAGE_STDZ        4.4753365
varImpPlot(rf_model)  # Plots variable importance


# PRINCIPAL COMPONENT ANALYSIS

library(FactoMineR)
# Select only numeric columns for PCA
numerical_data <- GLOW_data[sapply(GLOW_data, is.numeric)]

# Perform PCA
res.pca <- PCA(numerical_data, graph=FALSE)

# Print PCA results
print(res.pca)
## **Results for the Principal Component Analysis (PCA)**
## The analysis was performed on 500 individuals, described by 19 variables
## *The results are available in the following objects:
## 
##    name               description                          
## 1  "$eig"             "eigenvalues"                        
## 2  "$var"             "results for the variables"          
## 3  "$var$coord"       "coord. for the variables"           
## 4  "$var$cor"         "correlations variables - dimensions"
## 5  "$var$cos2"        "cos2 for the variables"             
## 6  "$var$contrib"     "contributions of the variables"     
## 7  "$ind"             "results for the individuals"        
## 8  "$ind$coord"       "coord. for the individuals"         
## 9  "$ind$cos2"        "cos2 for the individuals"           
## 10 "$ind$contrib"     "contributions of the individuals"   
## 11 "$call"            "summary statistics"                 
## 12 "$call$centre"     "mean of the variables"              
## 13 "$call$ecart.type" "standard error of the variables"    
## 14 "$call$row.w"      "weights for the individuals"        
## 15 "$call$col.w"      "weights for the variables"
# Optionally, plot the PCA
plot(res.pca, choix="var") # For variable contributions


plot(res.pca, choix="ind") # For individual (observation) contributions


# COMPUTING CORRELATION COEFFICIENTS
# Ensure FRACTURE is numeric for correlation computation
GLOW_data$FRACTURE <- as.numeric(as.factor(GLOW_data$FRACTURE)) - 1

# Re-run correlation with FRACTURE included if it's binary numeric
numerical_vars <- sapply(GLOW_data, is.numeric)  # Re-check numerical variables including FRACTURE
correlations <- cor(GLOW_data[, numerical_vars], use="pairwise.complete.obs")  # Compute the correlation matrix
fracture_correlations <- correlations[,"FRACTURE", drop = FALSE]  # Extract correlations with FRACTURE
print(fracture_correlations)
##                         FRACTURE
## SUB_ID                0.75000150
## SITE_ID               0.06935643
## PHY_ID                0.06745920
## PRIORFRAC             0.21808819
## AGE                   0.20765352
## WEIGHT               -0.03625944
## HEIGHT               -0.13640055
## BMI                   0.01498506
## MOMFRAC               0.10643875
## ARMASSIST             0.15256788
## SMOKE                -0.03167940
## FRACSCORE             0.26447951
## FRACTURE              1.00000000
## RATERISK_EQ_3         0.12419080
## RATERISK_num          0.15173188
## AGE_STDZ              0.20765352
## AGExPRIORFRAC         0.09727651
## MOMFRACxARMASSIST     0.05827942
## PRIORFRACxAGE_STDZ    0.09727651
## NOPRIORFRACxAGE_STDZ  0.18931686
# Computing Correlation Coefficients:
# GLOW_data is our dataset and FRACTURE is our binary target variable
numerical_vars <- sapply(GLOW_data, is.numeric)  # Identify numerical variables
correlations <- cor(GLOW_data[, numerical_vars])  # Compute the correlation matrix

# Extract the correlations of all variables with FRACTURE
fracture_correlations <- correlations[,"FRACTURE", drop = FALSE]  # Preserves the dataframe structure
sorted_correlations <- sort(fracture_correlations, decreasing = TRUE)  # Sort by absolute value

print(sorted_correlations)
##  [1]  1.00000000  0.75000150  0.26447951  0.21808819  0.20765352  0.20765352
##  [7]  0.18931686  0.15256788  0.15173188  0.12419080  0.10643875  0.09727651
## [13]  0.09727651  0.06935643  0.06745920  0.05827942  0.01498506 -0.03167940
## [19] -0.03625944 -0.13640055
# FEATURE SELECTION
# Recursive Feature Elimination (RFE) to Select Predictive Variables:

# FRACTURE is our first column
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(GLOW_data[, -1], GLOW_data[, 1], 
               sizes=c(1:5), rfeControl=control)

print(results)
## 
## Recursive feature selection
## 
## Outer resampling method: Cross-Validated (10 fold) 
## 
## Resampling performance over subset size:
## 
##  Variables   RMSE Rsquared   MAE RMSESD RsquaredSD MAESD Selected
##          1  95.47   0.5639 78.23  3.750    0.04155 3.786        *
##          2  96.81   0.5543 79.17  3.930    0.04536 3.745         
##          3  97.60   0.5498 79.88  2.353    0.04308 2.787         
##          4  98.88   0.5407 81.12  2.619    0.03719 3.472         
##          5 100.13   0.5392 83.28  3.284    0.03631 4.451         
##         24  98.37   0.5374 80.03  3.916    0.04202 3.995         
## 
## The top 1 variables (out of 1):
##    FRACTURE
# CHI SQUARED

#Chi-Squared Test for Categorical Variables: to see their relationship with the binary target FRACTURE, we perform a chi-squared test for each categorical variable:
# Identify categorical variables
categorical_vars <- sapply(GLOW_data, is.factor) | sapply(GLOW_data, is.character)

# Names of categorical variables
categorical_var_names <- names(GLOW_data)[categorical_vars]

# Perform a Chi-squared test for each categorical variable
for(var in categorical_var_names) {
  tryCatch({
    cat_table <- table(GLOW_data[[var]], GLOW_data$FRACTURE)
    
    # Ensure the table has more than one level for both rows and columns
    if (all(dim(cat_table) > 1)) {
      chi_res <- chisq.test(cat_table)
      print(paste("Chi-squared test for variable:", var))
      print(chi_res)
    } else {
      print(paste("Variable", var, "cannot be tested due to insufficient data or lack of variability."))
    }
  }, error = function(e) {
    print(paste("Error in chi-squared test for variable:", var))
    print(e)
  })
}
## [1] "Chi-squared test for variable: PREMENO"
## 
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  cat_table
## X-squared = 0.0042636, df = 1, p-value = 0.9479
## 
## [1] "Chi-squared test for variable: RATERISK"
## 
##  Pearson's Chi-squared test
## 
## data:  cat_table
## X-squared = 11.547, df = 2, p-value = 0.003109
## 
## [1] "Chi-squared test for variable: BONEMED"
## 
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  cat_table
## X-squared = 9.7822, df = 1, p-value = 0.001762
## 
## [1] "Chi-squared test for variable: BONEMED_FU"
## 
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  cat_table
## X-squared = 16.743, df = 1, p-value = 4.279e-05
## 
## [1] "Chi-squared test for variable: BONETREAT"
## 
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  cat_table
## X-squared = 5.9159, df = 1, p-value = 0.015
# NONPARAMETRIC
# Decision Tree w rpart

# Split the data into training and testing sets
set.seed(123) # For reproducibility
indices <- sample(1:nrow(GLOW_data), size = 0.7 * nrow(GLOW_data))
train_data <- GLOW_data[indices, ]
test_data <- GLOW_data[-indices, ]

# Fit the decision tree model
model <- rpart(FRACTURE ~ ., data = train_data, method = "class")

# Summary of the model
summary(model)
## Call:
## rpart(formula = FRACTURE ~ ., data = train_data, method = "class")
##   n= 350 
## 
##     CP nsplit rel error     xerror       xstd
## 1 1.00      0         1 1.00000000 0.09437989
## 2 0.01      1         0 0.02352941 0.01659020
## 
## Variable importance
##             SUB_ID          FRACSCORE      AGExPRIORFRAC PRIORFRACxAGE_STDZ 
##                 79                  6                  6                  6 
##             HEIGHT 
##                  4 
## 
## Node number 1: 350 observations,    complexity param=1
##   predicted class=0  expected loss=0.2428571  P(node) =1
##     class counts:   265    85
##    probabilities: 0.757 0.243 
##   left son=2 (265 obs) right son=3 (85 obs)
##   Primary splits:
##       SUB_ID               < 375.5       to the left,  improve=128.714300, (0 missing)
##       FRACSCORE            < 4.5         to the left,  improve= 10.000520, (0 missing)
##       AGExPRIORFRAC        < 0.7717861   to the left,  improve=  9.964286, (0 missing)
##       PRIORFRACxAGE_STDZ   < 0.7717861   to the left,  improve=  9.964286, (0 missing)
##       NOPRIORFRACxAGE_STDZ < -0.03125856 to the left,  improve=  9.575968, (0 missing)
##   Surrogate splits:
##       FRACSCORE          < 7.5         to the left,  agree=0.777, adj=0.082, (0 split)
##       AGExPRIORFRAC      < 0.7717861   to the left,  agree=0.774, adj=0.071, (0 split)
##       PRIORFRACxAGE_STDZ < 0.7717861   to the left,  agree=0.774, adj=0.071, (0 split)
##       HEIGHT             < 151.5       to the right, agree=0.769, adj=0.047, (0 split)
## 
## Node number 2: 265 observations
##   predicted class=0  expected loss=0  P(node) =0.7571429
##     class counts:   265     0
##    probabilities: 1.000 0.000 
## 
## Node number 3: 85 observations
##   predicted class=1  expected loss=0  P(node) =0.2428571
##     class counts:     0    85
##    probabilities: 0.000 1.000
# Predict on the test data
predictions <- predict(model, test_data, type = "class")

# Evaluate the model
table(Predicted = predictions, Actual = test_data$FRACTURE)
##          Actual
## Predicted   0   1
##         0 110   0
##         1   0  40
# Confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_data$FRACTURE)

# Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Precision
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])

# Recall
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])

# F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print(paste("Accuracy:", accuracy))
## [1] "Accuracy: 1"
print(paste("Precision:", precision))
## [1] "Precision: 1"
print(paste("Recall:", recall))
## [1] "Recall: 1"
print(paste("F1 Score:", f1_score))
## [1] "F1 Score: 1"
# Not great results here
# Lets create a model with variables :  FRACSCORE, WEIGHT, BMI, HEIGHT, and NOPRIORFRACxAGE_STDZ   and then one that also includes AGExPRIORFRAC  to test 

# Model 1 without 'AGExPRIORFRAC'
# Define the formula for the model without AGExPRIORFRAC
formula1 <- FRACTURE ~ FRACSCORE + WEIGHT + BMI + HEIGHT
# Train the model on the training data
model1 <- rpart(formula1, data = train_data, method = "class")

# Predict on the test data
predictions1 <- predict(model1, test_data, type = "class")

# Evaluate the model
confusion_matrix1 <- table(Predicted = predictions1, Actual = test_data$FRACTURE)
accuracy1 <- sum(diag(confusion_matrix1)) / sum(confusion_matrix1)

# Print the results
print(paste("Accuracy for Model 1:", accuracy1))
## [1] "Accuracy for Model 1: 0.666666666666667"
# Model 2 with 'AGExPRIORFRAC'
# Define the formula for the model with AGExPRIORFRAC
formula2 <- FRACTURE ~ FRACSCORE + WEIGHT + BMI + HEIGHT + AGExPRIORFRAC

# Train the model on the training data
model2 <- rpart(formula2, data = train_data, method = "class")

# Predict on the test data
predictions2 <- predict(model2, test_data, type = "class")

# Evaluate the model
confusion_matrix2 <- table(Predicted = predictions2, Actual = test_data$FRACTURE)
accuracy2 <- sum(diag(confusion_matrix2)) / sum(confusion_matrix2)

# Print the results
print(paste("Accuracy for Model 2:", accuracy2))
## [1] "Accuracy for Model 2: 0.666666666666667"
# Model 3 with AGExPRIORFRAC and MOMFRACxARMASSIST--as well as AGE, HEIGHT,  PRIORFRAC, MOMFRAC, ARMASSIST, and RATERISK_EQ_3.
# Split the data into training and testing sets
set.seed(123)  # for reproducibility
indices <- sample(1:nrow(GLOW_data), size = 0.8 * nrow(GLOW_data))
train_data <- GLOW_data[indices, ]
test_data <- GLOW_data[-indices, ]

# Define the model formula
formula <- FRACTURE ~ AGExPRIORFRAC + MOMFRACxARMASSIST + AGE + HEIGHT + PRIORFRAC + MOMFRAC + ARMASSIST + RATERISK_EQ_3

# Train the model on the training data
model <- rpart(formula, data = train_data, method = "class")

# Predict on the test data
predictions <- predict(model, test_data, type = "class")

# Evaluate the model
confusion_matrix <- table(Predicted = predictions, Actual = test_data$FRACTURE)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the confusion matrix and accuracy
print(confusion_matrix)
##          Actual
## Predicted  0  1
##         0 60 24
##         1  8  8
print(paste("Accuracy:", accuracy))
## [1] "Accuracy: 0.68"
# Now using only FRACSCORE, AGExPRIORFRAC, MOMFRACxARMASSIST

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
indices <- sample(1:nrow(GLOW_data), size = 0.8 * nrow(GLOW_data))
train_data <- GLOW_data[indices, ]
test_data <- GLOW_data[-indices, ]

# Define the model formula with the specified variables
formula <- FRACTURE ~ FRACSCORE + AGExPRIORFRAC + MOMFRACxARMASSIST

# Train the model on the training data
model <- rpart(formula, data = train_data, method = "class")

# Predict on the test data
predictions <- predict(model, test_data, type = "class")

# Evaluate the model
confusion_matrix <- table(Predicted = predictions, Actual = test_data$FRACTURE)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the confusion matrix and accuracy
print(confusion_matrix)
##          Actual
## Predicted  0  1
##         0 65 28
##         1  3  4
print(paste("Accuracy:", accuracy))
## [1] "Accuracy: 0.69"
# Rename factor levels for FRACTURE
glow_bonemed_NEW$FRACTURE <- factor(glow_bonemed_NEW$FRACTURE, levels = c("0", "1"), labels = c("Class0", "Class1"))

# Confirm the change
print(table(glow_bonemed_NEW$FRACTURE))  # This should now show the renamed classes
## 
## Class0 Class1 
##    375    125
# Set seed for reproducibility
set.seed(123)

# Splitting the data into training and testing sets again
trainIndex <- createDataPartition(glow_bonemed_NEW$FRACTURE, p = 0.8, list = FALSE)
train_data <- glow_bonemed_NEW[trainIndex, ]
test_data <- glow_bonemed_NEW[-trainIndex, ]

# Verifying that FRACTURE is included and properly formatted
head(train_data$FRACTURE)
## [1] Class0 Class0 Class0 Class0 Class0 Class0
## Levels: Class0 Class1
head(test_data$FRACTURE)
## [1] Class0 Class0 Class0 Class0 Class0 Class0
## Levels: Class0 Class1
## Set seed for reproducibility
set.seed(123)

# Define training control
train_control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)

# Train the model using caret with cross-validation
model_caret <- train(FRACTURE ~ ., data = glow_bonemed_NEW, method = "rpart",
                     trControl = train_control, tuneLength = 10)

# Print the best model's results
print(model_caret)
## CART 
## 
## 500 samples
##  24 predictor
##   2 classes: 'Class0', 'Class1' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 449, 449, 451, 451, 449, 451, ... 
## Resampling results across tuning parameters:
## 
##   cp         Accuracy   Kappa    
##   0.0000000  0.9979592  0.9946331
##   0.1111111  0.9979592  0.9946331
##   0.2222222  0.9979592  0.9946331
##   0.3333333  0.9979592  0.9946331
##   0.4444444  0.9979592  0.9946331
##   0.5555556  0.9979592  0.9946331
##   0.6666667  0.9979592  0.9946331
##   0.7777778  0.9979592  0.9946331
##   0.8888889  0.9979592  0.9946331
##   1.0000000  0.7501000  0.0000000
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was cp = 0.8888889.
# Ensure test data FRACTURE is also a factor (if it's not already)
test_data$FRACTURE <- factor(test_data$FRACTURE)

# Predict on the test data
predictions <- predict(model_caret, newdata = test_data, type = "raw")

# Evaluate the model using confusionMatrix from caret
conf_matrix <- confusionMatrix(predictions, test_data$FRACTURE)
print(conf_matrix)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction Class0 Class1
##     Class0     75      0
##     Class1      0     25
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9638, 1)
##     No Information Rate : 0.75       
##     P-Value [Acc > NIR] : 3.207e-13  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
##                                      
##             Sensitivity : 1.00       
##             Specificity : 1.00       
##          Pos Pred Value : 1.00       
##          Neg Pred Value : 1.00       
##              Prevalence : 0.75       
##          Detection Rate : 0.75       
##    Detection Prevalence : 0.75       
##       Balanced Accuracy : 1.00       
##                                      
##        'Positive' Class : Class0     
## 
# Model importance
importance <- varImp(model_caret, scale = FALSE)
print(importance)
## rpart variable importance
## 
##   only 20 most important variables shown (out of 25)
## 
##                      Overall
## SUB_ID               187.500
## NOPRIORFRACxAGE_STDZ  10.974
## FRACSCORE             10.202
## PRIORFRAC              8.918
## AGE                    7.261
## PREMENOYes             0.000
## RATERISK_EQ_3          0.000
## HEIGHT                 0.000
## PHY_ID                 0.000
## SMOKE                  0.000
## AGExPRIORFRAC          0.000
## AGE_STDZ               0.000
## WEIGHT                 0.000
## MOMFRAC                0.000
## BONETREATYes           0.000
## PRIORFRACxAGE_STDZ     0.000
## RATERISKSame           0.000
## ARMASSIST              0.000
## BMI                    0.000
## BONEMEDYes             0.000
plot(importance)


# Probability predictions for ROC curve
prob_predictions <- predict(model_caret, newdata = test_data, type = "prob")
roc_curve <- roc(response = test_data$FRACTURE, predictor = prob_predictions$Class1)
## Setting levels: control = Class0, case = Class1
## Setting direction: controls < cases
plot(roc_curve)

# Check the current size of classes in training data
table(train_data$FRACTURE)
## 
## Class0 Class1 
##    300    100
# Apply SMOTE to balance the classes, ensuring we have an even number of cases for each class
# Here we calculate the number of cases needed to balance the classes
majority_size <- max(table(train_data$FRACTURE))
minority_size <- min(table(train_data$FRACTURE))
desired_size <- 2 * majority_size  # Desired total size after oversampling

# Using SMOTE for oversampling the minority class
if (minority_size < majority_size) {
  smote_data <- ovun.sample(FRACTURE ~ ., data = train_data, method = "over", N = desired_size, seed = 123)$data
} else {
  smote_data <- train_data  # No need for oversampling if classes are balanced
}

# Check the new balance of the dataset after SMOTE
table(smote_data$FRACTURE)
## 
## Class0 Class1 
##    300    300
# Retrain the model using the balanced dataset
balanced_model <- train(FRACTURE ~ ., data = smote_data, method = "rpart",
                        trControl = train_control, tuneLength = 10)


# Predict on the original test set
balanced_predictions <- predict(balanced_model, newdata = test_data, type = "raw")

# Confusion matrix to evaluate the model
balanced_conf_matrix <- confusionMatrix(balanced_predictions, test_data$FRACTURE)
print(balanced_conf_matrix)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction Class0 Class1
##     Class0     75      0
##     Class1      0     25
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9638, 1)
##     No Information Rate : 0.75       
##     P-Value [Acc > NIR] : 3.207e-13  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
##                                      
##             Sensitivity : 1.00       
##             Specificity : 1.00       
##          Pos Pred Value : 1.00       
##          Neg Pred Value : 1.00       
##              Prevalence : 0.75       
##          Detection Rate : 0.75       
##    Detection Prevalence : 0.75       
##       Balanced Accuracy : 1.00       
##                                      
##        'Positive' Class : Class0     
## 
# Probability predictions for ROC curve
balanced_prob_predictions <- predict(balanced_model, newdata = test_data, type = "prob")
balanced_roc_curve <- roc(response = test_data$FRACTURE, predictor = balanced_prob_predictions$Class1)
## Setting levels: control = Class0, case = Class1
## Setting direction: controls < cases
plot(balanced_roc_curve)


# Model importance
balanced_importance <- varImp(balanced_model, scale = FALSE)
print(balanced_importance)
## rpart variable importance
## 
##   only 20 most important variables shown (out of 25)
## 
##                      Overall
## SUB_ID                300.00
## PRIORFRAC              33.40
## NOPRIORFRACxAGE_STDZ   32.83
## FRACSCORE              27.65
## BONEMED_FUYes          21.58
## RATERISK_num            0.00
## RATERISK_EQ_3           0.00
## MOMFRACxARMASSIST       0.00
## MOMFRAC                 0.00
## AGE_STDZ                0.00
## SMOKE                   0.00
## AGE                     0.00
## SITE_ID                 0.00
## PHY_ID                  0.00
## HEIGHT                  0.00
## WEIGHT                  0.00
## BONEMEDYes              0.00
## RATERISKGreater         0.00
## RATERISKSame            0.00
## BONETREATYes            0.00
plot(balanced_importance)


# Model Iteration
# Adjust dataset based on feature importance if necessary # For example, dropping a less important feature:

# train_data_adjusted <- train_data[, !(names(train_data) %in% c("LEAST_IMPORTANT_FEATURE"))]
# test_data_adjusted <- test_data[, !(names(test_data) %in% c("LEAST_IMPORTANT_FEATURE"))]

# Retrain the model on the adjusted data
# model_adjusted <- train(FRACTURE ~ ., data = train_data_adjusted, method = "rpart",
#                        trControl = train_control, tuneLength = 10)


# Cross-Validation Reevaluation
# Adjusted training control with class probabilities
# train_control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)

# Train the models (for both cv_model and rf_model, this is just a placeholder for the complete training code)

# Predict probabilities from both models
# cv_prob_predictions <- predict(cv_model, newdata = test_data_adjusted, type = "prob")
# rf_prob_predictions <- predict(rf_model, newdata = test_data_adjusted, type = "prob")

# Create ensemble predictions
# ensemble_prob <- (cv_prob_predictions$Class1 + rf_prob_predictions$Class1) / 2
# ensemble_predictions <- ifelse(ensemble_prob > 0.5, "Class1", "Class0")

# Evaluate ensemble model
# ensemble_conf_matrix <- confusionMatrix(as.factor(ensemble_predictions), test_data_adjusted$FRACTURE)
# print(ensemble_conf_matrix)

# Calculate different performance metrics
# conf_matrix <- confusionMatrix(predictions, test_data$FRACTURE)
# print(conf_matrix$byClass)  # Gives you Precision, Recall, F1 score etc.
# CV
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = "final", classProbs = TRUE)
model <- train(FRACTURE ~ ., data = train_data, method = "rf", trControl = train_control)
# Feature Importance Analysis
importance <- varImp(model, scale = FALSE)
print(importance)
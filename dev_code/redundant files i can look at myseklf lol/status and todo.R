next steps:  Review code.  

1. complete pending lines in code:
2. decision tree
3. regularization with lasso: complete the evaluation we would predict on the validation set and calculate the AUC to see if there's an improvement
4. find a way to improve models performace evaluation
5. test to see and if non-linear transformations of age (polynomial or spline) don'tt improve the model much, it might be best to stick with the simpler model for the sake of interpretability, unless domain knowledge strongly suggests a non-linear relationship.
6. possible further feature engineering.   e.g., including clinically relevant interactions that make sense given the subject matter, even though the project outline advises against complex models for now. - thi sis just for us to test top make sure we hit the marks to get an A
7. Decision trees and lasso regression might reveal interesting patterns or feature importance, so consider reevalutaing and working these further to see  if they can offer insights that a logistic regression model cannot.
8. Documenting model selection, rationale: 
    Clearly state the reasons for each model's selection or exclusion based on AIC, p-values, and AUC results.
    Discuss the balance between model complexity and interpretability, especially when simpler models provide almost as much predictive power as more complex ones.
    Consider the clinical relevance of each variable and how well the model with those variables aligns with clinical intuition or existing literature.
    Mention the potential for model overfitting with more complex models and the trade-offs associated with it.
    If using a simpler model, explain why it might be preferable in a clinical setting, such as ease of use or understanding by practitioners.
9. NOTE:  final model selection should not only be guided by statistical metrics but also by its utility in practice, ease of interpretation, and alignment with project objectives
10. make adjustments - clean it up, and finalize objective 1 with formal writeup and visual;izations.  
11. begin objective 2. 


# summary of what's been done along with the outcomes:

### Polynomial Model
added a squared term for age to your model, which can help capture non-linear relationships. However, the p-values for the age terms suggest that this polynomial transformation is not statistically significant, and the AIC is slightly higher than the simple logistic model with only age and BMI, indicating no improvement.

### Splines Model
fitted a model with spline terms for age to allow for more flexibility in the relationship between age and the probability of fracture. Some of the spline terms are statistically significant, which implies that there might be a non-linear relationship between age and the outcome. However, the AIC for this model is also slightly higher, suggesting no substantial improvement over the simple model.

### Decision Tree
A decision tree model was fitted, which is a non-linear approach and can capture more complex patterns. To evaluate the decision tree, we would use similar ROC curve and AUC calculations.  This needs to be done.  


### Regularization with Lasso
Lasso regression was performed, which is particularly useful when we suspect some of the predictors may not contribute much to the outcome and we want to perform feature selection automatically. I  selected the lambda that minimizes the cross-validation error. NOTE  to complete the evaluation we would predict on the validation set and calculate the AUC to see if there's an improvement.

### Model Performance Evaluation with ROC Curve
The AUC for the polynomial model is 0.5451, which is close to the original logistic regression model. It's still not particularly high, and it shows that while the model has some predictive power, it's not strong.



R version 4.3.2 (2023-10-31 ucrt) -- "Eye Holes"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> source("~/.active-rstudio-document", echo=TRUE)

> #sink("output5.txt")
> # Load the aplore3 package and the dataset
> library(aplore3)

> data("glow_bonemed")

> # View the first few rows to understand the data structure
> head(glow_bonemed)
  sub_id site_id phy_id priorfrac age weight height      bmi premeno momfrac armassist smoke raterisk fracscore fracture bonemed bonemed_fu bonetreat
1      1       1     14        No  62   70.3    158 28.16055      No      No        No    No     Same         1       No      No         No        No
2      2       4    284        No  65   87.1    160 34.02344      No      No        No    No     Same         2       No      No         No        No
3      3       6    305       Yes  88   50.8    157 20.60936      No     Yes       Yes    No     Less        11       No      No         No        No
4      4       6    309        No  82   62.1    160 24.25781      No      No        No    No     Less         5       No      No         No        No
5      5       1     37        No  61   68.0    152 29.43213      No      No        No    No     Same         1       No      No         No        No
6      6       5    299       Yes  67   68.0    161 26.23356      No      No        No   Yes     Same         4       No      No         No        No

> # Summary statistics to get an overview (mean, median, min, max, etc.)
> summary(glow_bonemed)
     sub_id         site_id          phy_id       priorfrac      age            weight           height           bmi        premeno   momfrac   armassist smoke        raterisk  
 Min.   :  1.0   Min.   :1.000   Min.   :  1.00   No :374   Min.   :55.00   Min.   : 39.90   Min.   :134.0   Min.   :14.88   No :403   No :435   No :312   No :465   Less   :167  
 1st Qu.:125.8   1st Qu.:2.000   1st Qu.: 57.75   Yes:126   1st Qu.:61.00   1st Qu.: 59.90   1st Qu.:157.0   1st Qu.:23.27   Yes: 97   Yes: 65   Yes:188   Yes: 35   Same   :186  
 Median :250.5   Median :3.000   Median :182.50             Median :67.00   Median : 68.00   Median :161.5   Median :26.42                                           Greater:147  
 Mean   :250.5   Mean   :3.436   Mean   :178.55             Mean   :68.56   Mean   : 71.82   Mean   :161.4   Mean   :27.55                                                        
 3rd Qu.:375.2   3rd Qu.:5.000   3rd Qu.:298.00             3rd Qu.:76.00   3rd Qu.: 81.30   3rd Qu.:165.0   3rd Qu.:30.79                                                        
 Max.   :500.0   Max.   :6.000   Max.   :325.00             Max.   :90.00   Max.   :127.00   Max.   :199.0   Max.   :49.08                                                        
   fracscore      fracture  bonemed   bonemed_fu bonetreat
 Min.   : 0.000   No :375   No :371   No :361    No :382  
 1st Qu.: 2.000   Yes:125   Yes:129   Yes:139    Yes:118  
 Median : 3.000                                           
 Mean   : 3.698                                           
 3rd Qu.: 5.000                                           
 Max.   :11.000                                           

> # Check for missing values
> sum(is.na(glow_bonemed))
[1] 0

> # Visualize distributions of numeric variables
> library(ggplot2)

> # Histogram for the 'age' variable
> ggplot(glow_bonemed, aes(x = age)) + 
+   geom_histogram(binwidth = 5, fill = "blue", color = "black") +
+   th .... [TRUNCATED] 

> # For categorical variables, use a bar plot to see the distribution
> # Bar plot for the 'priorfrac' variable
> ggplot(glow_bonemed, aes(x = priorfr .... [TRUNCATED] 

> # Fit a GLM - Example with a binary outcome
> # Fitting a GLM with fracture as the response and age, bmi, fracscore as predictors
> glm_model <- glm .... [TRUNCATED] 

> # Summary of the GLM model
> summary(glm_model)

Call:
glm(formula = fracture ~ age + bmi + fracscore, family = binomial, 
    data = glow_bonemed)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.57486    1.59666  -0.986   0.3240    
age         -0.01850    0.02399  -0.771   0.4405    
bmi          0.01791    0.01825   0.982   0.3263    
fracscore    0.31247    0.08644   3.615   0.0003 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 562.34  on 499  degrees of freedom
Residual deviance: 525.60  on 496  degrees of freedom
AIC: 533.6

Number of Fisher Scoring iterations: 4


> # is 'fracture' is the binary outcome variable I want to use?
> # 'age', 'bmi', and 'fracscore' as predictor variables?
> 
> 
> # logistic regressio .... [TRUNCATED] 

> # Summary of the logistic regression model
> summary(logistic_model)

Call:
glm(formula = fracture ~ age + bmi + fracscore, family = binomial(link = "logit"), 
    data = glow_bonemed)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.57486    1.59666  -0.986   0.3240    
age         -0.01850    0.02399  -0.771   0.4405    
bmi          0.01791    0.01825   0.982   0.3263    
fracscore    0.31247    0.08644   3.615   0.0003 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 562.34  on 499  degrees of freedom
Residual deviance: 525.60  on 496  degrees of freedom
AIC: 533.6

Number of Fisher Scoring iterations: 4


> # Run the linear regression model using GLM with the gaussian family for a continuous outcome
> glm_model_continuous <- glm(bmi ~ age + fracscore, f .... [TRUNCATED] 

> # Summary of the linear regression model
> summary(glm_model_continuous)

Call:
glm(formula = bmi ~ age + fracscore, family = gaussian, data = glow_bonemed)

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 46.76182    3.35211  13.950  < 2e-16 ***
age         -0.31845    0.05828  -5.465 7.35e-08 ***
fracscore    0.70986    0.20993   3.381 0.000778 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 33.31132)

    Null deviance: 17808  on 499  degrees of freedom
Residual deviance: 16556  on 497  degrees of freedom
AIC: 3176.9

Number of Fisher Scoring iterations: 2


> # Objective 1
> # step 1: train  / validation split
> 
> # Load necessary libraries
> library(caret)
Loading required package: lattice

> # dataset is named `glow_bonemed`
> set.seed(123) # For reproducibility

> trainIndex <- createDataPartition(glow_bonemed$fracture, p = .8, 
+                                   list = FALSE, 
+                               .... [TRUNCATED] 

> trainData <- glow_bonemed[trainIndex, ]

> validationData <- glow_bonemed[-trainIndex, ]

> # STEP 2: EXPLORATORY EDA
> # Summarizing the dataset
> summary(trainData)
     sub_id         site_id          phy_id       priorfrac      age            weight           height           bmi        premeno   momfrac   armassist smoke        raterisk  
 Min.   :  1.0   Min.   :1.000   Min.   :  1.00   No :308   Min.   :55.00   Min.   : 39.90   Min.   :134.0   Min.   :15.02   No :324   No :347   No :250   No :377   Less   :135  
 1st Qu.:128.5   1st Qu.:2.000   1st Qu.: 57.75   Yes: 92   1st Qu.:61.00   1st Qu.: 59.90   1st Qu.:157.0   1st Qu.:23.31   Yes: 76   Yes: 53   Yes:150   Yes: 23   Same   :151  
 Median :256.5   Median :3.000   Median :181.00             Median :67.00   Median : 68.00   Median :162.0   Median :26.39                                           Greater:114  
 Mean   :252.9   Mean   :3.417   Mean   :177.70             Mean   :68.47   Mean   : 72.02   Mean   :161.5   Mean   :27.58                                                        
 3rd Qu.:375.5   3rd Qu.:5.000   3rd Qu.:296.00             3rd Qu.:76.00   3rd Qu.: 81.60   3rd Qu.:165.0   3rd Qu.:30.89                                                        
 Max.   :499.0   Max.   :6.000   Max.   :325.00             Max.   :90.00   Max.   :127.00   Max.   :199.0   Max.   :49.08                                                        
   fracscore      fracture  bonemed   bonemed_fu bonetreat
 Min.   : 0.000   No :300   No :295   No :289    No :305  
 1st Qu.: 2.000   Yes:100   Yes:105   Yes:111    Yes: 95  
 Median : 4.000                                           
 Mean   : 3.647                                           
 3rd Qu.: 5.000                                           
 Max.   :11.000                                           

> str(trainData)
'data.frame':   400 obs. of  18 variables:
 $ sub_id    : int  1 2 4 5 7 9 10 11 12 13 ...
 $ site_id   : int  1 4 6 1 5 1 4 6 1 6 ...
 $ phy_id    : int  14 284 309 37 302 8 282 315 34 315 ...
 $ priorfrac : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 2 1 1 1 1 ...
 $ age       : int  62 65 82 61 84 86 58 67 56 59 ...
 $ weight    : num  70.3 87.1 62.1 68 50.8 ...
 $ height    : int  158 160 160 152 150 156 166 153 167 162 ...
 $ bmi       : num  28.2 34 24.3 29.4 22.6 ...
 $ premeno   : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
 $ momfrac   : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 2 1 1 ...
 $ armassist : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 2 1 ...
 $ smoke     : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 2 2 2 ...
 $ raterisk  : Factor w/ 3 levels "Less","Same",..: 2 2 1 2 1 2 1 1 2 1 ...
 $ fracscore : int  1 2 5 1 6 7 0 4 3 1 ...
 $ fracture  : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
 $ bonemed   : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
 $ bonemed_fu: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
 $ bonetreat : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...

> table(trainData$fracture)

 No Yes 
300 100 

> # Visualizing distributions of numeric variables
> hist(trainData$age, main="Histogram of Age", xlab="Age")

> hist(trainData$bmi, main="Histogram of BMI", xlab="BMI")

> # Relationship between predictor and outcome
> boxplot(age ~ fracture, data = trainData, main="Age by Fracture Status")

> # STEP 3 - feature selection - PENDING PENDING PENDING
> # a: assess variable distributions and relationships
> 
> 
> # step 3.5 - moving along 
> # .... [TRUNCATED] 
                 age        bmi  fracscore
age        1.0000000 -0.2232161  0.8676343
bmi       -0.2232161  1.0000000 -0.1059877
fracscore  0.8676343 -0.1059877  1.0000000

> # Univariate analysis for a single predictor
> summary(glm(fracture ~ age, family = binomial, data = trainData))

Call:
glm(formula = fracture ~ age, family = binomial, data = trainData)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -5.13356    0.93702  -5.479 4.29e-08 ***
age          0.05799    0.01316   4.406 1.05e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 449.87  on 399  degrees of freedom
Residual deviance: 429.70  on 398  degrees of freedom
AIC: 433.7

Number of Fisher Scoring iterations: 4


> # Compute Variance Inflation Factor (VIF) to check for multicollinearity
> # Note that you need to fit a model with all your predictors for VIF
> li .... [TRUNCATED] 
Loading required package: carData

> vif_model <- glm(fracture ~ age + bmi + fracscore, family = binomial, data = trainData)

> vif(vif_model)
      age       bmi fracscore 
 3.954104  1.129971  3.709336 

> # Final decision on features to include
> # This is conceptual; you would include variables based on the outcomes of your analyses
> selected_featur .... [TRUNCATED] 

> # STEP 4:  MLR ANALYSIS:
> # Fitting the logistic regression model
> logitModel <- glm(fracture ~ age + bmi + fracscore, family = binomial, data = t .... [TRUNCATED] 

> # Summary of the model
> summary(logitModel)

Call:
glm(formula = fracture ~ age + bmi + fracscore, family = binomial, 
    data = trainData)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -2.15695    1.81563  -1.188 0.234837    
age         -0.01719    0.02683  -0.641 0.521659    
bmi          0.03068    0.02059   1.490 0.136179    
fracscore    0.34587    0.09709   3.562 0.000368 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 449.87  on 399  degrees of freedom
Residual deviance: 412.40  on 396  degrees of freedom
AIC: 420.4

Number of Fisher Scoring iterations: 4


> # STEP 5: INTERPRETATION OF THE REGRESSION COEFFICENTS:
> # Calculating odds ratios and confidence intervals
> exp(coef(logitModel))
(Intercept)         age         bmi   fracscore 
  0.1156774   0.9829567   1.0311563   1.4132251 

> confint(logitModel)
Waiting for profiling to be done...
                  2.5 %     97.5 %
(Intercept) -5.75227929 1.38764288
age         -0.06997241 0.03550608
bmi         -0.00991338 0.07106372
fracscore    0.15694821 0.53855412

> # STEP 6: COMMENT ON PRACTICAL VERSUS STATISTICAL SIGNIFICANCE: PENDING - PENDING - PENDING
> 
> # STEP 7 VALIDATION
> # Predicting on the validatio .... [TRUNCATED] 

> # Evaluate the model's performance
> library(pROC)
Type 'citation("pROC")' for a citation.

Attaching package: ‘pROC’

The following objects are masked from ‘package:stats’:

    cov, smooth, var


> roc(response = validationData$fracture, predictor = validationData$predictedProb)
Setting levels: control = No, case = Yes
Setting direction: controls > cases

Call:
roc.default(response = validationData$fracture, predictor = validationData$predictedProb)

Data: validationData$predictedProb in 75 controls (validationData$fracture No) > 25 cases (validationData$fracture Yes).
Area under the curve: 0.4203

> # STEP 8: CONCLUSION - summarize findings including eda insights, model interpretation, model performance on the validation set, recomendations and  .... [TRUNCATED] 

> summary(model_age_bmi)

Call:
glm(formula = fracture ~ age + bmi, family = binomial, data = trainData)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -6.87282    1.28364  -5.354 8.60e-08 ***
age          0.06586    0.01393   4.728 2.27e-06 ***
bmi          0.04284    0.02037   2.103   0.0354 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 449.87  on 399  degrees of freedom
Residual deviance: 425.33  on 397  degrees of freedom
AIC: 431.33

Number of Fisher Scoring iterations: 4


> # Model with fracscore and bmi
> model_fracscore_bmi <- glm(fracture ~ fracscore + bmi, family = binomial, data = trainData)

> summary(model_fracscore_bmi)

Call:
glm(formula = fracture ~ fracscore + bmi, family = binomial, 
    data = trainData)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -3.24710    0.64750  -5.015 5.31e-07 ***
fracscore    0.29320    0.05128   5.717 1.08e-08 ***
bmi          0.03452    0.01975   1.747   0.0806 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 449.87  on 399  degrees of freedom
Residual deviance: 412.81  on 397  degrees of freedom
AIC: 418.81

Number of Fisher Scoring iterations: 4


> # Compare AIC values
> AIC(model_age_bmi)
[1] 431.3329

> AIC(model_fracscore_bmi)
[1] 418.813

> # advanced techniques even though not requested here - just for me:
> # Perform PCA on the predictors
> library(FactoMineR)

> pca_res <- PCA(trainData[, c("age", "bmi", "fracscore")], scale.unit = TRUE, ncp = 2, graph = FALSE)

> # Build the model using the principal components
> model_pca <- glm(fracture ~ pca_res$ind$coord[,1] + pca_res$ind$coord[,2], family = binomial, dat .... [TRUNCATED] 

> summary(model_pca)

Call:
glm(formula = fracture ~ pca_res$ind$coord[, 1] + pca_res$ind$coord[, 
    2], family = binomial, data = trainData)

Coefficients:
                       Estimate Std. Error z value Pr(>|z|)    
(Intercept)            -1.21261    0.12705  -9.544  < 2e-16 ***
pca_res$ind$coord[, 1]  0.42567    0.08694   4.896 9.77e-07 ***
pca_res$ind$coord[, 2]  0.36980    0.12365   2.991  0.00278 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 449.87  on 399  degrees of freedom
Residual deviance: 416.80  on 397  degrees of freedom
AIC: 422.8

Number of Fisher Scoring iterations: 4


> # evaluate model performace:
> # Predicting on the validation set with the chosen model
> validationData$predictedProb <- predict(model_age_bmi, new .... [TRUNCATED] 

> # Evaluate the model's performance using ROC curve
> library(pROC)

> roc_result <- roc(response = validationData$fracture, predictor = validationData$predictedProb)
Setting levels: control = No, case = Yes
Setting direction: controls < cases

> auc(roc_result)
Area under the curve: 0.5557

> # selecting final features:
> # This should reflect the outcome of your analyses
> selected_features <- c("age", "bmi") # or c("fracscore", "bmi")

> # Building final model:
> # Final model with selected features
> final_model <- glm(fracture ~ age + bmi, family = binomial, data = trainData) # Upd .... [TRUNCATED] 

> summary(final_model)

Call:
glm(formula = fracture ~ age + bmi, family = binomial, data = trainData)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -6.87282    1.28364  -5.354 8.60e-08 ***
age          0.06586    0.01393   4.728 2.27e-06 ***
bmi          0.04284    0.02037   2.103   0.0354 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 449.87  on 399  degrees of freedom
Residual deviance: 425.33  on 397  degrees of freedom
AIC: 431.33

Number of Fisher Scoring iterations: 4


> # improving model's predicitve performance: - PENDING
> 
> 
> # INTERPRETING MODEL COEFFICIENTS
> # Calculate odds ratios
> odds_ratios <- exp(coef( .... [TRUNCATED] 

> odds_ratios
(Intercept)         age         bmi 
0.001035555 1.068075318 1.043773817 

> # Calculate confidence intervals for the odds ratios
> conf_int <- exp(confint(model_age_bmi))
Waiting for profiling to be done...

> conf_int
                   2.5 %     97.5 %
(Intercept) 7.801067e-05 0.01209624
age         1.039747e+00 1.09824732
bmi         1.002681e+00 1.08633101

> # Feature Engineering: Adding Polynomial Features:
> # Example of adding a squared term for age
> trainData$age_squared <- trainData$age^2

> # Fit a new model with the additional polynomial term
> model_poly <- glm(fracture ~ age + I(age^2) + bmi, family = binomial, data = trainData)

> summary(model_poly)

Call:
glm(formula = fracture ~ age + I(age^2) + bmi, family = binomial, 
    data = trainData)

Coefficients:
              Estimate Std. Error z value Pr(>|z|)  
(Intercept) -3.1512904  7.4352675  -0.424   0.6717  
age         -0.0413255  0.2116302  -0.195   0.8452  
I(age^2)     0.0007543  0.0014870   0.507   0.6120  
bmi          0.0437252  0.0204237   2.141   0.0323 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 449.87  on 399  degrees of freedom
Residual deviance: 425.08  on 396  degrees of freedom
AIC: 433.08

Number of Fisher Scoring iterations: 4


> # Model Complexity: Exploring Non-Linear Models
> # Fit a model using a spline for age
> library(splines)

> model_spline <- glm(fracture ~ ns(age, df = 3) + bmi, family = binomial, data = trainData)

> summary(model_spline)

Call:
glm(formula = fracture ~ ns(age, df = 3) + bmi, family = binomial, 
    data = trainData)

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -2.97439    0.77350  -3.845  0.00012 ***
ns(age, df = 3)1  1.04841    0.52202   2.008  0.04460 *  
ns(age, df = 3)2  1.65724    1.13560   1.459  0.14447    
ns(age, df = 3)3  2.06967    0.55658   3.719  0.00020 ***
bmi               0.04387    0.02046   2.144  0.03205 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 449.87  on 399  degrees of freedom
Residual deviance: 424.77  on 395  degrees of freedom
AIC: 434.77

Number of Fisher Scoring iterations: 4


> # Additional Data: Including More Predictors
> # if 'new_variable' is an additional predictor available in dataset include the new variable in the m .... [TRUNCATED] 

> tree_model <- rpart(fracture ~ age + bmi, data = trainData, method = "class")

> # Regularization: Ridge or Lasso Regression
> # Fit a model using lasso (L1) regularization
> library(glmnet)
Loading required package: Matrix
Loaded glmnet 4.1-8

> x <- as.matrix(trainData[, c("age", "bmi")])

> y <- trainData$fracture

> # cv.glmnet will do cross-validation to find the optimal lambda
> cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1) # alpha = 1 for lasso; a .... [TRUNCATED] 

> lasso_model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = cv_fit$lambda.min)

> # Evaluating Model Performance
> # Predicting on the validation set with the new model
> validationData$predictedProb <- predict(model_poly, newdata .... [TRUNCATED] 

> # Evaluate the model's performance using ROC curve - for each new model evaluate performace using the AUC metric:
> library(pROC)

> roc_result <- roc(response = validationData$fracture, predictor = validationData$predictedProb)
Setting levels: control = No, case = Yes
Setting direction: controls < cases

> auc(roc_result)
Area under the curve: 0.5451

> #sink()
Warning messages:
1: package ‘aplore3’ was built under R version 4.3.3 
2: package ‘glmnet’ was built under R version 4.3.3 
library(aplore3)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(GGally)
library(dplyr)

data("glow_bonemed")
head(glow_bonemed)
tail(glow_bonemed)
View(glow_bonemed)
names(glow_bonemed)
summary(glow_bonemed)
sum(is.na(glow_bonemed))
str(glow_bonemed)

numeric_variables <- sapply(glow_bonemed, is.numeric)
lapply(names(glow_bonemed)[numeric_variables], function(var_name) {
  hist(glow_bonemed[[var_name]], 
       main = paste(var_name, "Distribution"), 
       xlab = var_name, 
       col = "pink", 
       border = "white")
})

boxplot(bmi ~ fracture, data = glow_bonemed, main = "BMI by Fracture Status", xlab = "Fracture", ylab = "BMI")
ggplot(glow_bonemed, aes(x = fracture, y = bmi, fill = fracture)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "BMI", x = "Fracture Status", title = "BMI Distribution by Fracture Status") +
  theme_minimal()

numeric_vars <- c("age", "weight", "height", "bmi", "fracscore") 
cor_matrix <- cor(glow_bonemed[, numeric_vars])
corrplot(cor_matrix, method = "circle")

numeric_variables <- c("age", "weight", "height", "bmi", "fracscore")
ggpairs(glow_bonemed[, c(numeric_variables, "fracture")], aes(colour = fracture))

plot(glow_bonemed$age, glow_bonemed$bmi, main = "Age vs. BMI", xlab = "Age", ylab = "BMI", pch = 19, col = "blue")

model1 <- glm(formula = fracture ~ priorfrac + age + weight + height + bmi + premeno + 
              momfrac + armassist + smoke + raterisk + fracscore + bonemed + 
              bonemed_fu + bonetreat, 
              family = binomial, data = glow_bonemed)

summary(model1)
confint(model1)

model2 <- glm(formula = fracture ~ age + bonemed + 
              bonemed_fu + bonetreat, 
              family = binomial, data = glow_bonemed)

summary(model2)
confint(model2)

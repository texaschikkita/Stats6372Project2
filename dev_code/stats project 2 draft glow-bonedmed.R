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



write.table(data, file = "bone_data.txt", sep = "\t", row.names = FALSE)

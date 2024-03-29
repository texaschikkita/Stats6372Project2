
library(aplore3) 
data(package= "aplore3")
data(glow_bonemed)  # Load the dataset

exists("glow_bonemed") # This should return 'TRUE' 



# verify data frame
class(glow_bonemed)  # Should output "data.frame"
str(glow_bonemed)   # View the dataset's structure

# export
write.csv(glow_bonemed, file = "bonedata.csv", row.names = FALSE) 

library(writexl) 
write_xlsx(glow_bonemed, path = "bonedata.xlsx") 


library(aplore3)
data(glow_bonemed) # Loads the glow_bonemed dataset
str(glow_bonemed) # Check the dataset's structure

#csv & excel:
write.csv(glow_bonemed, file = "bonedata.csv", row.names = FALSE)  # CSV
library(writexl)
write_xlsx(glow_bonemed, path = "bonedata.xlsx") 
write.table(glow_bonemed, file = "bone_data.txt", sep = "\t", row.names = FALSE) # Text

write.csv(glow_bonemed, file = "bonedata_wrow.csv", row.names = TRUE) 



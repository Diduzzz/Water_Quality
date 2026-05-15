# IMPORTAZIONE LIBRERIE NECESSARIE
library(caret)
library(missForest)
library(ROSE)
library(corrplot)
data <- read.csv("water_potability.csv")

## Controllo Rappresentatività  
data$Potability <- factor(data$Potability, levels = c(0,1), labels = c('Non_Potabile', 'Potabile'))

# distribuzione assoluta 
table(data$Potability)

# proporzioni percentuali originali
round(prop.table(table(data$Potability)),3)

## DATA SPLITTING
set.seed(123)
trainIndex <- createDataPartition(data$Potability, p = 0.8, list = FALSE)

train_set <- data[trainIndex, ]
test_set <- data[-trainIndex, ]

cat("\n--- Proporzioni nel Training Set ---\n")
round(prop.table(table(train_set$Potability)),3)

cat("\n--- Proporzioni nel Test Set ---\n")
round(prop.table(table(test_set$Potability)),3)

## Counting misisng values
missing_values = colSums(is.na(data))/nrow(data) * 100
print(missing_values)


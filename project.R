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

## IMPUTAZIONE CON MISSFOREST 
# prima cosa visualizziamo i missing values di tutte le colonne
missing_values = colSums(is.na(data))/nrow(data) * 100
print(missing_values)
# rimuoviamo temporaneamente il target per l'imputazione
set.seed(123)
train_imputed_obj <- missForest(as.data.frame(train_set[, -which(names(train_set) == "Potability")]))
train_final <- data.frame(train_imputed_obj$ximp)
train_final$Potability <- train_set$Potability

# facciamo la stessa cosa per per il Test set
test_imputed_obj <- missForest(as.data.frame(test_set[, -which(names(test_set) == "Potability")]))
test_final <- data.frame(test_imputed_obj$ximp)
test_final$Potability <- test_set$Potability

nzv <- nearZeroVar(train_final, saveMetrics = TRUE)
print(nzv)

#check missing values after imputation
missing_values_after_imputation = colSums(is.na(train_final))/nrow(train_final) * 100
print(missing_values_after_imputation)

## ANALISI DELLA MULTICOLLINEARITA
cor_matrix <- cor(train_final[, sapply(train_final, is.numeric)])
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black", number.cex = 0.7)

# Se trovi correlazioni > 0.90:
# high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)
# train_final <- train_final[, -high_cor]

## BILANCIAMENTO DELLE CLASSI
cat("\n--- Proporzioni nel Training Set ---\n")
round(prop.table(table(train_final$Potability)),3)

set.seed(123)
train_balanced <- ROSE(Potability~., data = train_final, seed = 123)$data
print(round(prop.table(table(train_balanced$Potability)),3))

## MODEL SELECTION UTILIZZANDO BORUTA
library(Boruta)
set.seed(123)
Boruta_output <- Boruta(
  Potability~., data = train_balanced,
  doTrace = 2,
  maxRuns = 100
) 
print(Boruta_output)
plot(Boruta_output, las = 2, cex.axis = 0.7, main = 'Risultato della selezione boruta')

## MODEL SELECTION CON RANDOM FOREST E CROSS VALIDATION
set.seed(1)

cvCtrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
fit_rf <- train(Potability ~ ., 
                data = train_balanced, 
                method = "rf", 
                metric = "ROC",
                trControl = cvCtrl)

vimp <- varImp(fit_rf, scale = TRUE)
print(vimp)
plot(vimp, main = "Importanza delle Variabili - Random Forest")

vimp=data.frame(vimp[1])
vimp$var=row.names(vimp)
vimp2=vimp[vimp$Overall>0,]


selected_features <- row.names(vimp2)   # notiamo che la variabile 'Organic_carbon' ha un valore pari a zero, quindi possiamo escluderlo
# subset di TRAINING con le sole variabili selezionate + il TARGET
df_subset <- train_balanced[, c(selected_features, "Potability")]
# subset di TEST con le STESSE identiche variabili + il TARGET
dataTestSubset <- test_final[, c(selected_features, "Potability")]



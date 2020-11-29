##Load library
library(randomForest)
library(psych)
library(caret)


##Baca data
diag <- read.csv(file.choose(), sep = ";", header = FALSE)
head(diag)
str(diag)
summary(diag)

##Konversi data
#Ubah tipe variabel menjadi tipe faktor

for(i in names(diag[,2:8])){
  diag[,i]=as.factor(diag[,i])
}
str(diag)


data_imp <- diag
# mean imputation
data_imp$V1[is.na(data_imp$V1)] <- mean(data_imp$V1, na.rm=TRUE)

# mengetahui jumlah missing value
sapply(data_imp, function(x) sum(is.na(x)))


##Pair plot
#Melihat korelasi dari tiap variabel, kalau ada korelasi yang tinggi, hilangkan salah satu variabel
pairs.panels(diag)


##Split data
#Memecah data menjadi data training(80% dari data awal) dan data test (20% dari data awal)
set.seed(1234)
sampel<-sample(2,nrow(diag),replace = T, prob = c(0.8,0.2))
trainingdat<-data_imp[sampel==1, ]
testingdat<-data_imp[sampel==2, ]
print(paste("Jumlah Train Data: ", nrow(trainingdat), "| Jumlah Test Data: ", nrow(testingdat)))


##Membuat model
set.seed(123)   
model <- randomForest(V8~., data=trainingdat)
model


##Model evaluation
#confusion matrix
prediksiRF <- predict(model, testingdat)
confusionMatrix(table(prediksiRF, testingdat$V8))

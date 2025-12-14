#import dulu datanya ke dalam Rstudio, dataset juga disediakan pada zip yang diberikan
data <- read.csv(file.choose())

#data description
head(data)
str(data)
summary(data)
library(ggplot2)

#Mengubah bmi dari character menjadi numerik
data$bmi <- as.numeric(data$bmi)

#menampilkan jumlah N/A di setiap atribut
dataisNA <- data.frame(Variables = c("id", "gender", "age", "hypertension", "heart_disease", "ever_married", 
                                     "work_type","residence_type", "avg_glucose_level","bmi", "smoking_status", "stroke"),
                       isNA = c(sum(is.na(data$id)),sum(is.na(data$gender)), sum(is.na(data$age)),sum(is.na(data$hypertension)), 
                                sum(is.na(data$heart_disease)),sum(is.na(data$ever_married)), sum(is.na(data$work_type)), 
                                sum(is.na(data$Residence_type)), sum(is.na(data$avg_glucose_level)), 
                                sum(is.na(data$bmi)), sum(is.na(data$smoking_status)), sum(is.na(data$stroke))))
dataisNA

options(scipen = 100, digits=10)#menampilkan 10 angka desimal

#Data Cleaning 
#mengubah nilai NA di atribut BMI dengan median dari BMI
data$bmi <- ifelse(is.na(data$bmi), round(median(data$bmi, na.rm = TRUE), 2), data$bmi)

#menampilkan kembali 15 data pertama dari dataset yang telah diperbarui
head(data,15)

#menampilkan jumlah N/A di setiap atribut dari dataset yang telah diperbarui
datanewisNA <- data.frame(Variables = c("id", "gender", "age", "hypertension", "heart_disease", "ever_married", 
                                        "work_type","residence_type", "avg_glucose_level","bmi", "smoking_status", "stroke"),
                          isNA = c(sum(is.na(data$id)),sum(is.na(data$gender)), sum(is.na(data$age)),sum(is.na(data$hypertension)), 
                                   sum(is.na(data$heart_disease)),sum(is.na(data$ever_married)), sum(is.na(data$work_type)), 
                                   sum(is.na(data$Residence_type)), sum(is.na(data$avg_glucose_level)), 
                                   sum(is.na(data$bmi)), sum(is.na(data$smoking_status)), sum(is.na(data$stroke))))
datanewisNA

#menampilkan nilai statictical measurement dari masing-masing attribute di dataset
summary(data)

datanewsummary <- data.frame(Variables = c("id", "age", "hypertension", "heart_disease", 
                                           "avg_glucose_level","bmi", "stroke"), Standard_Deviation = c(sd(data$id), 
                                                                                                        sd(data$age),sd(data$hypertension), sd(data$heart_disease),sd(data$avg_glucose_level), 
                                                                                                        sd(data$bmi), sd(data$stroke)), Variance = c(var(data$id),var(data$age), 
                                                                                                                                                     var(data$hypertension), var(data$heart_disease),var(data$avg_glucose_level),                                                                                                                                                     var(data$bmi), var(data$stroke)))
datanewsummary

#Visualisasi data
#Stroke
Strokes <- data$stroke
ggplot(data = data, aes(x = factor(Strokes, labels = c("No", "Yes")))) + 
  geom_bar(aes(y = after_stat(count)), fill = rainbow(2), color = 'black') +
  geom_text(stat = "count", aes(label = sprintf("%d", after_stat(count))),
            vjust = -0.5, size = 3) +
  labs(title = "Distribusi Jumlah Pasien yang Mengidap Stroke",x = "Status Stroke",y = "Frekuensi") +
  theme(plot.title = element_text(hjust = 0.5))

#Age
Age <- data$age
ggplot(data = data, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "firebrick1", color = 'black') +
  labs(title = "Distribusi Umur Pasien",x = "Umur",y = "Frekuensi") +
  theme(plot.title = element_text(hjust = 0.5))

#Hypertension
HyperTension <- data$hypertension
ggplot(data = data, aes(x = factor(HyperTension, labels = c("No", "Yes")))) + 
  geom_bar(aes(y = after_stat(count)), fill = rainbow(2), color = 'black') +
  geom_text(stat = "count", aes(label = sprintf("%d", after_stat(count))),
            vjust = -0.5, size = 3) +
  labs(title = "Distribusi Jumlah Pasien yang Mengidap Hipertensi",x = "Status", y = "Frekuensi") +
  theme(plot.title = element_text(hjust = 0.5))

#Heart Disease
HeartDisease <- data$heart_disease
ggplot(data = data, aes(x = factor(HeartDisease, labels = c("No", "Yes")))) + 
  geom_bar(aes(y = after_stat(count)), fill = rainbow(2), color = 'black') +
  geom_text(stat = "count", aes(label = sprintf("%d", after_stat(count))),
            vjust = -0.5, size = 3) +
  labs(title = "Distribusi Jumlah Pasien yang Mengidap Penyakit Jantung",x = "Status", y = "Frekuensi") +
  theme(plot.title = element_text(hjust = 0.5))

#Average Glucose Level
AvgGlucoseLevel <- data$avg_glucose_level
ggplot(data = data, aes(x = AvgGlucoseLevel)) + 
  geom_histogram(binwidth = 5, fill = "steelblue", color = 'black') +
  labs(title = "Distribusi Kadar Gula Darah Pasien",x = "Kadar Gula Darah",y = "Frekuensi") +
  theme(plot.title = element_text(hjust = 0.5))

#BMI
BMI <- data$bmi
ggplot(data = data, aes(x = BMI)) + 
  geom_histogram(binwidth = 5, fill = "orchid", color = 'black') +
  labs(title = "Distribusi BMI Pasien",x = "Body Mass Index",y = "Frekuensi") +
  theme(plot.title = element_text(hjust = 0.5))



#Box plot untuk mencari outlier
boxplot(data$avg_glucose_level, main = "Average Glucose Level", ylab = "skor",
        col = "lightblue", border = "blue", horizontal = FALSE)
boxplot(data$age, main = "Age", ylab = "skor",
        col = "lightblue", border = "blue", horizontal = FALSE)
boxplot(data$bmi, main = "BMI", ylab = "skor",
        col = "lightblue", border = "blue", horizontal = FALSE)

#correlation
res1<-cor.test(data$stroke, data$avg_glucose_level, method="spearman")
res2<-cor.test(data$stroke, data$age, method="spearman")
res3<-cor.test(data$stroke, data$hypertension, method="spearman")
res4<-cor.test(data$stroke, data$heart_disease, method="spearman")
res5<-cor.test(data$stroke, data$bmi, method="spearman")
res1
res2
res3
res4
res5

#Logistic Reggression plot
library(ggplot2)
library(ggthemes)
ggplot(data, aes(x=age, y=stroke, color=hypertension)) + 
  geom_jitter(width = 0, height = 0.05) +
  geom_smooth(method="glm",  method.args = list(family="binomial"))  + 
  labs(x = "Age", y = "P(Stroke)") +
  facet_wrap(~ hypertension)

#Membuat model Logistic Regression 
linearModelD <- lm(stroke ~ avg_glucose_level + hypertension + age + bmi + heart_disease, data = data)
summary(linearModelD)

#Accuracy
library(MLmetrics)
logreg <- glm(formula = stroke ~ avg_glucose_level + hypertension + age + bmi + heart_disease,
              family = binomial(link = "logit"), data = data)
pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
Accuracy(y_pred = pred, y_true = data$stroke)

#AUC
AUC(y_pred = logreg$fitted.values, y_true = data$stroke)



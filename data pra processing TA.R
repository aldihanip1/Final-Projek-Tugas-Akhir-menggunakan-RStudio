##data processing/persiapan data
#analisis regresi
##data processing/persiapan data
#analisis regresi
install.packages("heatmaply")
install.packages("summarytools")
library(heatmaply)
library(magrittr)
library(summarytools)
library(stargazer)
library(ggplot2)
library(tseries)
library(lmtest)
library(car)
install.packages("GGally")
library(GGally)
library(readr)
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_211.csv")

# Menghitung jumlah data yang hilang pada setiap baris
kolom_hilang <- colSums(is.na(data_aldi))

# Mencetak hasil
print(kolom_hilang)

library(car)
library(lmtest)
library(aod)
library(plm)

# Membuat data simulasi
set.seed(0)
n <- 100  # Jumlah observasi
potential <- rnorm(n)  # Variabel independen 1
physic <- rnorm(n)  # Variabel independen 2
epsilon <- rnorm(n)  # Error term
overall <- potential + 2*physic + epsilon  # Variabel dependen

# Mengubah data menjadi format panel
data_aldi <- data.frame(overall, potential, physic, time = rep(1:n, each = 1))

# Menghapus duplikat pada data frame
data_aldi <- data_aldi[!duplicated(data_aldi$time), ]

# Mengubah data menjadi objek panel
pdata <- pdata.frame(data_aldi, index = c("time"))

# Estimasi dengan metode Cochrane-Orcutt
model_co <- plm(overall ~ potential + physic, data = pdata, model = "pooling", effect = "twoways", method = "cork")
summary(model_co)


# Uji asumsi heteroskedastisitas
coeftest(model_co, vcov = sandwich)






summary(data_aldi)
data_aldi$hits_transformed <- log(data_aldi$hits)
data_aldi$league_rank_transformed <- log(data_aldi$league_rank)
data_aldi$team_jersey_number_transformed <- log(data_aldi$team_jersey_number)
data_aldi$physic_transformed <- log(data_aldi$physic)
model = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank+international_reputation+skill_moves, data_aldi)
summary(model)
summary(data_aldi)
str(data_aldi)
model <- lm(overall ~ potential + age + hits, data = data_aldi)
new_data <- data.frame(potential = 80, age = 25, hits = 50)
predictions <- predict(model, newdata = new_data)
predictions
str(data_aldi)
graphics.off()
heatmaply(data_aldi, scale = "row", k_col = 2)
model = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank+international_reputation+skill_moves, data_aldi)

numeric_data <- data_aldi[, c("potential", "age", "hits", "hits", "height_cm", "physic", "team_jersey_number", "league_rank", "international_reputation", "skill_moves")]
heatmaply(numeric_data, scale = "row", k_col = 2)


# Membaca dataset
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_21.csv")

# Membuat model regresi
model <- lm(overall ~ potential + age + hits + height_cm + physic + team_jersey_number + league_rank + international_reputation + skill_moves, data = data_aldi)

# Menampilkan hasil model regresi
summary(model)

# Menampilkan persamaan regresi
regression_equation <- paste("overall =", round(coef(model)[1], 4), "+",
                             paste(round(coef(model)[-1], 4), names(coef(model)[-1]), sep = "*", collapse = " + "))
print(regression_equation)

# Menampilkan nilai prediksi terhadap variabel "overall"
new_data <- data.frame(
  age = c(25.51, 25.00, 53.00, 16.00, 22.00, 29.00),  
  height_cm = c(181.2, 181.0, 206.0, 155.0, 176.0, 186.0),
  league_rank = c(1.343, 1.000, 4.000, 1.000, 1.000, 1.000),
  potential = c(71.43, 71.00, 95.00, 55.00, 8.00, 27.00),
  international_reputation = c(1.097, 1.000, 5.000, 1.000, 1.000, 1.000),
  team_jersey_number = c(20.17, 17.00, 99.00, 1.00, 8.00, 27.00),
  physic = c(65.04, 66.00, 91.00, 28.00, 59.00, 72.00),
  hits = c(2.689, 0.000, 371.00, 0.000, 0.000, 2.000),
  skill_moves = c(2.39, 2.00, 5.00, 1.00, 2.00, 3.00)
)

predicted_values <- predict(model, newdata = new_data)
new_data$predicted_overall <- predicted_values

print(new_data$predicted_overall)





# fungsi heatmap vsualisasi plot pada eda(expanatory data analysis)
install.packages("heatmaply")
#Gambar heatmap untuk membuat heatmap
library(dplyr)      # Untuk manipulasi data
library(ggplot2)    # Untuk visualisasi data
library(caret)      # Untuk pembagian data
library(heatmaply)
library(Hmisc)
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_21.csv")
# membuat kolom menggunakan subset data bagi baris yang memiliki jumlah yang ribuan
subset_data <- data_aldi[, c("overall","potential", "age", "hits", "height_cm", "physic", "team_jersey_number", "league_rank", "international_reputation", "skill_moves")]
# fungsi untuk baris pada data yang meimiliki data outlier aau data pencilan
subset_data <- na.omit(subset_data)

subset_matrix <- as.matrix(subset_data)
cor_matrix <- cor(subset_matrix)

# menggunakan numeric menanandakan x dan y
heatmaply(cor_matrix, scale = "none", main = "Heatmap Korelasi")






library(dplyr)
library(heatmaply)
install.packages("Hmisc")
library(Hmisc)
install.packages("ggcorrplot")
library(ggcorrplot)

data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_21.csv")
subset_data <- data_aldi[, c("potential", "age", "hits", "height_cm", "physic", "team_jersey_number", "league_rank", "international_reputation", "skill_moves")]
subset_data <- na.omit(subset_data)

subset_matrix <- as.matrix(subset_data)
cor_matrix <- cor(subset_matrix)


flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = cormat[ut],
    p = pmat[ut],
    stringsAsFactors = FALSE
  )
}

# Menggunakan fungsi flattenCorrMatrix dengan matriks r dan P yang benar
cor_data <- rcorr(as.matrix(subset_data))
cor_flat <- flattenCorrMatrix(cor_data$r, cor_data$P)

# Menampilkan hasil
print(cor_flat)




# Menggunakan heatmaply untuk membuat heatmap

library(dplyr)
library(heatmaply)
install.packages("Hmisc")
library(Hmisc)
install.packages("ggcorrplot")
library(ggcorrplot)

data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_21.csv")
subset_data <- data_aldi[, c("potential", "age", "hits", "height_cm", "physic", "team_jersey_number", "league_rank", "international_reputation", "skill_moves")]
subset_data <- na.omit(subset_data)

subset_matrix <- as.matrix(subset_data)
cor_matrix <- cor(subset_matrix)

rcorr(as.matrix(data_aldi), type="pearson")



# Mengatasi data yang hilang
# Menghapus baris dengan nilai yang hilang
data_aldi_clean <- na.omit(data_aldi)

install.packages("tidyimpute")
library(tidyverse)

# Mengisi nilai yang hilang dengan mean
data_aldi_clean <- data_aldi
data_aldi_clean$variabel1[is.na(data_aldi_clean$team_jersey_number)] <- mean(data_aldi_clean$team_jersey_number, na.rm = TRUE)
data_aldi_clean$variabel2[is.na(data_aldi_clean$hits)] <- mean(data_aldi_clean$hits, na.rm = TRUE)
data_aldi_clean$variabel3[is.na(data_aldi_clean$physic)] <- mean(data_aldi_clean$physic, na.rm = TRUE)
data_aldi_clean$variabel4[is.na(data_aldi_clean$league_rank)] <- mean(data_aldi_clean$league_rank, na.rm = TRUE)




# Membuat model regresi
# Melakukan fit multiple linear regression model menggunakan robust regression dengan metode RANSAC
model = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank+international_reputation+skill_moves, data_aldi)
summary(model)

# Mebuat model data baru je dalam data frame yaitu 
# fit MLR yang sudah dikelompokkan denga perhitungan statistika Deskriptif
new_data <- data.frame(
  age = c(25.51, 25.00, 53.00, 16.00, 22.00, 29.00),  # Contoh nilai variabel age
  height_cm = c(181.2,	181.0,	206.0,	155.0,	176.0,	186.0),  # Contoh nilai variabel height_cm
  league_rank = c(1.343,	1.000,	4.000,	1.000,	1.000,	1.000),  # Contoh nilai variabel league_rank
  overall = c(66.44,	66.00,	93.00,	54.00,	62.00,	71.00),  # Contoh nilai variabel overall
  potential = c(71.43,	71.00,	95.00,	55.00,	8.00,	27.00),  # Contoh nilai variabel potential
  international_reputation = c(1.097,	1.000,	5.000,	1.000,	1.000,	1.000),  # Contoh nilai variabel international_reputation
  team_jersey_number = c(20.17,	17.00,	99.00,	1.00,	8.00,	27.00),  # Contoh nilai variabel team_jersey_number
  physic = c(65.04,	66.00,	91.00,	28.00,	59.00,	72.00),  # Contoh nilai variabel physic
  hits = c(2.689,	0.000,	371.00,	0.000,	0.000,	2.000),  # Contoh nilai variabel hits
  skill_moves = c(2.39,	2.00,	5.00,	1.00,	2.00,	3.00))  # Contoh nilai variabel hits


# Menyimpan hasil model regresi linier berganda
model <- lm(overall ~ potential + age + hits + height_cm + physic + team_jersey_number + league_rank + international_reputation + skill_moves, data = data_aldi)

# Mengambil koefisien dari model
intercept <- coef(model)[[1]]
coefs <- coef(model)[-1]

# Membuat persamaan regresi
regression_equation <- paste("overall =", intercept, "+",
                             paste(coefs, names(coefs), sep = "*", collapse = " + "))

# Menampilkan persamaan regresi
print(regression_equation)

# Menampilkan nilai prediksi terhadap variabel "overall"
predicted_values <- predict(model)

# Menambahkan nilai prediksi sebagai kolom baru dalam dataset
model$predicted_overall <- predicted_values

# Menampilkan dataset dengan kolom nilai prediksi
print(model)

overall = -7.965211 + 0.738970*potential + 0.748915*age + 0.025042*hits - 0.055717*height_cm + 0.142369*physic - 0.014357*team_jersey_number - 0.365366*league_rank + 0.212667*international_reputation + 1.525418*skill_moves
print(overall)

# Melakukan prediksi pada data pengujian
predicted_values <- predict(model, newdata = testing)


# Mengambil variabel prediksi dari dataset
predictors <- data_aldi[, -which(names(data_aldi) == "age")]

# Membuat data frame dengan variabel prediksi dan variabel target
prediction_data <- data.frame(data_aldi$age, predict(model, newdata = predictors))

# Memberikan nama kolom pada data frame
colnames(prediction_data) <- c("Actual", "Predicted")

# Menampilkan data frame hasil prediksi
print(prediction_data)

# menggunakan numeric menanandakan x dan y
heatmaply(cor_matrix, scale = "noneq", main = "Heatmap Korelasi")



install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Contoh data
data <- data.frame(
  x = sample(1:1000, 1000, replace = TRUE),  # Kolom x dengan nilai acak
  y = sample(1:1000, 1000, replace = TRUE),  # Kolom y dengan nilai acak
  value = rnorm(1000)  # Kolom value dengan nilai acak dari distribusi normal
)

# Membuat heatmap dengan ggplot2
heatmap <- heatmaply(data, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red")  # Menyesuaikan skala warna

# Menampilkan heatmap
print(heatmap)



names(data_aldi)
descr()
summary(data_aldi)
View(data_aldi)
print(head(data_aldi))
head()


summary(data_aldi)
plot(data_aldi)
cor (data_aldi)
data11 <- as.data.frame(data_aldi)
data1
summary(data1)
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_211.csv")
data1=lm(overall~potential+weight_kg+height_cm+team_jersey_number+physic+league_rank+international_reputation,skill_moves, data=data_aldi)
data2= lm(overall~potential, data=data_aldi)
data3 = lm(overall~potential+age, data= data_aldi)
data4 = lm(overall~potential+age+hits, data=data_aldi)
data5 = lm(overall~potential+age+hits+weight_kg, data=data_aldi)
data6 = lm(overall~potential+age+hits+height_cm, data=data_aldi)
data7 = lm(overall~potential+age+hits+height_cm+physic, data_aldi)
data8 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number, data_aldi)
data9 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank, data_aldi)
data10 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank+international_reputation, data_aldi) 
data11 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank+international_reputation+skill_moves, data_aldi)

summary(data11)
summary(data10)
summary(data9)
summary(data8)
summary(data7)
summary(data6)
summary(data5)
summary(data4)
summary(data3)
summary(data2)




library(caret)
library(Metrics)
set.seed(123)
trainIndex <- createDataPartition(data_aldi$overall, p = 0.7, list = FALSE)
trainData <- data_aldi[trainIndex, ]
trainData
testData <- data_aldi[-trainIndex, ]
testData
data11 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank+international_reputation+skill_moves, data_aldi)
rSquared <- R2(prediksi, testData$overall)
mse <- mse(prediksi, testData$overall)
mae <- mae(prediksi, testData$overall)
data11 <- as.data.frame(data_aldi)

predict(model, data11, type="response")

y <- data11$overall
x1 <- data11$height_cm
x2 <- data11$league_rank
x3 <- data11$potential
x4 <- data11$international_reputation
x5 <- data11$skill_moves
x6 <- data11$team_jersey_number
x7 <- data11$physic
x8 <- data11$hits


data2= lm(overall~potential, data=data_aldi)
data3 = lm(overall~potential+age, data= data_aldi)
data4 = lm(overall~potential+age+hits, data=data_aldi)
data5 = lm(overall~potential+age+hits+weight_kg, data=data_aldi)
data6 = lm(overall~potential+age+hits+height_cm, data=data_aldi)
data7 = lm(overall~potential+age+hits+height_cm+physic, data_aldi)
data8 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number, data_aldi)
data9 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank, data_aldi)
data10 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank+international_reputation, data_aldi) 
data11 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank+international_reputation+skill_moves, data_aldi)
summary(data11)

data_aldi <- as.data.frame(data_aldi)


#membentuk visualisasi plot suatu predictor dan terikat/pengaruh
#visualisasi pengaruh visualisasi grafik boxplot 
library(ggplot2)
boxplot (overall~age, data=data_aldi)
boxplot (overall~hits, data=data_aldi)
plot (overall~potential, data=data_aldi)
plot (overall~height_cm, data=data_aldi)
plot (overall~physic, data=data_aldi)
plot (overall~international_reputation, data=data_aldi)
plot (overall~league_rank, data=data_aldi)




hist(data_aldi$hits)

##interpretasi regresi linier berganda
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_21.csv")
#1. uji F (simultan) menggunakan One Way Anova
boxplot(data_aldi$age, data_aldi$hits, data_aldi$height_cm, data_aldi$league_rank, data_aldi$potential, data_aldi$international_reputation, data_aldi$skill_moves)
# Analisis ANOVA -> analysis of variance. -
# Jika data memenuhi asumsi homogenitas variances
# membuat kolom menggunakan subset data bagi baris yang memiliki jumlah yang ribuan
subset_data <- data_aldi[, c("potential", "age", "hits", "height_cm", "physic", "team_jersey_number", "league_rank", "international_reputation", "skill_moves")]
# fungsi untuk baris pada data yang meimiliki data outlier atau data pencilan
subset_data <- na.omit(subset_data)

#mengubah data subset menjadi matriks
subset_matrix <- as.matrix(subset_data)
#menunjukkan sejauh mana hubungan linear antara variabel-variabel tersebut.
cor_matrix <- cor(subset_matrix)

# menggunakan numeric menanandakan x dan y
heatmaply(cor_matrix, scale = "none", main = "Heatmap Korelasi")


data11 <- as.data.frame(data_aldi)
res.aov <- aov(overall~potential, data = data11)
summary(res.aov)




# Mengatasi data yang hilang
# Menghapus baris dengan nilai yang hilang
data_aldi_clean <- na.omit(data_aldi)

install.packages("tidyimpute")
library(tidyverse)

# Mengisi nilai yang hilang dengan mean
data_aldi_clean <- data_aldi
data_aldi_clean$variabel1[is.na(data_aldi_clean$team_jersey_number)] <- mean(data_aldi_clean$team_jersey_number, na.rm = TRUE)
data_aldi_clean$variabel2[is.na(data_aldi_clean$hits)] <- mean(data_aldi_clean$hits, na.rm = TRUE)
data_aldi_clean$variabel3[is.na(data_aldi_clean$physic)] <- mean(data_aldi_clean$physic, na.rm = TRUE)
data_aldi_clean$variabel4[is.na(data_aldi_clean$league_rank)] <- mean(data_aldi_clean$league_rank, na.rm = TRUE)



# Memasukkan variabel yang tidak tercantum dalam output
model <- lm(overall ~ hits + team_jersey_number + physic, league_rank,  data = data_aldi_clean)
summary(model)

# Menghapus observasi dengan missing values pada variabel yang tidak tercantum dalam output
data_aldi_clean <- na.omit(data_aldi)

# Membuat model regresi dengan variabel yang tercantum dalam output
model <- lm(overall ~ potential + age + hits + team_jersey_number + physic, data = data_aldi_clean, subset = league_rank)

# Menampilkan summary model
summary(model)

# Menghapus variabel yang tidak tercantum dalam output
data_aldi_clean <- subset(data_aldi_clean, select = c("overall", "potential", "age", "hits", "height_cm", "physic", "team_jersey_number", "league_rank", "international_reputation", "skill_moves"))

# Membangun model regresi
model <- lm(overall ~ potential + age + hits + height_cm + physic + team_jersey_number + league_rank + international_reputation + skill_moves, data = data_aldi_clean)

# Menampilkan summary model
summary(model)




# Mengimputasi nilai yang hilang menggunakan metode imputasi yang tepat
# Contoh: Menggunakan mean untuk mengisi nilai yang hilang

# Memasukkan variabel yang tidak tercantum dalam output
model <- lm(overall ~ league_rank + team_jersey_number + physic, data = data_aldi_clean)
summary(model)

# Summary of the analysis
summary(res.aov)

# Mengatasi data yang hilang
# Menghapus baris dengan nilai yang hilang
data_aldi_clean <- na.omit(data_aldi)

# atau

# Mengimputasi nilai yang hilang menggunakan metode imputasi yang tepat
# Contoh: Menggunakan mean untuk mengisi nilai yang hilang

# Memasukkan variabel yang tidak tercantum dalam output
model <- lm(overall ~ potential + age + hits + variabel1 + variabel2, data = data_aldi_clean)
summary(model)


#2 uji T (parsial) menggunakan nilai summary statistik t
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
summary(model)








##model uji asumsi klasik        
#1.uji normalitas residuals
ghraphics.off()
boxplot(data11$residuals, main="Residual Box Plot")
ols_plot_resid_box(data11)



## Distribusi Normal menggunakan Anderson Darling
##(tidak bisa menggunakan metode shapiro.test atau shapiro wilk karena ukuran sample tidak bisa besar hanya 5000 records)
# clean workspace
rm(list=ls())

# Install required packages:
install.packages('nortest')
library(nortest)

#Model data tho use
data11 = beaver2$temp

#Do shapiro test with only the first 5000 records
#(hanya menggunakan 5000 records/sample)
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_21.csv")
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
shapiro.test(data_aldi$overall)
shapiro.test


#Anderson-Darling normality test
ad.test(data11)$p.value

#menggunakan uji kolmogorov-smirnov(karena sampel berukuran besar)
library(tseries)
sf.test(data11)
jb.norm.test(data11)
ad.test(data11)
shapiro.test(data11)
sisa <- residuals(data11)
sisa

data11 <- as.data.frame(data_aldi)
library("moments")
kanan <- c(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
stem(kanan)
kiri <- c(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
stem
simetris <- c(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
stem

#tampilan grafik 
par(mfrow=c(1,3))

data11 = lm(overall~potential+age+hits+height_cm+physic+team_jersey_number+league_rank+international_reputation+skill_moves, data_aldi)
skewness <- c(kanan)
skewness

qqnorm(kanan,main="Right Q-Q Plot")
qqline(kanan)


library(readr)
players_21_xlsx <- read_csv("New folder/players_21.csv", 
                            col_types = cols(age = col_number(), 
                            height_cm = col_number(), league_rank = col_number(), 
                            overall = col_number(), potential = col_number(), 
                            international_reputation = col_number(), 
                            skill_moves = col_number(), team_jersey_number = col_number(), 
                            physic = col_number(), hits = col_number()))
View(players_21_xlsx)
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)




#2. dugaan dengan histogram
par(mfrow=c(1,1))
hist(m, probability = TRUE, main = "HISTOGRAM")

#. uji kolmogorov smirnov
library(nortest)
nortest::lillie.test(model)
nortest::ad.test(data11)
nortest::cvm.test(data11)
lillie.test(x)

# uji chi-square
nortest::pearson.test(data11)

# uji franca-shapiro
nortest::pearson.test(data11)

data11 <- as.data.frame(data_aldi)


# Uji Normalitas dengan grafik Normal QQ
plot(res.aov)


                      
#2.install packages untuk uji asumsi klasik
install.packages("lmtest")
library(lmtest)

                      
#3.uji multikoliniearitas
install.packages("car")
library(car)
car::vif(model)
                      
#4.uji autokorelasi
library(lmtest)
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_21.csv")




# Membuat model regresi
# Melakukan fit multiple linear regression model menggunakan robust regression dengan metode RANSAC
model <- lm(overall ~ age+potential+hits+height_cm+international_reputation+league_rank+team_jersey_number+skill_moves+physic,
            data=data_aldi_clean)



prediction <- predict(model, newdata = data_aldi)
print(data_aldi)

# Mengambil residual dari model
residuals <- residuals(model)

# Plot ACF dari residual
acf_plot <- autoplot(acf(residuals))

# Plot PACF dari residual
pacf_plot <- autoplot(pacf(residuals))

# Uji autokorelasi menggunakan tes Durbin-Watson
dwtest(model)
lmtest::dwtest(model)


# Melakukan pengujian normalitas autokorelasi menggunakan uji Ljung-Box
library(lmtest)

# Menghapus baris dengan nilai NA
data <- na.omit(data)

# Menggantikan nilai NA dengan 0
data_aldi$physic[is.na(data_aldi$physic)] <- 0
data_aldi$team_jersey_number[is.na(data_aldi$team_jersey_number)] <- 0
data_aldi$hits[is.na(data_aldi$hits)] <- 0
data_aldi$league_rank[is.na(data_aldi$league_rank)] <- 0




# Mengambil residual model
residuals <- residuals(model)

# Melakukan pengujian Ljung-Box
ljung_box_test <- Box.test(residuals, lag = 12, type = "Ljung-Box")

# Menampilkan hasil pengujian
print(ljung_box_test)


library(nlme) 
model <- gls(overall ~ age+potential+hits+height_cm+international_reputation+league_rank+team_jersey_number+skill_moves+physic,
            data=data)
summary(model)

library(lmtest)  # Untuk menguji autokorelasi
library(forecast)# Untuk ARIMA
library(nlme)  # Memuat library nlme untuk GLS

# Membuat model regresi dengan GLS
model <- gls(overall ~ age + potential + hits + height_cm + international_reputation + league_rank + team_jersey_number + skill_moves + physic,
             data = data, correlation = corAR1(form = ~ 1))

# Menyimpulkan model dan menampilkan hasil
summary(model)


# Uji autokorelasi menggunakan tes Breusch-Godfrey
bg_test <- bgtest(model, order = 1)  # Mengasumsikan autokorelasi orde 1
print(bg_test)


# Membuat model ARIMA untuk residual
arima_model <- arima(residuals(model), order = c(1, 0, 0))  # Mengasumsikan autokorelasi orde 1

summary(arima_model)


# Menggabungkan model ARIMA dengan model regresi
combined_model <- update(model, correlation = corARMA(p = 1, q = 0))






#5. uji homogenitas/heterokedestisita
library(lmtest)
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_21.csv")
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
bptest(model,studentize = F ,data=data)
lmtest::bptest(model)



## menggunakan uji wls
# Memasang paket yang diperlukan
install.packages("sandwich")
install.packages("lmtest")

# Memuat paket yang diperlukan
library(sandwich)
library(lmtest)

# Mengestimasi model regresi awal dengan metode OLS
model <- lm(overall ~ age + hits + potential + height_cm + team_jersey_number + physic + league_rank + international_reputation + skill_moves, data = data_aldi)

# Menghitung residual model regresi awal
residuals <- resid(model)

# Mengidentifikasi pola heteroskedastisitas dalam residual
# Misalkan, Anda ingin menguji heteroskedastisitas dengan menggunakan uji Breusch-Pagan
bp_test <- bptest(model)

# Melakukan estimasi model dengan metode WLS
# Menentukan bobot berdasarkan estimasi varians residual
weights <- 1 / residuals^2  # Misalnya, menggunakan invers dari kuadrat residual

# Menghapus baris yang memiliki nilai NA pada variabel weights
data_wls <- data_aldi[complete.cases(weights), ]

wls_model <- lm(overall ~ age + hits + potential + height_cm + team_jersey_number + physic + league_rank + international_reputation + skill_moves, data = data_wls, weights = weights)

# Melakukan uji Breusch-Pagan untuk menguji heteroskedastisitas pada model WLS
bp_test_wls <- bptest(wls_model)

# Menampilkan hasil uji
print(bp_test)
print(bp_test_wls)



# menggunakn uji park
residuals <- resid(data11)
park_test <- bptest(data11)
print(park_test)

# menggunakan uji glejser
library(car)
residuals <- resid(data11)
white_test <- bptest(data11, ~., data = data_aldi)
# H0 : eror homogen (variabel eror sama)

# Uji homogenitas variances menggunakan grafik

# Mendapatkan residual standar
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
residuals <- resid(model) / sqrt(sigma(model)^2)

# Menggambar plot residual standar terhadap nilai prediksi
plot(predict(model), residuals, xlab = "Nilai Prediksi", ylab = "Residual Standar", main = "Plot Residual Standar")

# Menambahkan garis horizontal pada nilai residual nol
abline(h = 0, col = "red")

# Menambahkan garis regresi nonparametrik LOWESS
lines(lowess(predict(model), residuals), col = "blue")

# Menambahkan garis batas kesalahan (+/- 2) untuk residu standar
abline(h = 2, lty = 2, col = "green")
abline(h = -2, lty = 2, col = "green")

# Menambahkan distribusi normal dengan kurva teoritis
curve(dnorm(x, mean = 0, sd = sd(residuals)), add = TRUE, col = "orange")

#6 Selang kepercayaan ##
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
confint(model)

#7 Koefisien Determinasi (R2)
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
summary(model)

#8 Menghitung matriks korelasi
install.packages("corrr")
install.packages("PerformanceAnalytics")
install.packages("corrplot")
library(corrplot)
library("PerformanceAnalytics")
library(corrr)
cor <- c(data_aldi)
cor
cor.test(c(1:3,NA),1:4,use='complete.obs')

#9 Menghitung Uji Linieritas
lmtest::resettest(model, power=2)



#10 mengidentifikasi nilai outlier
pairs(~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves+weight_kg, data=data_aldi)



#11 membuat table prediksi kemungkinan
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_21.csv")
# Prediksi nilai menggunakan model yang sudah di-fit
#fit multiple linear regression model menggunakan robust regression dengan metode RANSAC
library(robustbase)
library(MASS)
model=lmrob(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)

# Membuat data frame observasi baru
new_data <- data.frame(
  age = c(25.51, 25.00, 53.00, 16.00, 22.00, 29.00),  # Contoh nilai variabel age
  height_cm = c(181.2,	181.0,	206.0,	155.0,	176.0,	186.0),  # Contoh nilai variabel height_cm
  league_rank = c(1.343,	1.000,	4.000,	1.000,	1.000,	1.000),  # Contoh nilai variabel league_rank
  overall = c(66.44,	66.00,	93.00,	54.00,	62.00,	71.00),  # Contoh nilai variabel overall
  potential = c(71.43,	71.00,	95.00,	55.00,	8.00,	27.00),  # Contoh nilai variabel potential
  international_reputation = c(1.097,	1.000,	5.000,	1.000,	1.000,	1.000),  # Contoh nilai variabel international_reputation
  team_jersey_number = c(20.17,	17.00,	99.00,	1.00,	8.00,	27.00),  # Contoh nilai variabel team_jersey_number
  physic = c(65.04,	66.00,	91.00,	28.00,	59.00,	72.00),  # Contoh nilai variabel physic
  hits = c(2.689,	0.000,	371.00,	0.000,	0.000,	2.000))  # Contoh nilai variabel hits
  skill_moves = c(2.39,	2.00,	5.00,	1.00,	2.00,	3.00)  # Contoh nilai variabel hits


new_data

# Melakukan prediksi dengan model regresi linear berganda
prediction <- predict(model, newdata = new_data)


# Menampilkan hasil prediksi
print(prediction)


prediction_df <- cbind(new_data, prediction)
# Contoh evaluasi menggunakan MSE
actual_values <- c(65.00, 67.00, 92.00, 53.00, 61.00, 70.00)  # Contoh nilai observasi actual
mse <- mean((actual_values - prediction)^2)





# Menghapus data pencilan dari data
data_clean <- data_aldi[data_aldi$potential >= lower_bound & data_aldi$potential <= upper_bound, ]
data_clean <- data_aldi[data_aldi$age >= lower_bound & data_aldi$age <= upper_bound, ]
data_clean <- data_aldi[data_aldi$hits >= lower_bound & data_aldi$hits <= upper_bound, ]
data_clean <- data_aldi[data_aldi$height_cm >= lower_bound & data_aldi$height_cm <= upper_bound, ]
data_clean <- data_aldi[data_aldi$international_reputation >= lower_bound & data_aldi$international_reputation <= upper_bound, ]
data_clean <- data_aldi[data_aldi$skill_moves >= lower_bound & data_aldi$skill_moves <= upper_bound, ]
data_clean <- data_aldi[data_aldi$league_rank >= lower_bound & data_aldi$league_rank <= upper_bound, ]
data_clean <- data_aldi[data_aldi$team_jersey_number >= lower_bound & data_aldi$team_jersey_number <= upper_bound, ]
data_clean <- data_aldi[data_aldi$physic >= lower_bound & data_aldi$physic <= upper_bound, ]
data_clean <- data_aldi[data_aldi$overall >= lower_bound & data_aldi$overall <= upper_bound, ]


#membuat model regresi linier berganda
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)

# Lanjutan

# Membuat tabel data prediksi
new_data <- data.frame(potential = c(1, 2, 3),
                       age = c(4, 5, 6),
                       hits = c(7, 8, 9),
                       height_cm = c(10, 11, 12),
                       physic = c(13, 14, 15),
                       team_jersey_number = c(16, 17, 18),
                       league_rank = c(19, 20, 21),
                       international_reputation = c(22, 23, 24),
                       skill_moves = c(25, 26, 27))

predictions <- predict(model, newdata = new_data)

# Menampilkan tabel prediksi
prediction_table <- data.frame(new_data, predicted_overall = predictions)
print(prediction_table)

predictions <- predict(model, newdata = data_aldi)



#Metode All Possible Regression
data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_211.csv")
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves+weight_kg, data=data_aldi)
olsrr::ols_step_all_possible(model)

#Metode Forward Selection
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves+weight_kg, data=data_aldi)
olsrr::ols_step_forward_p(model)

#Metode Backward Selection
model=lm(overall~age+hits+potential+height_cm+team_jersey_number+physic+league_rank+international_reputation+skill_moves, data=data_aldi)
olsrr::ols_step_backward_p(model)


# Contoh data
jam_belajar <- c(1, 2, 3, 4, 5, 6)
skor_ujian <- c(60, 70, 75, 85, 90, 95)

# Membuat model regresi
model <- lm(skor_ujian ~ jam_belajar)

# Plot garis persamaan regresi dan tebaran titik
plot(jam_belajar, skor_ujian, main = "Regresi Linier",
     xlab = "Jam Belajar", ylab = "Skor Ujian")
abline(model, col = "red")

# Menggunakan model untuk memprediksi skor ujian
jam_belajar_prediksi <- c(2.5, 4.5)
skor_ujian_prediksi <- predict(model, data.frame(jam_belajar = jam_belajar_prediksi))
skor_ujian_prediksi

# Menambahkan prediksi ke plot
points(jam_belajar_prediksi, skor_ujian_prediksi, col = "blue", pch = 16)
points

# Menampilkan nilai koefisien regresi
koefisien <- coef(model)
cat("Koefisien Regresi:\n")
cat("Intersep (a):", koefisien[1], "\n")
cat("Koefisien Regresi (b):", koefisien[2], "\n")


# Menghitung nilai R-squared
r_squared <- summary(model)$r.squared
cat("Koefisien Determinasi (R-squared):", r_squared, "\n")

# Menghitung nilai residual dan menampilkan plot residual
residuals <- resid(model)
plot(jam_belajar, residuals, main = "Plot Residual",
     xlab = "Jam Belajar", ylab = "Residual") 


# Install dan memuat library yang diperlukan
install.packages("ggplot2")
install.packages("forecast")
library(ggplot2)
library(forecast)

# Contoh deret waktu
data <- ts(c(3, 4, 2, 6, 8, 5, 7, 9, 4, 5, 3, 6, 8, 5, 7, 9), frequency = 1)

# Plot ACF
acf_data <- acf(data)
ggplot(acf_data, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity") +
  labs(title = "Autocorrelation Function (ACF)",
       x = "Lag", y = "ACF")

# Plot PACF
pacf_data <- pacf(data)
ggplot(pacf_data, aes(x = lag, y = pacf)) +
  geom_bar(stat = "identity") +
  labs(title = "Partial Autocorrelation Function (PACF)",
       x = "Lag", y = "PACF")

# Memuat library yang diperlukan
library(ggplot2)


data_aldi <- read.csv("C:/Users/ALDI-HANIP15/Documents/New folder/players_211.csv")


# Membuat model regresi
model=lm(overall~age+hits+potential, data=data_aldi)

# Mendapatkan residual dan nilai-nilai prediksi
residuals <- residuals(model)
fitted_values <- fitted(model)

# Membuat dataframe dari residual dan nilai-nilai prediksi
data_aldi <- data.frame(Residuals = residuals, Fitted = fitted_values)

# Membuat plot "Residual vs. Fitted"
ggplot(data_aldi, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual vs. Fitted",
       x = "Fitted Values",
       y = "Residuals")

qplot(fitted.values(model), residuals(model))


# Generate sample data from a normal distribution
set.seed(123)  # Untuk reproducibility
data <- rnorm(100)

# Create QQ plot to evaluate normality
qqnorm(data)
qqline(data)
plot(res.aov)





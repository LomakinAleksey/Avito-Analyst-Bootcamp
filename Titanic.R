# Установка и загрузка пакетов
library(readxl)
library(dplyr)
library(tidyr)
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
install.packages("caTools")
install.packages("ROCR")
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(caTools)
library(ROCR)
library(readxl)


# Обработка пропусков: замена пропущенных значений в Age медианой
# Сначала найдем медиану возраста (игнорируя пропуски)
age_median <- median(titanic$age, na.rm = TRUE)

# Заменим пропуски в Age медианой
titanic$age <- ifelse(is.na(titanic$age), age_median, titanic$age)

# Изменение типа данных для переменных survived и pclass на факторы
titanic$survived <- as.factor(titanic$survived)
titanic$pclass <- as.factor(titanic$pclass)

# Разбиваем данные на тренировочные и тестовые
set.seed(3000)
split <- sample.split(titanic$survived,SplitRatio = 0.7)
Train <- subset(titanic,split == TRUE)
Test <- subset(titanic,split == FALSE)

# Строим модель
titanictree <- rpart(
  survived ~ pclass + sex + fare + age + sibsp + parch,
  data = Train, method = "class",
  control=rpart.control(minbucket = 25)
)

# Изображение дерева
prp(titanictree) # Простое графическое изображение
fancyRpartPlot(titanictree) # Более наглядная версия

# Получаем предсказания
PredictCART <- predict(titanictree,newdata = Test,type="class")

# Определяем точность
table(Test$survived,PredictCART)
(205+110)/393*100 # 80,15%

# ROC кривая
PredictROC <- predict(titanictree,newdata = Test)
pref <- prediction(PredictROC[,2],Test$survived)
perf <- performance(pref,"tpr","fpr")
plot(perf)

table(titanic$pclass)
# Процент выживших по классам
prop.table(table(titanic$pclass, titanic$survived), 1)

# Процент выживаемости
prop.table(table(titanic$survived))

# Извлечение титулов
titanic$title <- gsub('(.*, )|(\\..*)', '', titanic$name)
table(titanic$title)

# Как пол влияет на выживаемость
prop.table(table(titanic$sex, titanic$survived), 1)

# Закономерности выживаемости и возраста
summary(titanic$age)

titanic$age_group <- cut(titanic$age, 
                         breaks = c(0, 12, 18, 30, 50, 100),
                         labels = c("Ребёнок", "Подросток", "Молодой", 
                                    "Средний", "Пожилой"))

prop.table(table(titanic$age_group, titanic$survived), 1)

# Выживаемость в зависимости от кол-ва родственников
table(titanic$sibsp)

titanic$has_sibsp <- ifelse(titanic$sibsp > 0, "Есть", "Нет")
prop.table(table(titanic$has_sibsp, titanic$survived), 1)

# Комбинированный показатель семьи
titanic$family_size <- titanic$sibsp + titanic$parch + 1

titanic$family_cat <- ifelse(titanic$family_size == 1, "Один",
                             ifelse(titanic$family_size <= 4, "Маленькая семья",
                                    "Большая семья"))

prop.table(table(titanic$family_cat, titanic$survived), 1)

# Извлекаем префикс билета (первые символы)
titanic$ticket_prefix <- gsub("[0-9]", "", titanic$ticket)
titanic$ticket_prefix <- gsub("\\.", "", titanic$ticket_prefix)
titanic$ticket_prefix <- gsub(" ", "", titanic$ticket_prefix)
titanic$ticket_prefix[titanic$ticket_prefix == ""] <- "None"

# Самые частые префиксы
head(sort(table(titanic$ticket_prefix), decreasing = TRUE), 10)

# Анализ стоимости билетов
summary(titanic$fare)

# Нулевая стоимость
sum(titanic$fare == 0)

# Выживаемость по квартилям цены
titanic$fare_quartile <- cut(titanic$fare,
                             breaks = quantile(titanic$fare, 
                                               probs = c(0, 0.25, 0.5, 0.75, 1),
                                               na.rm = TRUE),
                             labels = c("Q1 (дешёвые)", "Q2", "Q3", "Q4 (дорогие)"))

prop.table(table(titanic$fare_quartile, titanic$survived), 1)

# Количество спасшихся в каждой шлюпке
boat_counts <- table(titanic$boat)
head(sort(boat_counts, decreasing = TRUE), 20)

# Среднее количество людей на шлюпку
mean(boat_counts, na.rm = TRUE)  

# Сколько тел было найдено?
sum(!is.na(titanic$body))  

# Выжило не выжило
prop.table(table(titanic$survived)) * 100

# Сколько выжило женщин или мужчин среди общего количества женщин/мужчин
prop.table(table(titanic$sex, titanic$survived), margin = 1) * 100

# 6. Считаем AUC
PredictROC <- predict(titanictree, newdata = Test)
probs_survived <- PredictROC[, 2]
pred <- prediction(probs_survived, Test$survived)
perf <- performance(pred, "tpr", "fpr")

auc <- performance(pred, "auc")@y.values[[1]]
cat("AUC =", auc)

plot(perf, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)  # Диагональ случайности
# 1. Установка и загрузка необходимых пакетов (если не установлены)
install.packages(c("readxl", "caret", "ggplot2", "dplyr", "class", "RColorBrewer"))
library(readxl)
library(caret)
library(ggplot2)
library(dplyr)
library(class)
library(RColorBrewer)
# 2. Проверим структуру данных
str(glass)
head(glass)
# 3. Подготовка данных
# Разделяем на признаки (X) и целевую переменную (y)
X <- glass %>% select(-Type)
y <- glass$Type
# Преобразуем целевую переменную в фактор (для корректной классификации)
y <- as.factor(y)
# 4. Разделение на обучающую (70%) и тестовую (30%) выборки
set.seed(42)  # для воспроизводимости
train_index <- createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]
# 5. Масштабирование признаков (ВАЖНО для k-NN!)
# Обучаем scaler на тренировочных данных
preproc <- preProcess(X_train, method = c("center", "scale"))
# Применяем преобразование к обеим выборкам
X_train_scaled <- predict(preproc, X_train)
X_test_scaled <- predict(preproc, X_test)
# 6. Обучение модели k-NN с k = 5
k_value <- 5
y_pred <- knn(train = X_train_scaled, 
              test = X_test_scaled, 
              cl = y_train, 
              k = k_value)
# 7. Оценка точности (Accuracy)
accuracy <- mean(y_pred == y_test)
cat("Accuracy модели k-NN (k =", k_value, "):", round(accuracy, 3), "\n")
# Более подробный отчет
confusion_matrix <- confusionMatrix(y_pred, y_test)
print(confusion_matrix)
# 8. Визуализация в 2D с помощью PCA
# Применяем PCA для снижения размерности до 2 компонент
pca_result <- prcomp(X_test_scaled, center = TRUE, scale. = TRUE)
pca_df <- as.data.frame(pca_result$x[, 1:2])
pca_df$True_Class <- y_test
pca_df$Predicted_Class <- y_pred
# 9. Создание визуализации
# Создаем палитру цветов для классов
n_classes <- length(levels(y_test))
color_palette <- brewer.pal(min(n_classes, 8), "Set2")
# Визуализация: точки - истинные классы, крестики - предсказанные
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  # Точки для истинных классов
  geom_point(aes(color = True_Class), size = 4, alpha = 0.7) +
  
  # Крестики для предсказанных классов
  geom_point(aes(shape = Predicted_Class), size = 3, color = "black", stroke = 1) +
  
  scale_shape_manual(values = c(4, 4, 4, 4, 4, 4, 4)) +  # все крестики
  scale_color_manual(values = color_palette) +
  
  labs(title = paste("k-NN классификация стекла (k =", k_value, ")"),
       subtitle = paste("Accuracy:", round(accuracy, 3)),
       x = "Первая главная компонента (PC1)",
       y = "Вторая главная компонента (PC2)",
       color = "Истинный класс",
       shape = "Предсказанный класс") +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right")
# 10. Дополнительная визуализация: матрица ошибок (confusion matrix)
confusion_df <- as.data.frame(confusion_matrix$table)

ggplot(confusion_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Матрица ошибок (Confusion Matrix)",
       x = "Истинный класс",
       y = "Предсказанный класс",
       fill = "Количество") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
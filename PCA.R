# Установка библиотеки
install.packages("factoextra")
install.packages("tidyverse")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("corrr")
install.packages("plotly")
# Загрузка библиотек
library(plotly)
library(corrr)
library (factoextra)
library(ggcorrplot)
library(FactoMineR)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
# Данные
data(mtcars)
pca_data <- mtcars
?mtcars
# Проверка данных
str(mtcars)
summary(mtcars)
# Cоздание вектора с названием стран-производителей авто
mtcars_country <-
  c(
    rep("Japan",3),
    rep("USA",4),
    rep("Europe",7),
    rep("USA",3),
    "Europe",
    rep("Japan",3),
    rep("USA",4),
    rep("Europe",3),
    "USA",
    rep("Europe",3)
  )
mtcars_country

# Матрица парных коэффициентов корреляции
corr_matrix <- round(cor(pca_data),digits = 2)
ggcorrplot(corr_matrix)

# Метод главных компонент
pca_result <- prcomp(x = pca_data,center = TRUE,scale.= TRUE)
pca_result
# Результаты
summary(pca_result)

# График собственных значений
fviz_eig(pca_result, barfill = "MediumPurple",barcolor = "BlueViolet", addlabels = TRUE,ylim = c(0,65))

# Матрица нагрузок двух первых главных компонент
pca_result$rotation[, 1:2]

# Вклад каждой переменной
fviz_cos2(pca_result, choice = "var", fill = "MediumPurple",color = "BlueViolet", axes = 1:2)

# Сочетание биплота с информацией о вкладе каждой переменной
fviz_pca_var(pca_result,col.var = "cos2",
             gradient.cols = c("black","BlueViolet","pink"),
             repel = TRUE)

# Визуализация данных в пространстве главных компонент
fviz_pca_ind(X = pca_result, repel = TRUE, col.ind = mtcars_country)

# PCA с 3 компонентами
pca_3d <- prcomp(pca_data, center = TRUE, scale. = TRUE, rank. = 3)
# Визуализация
pca_scores <- as.data.frame(pca_result$x[, 1:3])
pca_scores$country <- mtcars_country
pca_scores$car <- rownames(mtcars)

plot_ly(pca_scores, x = ~PC1, y = ~PC2, z = ~PC3, 
        color = ~country, text = ~car,
        type = "scatter3d", mode = "markers+text")



# Простой и понятный график с четкой интерпретацией
simple_pca_plot <- function() {
  # Получаем данные PCA
  scores <- pca_result$x[, 1:2]
  df <- data.frame(
    PC1 = scores[,1],
    PC2 = scores[,2],
    Car = rownames(mtcars),
    Country = mtcars_country
  )
  
  # Определяем квадранты
  df$Quadrant <- ifelse(df$PC1 > 0 & df$PC2 > 0, "I (Правый верхний)",
                        ifelse(df$PC1 < 0 & df$PC2 > 0, "II (Левый верхний)",
                               ifelse(df$PC1 < 0 & df$PC2 < 0, "III (Левый нижний)",
                                      "IV (Правый нижний)")))
  
  # Создаем график
  ggplot(df, aes(x = PC1, y = PC2, color = Country, label = Car)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3) +
    geom_text_repel(size = 3, max.overlaps = 15) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "PCA анализа автомобилей mtcars",
         subtitle = "Каждая точка - автомобиль, цвет - страна производитель",
         x = "PC1: Экономичные ← → Мощные",
         y = "PC2: Автоматы медленные ← → Ручные быстрые",
         color = "Страна") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Запускаем
simple_pca_plot()
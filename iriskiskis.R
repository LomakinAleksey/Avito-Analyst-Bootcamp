install.packages("tidyverse") 
install.packages("cluster")
install.packages("ggplot2")
library(ggplot2)
library(cluster)

# Загружаем набор данных Iris
iriss <- iris
# Проверим набор данных перед исследованием 
head(iriss)
str(iriss)

# Проведем разведочный анализ с помощью боксплотов 
ggplot(iriss)+geom_boxplot(aes(x = Species,y = Sepal.Length,fill = Species)) +theme_bw()
ggplot(iriss)+geom_boxplot(aes(x = Species,y = Petal.Length,fill = Species)) +theme_bw()
ggplot(iriss)+geom_boxplot(aes(x = Species,y = Sepal.Width,fill = Species)) +theme_bw()
ggplot(iriss)+geom_boxplot(aes(x = Species,y = Petal.Width,fill = Species))+theme_bw()

# Возьмем каждую пятую строку данных с целью улучшения визуализации дендрограммы 
iriss <- iris[seq(1,nrow(iris),5),]
iriss.dist <- daisy(iriss[,1:4])
iriss.dist

# Проведем процедуру кластеризации методом ближйшего соседа
iriss.h <- hclust(iriss.dist,method="single")
# Нарисуем дендрограмму
plot(iriss.h,labels=abbreviate(iriss[,5],1,method="both.sides"),main="")

# Проведем кластерный анализ методом k-средних (количество кластеров 3)
iriss <- iris
str(iris)
km <- kmeans(x = iriss[,-5], centers = 3)
clus <- km$cluster
table(clus)

# Визуализируем кластеры 
clusplot(iriss,
         clus,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
)

# Шаг 1: Создаем подвыборку ОДИН РАЗ
iriss <- iris[seq(1, nrow(iris), 5), ]  # 30 цветков

# Шаг 2: Расстояния для этой подвыборки
iriss.dist <- daisy(iriss[, 1:4])

# Шаг 3: Кластеризация для этой подвыборки
h <- hclust(iriss.dist, method = "median")

# Шаг 4: Метки для этой же подвыборки
plot(h, labels = abbreviate(iriss[, 5], 1, method = "both.sides"), 
     main = "Дендрограмма (метод median)")


table(iris$Species)

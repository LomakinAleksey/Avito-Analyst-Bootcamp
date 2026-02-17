# Инсталляция пакетов и загрузка библиотек
install.packages("arules")
library(arules)

# Загрузка данных
data(Groceries)
summary(Groceries)

# Частотная диаграмма для 20 товаров, пользующихся наибольшим спросом
itemFrequencyPlot(Groceries, topN=20, type='absolute')
# Частотная диаграмма для 30 товаров, пользующихся наибольшим спросом
itemFrequencyPlot(Groceries, topN=30, type='absolute')

# частотная диаграмма продуктов, частота встречаемости которых не ниже 15%
itemFrequencyPlot(Groceries, support=0.15, type='relative')

# Априорный алгоритм. 
# Учитывать будем только
# - те товары, которые встречаются с частотой не ниже 1%;
# - те ассоциативные правила, у которых confidence не ниже 50%.
myrules = apriori(data = Groceries, parameter = list(support=0.001, confidence=0.9, minlen=1))
# первые 5 ассоциативных правил
inspect(myrules[1:5])

# Немного повысим значение support, т.е. популярность продукта.
# Кроме первого, правила изменились. Теперь в левой части наборы с более популярными продуктами. 
# Помимо этого, новые правила имеют более высокую степень надежности. 
# Можно сделать вывод, что принцип отбора списков улучшен.
rules1 = apriori(data = Groceries, parameter = list(support=0.0013, confidence=0.9, minlen=1))
inspect(rules1[1:5])

# Уменьшим support и увеличим confidence
# Теперь значение только одного правила достигает первоначального 1% поддержки,
# но у всех максимальная надежность, то есть гарантированное приобритение товара
rules2 = apriori(data = Groceries, parameter = list(support=0.0007, confidence=0.95, minlen=1))
inspect(rules2[1:5])

# Cводная информация о наборе полученных ассоциативных правил
summary(myrules)

# Отсортируем все правила в порядке убывания лифта
myrules=sort(myrules, by="lift")
inspect(myrules[1:5])

# Отсортируем скорректированные правила в порядке убывания поддержки
rules2S=sort(rules2, by="support")
inspect(rules2S[1:10])

# Отсортируем скорректированные правила в порядке убывания достоверности
rules2S=sort(rules2, by="confidence")
inspect(rules2[1:10])

# Отсортируем скорректированные правила в порядке убывания лифта
rules2S=sort(rules2, by="lift")
inspect(rules2S[1:10])

# Нет ни одного правила, которое входило бы во все три топа.
#Прагматичные правила: фрукты с косточками, сметана, хлеб -> овощи; цитрусовые, яйца, сахар -> молоко
#Тривиальные правила: ликер, красное вино -> пиво в бутылках; сыр, хлопья -> молоко

# Найдём все ассоциативные правила, где есть молоко
milkrules<-subset(myrules, items %in% "whole milk")
# Отсортируем список правил, включающих молоко, по убыванию лифта и
# распечатаем первые 5 правил:
inspect(sort(milkrules, by="lift")[1:5])

# все ассоциативные правила, где есть рис
ricerules <- subset(myrules, items %in% "rice")
inspect(sort(ricerules, by="lift")[1:5])

# правила, где есть сливочное или растительное масло
borules <- subset(myrules, items %in% c("butter", "oil"))
inspect(sort(borules, by="lift")[1:5])

# правила, где есть молоко и белый хлеб
mbrules <- subset(myrules, items %ain% c("whole milk", "white bread"))
inspect(sort(mbrules, by="lift"))

# сохраним найденное подмножество ассоциативных правил в файл
write(borules, file = "borules.txt", sep = ",", quote =
        TRUE, row.names = FALSE)
# представим в виде датасета
bo_df <- as(borules, "data.frame")
bo_df
# Сохраним ассоциативные правила, где есть сливочное и раст масло в папку Документы
write.csv2(bo_df, "Ассоциативные правила_масло.csv")


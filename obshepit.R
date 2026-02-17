#Загрузка пакетов
install.packages("rsconnect")
install.packages("tidyverse")
install.packages("leaflet")
install.packages("plotly")
install.packages("stringr")
library(rsconnect)
library(tidyverse) # для общей обработки данных
library(leaflet) # для отображения на карте
library(ggplot2) # для визуализации
library(plotly) # для интерактивных визуализаций
library(stringr) # для обработки текста

# Обработка даанных
kafe <- Obshchepit # Чтение данных
kafe$Longitude_WGS84 <- as.numeric(kafe$Longitude_WGS84) # Преобразование долготы в числовой формат
kafe$Latitude_WGS84 <- as.numeric(kafe$Latitude_WGS84) # Преобразование широты в числовой формат
kafe$SeatsCount <- as.numeric(kafe$SeatsCount) # Преобразование вместимости предприятий общепита в числовой формат
str(kafe)
summary(kafe)
colSums(is.na(kafe))# Определяем, есть ли пропуски в данных

# Удаление первой строки
kafe<-kafe[-1,]
plot_01 = NA %>%
  leaflet() %>% # Инициализация
  addTiles() %>% # Добавление холста
  addProviderTiles("OpenStreetMap.Mapnik") %>% # Выбор карты OpenStreetMap
  addCircleMarkers( # Определение параметров меток
    lng = kafe$Longitude_WGS84, # Добавление широты
    lat = kafe$Latitude_WGS84, # Добавление долготы
    clusterOptions = markerClusterOptions(), # Добавление условия кластеризации
    radius = kafe$SeatsCount / 25, # Привязка радиуса метки к вместимости 
    popup = paste(paste('<u>', 'Адрес', '</u>', ': ', kafe$Address, sep = ''), # Адрес
                  paste('<u>', 'Вместимость', '</u>', ': ', kafe$SeatsCount, ' мест', sep = ''), # Вместимость
                  sep = "<br>"), # Разделитель в подсказке
    label = kafe$Name) # Название

plot_01 # Запуск визуализации

#Визуализация анализа данных
distr = kafe %>% group_by(District) %>% summarise(count = n()) %>% # Расчёт количества кафе по районам
  mutate(paint = # Параметр цвета
           ifelse(count >= quantile(count)[5], 'rgba(0, 197, 192, 1.0)',
                  ifelse(count >= quantile(count)[4], 'rgba(0, 226, 167, 1.0)', 
                         ifelse(count >= quantile(count)[3], 'rgba(0, 226, 215, 1.0)',
                                'rgba(119, 128, 158, 1.0)'))))

# Построение графика с использованием plotly
plot_02 = plot_ly(distr, x = ~count, y = ~reorder(District, count),
                  type = 'bar', orientation = 'h',
                  text = ~count, textposition = 'inside', insidetextanchor = 'start',
                  marker = list(color = ~paint)) %>%
  layout(title = paste('<b>', 'Количество предприятий общепита', '</b>', sep = ''), 
         xaxis = list(title = "", zeroline = F, showline = F, showticklabels = F, showgrid = F),
         yaxis = list(title = ''),
         font = list(family = 'Arial', size = 14))

plot_02 # Запуск визуализации

#Покажем количество предприятий общепита по категориям вместимости     
b1 = 5; b2 = 10; b3 = 50; b4 = 100 # Границы категорий
colors = c('steelblue', 'cadetblue', 'orange', 'lightseagreen', 'yellow') # Цвета категорий

capacity = kafe %>%
  mutate(category = # Параметр границ категорий
           ifelse(SeatsCount < b1,
                  paste('До ', as.character(b1), ' мест', sep = ''),
                  ifelse(SeatsCount >= b1 & SeatsCount < b2,
                         paste('От ', as.character(b1), ' до ', as.character(b2), ' мест', sep = ''), 
                         ifelse(SeatsCount >= b2 & SeatsCount < b3,
                                paste('От ', as.character(b2), ' до ', as.character(b3), ' мест', sep = ''),
                                ifelse(SeatsCount >= b3 & SeatsCount < b4,
                                       paste('От ', as.character(b3), ' до ', as.character(b4), ' мест', sep = ''),
                                       paste('От ', as.character(b4), ' мест', sep = '')))))) %>%
  group_by(category) %>% summarise(count = n()) # Расчёт количества предприятий общепита по категориям вместимости

# Построение диаграммы с использованием plotly
plot_03 = plot_ly(capacity, labels = ~category, values = ~count, type = 'pie',
                  textposition = 'inside', textinfo = 'label+percent+value', showlegend = FALSE,
                  marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
  layout(title = paste('<b>', 'Предприятия общепита', '</b>', sep = ''),
         font = list(family = 'Arial', size = 13))

plot_03 # Запуск визуализации

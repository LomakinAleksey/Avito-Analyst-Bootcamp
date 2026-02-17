# Установка и загрузка пакетов
required_packages <- c("plotly", "quantmod", "lubridate", "dplyr", "ggplot2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(plotly)
library(quantmod)
library(lubridate)
library(dplyr)
library(ggplot2)

# ========== ЧАСТЬ 1: Загрузка и подготовка данных ==========
cat("Загрузка данных AAPL с Yahoo Finance...\n")
invisible(getSymbols("AAPL", src = 'yahoo'))

df <- data.frame(Date = index(AAPL), coredata(AAPL))
df <- tail(df, 100)

# Подготовка дат
df$Date_Formatted <- format(df$Date, "%d.%m.%Y")
df$Day_Number <- seq.int(nrow(df))

# Функция для анимации
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df_animated <- df %>% accumulate_by(~Day_Number)

# ========== ЧАСТЬ 2: Логарифмическая доходность и проверка гипотез ==========
cat("\nРасчёт логарифмической доходности...\n")
df$Log_Return <- c(NA, diff(log(df$AAPL.Close)))
df_returns <- df[-1, ]

# Разделение на два периода для демонстрации
half_point <- floor(nrow(df_returns) / 2)
returns_before <- df_returns$Log_Return[1:half_point]
returns_after <- df_returns$Log_Return[(half_point + 1):nrow(df_returns)]

# T-тест
t_test_result <- t.test(returns_before, returns_after, var.equal = FALSE)

# ========== ЧАСТЬ 3: Улучшенная анимация ==========
cat("Создание анимационного графика...\n")
p_final <- ggplot(df_animated, aes(x = Day_Number, y = AAPL.Close, frame = frame)) +
  geom_area(fill = "#2E86AB", alpha = 0.2) +
  geom_line(color = "#2E86AB", size = 1.5) +
  geom_point(data = df_animated %>% group_by(frame) %>% slice_tail(n = 1),
             aes(x = Day_Number, y = AAPL.Close),
             color = "#FF6B6B", size = 4) +
  geom_hline(yintercept = min(df$AAPL.Close), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = max(df$AAPL.Close), linetype = "dashed", color = "green", alpha = 0.5) +
  labs(
    title = "Динамика цены акций Apple (AAPL) за 100 дней",
    x = "Дата",
    y = "Цена закрытия ($)"
  ) +
  scale_x_continuous(
    breaks = seq(1, 100, by = 10),
    labels = function(x) {
      idx <- pmin(x, nrow(df))
      format(df$Date[idx], "%d.%m")
    }
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

animation_final <- df_animated %>%
  plot_ly(
    x = ~Day_Number,
    y = ~AAPL.Close,
    frame = ~frame,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(color = '#2E86AB', width = 2),
    marker = list(size = 6, color = '#FF6B6B'),
    hoverinfo = 'text',
    text = ~paste('Дата:', df$Date_Formatted[Day_Number], 
                  '<br>Цена: $', round(AAPL.Close, 2))
  ) %>%
  layout(
    title = "Динамика цены акций Apple (AAPL) за 100 дней",
    xaxis = list(
      title = "Дата",
      tickmode = "array",
      tickvals = seq(1, 100, by = 10),
      ticktext = format(df$Date[seq(1, 100, by = 10)], "%d.%m")
    ),
    yaxis = list(
      title = "Цена ($)",
      tickprefix = "$"
    )
  ) %>%
  animation_opts(
    frame = 150,
    transition = 150,
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "День: ", font = list(size = 14))
  )

# ========== ЧАСТЬ 4: Вывод результатов ==========
cat("\n=== РЕЗУЛЬТАТЫ АНАЛИЗА ===\n\n")

cat("1. Описательная статистика доходности:\n")
cat(sprintf("   Период 1 (первые %d дней):\n", half_point))
cat(sprintf("     Средняя дневная доходность: %.6f\n", mean(returns_before, na.rm = TRUE)))
cat(sprintf("     Волатильность: %.6f\n\n", sd(returns_before, na.rm = TRUE)))

cat(sprintf("   Период 2 (последние %d дней):\n", length(returns_after)))
cat(sprintf("     Средняя дневная доходность: %.6f\n", mean(returns_after, na.rm = TRUE)))
cat(sprintf("     Волатильность: %.6f\n\n", sd(returns_after, na.rm = TRUE)))

cat("2. Проверка гипотезы о равенстве средних:\n")
cat(sprintf("   t-статистика: %.4f\n", t_test_result$statistic))
cat(sprintf("   p-value: %.6f\n", t_test_result$p.value))
cat(sprintf("   Доверительный интервал 95%%: [%.6f, %.6f]\n\n", 
            t_test_result$conf.int[1], t_test_result$conf.int[2]))

cat("3. Ключевые ценовые метрики:\n")
cat(sprintf("   Начальная цена: $%.2f\n", head(df$AAPL.Close, 1)))
cat(sprintf("   Конечная цена: $%.2f\n", tail(df$AAPL.Close, 1)))
cat(sprintf("   Минимальная цена: $%.2f\n", min(df$AAPL.Close)))
cat(sprintf("   Максимальная цена: $%.2f\n", max(df$AAPL.Close)))
cat(sprintf("   Общая доходность за период: %.2f%%\n", 
            (tail(df$AAPL.Close, 1) - head(df$AAPL.Close, 1)) / head(df$AAPL.Close, 1) * 100))

# Вывод графиков
cat("\nГрафики отображаются в отдельных окнах...\n")
print(animation_final)

# График распределения доходностей
dist_plot <- plot_ly() %>%
  add_trace(x = ~returns_before, type = "histogram", name = "Период 1", 
            opacity = 0.7, marker = list(color = 'blue')) %>%
  add_trace(x = ~returns_after, type = "histogram", name = "Период 2", 
            opacity = 0.7, marker = list(color = 'red')) %>%
  layout(title = "Распределение логарифмической доходности",
         xaxis = list(title = "Доходность"),
         yaxis = list(title = "Частота"),
         barmode = "overlay")

print(dist_plot)

cat("\n=== АНАЛИЗ ЗАВЕРШЕН ===\n")
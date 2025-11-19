# ============================================================
# Скрипт для быстрой переустановки "моих" пакетов после обновления R
# ============================================================

# 1. Базовые настройки ---------------------------------------

# Задаём зеркало CRAN (можно сменить на любое другое)
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  # На Windows часто помогает libcurl; можно убрать, если не нужно
  download.file.method = "libcurl"
)

# Вспомогательная функция: установить только недостающие пакеты
install_if_missing <- function(pkgs) {
  installed <- rownames(installed.packages())
  pkgs_to_install <- setdiff(pkgs, installed)

  if (length(pkgs_to_install) == 0L) {
    message("Все указанные пакеты уже установлены.")
    return(invisible(NULL))
  }

  message("Устанавливаю пакеты: ", paste(pkgs_to_install, collapse = ", "))
  install.packages(pkgs_to_install)
}

# 2. Группы CRAN-пакетов -------------------------------------

# 2.1. Базовый workflow: данные, строки, даты, файлы ---------
core_pkgs <- c(
  "tidyverse",   # dplyr, ggplot2 и др.
  "data.table",  # быстрые таблицы
  "here",        # аккуратные пути к файлам в проекте
  "glue",        # строковый интерполяционный "клей"
  "stringr",     # работа со строками
  "stringi",     # низкоуровневая работа со строками
  "lubridate",   # даты и время
  "rlang",       # инфраструктура tidyverse
  "readr",       # быстрый импорт csv/tsv и т.п.
  "readxl",      # импорт из Excel
  "jsonlite",    # JSON
  "httr",        # HTTP-запросы
  "archive",     # работа с архивами
  "writexl",     # экспорт в Excel
  "tidyr",       # reshape/reshaping данных
  "purrr",       # функциональное программирование
  "reshape2"     # старый, но иногда удобный для wide/long
)

# 2.2. API и экономические данные ----------------------------
data_api_pkgs <- c(
  "WDI",         # World Bank
  "IMFData",     # API МВФ
  "imfr",        # ещё один интерфейс к данным МВФ
  "wbstats",     # ещё один World Bank API
  "Rilostat",    # ILOSTAT
  "countrycode", # коды стран
  "quantmod",    # загрузка финансовых данных
  "Quandl",      # Quandl API
  "sophisthse",  # sophist.hse.ru
  "tidyquant",   # tidy-интерфейс к финансовым данным
  "timetk"       # работа с временными рядами в tidy-стиле
)

# 2.3. Временные ряды ----------------------------------------
ts_pkgs <- c(
  "zoo",      # нерегулярные временные ряды
  "xts",      # ещё ряды
  "forecast", # ARIMA, экспон. сглаживание
  "rugarch"   # GARCH-модели
)

# 2.4. Визуализация и графика --------------------------------
viz_pkgs <- c(
  "ggplot2",
  "ggthemes",   # темы для ggplot
  "ggiraph",    # интерактивные графики
  "ggraph",     # графы поверх igraph
  "GGally",     # матрицы рассеяния и др.
  "hexbin",     # гексагоны
  "vcd",        # мозаичные графики
  "lattice",    # альтернативный графический движок
  "sjPlot",     # визуализация результатов регрессий
  "factoextra", # удобная PCA и кластеризация
  "igraph"      # теория графов
)

# 2.5. Отчёты, таблицы, LaTeX/markdown -----------------------
report_pkgs <- c(
  "knitr",    # движок для rmarkdown и LaTeX
  "rmarkdown",
  "xtable",   # таблицы -> LaTeX
  "texreg",   # сравнение моделей в LaTeX/HTML/word
  "pander",   # таблицы в markdown
  "memisc",   # таблицы моделей
  "huxtable", # красивые таблицы
  "psych",    # описательные статистики
  "skimr"     # быстрый overview по данным
)

# 2.6. Эконометрика и прикладная статистика -----------------
econometrics_pkgs <- c(
  "AER",       # Applied Econometrics with R
  "car",       # линейные гипотезы, vif и пр.
  "ivpack",    # инструментальные переменные
  "lmtest",    # тесты в линейных моделях
  "sandwich",  # робастные ковариационные матрицы
  "erer",      # набор полезных функций
  "estimatr",  # робастные стандартные ошибки
  "AUC",       # AUC для ROC
  "mfx",       # предельные эффекты для logit/probit
  "glmnet",    # LASSO / ridge / elastic net
  "sgof",      # множественные проверки
  "spikeslab", # spike-and-slab регрессия
  "quantreg",  # квантильная регрессия
  "MCMCpack",  # байесовские модели
  "HSAUR",     # набор данных и примеров
  "caret"      # ML-кросс-валидация и подбор параметров
)

# 2.7. Прочее -------------------------------------------------
misc_pkgs <- c(
  "gsubfn",   # удобные замены по шаблонам
  "shiny",    # веб-приложения
  "rJava",    # интерфейс к Java
  "devtools"  # разработка пакетов / GitHub
)

# Собираем всё в один уникальный список CRAN-пакетов ----------
all_cran_pkgs <- unique(c(
  core_pkgs,
  data_api_pkgs,
  ts_pkgs,
  viz_pkgs,
  report_pkgs,
  econometrics_pkgs,
  misc_pkgs
))

# Устанавливаем все CRAN-пакеты (будут поставлены только отсутствующие)
install_if_missing(all_cran_pkgs)

# 3. Пакеты не с CRAN -----------------------------------------

# 3.1. devtools для установки GitHub-пакетов
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# 3.2. rlms с GitHub (bdemeshev/rlms)
if (!requireNamespace("rlms", quietly = TRUE)) {
  devtools::install_github("bdemeshev/rlms")
  devtools::install_github("jasonhilton/ggfan")
  devtools::install_github("hrbrmstr/hrbrthemes")
}

# 3.3. rusquant с r-forge
if (!requireNamespace("rusquant", quietly = TRUE)) {
  install.packages(
    "rusquant",
    repos = "http://r-forge.r-project.org",
    type = "source"
  )
}

message("Готово: все указанные пакеты установлены (или уже были установлены).")
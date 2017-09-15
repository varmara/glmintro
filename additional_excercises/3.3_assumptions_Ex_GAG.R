# ## Упражнение по проверке условий применимости линейной регрессии ####
#
# Глюкозаминогликаны в моче
#
# Глюкозаминогликаны участвуют в регуляции межклеточных
# сигнальных путей, в развитии нейронов, в формировании
# хрящевой ткани. Измерение концентрации глюкозаминогликанов
# часто используется при диагностике и классификации
# мукополисахароидозов Данные, о которых пойдет речь - это
# концентрация глюкозаминогликанов в моче у 314 здоровых
# детей разного возраста (данные собраны S.Prosser,
# Paediatrics Department, University of Oxford; источник
# данных Venables, Ripley 2002). Подобного рода данные можно
# использовать для получения таблиц референсных значений
# концентрации глюкозаминогликанов в разном возрасте.
# Переменные:
# - Age - возраст ребенка
# - GAG - концентрация глюкозаминогликанов (единицы измерения утрачены)


#### Загрузка пакетов из библиотеки ####

library(readxl)
library(ggplot2)
library(car)

#### Знакомство с данными ####

# Не забудьте установить рабочую директорию или
# отредактируйте путь к файлу данных

# Открываем данные
gag <- read_excel("data/GAGurine.xlsx", sheet = 1)
head(gag)

str(gag)

# Есть ли пропущенные значения?
colSums(is.na(gag))

# Есть ли выбросы? Построим дот-плот
gg_dot <- ggplot(gag, aes(y = 1:nrow(gag))) + geom_point()
gg_dot + aes(x = GAG)
gg_dot + aes(x = Age)
# Выбросов нет

# График зависимости
ggplot(gag, aes(x = Age, y = GAG)) +
  geom_point()

# Добавим линию регрессии
ggplot(gag, aes(x = Age, y = GAG)) +
  geom_point() +
  geom_smooth(method = "lm")
# Сразу понятно, что линейная регрессия по сырым данным здесь не подойдет

# Попробуем трансформировать переменные
ggplot(gag, aes(x = Age, y = log(GAG))) +
  geom_point() +
  geom_smooth(method = "lm")
# не очень хорошо,  но сойдет
ggplot(gag, aes(x = Age, y = GAG^(0.1))) +
  geom_point() +
  geom_smooth(method = "lm")
# не очень, но похоже
ggplot(gag, aes(x = log(Age), y = GAG)) +
  geom_point() +
  geom_smooth(method = "lm")
# Линейно, но будет гетерогенность дисперсии.
# Печально, но придется трансформировать отклик, что плохо

# - трансформируем данные

#### ВАРИАНТ C ТРАНСФОРМАЦИЕЙ ############

# Трансформируем
gag$GAG_l <- log(gag$GAG)

M1 <- lm(GAG_l ~ Age, data = gag)
summary(M1)

#### Проверяем условия применимости ####

# Данные для графиков остатков
M1_diag <- fortify(M1)

# 1) График расстояния Кука
ggplot(M1_diag, aes(x = 1:nrow(M1_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = M1_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid
# Видна нелинейность и небольшая гетерогенность дисперсий

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = Age)
# Видна нелинейность и небольшая гетерогенность дисперсий

# 4) Квантильный график остатков
qqPlot(M1)
# Видны отклонения от нормального распределения

# Итог:
# На самом деле эти данные нужно анализировать при помощи нелинейной регрессии

#### Описываем результаты ####
summary(M1)

# Call:
#   lm(formula = GAG_l ~ Age, data = gag)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -1.57292 -0.21444 -0.04702  0.15543  1.29594
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  2.966099   0.029050  102.10   <2e-16 ***
#   Age         -0.113917   0.004003  -28.45   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.353 on 312 degrees of freedom
# Multiple R-squared:  0.7218,	Adjusted R-squared:  0.721
# F-statistic: 809.7 on 1 and 312 DF,  p-value: < 2.2e-16

# Способ 1: Концентрация глюкозаминогликанов в моче достоверно уменьшается с возрастом (t = -28.45, df = 312, p < 0.01)

# Способ 2: Концентрация глюкозаминогликанов в моче достоверно уменьшается с возрастом (F = 810, df1 = 1, df2 = 312, p < 0.01)

# Уравнение модели:
# GAG_l = 2.97 - 0.11Age

# Модель описывает 72% общей изменчивости (R^2 = 0.72)

#### График модели  ####

# Обычный график нам не подходит - там предиктор  трансформирован
ggplot(gag, aes(x = Age, y = GAG)) +
  geom_point() +
  geom_smooth(method = "lm")

# Построим график с обратной трансформацией данных

# Данные для графика
NewData <- data.frame(
  Age = seq(min(gag$Age), max(gag$Age), length = 100))

# предсказанные значения
Predictions <- predict(M1, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# Обратная трансформация
NewData$GAG <- exp(NewData$fit)
NewData$upr_tr <- exp(NewData$upr)
NewData$lwr_tr <- exp(NewData$lwr)

# График модели после обратной трансформации
ggplot(NewData, aes(x = Age, y = GAG)) +
  geom_point(data = gag, aes(x = Age, y = GAG), alpha = 0.3) +
  geom_ribbon(alpha = 0.7, aes(ymin = lwr_tr, ymax = upr_tr), fill = "red") +
  geom_line(color = "blue")

# Трансформация не помогла, возможно, эти данные стоит
# анализировать при помощи нелинейной регрессии


# ## Упражнение по проверке условий применимости линейной
# регрессии ####
#
# Недеструктивное определение возраста моллюсков
#
# Эта версия данных - это случайная выборка из полных данных
# исследования популяционной биологии моллюсков Haliotis
# rubra (Nash et al, 1994).
# Полная версия данных может быть найдена в
# UCI machine learning repository
# https://archive.ics.uci.edu/ml/datasets/abalone

# Nash, W.J., Sellers, T.L., Talbot, S.R., Cawthorn, A.J.
# and Ford, W.B., 1994. The Population Biology of Abalone
# (Haliotis species). Blacklip Abalone (H. rubra) from the
# North Coast and Islands of Bass Strait. Sea Fisheries
# Division Technical Report, 48.

# Можно ли предсказать возраст моллюска используя морфометрические измерения?
#
# 1335 наблюдения, 6 переменных
# Переменные (только часть по сравнению с полными данными):

# rings - число колец остановки роста (возраст)
# sex - пол (M - самцы, F - самки, I - незрелые особи
# length - длина раковины
# diameter - диаметр раковины
# shell_weight - вес раковины
# whole_weight - общий вес

#### Загрузка пакетов из библиотеки ####

library(readxl)
library(ggplot2)

#### Знакомство с данными ####

# Не забудьте установить рабочую директорию или
# отредактируйте путь к файлу данных

# Открываем данные
abal <- read_excel("data/abalone_short.xlsx", sheet = 1)
head(abal)

str(abal)

# Есть ли пропущенные значения?
colSums(is.na(abal))

table(abal$rings)
table(abal$sex, abal$rings)

# Есть ли выбросы? Построим дот-плот
gg_dot <- ggplot(abal, aes(y = 1:nrow(abal))) + geom_point()
gg_dot + aes(x = sex)
gg_dot + aes(x = rings)
gg_dot + aes(x = length)
gg_dot + aes(x = diameter)
gg_dot + aes(x = shell_weight)
gg_dot + aes(x = whole_weight)

# Есть один очень молодой моллюск, возможно, его придется удалить

# График зависимости
ggplot(abal, aes(x = diameter, y = rings)) +
  geom_point()

# Добавим линию регрессии
ggplot(abal, aes(x = diameter, y = rings)) +
  geom_point() +
  geom_smooth(method = "lm")

# Похоже на гетерогенность дисперсий.

# Попробуем трансформировать переменные
ggplot(abal, aes(x = diameter, y = log(rings))) +
  geom_point() +
  geom_smooth(method = "lm")

# В целях обучения попробуем сделать несколько вариантов анализа:
# - продолжим без трансформации и изучим остатки
# - трансформируем данные.

# Удалим из данных молодого моллюска
abal1 <- abal[abal$rings > 1, ]

#### ВАРИАНТ БЕЗ ТРАНСФОРМАЦИИ ############
#### Подбираем модель ####

M1 <- lm(rings ~ diameter, data = abal1)
summary(M1)
# Call:
#   lm(formula = rings ~ diameter, data = abal1)
#
# Residuals:
#   Min     1Q Median     3Q    Max
# -5.337 -1.705 -0.724  0.939 14.271
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)    2.012      0.305    6.59  6.2e-11 ***
#   diameter      19.631      0.726   27.04  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.63 on 1331 degrees of freedom
# Multiple R-squared:  0.355,	Adjusted R-squared:  0.354
# F-statistic:  731 on 1 and 1331 DF,  p-value: <2e-16

#### Проверяем условия применимости ####

# Расстояние Кука
cook_cutoff <- 4 / (nrow(abal1) - length(coef(M1) - 2))
plot(M1, which = 4, cook.levels = cook_cutoff)
# OK

# График остатков
residualPlot(M1)
# Вот теперь точно видно гетерогенность дисперсий

# Квантильный график остатков
set.seed(293234)
qqPlot(M1)
# OK

# Итог: на графике остатков видно, гетерогенность дисперсии
# необходима трансформация

#### ВАРИАНТ C ТРАНСФОРМАЦИЕЙ ############

# Трансформируем предиктор
# Внимательно смотрите в том ли датасете вы это делаете
abal1$rings_l <- log(abal1$rings)

M2 <- lm(rings_l ~ diameter, data = abal1)
summary(M2)

#### Проверяем условия применимости ####

## Расстояние Кука
cook_cutoff <- 4 / (nrow(abal1) - length(coef(M2) - 2))
plot(M2, which = 4, cook.levels = cook_cutoff)
# OK

## График остатков
residualPlot(M2)
# трансформация чуть помогла, но все равно видна
# гетерогенность. И небольшая нелинейность

## Квантильный график остатков
set.seed(26326499)
qqPlot(M2)
# ОСильные отклонения от нормального распределения.

## Графики остатков от предикторов в модели и нет
M2_diag <- data.frame(abal1,
                      .resid = resid(M2, type = "pearson"))
gg_res <- ggplot(M2_diag, aes(x = diameter, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point()
gg_res

#### Описываем результаты ####
summary(M2)

# Call:
#   lm(formula = rings_l ~ diameter, data = abal1)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -0.6115 -0.1613 -0.0447  0.1344  0.9001
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   1.3400     0.0273    49.1   <2e-16 ***
#   diameter      2.2384     0.0649    34.5   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.235 on 1331 degrees of freedom
# Multiple R-squared:  0.472,	Adjusted R-squared:  0.471
# F-statistic: 1.19e+03 on 1 and 1331 DF,  p-value: <2e-16



## Уравнение модели
# rings_l = 1.34 + 2.24diameter

## Доля объясненной изменчивости
# Adjusted R-squared:  0.471

#### График модели  ####

# Построим график с обратной трансформацией данных

# Данные для графика
NewData <- data.frame(
  diameter = seq(min(abal$diameter), max(abal$diameter), length = 100))

# предсказанные значения
Predictions <- predict(M2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# Обратная трансформация
NewData$rings <- exp(NewData$fit)
NewData$upr_tr <- exp(NewData$upr)
NewData$lwr_tr <- exp(NewData$lwr)

# График модели после обратной трансформации
ggplot(NewData, aes(x = diameter, y = rings)) +
  geom_ribbon(alpha = 0.5, aes(ymin = lwr_tr, ymax = upr_tr), fill = "red") +
  geom_line() +
  geom_point(data = abal, aes(x = diameter, y = rings), alpha = 0.2)

# Трансформация не очень помогла Эти данные стоит
# анализировать при помощи пуассоновской GLM, т.к.
# число колец  на раковине - счетная переменная, и
# пуассоновское распределение сможет смоделировать
# гетерогенность дисперсий.
# Кроме того, модель построенная  по трансформированным
# данным, недооценивает изменчивость


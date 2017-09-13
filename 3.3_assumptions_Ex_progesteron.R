# ## Упражнение по проверке условий применимости линейной
# регрессии ####
#
# Уровень прогестерона и выносливость
#
# Данные: Исследование влияния фазы менструального цикла на
# уровень потребления кислорода и вентиляцию легких у женщин
# на высоте >3600 м над уровнем моря (Brutsaert  et al.
# 2002; источник данных Whitlock, Schluter 2009)
#
# Brutsaert, T.D., Spielvogel, H., Caceres, E., Araoz, M.,
# Chatterton, R.T. and Vitzthum, V.J., 2002. Effect of
# menstrual cycle phase on exercise performance of
# high-altitude native women at 3600 m. Journal of
# experimental biology, 205(2), pp.233-239.
#
# В исследовании приняли участие 30 женщин. Эти данные -
# результат измерений во время лютеиновой фазы цикла.
#
# Переменные: - progesterone - уровень прогестерона в слюне
# (пг/мл) - ventilation - вентиляция легких (мл/мин)


#### Загрузка пакетов из библиотеки ########################

library(readxl)
library(ggplot2)

#### Знакомство с данными ##################################

# Не забудьте установить рабочую директорию или
# отредактируйте путь к файлу данных

# Открываем данные
prog <- read_excel("data/progesterone_Brutsaert_et al_2002_lm.xlsx", sheet = 1)
head(prog)

str(prog)

names(prog) <- c("pr", "vent")
# Есть ли пропущенные значения?
colSums(is.na(prog))

# Есть ли выбросы? Построим дот-плот
gg_dot <- ggplot(prog, aes(y = 1:nrow(prog))) + geom_point()
gg_dot + aes(x = vent)
gg_dot + aes(x = pr)
# Выбросов нет

# График зависимости
ggplot(prog, aes(x = pr, y = vent)) +
  geom_point()

# Добавим линию регрессии
ggplot(prog, aes(x = pr, y = vent)) +
  geom_point() +
  geom_smooth(method = "lm")

# Похоже на гетерогенность дисперсий.

# Попробуем трансформировать переменные
ggplot(prog, aes(x = log(pr), y = vent)) +
  geom_point() +
  geom_smooth(method = "lm")
# не очень
ggplot(prog, aes(x = pr, y = log(vent))) +
  geom_point() +
  geom_smooth(method = "lm")
# может стать немного лучше
ggplot(prog, aes(x = pr^2, y = vent)) +
  geom_point() +
  geom_smooth(method = "lm")
# стало хуже

# В целях обучения попробуем сделать несколько вариантов анализа:
# - продолжим без трансформации и изучим остатки
# - трансформируем данные.

#### ВАРИАНТ БЕЗ ТРАНСФОРМАЦИИ ##############################
#### Подбираем модель ######################################

M1 <- lm(vent ~ pr, data = prog)
summary(M1)

# Call:
#   lm(formula = vent ~ pr, data = prog)
#
# Residuals:
#   Min     1Q Median     3Q    Max
# -21.74  -5.64   0.15   5.47  32.17
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  59.0971     4.3845   13.48  9.2e-14 ***
#   pr            0.1734     0.0588    2.95   0.0063 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 12.8 on 28 degrees of freedom
# Multiple R-squared:  0.237,	Adjusted R-squared:  0.21
# F-statistic: 8.71 on 1 and 28 DF,  p-value: 0.00635

#### Проверяем условия применимости ########################

# Данные для графиков остатков
M1_diag <- fortify(M1)

# 1) График расстояния Кука
ggplot(M1_diag, aes(x = 1:nrow(M1_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = M1_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = pr)

# 4) Квантильный график остатков
qqPlot(M1)

# Итог: на графике остатков видна гетерогенность дисперсии,
# необходима трансформация

#### ВАРИАНТ C ТРАНСФОРМАЦИЕЙ ##############################

# Трансформируем предиктор
prog$pr_l <- log(prog$pr)

M2 <- lm(vent ~ pr_l, data = prog)
summary(M2)

#### Проверяем условия применимости ########################

# Данные для графиков остатков
M2_diag <- fortify(M2)

# 1) График расстояния Кука
ggplot(M2_diag, aes(x = 1:nrow(M2_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = M2_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid
# Гетерогенность дисперсий

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = pr_l)
# Гетерогенность дисперсий

# 4) Квантильный график остатков
qqPlot(M2)


#### Описываем результаты ##################################
summary(M2)

# Call:
#   lm(formula = vent ~ pr_l, data = prog)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -21.418  -7.159  -0.268   5.356  31.505
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)    38.26      12.64    3.03   0.0053 **
#   pr_l           8.15       3.18    2.56   0.0161 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 13.2 on 28 degrees of freedom
# Multiple R-squared:  0.19,	Adjusted R-squared:  0.161
# F-statistic: 6.56 on 1 and 28 DF,  p-value: 0.0161

# Вентиляция достоверно зависит от уровня прогестерона (t-тест, p < 0.05 )

## Уравнение модели
# vent = 38.26 + 8.15pr_l

## Доля объясненной изменчивости
# Adjusted R-squared:  0.161

#### График модели  ########################################

# Обычный график нам не подходит - там предиктор  трансформирован
ggplot(prog, aes(x = pr_l, y = vent)) +
  geom_point() +
  geom_smooth(method = "lm")

# Построим график с обратной трансформацией данных

# Данные для графика
NewData <- data.frame(
  pr_l = seq(min(prog$pr_l), max(prog$pr_l), length = 100))

# предсказанные значения
Predictions <- predict(M2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# Обратная трансформация предиктора
NewData$pr <- exp(NewData$pr_l)

# График модели после обратной трансформации
ggplot(NewData, aes(x = pr, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_point(data = prog, aes(x = pr, y = vent))

# Трансформация не помогла, возможно, эти данные стоит
# анализировать при помощи Gamma GLM, т.к. гамма
# распределение сможет смоделировать гетерогенность
# дисперсий.


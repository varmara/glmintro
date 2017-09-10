#### Упражнение по проверке условий применимости линейной регрессии ####
#
# Уровень прогестерона и выносливость
#
# Данные: Исследование влияния фазы менструального цикла на уровень потребления кислорода и вентиляцию легких у женщин на высоте >3600 м над уровнем моря (Brutsaert  et al. 2002; источник данных Whitlock, Schluter 2009)
#
# Brutsaert, T.D., Spielvogel, H., Caceres, E., Araoz, M., Chatterton, R.T. and Vitzthum, V.J., 2002. Effect of menstrual cycle phase on exercise performance of high-altitude native women at 3600 m. Journal of experimental biology, 205(2), pp.233-239.
#
# В исследовании приняли участие 30 женщин. Эти данные - результат измерений во время лютеиновой фазы цикла.
#
# Переменные:
# - progesterone - уровень прогестерона в слюне (пг/мл)
# - ventilation - вентиляция легких (мл/мин)


#### Загрузка пакетов из библиотеки ####

library(readxl)
library(ggplot2)

#### Знакомство с данными ####

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

# Попробуем логарифмировать предиктор
ggplot(prog, aes(x = log(pr), y = vent)) +
  geom_point() +
  geom_smooth(method = "lm")
# может стать немного лучше

# В целях обучения попробуем сделать несколько вариантов анализа:
# - продолжим без трансформации и изучим остатки
# - трансформируем данные.

#### ВАРИАНТ БЕЗ ТРАНСФОРМАЦИИ ############
#### Подбираем модель ####

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

#### Проверяем условия применимости ####

# Расстояние Кука
cook_cutoff <- 4 / (nrow(prog) - length(coef(M1) - 2))
plot(M1, which = 4, cook.levels = cook_cutoff)
# OK

# График остатков
residualPlot(M1)
# Вот теперь точно видно гетерогенность дисперсий

# Квантильный график остатков
set.seed(293234)
qqPlot(M1)
# OK

# Итог: на графике остатков видно, что связь нелинейна,
# необходима трансформация или применение нелинейной
# регрессии

#### ВАРИАНТ C ТРАНСФОРМАЦИЕЙ ############

# Трансформируем предиктор
prog$pr_tr <- (prog$pr)^2

M2 <- lm(vent ~ pr_tr, data = prog)
summary(M2)

#### Проверяем условия применимости ####

## Расстояние Кука
cook_cutoff <- 4 / (nrow(prog) - length(coef(M2) - 2))
plot(M2, which = 4, cook.levels = cook_cutoff)
# OK

## График остатков
residualPlot(M2)
# трансформация чуть помогла

## Квантильный график остатков
set.seed(293234)
qqPlot(M2)
# OK

## Графики остатков от предикторов в модели и нет
M2_diag <- data.frame(prog,
                      .resid = resid(M2, type = "pearson"))
gg_res <- ggplot(M2_diag, aes(x = pr_tr, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point()
gg_res

#### Описываем результаты ####
summary(M2)

# Call:
#   lm(formula = vent ~ pr_tr, data = prog)
#
# Residuals:
#   Min     1Q Median     3Q    Max
# -20.13  -7.96  -1.04   4.76  34.18
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) 64.090239   3.084832   20.78   <2e-16 ***
#   pr_tr        0.001071   0.000363    2.95   0.0063 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 12.8 on 28 degrees of freedom
# Multiple R-squared:  0.237,	Adjusted R-squared:  0.21
# F-statistic: 8.72 on 1 and 28 DF,  p-value: 0.00632

# Вентиляция достоверно зависит от уровня прогестерона (t-тест, p < 0.01 )

## Уравнение модели
# vent = 64.09 + 0.0001 pr_tr

## Доля объясненной изменчивости
# Adjusted R-squared:  0.21

#### График модели  ####

# Обычный график нам не подходит - там предиктор  трансформирован
ggplot(prog, aes(x = pr_tr, y = vent)) +
  geom_point() +
  geom_smooth(method = "lm")

# Построим график с обратной трансформацией данных

# Данные для графика
NewData <- data.frame(
  pr_tr = seq(min(prog$pr_tr), max(prog$pr_tr), length = 100))

# предсказанные значения
Predictions <- predict(M2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# Обратная трансформация предиктора
NewData$pr <- sqrt(NewData$pr_tr)

# График модели после обратной трансформации
ggplot(NewData, aes(x = pr, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_point(data = prog, aes(x = pr, y = vent))

# Трансформация не помогла, возможно, эти данные стоит
# анализировать при помощи Gamma GLM, т.к. гамма
# распределение сможет смоделировать гетерогенность
# дисперсий.

#### ####

#### Упражнение по проверке условий применимости линейной регрессии ####
#
# Транспорт кальция через плазматическую мембрану
#
# Данные Говарда Граймса (Howard Grimes) с каф. Ботаники
# Университета Северной Каролины (источник данных Rawlings
# 1988).
#
# (растительные?) клетки погружали в раствор радиоактивного
# кальция на разное время, затем измеряли количество
# поглощенного кальция. Всего было 9 вариантов экспозиции,
# каждая выполнена в 3 повторностях.
#
# Переменные:
# - time - время экспозиции в минутах
# - cal - количество поглощенного кальция (нмоль/мг)

#### Загрузка пакетов из библиотеки ####

library(readxl)
library(ggplot2)

#### Знакомство с данными ####

# Не забудьте установить рабочую директорию или
# отредактируйте путь к файлу данных

# Открываем данные
calc <- read_excel("data/calcium.xlsx", sheet = 1)
head(calc)

str(calc)

# Есть ли пропущенные значения?
colSums(is.na(calc))

# Сколько было разных экспозиций, и сколько было повторностей?
length(unique(calc$time))
table(calc$time)

# Есть ли выбросы? Построим дот-плот
gg_dot <- ggplot(calc, aes(y = 1:nrow(calc))) + geom_point()
gg_dot + aes(x = cal)
gg_dot + aes(x = time)
# Выбросов нет

# График зависимости
ggplot(calc, aes(x = time, y = cal)) +
  geom_point()

# Добавим линию регрессии
ggplot(calc, aes(x = time, y = cal)) +
  geom_point() +
  geom_smooth(method = "lm")

# Выглядит не очень линейно. Что делать?
# Нелинейная регрессия? - не в этом курсе
# Трансформация? - попробуем

# Попробуем трансформировать переменные
ggplot(calc, aes(x = log(time), y = cal)) +
  geom_point() +
  geom_smooth(method = "lm")
# Нелинейность исчезает
ggplot(calc, aes(x = time, y = (cal)^2)) +
  geom_point() +
  geom_smooth(method = "lm")
# Нелинейность исчезает, но появляется гетерогенность дисперсий

# В целях обучения попробуем сделать несколько вариантов анализа:
# - продолжим без трансформации и изучим остатки
# - трансформируем данные.

#### ВАРИАНТ БЕЗ ТРАНСФОРМАЦИИ ############
#### Подбираем модель ####

M1 <- lm(cal ~ time, data = calc)
summary(M1)
# регрессия объясняет довольно много изменчивости
# Adjusted R-squared:  0.741

# Время достоверно влияет на поглощение кальция
# Coefficients:
  #         Estimate Std. Error t value     Pr(>|t|)
  # time        0.2427   0.0279    8.69 0.0000000051 ***

# Но есть ли во всем этом смысл???

#### Проверяем условия применимости ####

# Расстояние Кука
cook_cutoff <- 4 / (nrow(calc) - length(coef(M1) - 2))
plot(M1, which = 4, cook.levels = cook_cutoff)
# OK

# График остатков
residualPlot(M1)
# Вот теперь точно видно, что связь нелинейная!

# Квантильный график остатков
set.seed(293234)
qqPlot(M1)
# OK

# Итог: на графике остатков видно, что связь нелинейна,
# необходима трансформация или применение нелинейной
# регрессии

#### ВАРИАНТ C ТРАНСФОРМАЦИЕЙ ############
#### Подбираем модель ####
calc$time_l <- log(calc$time)

M2 <- lm(cal ~ time_l, data = calc)
summary(M2)

#### Проверяем условия применимости ####

## Расстояние Кука
cook_cutoff <- 4 / (nrow(calc) - length(coef(M2) - 2))
plot(M2, which = 4, cook.levels = cook_cutoff)
# OK

## График остатков
residualPlot(M2)
# Немного лучше, но все равно заметна легкая нелинейность

## Квантильный график остатков
set.seed(293234)
qqPlot(M2)
# OK

## Графики остатков от предикторов в модели и нет
M2_diag <- data.frame(calc,
                      .resid = resid(M2, type = "pearson"))
gg_res <- ggplot(M2_diag, aes(x = time_l, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point()
gg_res

#### Описываем результаты ####
summary(M2)

# Call:
#   lm(formula = cal ~ time_l, data = calc)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.98392 -0.40448 -0.00531  0.40041  1.18014
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.9739     0.1875   5.194 2.26e-05 ***
#   time_l        1.1583     0.1014  11.428 2.03e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.5849 on 25 degrees of freedom
# Multiple R-squared:  0.8393,	Adjusted R-squared:  0.8329
# F-statistic: 130.6 on 1 and 25 DF,  p-value: 2.033e-11

# Поглощение кальция достоверно зависит от времени
# экспозиции (t-тест, p < 0.001 )

## Уравнение модели
# cal = 0.97 + 1.16time_l

## Доля объясненной изменчивости
# Adjusted R-squared:  0.833

#### График модели  ####

# Обычный график нам не подходит - там логарифм времени
ggplot(calc, aes(x = time_l, y = cal)) +
  geom_point() +
  geom_smooth(method = "lm")

# Построим график с обратной трансформацией данных

# Данные для графика
NewData <- data.frame(
  time_l = seq(min(calc$time_l), max(calc$time_l), length = 100))

# предсказанные значения
Predictions <- predict(M2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# Обратная трансформация предиктора
NewData$time <- exp(NewData$time_l)

# График модели после обратной трансформации
ggplot(NewData, aes(x = time, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_point(data = calc, aes(x = time, y = cal))

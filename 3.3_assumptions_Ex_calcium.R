#### Упражнение по проверке условий применимости линейной регрессии ####
#
# Транспорт кальция через плазматическую мембрану
#
# Данные Говарда Граймса (Howard Grimes) с каф. Ботаники Университета Северной Каролины (источник данных Rawlings 1988).
#
# (растительные?) клетки погружали в раствор радиоактивного кальция на разное время, затем измеряли количество поглощенного кальция. Всего было 9 вариантов экспозиции, каждая выполнена в 3 повторностях.
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
ggplot(calc, aes(x = cal, y = 1:nrow(calc))) +
  geom_point()
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

# Попробуем логарифмировать временную шкалу
ggplot(calc, aes(x = log(time), y = cal)) +
  geom_point() +
  geom_smooth(method = "lm")
# Попробуем продолжить без трансформации и изучим остатки

#### ВАРИАНТ БЕЗ ТРАНСФОРМАЦИИ ############
#### Подбираем модель ####

M1 <- lm(cal ~ time, data = calc)
summary(M1)
# 1) регрессия объясняет довольно много изменчивости
# Adjusted R-squared:  0.741

# 2) Время достоверно влияет на поглощение кальция
# Coefficients:
  #         Estimate Std. Error t value     Pr(>|t|)
  # time        0.2427   0.0279    8.69 0.0000000051 ***

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

#### ВАРИАНТ C ТРАНСФОРМАЦИЕЙ ############
#### Подбираем модель ####
calc$time_l <- log(calc$time)

M2 <- lm(cal ~ time_l, data = calc)
summary(M2)
# 1) регрессия объясняет еще больше изменчивости
# Adjusted R-squared:  0.833

# 2) Время достоверно влияет на поглощение кальция
# Coefficients:
#         Estimate Std. Error t value     Pr(>|t|)
# log(time)      1.158      0.101   11.43 0.00000000002 ***

# Интерпретация коэффициентов изменилась

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
# Нет таких

## Описываем модель
summary(M2)

## Уравнение модели

## Доля объясненной изменчивости


# График модели
# После трансформации простой график нам перестал подходить

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



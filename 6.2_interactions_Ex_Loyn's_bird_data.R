# Взаимодействие дискретных и непрерывных факторов######
# Птицы в лесах австралии

# (Пример взят из книги Quinn&Keugh,2002; Оригинальная работа Loyn, 1987)
#
# Фрагментация лесных местообитаний - одна из важнейших проблем Австралии.
# Вопрос: от каких факторов зависит обилие птиц во фрагментированных лесных массивах?
#
# *Зависимая перменная*
# `ABUND` - Обилие птиц на стандартном маршруте
#
# *Предикторы*
# `AREA` - площадь лесного массива (Га)
# `YRISOL` - год, в котором произошла изоляция лесного массива
# `DIST` - расстояние до ближайшего другого лесного массива (км)
# `LDIST` - расстояние до ближайшего более крупного массива (км)
# `GRAZE` - качественная оценка уровня выпаса скота (1 - низкий уровень, 5 - высокий урвень)
# `ALT` - высота над уровнем моря (м)
#

# Загружаем пакеты из библиотеки ##################

library(car)
library(ggplot2)

# ## Читаем данные ################################
bird <- read.csv("data/loyn.csv")
head(bird)


# ## Знакомство с данными #########################

# Выбросы
gg_dot <- ggplot(bird, aes(y = 1:nrow(bird))) + geom_point()
gg_dot + aes(x = ABUND)
gg_dot + aes(x = AREA)
# Есть два леса огромной площади -- кандидаты на удаление
# Можно логарифмировать
gg_dot + aes(x = YRISOL)
gg_dot + aes(x = DIST)
# Один лес расположен очень далеко от других
gg_dot + aes(x = LDIST)
gg_dot + aes(x = GRAZE)
# На самом деле это дискретная переменная.
# Ее нужно сделать фактором
gg_dot + aes(x = ALT)
# Один высокогорный лес

# Связи между переменными
scatterplotMatrix(bird)

# Явные проблемы
# Есть сильные корреляции между некоторым
# предикторами. Мы должны будем сделать проверку
# на мультиколлинеарность.

# Итог: Удалим отскакивающие наблюдения, сделаем
# фактором дискретную переменную и логарифмируем площадь

# Делаем фактором дискретную переменную
bird$GRAZE_factor <- factor(bird$GRAZE)

# Удалим влиятельные наблюдения Два леса огромной
# площади Один лес расположен очень далеко Один
# высокогорный лес Записываем в логические векторы
# все, что хотим оставить, ориентируясь по
# дот-плотам
include <- bird$AREA < 500 & bird$DIST < 1000 & bird$ALT < 250
bird_1 <- bird[include, ]

# Логарифмируем площадь
bird_1$AREA_l <- log(bird_1$AREA)

# Дальше работаем с bird_1

scatterplotMatrix(bird_1)

#### Анализ с учетом взаимодействия факторов, и с подбором оптимальной модели ########

#### Построим линейную модель ##############################

model <- lm(ABUND ~  AREA_l + YRISOL + DIST + LDIST + GRAZE_factor + ALT, data = bird_1)

# # Проверка условий применимости ##########################

# 0) ПРОВЕРКА НА МУЛЬТИКОЛЛИНЕАРНОСТЬ
vif(model)
# Возможно GRAZE_factor - лишний предиктор. Но, допустим,
# нас на самом деле гораздо больше интересует, как
# обилие птиц зависит от уровня выпаса скота, чем
# от года изоляции леса. Поэтому мы вместо GRAZE_factor
# удалим YRISOL

model2 <- update(model, ~ . -YRISOL)
vif(model2)

summary(model2)
# Мультиколлинеарности нет


model2 <- lm(ABUND ~ AREA_l + DIST + LDIST + GRAZE_factor + ALT, data = bird_1)
# Теперь работаем с моделью 2

summary(model2)

# Данные для графиков остатков
ABUND_diag <- fortify(model2)
# Удаляли YRISOL, нужно его вернуть
ABUND_diag_full <- data.frame(ABUND_diag,
                              YRISOL = bird_1$YRISOL)

# 1) График расстояния Кука
ggplot(ABUND_diag_full, aes(x = 1:nrow(ABUND_diag_full), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = ABUND_diag_full, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
#  ABUND ~ AREA_l + DIST + LDIST + GRAZE_factor + ALT, не в модели YRISOL
gg_resid + aes(x = AREA_l)
gg_resid + aes(x = YRISOL)
gg_resid + aes(x = DIST)
gg_resid + aes(x = LDIST)
gg_resid + aes(x = ALT)

# 4) Квантильный график остатков
qqPlot(model2)



summary(model2)
# Не все влияют, будем оптимизировать

#### Подбор оптимальной модели ###############

# Способ 1. Частный F-критерий
drop1(model2, test = "F")
# Удаляем DIST
model3 <- update(model2, . ~ . - DIST)
drop1(model3, test = "F")
# Удаляем LDIST
model4 <- update(model3, . ~ . - LDIST)
drop1(model4, test = "F")
# Удаляем ALT
model5 <- update(model4, . ~ . - ALT)
drop1(model5, test = "F")
# Больше ничего удалить нельзя

# финальная модель model5
# ABUND ~ AREA_l + GRAZE_factor

# То же самое можно сделать при помощи функции step()

model_step <- step(model2, direction = "backward")
# сравните, получились одинаковые модели
model5
model_step

#### Проверка условий применимости финальной модели ########

# Допустим, мы остановились на model5

# нужно протестировать, есть ли взаимодействие
model6 <- lm(ABUND ~ AREA_l + GRAZE_factor + AREA_l:GRAZE_factor, data = bird_1)
drop1(model6, test = "F")
# Удаление взаимодействия из модели ее достоверно ухудшает
# Т.е. наша финальная модель model6


# Данные для графиков остатков
model6_diag <- fortify(model6)
removed_vars <- c("YRISOL", "LDIST", "DIST", "ALT")
model6_diag_full <- data.frame(model6_diag, bird_1[, removed_vars])


# 1) График расстояния Кука
ggplot(model6_diag_full, aes(x = 1:nrow(model6_diag_full), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = model6_diag_full, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid
# ОК

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = AREA_l)
gg_resid + aes(x = YRISOL)
gg_resid + aes(x = DIST)
gg_resid + aes(x = LDIST)
gg_resid + aes(x = ALT)
# Эти графики уже хуже - видна гетерогенность дисперсий/

# 4) Квантильный график остатков
qqPlot(model6)

# Гетерогенность дисперсий не побороли
# Дисперсия предсказаний зависит от предиктора и от факторов не в модели. Из-за этого увеличится вероятность ошибки I рода в тестах. Чтобы это исправить, на самом деле нужно моделировать гетерогенность дисперсий (средствами GLM). Но сейчас мы продолжим дальше.

#### Описание результатов ##################################

summary(model6)
# Задание ------------------------------
# Посмотрите на summary нашей модели и запишите

# уравнение общее

# уравнения для разных уровней выпаса скота
# GRAZE_factor == 1

# GRAZE_factor == 2

# GRAZE_factor == 3

# GRAZE_factor == 4

# GRAZE_factor == 5
#---------------------------------------------

# За эффект GRAZE_factor у нас отвечает много коэффициентов
# И за эффект взаимодействия тоже - много коэффициентов, поэтому
# неудобно приводить в результатах
# таблицу коэффициентов как в summary.
# Лучше привести таблицу дисперсионного анализа
anova(model6)

# доля объясненной изменчивости


#### График модели  ########################################

# На графике нужно изобразить зависимость обилия птиц от площади леса для разных уровней выпаса скота. Всего будет 5 линий

library(plyr)
NewData <- ddply(
  .data = bird_1, .variables = .(GRAZE_factor), .fun = summarise,
  AREA_l = seq(min(AREA_l), max(AREA_l), length = 100))

# предсказанные значения
Predictions <- predict(model6, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# График без обратной трансформации
ggplot(NewData, aes(x = AREA_l, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = GRAZE_factor)) +
  geom_line(aes(colour = GRAZE_factor)) +
  geom_point(data = bird_1, aes(x = AREA_l, y = ABUND, colour = GRAZE_factor))

# Для интерпретации обязательно нужно сделать обратную трансформацию
NewData$AREA <- exp(NewData$AREA_l)
ggplot(NewData, aes(x = AREA, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = GRAZE_factor)) +
  geom_line(aes(colour = GRAZE_factor)) +
  geom_point(data = bird_1, aes(x = AREA, y = ABUND, colour = GRAZE_factor))

# Какой из этого вывод?
# Обилие птиц слабо зависит от площади леса, если там мало пасут скот (1-3), или слишком много пасут (5). Но зато оно сильно зависит от площади в лесах, где сильно пасут скот (4).
# Кроме того, на графике становится видно, что лесов большой площади мало, и это леса, где мало пасут скот (GRAZE_factor 1 или 2). А лесов малой площади много, и во многих из них сильно пасут скот.


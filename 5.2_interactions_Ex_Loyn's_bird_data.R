# Множественная регрессия. Упражнение 2.

# # Какие факторы определяют обилие птиц во фрагментированных лесных массивах Австралии?

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

#### Анализ с учетом взаимодействия факторов, и с подбором оптимальной модели ########

#### Построим линейную модель ##############################

model <- lm(ABUND ~  AREA + YRISOL + DIST + LDIST + GRAZE_factor + ALT, data = bird_1)

# # Проверка условий применимости ##########################

# 0) ПРОВЕРКА НА МУЛЬТИКОЛЛИНЕАРНОСТЬ
vif(model)
# Возможно GRAZE - лишний предиктор. Но, допустим,
# нас на самом деле гораздо больше интересует, как
# обилие птиц зависит от уровня выпаса скота, чем
# от года изоляции леса. Поэтому мы вместо GRAZE
# удалим YRISOL

model2 <- update(model, ~ . -YRISOL)
vif(model2)

model3 <- update(model2, ~ . -AREA)
vif(model3)

summary(model3)
# Мультиколлинеарности нет

# Теперь работаем с моделью 3

summary(model3)

# Данные для графиков остатков
ABUND_diag <- fortify(model3)
removed_vars <- c("YRISOL", "AREA")
ABUND_diag_full <- data.frame(ABUND_diag, bird_1[, removed_vars])


# 1) График расстояния Кука
ggplot(ABUND_diag_full, aes(x = 1:nrow(ABUND_diag_full), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = ABUND_diag_full, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
# В модели: AREA + YRISOL + DIST + LDIST + GRAZE + ALT
gg_resid + aes(x = AREA)
# Величина остатков зависит от площади леса. Возможно, придется вернуть в модель
gg_resid + aes(x = YRISOL)
# Величина остатков зависит от года изоляции. Гетерогенность дисперсий
gg_resid + aes(x = DIST)
gg_resid + aes(x = LDIST)
# Гетерогенность дисперсий
gg_resid + aes(x = ALT)

# 4) Квантильный график остатков
qqPlot(model2)


model3 <- lm(ABUND ~ DIST + LDIST + AREA*GRAZE_factor + ALT, data = bird_1)
summary(model3)


#### Подбор оптимальной модели ###############

# Способ 1. Частный F-критерий
drop1(model3, test = "F")
# Удаляем LDIST
model4 <- update(model3, . ~ . - LDIST)
drop1(model4, test = "F")
# Удаляем DIST
model5 <- update(model4, . ~ . - DIST)
drop1(model5, test = "F")
# Удаляем ALT
model6 <- update(model5, . ~ . - ALT)
drop1(model6, test = "F")
# Больше ничего удалить нельзя

# финальная модель model6
# ABUND ~ AREA + GRAZE_factor + AREA:GRAZE_factor

# Способ 2. Тесты соотношения правдоподобий

GLM3 <- glm(ABUND ~ DIST + LDIST + AREA*GRAZE_factor + ALT, data = bird_1)

drop1(GLM3, test = "Chi")
# Удаляем LDIST
GLM4 <- update(GLM3, . ~ . - LDIST)
drop1(GLM4, test = "Chi")
# Удаляем DIST
GLM5 <- update(GLM4, . ~ . - DIST)
drop1(GLM5, test = "Chi")
# Удаляем ALT
GLM6 <- update(GLM5, . ~ . - ALT)
drop1(GLM6, test = "Chi")
# Больше ничего удалить нельзя

# финальная модель GLM6
# ABUND ~ AREA + GRAZE_factor + AREA:GRAZE_factor

# Способ 3. Информационные критерии (Один из)
AIC(GLM3, GLM4, GLM5, GLM6)

# Финальная модель GLM6
# ABUND ~ AREA + GRAZE_factor + AREA:GRAZE_factor

BIC(GLM3, GLM4, GLM5, GLM6)
# финальная модель GLM6
# ABUND ~ AREA + GRAZE_factor + AREA:GRAZE_factor

#### Проверка условий применимости финальной модели ########

# Допустим, мы остановились на model6
# Данные для графиков остатков
model6_diag <- fortify(model6)
removed_before <- c("YRISOL")
removed_now <- c("LDIST", "DIST", "ALT")
removed_vars <- c(removed_before, removed_now)
model6_diag_full <- data.frame(model6_diag, bird_1[, removed_vars])


# 1) График расстояния Кука
ggplot(model6_diag_full, aes(x = 1:nrow(model6_diag_full), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = model6_diag_full, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = AREA)
# Разброс зависит от площади
gg_resid + aes(x = YRISOL)
# от года изоляции
gg_resid + aes(x = DIST)
# от расстояния до ближайшено леса
gg_resid + aes(x = LDIST)
# и от расстояния до ближайшего большого леса
gg_resid + aes(x = ALT)

# 4) Квантильный график остатков
qqPlot(model6)

# Итог не утешительный...
# Пока что продолжим с этой оптимизированной моделью

#### Описание результатов ##################################

summary(model6)

# уравнение общее

# уравнения для разных уровней выпаса скота

# значимость эффектов
anova(model6)

# доля объясненной изменчивости


#### График модели  ########################################

# На графике нужно изобразить зависимость обилия птиц от площади леса для разных уровней выпаса скота. Всего будет 5 линий


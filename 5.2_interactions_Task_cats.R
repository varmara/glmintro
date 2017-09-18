## Упражнение. Модели с дискретными и непрерывными предикторами и их взаимодействием #######

# Как зависит масса кошачьего сердца от веса тела и пола животного

# Источник данных
# R. A. Fisher (1947) The analysis of covariance
# method for the relation between a part and the
# whole, Biometrics 3, 65–68.

# В таблице три переменных:
# Sex - пол
# Bwt - body weight, вес тела, кг
# Hwt - heart weight, вес сердца, г

## Задание

# Исследуйте данные о кошках чтобы выяснить, как
# вес сердца зависит от
# - веса тела
# - от пола
# - от взаимодействия пола и веса тела

# Проверьте условия применимости этой модели
# Упростите модель, если это возможно
# Напишите общее уравнение и отдельные уравнения модели для котов и кошек
# Постройте график предсказаний модели


# Загружаем пакеты из библиотеки ##################

library(readxl)
library(car)
library(ggplot2)

# ## Читаем данные ################################
cat <- read_excel("data/cats.xlsx", sheet = 1)
head(cat)


# ## Знакомство с данными #########################

str(cat)
colSums(is.na(cat))
table(cat$Sex)

# Выбросы
ggplot(cat, aes(x = Hwt, y = 1:nrow(cat))) + geom_point()
ggplot(cat, aes(x = Bwt, y = 1:nrow(cat))) + geom_point()

#### ВАРИАНТ 1. Неправильный - без взаимодействия факторов ########

#### Постоим линейную модель ##############################

cat_mod_1 <- lm(Hwt ~ Bwt + Sex, data = cat)

# # Проверка условий применимости ##########################

# 0) ПРОВЕРКА НА МУЛЬТИКОЛЛИНЕАРНОСТЬ
vif(cat_mod_1)
# коллинеарности нет

# Данные для графиков остатков
cat_mod_1_diag <- fortify(cat_mod_1)

# 1) График расстояния Кука
ggplot(cat_mod_1_diag, aes(x = 1:nrow(cat_mod_1_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = cat_mod_1_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid
# Выглядит хорошо

# 3) Графики остатков от предикторов в модели и не в модели
# В модели: Bwt Sex
gg_resid + aes(x = Bwt)
gg_resid + aes(x = Sex)
# а лучше так:
ggplot(cat_mod_1_diag, aes(x = Sex, y = .stdresid)) +
  geom_boxplot()
# Выглядит хорошо

# 4) Квантильный график остатков
qqPlot(cat_mod_1)
# Отклонения от нормального распределения

#### Подбор оптимальной модели #############################

# В cat_mod_1 влияние пола на вес сердца
# недостоверно, может быть, его убрать? Подбираем
# модель без этого фактора, затем сравниваем их
# (вложенные модели) при помощи частного
# F-критерия (Вместо частного F-критерия, в
# принципе, еще можно использовать тесты отношения
# правдоподобий или информационные критерии)

drop1(cat_mod_1, test = "F")

# Фактор Sex можно удалить из модели

cat_mod_2 <- update(cat_mod_1, . ~ . - Sex)
drop1(cat_mod_2, test = "F")
# Больше ничего удалить нельзя

#### Описываем результаты ##################################

summary(cat_mod_2)

# F-statistic: 259.8 on 1 and 142 DF, p-value: < 0.01

## Уравнение модели
# Hwt = - 0.36 + 4.03Bwt

# Доля объясненной изменчивости
# 64%

#### График модели  ########################################

NewData <- data.frame(Bwt = seq(min(cat$Bwt), max(cat$Bwt), length.out = 100))

# предсказанные значения
Predictions <- predict(cat_mod_2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE


# График
ggplot(NewData, aes(x = Bwt, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_point(data = cat, aes(x = Bwt, y = Hwt, colour = Sex))

#### ВАРИАНТ 2. Со взаимодействием факторов ########


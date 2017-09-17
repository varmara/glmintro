## Множественная линейная регрессия --- подбор моделей ######
#
# Эти данные --- результаты ретроспективного
# исследования факторов, связанных с риском
# развития сердечно сосудистых заболеваний у
# мужчин. Исследование проведено в регионе с
# высокими рисками --- Вестерн Кейп, Южная Африка
#
# Выборка в этом файле неоднородна: у части мужчин
# было диагностировано заболевание коронарных
# сосудов. Многие из таких мужчин проходили
# терапию по снижению артериального давления уже
# после постановки диагноза. И в некоторых случаях
# измерения были сделаны уже после применения
# терапии.
#
# Эти данные взяты из большого датасета из работы
# Rousseauw et al, 1983 (опубликована в South
# African Medical Journal
#
# Rousseauw, J., du Plessis, J., Benade, A.,
# Jordaan, P., Kotze, J. and Ferreira, J. (1983).
# Coronary risk factor screening in three rural
# communities, South African Medical Journal 64:
# 430–436.

# Переменные:
# sbp --- systolic blood pressure#
# tobacco --- cumulative tobacco (kg)
# ldl --- low density lipoprotein cholesterol
# adiposity --- a numeric vector
# famhist ---  family history of heart disease, a factor with levels Absent Present
# typea --- type-A behavior
# obesity --- a numeric vector
# alcohol --- current alcohol consumption
# age --- age at onset
# chd --- coronary heart disease


# Задача: -------------------------------------------------

# Давайте проанализируем, от каких факторов
# зависит артериальное давление (sbp). Возьмите
# данные только о здоровых мужчинах (у которых не
# было заболевания коронарных сосудов).
# Проанализируйте эти данные при помощи
# множественной линейной регрессии. Не используйте
# для построения регрессии в качестве предикторов
# дискретную переменную famhist (мы еще не научили
# вас с ними работать). Но обязательно обратите
# внимание на нее во время анализа остатков.
# Наконец, попробуйте оптимизировать полную модель
# одним из способов:
# - при помощи частного F-критерия
# - при помощи тестов соотношения правдоподобий
# - при помощи AIC


# Загружаем пакеты из библиотеки ##################
library(readxl)
library(car)
library(ggplot2)

# ## Читаем данные ################################

SAheart <- read_excel("data/SAheart.xlsx")
# ## Фильтруем данные по условиям задачи ##########

# По условиям задачи нам нужно взять мужчин, у которых
# переменная chd не равна нулю, т.е. нет болезни
# коронарных артерий
filt <- SAheart$chd != 0
pressure <- SAheart[filt, -c(9, 10)]

# ## Знакомство с данными #########################
# как называются наши переменные?
colnames(pressure)

# Есть ли пропущенные значения
colSums(is.na(pressure))

# у скольких человек в семье были сердечно-сосудистые заболевания?
table(pressure$famhist)

# Связи между переменными
scatterplotMatrix(pressure[, -5])

# Связи выглядят линейными, но зато стоит логарифмировать tobacco, ldl, alcohol, obesity
pressure$tobacco_l <- log(pressure$tobacco + 1)
pressure$ldl_l <- log(pressure$ldl + 1)
pressure$obesity_l <- log(pressure$obesity + 1)
pressure$alcohol_l <- log(pressure$alcohol + 1)


# Выбросы
gg_dot <- ggplot(pressure, aes(y = 1:nrow(pressure))) + geom_point()
gg_dot + aes(x = sbp)
gg_dot + aes(x = tobacco_l)
# Есть три курильщика, которые слегка отличаются от всех
gg_dot + aes(x = ldl_l)
gg_dot + aes(x = adiposity)
gg_dot + aes(x = famhist)
# семейная история сердечных заболеваний --- это дискретная переменная
gg_dot + aes(x = typea)
# один курильщик редко использует поведение A
gg_dot + aes(x = obesity_l)
# Есть два-три человкеа, коториые отличаются от других
gg_dot + aes(x = alcohol_l)
# Есть несколько алкоголиков


# Итог: Выборка неоднородна, есть люди, которые сильно отличаются от других по значениям какого-либо признака. Возможно, некоторые из этих наблюдений могут оказать влияние на ход регрессии. Нужно подумать, не нужно ли их исключить из данных.
# Но пока что мы продолжим работать с полным массивом данных

#### ВАРИАНТ 1. Анализ  ########

#### Постоим линейную модель ##############################
# Строим модель без учета famhist (по условиям задачи)
# В предыдущем анализе мы начинали с полной модели

mod1 <- lm(sbp ~ tobacco_l + ldl_l + adiposity + typea + obesity_l + alcohol_l, data = pressure)

summary(mod1)

# # Проверка условий применимости ##########################

# 0) ПРОВЕРКА НА МУЛЬТИКОЛЛИНЕАРНОСТЬ

vif(mod1)
# Возможно adiposity  - лишний предиктор

mod2 <- update(mod1, ~ . -adiposity )
vif(mod2)
# Мультиколлинеарности нет

# Теперь работаем с моделью 2

# Данные для графиков остатков
pressure_diag <- fortify(mod2)

# 1) График расстояния Кука
ggplot(pressure_diag, aes(x = 1:nrow(pressure_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = pressure_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid
# Гетерогенность дисперсий не видна

# 3) Графики остатков от предикторов в модели и не в модели
# В модели: tobacco_l + ldl_l + typea + obesity + alcohol_l
# Вне модели: famhist

gg_resid + aes(x = tobacco_l)
gg_resid + aes(x = ldl_l)
gg_resid + aes(x = typea)
gg_resid + aes(x = obesity_l)
# Похоже, что величина остатков зависит от степени ожирения? Или может быть результаты сильно зависят от нескольких значений (один тощий и два толстых)
gg_resid + aes(x = alcohol_l)
gg_resid + aes(x = pressure$famhist)

# 4) Квантильный график остатков
qqPlot(mod2)
# Есть отклонения от нормального распределения

# Итог: сильной гетерогенности дисперсий нет, есть отклонения от нормального распределения. Величина остатков зависит от ожирения (возможно, определяется несколькими выбросами).

#### Описываем результаты? ##################################

summary(mod2)

## Уравнение модели
# Y = 80.4 + 1.2*tabacco_l + 3*ldl_l - 0.2*typea + 18.5*obesity_l + 2.5*alcohol_l + e

# Доля объясненной изменчивости
# 2.4%


#### Подбор оптимальной модели
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared

#Вторая модель стала хуже, но вернуться к первой мы не можем

#### ВАРИАНТ 2. Анализ, без взаимодействия факторов с подбором оптимальной модели ###############

# Способ 1. Частный F-критерий
drop1(mod2, test = "F")
# Удаляем ldl_l
mod3 <- update(mod2, . ~ . - ldl_l)
drop1(mod3, test = "F")
# Удаляем tobacco_l
mod4 <- update(mod3, . ~ . - tobacco_l)
drop1(mod4, test = "F")
# Удаляем typea
mod5 <- update(mod4, . ~ . - typea)
drop1(mod5, test = "F")
# Удаляем obesity_l
mod6 <- update(mod5, . ~ . - obesity_l)
# Больше ничего удалить нельзя, остался последний значимый предиктор

# финальная модель mod6
# sbp ~ alcohol_l

# Можем посмотреть на эту модель:
# summary(mod6)


# Способ 2. Тесты соотношения правдоподобий
GLM2 <- glm(sbp ~ ldl_l + tobacco_l + typea + obesity_l + alcohol_l, data = pressure)

drop1(GLM2, test = "Chi")
# Опять удаляем ldl_l
GLM3 <- update(GLM2, . ~ . - ldl_l)
drop1(GLM3, test = "Chi")
# Удаляем tobacco_l
GLM4 <- update(GLM3, . ~ . - tobacco_l)
drop1(GLM4, test = "Chi")
# Удаляем typea
GLM5 <- update(GLM4, . ~ . - typea)
drop1(GLM5, test = "Chi")
# Удаляем obesity_l
GLM6 <- update(GLM5, . ~ . - obesity_l)

# Больше ничего удалить нельзя

# финальная модель GLM6
# spb ~ alcohol_l

# Способ 3. Информационные критерии (Один из)
AIC(GLM2, GLM3, GLM4, GLM5, GLM6)

# Финальная модель GLM5
# spb ~ alcohol_l + obesity_l

BIC(GLM2, GLM3, GLM4, GLM5, GLM6)
# финальная модель GLM6
# spb ~ alcohol_l

#### Проверка условий применимости финальной модели ########

# Допустим, мы остановились на GLM6

summary(GLM6)

# Данные для графиков остатков
GLM6_diag <- fortify(GLM6)
removed_before <- c("adiposity")
removed_now <- c("ldl_l", "tobacco_l", "typea", "obesity_l")
removed_vars <- c(removed_before, removed_now)
GLM6_diag_full <- data.frame(GLM6_diag, pressure[, removed_vars])


# 1) График расстояния Кука
ggplot(GLM6_diag_full, aes(x = 1:nrow(GLM6_diag_full), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = GLM6_diag_full, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
# В модели: 
gg_resid + aes(x = adiposity)
gg_resid + aes(x = ldl_l)
gg_resid + aes(x = tobacco_l)
gg_resid + aes(x = typea)
gg_resid + aes(x = obesity_l)

gg_resid + aes(x = alcohol_l)

gg_resid + aes(x = pressure$famhist)

# 4) Квантильный график остатков
# нельзя построить для модели GLM6. используем mod6, идентиную по составу предикторов

qqPlot(mod6)



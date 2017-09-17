#### Упражнение по подбору линейной модели со взаимодействием дискретного и непрерывного предикторов #########

# Данные взяты из работы
# Tager, I. B., Weiss, S. T., Rosner, B., and Speizer, F. E. (1979). Effect of parental cigarette smoking on pulmonary function in children. American Journal of Epidemiology, 110, 15-26.
# Rosner, B. (1990). Fundamentals of Biostatistics, 3rd Edition. PWS-Kent, Boston, Massachusetts.
# Источник данных: http://www.statsci.org/data/general/fev.html

# Структура данных
# Age 	 -  	Возраст
# FEV 	 -  	Объем легких при выдохе (литры) (forced expiratory volume)
# Height 	 -  	Рост (дюймы)
# Sex 	 -  	 пол (Male or Female)
# Smoker 	 -  	некурящие (Non), курящие (Current)

# Загружаем пакеты из библиотеки ##################

library(readxl)
library(car)
library(ggplot2)

# ## Читаем данные ################################
fev <- read_excel("data/fev.xls", sheet = "tidy_data", col_names = TRUE, na = "NA", skip = 1 )
head(fev)


# ## Знакомство с данными #########################

str(fev)
# Есть две дискретные переменные

# Сколько людей в группах?
table(fev$Sex, fev$Smoker)

# К этому моменту мы с вами еще не научились
# работать с ситуациями, когда есть несколько
# дискретных факторов, поэтому давайте посмотрим
# только на часть наблюдений. Отберем только
# мальчиков, курящих и некурящих.

# Отфильтровываем данные
fltr <- fev$Sex == "Male"
fev_male <- fev[fltr, ]

# Есть ли пропущенные значения?
colSums(is.na(fev_male))
# Есть пропущенные значения

# Отбираем только строки без пропущенных значений
fltr_compl <- complete.cases(fev_male)
fev_male1 <- fev_male[fltr_compl, ]

# Дальше работаем только с fev_male1

# Сколько осталось наблюдений
nrow(fev_male1)

table(fev_male1$Sex, fev_male1$Smoker)

# Связи между переменными
scatterplotMatrix(fev_male1[, c("Age", "FEV", "Height")])

# Трансформируем данные
# Распределение объема легких ассимитрично (длинный правый хвост), поэтому логарифмируем FEV
fev_male1$FEV_l <- log(fev_male1$FEV)
# Объем определяется линейными размерами в кубе, поэтому возведем рост в куб
fev_male1$Height_3 <- (fev_male1$Height)^3

# Проверяем себя
scatterplotMatrix(fev_male1[, c("Age", "FEV_l", "Height_3")])

# Выбросы
gg_dot <- ggplot(fev_male1, aes(y = 1:nrow(fev_male1))) + geom_point()
gg_dot + aes(x = Age)
gg_dot + aes(x = FEV_l)
gg_dot + aes(x = Height_3)
gg_dot + aes(x = Sex)
gg_dot + aes(x = Smoker)

#### ВАРИАНТ 1. Неправильный - без взаимодействия факторов ########

#### Постоим линейную модель ##############################

model <- lm(FEV_l ~ Age + Height_3 + Smoker, data = fev_male1)

# # Проверка условий применимости ##########################

# 0) ПРОВЕРКА НА МУЛЬТИКОЛЛИНЕАРНОСТЬ

vif(model)
# Мультиколлинеарность, убираем Age

model2 <- update(model, ~ . -Age)
vif(model2)
# Мультиколлинеарности нет

# Теперь работаем с моделью 2

# Данные для графиков остатков
model2_diag <- fortify(model2)
model2_diag$Age <- fev_male1$Age
# 1) График расстояния Кука
ggplot(model2_diag, aes(x = 1:nrow(model2_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = model2_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid
# Выглядит хорошо

# 3) Графики остатков от предикторов в модели и не в модели
# В модели: Height_3  Smoker
gg_resid + aes(x = Height_3)
gg_resid + aes(x = Smoker)
# а лучше так:
ggplot(model2_diag, aes(x = Smoker, y = .stdresid)) +
  geom_boxplot()
# НЕ в модели: Age
gg_resid + aes(x = Age)
# Выглядит хорошо

# 4) Квантильный график остатков
qqPlot(model2)

# Трансформации данных помогли, теперь мы можем пользоваться линейной моделью.

#### Описываем результаты ##################################

summary(model2)

# F-statistic: 774.2 on 2 and 332 DF,  p-value: < 0.01

## Уравнение модели
# FEV_l = - 0.1515106 + 0.0000045 Height_3 + 0.0079405 SmokerNon

# Уравнение для курильщиков
# FEV_l = - 0.1515106 + 0.0000045 Height_3

# Уравнение для некурящих
# FEV_l = -0.1515061 + 0.0000045 Height_3

# Доля объясненной изменчивости
# 82%

#### График модели  ########################################

library(plyr)
NewData <- ddply(
  .data = fev_male1, .variables = .(Smoker), .fun = summarise,
  Height_3 = seq(min(Height_3), max(Height_3), length = 100))

# предсказанные значения
Predictions <- predict(model2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# обратная трансформация предиктора
NewData$Height <- (NewData$Height_3)^(1/3)
# обратная трансформация отклика
NewData$FEV <- exp(NewData$fit)
NewData$upr_tr <- exp(NewData$upr)
NewData$lwr_tr <- exp(NewData$lwr)

# График без обратной трансформации
ggplot(NewData, aes(x = Height_3, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = Smoker)) +
  geom_line(aes(colour = Smoker)) +
  geom_point(data = fev_male1, aes(x = Height_3, y = FEV_l, colour = Smoker))

# График после обратной трансформации
ggplot(NewData, aes(x = Height, y = FEV)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr_tr, ymax = upr_tr, group = Smoker)) +
  geom_line(aes(colour = Smoker)) +
  geom_point(data = fev_male1, aes(x = Height, y = FEV, colour = Smoker))

# Видно, что линии предсказанных значений идут параллельно.

#### ВАРИАНТ 2. Со взаимодействием факторов ########

#### Постоим линейную модель со взаимодействием факторов #####

# Два варианта записи
# model3 <- lm(FEV_l ~ Height_3 * Smoker, data = fev_male1)
model3 <- lm(FEV_l ~ Height_3 + Smoker + Height_3:Smoker, data = fev_male1)

# # Проверка условий применимости ##########################

# 0) ПРОВЕРКА НА МУЛЬТИКОЛЛИНЕАРНОСТЬ
# Мы уже сделали проверку на мультиколлинеарность в предыдущем анализе.
# Проверка на мультиколинеарность делается по модели без взаимодействий
# Наша model3 со взаимодействием --- рискуете получить что-то странное
# vif(model3) # не делайте так!


# Данные для графиков остатков
model3_diag <- fortify(model3)

# 1) График расстояния Кука
# мы это тоже уже делали - нет необходимости дублировать
# ggplot(model3_diag, aes(x = 1:nrow(model3_diag), y = .cooksd)) +
# geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = model3_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
# В модели: Height_3 + Smoker + Height_3:Smoker
gg_resid + aes(x = Height_3)
gg_resid + aes(x = Smoker) + geom_boxplot()


# 4) Квантильный график остатков
qqPlot(model3)

#### Подбор оптимальной модели ############################

# В model3 взаимодействие недостоверно, может
# быть, его убрать? У нас уже подобрана model2 без
# взаимодействия model2 вложена в model3, поэтому
# для их сравнения можно использовать, например,
# частный F-критерий (Вместо частного F-критерия,
# в принципе, еще можно использовать тесты
# отношения правдоподобий или информационные
# критерии)

# Делаем либо так
anova(model3, model2)
# либо так
drop1(model3, test = "F")

# Обратите внимание, мы сначала пытаемся удалить
# взаимодействие, а составляющие его эффекты можно
# удалять, только если само взаимодействие
# удалили.

# Т.к. исключение взаимодействия из модели не
# привело к значимому ее ухудшению, то
# взаимодействие можно удалить

drop1(model2, test = "F")
# Влияние курения тоже можно удалить

model4 <- update(model2, .~. - Smoker)
drop1(model4, test = "F")
# Больше ничего удалить не можем


#### Описываем результаты ##################################

summary(model4)
# Связь между логарифмом объема легких и кубом
# роста значима (F = ..., df1 = ..., df2 = ..., p
# < 0.01)

## Уравнение модели
# FEV_l = - 0.1420991 + 0.0000045 Height_3

# Доля объясненной изменчивости
# 82%

#### График модели  ########################################

NewData <- data.frame(
  Height_3 = seq(min(fev_male1$Height_3), max(fev_male1$Height_3), length = 100))

# предсказанные значения
Predictions <- predict(model4, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# обратная трансформация предиктора
NewData$Height <- (NewData$Height_3)^(1/3)
# обратная трансформация отклика
NewData$FEV <- exp(NewData$fit)
NewData$upr_tr <- exp(NewData$upr)
NewData$lwr_tr <- exp(NewData$lwr)

# График без обратной трансформации
ggplot(NewData, aes(x = Height_3, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_point(data = fev_male1, aes(x = Height_3, y = FEV_l, colour = Smoker))

# График после обратной трансформации
ggplot(NewData, aes(x = Height, y = FEV)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr_tr, ymax = upr_tr)) +
  geom_line() +
  geom_point(data = fev_male1, aes(x = Height, y = FEV, colour = Smoker))


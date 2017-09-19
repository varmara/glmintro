## Упражнение. Модели с дискретными и непрерывными предикторами #######

# Как зависит скорость реакции от количества дней с ограничением сна

# Источник данных
# G. Belenky et al. (2003) Patterns of performance degradation and restoration during sleep restriction and subsequent recovery: a sleep dose-response study. Journal of Sleep Research 12, 1–12.

# В исследовании участвовало 18 человек, оно продолжалось 10 дней. В день 0 сон участников не был ограничен. Начиная с этой ночи время сна составляло всего 3 часа. Наблюдения представляют среднее время реакции в серии тестов.

# В таблице 180 наблюдений по трем переменным:
# Reaction - среднее время реакции, мс
# Days - число дней с ограничением сна
# Subject - подяковый номер участника исследования


## Задание

# Исследуйте данные чтобы выяснить, как
# время реакции зависит от 
# - числа дней без сна
# - от личных особенностей участников

# Как каждый из участников реагировал на недостаток сна?

# Проверьте условия применимости моделей


# Загружаем пакеты из библиотеки ##################
library(readxl)
library(ggplot2)
library(car)

# ## Читаем данные ################################
sleepstudy <- read_excel("data/sleepstudy.xlsx")
head(sleepstudy)


# ## Знакомство с данными #########################

str(sleepstudy)
colSums(is.na(sleepstudy))
table(sleepstudy$Subject)

ggplot(sleepstudy, aes(x = Reaction, y = Subject)) + geom_point()
ggplot(sleepstudy, aes(x = Reaction, y = Days)) + geom_point()


# Выбросы
ggplot(sleepstudy, aes(x = Reaction, y = 1:nrow(sleepstudy))) + geom_point()


#### ВАРИАНТ 1. Неправильный - без взаимодействия факторов ########

#### Постоим минимальную линейную модель ##############################

mod_1 <- lm(Reaction ~ Days, sleepstudy)
summary(mod_1)

# Данные для графиков остатков
mod_1_diag <- fortify(mod_1)

# 1) График расстояния Кука
ggplot(mod_1_diag, aes(x = 1:nrow(mod_1_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod_1_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
# В модели: Bwt Sex
gg_resid + aes(x = Days)
gg_resid + aes(x = sleepstudy$Subject)
# а лучше так:
ggplot(mod_1_diag, aes(x = sleepstudy$Subject, y = .stdresid)) +
  geom_boxplot()
# Выглядит ужасно. Игнорировать особенности участников нельзя


# 4) Квантильный график остатков
qqPlot(mod_1)
# Отклонения от нормального распределения


#### Включим Subject без взаимодействия ##############################

M0 <- lm(Reaction ~ Days + Subject, data = sleepstudy)
summary(M0)


gg <- ggplot(sleepstudy, aes(x = Days, y = Reaction))
gg <- gg + geom_point(color = "blue", alpha = 0.7)
gg <- gg + geom_smooth(method = "lm", color = "black")
gg <- gg + theme_bw()
gg


# Данные для графиков остатков
M0_diag <- fortify(M0)

# 1) График расстояния Кука
ggplot(M0_diag, aes(x = 1:nrow(mod_1_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = M0_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

#кажется связь не линейная

# 3) Графики остатков от предикторов в модели и не в модели
# В модели: Bwt Sex
gg_resid + aes(x = Days)

# график остатков похож на бантик: в первые и последние дни исследования дисперсия больше, чем в середине.

gg_resid + aes(x = sleepstudy$Subject)
# или
ggplot(M0_diag, aes(x = sleepstudy$Subject, y = .stdresid)) +
  geom_boxplot()
# Выглядит лучше!

# 4) Квантильный график остатков
qqPlot(M0)
# Отклонения от нормального распределения



#### Добавим взаимодействие факторов ##############################

M1 <- lm(Reaction ~ Days * Subject, data = sleepstudy)
summary(M1)

# Значит, важны не столько стартовые личные особенности, сколько зависимость ответа каждого человека на продолжительность ограничения сна.

# Данные для графиков остатков
M1_diag <- fortify(M1)

# 1) График расстояния Кука
ggplot(M1_diag, aes(x = 1:nrow(M1_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid1 <- ggplot(data = M1_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid1

#нелинейность ушла, появились 3 значения с очень большими остатками

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid1 + aes(x = Days)

# график остатков больше не похож на бантик!

gg_resid1 + aes(x = sleepstudy$Subject)
# или
ggplot(M1_diag, aes(x = sleepstudy$Subject, y = .stdresid)) +
  geom_boxplot()

# 4) Квантильный график остатков
qqPlot(M1)
# Отклонения от нормального распределения


# Правильным графическим представлением данных будет отдельная регрессия для каждого участника

ggplot(sleepstudy, aes(x = Days, y = Reaction, colour = Subject)) + geom_point() + geom_smooth(method = "lm")

# или так

gg <- gg + facet_wrap(~Subject)
gg


# Для каждого испытуемого можно построить свою модель, но число наблюдений тогда будет слишком маленьким. Условия применимости будут часто нарушены.

#Нужен другой метод анализа.

sleep309 <- subset(sleepstudy, Subject == 309)
sleep370 <- subset(sleepstudy, Subject == 370)

M309 <- lm(Reaction ~ Days, data = sleep309)
summary(M309)
M370 <- lm(Reaction ~ Days, data = sleep370)
summary(M370)


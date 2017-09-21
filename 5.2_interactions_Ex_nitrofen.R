# Модель со взаимодействием на примере данных о токсичности нитрофена

## Токсичность нитрофена

# Нитрофен --- гербицид, тератоген, мутаген. Сейчас он не используется в США (первый пестицид, использование которого было запрещено из-за тератогенных эффектов), но все еще используется в России. Эти данные --- данные эксперимента по оценке токсичности нитрофена на дафниях _Ceriodaphnia dubia_ (Bailer, Oris 1994).
#
# 50 дафний случайно разделили на 5 групп, посадили в емкости с разной концентрацией нитрофена, оценили число яиц трех последовательных генераций.
#
# - `conc` --- концентрация нитрофена
# - `total` --- суммарная плодовитость
# - `brood` --- номер выводка
# - `N` --- размер потомства в выводке

## Задание
#
# - Исследуйте данные о токсичности нитрофена
# - Чтобы выяснить, как численность потомства дафний зависит от
# + концентрации нитрофена
# + от выводка
# + взаимодействия концентрации нитрофена и выводка
# + и от суммарной плодовитости дафнии
# - Проверьте условия применимости этой модели
# - Упростите модель, если это возможно
# - Напишите общее уравнение и отдельные уравнения модели для трех выводков
# - Постройте график предсказаний модели

library(ggplot2)
library(readxl)
library(car)

nit <- read_excel("data/nitrofen.xlsx", sheet = 1)
head(nit)

str(nit)
colSums(is.na(nit))
table(nit$conc, nit$brood)

ggplot(nit, aes(x = total, y = 1:nrow(nit))) + geom_point()
ggplot(nit, aes(x = N, y = 1:nrow(nit))) + geom_point()

### Проверка на колинеарность
nit_mod_1 <- lm(N ~ conc + brood + total, data = nit)
vif(nit_mod_1)
# - Колинеарности нет

### Проверка на гомогенность углов наклона
# Подберем полную модель.
nit_mod_2 <- lm(N ~ conc * brood + total, data = nit)
drop1(nit_mod_2, test = "F")
# - От исключения взаимодействия модель становится значительно хуже


# Данные для графиков остатков
nit_mod_2_diag <- fortify(nit_mod_2)

### 1) График расстояния Кука
ggplot(nit_mod_2_diag, aes(x = 1:nrow(nit_mod_2_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

### 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = nit_mod_2_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

### Подумайте, что не так с остатками на этом графике ###
# Две вещи


### 3) Графики остатков от предикторов в модели и не в модели
library(gridExtra)
grid.arrange(gg_resid + aes(x = conc),
             gg_resid + aes(x = brood),
             gg_resid + aes(x = total),
             nrow = 1)

### 4) Квантильный график остатков
qqPlot(nit_mod_2)

## Проверяем значимость коэффициентов
# Подумайте, что означают эти коэффициенты
summary(nit_mod_2)


## Записываем уравнение модели
coef(nit_mod_2)

# Общее уравнение:
# -6.7 + 0.03 * conc +
# + 8.8 * if_brood2 + 11.3 * if_brood3 +
# + 0.3 * total - 0.04 * conc* if_brood2 -
# - 0.05  * conc* if_brood3

# - Для выводка 1:
# N = -6.7 + 0.03conc + 0.3total
# - Для выводка 2:
# N = 2.1 - 0.01conc + 0.3total
# - Для выводка 3:
# N = 4.6 - 0.02conc + 0.3total


## Таблица дисперсионного анализа
anova(nit_mod_2)


### Данные для графика

# Мы не можем их одновременно изобразить на одном
# графике все три переменные из модели. Поэтому мы
# изобразим предсказанный размер потомства при
# разных концентрациях ниторофена и __при средней
# суммарной плодовитости__.

library(plyr)
# ряд значений концентрации один и тот же для
# каждого выводка, поэтому
NewData <- expand.grid(brood = unique(nit$brood),
                       conc = unique(nit$conc),
                       # средние значения суммарной плодовитости для каждого выводка
                       total = mean(nit$total))
# предсказанные значения
Predictions <- predict(nit_mod_2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit
# стандартные ошибки
NewData$SE <- Predictions$se.fit
# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE


## Рисуем график предсказаний
ggplot(NewData, aes(x = conc, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = brood)) +
  geom_line(aes(colour = brood)) +
  geom_point(data = nit, aes(x = conc, y = N, colour = brood))



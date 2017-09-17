## Масса кота и его сердца

library(readxl)
cat <- read_excel("data/cats.xlsx", sheet = 1)
head(cat)

# В таблице три переменных:
# Sex - пол
# Bwt - body weight, вес тела
# Hwt - heart weight, вес сердца

## Задание

# Исследуйте данные о кошках чтобы выяснить, как вес сердца зависит от 
# - веса тела
# - от пола
# - от взаимодействия пола и веса тела

# Проверьте условия применимости этой модели
# Упростите модель, если это возможно
# Напишите общее уравнение и отдельные уравнения модели для трех выводков
# Постройте график предсказаний модели

## Решение

str(cat)
colSums(is.na(cat))
table(cat$Sex)


ggplot(cat, aes(x = Hwt, y = 1:nrow(cat))) + geom_point()
ggplot(cat, aes(x = Bwt, y = 1:nrow(cat))) + geom_point()
```

### Проверка на колинеарность

cat_mod_1 <- lm(Hwt ~ Bwt + Sex, data = cat)
vif(cat_mod_1)

# Колинеарности нет


### Проверка на гомогенность углов наклона

# Подберем полную модель.

cat_mod_2 <- lm(Hwt ~ Bwt * Sex, data = cat)
drop1(cat_mod_2, test = "F")
```

# Исключить взаимодействия нельзя


# Данные для графиков остатков
cat_mod_2_diag <- fortify(cat_mod_2)


# 1) График расстояния Кука

ggplot(cat_mod_2_diag, aes(x = 1:nrow(cat_mod_2_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# выбросов нет

# 2) График остатков от предсказанных значений


gg_resid <- ggplot(data = cat_mod_2_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid


# 3) Графики остатков от предикторов в модели и не в модели

library(gridExtra)
grid.arrange(gg_resid + aes(x = Sex), 
             gg_resid + aes(x = Bwt),
             nrow = 1)
# Есть некоторая гетерогенность лисперсий: кошки варьируют меньше, чем коты. (?)

# 4) Квантильный график остатков

qqPlot(cat_mod_2)

# Небольшие отклонения от нормальности
# Решаем, что модель имеет право на существование


## Проверяем значимость коэффициентов

summary(cat_mod_2)

# Все коэффициенты значимы

# Записываем уравнение модели

coef(cat_mod_2)

# Уравнение для кошек (F): Hwt = -3 + 2.6Bwt
# Уравнение для котов (M): Hwt = -3 - 4.2 + 2.6Bwt + 1.7Bwt = -7.17 + 4.3Bwt



### Данные для графика

# У нас всего две переменные

library(plyr)
# ряд значений концентрации один и тот же для
# каждого выводка, поэтому
NewData <- expand.grid(Sex = unique(cat$Sex),
                       Bwt = unique(cat$Bwt))
# предсказанные значения
Predictions <- predict(cat_mod_2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit
# стандартные ошибки
NewData$SE <- Predictions$se.fit
# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE


## Рисуем график предсказаний

ggplot(NewData, aes(x = Bwt, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = Sex)) +
  geom_line(aes(colour = Sex))

## На графике предсказаний можно показать исходные значения

ggplot(NewData, aes(x = Bwt, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = Sex)) +
  geom_line(aes(colour = Sex)) +
geom_point(data = cat, aes(x = Bwt, y = Hwt, colour = Sex))



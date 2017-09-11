# ## Упражнение по проверке условий применимости линейной регрессии ####
#
# Глюкозаминогликаны в моче
#
# Глюкозаминогликаны участвуют в регуляции межклеточных сигнальных путей, в развитии нейронов, в формировании хрящевой ткани. Измерение концентрации глюкозаминогликанов часто используется при диагностике и классификации мукополисахароидозов
# Данные, о которых пойдет речь - это концентрация глюкозаминогликанов в моче у 314 здоровых детей разного возраста (данные собраны S.Prosser, Paediatrics Department, University of Oxford; источник данных Venables, Ripley 2002). Подобного рода данные можно использовать для получения таблиц референсных значений концентрации глюкозаминогликанов в разном возрасте.
# Переменные:
# - Age - возраст ребенка
# - GAG - концентрация глюкозаминогликанов (единицы измерения утрачены)


#### Загрузка пакетов из библиотеки ####

library(readxl)
library(ggplot2)

#### Знакомство с данными ####

# Не забудьте установить рабочую директорию или
# отредактируйте путь к файлу данных

# Открываем данные
gag <- read_excel("data/GAGurine.xlsx", sheet = 1)
head(gag)

str(gag)

# Есть ли пропущенные значения?
colSums(is.na(gag))

# Есть ли выбросы? Построим дот-плот
gg_dot <- ggplot(gag, aes(y = 1:nrow(gag))) + geom_point()
gg_dot + aes(x = GAG)
gg_dot + aes(x = Age)
# Выбросов нет

# График зависимости
ggplot(gag, aes(x = Age, y = GAG)) +
  geom_point()

# Добавим линию регрессии
ggplot(gag, aes(x = Age, y = GAG)) +
  geom_point() +
  geom_smooth(method = "lm")

# Попробуем трансформировать переменные
ggplot(gag, aes(x = Age, y = log(GAG))) +
  geom_point() +
  geom_smooth(method = "lm")
# не очень хорошо,  но сойдет
ggplot(gag, aes(x = Age, y = GAG^(0.7))) +
  geom_point() +
  geom_smooth(method = "lm")
# не оченьv
ggplot(gag, aes(x = Age, y = sqrt(GAG))) +
  geom_point() +
  geom_smooth(method = "lm")
# не очень
ggplot(gag, aes(x = Age, y = GAG^(1/4))) +
  geom_point() +
  geom_smooth(method = "lm")
# не очень

# - трансформируем данные.

#### ВАРИАНТ C ТРАНСФОРМАЦИЕЙ ############

# Трансформируем
gag$GAG_l <- log(gag$GAG)

M1 <- lm(GAG_l ~ Age, data = gag)
summary(M1)

#### Проверяем условия применимости ####

## Расстояние Кука
cook_cutoff <- 4 / (nrow(gag) - length(coef(M1) - 2))
plot(M1, which = 4, cook.levels = cook_cutoff)
# OK

## График остатков
residualPlot(M1)
# трансформация плохо помогла...

## Квантильный график остатков
set.seed(293234)
qqPlot(M1)
# OK

## Графики остатков от предикторов в модели и нет
M1_diag <- data.frame(gag,
                      .resid = resid(M1, type = "pearson"))
gg_res <- ggplot(M1_diag, aes(x = Age, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point()
gg_res

#### Описываем результаты ####
summary(M1)

# Call:
#   lm(formula = GAG_l ~ Age, data = gag)
#
# Residuals:
#   Min     1Q Median     3Q    Max
# -1.573 -0.214 -0.047  0.155  1.296
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)    2.966      0.029   102.1   <2e-16 ***
#   Age           -0.114      0.004   -28.4   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.353 on 312 degrees of freedom
# Multiple R-squared:  0.722,	Adjusted R-squared:  0.721
# F-statistic:  810 on 1 and 312 DF,  p-value: <2e-16

# Концентрация глюкозаминогликанов в моче с возрастом убывает (t-тест, p < 0.01 )

## Уравнение модели
# GAG_l = 2.97 - 0.11Age

## Доля объясненной изменчивости
# Adjusted R-squared:  0.721

#### График модели  ####

# Обычный график нам не подходит - там предиктор  трансформирован
ggplot(gag, aes(x = Age, y = GAG)) +
  geom_point() +
  geom_smooth(method = "lm")

# Построим график с обратной трансформацией данных

# Данные для графика
NewData <- data.frame(
  Age = seq(min(gag$Age), max(gag$Age), length = 100))

# предсказанные значения
Predictions <- predict(M1, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# Обратная трансформация
NewData$GAG <- exp(NewData$fit)
NewData$upr_tr <- exp(NewData$upr)
NewData$lwr_tr <- exp(NewData$lwr)

# График модели после обратной трансформации
ggplot(NewData, aes(x = Age, y = GAG)) +
  geom_ribbon(alpha = 0.5, aes(ymin = lwr_tr, ymax = upr_tr), fill = "red") +
  geom_line() +
  geom_point(data = gag, aes(x = Age, y = GAG), alpha = 0.5)

# Трансформация не помогла, возможно, эти данные стоит
#   при помощи нелинейной регрессии


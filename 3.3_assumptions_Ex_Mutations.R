# ## Упражнение по проверке условий применимости линейной
# регрессии ####
#
# Мутации у отцов и детей
#
# Kong, A., Frigge, M.L., Masson, G., Besenbacher, S.,
# Sulem, P., Magnusson, G., Gudjonsson, S.A., Sigurdsson,
# A., Jonasdottir, A., Jonasdottir, A. and Wong, W.S., 2012.
# Rate of de novo mutations and the importance of father/'s
# age to disease risk. Nature, 488(7412), pp.471-475.
#
# Конг с соавторами использовали полногеномное
# секвенирование, чтобы исследовать уровень передачи мутаций
# по наследству от родителей к детям (Kong  et al. 2012;
# источник данных Whitlock, Schluter 2009). Всего в
# исследовании приняли участие 78 семей. Мы проанализируем
# выборку, у всех детей в которой была диагностирована
# шизофрения (21 пара отец-ребенок). На этих данных мы
# оценим от возраста отца зависит число новых мутаций,
# которые они передали своим детям.
#
#
# Переменные:
# - Fage - возраст отца
# - Nmut - число унаследованных ребенком новых мутаций


#### Загрузка пакетов из библиотеки ####

library(readxl)
library(ggplot2)

#### Знакомство с данными ####

# Не забудьте установить рабочую директорию или
# отредактируйте путь к файлу данных

# Открываем данные
mut <- read_excel("data/mutations_Kong_et_al_2012_lm_assumptions.xlsx", sheet = 1)
head(mut)

str(mut)

# Есть ли пропущенные значения?
colSums(is.na(mut))

# Есть ли выбросы? Построим дот-плот
gg_dot <- ggplot(mut, aes(y = 1:nrow(mut))) + geom_point()
gg_dot + aes(x = Nmut)
gg_dot + aes(x = Fage)
# Выбросов нет

# График зависимости
ggplot(mut, aes(x = Fage, y = Nmut)) +
  geom_point()

# Добавим линию регрессии
ggplot(mut, aes(x = Fage, y = Nmut)) +
  geom_point() +
  geom_smooth(method = "lm")

# Похоже на гетерогенность дисперсий.

# Попробуем трансформировать переменные
ggplot(mut, aes(x = log(Fage), y = Nmut)) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(mut, aes(x = sqrt(Fage), y = Nmut)) +
  geom_point() +
  geom_smooth(method = "lm")
# может стать немного лучше
ggplot(mut, aes(x = Fage, y = log(Nmut))) +
  geom_point() +
  geom_smooth(method = "lm")
# тоже возможно

# В целях обучения попробуем сделать несколько вариантов анализа:
# - продолжим без трансформации и изучим остатки
# - трансформируем данные.

#### ВАРИАНТ БЕЗ ТРАНСФОРМАЦИИ ############
#### Подбираем модель ####

M1 <- lm(Nmut ~ Fage, data = mut)
summary(M1)
# Call:
#   lm(formula = Nmut ~ Fage, data = mut)
#
# Residuals:
#   Min     1Q Median     3Q    Max
# -8.74  -4.98  -2.06   2.78  20.06
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   17.349      7.335    2.37    0.029 *
#   Fage           1.520      0.265    5.74 0.000016 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 7.13 on 19 degrees of freedom
# Multiple R-squared:  0.635,	Adjusted R-squared:  0.615
# F-statistic:   33 on 1 and 19 DF,  p-value: 0.0000155

#### Проверяем условия применимости ####

# Расстояние Кука
cook_cutoff <- 4 / (nrow(mut) - length(coef(M1) - 2))
plot(M1, which = 4, cook.levels = cook_cutoff)
# OK

# График остатков
residualPlot(M1)
# Вот теперь точно видно гетерогенность дисперсий

# Квантильный график остатков
set.seed(293234)
qqPlot(M1)
# OK

# Итог: на графике остатков видно, что связь нелинейна,
# необходима трансформация или применение нелинейной
# регрессии

#### ВАРИАНТ C ТРАНСФОРМАЦИЕЙ ############

# Трансформируем предиктор
mut$pr_tr <- sqrt(mut$Fage)

M2 <- lm(Nmut ~ pr_tr, data = mut)
summary(M2)

#### Проверяем условия применимости ####

## Расстояние Кука
cook_cutoff <- 4 / (nrow(mut) - length(coef(M2) - 2))
plot(M2, which = 4, cook.levels = cook_cutoff)
# OK

## График остатков
residualPlot(M2)
# трансформация чуть помогла

## Квантильный график остатков
set.seed(293234)
qqPlot(M2)
# OK

## Графики остатков от предикторов в модели и нет
M2_diag <- data.frame(mut,
                      .resid = resid(M2, type = "pearson"))
gg_res <- ggplot(M2_diag, aes(x = pr_tr, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point()
gg_res

#### Описываем результаты ####
summary(M2)

# Call:
#   lm(formula = Nmut ~ pr_tr, data = mut)
#
# Residuals:
#   Min     1Q Median     3Q    Max
# -8.60  -5.06  -1.24   3.17  19.74
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   -22.10      13.82   -1.60     0.13
# pr_tr          15.59       2.65    5.87 0.000012 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 7.03 on 19 degrees of freedom
# Multiple R-squared:  0.645,	Adjusted R-squared:  0.626
# F-statistic: 34.5 on 1 and 19 DF,  p-value: 0.0000118

#

## Уравнение модели
# Nmut = 64.09 + 0.0001 pr_tr

## Доля объясненной изменчивости
# Adjusted R-squared:  0.21

#### График модели  ####

# Обычный график нам не подходит - там  трансформация
ggplot(mut, aes(x = pr_tr, y = Nmut)) +
  geom_point() +
  geom_smooth(method = "lm")

# Построим график с обратной трансформацией данных

# Данные для графика
NewData <- data.frame(
  pr_tr = seq(min(mut$pr_tr), max(mut$pr_tr), length = 100))

# предсказанные значения
Predictions <- predict(M2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# Обратная трансформация предиктора
NewData$Fage <- NewData$pr_tr^2

# График модели после обратной трансформации
ggplot(NewData, aes(x = Fage, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_point(data = mut, aes(x = Fage, y = Nmut))

# Трансформация не очень помогла Эти данные стоит
# анализировать при помощи пуассоновской GLM, т.к.
# число мутаций - счетная переменная, и
# пуассоновское распределение сможет смоделировать
# гетерогенность дисперсий.


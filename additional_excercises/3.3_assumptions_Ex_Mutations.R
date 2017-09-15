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
# оценим как от возраста отца зависит число новых мутаций,
# которые они передали своим детям.
#
#
# Переменные:
# - Fage - возраст отца
# - Nmut - число унаследованных ребенком новых мутаций


#### Загрузка пакетов из библиотеки ####

library(readxl)
library(ggplot2)
library(car)

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
# Выбросов нет, но данных не очень много. Могут быть проблемы.

# График зависимости
ggplot(mut, aes(x = Fage, y = Nmut)) +
  geom_point()

# Добавим линию регрессии
ggplot(mut, aes(x = Fage, y = Nmut)) +
  geom_point() +
  geom_smooth(method = "lm")
# Данных не очень много
# Похоже на гетерогенность дисперсий.

# Попробуем трансформировать переменные
# Проще трансформировать предиктор, чем отклик
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
# -8.742 -4.977 -2.056  2.778 20.062
#
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept)  17.3494     7.3349   2.365   0.0288 *
#   Fage        1.5196     0.2646   5.744 1.55e-05 ***
#   ---
#   Signif. codes:
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 7.127 on 19 degrees of freedom
# Multiple R-squared:  0.6346,	Adjusted R-squared:  0.6153
# F-statistic: 32.99 on 1 and 19 DF,  p-value: 1.552e-05

# Уравнение линейной регрессии
# Nmut = 17.35 + 1.52Fage

#### Проверяем условия применимости ########################

# Данные для графиков остатков
M1_diag <- fortify(M1)

# 1) График расстояния Кука
ggplot(M1_diag, aes(x = 1:nrow(M1_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = M1_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid
# Одно значение с одним большим остатком
# Видна гетерогенность  дисперсий?
# Маленький объем выборки?

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = Fage)
# Видна гетерогенность  дисперсий?
# Маленький объем выборки?

# 4) Квантильный график остатков
qqPlot(M1)
# Одна точка выбивается

# Итог: на графике остатков видна гетерогенность дисперсии,
# необходима трансформация

#### ВАРИАНТ C ТРАНСФОРМАЦИЕЙ ############

# Трансформируем предиктор
mut$Fage_l <- log(mut$Fage)

M2 <- lm(Nmut ~ Fage_l, data = mut)
summary(M2)

#### Проверяем условия применимости ####

# Данные для графиков остатков
M2_diag <- fortify(M2)

# 1) График расстояния Кука
ggplot(M2_diag, aes(x = 1:nrow(M2_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = M2_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid
# Одно значение с одним большим остатком
# Видна гетерогенность  дисперсий?
# Маленький объем выборки?

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = Fage_l)
# Гетерогенность дисперсий

# 4) Квантильный график остатков
qqPlot(M2)

#### Описываем результаты ####

summary(M2)

# Call:
#   lm(formula = Nmut ~ Fage_l, data = mut)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -8.8127 -4.7737 -0.6962  3.4370 19.4740
#
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -70.233     21.704  -3.236  0.00435 **
#   Fage_l      39.327      6.613   5.947    1e-05 ***
#   ---
#   Signif. codes:
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 6.97 on 19 degrees of freedom
# Multiple R-squared:  0.6505,	Adjusted R-squared:  0.6321
# F-statistic: 35.37 on 1 and 19 DF,  p-value: 1.005e-05

# Уравнение линейной регрессии
# Nmut = - 70.23 + 39.33Fage_l

# Регрессия объясняет 65% общей изменчивости (R^2 = 0.65)

#### График модели  ####

# Обычный график нам не подходит - там  трансформация
ggplot(mut, aes(x = Fage_l, y = Nmut)) +
  geom_point() +
  geom_smooth(method = "lm")

# Построим график с обратной трансформацией данных

# Данные для графика
NewData <- data.frame(
  Fage_l = seq(min(mut$Fage_l), max(mut$Fage_l), length = 100))

# предсказанные значения
Predictions <- predict(M2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit

# стандартные ошибки
NewData$SE <- Predictions$se.fit

# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

# Обратная трансформация предиктора
NewData$Fage <- exp(NewData$Fage_l)

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


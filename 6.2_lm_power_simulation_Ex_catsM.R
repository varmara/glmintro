## Упражнение. Мощность линейной регрессии #######

# ## Размеры сердца у котов
#
# Как зависит размер сердца от размера тела у котов? (Fisher 1947; Venables, Ripley 1994)
#
# 97 котов (самцы) весом больше 2 кг. Про каждого кота известно:
#   - `Sex` --- пол
# - `Bwt` --- вес тела в кг
# - `Hwt` --- вес сердца в г

# Загружаем пакеты из библиотеки ##################

library(readxl)
library(car)
library(ggplot2)

options(digits = 4, scipen = 20)

# ## Читаем данные ################################
cat <- read_excel("data/catsM.xlsx", sheet = 1)


# ## Знакомство с данными #########################

str(cat)
colSums(is.na(cat))
table(cat$Sex)

# Выбросы
ggplot(cat, aes(x = Hwt, y = 1:nrow(cat))) + geom_point()
ggplot(cat, aes(x = Bwt, y = 1:nrow(cat))) + geom_point()

#### Постоим линейную модель #####

cat_mod <- lm(Hwt ~ Bwt, data = cat)
summary(cat_mod)

# # Проверка условий применимости ##########################

# Данные для графиков остатков
cat_mod_diag <- fortify(cat_mod)


# 1) График расстояния Кука
ggplot(cat_mod_diag, aes(x = 1:nrow(cat_mod_diag), y = .cooksd)) +
  geom_bar(stat = "identity")
# выбросов нет

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = cat_mod_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = Bwt)

# 4) Квантильный график остатков
qqPlot(cat_mod)

# Небольшие отклонения от нормальности
# Решаем, что модель имеет право на существование

#### Описываем результаты ##################################

## Проверяем значимость коэффициентов
summary(cat_mod)

# Модель значима
# F-statistic:  161 on 1 and 95 DF,  p-value: < 0.01

# Коэффициенты значимы (приводим значения t критерия)

# Уравнение:
# Hwt = -1.2 + 4.313 Bwt

# Доля объясненной изменчивости
# 63%

### Данные для графика

# Диапазон значений Bwt
NewData <- data.frame(
  Bwt = seq(min(cat$Bwt), max(cat$Bwt), length = 100))

# предсказанные значения
Predictions <- predict(cat_mod, newdata = NewData, se.fit = TRUE)
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
  geom_point(data = cat, aes(x = Bwt, y = Hwt))

#### А теперь представим, что это данные пилотного исследования ####

#### Анализ мощности линейной регрессии средствами пакета pwr #######

# Для рассчета величины эффекта линейной регрессии используется
cat_smr <- summary(cat_mod)
r_sq <- cat_smr$r.squared
r_sq

# Величина эффекта F2
# R^2/(1−R^2)
# Коэн предлагает такие пороговые значения для F2
# 0.02 --- слабый эффект
# 0.15 --- средний эффект
# 0.35 --- сильный эффект

F2 <- r_sq / (1 - r_sq)
F2

# Это сильный эффект

# Теперь разберемся с объемом выборки
# В F-тесте есть степени свободы числителя u,
# и знаменателя v
# Число степеней свободы числителя u --- это число
# коэффициентов в модели минус интерсепт, т.е. 1
# для простой линейной регрессии (только b_1).
# Число степеней свободы знаменателя v --- это
# число степеней свободы остаточной изменчивости v
# = n − u − 1.
# Объем выборки складывается из числа степеней свободы
# n = v + u + 1

U <- 1

library(pwr)
pwr_res <- pwr.f2.test(u = U, v = NULL, f2 = F2, sig.level = 0.001, power = 0.8)

V <- pwr_res$v

N <- ceiling(V + U + 1)

N

#### Анализ мощности линейной регрессии с помощью симуляции #####

# Данные из пилотного исследования для инициализации симуляций
b_0 <- coef(cat_mod)[1]
b_1 <- coef(cat_mod)[2]
sigma_resid <- sigma(cat_mod)
xmin <- min(cat$Bwt)
xmax <- max(cat$Bwt)
alpha_cut_off <- 0.001

#### Как симулируют данные из линейной регрессии #####

# Модель
# y_i= b_0 + b_1 * x_i + epsilon, epsilon ~ N(0, sigma_resid^2)

# Проверяемый объем выборки
n_obs <- 16

# Симулируем данные
# значения предиктора
x <- round(runif(n = n_obs, xmin, xmax), 1)
# фиксированная часть
y_det <- b_0 + b_1 * x
# случайная часть
eps <- rnorm(n_obs, sd = sigma_resid)
# значения отклика
y <- y_det + eps
# Строим модель по симулированным данным
LM <- lm(y ~ x)
# Добываем p-value из результатов анализа
smr <- coef(summary(LM))
smr
# p-value для нашего предиктора это вторая строка и 4 столбец
p <- smr[2, 4]
p

#### Делаем 1000 симуляций при помощи циклов #####

n_sim <- 1000
p_vals <- rep(NA, n_sim)

for (i in 1:n_sim) {
  x <- round(runif(n = n_obs, xmin, xmax), 1)
  y_det <- b_0 + b_1 * x
  eps <- rnorm(n_obs, sd = sigma_resid)
  y <- y_det + eps
  LM <- lm(y ~ x)
  smr <- coef(summary(LM))
  p_val <- smr[2, 4]
  p_vals[i] = p_val
}
# Мощность теста это доля всех итераций, где
# получилось p-value <= alpha_cut_off
mean(p_vals <= alpha_cut_off)
# Это при объеме выборки
n_obs


#### Повторяем для разных проверяемых объемов выборок ####

bvec = seq(-2, 2, by = 0.1)
power.b = numeric(length(bvec))
for (j in 1:length(bvec)) {
  b = bvec[j]
  for (i in 1:nsim) {
    y_det = a + b * x
    y = rnorm(N, mean = y_det, sd = sd)
    m = lm(y ~ x)
    pval[i] = coef(summary(m))["x", "Pr(>|t|)"]
  }
  power.b[j] = sum(pval < 0.05)/nsim
}

##### То же самое можно реализовать при помощи функций #####

# TODO:

##### Задание #####

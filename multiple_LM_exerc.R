# "Множественная линейная регрессия" Упражнение 1.

#' Вопрос: можно ли судить о величине опухоли базируясь на знаниях клинических параметров. #' Исследовано 97 пациентов, перенесших простатотомию.
#'
#' Источник: Stamey, T.A., Kabalin, J.N., McNeal, J.E., Johnstone, I.M., Freiha, F., Redwine, E.A. and Yang, N. (1989) Prostate specific antigen in the diagnosis and treatment of adenocarcinoma of the prostate: II. radical prostatectomy treated patients, Journal of Urology 141(5), 1076–1083.

#'
#' **Зависимая перменная**
#'
#' - lcavol - log(cancer volume)
#'
#' **Предикторы**
#'
#' lweight - log(prostate weight)
#'
#' age - Возраст
#'
#' lbph - log(benign prostatic hyperplasia amount)
#'
#' svi - seminal vesicle invasion
#'
#' lcp - log(capsular penetration)
#'
#' gleason - Gleason score
#'
#' pgg45 - percentage Gleason scores 4 or 5
#'
#' lpsa - log(prostate specific antigen)


#' ## Читаем данные
library(readxl)
prost <- read_excel("data/Prostate.xlsx")

#' ## Проверяем, все ли правильно открылось
str(prost)

#' ## Есть ли пропущенные значения?

sapply(prost, function(x)sum(is.na(x)))

#' ## Исследование данных (Data Exploration)
#' Задание: Постройте dotplot для всех переменных, оцените присутствие отскакивающих значений


#' ##Решение
#'
#'
## ---- echo=FALSE---------------------------------------------------------
library(gridExtra)
Pl1 <- ggplot(prost, aes(x = 1:nrow(prost), y = lcavol)) + geom_point()
Pl2 <- ggplot(prost, aes(x = 1:nrow(prost), y = lweight)) + geom_point()
Pl3 <- ggplot(prost, aes(x = 1:nrow(prost), y = age)) + geom_point()
Pl4 <- ggplot(prost, aes(x = 1:nrow(prost), y = lbph)) + geom_point()
Pl5 <- ggplot(prost, aes(x = 1:nrow(prost), y = svi)) + geom_point()
Pl6 <- ggplot(prost, aes(x = 1:nrow(prost), y = lcp)) + geom_point()
Pl7 <- ggplot(prost, aes(x = 1:nrow(prost), y =  gleason)) + geom_point()
Pl8 <- ggplot(prost, aes(x = 1:nrow(prost), y =  pgg45)) + geom_point()
Pl9 <- ggplot(prost, aes(x = 1:nrow(prost), y =  lpsa)) + geom_point()

grid.arrange(Pl1, Pl2, Pl3, Pl4, Pl5, Pl6, Pl7, Pl8, Pl9, ncol =3, nrow = 3)

# Другой способ аализа данных
library(car)
scatterplotMatrix(prost[ , -1])

#' ## Задание
#' - Напишите код, который позволит рассчитать парамтеры линейной модели, описывющей зависимиоть lcavol от всех остальных величин (lweight, age, lbph, svi, lcp, gleason, pgg45, lpsa)

summary(mod1)

#' # Проверка валидности модели
#'
#' ## Решение средствами базовой графики
#'
op <- par(mfrow = c(1, 3))
plot(mod1, which = 4)
residualPlot(mod1)
qqPlot(mod1)
par(op)

#' - Выбросов нет
#' - Гетерогенность дисперсии не выявляется
#' - Отклонения от нормального распределения остатков не выявляются
#' - Есть намек на нелинейность связей

#' ## Решение в `ggplot2`
prost_diag <- fortify(mod1)

# квантильный график
mean_val <- mean(prost_diag$.stdresid)
sd_val <- sd(prost_diag$.stdresid)
gg_qq <- ggplot(prost_diag, aes(sample = .stdresid)) + geom_point(stat = "qq") + geom_abline(intercept = mean_val, slope = sd_val)

# остатки и расстояние Кука
gg_res <- ggplot(data = prost_diag, aes(x = .fitted, y = .stdresid, size = .cooksd)) + geom_point() + geom_hline(aes(yintercept = 0)) + geom_smooth(se = F)

# вместе
library(gridExtra)
grid.arrange(gg_qq, gg_res, nrow = 1, widths = c(0.45, 0.55))

#' # Мультиколинеарность

vif(mod1)

#' ## Удалим из модели избыточный предиктор

mod2 <- update(mod1, ~ . -pgg45)

vif(mod2)

summary(mod2)

#'
#' ## Какой из предиктов оказывает наиболее сильное влияние?
#'
#' Для ответа на этот вопрос надо "уравнять" шкалы, всех предикторов, то есть стандартизовать их.
#'
#' Коэффициенты при стандартизованных предикторах покажут, насколько сильно меняется отклик при изменении предиктора на одно стандартное отклонение.
#'
#' Для стандартизации используем функцию `scale()`
#'

mod2_scaled <- lm(lcavol ~ scale(lweight) + scale(age) + scale(lbph) + scale(svi) + scale(lcp) + scale(gleason) + scale(pgg45) + scale(lpsa), data = prost)

summary(mod2_scaled)


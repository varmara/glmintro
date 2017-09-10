#Множественная регрессия. Упражнение 2.

#' # Какие факторы определяют обилие птиц во фрагментированных лесных массивах Австралии?
#'

#' (Пример взять из книги Quinn&Keugh,2002; Оригинальная работа Loyn, 1987)
#'
#' Фрагментация лесных местообитаний - одна из важнейших проблем Австралии.
#' Вопрос: от каких факторов зависит обилие птиц во фрагментированных лесных массивах?
#'
#' *Зависимая перменная*
#' `ABUND` - Обилие птиц на стандартном маршруте
#'
#' *Предикторы*
#' `AREA` - площадь лесного массива (Га)
#' `YRISOL` - год, в котором произошла изоляция лесного массива
#' `DIST` - расстояние до ближайшего другого лесного массива (км)
#' `LDIST` - расстояние до ближайшего более крупного массива (км)
#' `GRAZE` - качественная оценка уровня выпаса скота (1 - низкий уровень, 5 - высокий урвень)
#' `ALT` - высота над уровнем моря (м)
#'


#' ## Читаем данные
#'

bird <- read.csv("data/loyn.csv")
head(bird)



#'
#' ## Исследование данных (Data Exploration) {.smaller}
#'

library(car)
scatterplotMatrix(bird)

#'
#' ## Явные проблемы
#' ### Есть сильные корреляции между некоторым предикторами.

#' ## Постоим линейную модель

model <- lm(ABUND ~  AREA + YRISOL + DIST + LDIST + GRAZE + ALT, data = bird)

model <- lm(ABUND ~  . , data = bird)

summary(model)

#'
#' # Проверка валидности модели
#'
#' ## Для начала выясним нет ли среди наблюдений влияющих наблюдений

library(ggplot2)
ABUND_diag <- fortify(model)


ggplot(ABUND_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point(aes(size = .cooksd)) +
  geom_smooth(se=FALSE) +
  geom_hline(yintercept=0)

#' ## Проверяем распределение остатков на соответствие нормальному распределению

library(car)
qqPlot(model)

# то же самое средствами ggplot

mean_val <- mean(ABUND_diag$.stdresid)
sd_val <- sd(ABUND_diag$.stdresid)
ggplot(prost_diag, aes(sample = .stdresid)) + geom_point(stat = "qq") + geom_abline(intercept = mean_val, slope = sd_val)


#' ## Проверяем на гетероскедастичность
ggplot(ABUND_diag, aes(x = .fitted, y = .stdresid)) + geom_point(size = 4) +  geom_hline(yintercept=0)

#' ## Проверяем на мультиколлинеарность

vif(model)

# Возможно GRAZE - лишний предиктор

model2 <- update(model, ~ . -GRAZE)
vif(model2)


summary(model2)

#' ## Сравним две модели
summary(model)$adj.r.squared
summary(model2)$adj.r.squared

#' *Удаление этого предиктора, скорее, ухудшает модель!*


#' title: "Линейная регрессия"
#' author: "Вадим Хайтов"
#'
#' ## Пример: Соотношения веса тела и веса сердца у кошек
#' Связан ли вес целого и вес части
#'
#' Было исследовано 97 котов
#'
#' У каждого индивида измеряли:
#'
#' - вес тела  (кг),
#' - вес сердца (г),
#'
#' Пример взят из работы: R. A. Fisher (1947) The analysis of covariance method for the relation between a part and the whole, Biometrics 3, 65–68.
#' Данные представлены в пакете `boot`
#'
#'
#' Загружаем необходимые пакеты

library(readxl)
library(ggplot2)
library(gridExtra)


#'
#'
#' ## Знакомство с данными
#'



cat <- read_excel("data/catsM.xlsx")
head(cat)


#'
#' Есть ли пропущенные значения?

sum(!complete.cases(cat))

#' Нет пропусков

#' ##Каков объем выборки

nrow(cat)

#' ##Есть ли отскакивающие значения?

# Рисуем dotplot

gg_dot <- ggplot(cat, aes(y = 1:nrow(cat))) + geom_point()
gg_dot + aes(x = Bwt)
gg_dot + aes(x = Hwt)


#' # Корреляционный анализ

# Попытайтесь самостоятельно найти функцию, вычисляющую корреляцию





#' ## Вопросы:
#' 1. Чему равны угловой коэффициент и свободный член полученной модели `cat_model`?
#' 2. Какое значение веса сердца предсказывает модель для кота весом 2.5 кг
#' 3. Чему равно значение остатка от модели для кота с порядковым номером 10?
#'
#' ## Ответы

coefficients(cat_model) [1]
coefficients(cat_model) [2]


as.numeric(coefficients(cat_model) [1] + coefficients(cat_model) [2] * 2.5)


#' ## Ответы

cat$Hwt[10] - fitted(cat_model)[10]
residuals(cat_model)[10]


#' ## Углубляемся в анализ модели: функция `summary()`
summary(cat_model)



#' ## Графическое представление результатов
#'

pl_cat <- ggplot(cat, aes(x = Bwt, y = Hwt)) + geom_point() + labs(x = "Вес кота", y = "Вес сердца")
pl_cat + geom_smooth(method="lm")

#'
#'
#' ## Доверительные интервалы для коэффициентов уравнения регрессии
#'
coef(cat_model)

confint(cat_model)

#'
#' ## Для разных уровней значимости можно построить разные доверительные интервалы
#'
pl_alpha1 <- pl_cat + geom_smooth(method="lm", level=0.8) + ggtitle(bquote(alpha==0.2))

pl_alpha2 <- pl_cat + geom_smooth(method="lm", level=0.95) + ggtitle(bquote(alpha==0.05))

pl_alpha3 <- pl_cat + geom_smooth(method="lm", level=0.999) + ggtitle(bquote(alpha==0.01))


grid.arrange(pl_alpha1, pl_alpha2, pl_alpha3, ncol=3)


#' ## Какое значение веса сердца можно ожидать у кота с весом 2.5 кг?

newdata <- data.frame(Bwt = 2.5)

Predicted <- predict(cat_model, newdata, interval = "prediction", level = 0.95, se = TRUE)$fit
Predicted


#'
#' ## Отражаем на графике область значений, в которую попадут 95% предсказанных величин IQ
#'
#' Подготавливаем данные
#'

cat_predicted <- predict(cat_model, interval="prediction")
cat_predicted <- data.frame(cat, cat_predicted)
head(cat_predicted)

#' ## Отражаем на графике область значений, в которую попадут 95% предсказанных величин IQ


pl_cat +

# 1) Линия регрессии и ее дов. интервал
# Если мы указываем fill внутри aes() и задаем фиксированное значение - появится соотв. легенда с названием.
# alpha - задает прозрачность
  geom_smooth(method = "lm", aes(fill = "Conf.interval"), alpha = 0.4, size = 2) +
# 2) Интервал предсказаний создаем при помощи геома ribbon ("лента")
# Данные берем из другого датафрейма - из cat_predicted
# ymin и ymax - эстетики геома ribbon, которые задают нижний и верхний край ленты в точках с заданным x (x = Bwt было задано в ggplot() при создании pl_cat, поэтому сейчас его указывать не обязательно)
  geom_ribbon(data = cat_predicted,  aes(ymin = lwr, ymax = upr, fill = "Conf. area for prediction"), alpha = 0.2) +

# 3) Вручную настраиваем цвета заливки при помощи шкалы fill_manual.
# Ее аргумент name - название соотв. легенды, values - вектор цветов
  scale_fill_manual(name = "Intervals", values = c("green", "blue")) +

# 4) Название графика
  ggtitle("Confidence interval \n and confidence area for prediction")




#"Дискретные предикторы в линейных моделях"


#' ## Глистогонные и рост коз
#'
#' Как связан прирост массы коз с начальным весом животного и интенсивностью профилактики паразитарных заболеваний?
#'
#'
#' - `Treatment` - обработка от глистов (стандартная, интенсивная)
#' - `Weightgain` - привес, кг
#' - `Initial.wt` - начальный вес, кг
#'Пример из библиотеки данных
#' http://www.statlab.uni-heidelberg.de/data/ancova/goats.story.html</div>
#'
#' ## Читаем данные и знакомимся с ними

library(readxl)
goat <- read_excel("data/goats.xlsx", sheet = 1)
head(goat)
str(goat)

colSums(is.na(goat))

# переименуем переменные для краткости
colnames(goat) <- c("Treatment", "Wt", "Init")

# объемы выборок
table(goat$Treatment)

goat$Treatment <- factor(goat$Treatment)

#' ## Есть ли выбросы?

library(ggplot2)
gg_dot <- ggplot(goat, aes(y = 1:nrow(goat))) + geom_point()
gg_dot + aes(x = Wt)
gg_dot + aes(x = Init)

##Строим модель#####

MG <- lm(Wt ~ Init + Treatment, data = goat)

#'
#' В этой модели мы молчаливо считаем,  что характер связи прироста коз с начальным весом будет одинаковым (нет взаимодействия предикторов). Но! Это надо специально проверять (об этом далее)


#'
##Проверяем условия применимости #####

#' ## Нет ли колинеарности между начальным весом и тритментом
vif(MG)

ggplot(goat, aes(x = Treatment, y = Init)) + geom_boxplot()

MG_diag <- fortify(MG)

library(gridExtra)
grid.arrange(
  ggplot(MG_diag, aes(x = 1:nrow(MG_diag), y = .cooksd)) +
    geom_bar(stat = "identity"),
  ggplot(data = MG_diag, aes(x = .fitted, y = .stdresid)) +
    geom_point() + geom_hline(yintercept = 0),
  ggplot(data = MG_diag, aes(x = Init, y = .stdresid)) +
    geom_point() + geom_hline(yintercept = 0),
  ggplot(data = MG_diag, aes(x = Treatment, y = .stdresid)) +
    geom_boxplot(),
  nrow = 1)


#' Все хорошо

#' ## Нормальнсть распределения остатков

library(car)
qqPlot(MG)


#' ## График модели

gg_g <- ggplot(data = goat, aes(y = Wt, x = Init, colour = Treatment)) +
  geom_point()  +
  labs(x = "Начальный вес, кг",
       y = "Привес, кг") +
  scale_colour_discrete("Способ обработки",
                        breaks = c("intensive", "standard"),
                        labels = c("Интенсивный", "Стандартный"))
gg_g + geom_smooth(method = "lm")

#' ##Результаты #####
#'

summary(MG)

#'
#' ##Меняем базовый уровень
#'
#' Это чисто формальная процедура от которой ничего не измеяется по сути, но это иногда необходимо для более удобной визуализации


goat$Treatment <- relevel(goat$Treatment, ref = "standard")

levels(goat$Treatment)

MG1 <- lm(Wt ~ Init + Treatment, data = goat)

summary(MG1)

#'

library(car)
Anova(MG, type = 3)



#'
#' ## Влияет ли стаж работы на предприятиях, вырабатывающих кадмий, на жизненнй объем легких?
#'
#' Пример взят из книги:
#' P. Armitage and G. Berry (1987), Statistical Methods in Medical Research, 2nd ed., Blackwell, p.286.
#'
#' Данные представлены в пакете `ISwR`
#'
#' Переменные:
#'
#' `group` - Группа 1: Более 10 лет в отрасли; Группа 2 - менее 10 лет; Группа 3 -  не подвергались воздействию.
#'
#' `age` - возраст
#'
#' `vital.capacity` - объем легких (л).
#'

## Загружаем данные #####

vit <- read.table("data/vitcap2.csv", header = TRUE, sep = ";")

#' ##Немного преобразуем исходный датасет

vit$Group [vit$group == 1] <- "Long exposed"
vit$Group [vit$group == 2] <- "Short exposed"
vit$Group [vit$group == 3] <- "Not exposed"

#' ## Меняем порядок уровней

vit$Group <- factor(vit$Group, levels = c("Not exposed", "Short exposed", "Long exposed"))

levels(vit$Group)



M1 <- lm(vital.capacity ~ Group, data = vit)

library(car)

Anova(M1, type = 3)

#' ## Геометрическая интерпретация модели с дискретным предиктором
#'
#' Это будет график, отражающий средние значения зависимой переменной, вычисленные для каждой градации дискретного фактора
#'

MyData <- data.frame(Group = levels(vit$Group))

MyData$Group <- factor(MyData$Group, levels = c("Not exposed", "Short exposed", "Long exposed"))

MyData$Predicted <- predict(M1, newdata = MyData, se.fit = TRUE)$fit

MyData$SE <- predict(M1, newdata = MyData, se.fit = TRUE)$se.fit


library(ggplot2)
ggplot(MyData, aes(x = Group, y = Predicted)) +  geom_bar(stat = "identity", aes(fill = Group)) + geom_errorbar(aes(ymin = Predicted - SE, ymax = Predicted + SE), width = 0.2)

summary(M1)

#'
#' Куда делась одна градация фактора?
#'
#' >- В качестве базового уровня предиктора `Group` взято значение "Not exposed"
#'
#'
#'
#' ## Задание
#' 1. Измените базовый уровень переменной `Group` на "Long exposed"
#' 2. Постройте модель, аналогичную `M1`
#' 3. Вычислите предсказанные моделью значения для каждой градации фактора `Group`
#'
#'






#'
#' Можно ли доверять полученным результатам?
#'
M1_diag <- fortify(M1)
qplot(vit$age, M1_diag$.stdresid) + geom_smooth(method = "lm")

#'
#' Очевидный паттерн в остатках!
#'
#' Необходимо включать еще одну переменную - **`ковариату`**
#'
#' ## Analysis of covariance (ANCOVA)
#'
#' ###Меняем модель

M3 <- lm(vital.capacity ~   Group + age , data = vit)

#'
#' ##Диагностика модели
#'

M3_diag <- fortify(M3)
qplot(vit$age, M3_diag$.stdresid) + geom_smooth(method = "lm")


#'
#' Паттерн исчез!
#'

summary(M3)

anova(M3)

#'
#' Противоречие с результатами `summary()`!
#'
#' ## Поменяем порядок предикторов



anova(lm(formula = vital.capacity ~ age + Group, data = vit))

#' `
#' Результат тестирования зависит от порядка предикторов.
#' Почему?
#'


#'
#' ## Вариант 1. Последовательное тестирование (SS type I)
#'
#' Факторы тестируются в порядке включения в модель. Результат тестирования зависит от порядка включения.

anova(lm(formula = vital.capacity ~ Group + age, data = vit))

anova(lm(formula = vital.capacity ~  age + Group, data = vit))

#'
#' ## Вариант 2. Иерархическое тестирование (SS type III)
#'
#' Каждый из факторов по отношению к модели только без него, но со всеми остальными.
#' Нужно, если много факторов и выборки разного размера. Тогда результат не будет зависеть от порядка включения факторов в модель.

library(car)
Anova(M3, type = 3)


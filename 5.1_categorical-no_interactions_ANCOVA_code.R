#'Дискретные предикторы в линейных моделях"
#'
#' ##Влияет ли стаж работы на предприятиях, вырабатывающих кадмий, на жизненнй объем легких?
#'
#' Прмер взят из книги:
#' P. Armitage and G. Berry (1987), Statistical Methods in Medical Research, 2nd ed., Blackwell, p.286.
#'
#' Данные представленны в пакете `ISwR`
#'
#' Переменные:
#'
#' `group` - Группа 1: Более 10 лет в отрасли; Группа 2 - менее 10 лет; Группа 3 -  не подвергались воздействию.
#'
#' `age` - возраст
#'
#' `vital.capacity` - объем легких (л).


## Загружаем данные############

vit <- read.table("data/vitcap2.csv", header = TRUE, sep = ";")

vit$Group [vit$group == 1] <- "Long exposed"
vit$Group [vit$group == 2] <- "Short exposed"
vit$Group [vit$group == 3] <- "Not exposed"


## Меняем порядок уровней ############

vit$Group <- factor(vit$Group, levels = c("Not exposed", "Short exposed", "Long exposed"))

levels(vit$Group)

##Вычисляем средние #########

library(dplyr)


vit_summary <- vit %>%  group_by(Group) %>%  #Передаем датафрейм группирующей функции
            summarise(mean_capac = mean(vital.capacity),  # Вычисляем среднюю
            SD_capac = sd(vital.capacity), # Стандартное отклонение
            n_capac = n(),  # объем выборки
            SE_capac = sd(vital.capacity)/sqrt(n())) # Стандартную ошибку

vit_summary

##Визуализируем результаты

library(ggplot2)

ggplot(vit_summary, aes(x = Group, y = mean_capac)) +  geom_bar(stat = "identity", aes(fill = Group)) + geom_errorbar(aes(ymin = mean_capac - SE_capac, ymax = mean_capac + SE_capac), width = 0.2)



##Строим модель с дискретным предиктором

M1 <- lm(vital.capacity ~ Group, data = vit)

summary(M1)


anova(M1)


#Вычисление предсказанных значений с помощью матричной алгебры

model.matrix(M1)

#' В модельной матрице три колонки
#'
#' 1 - `Intercept`
#' 2 - `GroupShort exposed `- в тех строках, которые соответствуют градации переменной `Group` = "Short exposed", стоит 1, в остальных строках  - 0
#' 3.  `GroupLong exposed` - в тех строках, которые соответствуют градации переменной `Group` = "Long exposed" стоит 1, в остальных строках  - 0
#'
#' Одна из градаций фактора (первая, если специально не указать иное) становится **базовым уровнем**

#' ##Предсказанные значения
#'
#' Мы уже умеем вытаскивать из модели предсказанные значения
#'

fitted(M1)


#' ##Предсказанные значения, вычисленные через матричное умножение

fitted <- model.matrix(M1) %*% coefficients(M1)

as.vector(fitted)


# Что будет если изменить базовый уровень

vit$Group <-  relevel(vit$Group, ref = "Long exposed" )


#' ##Изменились ли результаты?

M2 <- lm(vital.capacity ~ Group, data = vit)

summary(M2)

#' ##Вычислим предсказание модели M2 для индивидов из всех трех групп
#'
#' Для "Not exposed"
#'

3.949 + 0.513 * 1 + 0.523 * 0

#' Для "Short exposed"

3.949 + 0.513 * 0 + 0.523 * 1

#' Для "Long exposed"

3.949 + 0.513 * 0 + 0.523 * 0


#' Можно ли доверять полученным результатам?

M1_diag <- fortify(M1)

qplot(vit$age, M1_diag$.stdresid) + geom_smooth(method = "lm")


# Analysis of covariance (ANCOVA)############

M3 <- lm(vital.capacity ~ Group + age, data = vit)

#' ##Диагностика модели

M3_diag <- fortify(M3)

qplot(vit$age, M3_diag$.stdresid) + geom_smooth(method = "lm")


#'
#' Паттерн исчез!


summary(M3)

anova(M3)

#'
#' Противоречие с результатами `summary()`!


#' ## Поменяем порядок предикторов


anova(lm(formula = vital.capacity ~ age + Group, data = vit))

#' `
#' Результат тестирования зависит от порядка предикторов.
#' Почему?
#'


#' ## Два варианта тестирования
#'
#' - Последовательное
#' - Иерархическое

#' ## Вариант 1. Последовательное тестирование (SS type I)
#'
#' Факторы тестируются в порядке включения в модель. Результат тестирования зависит от порядка включения.

anova(lm(formula = vital.capacity ~ Group + age, data = vit))

#' Интересующй нас фактор должен бЫть на последнем месте.
#'

anova(lm(formula = vital.capacity ~ age + Group, data = vit))

#'
#' ### Вариант 2. Иерархическое тестирование (SS type III)
#'
#' Каждый из факторов по отношению к модели только без него, но со всеми остальными.
#' Нужно, если много факторов и выборки разного размера. Тогда результат не будет зависеть от порядка включения факторов в модель.

library(car)
Anova(M3, type = 3)


#' #ANCOVA на примере данных о влиянии глистогонных на рост коз
#'
#' ## Глистогонные и рост коз
#'
#' Как влияет на прирост массы коз интенсивность профилактики паразитарных заболеваний?
#'
#' Известно, что у легких животных привес больше, поэтому мы должны учитывать в модели не только способ обработки от глистов, но и влияние начального веса.
#'
#'
#' - `Treatment` - обработка от глистов (стандартная, интенсивная)
#' - `Weightgain` - привес, кг
#' - `Initial.wt` - начальный вес, кг
#'Пример из библиотеки данных
#' http://www.statlab.uni-heidelberg.de/data/ancova/goats.story.html</div>
#'
#'
#'

#' ## Читаем данные и знакомимся с ними
#'

library(readxl)
goat <- read_excel("data/goats.xlsx", sheet = 1)

head(goat)

str(goat)

colSums(is.na(goat))

# переименуем переменные для краткости
colnames(goat) <- c("treat", "wt", "init")

# объемы выборок
table(goat$treat)

# зададим порядок уровней фактора для удобства интерпретации
goat$treat <- factor(goat$treat, levels = c("standard", "intensive"))

#' ## Есть ли выбросы?

library(ggplot2)
gg_dot <- ggplot(goat, aes(y = 1:nrow(goat))) + geom_point()
gg_dot + aes(x = wt)
gg_dot + aes(x = init)

#'
#' ## Задание
#'
#' - Подберите модель, описывающую зависимость между увеличением веса коз и способом прфилактической обработки животных
#' - Проверьте условия применимости этой модели
#' - Упростите модель, если это возможно
#' - Проверьте условия применимости финальной модели
#' - Напишите общее уравнение и отдельные уравнения модели для двух способов профилактики
#' - Постройте график предсказаний модели
#'
#' ## Решение

# Пусть модель имеет название MG


summary(MG)

anova(MG)

library(car)
Anova(MG, type = 3)


# Задание: Постройте график, описывающий данную модель

#' ## График модели


#Задание: Проверьте условия применимости



# Вопрос: Как изменяется привес коз при интенсивной терапии?



# Дискретные предикторы в линейных моделях
#
# Данные взяты из
# D. Kronborg and L.T. Skovgaard (1990),
# Regressionsanalyse, Table 1.1, FADLs Forlag (in
# Danish).
#
# Данные приведены в пакете {ISwR}
#
# Изучается зависимость диаметра клеток инфузорий
# Tetrahymena от того добавлена ли глюкоза в
# среду, в которой живут клетки
#
#
# Переменные
# glucose – добавлена ли глюкоза, 1: Да, 2: Нет.
# conc - концентрация клеток в культуре(counts/ml).
# diameter – диметр клеток (micrometre).
#
# Вопросы:
# Зависит ли диаметр клеток от того добавлена ли глюкоза?
# На сколько изменится диаметр клетки если добавить глюкозу?

library(readxl)
library(car)
library(ggplot2)

# Читаем данные ####

inf <- read_excel("data/hellung.xls")

# Вводим удобное обозначение факторов ##########

inf$glucose_f <- factor(inf$glucose, levels = c(1, 2), labels = c("Yes", "No"))


scatterplotMatrix(inf[, -c(1, 4)])

ggplot(inf, aes(x = log(conc), y = diameter, colour = glucose_f)) + geom_point()

# Строим модель ####
M1 <- lm(diameter ~ log(conc)*glucose_f, data = inf)

summary(M1)

anova(M1)


#Проверяем условия применимости#####

library(ggplot2)
M1_diag <- fortify(M1)

ggplot(M1_diag, aes(x = .fitted, y = .stdresid)) + geom_point() + geom_smooth()

ggplot(M1_diag, aes(x = glucose_f, y = .stdresid)) + geom_boxplot()


ggplot(M1_diag, aes(x = log(inf$conc), y = .stdresid)) + geom_point() + geom_smooth()


# Дискретные предикторы в линейных моделях
#
# Данные взяты из
# D. Kronborg and L.T. Skovgaard (1990), Regressionsanalyse, Table 1.1, FADLs Forlag (in Danish).
#
# Данные приведены в пакете {ISwR}
#
# Изучается зависимость диаметра клеток инфузорий Tetrahymena от того добавлена ли глюкоза в среду, в которой живут клетки
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
#
#


#Читаем данные####

inf <- read_excel("data/hellung.xls")

#Вводим удобное обозначение факторов##########

inf$Gluc [inf$glucose == 1] <- "Yes"
inf$Gluc [inf$glucose == 2] <- "No"

inf$Gluc <- factor(inf$Gluc)

# Строим модель####
M1 <- lm(diameter ~ conc + Gluc, data = inf)

summary(M1)

anova(M1)


#Проверяем условия применимости#####
library(ggplot2)
M1_diag <- fortify(M1)

ggplot(M1_diag, aes(x = .fitted, y = .stdresid)) + geom_point() + geom_smooth()

ggplot(M1_diag, aes(x = Gluc, y = .stdresid)) + geom_boxplot()


ggplot(M1_diag, aes(x = conc, y = .stdresid)) + geom_point() + geom_smooth()

# Проблемы!!! Нужно изменить модель!


# Есть подозрение, что авторы дают временные ряды!!!
# Посмотрим, как ведет себя зависимая переменная

# Создаем маркеры времени

inf$Time [inf$Gluc == "Yes"] <- 1:sum(inf$Gluc == "Yes")
inf$Time [inf$Gluc == "No"] <- 1:sum(inf$Gluc == "No")


ggplot(inf, aes(x = Time, y = diameter, color = Gluc)) + geom_point() + geom_smooth()


# Пробуем использовать время, как предиктор

M2 <- lm(diameter ~ Time + Gluc, data = inf)

M2_diag <- fortify(M2)

ggplot(M2_diag, aes(x = .fitted, y = .stdresid)) + geom_point() + geom_smooth()

ggplot(M2_diag, aes(x = Gluc, y = .stdresid)) + geom_boxplot()

ggplot(M2_diag, aes(x = Time, y = .stdresid)) + geom_point() + geom_smooth()

# Чуть лучше....

anova(M2)

# Но! может быть есть взаимодействие?

M3 <- lm(diameter ~ Time*Gluc, data = inf)

M3_diag <- fortify(M3)

ggplot(M3_diag, aes(x = .fitted, y = .stdresid)) + geom_point() + geom_smooth()

ggplot(M3_diag, aes(x = Gluc, y = .stdresid)) + geom_boxplot()

ggplot(M3_diag, aes(x = Time, y = .stdresid)) + geom_point() + geom_smooth()

anova(M3)



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
# Вопрос:
# Зависит ли диаметр клеток от того добавлена ли глюкоза?

# Читаем данные ####

inf <- read_excel("data/hellung.xls")

#Вводим удобное обозначение факторов##########

inf$Gluc [inf$glucose == 1] <- "Yes"
inf$Gluc [inf$glucose == 2] <- "No"

inf$Gluc <- factor(inf$Gluc)

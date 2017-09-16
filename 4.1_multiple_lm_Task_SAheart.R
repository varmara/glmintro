## Множественная линейная регрессия --- подбор моделей ######
#
# Эти данные --- результаты ретроспективного
# исследования факторов, связанных с риском
# развития сердечно сосудистых заболеваний у
# мужчин. Исследование проведено в регионе с
# высокими рисками --- Вестерн Кейп, Южная Африка
#
# Выборка в этом файле неоднородна: у части мужчин
# было диагностировано заболевание коронарных
# сосудов. Многие из таких мужчин проходили
# терапию по снижению артериального давления уже
# после постановки диагноза. И в некоторых случаях
# измерения были сделаны уже после применения
# терапии.
#
# Эти данные взяты из большого датасета из работы
# Rousseauw et al, 1983 (опубликована в South
# African Medical Journal)
#
# Rousseauw, J., du Plessis, J., Benade, A.,
# Jordaan, P., Kotze, J. and Ferreira, J. (1983).
# Coronary risk factor screening in three rural
# communities, South African Medical Journal 64:
# 430–436.

# Переменные:
# sbp --- systolic blood pressure
# tobacco --- cumulative tobacco (kg)
# ldl --- low density lipoprotein cholesterol
# adiposity --- a numeric vector
# famhist ---  family history of heart disease, a factor with levels Absent Present
# typea --- type-A behavior
# obesity --- a numeric vector
# alcohol --- current alcohol consumption
# age --- age at onset
# chd --- coronary heart disease


# Задача: ----------------------------------------

# Давайте проанализируем, от каких факторов
# зависит артериальное давление (sbp). Возьмите
# данные только о здоровых мужчинах (у которых не
# было заболевания коронарных сосудов).
# Проанализируйте эти данные при помощи
# множественной линейной регрессии. Не используйте
# для построения регрессии в качестве предикторов
# дискретную переменную famhist (мы еще не научили
# вас с ними работать). Но обязательно обратите
# внимание на нее во время анализа остатков.
# Разведочный анализ показал, что нужно
# логарифмировать tobacco, ldl, alcohol, obesity
# --- мы сделали это за вас


# Загружаем пакеты из библиотеки ##################
library(readxl)
library(car)
library(ggplot2)

# ## Читаем данные ################################

SAheart <- read_excel("data/SAheart.xlsx")
# ## Фильтруем данные по условиям задачи ##########

# По условиям задачи нам нужно взять мужчин, у которых
# переменная chd не равна нулю, т.е. нет болезни
# коронарных артерий
filt <- SAheart$chd != 0
pressure <- SAheart[filt, -c(9, 10)]

# ## Знакомство с данными #########################
# как называются наши переменные?
colnames(pressure)

# Есть ли пропущенные значения
colSums(is.na(pressure))

# у скольких человек в семье были сердечно-сосудистые заболевания?
table(pressure$famhist)

# Связи между переменными
scatterplotMatrix(pressure[, -5])
# На скаттерплотах связи выглядят линейными, но зато стоит
# логарифмировать tobacco, ldl, alcohol, obesity
pressure$tobacco_l <- log(pressure$tobacco + 1)
pressure$ldl_l <- log(pressure$ldl + 1)
pressure$obesity_l <- log(pressure$obesity + 1)
pressure$alcohol_l <- log(pressure$alcohol + 1)



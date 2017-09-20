# Проверка усвоенного
#
# Прочитайте данные из файла "fev.xls" и удалите строки, в которых есть пропущенные значения

library(readxl)

fev <- read_excel("data/fev.xls", sheet = "tidy_data", skip = 1, na = "NA")
fev <- fev[complete.cases(fev), ]
fev$Sex <- factor(fev$Sex)
fev$Smoker <- factor(fev$Smoker)

#
# Задание1
# Определите значение 3-го квартиля для переменной Height для девочек
##########################

summary(fev$Height[fev$Sex == "Female"])
quantile(fev$Height[fev$Sex == "Female"], probs = 0.75)

#
# Задание 2
# Чему равно среднее значение роста мальчиков в данной выборке?
##########################
filter_males <- fev$Sex == "Male"
males <- fev$Height[filter_males]
mean(males)


#
# Задание 3
# Чему равна стандартная ошибка среднего для роста мальчиков в данной выборке?
##########################
filter_males <- fev$Sex == "Male"
males <- fev$Height[filter_males]
N <- length(males)
sd(males) / sqrt(N)


# Задание 4.
# Сравните с помощью t-критерия рост мальчиков и девочек. Чему равно значение t-критерия? Значимы ли различия?
##########################
filter_males <- fev$Sex == "Male"
males <- fev$Height[filter_males]
females <- fev$Height[!filter_males]

# 1 способ
t.test(females, males)
# 1 способ
t.test(Height ~ Sex, data = fev)

#
# Задание 5.
# Перед вами ряд значений уровня значимости, которые получены в результате множественного сравнения выборок с помощью t-критерия. Сколько из них говорят о достоверных различиях, если в качестве порога достоверности взят уровень значимости 0.05? Используйте поправку Бонферрони
###############

p <- c(0.039046442, 0.007168042, 0.043416246, 0.005906894, 0.004664741, 0.037094342, 0.019788120)

p_adj <- p.adjust(p, method = "bonferroni", n = 7)
p_adj
sum(p_adj < 0.05)

# Задание 6.
# Постройте график, который вы найдете в файле picture.jpg
##########################

library(ggplot2)
library(gridExtra)
library(dplyr)

# первый график
pl1 <- ggplot(fev, aes(x = Height)) +
  geom_density(aes(fill = Smoker), alpha = 0.5) +
  facet_wrap(~ Sex) +
  theme_bw() +
  labs(title = "Частотное распределение роста")
pl1

# Данные для второго графика
# 1 способ
fev_sum <- fev %>% group_by(Age) %>% summarise(fev_mean = mean(Height), fev_SD = sd(Height))
# 2 способ
means <- tapply(fev$Height, INDEX = fev$Age, FUN = mean)
ages <- as.numeric(names(means))
sds <- tapply(fev$Height, INDEX = fev$Age, FUN = sd)
fev_sum <- data.frame(Age = ages,
                      fev_mean = means,
                      fev_SD = sds)

# второй график
pl2 <- ggplot(fev_sum, aes(x = Age, y = fev_mean)) +
  geom_line(color = "blue") +
  geom_errorbar(aes(ymin = fev_mean - fev_SD, ymax = fev_mean + fev_SD), width = 0.2) +
  geom_point(color = "red") +
  theme_classic() +
  ggtitle("Зависимость роста от возраста \n Усы отражают среднеквадратичное отклонение") +
  ylab("Средний рост")
pl2

library(gridExtra)
Pl <- grid.arrange(pl1, pl2, ncol = 1)
Pl
ggsave("picture.jpg", plot = Pl )

# Задание 7. Постройте линейную модель, описывающую зависимость роста от возраста и пола с учетом взаимодействия между этими предикторами
# Какую долю суммарной дисперсии описывает эта модель?
# Значимо ли влияние взаимодействия возраста и пола на рост?
# Чему равен AIC полной модели?
# Чему будет равен AIC, если выкинуть из модели взаимодействие предикторов?
# Чему равен предсказанный полной моделью рост у мальчиков при возрасте в 15 лет?
##########################
# Какую долю суммарной дисперсии описывает эта модель?
M1 <- lm(Height ~ Age*Sex, data = fev)
summary(M1)$adj.r.squared

# Значимо ли влияние взаимодействия возраста и пола на рост?
library(car)
Anova(M1, type = 3)

# Чему равен AIC полной модели?
M2 <- glm(Height ~ Age*Sex, data = fev)
AIC(M2)

# Чему будет равен AIC, если выкинуть из модели взаимодействие предикторов?
M3 <- glm(Height ~ Age + Sex, data = fev)
AIC(M3)

# Чему равен предсказанный полной моделью рост у мальчиков при возрасте в 15 лет?
MyData <- data.frame(Sex = c("Male", "Female"), Age = 15)

MyData$Predict <- predict(M1, newdata = MyData)




# Задание 8.
# Постройте линейную модель, описывающую связь между объемом легких (FEV) со всеми остальными перменными  без учета взаимодействия между предикторами. Какие три условия применения множественной регрессии здесь будут нарушены?

M <- lm (FEV ~ Age + Height + Sex + Smoker, data = fev)

vif(M) #Коллинеарность предикторов

M_diag <- fortify(M)

ggplot(M_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_smooth()

# Нелинейность
# Гетероскедастичность


# Задание 9.
# Постройте линейную модель, описывающую связь между объемом легких (FEV) и переменными Smoker и Sex без учета взаимодействия. Какое значение предикторов взято в качестве базового уровня?

M <- lm(FEV ~ Sex + Smoker, data = fev)

summary(M)
# Sex --- Female, Smoker --- Current

# Задание 10.
# Измените в предыдущей модели базовый уровень, приняв в качестве него «Kурящих мальчиков». Запишите уравнение этой модели.

fev$Sex <- relevel(fev$Sex, ref = "Male")
fev$Smoker <- relevel(fev$Smoker, ref = "Current")

M <- lm(FEV ~ Sex + Smoker, data = fev)

summary(M)

coef(M)

# FEV = 3.5166176 -0.3911191*if_Female  -0.7688961*if_NonSmoker



# Проверка усвоенного
#
# Прочитайте данные из файла "fev.xls" и удалите строки, в которых есть пропущенные значеиния

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

#
# Задание 2
# Чему равно среднее значение роста мальчиков в данной выборке?
##########################

mean(fev$Height[fev$Sex == "Male"])


#
# Задание 3
# Чему равна стандартная ошибка среднего для роста мальчиков в данной выборке?
##########################

sd(fev$Height[fev$Sex == "Male"]) / sqrt(length(fev$Height[fev$Sex == "Male"]))


#
# Задание 4.
# Сравните с помощью t-критерия рост мальчиков и девочек. Чему равно значение t-критерия?
##########################

t.test(fev$Height[fev$Sex == "Male"], fev$Height[fev$Sex == "Female"])

#
# Задание 5.
# Перед вами ряд значений уровня значимости, которые получены в результате множественного сравнения выборок с помощью t-критерия. Сколько из них говорят о достоверных различиях, если в качестве порога достоверности взят уровень значимости 0.05?
###############

c(0.039046442, 0.007168042, 0.043416246, 0.005906894, 0.004664741, 0.037094342, 0.019788120)

p.adjust(p, method = "bonferroni", n = 7)

#
#
# Задание 6.
# Постройте график, который вы найдете в файле picture.jpg
##########################


p.adjust(0.05, method = "bonferroni", n = 4)

sum(is.na(fev))

library(ggplot2)

pl1 <- ggplot(fev, aes(x = Height)) + geom_density(aes(fill = Smoker), alpha = 0.5) + facet_wrap(~Sex) + theme_bw() + ggtitle("Частотное распределение роста")


library(dplyr)

fev_sum <- fev %>% group_by(Age) %>% summarise(fev_mean = mean(Height), fev_SD = sd(Height))

pl2 <- ggplot(fev_sum, aes(x = Age, y = fev_mean)) + geom_line(color = "blue") + geom_errorbar(aes(ymin = fev_mean - fev_SD, ymax = fev_mean + fev_SD), width = 0.2) + geom_point(color = "red") + theme_classic() + ggtitle("Зависимость роста от возраста \n Усы отражают среднеквадратичное отклонение") + ylab("Средний рост")

library(gridExtra)
Pl <- grid.arrange(pl1, pl2, ncol = 1)
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

ggplot(M_diag, aes(x = .fitted, y = .stdresid)) + geom_point() + geom_smooth()

#Нелинейность
#Гетероскедастичность


# Задание 9.
# Постройте линейную модель, описывающую связь между объемом легких (FEV) и переменными Smoker и Sexб без учета взаимодействия. Какое значение предикторов взято в качестве базового уровня?

M <- lm(FEV ~ Sex + Smoker, data = fev)

summary(M)


# Задание 10.
# Измените в предыдущей модели базовый уровень, приняв в качестве него «Kурящих мальчиков». Запишите уравнение этой модели.

fev$Sex <- relevel(fev$Sex, ref = "Male")
fev$Smoker <- relevel(fev$Smoker, ref = "Current")

M <- lm(FEV ~ Sex + Smoker, data = fev)

summary(M)

coef(M)

# predict = 3.5166176 -0.3911191*I  -0.7688961*I



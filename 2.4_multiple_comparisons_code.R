#' ---
#' title: "Множественные сравнения"
#' author: "Юта Тамберг, Марина Варфоломеева"
#' ---
#' 
#' ## Family-Wise Error Rate – допустимая частота ошибок I рода для всего семейства тестов.
#' 
#' ## Поправка Бонферрони
#' 
#' Полученные в каждом тесте p-значения нужно умножить на общее число тестов (m) и только после этого сравнить с критическим значением
#' 
#' ## Поправка Хольма-Бонферрони
#' 
#' Процедура применения поправки пошаговая. Сначала сортируем p-values, полученные в тестах, в порядке возрастания и присвоим каждой ранг j от 1 до m.
#' 
#' Затем применяем поправку p * (m – j + 1), после чего сравниваем с уровнем занчимости.
#' 
#' 
#' ## False Discovery Rate задает количество ложно-положительных результатов в отношении к истинно-положительным.
#' 
#' ## Метод Беньямини-Хохберга
#' 
#' Процедура Беньямини-Хохберга прошаговая. Начинаем опять сортировки и придания рангов p-значениям
#' 
#' Затем находим такое значение p-value с наибольшим рангом j чтобы p_j =< FDR * (j/m)
#' 
#' Все тесты с рангами меньше j считаем значимыми.
#' 

#########################################################

#' 
#' # Множественные сравнения (t-тесты) в R
#' 
#' ## Case study 1: Fake gene expression
#' 
#' Прочитаем таблицу симулированных данных и посмотрим как они устроены:
#' 
expr <- read.table("data/fake_expression_samples.csv", header=T, sep = ",")
head(expr)
str(expr)

#' 
#' Нам надо сравнить выборки 1 и 2 по каждому из 10 генов. Начнем с первого

t.test(expr$sample1[expr$gene_ID=="1"],
       expr$sample2[expr$gene_ID=="1"])

#' 
#' Нам понадобятся только р-значение, поэтому хорошо если мы сможем ее извлечь и записать в новую переменную
#' 

t_result <- t.test(expr$sample1[expr$gene_ID=="1"],
                   expr$sample2[expr$gene_ID=="1"])
t_result$p.value

#' 
#' Мы можем добиться того же самого и одной командой:
t1 <- t.test(expr$sample1[expr$gene_ID=="1"],
             expr$sample2[expr$gene_ID=="1"])$p.value
t1

#' 
#' Теперь надо лишь выполнить эту же операцию для оставшихся девяти сравнений
#' 
t2 <- t.test(expr$sample1[expr$gene_ID=="2"],
             expr$sample2[expr$gene_ID=="2"])$p.value
t3 <- t.test(expr$sample1[expr$gene_ID=="3"],
             expr$sample2[expr$gene_ID=="3"])$p.value
t4 <- t.test(expr$sample1[expr$gene_ID=="4"],
             expr$sample2[expr$gene_ID=="4"])$p.value
t5 <- t.test(expr$sample1[expr$gene_ID=="5"],
             expr$sample2[expr$gene_ID=="5"])$p.value
t6 <- t.test(expr$sample1[expr$gene_ID=="6"],
             expr$sample2[expr$gene_ID=="6"])$p.value
t7 <- t.test(expr$sample1[expr$gene_ID=="7"],
             expr$sample2[expr$gene_ID=="7"])$p.value
t8 <- t.test(expr$sample1[expr$gene_ID=="8"],
             expr$sample2[expr$gene_ID=="8"])$p.value
t9 <- t.test(expr$sample1[expr$gene_ID=="9"],
             expr$sample2[expr$gene_ID=="9"])$p.value
t10 <- t.test(expr$sample1[expr$gene_ID=="10"],
              expr$sample2[expr$gene_ID=="10"])$p.value

#' 
#' Мы посчитали десять p-values для всех генов. Соберем их в один вектор и присвоим имена.
#' 
pvals <- c(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
names(pvals) <- c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10")
pvals

#' 
#' Сколько генов, достоверно меняющих экспрессию, мы нашли в "сырых" t-тестах?
sum(pvals <= 0.05)

#' 
#' А теперь внесем поправки.
#' 
#' Поправки к p-values в R можно сделать при помощи функции `p.adjust()`
#' Аргумент `method` задает тип поправки.
#' 
p_bonf <- p.adjust(pvals, method = "bonferroni")

#' 
#' У скольких генов экспрессия достоверно различается после поправки Бонферрони?
#' 
sum(p_bonf <= 0.05, na.rm = TRUE)

#' 
#' После поправки Хольма?
#' 
p_holm <- p.adjust(pvals, method = "holm")
sum(p_holm <= 0.05, na.rm = TRUE)

#' 
#' После применения процедуры Беньямини-Хохберга?
#' 
p_bh <- p.adjust(pvals, method = "BH")
sum(p_bh <= 0.05, na.rm = TRUE)

#' 
#' Результаты каких именно тестов показали значимые различия?
#' 
names(p_bh[p_bh <= 0.05])

#' 

#######################################################

#' ## Case study 2: Экскреция метаболитов и синдром Кушинга. Самостоятельная работа
#' 
#' Синдром Кушинга это сборное название для ряда гормональных заболеваний разной этиологии, которые ведут к перевыработке гормона кортизола. 
#' 
#' Датасет `Cushings` содержит данные по объему выведения с мочой двух метаболитов стероидных гормонов. 
#' 
#' 

library(readxl)

Cushings <- read_excel("data/Cushings.xlsx", sheet = 1)
head(Cushings)

str(Cushings)

#' 
#' Как видим, в таблице данных три переменные:
#' 
#' `Tetrahydrocortisone` -- объем (мг в сутки) выведения тетрагидрокортизона
#' `Pregnanetriol` --  объем (мг в сутки) выведения прегнанетриола
#' `Type` -- разновидности синдрома: a = adenoma, b = bilateral hyperplasia, c = carcinoma и u = unknown.
#' 
#' Названия переменных слишком замысловатые, переназовем их покороче.
#' 
names(Cushings)[names(Cushings)=="Tetrahydrocortisone"] <- "Tetr"
names(Cushings)[names(Cushings)=="Pregnanetriol"] <- "Pre"

#' 
#' Ваша задача - выбрать один из метаболитов, и сравнить уровни его экскреции между всеми типами синдромов. Какие типы заболеваний вы стали бы исследовать дальше и почему?
#' 
#' ## Решение. Tetrahydrocortisone
#' 








#' 
#' ## Решение. Pregnanetriol
#' 










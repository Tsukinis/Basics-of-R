'''
Ваша задача в переменную red_men сохранить долю рыжеволосых (Red) от общего числа голубоглазых мужчин.
'''
?prop.table
red_men <- prop.table(HairEyeColor[,'Blue','Male'])['Red']

'''
Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только у женщин из
таблицы HairEyeColor. По оси X должен идти цвет волос, цвет столбиков должен отражать цвет глаз. По оси Y - количество наблюдений.
'''

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
mydata2 <- subset(mydata, Sex == "Female") 
obj <- ggplot(data = mydata2, aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj

'''
Воспользуемся данными diamonds из библиотеки ggplot2. 
При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи качества огранки бриллианта (сut) и его цвета (color). 
В переменную main_stat сохраните значение статистики критерия Хи - квадрат. 
Обратите внимание, main_stat должен быть вектором из одного элемента, а не списком (листом).
'''
diamods_table <- table(diamonds$cut, diamonds$color) 
chi_result <- chisq.test(diamods_table ) 
main_stat <- chi_result$statistic 

'''
Опять воспользуемся данными diamonds из библиотеки ggplot2. При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи цены (price) и каратов (carat) бриллиантов. Для этого сначала нужно перевести эти количественные переменные в формат пригодный для Хи - квадрат. Создайте две новые переменные в данных diamonds:
factor_price - где будет 1, если значение цены больше либо равно чем среднее, и 0, если значение цены ниже среднего цены по выборке.
factor_carat - где будет 1, если число карат больше либо равно чем среднее,  и 0, если ниже среднего числа карат по выборке.
Важный момент - на больших данных цикл for() работает довольно медленно, постарайтесь решить эту задачу без его использования!
Используя эти шкалы при помощи Хи - квадрат проверьте исходную гипотезу. Сохраните в переменную main_stat значение критерия  Хи - квадрат.
'''
diamonds$factor_price <- factor(ifelse(diamonds$price >= mean(diamonds$price), 1, 0)) 
diamonds$factor_carat <- factor(ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)) 
main_stat<- chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic 

main_stat <- chisq.test(as.integer
                  (diamonds$price >= mean(diamonds$price)), 
                  as.integer(diamonds$carat >= mean(diamonds$carat)))$statistic
main_stat

'''
При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа коробки передач (am) и типа двигателя (vs) в данных mtcars. Результат выполнения критерия сохраните в переменную.Получившийся p - уровень значимости сохраните в переменную fisher_test.
'''
fisher_test <- fisher.test(mtcars$vs, mtcars$am)$p
fisher_test

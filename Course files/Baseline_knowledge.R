library(help = "datasets")

my_data <- mtcars

#В этой задче поработаем со встроенными данными mtcars. В датафрэйме mtcars создайте новую колонку (переменную) 
# под названием even_gear, в которой будут единицы, если значение переменной (gear) четное, и нули если количество нечетное.   
mtcars$even_gear <- ifelse(mtcars$gear %% 2 ==1,0,1)

#Продолжим нашу работу с данными mtcars. 
#Теперь ваша задача создать переменную - вектор mpg_4 и сохранить в нее значения расхода топлива (mpg) для машин с четырьмя цилиндрами (cyl).

mpg_4 <- mtcars$mpg[mtcars$cyl == 4]

# А теперь научимся отбирать только некоторые строчки из исходных данных. 

#Ваша задача создать новый dataframe под названием mini_mtcars, 
#в котором будут сохранены только третья, седьмая, десятая, двенадцатая и последняя строчка датафрейма mtcars.

mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)), ]

'''
Создайте новую числовую переменную  new_var в данных mtcars, которая содержит единицы в строчках, 
если в машине не меньше четырёх карбюраторов (переменная "carb") или больше шести цилиндров (переменная "cyl"). 
В строчках, в которых условие не выполняется, должны стоять нули.
'''
mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)

'''
Для встроенных в R данных AirPassengers хранится 144 значения (количество пассажиров в месяц) с 1949 по 1960 год. Данные Time-Series очень похожи на вектор по своей структуре, например мы можем обратиться к любому из 144 элементов используя уже знакомую нам индексацию AirPassengers[1] или AirPassengers[56].

Можно вообще перевести исходные данные в вектор при помощи команды as.vector(AirPassengers) и продолжить с ними работу как с вектором.

И так ваша задача создать переменную good_months и сохранить в нее число пассажиров только в тех месяцах, в которых это число больше, чем показатель в предыдущем месяце.  

Важный момент! В R оператор : для создания последовательности имеет приоритет над арифметическими действиями. Таким образом, если у вас есть переменная i, равная 10, и вы хотите создать вектор от 1 до i - 1, воспользуйтесь скобками, чтобы указать последовательность действий.
'''
good_months <- AirPassengers[c(FALSE, AirPassengers[2:144] > AirPassengers[1:143])]

'''
Задачка  для супер героев, повышенной сложности!

Для встроенных в R данных AirPassengers рассчитайте скользящее среднее с интервалом сглаживания равным 10. Напечатайте получившийся результат (первым значением в выводе должно быть среднее для элементов 1:10, во втором значении - среднее для элементов 2:11 и т.д., в последнем  - среднее для элементов 135 :144)

Все полученные значения средних сохраните в переменную moving_average.
'''
moving_average <- numeric(135)
index <- 0

for (i in 1:135){
  moving_average[i] <- mean(AirPassengers[(1:10)+index])
  index <- index + 1
}
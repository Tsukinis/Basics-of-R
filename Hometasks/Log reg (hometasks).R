'''
Используем данные mtcars. Сохраните в переменную логистическую регрессионную модель, где в качестве зависимой переменной выступает тип коробки передач (am), в качестве предикторов переменные disp, vs, mpg.

Значения коэффициентов регрессии сохраните в переменную log_coef.
'''
log_coef <- glm(am ~ disp+vs+mpg, data = mtcars, family = "binomial")$coefficients

'''
Дополните предложенный в задании код, чтобы построить следующий график по данным ToothGrowth.
Изобразите различия длины зубов морских свинок в различных условиях дозировки и типа потребляемого продукта.

По оси x - переменная supp.
По оси y - переменная len.
Цвет ящиков с усами (boxplot) - переменная dose.
'''
library("ggplot2")

obj <- ggplot(ToothGrowth, aes(supp, len, fill = as.factor(df$dose)))+
  geom_boxplot()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))


df <- read.csv('~/RScripts/Datasets/data.csv')
df$admit <− factor(df$admit)
dfrank <− factor(df$rank)
fit_df<- glm(admit ~ rank * gpa, df, family = "binomial")
df_na <- subset(df, is.na(df$admit) == TRUE)
df_na$prob <- predict(fit_df, newdata = df_na, type = "response")

df_na$prob_final <- ifelse(df_na$prob > 0.4, 1, 0)
sum(df_na$prob_final)

my_data <- ToothGrowth

tooth_aov <- aov(len~supp + dose + supp:dose, data = my_data)
qqnorm(resid(tooth_aov))
qqline(resid(tooth_aov))

boxplot(resid(tooth_aov)~my_data$supp)
boxplot(resid(tooth_aov)~my_data$dose)


library(ggplot2)
ggplot(my_data, aes(x=dose, y=len)) +
  geom_boxplot() +
  facet_wrap(~supp)

e <- ggplot(ToothGrowth, aes(x = dose, y = len))
e + geom_boxplot()

summary(tooth_aov)

TukeyHSD(tooth_aov, which = "dose")


library("ggpubr")
ggboxplot(my_data, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))

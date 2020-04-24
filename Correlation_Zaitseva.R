#Самостоятельная работа по корреляции данных
#Автор: Зайцева Елена Валерьевна, группа СЛАмд-01-18

data = read.csv("data.csv")
summary(data)

#Задание:
#для  ночных данных за лето 2019 года, постройте регрессионную зависимость скорости сокотечения (переменная Flux) 
#для деревьев вида с низкой антропогенной нагрузкой

data_filtered_1 = data[data$doy > 151 & data$doy < 243,]
data_filtered_2 = data_filtered_1[data_filtered_1$hour > 19 | data_filtered_1$hour < 7,]
data_filtered_3 = data_filtered_2[data_filtered_2$in_site_antrop_load == "Low",]



data_filtered_3=select(data_filtered_3,-c("id", "Species","age_group_index","time", "antrop_load",
                                          "in_site_antrop_load") )

library(dplyr)

#Корреляция

corr = cor(data_filtered_3,use = "na.or.complete")^2
##corr=corr[corr>0.49]
#install.packages("ggcorrplot")
library(ggcorrplot)

#Построение графика корреляции

ggcorrplot(corr,
           tl.cex=4
)
flux_corr=corr[ ,"Flux"]
flux_corr=flux_corr[flux_corr>0.1]
flux_corr



formula8 = Flux ~ u+rh
formula9 = Flux ~ u+rh + u:rh

model8 = lm(data=data_filtered_3, formula8)
model9 = lm(data=data_filtered_3, formula9)

summary(model8)
summary(model9)

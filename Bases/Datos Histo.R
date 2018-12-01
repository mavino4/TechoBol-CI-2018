library("ggplot2")


##### 2014
p2014 <- c(2534917,8197916)
v2014 <- c(2226508,694884) 


##### 2015 
p2015<- c(2275277,8638109)
v2015 <- c(2374029,  638412) 


##### 2016
p2016<- c(2050601,9005129)
v2016 <- c(2591572,  617678) 


### 2017
p2017 <- c(1984191,9232081)
v2017 <- c(2707021, 640077) 


personas<- matrix(c(p2014, p2015 , p2016 , p2017),2,4)
vivienda <-matrix(c(v2014, v2015, v2016, v2017),2,4)
years <- c(2014, 2015, 2016, 2017)
personas <- as.data.frame(personas)
personas <- transpose(personas)
row.names(personas) <- years
names(personas) <- c("1", "0")

vivienda <- as.data.frame(vivienda)
vivienda <-transpose(vivienda)
row.names(vivienda) <- years
names(vivienda) <- c("0", "1")


vivienda = vivienda/ rowSums(vivienda)
personas = personas / rowSums(personas)

###  Generando Gráficas

ggplot(personas, aes(x = years, y = personas$`0`)) + geom_area(position = "stack", fill = "steelblue3", alpha= 0.9)+ 
  geom_area(aes(x=years, y=personas$`1`), position = "stack", alpha= 1, fill = "coral2") +
  theme(legend.position = "rigth") + labs(x="Años", y = "Porcentaje", title = "Evolución personas sin\ncondiciones mínimas de vivienda", 
                                          subtitle="(en porcentajes)", caption = "Elaboración propia en base a la encuesta de hogares INE") 
ggsave("EvoluciónRelativaPersonas.png",width = 5.5, height = 4.5)  

ggplot(vivienda, aes(x = years, y = vivienda$`0`)) + geom_area(position = "stack", fill = "steelblue3", alpha= 0.9)+ 
  geom_area(aes(x=years, y=vivienda$`1`), position = "stack", alpha= 1, fill = "coral2") +
  theme(legend.position = "rigth") + labs(x="Año", y = "Porcentaje", title = "Evolución hogares sin\ncondiciones mínimas de vivienda", 
                                          subtitle="(en porcentajes)", caption = "Elaboración propia en base a la encuesta de hogares INE") 
ggsave("EvoluciónRelativaHogares.png",width = 5.5, height = 4.5)



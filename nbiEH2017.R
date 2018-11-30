library(readr)
require(data.table)
library("ggplot2")

setwd("/home/mavino/Documents/TECHO/TechoBol-CI-2018/")

vivienda <- read_delim("CSV/EH2017_Vivienda.csv",
                       ";", escape_double = FALSE, trim_ws = TRUE)


personas <- read_delim("CSV/EH2017_Persona.csv",
                       ";", escape_double = FALSE, trim_ws = TRUE)
personas <- personas[,c("folio","nro","area", "niv_ed" , "condact" , "s02a_02")]
personas <- as.data.table(personas)
personas <- personas[personas[, .I[nro == max(nro)], by=folio]$V1]

vivienda <- merge(vivienda, personas, by = "folio")

table(vivienda$s01a_06)
table(vivienda$s01a_07)


# Puntajes para los muros
vivienda$punMuro <-0
table(vivienda$punMuro)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s01a_06 == 1 ,1.5) 
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s01a_06 == 2 & vivienda$s01a_07 == 2  ,0.5)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s01a_06 == 2 & vivienda$s01a_07 == 1  ,1)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s01a_06 == 3 & vivienda$s01a_07 == 2  ,0.5)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s01a_06 == 3 & vivienda$s01a_07 == 1  ,1)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s01a_06 == 4 ,0.5)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s01a_06 == 5 ,1)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s01a_06 == 6 ,0.5)
table(vivienda$punMuro)

# Puntaje para los techos
vivienda$punTecho <-0  
vivienda$punTecho = replace(vivienda$punTecho, vivienda$s01a_08 == 1 , 1)
vivienda$punTecho = replace(vivienda$punTecho, vivienda$s01a_08 == 2 | vivienda$s01a_08 == 3  , 1.5)
vivienda$punTecho = replace(vivienda$punTecho, vivienda$s01a_08 == 4 , 0.5)
table(vivienda$punTecho)

# Puntaje para el piso
vivienda$punPiso <- 0
vivienda$punPiso = replace(vivienda$punPiso, vivienda$s01a_09 == 5 | vivienda$s01a_09 == 7, 1  )
vivienda$punPiso = replace(vivienda$punPiso, vivienda$s01a_09 == 2 | vivienda$s01a_09 == 3 | vivienda$s01a_09 == 4 | vivienda$s01a_09 == 6, 2  )
table(vivienda$punPiso)

vivienda$nbiPiso = 1 - vivienda$punPiso 
vivienda$nbiTecho = 1 - vivienda$punTecho 
vivienda$nbiMuro = 1 - vivienda$punMuro

vivienda$nbiVivienda = (vivienda$nbiMuro + vivienda$nbiPiso + vivienda$nbiTecho)/3
table(vivienda$nbiVivienda)
vivienda$nbiViviendaBinario = 0
vivienda$nbiViviendaBinario = replace(vivienda$nbiViviendaBinario, vivienda$nbiVivienda > 0, 1)
table(vivienda$nbiViviendaBinario)

# datos de vivienda 
expandido <- vivienda[rep(seq_len(nrow(vivienda)), vivienda$factor),]

#Distribución viviendas sin condiciones básicas
table(expandido$nbiViviendaBinario)

#Distribición personas sin condiciones básicas en la vivienda 
sum(expandido$nro[expandido$nbiViviendaBinario ==1])
sum(expandido$nro[expandido$nbiViviendaBinario ==0])
sum(expandido$nro)

distPer <- as.data.frame(table(expandido$nro, expandido$nbiViviendaBinario) )
distPer


ggplot(expandido,aes(x=nro)) + 
  geom_histogram(data=subset(expandido,nbiViviendaBinario == 0),fill = "blue", alpha = 0.2) +
  geom_histogram(data=subset(expandido,nbiViviendaBinario == 1),fill = "red", alpha = 0.3)

ggsave("Ditribución Hogares nbiVivienda.png")

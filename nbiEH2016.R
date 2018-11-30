library(readr)
require(data.table)
library(haven)
setwd("/home/mavino/Documents/TECHO/TechoBol-CI-2018/Bases")

#trabajando con personas 
personas <- read_sav("2016/EH2016_Persona.sav")
names(personas)

## Extrayendo el número de personas por vivienda
personas <- personas[,c("folio","nro","area", "niv_ed" , "condact" , "s02a_02")]
personas <- as.data.table(personas)
personas <- personas[personas[, .I[nro == max(nro)], by=folio]$V1]
head(personas)


#trabajando con vivienda
vivienda <- read_sav("2016/EH2016_Vivienda.sav")
names(vivienda)
vivienda <- merge(vivienda, personas, by = "folio")


# Puntajes para los muros
# Asignando puntajes según las categorías definidas en la metodología nbi
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
# Asignando puntajes según las categorías definidas en la metodología nbi
vivienda$punTecho <-0  
vivienda$punTecho = replace(vivienda$punTecho, vivienda$s01a_08 == 1 , 1)
vivienda$punTecho = replace(vivienda$punTecho, vivienda$s01a_08 == 2 | vivienda$s01a_08 == 3  , 1.5)
vivienda$punTecho = replace(vivienda$punTecho, vivienda$s01a_08 == 4 , 0.5)
table(vivienda$punTecho)

# Puntaje para el piso
# Asignando puntajes según las categorías definidas en la metodología nbi
vivienda$punPiso <- 0
vivienda$punPiso = replace(vivienda$punPiso, vivienda$s01a_09 == 5 | vivienda$s01a_09 == 7, 1  )
vivienda$punPiso = replace(vivienda$punPiso, vivienda$s01a_09 == 2 | vivienda$s01a_09 == 3 | vivienda$s01a_09 == 4 | vivienda$s01a_09 == 6, 2  )
table(vivienda$punPiso)


#Calculando el puntaje
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





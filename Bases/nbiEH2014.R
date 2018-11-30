library(readr)
require(data.table)
library(foreign)

setwd("/home/mavino/Documents/TECHO/TechoBol-CI-2018/Bases")

#trabajando con personas 

personas <- read.spss("2014/EH2014_Persona.sav")
personas <- as.data.frame(personas)

names(personas)

## Extrayendo el número de personas por vivienda

personas <- personas[,c("folio","nro")]
personas <- as.data.table(personas)
personas <- personas[personas[, .I[nro == max(nro)], by=folio]$V1]
head(personas)

#trabajando con vivienda
vivienda <- read.spss("2014/EH2014_Vivienda.sav")
vivienda <- as.data.frame(vivienda)
names(vivienda)
vivienda$folio <- vivienda$Folio
vivienda <- merge(vivienda, personas, by = "folio")

# Puntajes para los muros
# Asignando puntajes según las categorías definidas en la metodología nbi
vivienda$punMuro <-0
table(vivienda$punMuro)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s1a_05 == "LADRILLO/BLOQUES DE CEMENTO/HORMIGON" ,1.5) 
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s1a_05 == "ADOBE/TAPIAL" & vivienda$s1a_06 == "No"  ,0.5)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s1a_05 == "ADOBE/TAPIAL" & vivienda$s1a_06 == "Si"  ,1)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s1a_05 == "TABIQUE/QUINCHE" & vivienda$s1a_06 == "No"  ,0.5)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s1a_05 == "TABIQUE/QUINCHE" & vivienda$s1a_06 == "Si"  ,1)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s1a_05 == "PIEDRA" ,0.5)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s1a_05 == "MADERA" ,1)
vivienda$punMuro = replace(vivienda$punMuro,vivienda$s1a_05 == "CAÑA/PALMA/TRONCO" ,0.5)
table(vivienda$punMuro)

# Puntaje para los techos
# Asignando puntajes según las categorías definidas en la metodología nbi
vivienda$punTecho <-0  
vivienda$punTecho = replace(vivienda$punTecho, vivienda$s1a_07 == "CALAMINA O PLANCHA" , 1)
vivienda$punTecho = replace(vivienda$punTecho, vivienda$s1a_07 == "TEJA (CEMENTO/ARCILLA/FIBROCEMENTO)" | vivienda$s1a_07 == "LOSA DE HORMIGÓN ARMADO"  , 1.5)
vivienda$punTecho = replace(vivienda$punTecho, vivienda$s1a_07 == "PAJA/CAÑA/PALMA/BARRO" , 0.5)
table(vivienda$punTecho)
table(vivienda$s1a_07)


# Puntaje para el piso
# Asignando puntajes según las categorías definidas en la metodología nbi
vivienda$punPiso <- 0
vivienda$punPiso = replace(vivienda$punPiso, vivienda$s1a_08 == "CEMENTO" | vivienda$s1a_08 == "LADRILLO", 1  )
vivienda$punPiso = replace(vivienda$punPiso, vivienda$s1a_08 == "TABLÓN DE MADERA" | vivienda$s1a_08 == "MACHIHEMBRE/PARQUET" | vivienda$s1a_08 == "ALFOMBRA/TAPIZÓN" | vivienda$s1a_08 == "MOSAICO/BALDOSAS/CERÁMICA", 2  )
table(vivienda$punPiso)
table(vivienda$s1a_08)


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
sum(expandido$nro)


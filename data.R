library(readr)
require(data.table)
setwd("/media/marco/165d5eb4-ca0f-4d82-a5d6-3052d1663348/TECHO/DatosCamp")
personas <- read_delim("CSV/EH2017_Persona.csv",
                       ";", escape_double = FALSE, trim_ws = TRUE)
vivienda <- read_delim("CSV/EH2017_Vivienda.csv",
                       ";", escape_double = FALSE, trim_ws = TRUE)
names(personas)
personas <- as.data.table(personas)

## Quedandonos solamente con los jefes de hogar
personas <- personas[,c("folio","nro","area", "niv_ed" , "condact" , "s02a_02")]
personas <- as.data.table(personas)
personas <- personas[personas[, .I[nro == max(nro)], by=folio]$V1]

total <- merge(vivienda, personas, by = "folio")





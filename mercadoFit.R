library(readr)
require(data.table)
setwd("/home/mavino/Documents/TECHO/TechoBol-CI-2018/")


personas <- read_delim("CSV/EH2017_Persona.csv",
                       ";", escape_double = FALSE, trim_ws = TRUE)

personas <- personas[,c("folio", "factor","nro","area", "niv_ed" , "condact" , "s02a_02" , "s02a_03", "s04a_05a","s04a_05b",
                        "s04a_05c","s04a_05d", "s04a_05e", "s04e_28a" ,      "s04e_28b",       "s04e_29a"  ,     "s04e_29b"    ,  
                        "s04e_30a",       "s04e_30b" ,      "s04e_30c"  ,     "s04e_31a"  ,     "s04e_31b"   ,    "s04e_31c"  ,     "s04e_31d",      
                        "s04e_31e",   "s04e_31f", "s05c_16aa"   ,   "s05c_16ab"    ,  "s05c_16e")]

personas <- as.data.table(personas)


expandido <- personas[rep(seq_len(nrow(personas)), personas$factor),]
table(expandido$nbiViviendaBinario)

table(personas$s02a_02)
table(expandido$s02a_03)

library(ggplot2)


## distribución de edades
ggplot(data=expandido, aes(x=s02a_03 )) +
  geom_bar(stat="bin")


# Script que prepara los aranceles para pegarlos a la base procesada
rm(list=ls())
library(data.table)

# Para remover espacios en blanco
library(stringr)

# Modificar WD a conveniencia

setwd('/Users/fabiangarcia/Documents/BBVA/Comercio')

partidas <-read.csv(file = 'Correlativa/TOTPART62.csv', colClasses = c('character', 'numeric', rep('character',7 ) ), header = T )

# Reemplazo espacios por nada
partidas$subpartida <- str_replace_all(partidas$subpartida, fixed(" "), "")

# Quito las filas vacías
partidas <- partidas[!is.na(partidas$year), ]

partidasdt <- data.table(partidas)

partidas <- as.data.frame(partidasdt[,.SD[which.max(year)], by = c('subpartida')])
           
save(partidas, file = 'Correlativa/partidas.Rda')

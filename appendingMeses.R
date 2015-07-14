rm(list=ls())

# Script que une toda la data
# Modificar WD a conveniencia
setwd('/Users/fabiangarcia/Documents/BBVA/Comercio/Total/')

# Cambiar ano
year = "2015"

# Cargamos el ultimo mes
load("2015_05.Rda")
XX <- data

for ( n in seq(2007, 2015) ) {
  for (m in seq(1,12) ) {
    if (m < 10) {
      ruta <- (paste0(n,"_0",m,".Rda"))
    } else {
      ruta <- (paste0(n,"_",m,".Rda"))
    }
   
    if (n == 2015 & m == 6) {
      break
    }
    load(ruta)
    XX <- rbind(XX,data)
  } 
}

# Finalmente salvamos la data
save(XX, file= "xx.Rda")

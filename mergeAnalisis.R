# Script que une la base de datos de comercio con la información de las otras partidas arancelarias

rm(list=ls())
library(data.table)
library(zoo)
  
# Modificar WD a conveniencia
  
setwd('/Users/fabiangarcia/Documents/BBVA/Exportaciones')

# Cargo la base de comercio
load("Total/xx.Rda")

# Cargo la base de correlativas
load("Correlativa/partidas.Rda")
  
setnames(xx,"NANDINA", "subpartida") 
  
xx_cuci <- merge(x = xx, y = partidas, by = "subpartida", all = T)

save(xx_cuci, file = "Total/xxCorrelativas.Rda")
  
  
  
# Detectamos las partidas que no pudimos clasificar
sinClasificar <- xx_cuci[!is.null(xx_cuci$cuode), ]
  
  # Partidas sin clasificar. Ya clasificadas. Yohooo!!!
  # 2106906100
  # 2106906900
  # 7213919010
  # 7321909010
  # 7321909090
  # 8516800010
  # 8516800090
  # 8516900090
  # 8543709010
  # 8543709090
  
  
  # Creacion de grupos
  library(dplyr)
  library(data.table)
  
  # Para las fechas
  library(lubridate)
  library(zoo)

################################################################
#### PEDIDO DE MARIA C #########################################

carros <- xx_cuci[substr(xx_cuci$subpartida,1,2) == 87,]

# Partidas 1 a la 5
carros$mc <- NULL
carros$mc[substr(carros$subpartida,3,4) == "01"] = "1 a 5" 
carros$mc[substr(carros$subpartida,3,4) == "02"] = "1 a 5" 
carros$mc[substr(carros$subpartida,3,4) == "03"] = "1 a 5" 
carros$mc[substr(carros$subpartida,3,4) == "04"] = "1 a 5" 
carros$mc[substr(carros$subpartida,3,4) == "05"] = "1 a 5" 

# Partidas 8
carros$mc[substr(carros$subpartida,3,4) == "08"] = "8"

# Partida 16
carros$mc[substr(carros$subpartida,3,4) == "16"] = "16"

# Partida 11
carros$mc[substr(carros$subpartida,3,4) == "11"] = "11"

# Partida 14
carros$mc[substr(carros$subpartida,3,4) == "14"] = "14"

carros <- carros[!is.null(carros$mc),]

# Creacion de ano y mes
carros$m <- month(carros$date)
carros$y <- year(carros$date)

# Salidas
by_my <- group_by(carros, y, m, mc)

salida <- summarise(by_my,
                    valor = sum(FOBDOL),
                    peso = sum(as.numeric(PESOBRKI)),
                    cantidades = sum(as.numeric(NARTIC)))

salida <- salida[!is.na(salida$y),]
salida <- salida[!is.na(salida$mc),]



write.csv(salida, file = "CarrosMC.csv")

################################################################
################################################################
# Para las de agricultura
xx_cuci$grupo <- ''

# Primero Flores
xx_cuci$grupo[xx_cuci$subpartida == '0603110000'] <- "Flores"
xx_cuci$grupo[xx_cuci$subpartida == '0603121000'] <- "Flores"
xx_cuci$grupo[xx_cuci$subpartida == '0603141000'] <- "Flores"
xx_cuci$grupo[xx_cuci$subpartida == '0603129000'] <- "Flores"
xx_cuci$grupo[xx_cuci$subpartida == '0603199000'] <- "Flores"

# Ferroniquel
xx_cuci$grupo[xx_cuci$subpartida == '7202600000'] <- "Ferroníquel"

# Carbón
xx_cuci$grupo[xx_cuci$subpartida == '2701120010'] <- "Carbon"
xx_cuci$grupo[xx_cuci$subpartida == '2704001000'] <- "Ferroníquel"

# Oro
xx_cuci$grupo[xx_cuci$subpartida == '7108120000'] <- "Oro"

# Bananas
xx_cuci$grupo[xx_cuci$subpartida == '0803901100'] <- "Bananas"

# Filtramos el grupo final
pibOferta <- xx_cuci[xx_cuci$grupo != '',]

# Creacion de ano y mes
pibOferta$m <- month(pibOferta$date)
pibOferta$y <- year(pibOferta$date)

# Salidas
by_my <- group_by(pibOferta, y, m, grupo)

salida <- summarise(by_my,
                    valor = sum(FOBDOL),
                    peso = sum(as.numeric(PESOBRKI)) )
                    


salida$my <- paste0(salida$y,'-', salida$m)
salida$my[nchar(salida$m) == 1] <- paste0( salida$y[nchar(salida$m) == 1],'-', '0', salida$m[nchar(salida$m) == 1] )
salida$my <- as.yearmon(salida$my)

# Grafica
library(ggplot2)
source("/Users/fabiangarcia/Dropbox/1_DATA_BBVA/comercio/theme_fivethirtyeight.R")
ggplot(data = salida, aes(x = as.POSIXct(my), y = peso/(10^6) ))  + geom_line() + 
  facet_wrap(~grupo,  scales = "free") + ggtitle("Dinámica de algunas exportaciones") +  # scales = "free" esto dentro del facet_wrap
  geom_line(size = 1.2, color = '#008FD5') +  
  theme_fivethirtyeight() 


  scale_x_datetime( breaks = "1 year", minor_breaks = "1 month", labels = date_format("%y"))













  
  # Creacion de grupo con los primeros dos códigos de cuciDos
  dataUnida <- mutate(dataUnida, cuciDos = substr(cuci,1,2))
  
  dataUnida <- data.table(dataUnida)
  
  # Creacion de Fecha y mes
  dataUnida$mes <- as.yearmon(dataUnida$date)
  
  
  
  
  # Subimos la base de cuci para las grandes categorias
  cuci <- read.csv("cuci.csv", colClasses = c(rep('character', 3) ) )
  cuci <- data.table(cuci)
  
  dataUnidaF <- merge(x = dataUnida, y = cuci, by = "cuciDos", all = T)
  
  # Reemplazamos las otras categorías. Las 891 no van en manufacturas sino en otros
  # Lo mismo que el capítulo 9 del CUCI
  dataUnidaF[grep("^891", dataUnidaF$cuci ), ]$grupo <- "Otros"
  dataUnidaF[grep("^9", dataUnidaF$cuci ), ]$grupo <- "Otros"
  
  # Este Data FRAME me sirve para verificar la suma. De acuerdo a lo reportado por el DANE
  agrega <- group_by(dataUnidaF, mes, grupo )
  agrega$grupo <- as.factor(agrega$grupo)
  
  aja <- summarise(agrega, valor = sum(FOBDOL))
  
  
  # Gráficas
  library(ggplot2)
  library(scales)
  #library(plyr)
  source('/Users/fabiangarcia/Dropbox/1_DATA_BBVA/empleotrends/scripts/theme_fivethirtyeight.R')


# Los 3 grandes grupos
ggplot(data = aja, aes(x = as.POSIXct(mes), y = valor/(10^6) ))  + geom_line() + 
  facet_wrap(~grupo,  scales = "free") + ggtitle("Exportacionesa a mayo de 2015 (millones de USD)") +  # scales = "free" esto dentro del facet_wrap
  geom_line(size = 1.2, color = '#008FD5') +  
  theme_fivethirtyeight() +
  scale_x_datetime( breaks = "1 year", minor_breaks = "1 month", labels=date_format("%Y"))


# Primer Grupo Alimenticios
alimenticiosBas <- filter(dataUnidaF, cuciDos == '07' |
                         cuciDos == '05' |
                         cuciDos == '06' |
                         cuciDos == '04' |
                         cuciDos == '03' |
                         cuciDos == '09' |
                         cuciDos == '00' |
                         cuciDos == '08' |
                         cuciDos == '01' |
                         cuciDos == '02' )
                         
                       
agrega <- group_by(alimenticiosBas, mes, descrCuci )


aja <- summarise(agrega, valor = sum(FOBDOL))

# Para poner en dos líneas los títulos
wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=40),collapse=" \n ")
  return(wtext)
}

aja$descrCuci <- lapply(aja$descrCuci, wrapit)
aja$descrCuci <- unlist(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci, levels =
                             c("Café, té, cacao, especias y sus \n preparados",
                               "Legumbres y frutas" ,
                               "Azúcares, preparados de azúcar y miel",
                               "Cereales y preparados de cereales",
                               "Pescado (no incluídos los mamíferos \n marinos), crustáceos, moluscos e \n invertebrados acuáticos y sus \n preparados",
                               "Productos y preparados comestibles \n diversos" ,
                               "Animales vivos no incluídos en el \n capítulo 03"  ,
                               "Pienso para animales (excepto cereales \n sin moler)" ,
                               "Carne y preparados de carne" ,
                               "Productos lácteos y huevos de aves"))


ggplot(data = aja, aes(x = as.POSIXct(mes), y = valor/(10^6) ))  + geom_line() + 
  facet_wrap(~descrCuci,  scales = "free") + ggtitle("Exportaciones de alimentos básicos a Mayo de 2015 (millones de USD)") +  # scales = "free" esto dentro del facet_wrap
  geom_line(size = 1.2, color = '#008FD5') +  
  theme_fivethirtyeight() +
  scale_x_datetime( breaks = "1 year", minor_breaks = "1 month", labels=date_format("%y"))



# Alimentos más elaborados
aliElaborados<- filter(dataUnidaF, cuciDos == '11' |
                         cuciDos == '12' |
                         cuciDos == '21' |
                         cuciDos == '22' |
                         cuciDos == '23' |
                         cuciDos == '24' |
                         cuciDos == '25' |
                         cuciDos == '26' |
                         cuciDos == '29' |
                         cuciDos == '41' |
                         cuciDos == '42' |
                         cuciDos == '43' )

agrega <- group_by(aliElaborados, mes, descrCuci )
agrega$descrCuci <- as.factor(agrega$descrCuci)

aja <- summarise(agrega, valor = sum(FOBDOL))

aja$descrCuci <- as.character(aja$descrCuci)
aja$descrCuci <- lapply(aja$descrCuci, wrapit)
aja$descrCuci <- unlist(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci, levels = 
                          c("Productos animales y vegetales en \n bruto, n.e.p.",
                            "Aceites y grasas fijos de origen \n vegetal, en bruto, refinados o \n fraccionados",
                            "Tabaco y sus productos",
                            "Cueros, pieles y pieles finas, sin \n curtir",
                            "Bebidas",
                            "Corcho y madera",
                            "Aceites y grasas de origen animal o \n vegetal, elaborados; ceras de origen \n animal o vegetal; mezclas o preparados \n no comestibles de grasas o aceites de \n origen animal o vegetal, n.e.p.",
                            "Fibras textiles (excepto las mechas \n (tops) y otras formas de lana peinada) \n y sus desperdicios (no manufacturadas \n en hilados, hilos o tejidos)",
                            "Semillas y frutos oleaginosos",
                            "Caucho en bruto (incluso el caucho \n sintético y regenerado)",
                            "Pasta y desperdicios de papel" ,
                            "Aceites y grasas de origen animal" ))


ggplot(data = aja, aes(x = as.POSIXct(mes), y = valor/(10^6) ))  + geom_line() + 
  facet_wrap(~descrCuci,  scales = "free") + ggtitle("Exportaciones de alimentos elaborados a Mayo de 2015 (millones de USD)") +  # scales = "free" esto dentro del facet_wrap
  geom_line(size = 1.2, color = '#008FD5') +  
  theme_fivethirtyeight() +
  scale_x_datetime( breaks = "1 year", minor_breaks = "1 month", labels=date_format("%y"))



# Combustibles

combustibles <- filter(dataUnidaF, cuciDos == '27' |
                       cuciDos == '28' |
                       cuciDos == '32' |
                       cuciDos == '33' |
                       cuciDos == '34' |
                       cuciDos == '35' |
                       cuciDos == '68' )

agrega <- group_by(combustibles, mes, descrCuci )
agrega$descrCuci <- as.factor(agrega$descrCuci)

aja <- summarise(agrega, valor = sum(FOBDOL))

aja$descrCuci <- as.character(aja$descrCuci)
aja$descrCuci <- lapply(aja$descrCuci, wrapit)
aja$descrCuci <- unlist(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci, levels = 
                          c("Petróleo, productos derivados del \n petróleo y productos conexos",
                            "Hulla, coque y briquetas",
                            "Menas y desechos de metales",
                            "Gas natural y manufacturado",
                            "Corriente eléctrica",
                            "Metales no ferrosos",
                            "Abonos en bruto, excepto los del \n capítulo 56, y minerales en bruto \n (excepto carbón, petróleo y piedras \n preciosas)"))


ggplot(data = aja, aes(x = as.POSIXct(mes), y = valor/(10^6) ))  + geom_line() + 
  facet_wrap(~descrCuci,  scales = "free") + ggtitle("Exportaciones de combustibles a Mayo de 2015 (millones de USD)") +  # scales = "free" esto dentro del facet_wrap
  geom_line(size = 1.2, color = '#008FD5') +  
  theme_fivethirtyeight() +
  scale_x_datetime( breaks = "1 year", minor_breaks = "1 month", labels=date_format("%y"))


# Manufacturas. Primer Grupo
manufacturas1 <- filter(dataUnidaF, cuciDos == '57' |
                          cuciDos == '55' |
                          cuciDos == '67' |
                          cuciDos == '84' |
                          cuciDos == '54' |
                          cuciDos == '59' |
                          cuciDos == '77' |
                          cuciDos == '64' |
                          cuciDos == '89' )

agrega <- group_by(manufacturas1, mes, descrCuci )
agrega$descrCuci <- as.factor(agrega$descrCuci)

aja <- summarise(agrega, valor = sum(FOBDOL))

aja$descrCuci <- as.character(aja$descrCuci)
aja$descrCuci <- lapply(aja$descrCuci, wrapit)
aja$descrCuci <- unlist(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci, levels = 
                          c("Plásticos en formas primarias",
                            "Aceites esenciales y resinoides y \n productos de perfumería; preparados de \n tocador y para pulir y limpiar" ,
                            "Hierro y acero",
                            "Prendas y accesorios de vestir",
                            "Productos medicinales y farmacéutico",
                            "Materias y productos químicos, n.e.p" ,
                            "Maquinaria, aparatos y artefactos \n eléctricos, n.e.p., y sus partes y \n piezas eléctricas (incluso las \n contrapartes no eléctricas, n.e.p., del \n equipo eléctrico de uso doméstico)",
                            "Papel, cartón y artículos de pasta de \n papel, de papel o de cartón",
                            "Artículos manufacturados diversos, \n n.e.p."))


ggplot(data = aja, aes(x = as.POSIXct(mes), y = valor/(10^6) ))  + geom_line() + 
  facet_wrap(~descrCuci,  scales = "free") + ggtitle("Exportaciones de manufacturas (I) a Mayo de 2015 (millones de USD)") +  # scales = "free" esto dentro del facet_wrap
  geom_line(size = 1.2, color = '#008FD5') +  
  theme_fivethirtyeight() +
  scale_x_datetime( breaks = "1 year", minor_breaks = "1 month", labels=date_format("%y"))



# Manufacturas. Segundo Grupo
manufacturas2 <- filter(dataUnidaF, cuciDos == '78' |
                          cuciDos == '66' |
                          cuciDos == '69' |
                          cuciDos == '65' |
                          cuciDos == '58' |
                          cuciDos == '61' |
                          cuciDos == '74' |
                          cuciDos == '51' |
                          cuciDos == '56' )

agrega <- group_by(manufacturas2, mes, descrCuci )
agrega$descrCuci <- as.factor(agrega$descrCuci)

aja <- summarise(agrega, valor = sum(FOBDOL))

aja$descrCuci <- as.character(aja$descrCuci)
aja$descrCuci <- lapply(aja$descrCuci, wrapit)
aja$descrCuci <- unlist(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci, levels = 
                          c("Vehículos de carretera (incluso \n aerodeslizadores)",
                            "Manufacturas de minerales no metálicos, \n n.e.p",
                            "Manufacturas de metales, n.e.p.",
                            "Hilados, tejidos, articulos \n confeccionados de fibras textiles, \n n.e.p., y productos conexos",
                            "Plásticos en formas no primarias" ,
                            "Cuero y manufacturas de cuero, n.e.p., \n y pieles finas curtidas",
                            "Maquinaria y equipo industrial en \n general, n.e.p., y partes y piezas de \n máquinas, n.e.p." ,
                            "Productos químicos orgánicos" ,
                            "Abonos (excepto los del grupo 272)"))


ggplot(data = aja, aes(x = as.POSIXct(mes), y = valor/(10^6) ))  + geom_line() + 
  facet_wrap(~descrCuci,  scales = "free") + ggtitle("Exportaciones de manufacturas (II) a Mayo de 2015 (millones de USD)") +  # scales = "free" esto dentro del facet_wrap
  geom_line(size = 1.2, color = '#008FD5') +  
  theme_fivethirtyeight() +
  scale_x_datetime( breaks = "1 year", minor_breaks = "1 month", labels=date_format("%y"))




# Manufacturas. Tercer Grupo
manufacturas3 <- filter(dataUnidaF, cuciDos == '72' |
                          cuciDos == '53' |
                          cuciDos == '87' |
                          cuciDos == '52' |
                          cuciDos == '82' |
                          cuciDos == '81' |
                          cuciDos == '71' |
                          cuciDos == '62' |
                          cuciDos == '76' )

agrega <- group_by(manufacturas3, mes, descrCuci )
agrega$descrCuci <- as.factor(agrega$descrCuci)

aja <- summarise(agrega, valor = sum(FOBDOL))

aja$descrCuci <- as.character(aja$descrCuci)
aja$descrCuci <- lapply(aja$descrCuci, wrapit)
aja$descrCuci <- unlist(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci, levels = 
                          c("Maquinarias especiales para \n determinadas industrias",
                            "Materias tintóreas, curtientes y \n colorantes" ,
                            "Instrumentos y aparatos profesionales, \n científicos y de control, n.e.p." ,
                            "Productos químicos inorgánicos",
                            "Muebles y sus partes; camas, colchones, \n somieres, cojines y artículos rellenos \n similares",
                            "Edificios prefabricados; artefactos y \n accesorios sanitarios y para sistemas \n de conducción de aguas, calefacción y \n alumbrado, n.e.p.",
                            "Maquinaria y equipo generadores de \n fuerza",
                            "Manufacturas de caucho, n.e.p.",
                            "Aparatos y equipo para \n telecomunicaciones y para grabación y \n reproducción de sonido"))


ggplot(data = aja, aes(x = as.POSIXct(mes), y = valor/(10^6) ))  + geom_line() + 
  facet_wrap(~descrCuci,  scales = "free") + ggtitle("Exportaciones de manufacturas (III) a Mayo de 2015 (millones de USD)") +  # scales = "free" esto dentro del facet_wrap
  geom_line(size = 1.2, color = '#008FD5') +  
  theme_fivethirtyeight() +
  scale_x_datetime( breaks = "1 year", minor_breaks = "1 month", labels=date_format("%y"))


# Manufacturas. Cuarto Grupo
manufacturas4 <- filter(dataUnidaF, cuciDos == '85' |
                          cuciDos == '83' |
                          cuciDos == '63' |
                          cuciDos == '75' |
                          cuciDos == '79' |
                          cuciDos == '73' |
                          cuciDos == '88' )

agrega <- group_by(manufacturas4, mes, descrCuci )
agrega$descrCuci <- as.factor(agrega$descrCuci)

aja <- summarise(agrega, valor = sum(FOBDOL))

aja$descrCuci <- as.character(aja$descrCuci)
aja$descrCuci <- lapply(aja$descrCuci, wrapit)
aja$descrCuci <- unlist(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci)

aja$descrCuci <- factor(aja$descrCuci, levels = 
                          c("Calzado",
                            "Artículos de viajes, bolsos de mano y \n otros artículos análogos para contener \n objetos",
                            "Manufacturas de corcho y de madera \n (excepto muebles)",
                            "Máquinas de oficina y máquinas de \n procesamiento automático de datos" ,
                            "Otro equipo de transporte" ,
                            "Máquinas para trabajar metales" ,
                            "Aparatos, equipos y materiales \n fotográficos y artículos de óptica, \n n.e.p., relojes" ))


ggplot(data = aja, aes(x = as.POSIXct(mes), y = valor/(10^6) ))  + geom_line() + 
  facet_wrap(~descrCuci,  scales = "free") + ggtitle("Exportaciones de manufacturas (IV) a Mayo de 2015 (millones de USD)") +  # scales = "free" esto dentro del facet_wrap
  geom_line(size = 1.2, color = '#008FD5') +  
  theme_fivethirtyeight() +
  scale_x_datetime( breaks = "1 year", minor_breaks = "1 month", labels=date_format("%y"))


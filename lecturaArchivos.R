# Este es un script que lee los archivos planos de exportaciones del DANE
# Input: Arhivo plano del mes
# Output: Data Frame del mes, con columnas seleccionadas

rm(list=ls())
library(readr)

# Modificar WD a conveniencia. La data esta organizada por carpetas por año.
ruta <- "/Users/fabiangarcia/Documents/BBVA/Exportaciones/AVAN"

# Cambiar ano a conveniencia y correr
year = "2015"
ano = substr(year, 3, 4)

# Directorio
setwd(paste0(ruta,year))


# Definimos los nombres de nuestras variables
nombres <- c("FECHA_PRO", "NARTIC", "OFINCO", "CODADUA", "TID", "NIT", "TIP_USU", "COD_USU", 
             "TIP_EXP", "DPMPIO", "PAISDES", "PADES", "CIU_DES", "AUTO", "TIP_DEC", "COD_LUGSAL", 
             "SAC", "DEPPRO", "DEC_EXP", "FECH_EXP", "DEC_IMP", "FECH_IMP", "FINALIDAD", "MONEDA",
             "VIATRANS", "BANDERA", "REGIMEN", "MODALIDAD", "FORPAGO", "COD_EMB", "COD_DATO", 
             "CER_ORI", "SIESP", "EXP_TRAN", "NANDINA", "DEPORIG", "CODUNI", "UNIDAD", "CANTUNI", 
             "PESOBRKI", "PESONEKI", "FOBDOL", "FOBPESOS", "VAGPOS", "FLETES", "SEGUROS", "OTROSGAS",
             "CODADEMB", "FECH_EMB", "NDECLAR", "FECH_DECL", "RAZ_SOC", "DIR_EXP", "NIT_DECL", "RAZ_DEC",
             "RAZ_IMP", "DIR_IMP")


# Creamos funcion que carga el archivo y devuelve las columnas que nos interesan (PAIS DESTINO, VALOR FOB, PESO, y PARTIDA)
# y el nombre del archivo tiene en cuenta el nombre y el ano 
# del archivo

procesamiento <- function(archivo, ano, mes) {
  data <- read_fwf(archivo, fwf_widths( c(6,4,2,2,1,12,2,5,2,5,3,3,20,16,1,2,3,2,14,6,14,6,4,3,1,3,1,3,1,
                                             1,1,1,1,1,18,2,2,3,15,15,15,15,20,15,15,15,15,2,6,13,8,60,60,12,60,60,80), col_names = nombres),
                 col_types =  'nncnncncnnncccnncccncnccncncnccnncccncnnnnnnnnncncncccccc')
  name <- paste0(ano,"_",mes,".Rda")
  print(name)
  data <- data[, c("PAISDES", "NARTIC",  "FOBDOL", "PESOBRKI", "NANDINA")]
  data$date <- as.Date(paste(ano,mes,'01'), format = "%Y %m %d")
  data[nchar(data[,4])==9,4] <- paste("0", data[nchar(data[,4])==9,4], sep="") # Para uniformar el codigo de la partida
  data$NANDINA <- substr(data$NANDINA,1,10)
  save(file = name, x = data)
}

# Procesameniento mes a mes. Conveniente para ir pricesando el mes, a medida que vaya saliendo
procesamiento(paste0("M101", ano, ".AVA"), year, "01")
procesamiento(paste0("M102", ano, ".AVA"), year, "02")
procesamiento(paste0("M103", ano, ".AVA"), year, "03")
procesamiento(paste0("M104", ano, ".AVA"), year, "04")
procesamiento(paste0("M105", ano, ".AVA"), year, "05")
procesamiento(paste0("M106", ano, ".AVA"), year, "06")
procesamiento(paste0("M107", ano, ".AVA"), year, "07")
procesamiento(paste0("M108", ano, ".AVA"), year, "08")
procesamiento(paste0("M109", ano, ".AVA"), year, "09")
procesamiento(paste0("M110", ano, ".AVA"), year, "10")
procesamiento(paste0("M111", ano, ".AVA"), year, "11")
procesamiento(paste0("M112", ano, ".AVA"), year, "12")

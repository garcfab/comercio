#!/bin/bash


# Este script mueve los archivos procesados con el script lecturaArchivos.R a una sola carpeta para el posterior procesamiento


for ((a=2008; a <= 2015 ; a++))
do
   ruta="/Users/fabiangarcia/Documents/BBVA/Comercio/AVAN"
   ruta=$ruta$a
   echo $ruta
   cd $ruta      
   mv *.Rda ../Total
done

#==============================================================================#
# Autores: David Acero Acero: 201228148 
# Colaboradores:
# Fecha elaboracion: 25 de mayo de 2021
# Ultima modificacion: 25 de mayo de 2021
# Version de R: 4.0.3
#==============================================================================#
rm(list = ls()) # Limpiar el ambiente 
pacman::p_load(tidyverse,sf) #Cargar los paquetes necesarios 

# 1. Datos espaciales 

#1.1.1
via=st_read("data/input/VIAS.shp") #Cargar shp de VIAS
puntos=st_read("data/input/MGN_URB_TOPONIMIA.shp") #Cargar shp MGN_URB_TOPONIMIA

#1.1.2
c_medico=subset(puntos,CSIMBOL=="021001"|CSIMBOL=="021002"|CSIMBOL=="021003") #Crear c_medico a partir de puntos 

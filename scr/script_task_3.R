#==============================================================================#
# Autores: David Acero Acero: 201228148 
# Colaboradores:
# Fecha elaboracion: 25 de mayo de 2021
# Ultima modificacion: 25 de mayo de 2021
# Version de R: 4.0.3
#==============================================================================#
rm(list = ls()) # Limpiar el ambiente 
pacman::p_load(tidyverse,raster,sf,skimr,leaflet,ggspatial,outreg,margins,broom,xml2,XML,rvest) #Cargar los paquetes necesarios 

# 1.1 Datos espaciales 

#1.1.1
via=st_read("data/input/VIAS.shp") #Cargar shp de VIAS
puntos=st_read("data/input/MGN_URB_TOPONIMIA.shp") #Cargar shp MGN_URB_TOPONIMIA

#1.1.2
c_medico=subset(puntos,CSIMBOL=="021001"|CSIMBOL=="021002"|CSIMBOL=="021003") #Crear c_medico a partir de puntos 

#1.1.3
c_poblado=readRDS("data/input/c poblado (2017).rds") #Cargar centro poblado 
depto=readRDS("data/input/dp deptos (2017).rds")  #Cargar deptos
mapmuse=readRDS("data/input/victimas_map-muse.rds")  #Cargar map muse
c_poblado=subset(c_poblado,codmpio>=54001 & codmpio<55000) #Filtrar la base por el codigo de municipio
depto=subset(depto,name_dpto=="NORTE DE SANTANDER") #Seleccionar solo a norte de santander 
mapmuse=subset(mapmuse,cod_mpio>=54001 & cod_mpio<55000) #Filtrar la base por el codigo de municipio

# 1.2 Atributos de los objetos

#1.2
bases=ls()
for (i in 1:6) {
print(paste("*******************************Skim de",bases[i],"*******************************"))
print(skim(get(bases[i])))
} #Resumir todas las bases

#1.3 Geometrias del objeto 

#1.3.1 
for (i in 1:6) {
  print(paste("*******************************st_box de",bases[i],"*******************************"))
  print(st_bbox(get(bases[i])))
  print(paste("*******************************crs de",bases[i],"*******************************"))
  print(st_crs(get(bases[i])))
} #Pintar la caja de cordenadas y el sistema de referencias de cada base

#1.3.2
c_medico2=st_transform(x=c_medico,crs = "+proj=utm +zone=19+ datum=WGS84 +units=m +no_defs") #Cambiar el CRS de c_medico
c_poblado2=st_transform(x=c_poblado,crs = "+proj=utm +zone=19+ datum=WGS84 +units=m +no_defs") #Cambiar el CRS de c_poblado
depto2=st_transform(x=depto,crs = "+proj=utm +zone=19+ datum=WGS84 +units=m +no_defs") #Cambiar el CRS de depto
mapmuse=st_transform(x=mapmuse,crs = "+proj=utm +zone=19+ datum=WGS84 +units=m +no_defs") #Cambiar el CRS de mapmuse
puntos=st_transform(x=puntos,crs = "+proj=utm +zone=19+ datum=WGS84 +units=m +no_defs") #Cambiar el CRS de puntos
via=st_transform(x=via,crs = "+proj=utm +zone=19+ datum=WGS84 +units=m +no_defs") #Cambiar el CRS de via

#1.4 Operaciones geometricas 

#1.4.1
mapmuse=st_intersection(x=mapmuse,y=depto2) # Clipping dejando solo puntos de Norte de Santander

#1.4.2
via_54498=st_intersection(x=via,y=c_poblado2[c_poblado2$cod_dane=="54498",]) #Crear un shp de solo las vias del municipio 54498
sum(st_length(via_54498)) #Medir el largo de las vias del municipio 54498

#1.5 Pintar mapas

#1.5.1
leaflet() %>% addTiles() %>% addCircleMarkers(data = c_medico) %>%
  addPolygons(data = c_poblado,color = "green") %>%
  addPolygons(data = depto,color = "red") #Crear el mapa de centros medicos, centros poblados y departamento de norte de santander

#1.5.2
mapa=ggplot() + geom_sf(data=depto) + geom_sf(data=c_poblado,col="blue") +
  geom_sf(data=c_medico,col="green") + annotation_scale() +
  annotation_north_arrow(location='tr') + ggtitle("Norte de Santander
  Centros poblados y hospitales") +
  xlab("Longitud") + ylab("Latitud") + theme_bw() #Mapa Norte de Santander, Centros poblados y hostpitales
pdf("views/mapa_norte_santander.pdf") #Abrir pdf
mapa #Generar el mapa
dev.off() #cerrar el pdf

#2. Regresiones 
rm(list = ls()) # Limpiar el ambiente 
source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")

#2.1.
db=readRDS("data/output/f_mapmuse.rds") #Importar los datos
ols=lm(formula = fallecido ~ dist_vias + dist_cpoblado + dist_hospi + actividad , data = db) # almacenar y correr regresión

#2.2.
jpeg(file="views/ols.jpeg") #crear el jpeg 
coefplot(ols) #Pintar los coeficientes
dev.off() #Cerrar el jpeg

#2.3.
logit = glm(formula = fallecido ~ dist_vias + dist_cpoblado + dist_hospi + actividad , data = db, family = binomial(link = "logit")) #Modelo logit 
probit = glm(formula = fallecido ~ dist_vias + dist_cpoblado + dist_hospi + actividad, data = db, family = binomial(link = "probit")) #Modelo probit

#2.4.
outreg(list("ols" = ols, "logit" = logit,"probit"=probit)) #Mostrar los resultados de los tres modelos en una tabla

#2.5



#3. Web-scraping 
rm(list = ls()) # Limpiar el ambiente 

#3.1.
url="https://es.wikipedia.org/wiki/Departamentos_de_Colombia" #Definir la URL
xml_document=read_html(url) #Leer la URL

#3.2.
html_nodes(xml_document,xpath = '//*[@id="firstHeading"]') %>% html_text() #Extraer el titulo


#3.3.
tablas=readHTMLTable(htmlParse(xml_document),header = T) #Extraer las tablas del HTML
Dptos_colombia=tablas[[4]] #Extraer la tabla de departamentos de Colombia


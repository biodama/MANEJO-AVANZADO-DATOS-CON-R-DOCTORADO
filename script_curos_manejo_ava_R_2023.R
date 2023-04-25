
#######################################################
#######################################################
# SCRIPT CURSO MANEJO AVANZADO DE DATOS CON R
#######################################################
#######################################################

rm(list=ls())
gc()

#######################################################
#######################################################
# RECODIFICACION
#######################################################
#######################################################

setwd("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_R_2023/")

# Cargamos los datos del curso

datos <- read.table("datos/datos.curso1.txt",sep="\t",header=T,stringsAsFactors=F)

head(datos)
dim(datos)
str(datos)

# load("datos/datos.curso1.RData")


indice.registros<- c(2,15,28)

datos$"peso"[indice.registros] <- 44.33

datos$peso[1]<- 60

# Recodificación de uno de los valores del estado civil (“Casado” a “cas”):
table(datos$"estado.civil",exclude=NULL)


datos$"estado.civil.new" <- datos$"estado.civil"
datos$"estado.civil.new" [datos$"estado.civil.new"%in%"Casado"]<-"cas"
#datos$"estado.civil.new" [datos$"estado.civil.new"=="Casado"]<-"cas"

table(datos$estado.civil.new,exclude=NULL)
table(datos$estado.civil,datos$estado.civil.new,exclude=NULL)


# Variable numerica

datos$"edad_gr" <- datos$"edad"
datos$"edad_gr"[datos$"edad"<20]<- "[0-20)"
datos$"edad_gr"[datos$"edad">=20 & datos$"edad"<40 ]<- "[20-40)"

table(datos$"edad_gr",exclude=NULL)

class(datos$"edad_gr")

datos$"edad_gr"<-as.factor(datos$"edad_gr")
class(datos$"edad_gr")
datos$"edad_gr"<-as.character(datos$"edad_gr")

# Utilizando funcion cut

range(datos$"edad")
datos$"edad_gr_new" <- cut(x=datos$"edad", breaks=seq(0,100,20),right=F,include.lowest=T)

table(datos$"edad_gr_new",exclude=NULL)

range(datos$"edad")
datos$"edad_gr_new2" <- cut(x=datos$"edad", breaks=c(0,10,20,40,66,83,90),
right=F,include.lowest=T)

table(datos$"edad_gr_new2",exclude=NULL)


datos$"edad_gr_new3" <- cut(x=datos$"edad", breaks=c(0,20,60,75,90),
right=F,include.lowest=T)

table(datos$"edad_gr_new3",exclude=NULL)

minimo<-min(datos$edad)
maximo<-max(datos$edad)

datos$"edad_gr_new3" <- cut(x=datos$"edad", 
breaks=c(minimo,20,60,75,maximo),
right=F,include.lowest=T)

table(datos$"edad_gr_new3",exclude=NULL)


puntos_corte<-quantile(datos$"edad")

datos$"edad_gr_new3" <- cut(x=datos$"edad", 
breaks=puntos_corte,
right=F,include.lowest=T)

table(datos$"edad_gr_new3",exclude=NULL)


# Funion factor()

datos$"estado.civil.factor" <- as.factor(datos$"estado.civil")
levels(datos$"estado.civil.factor")

nuevo.orden<- c("Divorciado", "Soltero", "Casado")
datos$"estado.civil.new"<- factor(datos$"estado.civil.new",levels=nuevo.orden)
levels(datos$"estado.civil.new")
table(datos$"estado.civil",datos$"estado.civil.new",exclude=NULL)

# levels ()

levels(datos$"estado.civil.factor")

levels(datos$"estado.civil.factor")<- c("cas","div","sol")

levels(datos$"estado.civil.factor")





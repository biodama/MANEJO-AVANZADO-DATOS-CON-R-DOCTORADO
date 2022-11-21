

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/CURSOS_R/ISCIII_UNED_DOCTORADO/Manejo_avanzado_datos_con_R/")

load("datos/datos.curso1.RData")

# RECODIFICACION

# Recodificacion variable numerica

str(datos)

range(datos$"edad")

min(datos$"edad")

max(datos$"edad")

class(datos$"edad")


datos$"edad_gr" <- datos$"edad"
datos$"edad_gr"[datos$"edad"<20]<- "[0-20)"
datos$"edad_gr"[datos$"edad">=20 & datos$"edad"<40 ]<- "[20-40)"
table(datos$"edad_gr",exclude=NULL)
class(datos$"edad_gr")

datos$"edad_gr"<-as.factor(datos$"edad_gr")
class(datos$"edad_gr")


datos$"edad_gr_new" <- cut(x=datos$"edad", breaks=seq(0,100,20),right=F,include.lowest=T)
table(datos$"edad_gr_new",exclude=NULL)

datos$"edad_gr_new2" <- cut(x=datos$"edad", breaks=seq(0,100,20),right=T,include.lowest=F)
table(datos$"edad_gr_new2",exclude=NULL)

class(datos$"edad_gr_new")

range(datos$"altura") # <=160 >160
datos$"size"<-cut(x=datos$"altura", breaks=c(0,160,175),right=T,include.lowest=T)
table(datos$"size",exclude=NULL)

# class(datos$estado.civil)

table(datos$"estado.civil")

datos$"estado.civil.factor" <- as.factor(datos$"estado.civil")

levels(datos$"estado.civil.factor")


# Cambiar el orden de las categorias de una varible factor

nuevo.orden<- c("Divorciado", "Soltero", "Casado")
datos$"estado.civil.factor.new"<- factor(datos$"estado.civil.factor",levels=nuevo.orden)
levels(datos$"estado.civil.factor.new")

head(datos[,c("estado.civil","estado.civil.factor","estado.civil.factor.new")])


# Recodificacion levels

levels(estado.civil.factor)

levels(estado.civil.factor) <- c("cas", "div", "sol")
table(estado.civil.factor)

levels(estado.civil.factor)[2]<-c("divo")
table(estado.civil.factor)

estado.civil.caracter <- as.character(estado.civil.factor)















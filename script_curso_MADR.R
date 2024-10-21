##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
#
#    MADR: MANEJO AVANZADO DE DATOS CON R
#
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################


rm(list=ls()) # borra todos los objetos que hay en el workspace de la sesion
gc()


# Cargar / importar los datos

setwd("/Users/pfernandezn/Desktop/MADR/") # establezco la ruta principal


load("datos/datos.curso1.RData") # cargo los datos del curso


ls() # listar los nombres de los objetos creados o cargados/importados en la sesion

dim(datos)
head(datos)
head(datos[,c(1:10)])
str(datos)


#####################################################
#####################################################
# RECODIFDICACION
#####################################################
#####################################################

####################################################
# Posiciones
####################################################

indice.registros <- c(1,15,28)
datos$"peso"[indice.registros]
datos$"peso"[indice.registros] <- 44.33
datos$"peso"[indice.registros]

####################################################
# Condiciones logicas
####################################################
criterio <- datos$"peso"<=50
datos$"peso_cat" <- c("High")
datos$"peso_cat"[criterio]<- "Low"


table(datos$"estado.civil")
datos$"estado.civil.new" <- datos$"estado.civil"
datos$"estado.civil.new" [datos$"estado.civil.new"%in%"Casado"]<-"cas"
table(datos$estado.civil.new)

####################################################
# Funcion CUT (numerica a categorica) con la EDAD
####################################################

datos$"edad"[c(1,3,6)]<-NA

str(datos) # cuidado si la base de datos es muy grande

range(datos$"edad") # maximo y minimo

min(datos$"edad")

max(datos$"edad")

class(datos$"edad")

# HAY QUE TOMAR UNA DECISION CON LOS MISSING PORQUE SABEMOS QUE NOS PUEDEN DAR PROBLEMAS

sum(is.na(datos$"edad"))

# Esther question
# which(is.na(datos$"edad")) # posiciones donde hay missing

datos$"edad"[which(is.na(datos$"edad"))] <- 15

sum(is.na(datos$"edad"))

# Categorizacion basica

datos$"edad_gr" <- datos$"edad"

datos$"edad_gr"[datos$"edad"<20] <- "[0-20)"
datos$"edad_gr"[datos$"edad">=20 & datos$"edad"<40]<- "[20-40)"
datos$"edad_gr"[datos$"edad">=40]<- "[40-85]"

table(datos$"edad_gr",exclude=NULL)

class(datos$"edad_gr")

datos$"edad_gr" <- as.factor(datos$"edad_gr")
class(datos$"edad_gr")


# Con la funcion cut

datos$"edad_gr_new" <- cut(x=datos$"edad", 
						breaks=c(0,20,40,85),right=FALSE,include.lowest=TRUE)

table(datos$"edad_gr_new",exclude=NULL)
class(datos$"edad_gr_new")


# Con la funcion cut y categorias hechas con la funcion seq

c(1,2,3,4,5)
c(1:5)
c(0,20,40,60,80,100)

seq(from=0,to=100,by=20)

datos$"edad_gr_new2" <- cut(x=datos$"edad", breaks=seq(0,100,20),right=F,include.lowest=T)

table(datos$"edad_gr_new2",exclude=NULL)



datos$"edad_gr_new3" <- cut(x=datos$"edad", breaks=seq(0,100,20),right=T,include.lowest=F)

table(datos$"edad_gr_new3",exclude=NULL)


# QUE PASA CON ESTO

hist(datos$"edad")
datos$"QUE_PASA" <- cut(x=datos$"edad", breaks=c(50,70,100),right=F,include.lowest=T)
table(datos$"QUE_PASA",exclude=NULL)

# QUE PASA CON ESTO DE LOS MISSING

datos$"edad"[c(1,3,6)]<-NA

datos$"edad_gr_new4" <- cut(x=datos$"edad", 
						breaks=c(0,20,40,85),right=FALSE,include.lowest=TRUE)

table(datos$"edad_gr_new4",exclude=NULL)

# Algo parecido al ejercicio de evaluacion

range(datos$"altura") # 147.8264 172.8742
datos$"size"<-cut(x=datos$"altura", breaks=c(0,150,175),right=T,include.lowest=T)
table(datos$"size",exclude=NULL)









############################################################
############################################################
# INTRODUCCION
############################################################
############################################################

# Ver el contenido del workspace o area de trabajo

ls()

# Ayuda

?rnorm

# Objetos y creacion

x <- c(1,2,3)

z <- c(TRUE,FALSE,TRUE)

y<-c("Low","Low","Medium","High")

tabla1 <- data.frame(ID = c("gen0", "genB","genZ"),
subj1 = c(10, 25, 33), 
subj2 = c(NA, 34, 15),
oncogen = c(TRUE, TRUE,FALSE),
loc = c(1,40, 125))

# Nombres

# ¡CUIDADOOOOOOOOOO!

# Workspace

ls()

rm(tabla1,x)

rm(list=ls()) # borramos todo el workspace


# Ejecucion de codigo seleccionado

Control + R (en el Software de R básico)

Control + Enter (en el RStudio)

Command + Enter (en el Software de R basico en Mac)

Command + Shift + c (en el Software de R basico en Mac y en R Studio) => Comentar texto


# Atributos

x <- c(1,2,3)
z <- c(TRUE,FALSE,TRUE)
y<-c("Low","Low","Medium","High")

tabla1 <- data.frame(ID = c("gen0", "genB","genZ"),
subj1 = c(10, 25, 33), 
subj2 = c(NA, 34, 15),
oncogen = c(TRUE, TRUE,FALSE),
loc = c(1,40, 125))


length(x)
length(y)
length(z)

class(x)
class(y)
class(z)


class(tabla1)
dim(tabla1) 
str(tabla1) # Cuidado con este si la base de datos es muy grande


edad <- c("14","21","33")

class(edad)

edad/2

edad.numerica <- as.numeric(edad)

edad.numerica/2


# Packages (Libreria es un direcctorio donde guardo paquetes)

# Tipos:

# 1. Los que vienen instalados y cargados con el software basico.

# 2. Los que necesitamos instalarlos una vez (quedando en nuestro ordenador en una libreria (que es una carpeta)) 
# y cargarlos siempre que necesitemos alguna funcion que contiene.

# install.packages("randomForest") # instalacion

# library("randomForest") # carga



install.packages("/Users/pfernandezn/Desktop/ABM_0.2.tar.gz",repos=NULL,type="source")



# Paquete de Bioconductor

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("DESeq2")



# Importacion de excel y de formato tipo R

# install.packages("openxlsx")

setwd("/Users/pfernandezn/Desktop/CURSO.MANEJO/")

library("openxlsx")

?read.xlsx

datos_importados <- read.xlsx("datos/datos.curso1.xlsx",sheet=1)

ls()

class(datos_importados)

dim(datos_importados)

str(datos_importados)


# Importacion / Carga de workspace (formato .RData)

load("datos/datos.curso1.RData")


# Visualizacion datos


head(datos_importados,n=10)

tail(datos_importados,n=10)

View(datos_importados)

fix(datos_importados) # Cuidado que hay que cerrar la pantalla que sale; si no se cierra no puedo seguir operando.
		# Cuidado tambien que puedo modificar datos y no queda constancia de las modificaciones 


########################################################
# Acceso a elementos
########################################################

datos_importados$"edad"
datos_importados$edad

# datos[ FILAS  ,  COLUMNAS  ]

datos[ c(3,1,5) ,     ]
datos[ c(3,1,5) ,  c(2,1)   ]

subbase1 <- datos[ c(3,1,5) ,     ]
subbase2 <- datos[ c(3,1,5) ,  c(2,1)   ]

# datos$"nombre_variable"[ ]

datos_importados$"edad"[ c(2,1) ]


# datos[ FILAS (condicion)  ,  COLUMNAS (nombres)  ]

table(datos$"sexo",exclude=NULL)
class(datos$"sexo")

datos[ datos$"sexo"=="Mujer"         ,        ]

# datos_mujeres <- datos[ datos$"sexo"=="Mujer" ,   ]

datos[ which(datos$"sexo"=="Mujer")        ,        ]


datos[ datos$"sexo"%in%"Mujer"         ,        ] # Cuando sexo tenga valores missing
datos[ which(datos$"sexo"%in%"Mujer")         ,        ] # Cuando sexo tenga valores missing


table(datos$"sexo",exclude=NULL)
class(datos$"sexo")
sum(is.na(datos$"sexo"))

class(datos$"peso")
sum(is.na(datos$"peso"))

mujeres_pesadas <- datos[ which(datos$"sexo"=="Mujer" & datos$"peso">=60)    ,        ]

table(mujeres_pesadas$"sexo",exclude=NULL)

min(mujeres_pesadas$"peso")
max(mujeres_pesadas$"peso")
range(mujeres_pesadas$"peso")


mujeres_pesadas_especial <- datos[ which(datos$"sexo"=="Mujer" & datos$"peso">=60), c("ID","sexo","edad","peso") ]

names(mujeres_pesadas_especial)



########################################################
# Crear una variable en un data.frame
########################################################

names(datos)

datos$"cancer.mama_new" <- NA

datos$"estado.civil_new"<- NA


########################################################
# Eliminar
########################################################

datos_nuevos<-datos[,c("ID","sexo")]

# datos_nuevos<-datos[,-c(1,2,3)]

########################################################
# Operaciones aritmeticas
########################################################

class(datos$"peso")
datos$"peso"<-as.numeric(datos$"peso")
range(datos$"peso")
sum(is.na(datos$"peso"))


class(datos$"altura")
datos$"altura"<-as.numeric(datos$"altura")
range(datos$"altura")
sum(is.na(datos$"altura"))


datos$"peso_new" <- NA
datos$"peso_new" <-  datos$"peso" + 2

datos$"ratio" <- NA
datos$"ratio" <- c(datos$"altura"^2) / c(datos$"peso")


########################################################
# Operaciones logicas
########################################################


datos_mirar <- datos[ which(datos$"sexo"=="Mujer" | datos$"peso">=60), ]

names(datos_mirar)

head(datos_mirar)


########################################################
# Recodificar
########################################################


datos$"cancer.mama_new" <- NA # recodificar missing values
 
datos$"cancer.mama_new" <- datos$"cancer.mama"
sum(is.na(datos$"cancer.mama"))
datos$"cancer.mama_new" [ which(is.na(datos$"cancer.mama"))  ] <- "NO SE"
# datos$"cancer.mama_new" [ is.na(datos$"cancer.mama")  ] <- "NO SE"

table(datos$"cancer.mama",datos$"cancer.mama_new",exclude=NULL)



datos$"estado.civil_new"<- NA # recodificar Casado por cas

table(datos$"estado.civil",exclude=NULL)
datos$"estado.civil_new" <- datos$"estado.civil"
datos$"estado.civil_new"[ which(datos$"estado.civil_new"=="Casado") ] <- "cas"

table(datos$"estado.civil",datos$"estado.civil_new", exclude=NULL)



########################################################
# Exportar
########################################################


datos_exportar <- datos[   , c("ID","cancer.mama_new","estado.civil_new","peso_new","ratio")]

dim(datos_exportar)
head(datos_exportar)


library("openxlsx")

write.xlsx(x=datos_exportar,file="/Users/pfernandezn/Desktop/datos_exportar.xlsx")

save(datos_exportar,file="/Users/pfernandezn/Desktop/datos_manipulados.RData")




################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################


rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/CURSO.MANEJO/")

load("datos/datos.curso1.RData")


source("scripts/mis_funciones.R")


# RECODIFICACION

# Recodificacion variable numerica

str(datos)

class(datos$"edad")

datos$"edad_new"<-as.numeric(datos$"edad")

datos$"edad" [which(is.na(datos$"edad.new"))]


range(datos$"edad")

min(datos$"edad")

max(datos$"edad")

# [0-20),[20-40), [40,85]
# la manera que conoceis

datos$"edad_gr" <- datos$"edad"
datos$"edad_gr"[datos$"edad"<20] <- "[0-20)"
datos$"edad_gr"[datos$"edad">=20 & datos$"edad"<40 ] <- "[20-40)"
datos$"edad_gr"[datos$"edad">=40 ]<- "[40,85]"
table(datos$"edad_gr",exclude=NULL)
class(datos$"edad_gr")

datos$"edad_gr"<-as.factor(datos$"edad_gr")
class(datos$"edad_gr")


# la nueva manera
# [0-20),[20-40), [40,85]
datos$"edad_gr_new" <- cut(x=datos$"edad", breaks=c(0,20,40,85),right=F,include.lowest=T)
table(datos$"edad_gr_new",exclude=NULL)


#datos$"edad_gr_new" <- cut(x=datos$"edad", breaks=seq(0,100,20),right=F,include.lowest=T)
#table(datos$"edad_gr_new",exclude=NULL)

#datos$"edad_gr_new2" <- cut(x=datos$"edad", breaks=seq(0,100,20),right=T,include.lowest=F)
#table(datos$"edad_gr_new2",exclude=NULL)

class(datos$"edad_gr_new")


# datos$"edad_gr_new"<-as.character(datos$"edad_gr_new")


range(datos$"altura") # <=160 >160
datos$"size" <- cut(x=datos$"altura", breaks=c(0,160,175),right=T,include.lowest=T)
table(datos$"size",exclude=NULL)


# class(datos$estado.civil)

table(datos$"estado.civil",exclude=NULL)
class(datos$"estado.civil")

datos$"estado.civil.factor" <- as.factor(datos$"estado.civil")
class(datos$"estado.civil.factor")
table(datos$"estado.civil.factor",exclude=NULL)

levels(datos$"estado.civil.factor")


# Cambiar el orden de las categorias de una varible factor

nuevo.orden<- c("Divorciado", "Soltero", "Casado")
datos$"estado.civil.factor.new"<- factor(datos$"estado.civil.factor",levels=nuevo.orden)
levels(datos$"estado.civil.factor.new")

head(datos[,c("estado.civil","estado.civil.factor","estado.civil.factor.new")])


# Recodificacion levels

levels(datos$"estado.civil.factor")

levels(datos$"estado.civil.factor") <- c("cas", "div", "sol")
table(datos$"estado.civil.factor")

levels(datos$"estado.civil.factor")[2]<-c("divo")
table(datos$"estado.civil.factor")

datos$"estado.civil.caracter" <- as.character(datos$"estado.civil.factor")



class(datos$"nivel.estudios")

datos$"nivel.estudios.factor"<-as.factor(datos$"nivel.estudios")

class(datos$"nivel.estudios.factor")

levels(datos$"nivel.estudios.factor")

# "Bajo" en "escuela"

# levels(datos$"nivel.estudios.factor")<-c("Alto" , "escuela" , "Medio")

levels(datos$"nivel.estudios.factor")[2]<-c("escuela")

levels(datos$"nivel.estudios.factor")



#1. Importa los datos del ejercicio de evaluacion (datos.evaluacion.txt)
#2. Crea las siguentes variables tal y como se indica:
#2.1. estado.civil.reducido: Es igual a la variable estado.civil pero donde la categoría de “Casado” debe
#llamarse “cas”, la de “Soltero”, “otro”, y la de “Divorciado”, “otro”.
#2.2. “talla”: variable que tenga valores de “XL” cuando el altura >=150 y “L” cuando la altura sea <150.
#2.3. grupo.edad: Una variable categórica (factor) de la edad, con los siguientes grupos; “[0,30)” “[30,50)”
#“[50,85]”


setwd("/Users/pfernandezn/Desktop/CURSO.MANEJO/")
?read.table
datos <- read.table("datos/datos.evaluacion.txt",header=T,sep="\t")

head(datos)
str(datos)

class(datos$"estado.civil")
sum(is.na(datos$"estado.civil"))

datos$"estado.civil.reducido"<-as.factor(datos$"estado.civil")
levels(datos$"estado.civil.reducido")


datos$"altura"
datos$"talla" <- cut()
levels(datos$"talla")
levels(datos$"talla")<-c("??","??")

datos$"edad"
datos$"grupo.edad"<-cut(datos$"edad",xxxxxxx)
levels(datos$"grupo.edad")


#################################################################
# FECHAS
#################################################################


rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/CURSO.MANEJO/")

load("datos/datos.curso1.RData")

ls()

class(datos)

str(datos)



# Formato fecha perfecto para R

class(datos$"fdiag_cm")
sum(is.na(datos$"fdiag_cm"))
unique(datos$"fdiag_cm")
sort(unique(datos$"fdiag_cm"),na.last=FALSE)

datos$"fechas.CM" <- as.Date(datos$"fdiag_cm")

class(datos$"fechas.CM")
sum(is.na(datos$"fechas.CM"))
datos$"fechas.CM"[1:6]


# Otro formato 10.07.88

class(datos$"fdiag_cp")
sum(is.na(datos$"fdiag_cp"))
unique(datos$"fdiag_cp")
sort(unique(datos$"fdiag_cp"),na.last=FALSE)

datos$"fechas.CP" <- as.Date(datos$"fdiag_cp",format="%d.%m.%y")

class(datos$"fechas.CP")
sum(is.na(datos$"fechas.CP"))
unique(datos$"fechas.CP")


# Otro formato 2007.12.13

class(datos$"fdef")
sum(is.na(datos$"fdef"))
unique(datos$"fdef")
sort(unique(datos$"fdef"),na.last=FALSE)

datos$"fechas.DF" <- as.Date(datos$"fdef",format="%Y.%m.%d")

class(datos$"fechas.DF")
sum(is.na(datos$"fechas.DF"))
unique(datos$"fechas.DF")



# Restar o sumar dias a una fecha

datos$"fecha.DF_especial" <- datos$"fechas.DF" - 3



# Otros formatos especiales (CUIDADO!!!!!!!!!!)

datos$"ano_def"<-format(datos$"fechas.DF",format="%Y")


dates <- c("01ene1960", "02ene1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")

dates <- c("01jan1960", "02jan1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")


###########################
# OPERACIONES CON FECHAS
###########################

datos$"dias"<-c(datos$"fechas.DF" - datos$"fechas.CM")
class(datos$"dias")
#datos$"dias"<-as.numeric(datos$"dias")

datos$"dias2"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="days")

datos$"weeks"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="weeks")
 
datos$"years"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="days") / 365.25
attr(datos$"years","units")="years"



datos$"fecha.IN" <- as.Date(datos$"finterv",format="%Y.%m.%d")

datos$"fecha.IN_new"<-as.character(format(datos$"fechas.IN",format="%b;%Y"))


# Secuencias de fechas

seq(as.Date("2021-01-01"),by="days",length=6)

seq(as.Date("2021-01-01"),to=as.Date("2023-03-01"),by="days")

datos_temporales <- data.frame(fecha=seq(as.Date("2021-01-01"),to=as.Date("2023-03-01"),by="days"),casos=NA)


# Formato fecha con horas, minutos y segundos

first <- "2022-08-20 08:15:22"
second<- "2022-08-20 08:17:22"
difftime( second,first, units="mins")

# Eliminar variables

datos$"fecha_hora1_new"<-NULL


#2.4. fecha.CM: variable tipo fecha a partir de la variable “fdiag_cm”
#2.5. fecha.CP: variable tipo fecha a partir de la variable “fdiag_cp”
#2.6. fecha.IN: variable tipo fecha a partir de la variable “finterv”
#2.7. fecha.IN_new: variable tipo caracter a partir de la variable “fecha.IN” de tal manera que figure solo el
#mes y el año como en el siguiente ejemplo (May;1980)
#2.8. dias.in.CM: número de días entre la fecha de diagnostico de cáncer de mama (fecha.CM) y la fecha de
#intervencion (fecha.IN)
#2.9. semanas.df.CM: número de semanas entre la fecha de diagnostico de cáncer de mama (fecha.CM) y la
#fecha de intervención (fecha.IN)
#2.10. fecha_analisis_IN: variable tipo fecha resultante de restar 7 días a la fecha.IN

setwd("/Users/pfernandezn/Desktop/CURSO.MANEJO/")
datos_evaluacion <- read.table("datos/datos.evaluacion.txt",header=T,sep="\t")


class(datos_evaluacion$"fdiag_cm")
sum(is.na(datos_evaluacion$"fdiag_cm"))
sort(unique(datos_evaluacion$"fdiag_cm"),na.last=FALSE) # !CUIDADO AQUI EL SORT NO HACE LO QUE QUEREMOS


datos_evaluacion$"fecha.CM" <- as.Date(datos_evaluacion$"fdiag_cm",format="%d.%m.%y")
sum(is.na(datos_evaluacion$"fecha.CM"))
sort(unique(datos_evaluacion$"fecha.CM"),na.last=FALSE)




###########################
# CARACTERES
###########################

# nchar() # numero de caracteres (de la librería gdata)

nchar(as.character(datos$"ID"))
table(nchar(as.character(datos$"ID")))


# paste() # concatenar caracteres

datos$"ID_new"<-datos$"ID"
datos$"ID_new"[nchar(as.character(datos$"ID"))==1]<-paste("00",datos$"ID_new"[nchar(as.character(datos$"ID"))==1],sep="")
datos$"ID_new"[nchar(as.character(datos$"ID"))==2]<-paste("0",datos$"ID_new"[nchar(as.character(datos$"ID"))==2],sep="")
table(nchar(as.character(datos$"ID_new")))


x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")

paste(x,collapse="/")


datos$"id_combinado"<-paste(datos$"ID",datos$"sexo",sep="***")

paste(datos$"ID",datos$"sexo",datos$"edad",sep="***")


# strsplit() # Dividir los elementos de un vector de caracteres en subcadenas de acuerdo con criterio

library("plyr") # para estructuras regulares


res<-strsplit(datos$"fdiag_cm",split="-")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)

table(res$"day",exclude=NULL)
table(res$"month",exclude=NULL)
table(res$"year",exclude=NULL)


datos<-cbind(datos,res)
datos$"year_otro"<-res$"year"


table(nchar(res$"year"))
table(nchar(res$"month"))

sort(unique(res$"year"))
sort(unique(res$"month"))



res<-strsplit(datos$"fdef",split="[.]")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)


unique(interaction(datos$estado.civil,datos$sexo))

table(interaction(datos$estado.civil,datos$sexo))

nombres<-c("María","Pachón")
# toupper
nombres<-tolower(nombres)

nombres<-gsub("á","a",nombres)
nombres<-gsub("é","e",nombres)
nombres<-gsub("í","i",nombres)
nombres<-gsub("ó","o",nombres)
nombres<-gsub("ú","u",nombres)

nombres<-gsub("ñ","n",nombres)

nombres<-gsub("ü","u",nombres)
nombres<-gsub("ö","o",nombres)



nombres_may<-toupper(nombres)
nombres_min<-tolower(nombres)


datos$"fdiag_cm_nuevo"<-gsub("[.]","",datos$"fdiag_cm")


# Unique

length(datos$"ID")
length(unique(datos$"ID"))

# duplicated

table(duplicated(datos$"ID"))



# Grep y match
ID_new<-paste(datos$"ID",datos$"sexo",sep=".")
ID2<-gsub("[.]","",ID_new)

datos$"busqueda"<-"NO"
indice<-grep("Muj",ID2)
datos$busqueda[indice]<-"LO HE ENCONTRADO"

match("Muj",ID2)



datos_evaluacion$"complicaciones_new" <- datos_evaluacion$"complicaciones"

datos_evaluacion$"complicaciones_new"<-tolower(datos_evaluacion$"complicaciones_new")

datos_evaluacion$"complicaciones_new"<-gsub("á","a",datos_evaluacion$"complicaciones_new")
datos_evaluacion$"complicaciones_new"<-gsub("é","e",datos_evaluacion$"complicaciones_new")
datos_evaluacion$"complicaciones_new"<-gsub("í","i",datos_evaluacion$"complicaciones_new")
datos_evaluacion$"complicaciones_new"<-gsub("ó","o",datos_evaluacion$"complicaciones_new")
datos_evaluacion$"complicaciones_new"<-gsub("ú","u",datos_evaluacion$"complicaciones_new")

datos_evaluacion$"complicaciones_new"<-gsub("ñ","n",datos_evaluacion$"complicaciones_new")

datos_evaluacion$"complicaciones_new"<-gsub("ü","u",datos_evaluacion$"complicaciones_new")
datos_evaluacion$"complicaciones_new"<-gsub("ö","o",datos_evaluacion$"complicaciones_new")


indice<-grep("cancer",datos_evaluacion$"complicaciones_new")

datos$"CANCER_comp"<-0
datos$"CANCER_comp"[indice]<-1

table(datos$"CANCER_comp",exclude=NULL)




# Funciones


mi.funcion <- function(x){
	
	res<-x/10
	
	return(res)
		
}

datos$"edad_new"<-mi.funcion(datos$"edad")



mi.funcion2 <- function(x,valor=10){
	
	
	if(class(x)=="numeric"){
	
		res<-x/valor
	
	}
	
	if(class(x)!="numeric"){
		
		res<-"no puedo hacer esa operacion"
	
	}
	
	if(class(x)=="Date"){
		
		res<-x-5  # si es fecha que le reste 5 dias
	
	}
	
	
	return(res)
		
}

mi.funcion2(x=datos$"estado.civil")
mi.funcion2(x=datos$"edad")
mi.funcion2(x=as.Date(datos$"fdiag_cm"))
mi.funcion2(x=datos$"edad",valor=25)

# Ifelse

ifelse(datos$"edad">50, "Mayor", "Menor")


# For, while

datos$"edad_new"<-datos$"edad"/10


datos$"edad_new"<-NA
for(i in 1:dim(datos)[1]){
	
	print(i)
	datos$"edad_new"[i]<-datos$"edad"[i]/10
		
}


while(x>10){
	
	print(" seguir")	
	
	
}



apply(datos[,c("peso","altura")],2,function(x) x/10)

# Funcion depuracion
#que revise una base de datos del curso
# y que si encuentra algún registro mujer, divorciada, muestre el siguiente mensaje:
#"hay mujeres a revisar"
#y proporcione los identificadores (IDs) de los registros que cumplan esta condición

table(datos$sexo)
table(datos$estado.civil)

depuracion<-function(x){

 	mirar<-x[c(x$"sexo"%in%"Mujer" & x$estado.civil%in%"Divorciado"),] 
	
	if(dim(mirar)[1]>0){
        
		print("hay mujeres a revisar")
	
        		ids_mirar<-mirar$ID 

	}

	if(dim(mirar)[1]==0){ 
		
		ids_mirar<-NA


	}

	return(ids_mirar)

}

depuracion(datos)



prepara_character <- function(var){
	
	variable<-var
	
	variable<-toupper(variable)
	
	variable<-gsub("Ó","O",variable)
	variable<-gsub("Í","I",variable)
	variable<-gsub("Á","A",variable)	
	variable<-gsub("É","E",variable)	
	variable<-gsub("Ú","U",variable)	
	
	return(variable)
	
}


datos_depurados<-apply(datos[,c("sexo","estado.civil","nivel.estudios")],2,
function(x) prepara_character(x))

datos<-cbind(datos,datos_depurados)







##############
# Tiempos
##############

inicio<-Sys.time()


# Funciones y ejecuciones


final<-Sys.time()

final-inicio
difftime(final,inicio,units="mins")
































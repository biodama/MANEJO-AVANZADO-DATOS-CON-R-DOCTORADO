

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/CURSOS_R/ISCIII_UNED_DOCTORADO/Manejo_avanzado_datos_con_R/")

load("datos/datos.curso1.RData")


source("/Users/pfernandezn/Desktop/mis_funciones.R")


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



class(datos$"nivel.estudios")

datos$"nivel.estudios.factor"<-as.factor(datos$"nivel.estudios")

class(datos$"nivel.estudios.factor")

levels(datos$"nivel.estudios.factor")

# "Bajo" en "escuela"

# levels(datos$"nivel.estudios.factor")<-c("Alto" , "escuela" , "Medio")

levels(datos$"nivel.estudios.factor")[2]<-c("escuela")

levels(datos$"nivel.estudios.factor")


#################################################################
# FECHAS
#################################################################

# Formato fecha perfecto para R

class(datos$"fdiag_cm")
sum(is.na(datos$"fdiag_cm"))
unique(datos$"fdiag_cm")

datos$"fechas.CM" <- as.Date(datos$"fdiag_cm")

class(datos$"fechas.CM")
sum(is.na(datos$"fechas.CM"))
datos$"fechas.CM"[1:6]


# Otro formato 10.07.88

class(datos$"fdiag_cp")
sum(is.na(datos$"fdiag_cp"))
unique(datos$"fdiag_cp")

datos$"fechas.CP" <- as.Date(datos$"fdiag_cp",format="%d.%m.%y")

class(datos$"fechas.CP")
sum(is.na(datos$"fechas.CP"))
unique(datos$"fechas.CP")


# Otro formato 2007.12.13

class(datos$"fdef")
sum(is.na(datos$"fdef"))
unique(datos$"fdef")


datos$"fechas.DF" <- as.Date(datos$"fdef",format="%Y.%m.%d")

class(datos$"fechas.DF")
sum(is.na(datos$"fechas.DF"))
unique(datos$"fechas.DF")



# Otros formatos especiales (CUIDADO!!!!!!!!!!)


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


# Formato fecha con horas, minutos y segundos

first <- "2022-08-20 08:15:22"
second<- "2022-08-20 08:17:22"
difftime( second,first, units="mins")

# Eliminar variables

datos$"fecha_hora1_new"<-NULL

###########################
# CARACTERES
###########################

# nchar() # numero de caracteres (de la librería gdata)

nchar(as.character(datos$ID))
table(nchar(as.character(datos$ID)))


# paste() # concatenar caracteres

x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")

paste(x,collapse="/")


datos$"id_combinado"<-paste(datos$"ID",datos$"sexo",sep="***")

# strsplit() # Dividir los elementos de un vector de caracteres en subcadenas de acuerdo con criterio

res<-strsplit(datos$"fdiag_cm",split="-")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)

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


# Grep
ID_new<-paste(datos$"ID",datos$"sexo",sep=".")
ID2<-gsub("[.]","",ID_new)

datos$"busqueda"<-"NO"
indice<-grep("Muj",ID2)
datos$busqueda[indice]<-"LO HE ENCONTRADO"

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
































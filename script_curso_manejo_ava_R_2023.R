
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


#######################################################
#######################################################
# FECHAS
#######################################################
#######################################################

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_R_2023/")

# Cargamos los datos del curso

load("datos/datos.curso1.RData")


# Formato fecha perfecto para R

class(datos$"fdiag_cm")
sum(is.na(datos$"fdiag_cm"))
unique(datos$"fdiag_cm")
table(datos$"fdiag_cm")

datos$"fechas.CM" <- as.Date(datos$"fdiag_cm")

class(datos$"fechas.CM")
sum(is.na(datos$"fechas.CM"))
datos$"fechas.CM"[1:6]
head(datos[,c("fdiag_cm","fechas.CM")])


# Otro formato 10.07.88

class(datos$"fdiag_cp")
sum(is.na(datos$"fdiag_cp"))
unique(datos$"fdiag_cp")

datos$"fechas.CP" <- as.Date(datos$"fdiag_cp",format="%d.%m.%y")

head(datos[,c("fdiag_cp","fechas.CP")])

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

dates <- c("01ene1960.", "02ene1960.", "31mar1960.", "30jul1960.")
as.Date(x=dates, format="%d%b%Y")

dates <- c("01jan1960.", "02jan1960.", "31mar1960.", "30jul1960.")
as.Date(x=dates, format="%d%b%Y")




dates <- c("01ene.1960", "02ene.1960", "31mar.1960", "30jul.1960")
as.Date(x=dates, format="%d%b%Y")

dates <- c("01jan.1960", "02jan.1960", "31mar.1960", "30jul.1960")
as.Date(x=dates, format="%d%b%Y")


dates <- c("01jan.1960")
as.Date(x=dates,format="%d%b%Y")

?strptime

###########################
# OPERACIONES CON FECHAS
###########################


datos$"dias"<-c(datos$"fechas.DF" - datos$"fechas.CM")
class(datos$"dias")


datos$"dias2"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="days")

datos$"weeks"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="weeks")
 
datos$"years"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="days") / 365.25
attr(datos$"years","units")="years"

# Solo para variables tipo Date

datos$"fecha.CP_NUEVA"<-as.character(format(datos$"fechas.CP",format="%b;%Y"))


# Formato fecha con horas, minutos y segundos

first <- "2022-08-20 08:15:22"
second<- "2022-08-20 08:17:22"
difftime( second,first, units="mins")

# Eliminar variables

datos$"fecha.CP_NUEVA"<-NULL


# Secuencia de fechas

datos_secuencia<-data.frame(fecha=seq(as.Date("1976-01-01"),to=as.Date("1976-03-01"),by="days"))


datos_infeccion<-data.frame(fecha=c("1976-02-01","1976-02-01","1976-02-01","1976-03-01","1976-03-01"),
infeccion=rep("Si",5))
table(datos_infeccion$fecha)



seq(as.Date("1976-01-01"),by="days",length=6)

seq(as.Date("1976-01-01"),to=as.Date("1976-03-01"),by="days")[1:6]

seq(as.Date("1976-01-01"),to=as.Date("1976-03-01"),by="2 weeks")[1:6]

seq(as.Date("1976-01-01"),to=as.Date("1976-03-01"),by="week")[1:6]


#######################################################
#######################################################
# CARACTERES
#######################################################
#######################################################

# nchar() # numero de caracteres (de la librería gdata)

nchar(as.character(datos$"ID"))
table(nchar(as.character(datos$"ID")))


# paste() # concatenar caracteres

datos$"id_combinado"<-paste(datos$"ID",datos$"sexo",sep="***")
head(datos[,c("ID","sexo","id_combinado")])


x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
paste(x,collapse="/")

datos$"ID_new"<-datos$"ID"
datos$"ID_new"[nchar(datos$"ID")==1]<- paste("00",datos$"ID_new"[nchar(datos$"ID")==1],sep="")
datos$"ID_new"[nchar(datos$"ID")==2]<- paste("0",datos$"ID_new"[nchar(datos$"ID")==2],sep="")

# sprintf Susana


paste("00",datos$"ID_new"[nchar(datos$"ID")==1]," ESTO ES NUEVO "," MIRA QUE BIEN",sep="")

paste0("00 ",datos$"ID_new"[nchar(datos$"ID")==1],"ESTO ES NUEVO"," MIRA QUE BIEN")


# strsplit() # Dividir los elementos de un vector de caracteres en subcadenas de acuerdo con criterio


res<-strsplit(datos$"fdiag_cm",split="-") # el resultado es un objeto tipo lista

datos$"fdiag_cm"[1]
res[[1]]

datos$"fdiag_cm"[2]
res[[2]]

datos$"fdiag_cm"[3]
res[[3]]

res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)

table(res$"year",exclude=NULL)
table(res$"month",exclude=NULL)
table(res$"day",exclude=NULL)

sort(unique(res$"year"))
sort(unique(res$"month"))


datos$"year"<-res$"year"
datos$"month"<-res$"month"
datos$"day"<-res$"day"

head(datos[,c("ID","fdiag_cm","year","month","day")])

res$fecha<-paste(res$"day",res$"month",res$"year",sep=".")
res$fecha[is.na(res$year)]<-NA

datos<-cbind(datos,res)





res<-strsplit(datos$"fdef",split="[.]")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)

# OTRAS COSAS

unique(interaction(datos$"estado.civil",datos$"sexo"))
table(interaction(datos$"estado.civil",datos$"sexo"))
paste(datos$"estado.civil",datos$"sexo",sep=".")


nombres<-c("María","Pachón")

nombres<-tolower(nombres)
# nombres<-toupper(nombres)

# sub gsub

sub("á","a",c("hólálá"))

nombres<-gsub("á","a",nombres)
nombres<-gsub("é","e",nombres)
nombres<-gsub("í","i",nombres)
nombres<-gsub("ó","o",nombres)
nombres<-gsub("ú","u",nombres)

nombres<-gsub("ñ","n",nombres)

nombres<-gsub("ü","u",nombres)
nombres<-gsub("ö","o",nombres)

# substring
table(nchar(as.character(datos$"ID")))
datos$ID_new<-substring(as.character(datos$"ID"),first=1,last=2)

datos$"month_new"<-substring(datos$"fdiag_cm",first=6,last=7)

# substring inverso

substring(datos$"fdiag_cm",first=nchar(datos$"fdiag_cm")-1,last=nchar(datos$"fdiag_cm"))

library("stringr")
str_sub(datos$"fdiag_cm",-2,-1)


# duplicados

which(duplicated(c("AA","BB","AA","AA","CC","DD","DD")))

datos$"ID"[4]<-200
head(datos)

table(duplicated(datos$"ID"))
length(datos$"ID")
length(unique(datos$"ID"))

datos$"duplicado_id"<-duplicated(datos$"ID")

datos[datos$"ID"%in%datos$"ID"[datos$"duplicado_id"],]

# Eliminar los duplicados

datos$"duplicado_perfecto"<-duplicated(paste(datos$"ID",datos$"edad",datos$"sexo",sep=""))
table(datos$"duplicado_perfecto")

# datos_new<-datos[-which(datos$"duplicado_perfecto"),]

# match y grep

nombres<-c("Enrique","Susana","Lidia","Estefanía","Enrique","EnriqueIV","enrique","Estefania")

match("Enrique",nombres)

grep("Enrique",nombres)


datos$"observaciones"<-"cardiovascular"
datos$"observaciones"[c(10,15,16)]<-"cardiovascular;cancer"

indice_cancer<-grep("cancer",datos$"observaciones")

datos$"cancer"<-0
datos$"cancer"[indice_cancer]<-1

datos$"infeccion"<-"No"
datos$"infeccion"[c(2,5,6)]<-"infección por rinovirus"
datos$"infeccion"[c(8,9,10)]<-"infección por rhinovirus"

busqueda1<-grep("rinovirus",datos$"infeccion")
busqueda2<-grep("rhinovirus",datos$"infeccion")

busqueda_total<-unique(c(busqueda1,busqueda2))

datos$"rinovirus"<-0
datos$"rinovirus"[busqueda_total]<-1

###################
# FUNCIONES
###################


normalizacion <- function(var){
	
	variable<-var
	
	variable<-toupper(variable)
	
	variable<-gsub("Ó","O",variable)
	variable<-gsub("Í","I",variable)
	variable<-gsub("Á","A",variable)	
	variable<-gsub("É","E",variable)	
	variable<-gsub("Ú","U",variable)	
	
	
	variable<-gsub("Ü","U",variable)
	variable<-gsub("Ñ","N",variable)
	
	variable<-gsub(" ","_",variable)
	
	
	return(variable)
	
}

datos$"diagnosticos"<-"Cáncer"
datos$"diagnosticos"[c(1,5,8)]<-c("Cáncer,Cardiovascular, Cigüeña, España")


normalizacion(var=datos$"diagnosticos")


datos$"diag_norm"<-normalizacion(var=datos$"diagnosticos")


normalizacion_total <- function(x,valor=10){
	
	
	if(class(x)=="numeric"){
	
		res<-x/valor
	
	}
	
	if(class(x)=="character"){
		
		variable<-x
		
		variable<-toupper(variable)
	
		variable<-gsub("Ó","O",variable)
		variable<-gsub("Í","I",variable)
		variable<-gsub("Á","A",variable)	
		variable<-gsub("É","E",variable)	
		variable<-gsub("Ú","U",variable)	
	
	
		variable<-gsub("Ü","U",variable)
		variable<-gsub("Ñ","N",variable)
	
		variable<-gsub(" ","_",variable)
		
		res<-variable
	
	}
	
	if(class(x)=="Date"){
		
		res<-x-5  # si es fecha que le reste 5 dias
	
	}
	
	
	return(res)
		
}

normalizacion_total(x=datos$"estado.civil")


normalizacion_total(x=datos$"edad")


normalizacion_total(x=as.Date(datos$"fdiag_cm"))


normalizacion_total(x=datos$"edad",valor=100)

#################################################

rm(list=ls())
gc()

source("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_R_2023/scripts/mis.funciones_curso.R")

setwd("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_R_2023/")

# Cargamos los datos del curso

load("datos/datos.curso1.RData")


normalizacion_total(x=datos$"edad",valor=100)



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

date() # "Tue Oct 22 10:03:19 2024"

# Cargar / importar los datos

# setwd("/Users/pfernandezn/Desktop/MADR/") # establezco la ruta principal

setwd("C:/Users/usuario/Desktop/MADR/")

load("datos/datos.curso1.RData") # cargo los datos del curso


ls() # listar los nombres de los objetos creados o cargados/importados en la sesion

dim(datos)
head(datos)
head(datos[,c(1:10)])
str(datos)


datos.evaluacion <- read.table(file="datos.evaluacion.txt",header=TRUE,sep="\t",encoding="UTF-8")

dim(datos.evaluacion)

head(datos.evaluacion)

str(datos.evaluacion)



#library("openxlsx")

#datos.importar <- read.xlsx("")




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


####################################################
# FACTOR
####################################################

####################################################
# as.factor
####################################################

class(datos$"estado.civil")

table(datos$"estado.civil",exclude=NULL)

datos$"estado.civil.factor" <- as.factor(datos$"estado.civil")

table(datos$"estado.civil.factor",exclude=NULL)

####################################################
# levels
####################################################

levels(datos$"estado.civil.factor")

####################################################
# factor
####################################################

datos$"estado.civil.factor.new" <- factor(datos$"estado.civil.factor",
											levels=c("Soltero","Casado","Divorciado"))


levels(datos$"estado.civil.factor.new")

####################################################
# levels (recodificacion etiquetas)
####################################################

levels(datos$"estado.civil.factor") <- c("cas", "div", "sol")

levels(datos$"estado.civil.factor")[2]<-"Div.new"


####################################################
# Combinacion de variables tipo factor
####################################################

datos$"estado.civil"<-as.factor(datos$"estado.civil")
datos$"nivel.estudios" <- as.factor(datos$"nivel.estudios")

datos$"comb"<-interaction(datos$"estado.civil", datos$"nivel.estudios")

table(datos$"comb",exclude=NULL)


#####################################################
#####################################################
# FECHAS
#####################################################
#####################################################

####################################################
# Forma de presentar las fechas = 1977-05-05
####################################################

class(datos$"fdiag_cm")

sum(is.na(datos$"fdiag_cm"))

unique(datos$"fdiag_cm")

datos$"fechas.CM" <- as.Date(datos$"fdiag_cm")

sum(is.na(datos$"fechas.CM"))

####################################################
# Otra forma de presentar las fechas 10.07.88
####################################################

class(datos$"fdiag_cp")
sum(is.na(datos$"fdiag_cp"))
unique(datos$"fdiag_cp")

####################################################
# datos$"fechas.CP" <- as.Date(datos$"fdiag_cp")
####################################################


datos$"fechas.CP" <- as.Date(datos$"fdiag_cp",format="%d.%m.%y")

sum(is.na(datos$"fechas.CP"))


####################################################
# Otro forma de presentar las fechas 2007.12.13
####################################################

class(datos$"fdef")
sum(is.na(datos$"fdef"))
unique(datos$"fdef")

datos$"fechas.DF" <- as.Date(datos$"fdef",format="%Y.%m.%d")

class(datos$"fechas.DF")
sum(is.na(datos$"fechas.DF"))
unique(datos$"fechas.DF")

####################################################
# CALCULOS
####################################################

datos$"dias"<-c(datos$"fechas.DF" - datos$"fechas.CM")
class(datos$"dias")

datos$"dias2"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="days")

datos$"weeks"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="weeks")
 
datos$"years"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="days") / 365.25
attr(datos$"years","units")="years"

datos$"fecha.DF_rev"<-datos$"fechas.DF" - 5

####################################################
# Casos raros que pueden no funcionar
####################################################

dates <- c("01ene1960", "02ene1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")

dates <- c("01jan1960", "02jan1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")



####################################################
# Presentacion de las fechas como yo quiera (CUIDADO: se convierte en caracter)
####################################################

datos$DF_new <- format(datos$"fechas.DF",format="%m/%Y")

datos$DF_new2 <- format(datos$"fechas.DF",format="%d-%m-%Y")

datos$DF_new3 <- format(datos$"fechas.DF",format="%d-%B-%Y")

datos$DF_new4 <- format(datos$"fechas.DF",format="%B")
table(datos$DF_new4)


#####################################################
#####################################################
# CARACTERES
#####################################################
#####################################################

# nchar() # numero de caracteres (de la librería gdata)

class(datos$"ID")

nchar(as.character(datos$"ID"))

table(nchar(as.character(datos$"ID")))


# paste() # concatenar caracteres

x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")

paste(x,collapse="/")


datos$"id_combinado"<-paste(datos$"ID",datos$"sexo",sep="***")

paste(datos$"ID",datos$"sexo",datos$"edad",sep="***")


#  paste0

paste0(datos$"ID","-",datos$"sexo","***",datos$edad,"----")

paste0(datos$"ID",datos$"sexo")
paste(datos$"ID",datos$"sexo",sep="")

datos$id.unico<-paste0(datos$"ID",datos$"sexo",datos$"edad")



# strsplit() # Dividir los elementos de un vector de caracteres en subcadenas de acuerdo con criterio

res<-strsplit(datos$"fdiag_cm",split="-")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)
datos<-cbind(datos,res)
table(nchar(res$"year"))
table(nchar(res$"month"))



res<-strsplit(datos$"fdef",split="[.]")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)


# Lo que hablamos de combinar variables

interaction(datos$estado.civil,datos$sexo)

table(interaction(datos$estado.civil,datos$sexo))

as.factor(paste(datos$"estado.civil",datos$"sexo",sep="."))



# gsub

nombres<-c("María","Pachón")

nombres<-tolower(nombres)  # toupper

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

gsub("-","",datos$"fdiag_cm")
nchar(gsub("-","",datos$"fdiag_cm"))

datos$"fdiag_cp_nuevo"<-gsub("[.]","",datos$"fdiag_cp")


# Unique

length(datos$"ID")
length(unique(datos$"ID"))

unique(datos$"nivel.estudios")
sort(unique(datos$"year"))

# duplicated

duplicated(datos$"ID")
table(duplicated(datos$"ID"))



# Grep

ID_new<-paste(datos$"ID",datos$"sexo",sep=".")
ID2<-gsub("[.]","",ID_new)

indice<-grep("Muj",ID2)

datos$"busqueda"<-"NO MUJ"
datos$busqueda[indice]<-"SI MUJ"

mujeres <- datos[indice , ]

grep(x=ID2,pattern="Muj")


#############################################
#############################################
# FUNCIONES
#############################################
#############################################


nombres<-c("María","Pachón")



recode <- function(var){

	var_new<-tolower(var)
	var_new<-gsub("á","a",var_new)
	var_new<-gsub("é","e",var_new)
	var_new<-gsub("í","i",var_new)
	var_new<-gsub("ó","o",var_new)
	var_new<-gsub("ú","u",var_new)

	var_new<-gsub("ñ","n",var_new)

	var_new<-gsub("ü","u",var_new)
	var_new<-gsub("ö","o",var_new)

	return(var_new)	

}


recode(var=nombres)


# SOURCE

source("C:/Users/usuario/Desktop/MADR/jaime_func.R")


test_puntos <- function(a, b, ...){ 

   print(paste(a,b,..., sep=" ")) 

} 

test_puntos(a= "AA", b="BB", "otro text") #"otro text"




mi.funcion <- function(x){
	
	res<-x/10
	
	return(res)
		
}

datos$"edad_new"<-mi.funcion(datos$"edad")



mi.funcion2 <- function(x,valor=10){
	
	print(class(x))	

	if(class(x)=="numeric"){
	
		res<-x/valor
	
	}
	
	if(class(x)!="numeric"){
		
		res<-"no puedo hacer esa operacion"
	
	}
	
	if(class(x)=="Date"){
		
		res<-x-5  # si es fecha que le reste 5 dias
	
	}
	
	print("YA HE TERMINADO")	

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

dim(datos)[1] # 200


for (i in 1:dim(datos)[1]){
	
	print(i)
	datos$"edad_new"[i]<-datos$"edad"[i]/10
		
}

datos$edad/10



while(x>10){
	
	print(" seguir")	
	
	
}



apply(datos[,c("peso","altura")],2,function(x) x/10)



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
	
	variable<-gsub("Ã“","O",variable)
	variable<-gsub("Ã","I",variable)
	variable<-gsub("Ã","A",variable)	
	variable<-gsub("Ã‰","E",variable)	
	variable<-gsub("Ãš","U",variable)	
	
	return(variable)
	
}


datos_depurados<-apply(datos[,c("sexo","estado.civil","nivel.estudios")],2,
function(x) prepara_character(x))

datos<-cbind(datos,datos_depurados)


for(i in c(3,4,5)){

	print(i)

	datos[,i]<-prepara_character(datos[,i])

}  



datos$missing<-apply(datos,1,function(x) sum(is.na(x)))


apply(datos,2,function(x) sum(is.na(x)))
apply(datos,1,function(x) sum(is.na(x)))


##############
# Tiempos
##############

inicio<-Sys.time()


# Funciones y ejecuciones


final<-Sys.time()

final-inicio

difftime(final,inicio,units="mins")





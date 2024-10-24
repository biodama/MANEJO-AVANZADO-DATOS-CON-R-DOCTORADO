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

library("openxlsx")

cristina<-read.xlsx("prueba.xlsx",sheet=1)

str(cristina)

cristina<-read.csv("prueba.csv",header=T,sep=",",encoding="UTF-8")

str(cristina)

library("readxl")

cristina<-read_excel("prueba.xlsx",col_types=rep("text",19))


#read.table
#read.csv
#read.csv2

#Otras librarias de excel

#Mirar nuestro propio excel

#Mirar otras exportaciones (tipos)

#Mirar con data.table

library(data.table)

DT_curso <- fread("/Users/pfernandezn/Desktop/MADR/datos/datos.evaluacion.txt")


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

nombres<-c("María","Ramón")

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
	
	variable<-gsub("A","O",variable)
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


#####################################################
#####################################################
# FILTROS Y CALCULOS
#####################################################
#####################################################

#######################
# Hacer Subsets
#######################

hombres_casados <- datos[datos$"sexo"=="Hombre" & datos$estado.civil=="Casado" ,  ]

dim(hombres_casados)


# subset data.frame

dat1 <- subset(datos,sexo=="Hombre" & estado.civil=="Casado")

dat2 <- subset(datos,peso<58)

dat3 <- subset(datos,peso<58, select = c(ID,peso,sexo))
 
 
# data.table 
 
 
library(data.table) 

DT <- as.data.table(datos)

DT

# subset data.table

dat4 <- subset(DT,peso<58)

DT$"imc_clasico" <- DT$peso/c(DT$altura/100)^2

# Extra!!!!!!!!!!!!!!
DT[,imc:=peso/(altura/100)^2] # cálculo del índice de masa corporal
DT



# Calculos de promedios e incorporar este calculo a una variable
mean(DT$imc)
DT$exceso.imc <- datos$imc / mean(imc)

DT[,exceso.imc := imc / mean(imc)]
DT

DT[,exceso.imc := imc / mean(imc) , by=nivel.estudios]
DT


# fechas de cm
# fechas de df
# dias
# por estado civil y nivel estudios






#####################################################
#####################################################
# COMBINACION....
#####################################################
#####################################################


DT$altura

DT[,exceso.imc := imc / mean(imc)]



require(data.table) # simplifica la manipulación de data.frame

DF1=data.table(id="Luke Skywalker", altura=172, color.ojos="azul")
DF2=data.table(id="Darth Vader", altura=202, color.ojos="amarillo")
DF3=data.table(id="Leia Organa", altura=150, color.ojos="marrón")

rbind(DF1,DF2,DF3)

hombres<-subset(DT,sexo=="Hombre")
mujeres<-subset(DT,sexo=="Mujer")

# !!!!!!!!!!!!!!!!!! require estudiar las bases de datos previamente a ser unidas.   formatos, dimensiones,........

ambos <- rbind (hombres,mujeres)


#### SPLIT

hombres_casados <- subset(DT, sexo=="Hombre" & estado.civil=="Casado")
hombres_solteros <- subset(DT, sexo=="Hombre" & estado.civil=="Soltero")
#..........


estratos_sexo <- split(DT, by="sexo")

estratos_sexo$"Hombre"[,imc:=peso/(altura/100)^2]

estratos_sexo_ec <- split(DT, by=c("sexo","estado.civil"))

names(estratos_sexo_ec)

estratos_sexo_ec$"Mujer.Casado"


# Merge

DT1=data.table(id=c("C-3PO","R2-D2","Chewbacca"),altura=c(167,96,228))
DT2=data.table(id=c("C-3PO","R2-D2"), peso=c(75,32))

length(DT1$id)
length(unique(DT1$id))
class(DT1$id)

length(DT2$id)
length(unique(DT2$id))
class(DT2$id)

length(intersect(DT1$id,DT2$id))
length(setdiff(DT1$id,DT2$id))
length(setdiff(DT2$id,DT1$id))


union<-merge(DT1,DT2,by="id",all.x=TRUE,all.y=TRUE)


DT1=data.table(index=c("C-3PO","R2-D2","Chewbacca"),altura=c(167,96,228))
DT2=data.table(id=c("C-3PO","R2-D2"), peso=c(75,32))

# names(DT1)[1]<-"id"

union<-merge(DT1,DT2,all.x=TRUE,all.y=TRUE,by.x="index",by.y="id")

# union2<-merge(union,DT3,.....)



# FORMATO WIDE /LONG (data.table)

wide <- subset(DT, sexo=="Mujer", select=c(ID,edad,sexo,fdiag_cm,fdef))
head(wide)

# Dos formas

long <- melt(wide, id=1:3) # id : variables que se quedan fijas

long <- melt(wide, measure=4:5) # measure : identifico las variables a unir

retorno_a_wide <- dcast(long, ID + sexo + edad ~ variable)

head(retorno_a_wide)


guille<-data.table(index=c("C-3PO","R2-D2","Chewbacca"),altura=c(167,96,228),
hg=c(10,23,55),cd=c(44,55,67),pb=c(4,3,5),as=c(100,300,400))

guille_long <- melt(guille,id=1:2)


##################################################

# VARIADO

library(tibble)

TD<-as_tibble(datos)



# Quitar las tildes,... de todas las variables tipo caracter

quitar_todo<-function(var){
		
	if(class(var)=="character"){
		
		var_new<-tolower(var)
		var_new<-gsub("á","a",var_new)
		var_new<-gsub("é","e",var_new)
		var_new<-gsub("í","i",var_new)
		var_new<-gsub("ó","o",var_new)
		var_new<-gsub("ú","u",var_new)

		var_new<-gsub("ñ","n",var_new)

		var_new<-gsub("ü","u",var_new)
		var_new<-gsub("ö","o",var_new)
	}
	
	if(class(var)=="factor"){
		
		var_new<-var
		
		niveles<-levels(var_new)
		
		niveles_new<-tolower(niveles)
		niveles_new<-gsub("á","a",niveles_new)
		niveles_new<-gsub("é","e",niveles_new)
		niveles_new<-gsub("í","i",niveles_new)
		niveles_new<-gsub("ó","o",niveles_new)
		niveles_new<-gsub("ú","u",niveles_new)

		niveles_new<-gsub("ñ","n",niveles_new)

		niveles_new<-gsub("ü","u",niveles_new)
		niveles_new<-gsub("ö","o",niveles_new)
		
		levels(var_new)<-niveles_new
			
	}
	
	if(class(var)=="numeric"){
		
		print("aqui no hago nada")
		var_new<-var
		
	}

	return(var_new)	
	
	
}

datos$"estado.civil.factor"<-as.factor(datos$"estado.civil")
datos$"nivel.estudios.new"<-datos$"nivel.estudios"
datos$"nivel.estudios.new"[datos$"nivel.estudios.new"%in%"Alto"]<-"Altísimo"
datos$"nivel.estudios.new"[datos$"nivel.estudios.new"%in%"Bajo"]<-"Bajísimo"



var<-datos$"nivel.estudios.new"
var<-datos$"estado.civil.factor"


check1<-quitar_todo(var=datos$"nivel.estudios.new")
check2<-quitar_todo(var=datos$"estado.civil.factor")
check3<-quitar_todo(var=datos$"ID")

# datos$sexo<-quitar_todo(var=datos$sexo)

# Apply

datos.new<-datos

datos.new[,c(3,4,5)]<-apply(datos.new[,c(3,4,5)],2,function(x) quitar_todo(x))


# Formato loop

datos.new<-datos

for(m in 1:dim(datos)[2]){
	
	print(m)
	datos.new[ , m ]<-quitar_todo(var=datos.new[, m ])
		
}



datos.new<-datos
variables<-names(datos)

for(n in variables){
	
	print(n)
	datos.new[,n]<-quitar_todo(var=datos.new[,n])
	
	
}


# Recodificacion

datos$"edad_gr"<-ifelse(datos$"edad">=50, "Mayor", "Menor")

datos$"edad"[c(1,2,3)]<-NA

datos$"edad_gr"<-ifelse(datos$"edad">=50, "Mayor", "Menor")

class(datos$"edad_gr")


datos_codigos<-data.frame(ID=c(1,2,3),CIE=c("C100.1","C200.1","C300.1"))

tabulacion<-function(x){
	
	cie <- as.character(x)
	cod <-rep(0,length(cie))
	cod <- ifelse(cie=="C100.1",			"CANCER",cod)
	cod <- ifelse(cie=="C200.1" ,			"CARDIO",cod)
	cod <- ifelse(cie=="C300.1",			"DIAB",cod)
	
	cod
	
}
tabulacion(x=datos_codigos$CIE)

####################################

datos_casos<-data.frame(codINE=c(10,12,34,300),casos=c(100,200,300,1000))

datos_pob<-data.frame(INE=c(34,10,12,14),pob=c(1000,3000,1500,5000))

union<-merge(datos_pob,datos_casos,by.x="INE",by.y="codINE",all.x=T,all.y=T)

union$TI<-union$casos/union$pob


####################################

datos$"fechas.CM" <- as.Date(datos$"fdiag_cm")
datos$"fechas.DF" <- as.Date(datos$"fdef",format="%Y.%m.%d")
datos$"dias"<-datos$"fechas.DF"-datos$"fechas.CM"


DT<-as.data.table(datos)

mujeres<-subset(DT,sexo=="Mujer" & cancer.mama=="Si")
mujeres[,.(super = median(dias) ),by=.(estado.civil,nivel.estudios)]

median(mujeres$dias[mujeres$estado.civil=="Soltero" & mujeres$nivel.estudios=="Alto"])


##################################
# Caso evaluacion
##################################

setwd("/Users/pfernandezn/Desktop/MADR/") 

datos.evaluacion <- read.table(file="datos.evaluacion.txt",header=TRUE,sep="\t",encoding="UTF-8")

# library(data.table)
# DT<-fread("datos/datos.evaluacion.txt")

dim(datos.evaluacion)

head(datos.evaluacion)

str(datos.evaluacion)


DT$talla <- ifelse(DT$altura >= 160, "XXL", "XL")
table(DT$talla,exclude=NULL)


nchar(DT$ID)
DT$"ID_new" <- paste0(DT$ID,".",DT$sexo,".",DT$edad)
# paste(DT$ID,DT$sexo,DT$edad,sep=".")


res<-strsplit(DT$"ID_new",split="_")

res<-do.call(rbind.data.frame, res)
dim(res)
names(res)<-c("ID_esp","lo otro")
DT$"ID_esp"<-res$"ID_esp"



table(DT$complicaciones)

DT$"comp_min"<-tolower(DT$"complicaciones")

busqueda <- grep("infarto",DT$"comp_min")

DT$"infarto"<-"No"

DT$"infarto"[busqueda]<-"Si"


# Busquedas de varios terminos

posibles_valores<-c("infarto","infartado")

busqueda_a <- grep(posibles_valores[1],DT$"comp_min")
busqueda_b <- grep(posibles_valores[2],DT$"comp_min")

busqueda<-unique(busqueda_a,busqueda_b)


unique(DT$"comp_min" [grep("infarto", DT$"comp_min")])



library("stringr") 

str_detect(DT$"comp_min", "inf") 

str_extract(DT$"comp_min", "inf")

str_match(DT$"comp_min", "inf")


# Extract/match all

str_extract_all(DT$"comp_min", "inf")

str_match_all(DT$"comp_min", "inf")


# Importar bases de datos

library(data.table)

datos_genes <- fread("datos/datos.evaluacion.genes.txt")

datos_biomarcadores <- fread("datos/datos.evaluacion.biomarcadores.txt")


# Unir bases de datos

DT
datos_genes
datos_biomarcadores

length(DT$"ID")
length(unique(DT$"ID"))

length(datos_genes$"ID")
length(unique(datos_genes$"ID"))

length(datos_biomarcadores$"ID")
length(unique(datos_biomarcadores$"ID"))


length(intersect(DT$"ID",datos_genes$"ID"))

length(intersect(DT$"ID",datos_biomarcadores$"ID"))


union_1<-merge(DT,datos_genes,by= "ID", all.x = TRUE, all.y = TRUE)

union <- merge(union_1,datos_biomarcadores,by= "ID", all.x = TRUE, all.y = TRUE)


# Formato long

union_new<-union[,c("ID","Al","Hg","Cd","Be","Se","Cu","As")]

datos_long_union <- melt(union_new,id=1)

names(datos_long_union)[c(2,3)]<-c("Biomarcador","Concentracion")



# Para toda la base de datos

datos_long_union <- melt(union,measure=120:126)



# Cosas para los SNPs 

union$"SNP_1" <- gsub(" ", "",union$"SNP_1")
union$"SNP_1" <- gsub("", " ",union$"SNP_1")


# Tabulaciones

check<- c("	homa	adios")
gsub("\t","",check)

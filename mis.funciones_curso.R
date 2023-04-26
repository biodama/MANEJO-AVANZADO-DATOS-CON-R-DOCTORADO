# Funcion para normalizar inicial

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

# Funcion para normalizar total

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

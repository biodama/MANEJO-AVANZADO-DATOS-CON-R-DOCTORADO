
# Mis funciones

mi.funcion <- function(x){
	
	res<-x/10
	
	return(res)
		
}


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

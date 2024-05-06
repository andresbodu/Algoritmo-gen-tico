library(readxl)
library(igraph)
Datos_Ruteo <- read_excel("Datos Ruteo.xlsx")

n <- 32 #numero de ciudades
dep <- 1 #número de depositos
dj <- Datos_Ruteo$`Demanda (uds.)`[-1] #demandas
Q <- 100 # Capacidad de un vehiculo
tampo <- 20
m <- 4
gen <- 100
iter <- gen*tampo
tc <- 0.8
tm <- 0.2

distancias <- matrix(0,nrow = (n+dep),ncol = (n+dep),byrow = T)
for (i in 1:(n+dep)) {
  for (j in 1:(n+dep)) {
    a <- Datos_Ruteo$x[j]-Datos_Ruteo$x[i]
    b <- Datos_Ruteo$y[j]-Datos_Ruteo$y[i]
    distancias[i,j] <- sqrt((a^2)+(b^2))  
  }
}


rut <- function(x){
  bodega <- Q
  ruta <- c(0)
  for (i in 1:n) {
    if (bodega>= dj[x[i]]) {
      ruta <- c(ruta,x[i])
      bodega <- bodega-dj[x[i]]
    } else{
      ruta <- c(ruta,0,x[i])
      bodega <- Q-dj[x[i]]
    }
  }
  ruta <- c(ruta,0)
  ruta <- ruta+1
  return(ruta)
}
f.o <- function(x){
  ruta <- rut(x)
  distan1 <- 0
  for (i in 1:(length(ruta)-1)) {
    distan1[i] <- distancias[ruta[i],ruta[(i+1)]]  
  }
  EV <- sum(distan1)
  return(EV)
}
poblacion = matrix(0, nrow = tampo, ncol = n)
for (i in 1:tampo) {
  poblacion[i,] <- sample(1:n, n, replace = F)
}
EV = apply(poblacion, 1, f.o)
selector <- function(){
  muestra = sample(1:tampo, m)
  
  EV[muestra]
  
  mejor <- which.min(EV[muestra])
  
  posi <- muestra[mejor]
  
  return(poblacion[posi,])
}
cruzamiento=function(pa,ma){
  corte1 <- sample(1:(n-1),1)
  excluir <- c(corte1-1,corte1,corte1+1)
  conj <- setdiff(1:(n-1),excluir)
  corte2 <- sample(conj,1)
  pc <- c(corte1,corte2)
  
  extr_iz1 <- pa[1:min(pc)]
  extr_dere1 <- pa[-(1:max(pc))]
  vector_temp1 <- c(extr_iz1,extr_dere1)
  
  cont <- 0
  for(i in 1:n){
    cont[i] <- sum(ma[i]==vector_temp1)
    pos1 <- which(cont==0)
    extr_cent1 <- c(ma[pos1])
  }
  
  hijo1 <- c(extr_iz1,extr_cent1,extr_dere1)
  
  extr_iz2 <- ma[1:min(pc)]
  extr_dere2 <- ma[-(1:max(pc))]
  vector_temp2 <- c(extr_iz2,extr_dere2)
  
  cont <- 0
  for(i in 1:n){
    cont[i] <- sum(pa[i]==vector_temp2)
    pos2 <- which(cont==0)
    extr_cent2 <- c(pa[pos2])  
    
    
  }
  hijo2 <- c(extr_iz2,extr_cent2,extr_dere2)
  if(runif(1)<0.5){
    hijo_select <- hijo1
  }else{
    hijo_select <- hijo2
  }
  
  return(hijo_select)
}
mutacion <- function(hijo){
  cambio <- sample(1:n,2,replace = F)
  provi <- hijo[cambio[1]]
  hijo[cambio[1]] <- hijo[cambio[2]]
  hijo[cambio[2]] <- provi
  return(hijo)
}

t <- proc.time()
x <- 0
for (i in 1:iter) {
  pa <- selector()
  ma <- selector()
  
  if(runif(1)<=tc){
    hijo_select <- cruzamiento(pa,ma)
  }else{
    hijo_1 <- pa
    hijo_2 <- ma
    if (runif(1)<=0.5) {
      hijo_select <- hijo_1
    }else{
      hijo_select <- hijo_2
    }
  }
  
  
  #Mutacion
  if(runif(1)<=tm){
    hijo_mutado <- mutacion(hijo_select)
  }else{
    hijo_mutado <- hijo_select
  }
  
  #Ingreso del hijo a la población
  existe = 0
  total=matrix(0,nrow = tampo,ncol = n)
  for (k in 1:tampo) {
    total[k,]=abs(hijo_mutado-poblacion[k,])
    if(sum(total[k,])==0){
      existe=1
      break
    }
  }
  
  if(existe==0 & f.o(hijo_mutado)<max(EV)){
    pos_reem <- which.max(EV)
    poblacion[pos_reem,]=hijo_mutado
    EV[pos_reem]=f.o(hijo_mutado)
  }
  
  if(i%%tampo==0){
    x[i/tampo] <- min(EV)
  }
}
proc.time() - t
EV
poblacion
dis_incumbente <- min(EV)
ruta <- rut(poblacion[which.min(EV),])
plot(x)

enlaces <- matrix(NA, ncol = 2, nrow = length(ruta) - 1)
for (i in 1:(length(ruta) - 1)) {
  enlaces[i, ] <- c(ruta[i], ruta[i + 1])
}

red <- graph_from_edgelist(enlaces, directed = FALSE)
plot(red)

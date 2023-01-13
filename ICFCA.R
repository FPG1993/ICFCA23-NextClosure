genmatrices<-function(atrib,obj,dens,tas,n){
  if (n<=0){
    return("dame un número de matrices correcto")
  }
  else{
  vector<-c(sample(c(-1,0,1),replace=TRUE,prob=c(dens*tas/100,1-dens,dens-dens*tas/100),obj*atrib))
  matrices<-list(matrix(nrow=obj,ncol=atrib,vector,byrow=TRUE))
  if (n>1){
  for(i in 2:n){
    vector<-c(sample(c(-1,0,1),replace=TRUE,prob=c(dens*tas/100,1-dens,dens-dens*tas/100),obj*atrib))
    matrices[i]<-list(matrix(nrow=obj,ncol=atrib,vector,byrow=TRUE))
  }
  }
  return(matrices)
  }
}

prueba<-function(i){
  for(j in c(5,10,20,50)){
  for(k in c(0.1,0.25,0.5)){
  for(l in c(10,25,50)){
    lista<-genmatrices(i,j,k,l,100)
saveRDS(lista,file=paste("combinacion",toString(i),"atributos",toString(j),"objetos",toString(k),"densidad",toString(l),"tasanegativo",".rds"))
        }
  }
  }
    }

documentos<-function(atrib){
  for(i in c(5,10,20,50)){
    for(j in c(0.1,0.25,0.5)){
      for(k in c(10,25,50)){
        doc<-genmatrices(atrib,i,j,k,100)
        saveRDS(doc,file=cbind("comb",toString(atrib),"atrib",toString(i),"obj",toString(j),"dens",toString(k),"tas"))
      }
    }
  }
}
join_parroquias <- function(datos, df.poblacion){
  tipo <- lapply(datos, function(x){
    #unlist(x)
    t <- ifelse(x==df.poblacion[["DPA_PARROQ"]], df.poblacion[["TIPO"]], NA)
    ifelse(which(!is.na(t)), t[which(!is.na(t))], "HOO")
  })
  tipo
}
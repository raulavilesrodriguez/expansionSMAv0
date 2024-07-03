grades_to_decimal <- function(datos){
  result <- sapply(datos, function(x){
    x <- unlist(x)
    degrees <- as.numeric(x[1])
    minutes <- as.numeric(x[2])
    seconds <- as.numeric(x[3])
    direction <- x[5]
    # Convert to decimal degrees
    decimal_degrees <- degrees + minutes / 60 + seconds / 3600
    if(direction == 'S' | direction == 'W'){
      decimal_degrees <- -decimal_degrees
    } else{
      decimal_degrees
    }
  })
  result
}


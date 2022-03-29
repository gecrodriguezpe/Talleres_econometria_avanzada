#### Example of a function with a varible number of arguments #### 

# Función que calcular la media y la varianza de un número arbitrario de vectores 
many_means <- function (...){
  data = list(...) # Almaceno todos los vectores proveídos en una lista 
  n = length(data) # n es el número de vectores que se provee como argumentos de la función 
  means = numeric(n) # vector que almacena la media de los vectores
  vars = numeric(n) # vector que almacena la varianza de los vectores
  # Loop para calcular la media y varianza de cada vector
  for (i in 1:n) {
    means[i] = mean(data[[i]])
    vars[i] = var(data[[i]])
  }
  print(means)
  print(vars)
  invisible(NULL)
}

# ... is the only argument of the function
# allows ... the function to accept additional
# arguments of unspecified name and number, 
# and this introduces tremendous flexibility into the 
# structure and behaviour of functions. 

# Vectore de prueba para prover a la función 
vect1 = 1:5
vect2 = 6:10
vect3 = 11:15
vect4 = 20:100

# Nota: Aparte para saber como funciona el comando cbind
matriz = cbind(vect1, vect2, vect3) # Es posible combinar varios vectores al mismo tiempo

many_means(vect1, vect2, vect3, vect4)

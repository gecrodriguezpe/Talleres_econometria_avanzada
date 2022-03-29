library(tidyverse)

# Definir la semilla para reproducibilidad en los resultados
set.seed(12345)

# Punto 3 ----

# Ejercicio a. ----

# Switching regression: 
## y_{i} = y_{i}^{1} * D_{i} + (1 - D_{i}) * y_{i}^{0}
## y_{i} = y_{i}^{0} + (y_{i}^{1} - y_{i}^{0}) * D_{i}
## y_{i} = delta * D_{i} + epsilon_{i}

# Donde el parámetro causal: 
## delta = E[y_{i}^{1} | D_{i} = 1] - E[y_{i}^{0} | D_{i} = 0]
## delta = 3 - 0
## delta = 3 

# Ejercicio b. ----

# Nota: Dado las condiciones para simular y_0 = y_{i}^{0} y y_1 = y_{i}^{2},
#       uno esperaría que el SDO = 3 y por tanto el delta = 3
#       Ademas, dado que el SB = 0 (teniendo en cuenta que y_0 y y_1 son independientes de D)
#       se tiene que delta representa el impcto causal del programa del ministerio

# I. ----

# Definición de parámetros para la simulación 

## Número de muestras: 
t = 100

## Tamaño de muestras: 

n10 = 10
n20 = 20
n50 = 50
n100 = 100
n500 = 500
n1000 = 1000

#__________________________
# Funciones auxiliares: 
#__________________________

# Cálculo del SDO: (SDO := Simple difference of outcomes) (Función auxiliar)
SDO_calculo = function(Y, D){
  # Por construcción
  ## Y = Y_0 Si D = 0
  ## Y = Y_1 Si D = 1
  # Nota: Y y D son vectores del mismo tamaño
  # Inicialización de contadores
  count0 = 0 
  count1 = 0
  # Inicialización de la suma para cálculo del SDO
  suma0 = 0
  suma1 = 0
  # Iteración a través de la muestra   
  for (i in 1:length(Y)){
    if (D[i] == 0){
      suma0 = suma0 + Y[i]
      count0 = count0 + 1
    }else{
      suma1 = suma1 + Y[i]
      count1 = count1 + 1
    }
  }
  SDO = (suma1/count1) -(suma0/count0) # SDO: Simple diferencia de muestras
  return(SDO)
}

# Construcción matriz X para un modelo de regresión lineal con constante: (Función auxiliar)
ols_X = function(...){
  # ... contiene los regresores necesarios para construir la matriz X
  regresores = list(...)
  # Nota: Cada regresor es un vector con el mismo número de observaciones n
  n = length(regresores[[1]]) # No importa que se tome el primer regresor dado que todos los regresores tienen el mismo tamaño 
  const = rep(1, times = n)
  X = cbind(const, ...)
  return(X)
}

# Construcción del estimador \Hat{sigma^{2}}: (Función auxiliar)
estimador_sigma = function(y, X, beta_est){
  # Variables: 
  ## y: es el vector que contiene la variable dependiente
  ## X: es la matriz que contiene las variables regresoras 
  ## beta_est: es el vector de los parámetros estimados por OLS 
  e = y - X %*% beta_est # e es el vector de residuales 
  k = ncol(X)  # El número de parámetros a estimar      
  n = length(y)  # El número de observaciones en la mustra
  sigma_est = (t(e) %*% e) / (n - k)   # estimador de sigma (varianza del término de error)
  # El resultado de la función es: sigma_est (que es un escalar)
  return(sigma_est)
}

# Construcción del estimador para la matriz \Hat{Q}_{XX}^{-1}: (Función auxiliar)
estimador_Q_xx_inv = function(X){
  n = nrow(X) # donde n es el tamaño de la muestra 
  Q = solve((1/n) * (t(X) %*% X)) # Q es el estimador que se está buscando
  # Q es una matriz de tamaño k x k, donde k es en número de parámetros a estimar 
  # La función me retorna a una matriz Q de tamaño k x k
  return(Q)
}

# Construcción del estimador de la  matriz de varianzas y covarianzas 
# Huber-White: (Función auxiliar)
Huber_White = function(y, X, beta_est){
  # Variables: 
  ## y: es el vector que contiene la variable dependiente
  ## X: es la matriz que contiene las variables regresoras 
  ## beta_est: es el vector de los parámetros estimados por OLS 
  e = y - X %*% beta_est # e es el vector de residuales 
  k = ncol(X)  # El número de parámetros a estimar      
  n = length(y)  # El número de observaciones en la mustra
  # Construcción de (X^{'}X)^{-1}
  mat_X = solve(t(X) %*% X)
  # Matriz intermedia de los errores robustos White (que se encuentra por medio de una suma)
  # Inicializo la matriz como una matriz de ceros
  mat_intermedia = matrix(0, nrow = k, ncol = k)
  # El for se diseña para llenar la matriz mat_intermedia
  for (i in 1:n){
    mat_intermedia = mat_intermedia + (e[i])^2 * (X[i, ] %*% t(X[i, ]))
  }
  # mat_final me dal estimador de la matriz de varianzas y covarianzas de Huber-White
  mat_final = mat_X %*% mat_intermedia %*% mat_X
  return(mat_final)
}

# y = rnorm(1000)
# D = rnorm(1000, mean = 2)
# beta = c(1, 2)
# X = ols_X(D)
# 
# Huber_White(y, X, beta)


# l estimador para la matriz \Hat{Q}_{XX}^{-1}: (Función auxiliar)

##__________________________________________________________________________
# delta: Es la función principal (más importante) de todo el código. 
#        Con base en la función delta es que se deduce todo lo demás 
#        en el código. 
#        Es una función bastante general que contempla todas las situaciones
#        que puedan surgir en el código. En ese caso, contempla la simulación
#        de la Cauchy y de una muestra heterocedástico donde la volatilidad
#        de los tratados es diferente a la volatilidad de los no tratados
# Nota:  Revisar toda el enunciado del ejercicio primero para entender mejor
#        la lógica de la función 
##__________________________________________________________________________

# Función para estimar el efecto tratamiento (delta) en una base de datos
delta = function(t, n, distro, hetero = F, varianza_y0 = 1, varianza_y1 = 1, robustos = F){
  # Defino la semilla para reproducibilidad del resultado
  set.seed(5678)
  # Definición de variables: 
  ## t: número de muestras a simular 
  ## n: tamaño de las muestras
  ## distro: tipo de distribución con el que se va a simular Y_{i}^{0}
  ## Nota: Si distro == "normal", entonces los parámetros opcionales 
  ##       empiezan a tomar relevancia
  ## hetero: Para saber si la volatilidad entre tratados y no tratados es diferente
  ## varianza_y0: El valor de la varianza de la distribución normal con la que se simula y_{i}^{0}
  ## varianza_y1: El valor de la varianza de la distribución normal con la que se simula y_{i}^{1}
  ## robustos: Solo se activa si se escoge la opción hetero == T y además robustos == T
  ##           Permite estimar errores robustos usando la matriz de varianzas y covarianzas Huber-White
  # Consideraciones adicionales:
  # Condición lógica para saber si y_0 sigue una distribución normal estándar o una distribución Cauchy estándar
  if (distro == "normal"){
    # Hay dos posibles casos en caso de que la distribución sea normal o no: 
    # 1. Homocedásticidad entre tratados y no tratados
    ## En caso que la volatilidad de y_{i}^{0} y y_{i}^{1} sea la misma hetero == F
    # 2. Heterocedastidad entre tratados y no tratados (diferente volatilidad dependiendo si fueron tratados o no)
    ## En caso que la volatilidad de y_{i}^{0} y y_{i}^{1} sea diferente hetero == T
    if (hetero == F){
      # df: Dataframe que almacena el SDO y el delta_estimado por OLS
      # El parámetro delta es el paraḿetro asociado a la asignación a tratamiento (D)
      df = data.frame(SDO = double(), delta_est = double(), sigma = double(), var_tlc = double(), var_ols = double())
      # Meter un for para generar las t diferentes muestas
      for (muestra in 1:t){
        # Simulación de los outcomes potenciales y de la variable trataminto
        y_0 = rnorm(n, mean = 0, sd = 1) # Simulación del outcome potencial de ausencia de tratamiento 
        y_1 = y_0 + 3  # Simulación del outcome potencial de presencia de tratamiento 
        D = rbinom(n, 1, prob = 0.3) # Simulación de una variable Bernoulli con una probabilidad de éxito de 0.3
        # Modelo causal de Rubin
        y = y_0 + (y_1 - y_0) * D    # y es un vector Nx1, donde n es el número de observaciones (tamaño de muestra)      
        # Corregir (Debo calcular es el SDO)
        SDO = SDO_calculo(y, D) # Cálculo del SDO utlilizando la función SDO_calculo
        # Estimación del parámetro delta mediante una regresión lineal con constante
        # X es una matriz NxK, donde n es el número de observaciones y K el número de paraḿetros
        X = ols_X(D) # D es la variable tratamiento 
        beta_ols = solve(t(X) %*% X) %*% t(X) %*% y # beta_ols es un vector de Kx1, donde K es el número de parámetos
        # Ahora, se calcularon 3 parámetros adicionales: 
        # el estimador de sigma, el estimador de var(sqrt(n) (delta_{OLS} - delta) y el estimador de var(delta_{OLS}))
        ## estimador de sigma:
        sigma = estimador_sigma(y, X, beta_ols)
        ## estimador de \Hat{Q}_{XX}^{-1}:
        Q_mat = estimador_Q_xx_inv(X)
        ## estimador de var_tlc = var(sqrt(n) (delta_{OLS} - delta))
        var_tlc = sigma * Q_mat[[2,2]]
        ## estimador de var_ols = var(delta_{OLS})
        n = nrow(X)
        var_ols = 1/n * sigma * Q_mat[[2,2]]
        # Dataframe que contiene todos los parámetros de interés 
        df[muestra, ] = c(SDO, beta_ols[[2,1]], sigma, var_tlc, var_ols)    # Extraigo el segundo parámetro que es el parámetro delta
      }
    }else{
      # df: Dataframe que almacena el SDO y el delta_estimado por OLS
      # El parámetro delta es el paraḿetro asociado a la asignación a tratamiento (D)
      df = data.frame(SDO = double(), delta_est = double(), sigma = double(), var_ols = double())
      # Meter un for para generar las t diferentes muestas
      for (muestra in 1:t){
        # Simulación de los outcomes potenciales y de la variable trataminto
        D = rbinom(n, 1, prob = 0.3) # Simulación de una variable Bernoulli con una probabilidad de éxito de 0.3
        y_0 = rnorm(n, mean = 0, sd = sqrt(varianza_y0)) # Simulación del outcome potencial de ausencia de tratamiento 
        y_1 = rnorm(n, mean = 3, sd = sqrt(varianza_y1))  # Simulación del outcome potencial de presencia de tratamiento 
        # Modelo causal de Rubin
        y = y_0 + (y_1 - y_0) * D    # y es un vector Nx1, donde n es el número de observaciones (tamaño de muestra)
        # Corregir (Debo calcular es el SDO)
        SDO = SDO_calculo(y, D) # Cálculo del SDO utlilizando la función SDO_calculo
        # Estimación del parámetro delta mediante una regresión lineal con constante
        # X es una matriz NxK, donde n es el número de observaciones y K el número de paraḿetros
        X = ols_X(D) # D es la variable tratamiento 
        beta_ols = solve(t(X) %*% X) %*% t(X) %*% y # beta_ols es un vector de Kx1, donde K es el número de parámetos
        if (robustos == F){
          # Ahora, se calcularon 3 parámetros adicionales: 
          # el estimador de sigma, el estimador de var(sqrt(n) (delta_{OLS} - delta) y el estimador de var(delta_{OLS}))
          ## estimador de sigma:
          sigma = estimador_sigma(y, X, beta_ols)
          ## estimador de \Hat{Q}_{XX}^{-1}:
          Q_mat = estimador_Q_xx_inv(X)
          ## estimador de var_ols = var(delta_{OLS})
          n = nrow(X)
          var_ols = 1/n * sigma * Q_mat[[2,2]]
          # Dataframe que contiene todos los parámetros de interés 
          df[muestra, ] = c(SDO, beta_ols[[2,1]], sigma, var_ols)    # Extraigo el segundo parámetro que es el parámetro delta
        }else{
          # Sigma, es provisional, luego se retira porque no tiene sentido para un estimador con matriz de varianzas y covarianzas
          # con errores robustos Huber-White (Dado la heterocedasticidad de los errores)
          sigma = 0
          # Cálculo errores robustos Huber-White (me genera una matriz k x k, donde k es el número de parámetros)
          var_ols = Huber_White(y, X, beta_ols)[[2,2]] # Extraigo la componente 2 de la matriz de varianzas y covarianzas de Huber-White
          df[muestra, ] = c(SDO, beta_ols[[2,1]], sigma, var_ols)    # Extraigo el segundo parámetro que es el parámetro delta
        }
      }
      # Condicional final para eliminar la columna sigma si robustos == T
      if (robustos == T){
        df = df %>% 
          select(SDO, delta_est, var_ols)
      }
    }
  }else if (distro == "cauchy"){
    # df: Dataframe que almacena el SDO y el delta_estimado por OLS
    # El parámetro delta es el paraḿetro asociado a la asignación a tratamiento (D)
    df = data.frame(SDO = double(), delta_est = double())
    for (muestra in 1:t){
      # Simulación de los outcomes potenciales y de la variable trataminto
      y_0 = rcauchy(n, location = 0, scale = 1) # Simulación del outcome potencial de ausencia de tratamiento 
      y_1 = y_0 + 3  # Simulación del outcome potencial de presencia de tratamiento 
      D = rbinom(n, 1, prob = 0.3) # Simulación de una variable Bernoulli con una probabilidad de éxito de 0.3
      # Modelo causal de Rubin
      y = y_0 + (y_1 - y_0) * D    # y es un vector Nx1, donde n es el número de observaciones (tamaño de muestra)
      # Corregir (Debo calcular es el SDO)
      SDO = SDO_calculo(y, D) # Cálculo del SDO utlilizando la función SDO_calculo
      # Estimación del parámetro delta mediante una regresión lineal con constante
      # X es una matriz NxK, donde n es el número de observaciones y K el número de paraḿetros
      X = ols_X(D) # D es la variable tratamiento 
      beta_ols = solve(t(X) %*% X) %*% t(X) %*% y # beta_ols es un vector de Kx1, donde K es el número de parámetos
      df[muestra, ] = c(SDO, beta_ols[[2,1]])    # Extraigo el segundo parámetro que es el parámetro delta
    }
  }
  # La función delta retorna un df con el cálculo de la SDO 
  # y el parámetro estimado delta, que proviene de una switching regression
  return(df)
}

# Simulación 
# delta(t, n10, distro = "normal") # Existe el riesgo de que haya
              # multicolinealidad perfecta cuando se usa n10 = 10
              # Puede generar un vector D = rep(0, times = 10)

delta20 = delta(t, n20, distro = "normal") 
delta50 = delta(t, n50, distro = "normal") 
delta100 = delta(t, n100, distro = "normal") 
delta500 = delta(t, n500, distro = "normal") 
delta1000 = delta(t, n1000, distro = "normal") 

# II. ----

# n_vector es un vector que almacena los diferentes tamaños de muestra
n_vector = seq(from = 20, to = 1000, by = 10)

# Función que genera un dataframe con el tamaño de muestra, 
# la media y la varianza del estimador de delta para cada 
# tamaño diferente de muestra

media_varianza_delta = function(t, n_vector, distro, hetero = F, varianza_y0 = 1, varianza_y1 = 1, robustos = F){
  # Variables: 
  ## t: Número de muestras que se van a generar por cada tamaño de muestra
  ## n_vector: Variable que almacena los diferentes tamaños de muestras
  ## distro, hetero, varianza_y0 y varianza_y1 son parámetros definidos para la función delta
  # df: DataFrame que almacena el tamaño de muestra,
  #     la media y la varianza del estimador de delta para cada tamaño de muestra
  df = data.frame(tamaño = double(), media = double(), varianza = double())
  # Llamo a la función delta para cada tamaño diferente de muestra 
  for (i in 1:length(n_vector)){
    n_muestra = n_vector[i]
    delta_n = delta(t, n_muestra, distro, hetero, varianza_y0, varianza_y1, robustos)$delta_est # delta_n es el vector de deltas por cada tamaño de muestra
    df[i,] = c(n_muestra, mean(delta_n), var(delta_n))
  }
  return(df)
}

# Dataframe con el tamaño de muestra y la media y varianza del 
# estimador de delta para los diferentes tamaños de muestra
media_varianza = media_varianza_delta(100, n_vector, distro = "normal"); glimpse(media_varianza)

# III. ----

grafica_propiedas = function(df, variable_y, y_intercepto = 0){
  variable_y = ensym(variable_y) 
  graph = df %>% 
    ggplot(aes(x = tamaño, y = !!variable_y)) + 
    geom_line(color = "green", size = 1) +
    geom_hline(yintercept = y_intercepto, color = "red") +
    theme_light()
  return(graph)
}

grafica_propiedas(media_varianza, media, y_intercepto = 3)
grafica_propiedas(media_varianza, varianza)

# Ejercicio c. ----

# I. ----

# Definición de parámetros para la simulación 

## Número de muestras: 
t2 = 1000

## Tamaño de muestras: 

# No se puede trabajar con n10 = 10 porque no satisface la condición de rango
# n10 = 10 Genera probelmas de singularidad cuando se generan muchas muestras
#          Porque por chance, se genera una muestra donde D = rep(0, times = 10)
n20 = 20
n100 = 100
n1000 = 1000
n5000 = 5000
n10000 = 10000
n20000 = 20000
n50000 = 50000
n100000 = 100000
n_vector2 = c(n20, n100, n1000, n5000, n10000, n20000, n50000, n100000)

# Función que calcula a = sqrt(n) (delta_est - truth_delta) para cada tamaño de muestra n
calculo_a = function(t, n, distro, truth_delta){
  # Variables: 
  ## t: número de muestras a simular 
  ## n: tamaño de la muestra
  ## truth_delta: verdadero valor del parámetro delta (valor poblcional del parámetro)
  # df_estimados es el dataframe que contiene el delta estimado por SDO 
  # y por medio de regresión
  df_estimados = delta(t, n, distro) 
  # df_a es el dataframe que contiene 
  df_a = df_estimados %>% 
    mutate(a = sqrt(n) * (delta_est - truth_delta), tamaño = rep(n, times = t))
  return(df_a)
}

# Defino una función para crear un dataframe que se va a utilizar 
# Para construir la gráfica multipanel. 
base_para_graficas = function(t, n_vector, distro, truth_delta){
  for (i in 1:length(n_vector)){
    # Si i == 1 significa que estamos en la primera iteración
    if (i == 1){
      df_a_total = calculo_a(t, n_vector[i], distro, truth_delta = 3)
    }else{
      df_prov = calculo_a(t, n_vector[i], distro, truth_delta = 3)
      df_a_total = bind_rows(df_a_total, df_prov)
    }
  }
  # Retorna un dataframe con los diferentes valores de a para cada tamaño de muestra listo 
  # para construir la gráfica multipanel
  return(df_a_total)
}

# La base multipanel me tiene lista los diferentes a
# para cada tamaño de muestra
base_multipanel = base_para_graficas(t2, n_vector2, distro = "normal", truth_delta = 3)

# II. ----

# Defino una función para crear la gráfica multipanel con el histograma y la función de densidad 
# para cada tamaño de muestra n
histogram_grid = function(df, titulo, x_lab, y_lab, num_bins = 30, y_upper_limit){
  histog_grid = df %>% 
    ggplot(aes(a)) +
    scale_y_continuous(limits = c(0, y_upper_limit)) +
    geom_histogram(aes(y = ..density..), color = "black", bins = num_bins) + 
    geom_density(color = "green") +
    facet_grid(cols = vars(tamaño)) +
    ggtitle(titulo) +
    xlab(x_lab) + 
    ylab(y_lab) +
    theme_light() 
}

grafica_multi = histogram_grid(df = base_multipanel, titulo = "Gráfico multipanel para a con diferentes tamaños de muestra", x_lab = "Tamaño de muestras (n)", y_lab = "", num_bins = 30, y_upper_limit = 0.2); grafica_multi

# Gráfica que compara directamente las densidades 
compracion_densidades_grafica = function(t, df, var){
  # Variables: 
  ## t: número de muestras simuladas por tamaño de muestra
  ## df: df multipanel que tiene las simulaciones de las diferentes muestra por tamaño de muestra
  ## var: variable simulada de la cual se va a generar las funciones de densidad 
  # Nota: 
  # Modifico el dataframe multipanel para seleccionar solo
  # las variables a y tamaño que permiten hacer la gráfica de densidades
  # df_mod es un dataframe modifiado del dataframe multipanel
  df_mod = df %>% 
    rename(valores_sim = {{ var }}, distribuciones = tamaño) %>% 
    mutate(distribuciones = as.character(distribuciones)) %>% 
    select(valores_sim, distribuciones)
  # Simulo de una distribución normal estándar porque quiero probar
  # si se cumple o no el teorema del límite central
  df_normal = data.frame(valores_sim = rnorm(t), distribuciones = rep("Normal estándar", times = t))
  # base_grafica ya es la base de datos lista para realizar las gráficas de 
  # las densidades de las a y de una normal estándar
  base_grafica = df_mod
  # base_grafica = bind_rows(df_mod, df_normal)
  # Gráfica de densidades
  density_comparacion = base_grafica %>% 
    ggplot(aes(x = valores_sim, color = distribuciones)) +
    geom_density() + 
    theme_light() +
    ggtitle("Comparación de funciones de densidad de a\n para muestras de diferentes tamaños \n(todas simuladas)") +
    ylab("Densidades") +
    xlab("Distribuciones de a para diferente tamaño de muestra")
  return(density_comparacion)
}

# Visualización de la base de datos con las variables simuladas
# delta_est y a
glimpse(base_multipanel)

base_filtrada = base_multipanel %>% 
  filter(100 < tamaño)

base_filtrada_a = base_multipanel %>% 
  filter(10 < tamaño) %>% 
  filter(tamaño < 2000)

# Gráficas de las funciones de densidad para la simulación de los delta estimados
# y de los a, para el caso de un Y_{i}^{0} = norm(0, 1)
grafica_a = compracion_densidades_grafica(t = t2, df = base_filtrada_a, var = a); grafica_a
grafica_delta_est = compracion_densidades_grafica(t = t2, df = base_filtrada, var = delta_est); grafica_delta_est

# Nota: Crear la otra gráfica mejor (script de simulación de df)

# Ejercicio d. ----

# Va a replicarse el ejercicio c. pero asumiendo que: 
# Y_{i}^{0} ~ cauchy(0, 1) (Distribución Cauchy estándar)

# I. ----

# Construyo la base multipanel para los diferentes a, asumiendo que Y_{i}^{0} sigue una distribución Cauchy

base_multipanel_cauchy = base_para_graficas(t2, n_vector2, distro = "cauchy", truth_delta = 3)

# II. ----

base_filtrada_cauchy = base_multipanel_cauchy %>% 
  filter(tamaño > 20) 
  # filter(tamaño < 10000)

base_filtrada_cauchy2 = base_multipanel_cauchy %>% 
  filter(tamaño > 10) %>% 
  filter(tamaño < 2000)

# Gráficas de las funciones de densidad para la simulación de los delta estimados
# y de los a, para el caso de un Y_{i}^{0} = cauchy(0, 1)
grafica2_a = compracion_densidades_grafica(t = t2, df = base_filtrada_cauchy2, var = a); grafica2_a
grafica2_delta_est = compracion_densidades_grafica(t = t2, df = base_filtrada_cauchy, var = delta_est); grafica2_delta_est

# Nota: Super interesante ver como cambia la distribución Cauchy 
#       bajo los mismos paraḿetros de localización y escala 
#       La distribución Cauchy es una distribución patológica 
#       en el sentido que su media y varianza es infinita

# plot(density(rcauchy(1000, location = 0, scale = 1)))

# Ejercicio e. ---- 

# I. ----

# Tamaños de muestra 
n20 = 20
n100 = 100
n200 = 200
n500 = 500
n1000 = 1000
n10000 = 10000

hetero20_no_robustos = delta(t = 1000, n = 20, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = F)
hetero100_no_robustos = delta(t = 1000, n = 100, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = F)
hetero200_no_robustos = delta(t = 1000, n = 200, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = F)
hetero500_no_robustos = delta(t = 1000, n = 500, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = F)
hetero1000_no_robustos = delta(t = 1000, n = 1000, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = F)
hetero10000_no_robustos = delta(t = 1000, n = 10000, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = F)

n_vect_hetero = c(n20, n100, n200, n500, n1000, n10000)

# Dataframe con el tamaño de muestra y la media y varianza del 
# estimador de delta para los diferentes tamaños de muestra
media_varianza_hetero = media_varianza_delta(t = 1000, n_vect_hetero, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = F); glimpse(media_varianza_hetero)

# Nota: Se observa que a pesar de la heterocedasticidad en Y_{i} hay insesgadez 
#       En la estimación de delta
#       De igual forma, la varianza muestral también disminuye a medida que aumenta
#       la muestra (independiente de que haya heteroecedasticidad en Y_{i})

# Construir una función que me compute los intervalos de confianza 
# Intervalos de confianza: Ya sea para intervalos clásicos o para intervalos robustos
#                          a la heterocedasticidad como los calculados por la matriz de varianzas y covarianzas Huber-White

inter_confianza = function(df_estimaciones, delta_true, int_conf){
  norm_inf = qnorm((1 - int_conf)/2)
  norm_sup = qnorm((1 - int_conf)/2 + int_conf)
  lim_inf = df_estimaciones$delta_est + norm_inf * sqrt(df_estimaciones$var_ols)
  lim_sup = df_estimaciones$delta_est + norm_sup * sqrt(df_estimaciones$var_ols)
  contiene_o_no = c()
  for (i in 1:length(lim_inf)){
    if ((lim_inf[i] < delta_true) && (delta_true < lim_sup[i])){
      contiene_o_no = append(contiene_o_no, 1)
    }else{
      contiene_o_no = append(contiene_o_no, 0)
    }
  }
  # Variable del dataframe
  df = data.frame(lim_inf, lim_sup, contiene_o_no)
  return(df)
}

# Intervalos de confianza del 90 % 
int_conf1000_no_robustos_90 = inter_confianza(hetero1000_no_robustos, delta_true = 3, int_conf = 0.9)

# Intervalos de confianza del 95 % 
int_conf1000_no_robustos_95 = inter_confianza(hetero1000_no_robustos, delta_true = 3, int_conf = 0.95)

# Intervalos de confianza del 99 % 
int_conf1000_no_robustos_99 = inter_confianza(hetero1000_no_robustos, delta_true = 3, int_conf = 0.99)

# II. ----

# Porcentaje de los intervalos de confianza del 90 % que contienen el parámetro verdadero
porcentaje_no_robustos_90 = sum(int_conf1000_no_robustos_90$contiene_o_no)/nrow(int_conf1000_no_robustos_90) * 100; porcentaje_no_robustos_90

# Porcentaje de los intervalos de confianza del 95 % que contienen el parámetro verdadero
porcentaje_no_robustos_95 =  sum(int_conf1000_no_robustos_95$contiene_o_no)/nrow(int_conf1000_no_robustos_95) * 100; porcentaje_no_robustos_95

# Porcentaje de los intervalos de confianza del 99 % que contienen el parámetro verdadero
porcentaje_no_robustos_99 = sum(int_conf1000_no_robustos_99$contiene_o_no)/nrow(int_conf1000_no_robustos_99) * 100; porcentaje_no_robustos_99

# III. ----

hetero20_robustos = delta(t = 1000, n = 20, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = T)
hetero100_robustos = delta(t = 1000, n = 100, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = T)
hetero200_robustos = delta(t = 1000, n = 200, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = T)
hetero500_robustos = delta(t = 1000, n = 500, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = T)
hetero1000_robustos = delta(t = 1000, n = 1000, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = T)
hetero10000_robustos = delta(t = 1000, n = 10000, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = T)

n_vect_hetero = c(n20, n100, n200, n500, n1000, n10000)

# Dataframe con el tamaño de muestra y la media y varianza del 
# estimador de delta para los diferentes tamaños de muestra
media_varianza_hetero = media_varianza_delta(t = 1000, n_vect_hetero, distro = "normal", hetero = T, varianza_y0 = 2, varianza_y1 = 1, robustos = T); glimpse(media_varianza_hetero)

# III. Cálculo de intervalos de confianza cuando se tienen errores robustos ----

# Intervalos de confianza del 90 % 
int_conf1000_robustos_90 = inter_confianza(hetero1000_robustos, delta_true = 3, int_conf = 0.9)

# Intervalos de confianza del 95 % 
int_conf1000_robustos_95 = inter_confianza(hetero1000_robustos, delta_true = 3, int_conf = 0.95)

# Intervalos de confianza del 99 % 
int_conf1000_robustos_99 = inter_confianza(hetero1000_robustos, delta_true = 3, int_conf = 0.99)

# III. Porcentaje de los intervalos de confianza cuando se tienen errores robustos ----

# Porcentaje de los intervalos de confianza del 90 % que contienen el parámetro verdadero
porcentaje_robustos_90 = sum(int_conf1000_robustos_90$contiene_o_no)/nrow(int_conf1000_robustos_90) * 100; porcentaje_robustos_90

# Porcentaje de los intervalos de confianza del 95 % que contienen el parámetro verdadero
porcentaje_robustos_95 = sum(int_conf1000_robustos_95$contiene_o_no)/nrow(int_conf1000_robustos_95) * 100; porcentaje_robustos_95

# Porcentaje de los intervalos de confianza del 99 % que contienen el parámetro verdadero
porcentaje_robustos_99 = sum(int_conf1000_robustos_99$contiene_o_no)/nrow(int_conf1000_robustos_99) * 100; porcentaje_robustos_99

# Conclusión: Los errores robustos cálculados con matriz de varianzas y covarianzas Huber-White, 
#             sí sirven cuándo hay heterocedasticidad en los errores de modelo
#             Por el contrario, si se utilizan intervalos de confianza clásicos
#             sin corregir por heterocedasticidad, se observa que los intervalos
#             de confianza clásicos de manera sistemática para los nivelos de 
#             significancia sobreestiman, es decir calculan intervalos de 
#             confianza más amplios de los que en realidad deberian computarse
#             dada la presencia de la heterocedasticad

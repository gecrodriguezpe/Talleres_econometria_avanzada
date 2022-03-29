library(tidyverse)
library(readxl)
library(xtable)
library(rdrobust)

# Preliminares ---- 

# Definición del directorio de trabajo
setwd("~/Documents/GitHub/semestre8_git/Econometría_avanzada/Talleres_econometria_avanzada/Taller2/Bases_datos")

# Importación base de datos 
base = read_delim("SPP_Base.csv", delim = "$")

# Visualización de la base de datos 
glimpse(base)

# a) tabla de estadı́sticas descriptivas ---- 

# Se va a generar una tabla de estadísticas descriptivas para: puntaje_saber11, edad, estrato, miembros_familia
# Se reporta el número de observaciones, el promedio de la variable, su desviación estándar y el mı́nimo y máximo observados.

# Estadísticas descriptivas para la variable "puntaje_saber11"
base_puntaje_saber11 = base %>% 
  summarise(num_observaciones = sum(!is.na(puntaje_saber11)), 
            promedio = mean(puntaje_saber11, na.rm = T),
            desv_estandar = sd(puntaje_saber11, na.rm = T), 
            minimo = min(puntaje_saber11, na.rm = T),
            maximo = max(puntaje_saber11, na.rm = T)); base_puntaje_saber11

# Estadísticas descriptivas para la variable "edad"
base_edad = base %>% 
  summarise(num_observaciones = sum(!is.na(edad)), 
            promedio = mean(edad, na.rm = T),
            desv_estandar = sd(edad, na.rm = T), 
            minimo = min(edad, na.rm = T),
            maximo = max(edad, na.rm = T)); base_edad

# Estadísticas descriptivas para la variable "estrato"
base_estrato = base %>% 
  summarise(num_observaciones = sum(!is.na(estrato)), 
            promedio = mean(estrato, na.rm = T),
            desv_estandar = sd(estrato, na.rm = T), 
            minimo = min(estrato, na.rm = T),
            maximo = max(estrato, na.rm = T)); base_estrato

# Estadísticas descriptivas para la variable "miembros_familia"
base_miembros_familia = base %>% 
  summarise(num_observaciones = sum(!is.na(miembros_familia)), 
            promedio = mean(miembros_familia, na.rm = T),
            desv_estandar = sd(miembros_familia, na.rm = T), 
            minimo = min(miembros_familia, na.rm = T),
            maximo = max(miembros_familia, na.rm = T)); base_miembros_familia

# Dataframe con todas las estadísticas descriptivas juntas para las 4 variables de interés 
base_exportar = bind_rows(base_puntaje_saber11, 
                          base_edad, 
                          base_estrato, 
                          base_miembros_familia) %>% 
  mutate(variable = c("puntaje_saber11",
                     "edad",
                     "estrato",
                     "miembros_familia"),
         num_observaciones = as.double(num_observaciones)) %>% 
  select(variable, !variable); base_exportar

# Transformar la tabla de estadísticas descriptivas "base_exportar" a una tabla formato latex
xtable(base_exportar, type = "latex", digits = 2)

# b) Gráficos que periten analizar la discontinuidad en la probabilidad de ----

# (i) ser elegible para el programa ----

x11()
grafica1 = ggplot(base, aes(x = puntaje_saber11, y = elegible_SPP)) +
    geom_point(size = 0.8, alpha = 0.1, color = "grey") + 
    geom_vline(xintercept = 310, color = "red") + 
    theme_light(); grafica1

rdplot(y = base$elegible_SPP, 
       x = base$puntaje_saber11, 
       c = 310, p = 2)

# (ii) acceder efectivamente al programa ----

x11()
grafica2 = ggplot(base, aes(x = puntaje_saber11, y = beneficiario_spp)) +
  geom_point(size = 0.8, alpha = 0.1, color = "grey") + 
  geom_vline(xintercept = 310, color = "red") + 
  theme_light(); grafica2

rdplot(y = base$beneficiario_spp, 
       x = base$puntaje_saber11, 
       c = 310, p = 2) # mostrar con p = 1 y p = 2

# c) Estimaciones del RDD empleando estimación no paramétrica (rdrobust) ----

# (i) Elegibilidad

# i.i matricula 

# Se estima un sharp RDD (polinomio grado 1)
prueba1 = rdrobust(y = base$ies_cualquiera, 
         x = base$puntaje_saber11, 
         c = 310, p = 1, 
         all = TRUE, 
         masspoints = "off")

# Se estima un sharp RDD (polinomio grado 2)
rdrobust(y = base$ies_cualquiera, x = base$puntaje_saber11, c = 310, p = 2, all = TRUE)

# i.ii IES alta calidad

# Se estima un sharp RDD (polinomio grado 1)
rdrobust(y = base$ies_altacalidad, x = base$puntaje_saber11, c = 310, p = 1, all = TRUE)

# Se estima un sharp RDD (polinomio grado 2)
rdrobust(y = base$ies_altacalidad, x = base$puntaje_saber11, c = 310, p = 2, all = TRUE)

# i.iii IES privada

# Se estima un sharp RDD (polinomio grado 1)
rdrobust(y = base$ies_altacalidad_priv, x = base$puntaje_saber11, c = 310, p = 1, all = TRUE)

# Se estima un sharp RDD (polinomio grado 2)
rdrobust(y = base$ies_altacalidad_priv, x = base$puntaje_saber11, c = 310, p = 2, all = TRUE)

# (ii) Beneficiario del programa

# Se estima un fuzzy RDD (polinomio grado 1)
rdrobust(y = base$beneficiario_spp, x = base$puntaje_saber11, 
         c = 310, p = 1, kernel = "triangular", all = TRUE)

# Se estima un fuzzy RDD (polinomio grado 2)
rdrobust(y = base$beneficiario_spp, x = base$puntaje_saber11, 
         c = 310, p = 2, kernel = "triangular", all = TRUE)

# (ii) Beneficiario 

# i.i matricula 

# Se estima un sharp RDD (polinomio grado 1)
rdrobust(y = base$ies_cualquiera, x = base$puntaje_saber11, c = 310, p = 1, all = TRUE, fuzzy = base$beneficiario_spp)

# Se estima un sharp RDD (polinomio grado 2)
rdrobust(y = base$ies_cualquiera, x = base$puntaje_saber11, c = 310, p = 2, all = TRUE, fuzzy = base$beneficiario_spp)

# i.ii IES alta calidad

# Se estima un sharp RDD (polinomio grado 1)
rdrobust(y = base$ies_altacalidad, x = base$puntaje_saber11, c = 310, p = 1, all = TRUE, fuzzy = base$beneficiario_spp)

# Se estima un sharp RDD (polinomio grado 2)
rdrobust(y = base$ies_altacalidad, x = base$puntaje_saber11, c = 310, p = 2, all = TRUE, fuzzy = base$beneficiario_spp)

# i.iii IES privada

# Se estima un sharp RDD (polinomio grado 1)
rdrobust(y = base$ies_altacalidad_priv, x = base$puntaje_saber11, c = 310, p = 1, all = TRUE, fuzzy = base$beneficiario_spp)

# Se estima un sharp RDD (polinomio grado 2)
rdrobust(y = base$ies_altacalidad_priv, x = base$puntaje_saber11, c = 310, p = 2, all = TRUE, fuzzy = base$beneficiario_spp)

# d) Validación de supuestos 
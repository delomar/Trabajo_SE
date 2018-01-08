##########################################
### TRABAJO FINAL DE SISTEMAS EXPERTOS ###
##########################################

############################################
## Logica Difusa para un Sistema de Riego ##
############################################

# Cargar el paquete
library(sets)

# Configurar el universo
sets_options("universe", seq(1, 100, 0.1))

# Variables del sistema difuso
variables <- set(
  humedad_suelo = fuzzy_partition(varnames = c(seco = 0, humedo = 40, mojado = 90),
                                sd = 5.0),
  temperatura = fuzzy_partition(varnames = c(frio = -5, templado = 20, calor = 45),
                              sd = 3.0),
  nubosidad = fuzzy_partition(varnames = c(despejado = 0, casi_despejado = 30, nublado = 60,
                                           muy_nublado = 90), sd = 3.0),
  estacion = fuzzy_partition(varnames = c(invierno = 1, primavera = 4, verano = 8, otoño = 12),
                              sd = 1.0),
  dur_regadio = fuzzy_partition(varnames = c(nada = 0, poco = 40, medio = 70, bastante = 90,
                                             prolongado = 30), FUN = fuzzy_cone, radius = 10)
)

# Reglas difusas del sistema difuso
reglas <- set(
  fuzzy_rule(humedad_suelo %is% seco && temperatura %is% calor, dur_regadio %is% prolongado),
  fuzzy_rule(humedad_suelo %is% seco && temperatura %is% templado, dur_regadio %is% medio),
  fuzzy_rule(humedad_suelo %is% humedo && temperatura %is% templado, dur_regadio %is% poco),
  fuzzy_rule(humedad_suelo %is% mojado, dur_regadio %is% nada)
)

# Contrucción del Modelo
# Convierte las entradas a valores difusos
modelo <- fuzzy_system(variables, reglas)

# Mostrar las variables y reglas del sistema difuso
print(modelo)

# Mostrar el gráfico del sistema
plot(modelo)

################################
## PRUEBAS DEL SISTEMA DIFUSO ##
################################

# Prueba N° 01
# fuzzy_inference: procesa la salida en función de las entradas
Prueba_01 <- fuzzy_inference(modelo, list(humedad_suelo = 40, temperatura = 20))

# Ahora, defuzzificamos la prueba para transformar los parámetros en un número real
gset_defuzzify(Prueba_01, "centroid")
plot(Prueba_01)

# Prueba N° 02
Prueba_02 <- fuzzy_inference(modelo, list(humedad_suelo = 90, temperatura = 30))

# Ahora, defuzzificamos la prueba para transformar los parámetros en un número real
gset_defuzzify(Prueba_02, "centroid")
plot(Prueba_02)

# Resetear el universo
sets_options("universe", NULL)


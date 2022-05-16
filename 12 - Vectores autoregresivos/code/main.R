# ------------------------------- Script ----------------------------------
#                          Universidad del Rosario
#                           Facultad de Economía
#                          Econometría Intermedia
#                        Julián David Rojas Aguilar
# ---------------------------- Instrucciones ------------------------------
# 1. Cada línea en el script se ejecuta con Ctrl + Enter o con el botón run de 
#    esta ventana.
# 2. Todo el texto que se encuentre después del numeral (#) es un comentario.
# 3. A la hora de ejecutar cada línea, R no tendrá en cuenta los comentarios.
# 4. Todos los resultados de comandos ejecutados se muestran en la consola.
# 5. Es posible ejecutar comandos directamente desde la consola con la tecla 
#    Enter.

# 1| Preparación ----------------------------------------------------------
# 1.1| Entorno de trabajo -------------------------------------------------
rm(list = ls())

# Librerías necesarias:
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(urca)
library(vars)
library(ggplot2)
library(ggfortify)
library(gridExtra)

# Directorios donde se almacena la información del proyecto:
directorioPrincipal         <- enc2native(here())
directorioCodigo            <- paste0(directorioPrincipal, '/code/')
directorioDatosCrudos       <- paste0(directorioPrincipal, '/input/raw/')
directorioDatosLimpios      <- paste0(directorioPrincipal, '/input/cleaned/')
directorioResultadosFiguras <- paste0(directorioPrincipal, '/output/figures/')
directorioResultadosTablas  <- paste0(directorioPrincipal, '/output/tables/')
setwd(directorioPrincipal)

# 2| Ejemplo simulado -----------------------------------------------------
# Objetivo: simular un VAR(1) bivariado.

# 2.1| Condiciones iniciales ----------------------------------------------
set.seed(2022)
observaciones <- 500

# Inicializamos los vectores de las variables endógenas, así como la
# estructura del proceso estacionario que moverá los datos a través
# del tiempo.
y <- cbind(rep(x = 0, times = observaciones), 
           rep(x = 0, times = observaciones))
A <- matrix(data = c(0.3, 0.2, 0.5, -0.2), nrow = 2, byrow = TRUE)

# Y también los errores, asumiendo distribuciones normales y no-correlacionadas.
u <- replicate(n = 2, expr = rnorm(n = observaciones))

# 2.2| Simulación ---------------------------------------------------------
# Partiendo de un Proceso Generador de Datos no en su forma primitiva, sino
# en su forma reducida, podemos encontrar los valores de series simuladas
# a través del siguiente bucle:
for (i in 2:observaciones) {
  y[i, ] = A %*% y[i - 1, ] + u[i, ]
}

# Y, con el objetivo de evitar interferencias en los resultados consecuencia 
# de los valores iniciales, eliminamos algunos datos del inicio.
y <- y[-c(1:100), ]
y <- ts(data = y, start = c(1985, 1), frequency = 12)

primerVariable <- autoplot(object    = y[, 1],
                           ts.colour = 'red', 
                           xlab      = 'Fecha',
                           ylab      = 'Valores simulados', 
                           main      = 'Simulación de la primer variable') +
  theme_classic()

segundaVariable <- autoplot(object    = y[, 2],
                            ts.colour = 'red', 
                            xlab      = 'Fecha',
                            ylab      = 'Valores simulados', 
                            main      = 'Simulación de la segunda variable') +
  theme_classic()

grid.arrange(primerVariable, segundaVariable, ncol = 2)

# 2.3| Modelación ---------------------------------------------------------
# A| Estacionariedad ------------------------------------------------------
summary(ur.df(y = y[, 1], type = 'trend', selectlags = 'AIC'))
summary(ur.df(y = y[, 1], type = 'drift', selectlags = 'AIC'))
summary(ur.df(y = y[, 1], type = 'none', selectlags = 'AIC'))

summary(ur.df(y = y[, 2], type = 'trend', selectlags = 'AIC'))
summary(ur.df(y = y[, 2], type = 'drift', selectlags = 'AIC'))
summary(ur.df(y = y[, 2], type = 'none', selectlags = 'AIC'))

# Conclusion: como se rechaza la hipótesis nula para ambos casos, tenemos
# evidencia estadística para argumentar estacionariedad en las variables
# endógenas que deseamos modelar.

# B| Identificación del modelo --------------------------------------------
# Hay una discrepancia entre los diferentes criterios de información,
# quedando siempre 'empatados' 2 vs. 2. Sin embargo, acorde al criterio
# de información bayesiano, el modelo óptimo tiene solo un rezago.
VARselect(y, lag.max = 24, type = 'both')
VARselect(y, lag.max = 24, type = 'const')
VARselect(y, lag.max = 24, type = 'none')

# C| Estimación -----------------------------------------------------------
# Dado que conocemos el PGD, y en este no incluimos ningún intercepto,
# el proceso no tiene un atractor diferente de cero. Por consiguiente,
# estimamos el modelo sin intercepto.
estimacion <- VAR(y, p = 1, type = 'none')
summary(estimacion)

# D| Estabilidad ----------------------------------------------------------
# Para que el proceso sea estable, las raíces del polinomio característico
# deben estar por fuera del círculo unitario o, lo que es lo mismo, los
# valores propios deben ser menores a uno en módulo.
roots(estimacion, modulus = TRUE)

# E| Pruebas de hipótesis -------------------------------------------------
# - Autocorrelación: H0 implica que no hay autocorrelación.
# Nota: 'PT.asymptotic' es para una muestra grande y'PT.adjusted' permite 
# calcular una corrección para muestras pequeñas. 
pruebaAutocor <- serial.test(estimacion, lags.pt = 12, type = 'PT.asymptotic')
pruebaAutocor
plot(pruebaAutocor)

# - Homocedasticidad: H0 implica que no hay heterocedasticidad.
pruebaHomo <- arch.test(estimacion, lags.multi = 12, multivariate.only = TRUE)
pruebaHomo
plot(pruebaAutocor)

# - Normalidad: H0 implica que se cumple normalidad.
normality.test(estimacion)

# F| Pronóstico -----------------------------------------------------------
# Podemos calcular la predicción puntual, encontrar un rango de posibles
# realizaciones y, dentro de ese rango, calcular la probabilidad de ver
# observaciones en ciertos puntos.
predict(estimacion, n.ahead = 12, ci=0.95) 
autoplot(predict(estimacion, n.ahead = 12,ci=0.95))
fanchart(predict(estimacion, n.ahead = 12))

# G| Función Impulso-Respuesta --------------------------------------------
# Los errores u_1 y u_2 individualmente son ruido blanco. Sin embargo,
# están correlacionados de forma contemporánea. Así, las IRF ortogonales
# hacen una transformación de la matriz de varianzas y covarianzas
# (vía Choleski) de forma tal que se evita la correlación. 
# Nota: como costo de la identificación, el orden de las variables importa. 
# La primer variable afecta, contemporáneamente, a las demás variables del
# sistema --mientras que la segunda variable afecta contemporáneamente a todas
# menos a la primera--.  
irf(x = estimacion, impulse = 'Series.1', response = 'Series.2', 
    n.ahead = 12, ortho = TRUE, cumulative = FALSE, boot = TRUE) |> plot()

# H| Descomposición del error de pronóstico -------------------------------
fevd(estimacion, n.ahead = 12)
plot(fevd(estimacion, n.ahead = 18), col = c('red', 'green'))

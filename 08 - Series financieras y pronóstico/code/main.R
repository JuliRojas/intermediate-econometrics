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
library(readxl)
library(here)
library(tidyverse)
library(quantmod)
library(forecast)
library(urca)
library(aTSA)
library(rugarch)
library(fGarch)

# 1.2| Bases de datos -----------------------------------------------------
# 1.2.1| Serie financiera -------------------------------------------------
getSymbols(Symbols = 'AAPL')
AAPL       <- AAPL[, 'AAPL.Adjusted']
priceAAPL  <- do.call(rbind, lapply(split(AAPL, "months"), last))
returnAAPL <- 100*diff(log(priceAAPL))

# 1.2.2| Serie macroeconómica ---------------------------------------------
# En los datos de pronóstico se parte las observaciones en muestra de 
# entrenamiento y muestra de evaluación, de forma tal que sea factible
# calcular una métrica de desempeño por fuera de muestra.


# 2| Análisis de series financieras ---------------------------------------
# 2.1| Autocorrelación simple y parcial -----------------------------------
# Análisis sobre los retornos.
acf(returnAAPL, 
    lag.max   = 20,
    plot      = TRUE,
    xlab      = 'Rezagos',
    ylab      = 'Valor de autocorrelación simple',
    main      = 'Retornos de Apple',
    na.action = na.pass) # Al diferenciar se perdió el último dato.
pacf(returnAAPL, 
     lag.max   = 20,
     plot      = TRUE,
     xlab      = 'Rezagos',
     ylab      = 'Valor de autocorrelación parcial',
     main      = 'Retornos de Apple',
     na.action = na.pass) # Al diferenciar se perdió el último dato.

# Análisis sobre la varianza de los retornos.
acf((returnAAPL-mean(returnAAPL, na.rm = TRUE))^2, 
    lag.max   = 20,
    plot      = TRUE,
    xlab      = 'Rezagos',
    ylab      = 'Valor de autocorrelación parcial',
    main      = 'Retornos de Apple',
    na.action = na.pass)
pacf((returnAAPL-mean(returnAAPL, na.rm = TRUE))^2, 
     lag.max   = 20,
     plot      = TRUE,
     xlab      = 'Rezagos',
     ylab      = 'Valor de autocorrelación parcial',
     main      = 'Retornos de Apple',
     na.action = na.pass)

# 2.2| Estacionariedad ----------------------------------------------------
returnAAPL <- na.omit(returnAAPL) # Eliminar NA inicial para el análisis.
tendencia  <- ur.df(y = returnAAPL, type = 'drift', selectlags = 'BIC')
summary(tendencia)

# 2.3| Grilla de búsqueda -------------------------------------------------
lambdaEstimation <- BoxCox.lambda(returnAAPL, lower = 0)
results <- tibble(model = character(),
                  aic   = numeric(),
                  bic   = numeric())
aux <- 0
for(p in 0:4) {
  for(q in 0:4) {
    aux = aux + 1
    try(model <- returnAAPL %>% 
          Arima(order = c(p, 0, q), # ¡El retorno es estacionario!
                method = 'CSS-ML'))
    results[aux, 1] = paste0('(', p, ',0,', q, ')')
    results[aux, 2] = model[['aic']]
    results[aux, 3] = model[['bic']]
  }
}

# Extracción del resultado:
results %>% subset(aic == min(results$aic))
results %>% subset(bic == min(results$bic))

# Estimación del mejor modelo:
model <- returnAAPL %>% 
  arima(order = c(0, 0, 0)) # Empleamos el BIC en este caso.
summary(model)

# 2.4| Pruebas diagnóstico ------------------------------------------------
residuales <- residuals(model)

# 2.4.1| No-autocorrelación -----------------------------------------------
numeroDeRezagos <- round(x = length(residuales)/4, digits = 0)
Box.test(residuales, lag = numeroDeRezagos, type = c('Box-Pierce'))
Box.test(residuales, lag = numeroDeRezagos, type = c('Ljung-Box'))

# 2.4.2| Normalidad -------------------------------------------------------
# A continuación se presenta un análisis gráfico. Sin embargo, debería
# presentarse la prueba de Jarque Bera.
plot(density(residuales))

qqnorm(residuales, 
       xlab = 'Cuantiles de la distribución normal',
       ylab = 'Cuantiles empíricos',
       pch  = 19,
       col  = 'steelblue', 
       main = 'Q-Q plot de los errores del modelo ARIMA(0, 0, 0)')
qqline(residuales,
       lty = 2,
       lwd = 2,
       col = 'tomato')

# 2.4.3| Homocedasticidad -------------------------------------------------
# Prueba tradicional ------------------------------------------------------
residuales <- as.matrix(residuales)
res <- embed(residuales,20)
u   <- res[,1]
efectosARCH <- lm(u^2 ~ res[,2]^2+res[,3]^2+res[,4]^2+res[,5]^2+res[,6]^2+
                    res[,7]^2+res[,8]^2+res[,9]^2+res[,10]^2+res[,11]^2+
                    res[,12]^2+res[,13]^2+res[,14]^2+res[,15]^2+res[,16]^2+
                    res[,17]^2+res[,18]^2+res[,19]^2)
summary(efectosARCH)

arch.test(model, output = TRUE)

# Prueba asimétrica -------------------------------------------------------
signos  <- sign(residuales)
S.menos <- rep(x = -1, times = length(signos))
S.mas   <- rep(x = -1, times = length(signos))
for (i in seq_len(length(signos))) {
  if (signos[i] < 0) {
    S.menos[i] <- 1
    S.mas[i]   <- 0 
  } else {
    S.menos[i] <- 0
    S.mas[i]   <- 1 
  }
}

S.menos.rezago <- embed(S.menos,2) 
S.mas.rezago   <- embed(S.mas,2) 
eps.rezago     <- embed(residuales,2)
eps.rezago.2   <- eps.rezago[,1]^2
producto.menos <- eps.rezago[,2] * S.menos.rezago[,2]
producto.mas   <- eps.rezago[,2] * S.mas.rezago[,2]

# ¿Importa el signo del choque?
# H0: No hay asimetría por el signo del choque.
# HA: Existe asimetría por el signo del choque.
Engle.Ng   <- lm(eps.rezago.2 ~ S.menos.rezago[,2])
resultados <- summary(Engle.Ng)
resultados

# ¿Importa tanto el signo como el tamaño?
# H0: No hay asimetría ni por el signo ni por el tamaño del choque.
# HA: Existe asimetría tanto por el signo como por el tamaño del choque.
Engle.Ng2   <- lm(eps.rezago.2 ~ S.menos.rezago[,2] + producto.menos + producto.mas)
resultados2 <- summary(Engle.Ng2)
resultados2

# 2.5| Modelo GARCH -------------------------------------------------------
# Librería fGarch ---------------------------------------------------------
# Versión simétrica:
newModel <- garchFit(formula      = ~ arma(0, 0) + garch(1, 1), 
                     data         = returnAAPL, 
                     cond.dist    = c("norm"), 
                     include.mean = FALSE)
summary(newModel)
plot(newModel)

# En los datos no se encontró evidencia de efectos asimétricos, pero en caso
# que suceda en alguna de las series de tiempo.
# - GJR-GARCH: es un caso particular del APARCH, donde delta = 2.
newModel <- garchFit(formula       = ~ arma(0, 0) + aparch(1, 1), 
                     data          = returnAAPL, 
                     delta         = 2,
                     include.delta = FALSE)
summary(newModel)
plot(newModel)

# Para obtener el pronóstico de la serie:
pronostico <- predict(newModel, n.ahead = 12)

# Librería rugarch --------------------------------------------------------
specs    <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                       distribution.model = "norm")
newModel <- ugarchfit(spec = specs, data = returnAAPL)
plot(newModel)

pronostico <- ugarchforecast(fitORspec = newModel, data = returnAAPL, n.ahead = 12)
plot(pronostico)

specs <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                    distribution.model = "norm")
newModel <- ugarchfit(spec = specs, data = returnAAPL)
plot(newModel)

# 3| Métricas de evaluación -----------------------------------------------
# ¡Ojo! Esto tiene que ser por fuera de muestra.
RMSE <- sqrt((observado-predicho)^2)

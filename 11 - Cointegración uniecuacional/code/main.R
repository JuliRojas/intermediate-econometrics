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
library(forecast)
library(tseries)
library(dynlm)
library(urca)
library(aTSA)
library(scales)

# Directorios donde se almacena la información del proyecto:
directorioPrincipal         <- enc2native(here())
directorioCodigo            <- paste0(directorioPrincipal, '/code/')
directorioDatosCrudos       <- paste0(directorioPrincipal, '/input/raw/')
directorioDatosLimpios      <- paste0(directorioPrincipal, '/input/cleaned/')
directorioResultadosFiguras <- paste0(directorioPrincipal, '/output/figures/')
directorioResultadosTablas  <- paste0(directorioPrincipal, '/output/tables/')
setwd(directorioPrincipal)

# 1.2| Bases de datos -----------------------------------------------------
USMacroSWQ <- read_xlsx(path      = paste0(directorioDatosCrudos, 'us_macro_quarterly.xlsx'),
                        sheet     = 1,
                        col_types = c('text', rep('numeric', 9)))

USMacroSWQ <- USMacroSWQ[, c(1, which(colnames(USMacroSWQ) %in% c('TB3MS', 'GS10')))]
colnames(USMacroSWQ) <- c('Fecha', '3M', '10Y')
  
# 2| Limpieza -------------------------------------------------------------
USMacroSWQ$Fecha <- as.yearqtr(USMacroSWQ$Fecha, format = '%Y:0%q')
TB3MS   <- ts(USMacroSWQ$`3M`, start = c(1957, 1), end = c(2013,4), frequency = 4)
TB10YS  <- ts(USMacroSWQ$`10Y`,start = c(1957, 1), end = c(2013,4), frequency = 4)
TSpread <- TB10YS - TB3MS

# 3| Visualización --------------------------------------------------------
# 3.1| Comportamiento conjunto --------------------------------------------
# Ambas series parecen tener el mismo comportamiento de largo plazo y el
# spread generado entre las curvas pareciera ser estacionario, tal y como
# predice la teoria de las expectativas racionales sobre la estructura
# a termino.
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)), 
     plot.type = 'single', 
     col       = c('darkred', 'steelblue'),
     lwd       = 2,
     xlab      = 'Fecha',
     ylab      = 'Tasa de interés efectiva anual',
     main      = 'Comparación de tasas a 3 meses vs. tasas a 10 años')

# Definimos una función que retorna, para el número ingresado, una cadena de
# texto conformada tanto por el año como por los cuatro semestres que podrían
# observarse en dicho año.
funcionFechas <- function(x) {
  fechas <- sort(as.yearqtr(sapply(X = x, FUN = paste, c('Q1', 'Q2', 'Q3', 'Q4'))))
  return(fechas)
}

# Dados unos años en que se evidenció recesiones, creamos un vector que 
# contiene los cuatro trimestres para cada uno de los años reportado.
fechasRecesiones <- c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991, 2001, 2007:2008)
recesiones <- funcionFechas(fechasRecesiones)

# Dado el vector de crisis, coloreamos la serie de tiempo para ver si hay
# un comportamiento heterogéneo a través de los ciclos económicos.
xblocks(x   = time(as.zoo(TB3MS)), 
        y   = time(TB3MS) %in% recesiones, 
        col = alpha('steelblue', alpha = 0.3))

# Finalmente, añadimos una leyenda para distinguir una tasa de interés sobre
# otra.
legend('topright', 
       legend = c('Bonos a 3 meses', 'Bonos a 10 años'),
       col    = c('darkred', 'steelblue'),
       lwd    = c(2, 2))

# 3.2| Comportamiento del spread ------------------------------------------
# Notese que antes de las recesiones las brechas tienden a disminuir.
plot(as.zoo(TSpread), 
     col = 'steelblue',
     lwd = 2,
     xlab = 'Fecha',
     ylab = 'Puntos porcentuales',
     main = 'Spread entre bonos de 10 años y 3 meses')

# Como en el caso anterior, analizamos qué sucede con el spread ante períodos
# de crisis.
xblocks(x   = time(as.zoo(TB3MS)), 
        y   = time(TB3MS) %in% recesiones, 
        col = alpha('steelblue', alpha = 0.3))

# 3.3| Todo junto ---------------------------------------------------------
# Modificamos los límites con respecto al eje y para graficar no solo las 
# tasas, sino los puntos básicos producto del spread entre las tasas de 
# interés.
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)), 
     plot.type = 'single', 
     lty       = c(2, 1),
     lwd       = 2,
     xlab      = 'Fecha',
     ylab      = 'Puntos porcentuales',
     ylim      = c(-5, 17),
     main      = 'Comparación de tasas a 3 meses vs. tasas a 10 años')

# Añadimos el spread y coloreamos el área entre las curvas de tasas de interés
# observadas a través de la estructura a término,
lines(as.zoo(TSpread), 
      col = 'steelblue',
      lwd = 2,
      xlab = 'Fecha',
      ylab = 'Puntos porcentuales',
      main = 'Comparación de tasas a 3 meses vs. tasas a 10 años')

polygon(x = c(time(TB3MS), rev(time(TB3MS))), 
        y = c(TB10YS, rev(TB3MS)),
        col = alpha('steelblue', alpha = 0.3),
        border = NA)

# Añadimos una línea horizontal en y = 0, con el objetivo de ver si hay alguna
# convergencia en el largo plazo para el spread.
abline(0, 0)

# Finalmente, añadimos la leyenda
legend('topright', 
       legend = c('Bonos a 3 meses', 'Bonos a 10 años', 'Spread'),
       col = c('black', 'black', 'steelblue'),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1))

# 3| Modelo de corrección de errores --------------------------------------
# 3.1| Validación de la cointegración -------------------------------------
# A| Bonos a 3 meses ------------------------------------------------------
summary(ur.df(y = TB3MS, type = 'trend', selectlags = 'AIC'))
summary(ur.df(y = TB3MS, type = 'drift', selectlags = 'AIC'))
summary(ur.df(y = TB3MS, type = 'none', selectlags = 'AIC'))

# Como no hubo ni tendencia ni intercepto en la serie original, en la version
# diferenciada en realidad tampoco deberíamos encontrar términos 
# determinísticos.
summary(ur.df(y = diff(TB3MS), type = 'none', selectlags = 'AIC'))

# Conclusión: la serie es I(1).

# B| Bonos a 10 años ------------------------------------------------------
summary(ur.df(y = TB10YS, type = 'trend', selectlags = 'AIC'))
summary(ur.df(y = TB10YS, type = 'drift', selectlags = 'AIC'))
summary(ur.df(y = TB10YS, type = 'none', selectlags = 'AIC'))

# Como no hubo ni tendencia ni intercepto en la serie original, en la version
# diferenciada en realidad tampoco deberíamos encontrar términos 
# determinísticos.
summary(ur.df(y = diff(TB10YS), type = 'none', selectlags = 'AIC'))

# Conclusión: la serie es I(1).

# C| Errores de MCO -------------------------------------------------------
modelo     <- lm(TB10YS ~ TB3MS)
residuales <- resid(modelo)

# ¿Cómo se comportan los residuales?
par(mfrow = c(1,2))
acf(x     = residuales,
    plot  = TRUE,
    lwd   = 2,
    xlab  = 'Rezagos',
    main  = 'Función de autocorrelación simple de los residuales') 
pacf(x    = residuales,
     plot = TRUE,
     lwd  = 2,
     xlab = 'Rezagos',
     main = 'Función de autocorrelación parcial de los residuales')
par(mfrow = c(1,1))

# Conclusión gráfica: si bien no son ruido blanco, son estacionarios. Sin
# embargo, ¡veamos una prueba estadística más formal! En particular,
# la prueba de Engle y Granger (1987), donde emplearemos los valores críticos 
# calculados por MacKinnon (1991).
coint.test(y = TB10YS, X = TB3MS, nlag = 3)

# Conclusión: al rechazar la hipótesis nula, concluimos estacionariedad en el
# error y podemos argumentar una posible relación de cointegración entre
# las variables.

# 3.2| Estimación del modelo de corrección de errores ---------------------
# A| Configurar el error a corregir ---------------------------------------
# Nótese que es equivalente a los residuales previamente calculados.
correccion <- TB10YS - modelo$coefficients[1] - modelo$coefficients[2] * TB3MS

# B| Estimación -----------------------------------------------------------
# De hecho, podríamos hacer una red de búsqueda minimizando algún criterio
# de información.
modelo <- dynlm(d(TB10YS) ~ L(correccion) + L(d(TB3MS), 1:2) + L(d(TB10YS), 1:2))
names(modelo$coefficients) <- c('Intercepto', 'Corrección', 'D TB3MS L1',
                                'D TB3MS L2', 'D TB10YS L1', 'D TB10YS L2') 
summary(modelo)

# C| Supuestos ------------------------------------------------------------
residuales <- resid(modelo)
Box.test(x    = residuales,
         lag  = 20, 
         type = 'Box-Pierce') 
Box.test(x   = residuales,
         lag = 20,
         type = 'Ljung-Box')

# Conclusión: al rechazar la hipótesis nula, concluimos autocorrelación en
# el error y requerimos de estimar una especificación menos parsimoniosa que
# nos permita obtener residuales ruido blanco. 

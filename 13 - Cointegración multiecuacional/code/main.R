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
library(tidyquant)
library(xts)
library(vars)
library(urca)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(gridExtra)

# 2| Petróleo y USDCOP ----------------------------------------------------
# 2.1| Preparación de datos -----------------------------------------------
# Descargamos de Yahoo Finance los datos de los precios del petróleo bajo
# una referencia Brent (dado que es la utilizada en Colombia).
brent <- getSymbols(Symbols     = 'BZ=F', 
                    from        = '2008-01-01', 
                    to          = '2022-01-01', 
                    auto.assign = FALSE)
brent <- brent[, 'BZ=F.Close']

# Descargamos de Yahoo Finance los datos de la negociación USD/COP --o, lo
# que es lo mismo, la cantidad de pesos que se paga por un dólar
# estadounidense--. 
USDCOP <- getSymbols(Symbols     = 'USDCOP=X', 
                     from        = '2008-01-01', 
                     to          = '2022-01-01', 
                     auto.assign = FALSE)
USDCOP <- USDCOP[, 'USDCOP=X.Close']

# Nótese que hay algunos valores atípicos y que parecen no tener sentido.
# Podríamos limpiarlos, pero en este caso vamos a tratarlos como valores
# perdidos.
USDCOP[USDCOP$`USDCOP=X.Close` < 1000, ] <- NA_real_

# Finalmente, reemplazamos cualquier valor no observado por el último
# precio observado en la serie de tiempo.
data <- na.locf(merge(brent, USDCOP)) |> na.omit()
colnames(data) <- c('BRENT', 'USDCOP')

# 2.2| Visualización ------------------------------------------------------
# La idea de visualizar a dos ejes es ver si a las variables las mueve una 
# fuerza económica en común. Para ello, nótese que las series de tiempo 
# trabajadas en 'xts' no tienen nombres de fila, sino un índice. La idea es
# transformar las variables de 'xts' a 'tibble', tal que podamos ver el
# patrón a través de ggplot2.
dates    <- index(data)
dataPlot <- as_tibble(data) |>
  add_column(FECHA = dates, .before = 1) |> 
  drop_na()

# Creamos el factor de escala. Para más información, véase: 
# https://stackoverflow.com/questions/5294955/how-to-scale-down-a-range-of-numbers-with-a-known-min-and-max-value
scaleFactor <- (max(dataPlot$BRENT) - min(dataPlot$BRENT)) / (max(dataPlot$USDCOP) - min(dataPlot$USDCOP))

# Finalmente, la escala a dos ejes muestra:
ggplot(dataPlot, aes(x = FECHA)) +
  scale_colour_manual(values = c('BRENT' = '#00AFBB', 'USDCOP' = '#E7B800'),
                      name   = 'Series', 
                      labels = c('Precio de referencia Brent', 'Tipo de cambio USD/COP')) +
  geom_line(aes(y = BRENT, colour = 'BRENT')) +
  geom_line(aes(y = (USDCOP - min(dataPlot$USDCOP)) * scaleFactor + min(dataPlot$BRENT), 
                colour = 'USDCOP')) +
  scale_y_continuous(name     = 'Precio de referencia Brent\n(Medido en dólares estadounidenses)', 
                     limits   = c(min(dataPlot$BRENT), max(dataPlot$BRENT)), 
                     sec.axis = sec_axis(~(. - min(dataPlot$BRENT))/scaleFactor + min(dataPlot$USDCOP), 
                                         name = 'Tipo de cambio USD/COP\n(Medido en pesos colombianos)',
                                         labels   = scales::label_number(scale = 1, accuracy = 1, big.mark = ','))) + 
  labs(title    = paste0('Relación a través del tiempo del precio de referencia Brent\n', 
                         'y el tipo de cambio USD/COP'),
       caption  = 'Fuente: elaboración propia, con base en los datos extraídos de Yahoo Finance.', 
       x        = 'Fecha') +
  theme_classic() +
  theme(plot.title            = element_text(size = 12, face = 'bold', hjust = 0.5),
        plot.subtitle         = element_text(face = 'italic'),
        plot.caption          = element_text(hjust = 1, face = 'italic'),
        legend.title          = element_text(face = 'bold'),
        axis.title.y.right    = element_text(angle = 90),
        legend.position       = 'bottom',
        legend.justification  = 'center',
        legend.box.margin     = margin(6, 6, 6, 6)) +
  guides(color = guide_legend(''))

# 2.3| Estimación ---------------------------------------------------------
# El procedimiento consiste en cinco pasos:
# A. Verificar el orden de integración de las variables a analizar
# B. Identificar el número de rezagos del VECM a partir de los criterios
#    de información del VAR en nivel.
# C. Determinar el rango de la matriz Pi (es decir, el número de relaciones
#    de cointegración).
# D. Validar supuestos.
# E. Usar el modelo.

# Sin embargo, antes de continuar, nótese que la frecuencia de los datos hasta
# ahora ha sido diaria. No obstante, las técnicas abordadas en el curso no
# cubren frecuencias tal altas. Por consiguiente, vamos a cortar por el
# último precio observado en cada uno de los meses.
data <- do.call(rbind, lapply(split(data, 'months'), function(x) {
    tail(x, 1)
  }))

# A| Estacionariedad ------------------------------------------------------
# Con respecto al precio del petróleo...
summary(ur.df(y = data$BRENT, type = 'trend', selectlags = 'AIC'))
summary(ur.df(y = data$BRENT, type = 'drift', selectlags = 'AIC'))

summary(ur.df(y = na.omit(diff(data$BRENT)), type = 'none', selectlags = 'AIC'))

# ... concluimos que es una variable integrada de primer orden. Así mismo,
# para el caso del tipo de cambio...
summary(ur.df(y = data$USDCOP, type = 'trend', selectlags = 'AIC'))

summary(ur.df(y = na.omit(diff(data$USDCOP)), type = 'drift', selectlags = 'AIC'))
summary(ur.df(y = na.omit(diff(data$USDCOP)), type = 'none', selectlags = 'AIC'))

# ... también concluimos que es una variable integrada de primer orden. Por
# tanto, es posible analizar una representación de corrección de errores.

# B| Identificación -------------------------------------------------------
# En búsqueda de un modelo parsimonioso, y entendiendo la posibilidad del
# cumplimiento de la hipótesis de mercados eficientes, nos decantamos
# por la estimación de un VECM(1).
VARselect(y = data, type = 'const', lag.max = 12)

# C| Prueba de cointegración ----------------------------------------------
# Analizamos la necesidad de incluir un término constante en el vector
# de cointegración a través de una regresión lineal...
summary(lm(USDCOP ~ BRENT, data = data))

# ... y concluimos que el vector de cointegración debería contener un término
# constante.

# - Prueba de la traza.
#   H0: el número de relaciones de cointegración es igual al evaluado.
#   HA: existen más relaciones de cointegración.
summary(
  ca.jo(x      = data, 
        ecdet  = 'const', 
        type   = 'trace', 
        K      = 2, 
        spec   = 'transitory',
        season = NULL)
)

# - Prueba del máximo valor propio.
#   H0: el número de relaciones de cointegración es igual al evaluado.
#   HA: existen más relaciones de cointegración.
summary(
  ca.jo(x      = data, 
        ecdet  = 'const', 
        type   = 'eigen', 
        K      = 2, 
        spec   = 'transitory',
        season = NULL)
)

# Por lo anterior, concluimos que no existe evidencia estadística para
# argumentar una relación de cointegración. Sin embargo, puede ser que, si
# bien existe una relación de cointegración, esta no fue constante a través
# del tiempo. Por consiguiente, realizamos una prueba que tiene en cuenta
# un posible cambio estructural en el proceso (en algún punto desconocido).
summary(
  cajolst(x      = data, 
          trend  = FALSE, 
          K      = 2, 
          season = NULL)
)

# En realidad, podríamos emplear una metodología más avanzada para modelar
# la relación de equilibrio variable a través del tiempo. Sin embargo, al nivel
# de este curso, lo que tendríamos que hacer es estimar el VAR en diferencias.

# D| Supuestos ------------------------------------------------------------
# Para validar los supuestos sobre los errores, es necesario primero extraerlos.
# Por consiguiente, realizamos la estimación con la especificación que
# encontramos anteriormente.
temp <- ca.jo(x      = data, 
              ecdet  = 'const', 
              type   = 'trace', 
              K      = 2, 
              spec   = 'transitory',
              season = NULL)

# La función que se empleará a continuación realiza una estimación por MCO.
# La literatura parece indicar una preferencia por MV, pero eso no es posible
# con el paquete 'vars'. Para ello, tendríamos que emplear 'tsDyn'.
VEC <- cajorls(z = temp, r = 1)
residuals <- VEC[['rlm']][['residuals']]

# Analizamos, gráficamente, los resultados de los residuales.
plotres(temp)

# Y, con el fin de que podamos emplear las funciones utilizadas en las clases
# anteriores, convertimos el VECM a un VAR.
VAR <- vec2var(z = temp, r = 1)

# E| Uso ------------------------------------------------------------------
# Es posible analizar el pronóstico, funciones de impulso-respuesta, y
# descomposición del error de pronóstico. Por ejemplo, los pronósticos son:
predict(VAR, n.ahead = 4)
fanchart(predict(VAR, n.ahead = 4))

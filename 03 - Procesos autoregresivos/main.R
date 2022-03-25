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
library(forecast) # Realizar pronósticos con los modelos ARMA.
library(lmtest)   # Significancia individual de los coeficientes ARMA.
library(tseries)  # Estimar modelos de series de tiempo.
library(scales)

# 1.2| Valores iniciales --------------------------------------------------
set.seed(seed = 123)
obs   <- 120 # Observaciones suficientes para 10 años.
nSim  <- 100 # Número de caminos que el proceso estocástico puede tomar.
nLag  <- 20  # Número de rezagos para validar la ACP y PACF.
auxTS <- sample(x = seq_len(length.out = nSim), size = 1)

# 2| Simulaciones ---------------------------------------------------------
# 2.1| Proceso de ruido blanco --------------------------------------------
# A| Único camino ---------------------------------------------------------
yt_rb <- ts(data      = arima.sim(model = list(order = c(0,0,0)), 
                                  n     = obs, 
                                  innov = rnorm(n = obs, mean = 0, sd = 1)), 
            start     = c(2000, 1), 
            frequency = 12) 

plot(yt_rb,
     type = 'l',
     main = paste0('Simulación de un proceso ruido blanco\n',
                   'Generado con una innovación que distribuye normal estándar'),
     sub  = 'Nota: elaboración propia.',
     xlab = 'Fecha',
     ylab = 'Realización',
     col  = 'red')

# B| Múltiples caminos ----------------------------------------------------
yt_rb <- ts(data      = replicate(n = nSim,
                                  expr = arima.sim(model = list(order = c(0,0,0)), 
                                                   n     = obs, 
                                                   innov = rnorm(n = obs, mean = 0, sd = 1))), 
            start     = c(2000, 1), 
            frequency = 12) 

# Visualización de los 10 primeros caminos simulados.
plot(yt_rb[,1:10],
     type = 'l',
     main = 'Visualización de 10 posibles caminos',
     sub  = 'Nota: elaboración propia.',
     xlab = 'Fecha',
     ylab = 'Realización',
     col  = 'red')

# Visualización de todos los caminos simulados (densidad de los caminos).
matplot(yt_rb,
        type = 'l',
        main = paste0('Posible comportamiento del ruido blanco\n',
                      'Resultado para ', nSim, ' simulaciones'),
        sub  = 'Nota: elaboración propia.',
        xlab = 'Fecha',
        ylab = 'Realización',
        col  = alpha('red', 0.2))

# Presentación de los resultados como en el caso del profesor.
par(mfrow = c(1, 2)) # El entorno gráfico se divide en una fila y dos columnas.
matplot(yt_rb,
        type = 'l',
        main = paste0('Posible comportamiento del ruido blanco\n',
                      'Resultado para ', nSim, ' simulaciones'),
        sub  = 'Nota: elaboración propia.',
        xlab = 'Fecha',
        ylab = 'Realización',
        col  = alpha('red', 0.2))

plot(yt_rb[, auxTS],
     type = 'l',
     main = 'Visualización de uno de los posibles caminos',
     sub  = 'Nota: elaboración propia.',
     xlab = 'Fecha',
     ylab = 'Realización',
     col  = 'red')
par(mfrow = c(1, 1)) # Retornar a los valores por defecto.

# Análisis de la función de autocorrelación y autocorrelación parcial.
par(mfrow = c(1, 2))
acf(yt_rb[, auxTS],
    lag.max = nLag, 
    plot    = T, 
    main    = paste0('Función de Autocorrelación del Proceso Generador de Datos\n',
                     'ARMA(0,0) (Ruido Blanco)'),
    sub     = paste0('Nota: elaboración propia.\n',
                     'Las bandas azules indican umbrales de significancia estadística.'),
    xlab    = 'Número de rezagos',
    ylab    = 'Coeficiente de autocorrelación') 
pacf(yt_rb[, auxTS],
     lag.max = nLag,
     plot    = T,
     main    = paste0('Función de Autocorrelación Parcial del Proceso Generador de Datos\n',
                      'ARMA(0,0) (Ruido Blanco)'),
     sub     = paste0('Nota: elaboración propia.\n',
                      'Las bandas azules indican umbrales de significancia estadística.'),
     xlab    = 'Número de rezagos',
     ylab    = 'Coeficiente de autocorrelación parcial')
par(mfrow = c(1, 1))

# 2.2| Proceso autorregresivo de orden 1 ----------------------------------
# Entre mayor sea el coeficiente, más lento va a converger el proceso a su 
# media, pues las perturbaciones del pasado afectan en mayor medida su
# estado actual.

# A| Forma manual ---------------------------------------------------------
innov     <- rnorm(n = obs, mean = 0, sd = 1)
yt1s      <- vector(mode = 'numeric', length = obs) # Inicialización del vector.

# Vector generador del AR(1) con coeficiente de 0.6 para su primer rezago.
for (t in seq_len(obs)) {
    yt1s[t+1] <- 1 + 0.6 * yt1s[t] + innov[t]
}

yt1s <- ts(data = yt1s, start = c(2000, 1), frequency = 12)

# B| Forma automática -----------------------------------------------------
yt1s <- ts(data      = arima.sim(model = list(order = c(1,0,0),
                                              ar    = c(0.6)), 
                                 n     = obs, 
                                 innov = rnorm(n = obs, mean = 0, sd = 1)), 
            start     = c(2000, 1), 
            frequency = 12) 

# La función no permite agregar términos determinísticos a la ecuación.
# Luego, para agregar una media al proceso lo que se hace es:
mu   <- 5
yt1s <- mu + yt1s

# Visualización del proceso.
# Nota: en caso de querer incluir expresiones en una gráfica, véase
# https://www.r-exercises.com/2017/10/21/mathematical-expressions-in-r-plots-tutorial/
plot(yt1s,
     type = 'l',
     main = paste0('Simulación de un proceso AR(1)\n',
                   'Generado con una innovación que distribuye normal estándar'),
     sub  = 'Nota: elaboración propia.',
     xlab = 'Fecha',
     ylab = 'Realización',
     col  = 'red')
abline(h = mu, col = 'grey', lty = 2) # Línea punteada que representa la media.

# Análisis de la función de autocorrelación y autocorrelación parcial.
# - En un AR(1) el coeficiente de correlación sigue rho_s = a_1^s.
# - Por el contrario, la autocorrelación parcial es 0 para todo s > 1.
# - En la PACF se encuentra las primeras p (del AR(p)) diferentes de cero.
# - ¿Qué sucedería si el coeficiente asociado fuese negativo?
par(mfrow = c(1, 2))
acf(yt1s,
    lag.max = nLag, 
    plot    = T, 
    main    = paste0('Función de Autocorrelación del\n',
                     'Proceso Generador de Datos AR(1)'),
    sub     = paste0('Nota: elaboración propia.\n',
                     'Las bandas azules indican umbrales de significancia estadística.'),
    xlab    = 'Número de rezagos',
    ylab    = 'Coeficiente de autocorrelación') 
pacf(yt1s,
     lag.max = nLag,
     plot    = T,
     main    = paste0('Función de Autocorrelación Parcial del\n',
                      'Proceso Generador de Datos AR(1)'),
     sub     = paste0('Nota: elaboración propia.\n',
                      'Las bandas azules indican umbrales de significancia estadística.'),
     xlab    = 'Número de rezagos',
     ylab    = 'Coeficiente de autocorrelación parcial')
par(mfrow = c(1, 1))

# C| Raíces del proceso ---------------------------------------------------
# El proceso es estacionario si la raíz descansa por fuera del círculo unitario.
ar1 <- arima(yt1s, 
             order          = c(1, 0, 0), 
             include.mean   = FALSE, 
             transform.pars = FALSE,
             method         = 'ML')
polyroot(c(1, -ar1$coef))
Mod(polyroot(c(1, -ar1$coef)))

# 2.3| Proceso no-estacionario --------------------------------------------
# A| Serie cruda ----------------------------------------------------------
yt1n <- ts(data      = arima.sim(model = list(order = c(1,1,0),
                                              ar    = c(0.6)), 
                                 n     = obs, 
                                 innov = rnorm(n = obs, mean = 0, sd = 1)), 
           start     = c(2000, 1), 
           frequency = 12) 

plot(yt1n,
     type = 'l',
     main = paste0('Simulación de una ecuación en diferencias estocástica de primer orden\n',
                   'Generado con una innovación que distribuye normal estándar'),
     sub  = 'Nota: elaboración propia.',
     xlab = 'Fecha',
     ylab = 'Realización',
     col  = 'red')

# Nótese que el proceso es altamente persistente, pues incluso con 20 rezagos 
# la autocorrelación persiste. En otros términos, el proceso NO es débilmente 
# dependiente del pasado y, por tanto, NO es estacionario.

# Análisis de la función de autocorrelación y autocorrelación parcial.
par(mfrow = c(1, 2))
acf(yt1n,
    lag.max = nLag, 
    plot    = T, 
    main    = paste0('Función de Autocorrelación del\n',
                     'Proceso Generador de Datos No-Estacionario'),
    sub     = paste0('Nota: elaboración propia.\n',
                     'Las bandas azules indican umbrales de significancia estadística.'),
    xlab    = 'Número de rezagos',
    ylab    = 'Coeficiente de autocorrelación') 
pacf(yt1n,
     lag.max = nLag,
     plot    = T,
     main    = paste0('Función de Autocorrelación Parcial del\n',
                      'Proceso Generador de Datos No-Estacionario'),
     sub     = paste0('Nota: elaboración propia.\n',
                      'Las bandas azules indican umbrales de significancia estadística.'),
     xlab    = 'Número de rezagos',
     ylab    = 'Coeficiente de autocorrelación parcial')
par(mfrow = c(1, 1))

# B| Serie diferenciada ---------------------------------------------------
diffyt1n <- diff(yt1n)
auxMean <- mean(diffyt1n)
plot(diffyt1n,
     type = 'l',
     main = paste0('Diferecnai de la ecuación en diferencias estocástica de primer orden\n',
                   'Generado con una innovación que distribuye normal estándar'),
     sub  = 'Nota: elaboración propia.',
     xlab = 'Fecha',
     ylab = 'Realización',
     col  = 'red')
abline(h = auxMean, col = 'grey', lty = 2)

# Análisis de la función de autocorrelación y autocorrelación parcial.
par(mfrow = c(1, 2))
acf(diffyt1n,
    lag.max = nLag, 
    plot    = T, 
    main    = paste0('Función de Autocorrelación del\n',
                     'Proceso Generador de Datos Estacionario'),
    sub     = paste0('Nota: elaboración propia.\n',
                     'Las bandas azules indican umbrales de significancia estadística.'),
    xlab    = 'Número de rezagos',
    ylab    = 'Coeficiente de autocorrelación') 
pacf(diffyt1n,
     lag.max = nLag,
     plot    = T,
     main    = paste0('Función de Autocorrelación Parcial del\n',
                      'Proceso Generador de Datos Estacionario'),
     sub     = paste0('Nota: elaboración propia.\n',
                      'Las bandas azules indican umbrales de significancia estadística.'),
     xlab    = 'Número de rezagos',
     ylab    = 'Coeficiente de autocorrelación parcial')
par(mfrow = c(1, 1))

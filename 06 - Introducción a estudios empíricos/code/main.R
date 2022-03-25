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
library(tidyverse)
library(dynlm)
library(urca)

# Directorios donde se almacena la información del proyecto:
directorioPrincipal         <- enc2native(here())
directorioCodigo            <- paste0(directorioPrincipal, '/code/')
directorioDatosCrudos       <- paste0(directorioPrincipal, '/input/raw/')
directorioDatosLimpios      <- paste0(directorioPrincipal, '/input/cleaned/')
directorioResultadosFiguras <- paste0(directorioPrincipal, '/output/figures/')
directorioResultadosTablas  <- paste0(directorioPrincipal, '/output/tables/')
setwd(directorioPrincipal)

# 1.4| Valores iniciales --------------------------------------------------
# Valores necesarios para recuperar los valores obtenidos por Dickey-Fuller.
set.seed(seed = 2022)
obs   <- 1000 # Observaciones.
nSim  <- 2000 # Número de caminos que el proceso estocástico puede tomar.
drift <- 0.5  # Deriva.
nLag  <- 20   # Número de rezagos para validar la ACP y PACF.

# 1.3| Bases de datos -----------------------------------------------------
# Se carga, en primer instancia, como un data frame pero, posteriormente,
# se convierte en una serie de tiempo con frecuencia mensual.
IPP <- readxl::read_xls(path      = paste0(directorioDatosCrudos, 
                                           'IPP a Febrero de 2022.xls'), 
                        sheet     = 'IPP Histórico', 
                        range     = 'C6:C278',
                        col_names = FALSE)
IPPin  <- IPP[seq_len(224), 1, drop = TRUE]
IPPout <- IPP[224:length(IPP), 1, drop = TRUE]
IPP <- ts(data = IPPin, start = c(1999, 6), frequency = 12)

# 2| Simulación de los valores críticos -----------------------------------
# Las funciones comentadas no pueden llevar carácteres especiales, a diferencia
# de los comentarios tradicionales que realizamos en el script.

#' Simulacion con la especificacion de Dickey-Fuller
#'
#' @param n Numero de observaciones.
#' @param N Numero de simulaciones
#' @param type Tipo de prueba. Puede ser \code{two}, \code{one}, o \code{none}.
#' @param drift Valor del termino constante en el PGD.
#'
#' @return Valores de las simulaciones.
simulationDF <- function(n, N, type, drift){
  # El estadístico de DF surge como el estadístico-t de una regresión, cuya 
  # forma funcional depende de los términos determíniscos que tenga la prueba.
  # 'sim_df' almacena los valores del parámtero de interés (rho estimado) y
  # del estadístico-t.
  
  trend  <- seq_len(n) # Tendencia lineal.
  sim_df <- data.frame(rho = numeric(), 
                       tau = numeric())
  if (type == 'two') {
    
    for (i in seq_len(N)) {
      
      # 1. Se simula la serie de tiempo.
      yt <- arima.sim(model = list(order = c(0,1,0)), 
                      n     = n - 1, 
                      innov = rnorm(n, mean = 0, sd =1)) + 
        drift + trend
      # 2. Se obtiene el estadístico t de Dickey-Fuller asociado a la anterior 
      #    simulación. Para la regresión, se usa el comando 'dynlm', que es 
      #    un comando que permite estimar modelos vía mínimos cuadrados 
      #    ordinarios dinámicos.
      #    - diff(yt,1): primera diferencia de la serie.
      #    - L(yt,1): serie rezagada.
      #    - trend(yt): tendencia determinística de la serie. 
      summa <- summary(dynlm(diff(yt, 1) ~ L(yt, 1) + trend(yt))) 
      sim_df[i,] = c(summa$coef[2,1], summa$coef[2,3])
    }
    
  } else if (type == 'one') {
    
    for (i in seq_len(N)) {
      
      # 1. Se simula la serie de tiempo.
      yt <- arima.sim(model = list(order = c(0,1,0)), 
                      n     = n - 1, 
                      innov = rnorm(n, mean = 0, sd =1)) + 
        drift
      # 2. Se obtiene el estadístico t de Dickey-Fuller asociado a la anterior 
      #    simulación. Para la regresión, se usa el comando 'dynlm', que es 
      #    un comando que permite estimar modelos vía mínimos cuadrados 
      #    ordinarios dinámicos.
      summa <- summary(dynlm(diff(yt, 1) ~ L(yt, 1))) 
      sim_df[i,] = c(summa$coef[2,1], summa$coef[2,3])
    }
    
  } else if (type == 'none') {
    
    for(i in seq_len(N)) {
      
      # 1. Se simula la serie de tiempo.
      yt <- arima.sim(model = list(order = c(0,1,0)), 
                      n     = n - 1, 
                      innov = rnorm(n, mean = 0, sd =1))
      # 2. Se obtiene el estadístico t de Dickey-Fuller asociado a la anterior 
      #    simulación. Para la regresión, se usa el comando 'dynlm', que es 
      #    un comando que permite estimar modelos vía mínimos cuadrados 
      #    ordinarios dinámicos.
      summa <- summary(dynlm(diff(yt, 1) ~ L(yt, 1) - 1)) 
      sim_df[i,] = c(summa$coef[1,1], summa$coef[1,3])
    }
    
  }
  
  return(sim_df)
}


histogram_rho <- function(df, title, xlab, fill, num_bins = 30) {
  
  result <- df %>%
    ggplot(aes(x = rho)) +
    geom_histogram(color = 'black', fill = fill, bins = num_bins) + 
    theme_classic() +
    ggtitle(title) +
    ylab('Frencuencia') +
    xlab(xlab)
  
  return(result)
}

histogram_tau <- function(df, title, xlab, fill, num_bins = 30) {
  
  result <- df %>%
    ggplot(aes(x = tau)) +
    geom_histogram(color = 'black', fill = fill, bins = num_bins, 
                   aes(y = ..density..)) + 
    geom_density() + 
    theme_classic() +
    ggtitle(title) +
    ylab('Densidad') +
    xlab(xlab)
  
  return(result)
}

# 2.1| Simulación con deriva y tendencia ----------------------------------
simulation <- simulationDF(n = obs, N = nSim, type = 'two', drift = drift)

histogramRho  <- histogram_rho(df    = simulation,
                               title = 'Rho estimado',
                               xlab  = 'Valores rho',
                               fill  = 'red1') 
histogramRho

# Histograma del estadístico-t de DF con trend y drift
histogramTau <- histogram_tau(df    = simulation,
                              title = 'Tau estimado',
                              xlab  = 'Valores tau',
                              fill  = 'red1')
histogramTau

# Valores críticos estadístico-t de DF con trend y drift
criticalValues <- round(quantile(simulation$tau, c(0.1, 0.05, 0.01)), 2)
criticalValues

# Veamos si concuerda con los resultados de DF.
trend <- seq_len(obs) # Tendencia lineal.
yt    <- arima.sim(model = list(order = c(0,1,0)), 
                   n     = obs - 1, 
                   innov = rnorm(obs, mean = 0, sd =1)) + 
  drift + trend

yt     <- ts(data = yt, start = 1, frequency = 1)
yt.adf <- ur.df(y = yt, type = 'trend', selectlags = 'BIC')
summary(yt.adf)

# 2.2| Simulación con deriva ----------------------------------------------
# TODO

# 2.3| Simulación sin determinísticas -------------------------------------
# TODO

# 3| Ejemplo práctico -----------------------------------------------------
# 3.1| Antes de diferenciar -----------------------------------------------
plot(IPP, 
     type = 'l',
     main = 'Índice de Precios al Productor',
     sub  = 'Nota: elaboración propia.',
     xlab = 'Fecha',
     ylab = 'Valor del índice',
     col  = 'red')

# H0: Raiz unitaria.
# HA: Estacionaria alrededor de una tendencia.
tendencia <- ur.df(y = IPP, type = 'trend', selectlags = 'BIC')
summary(tendencia)

# H0: Raiz unitaria.
# HA: Estacionaria alrededor de una constante diferente de cero.
constante <- ur.df(y = IPP, type = 'drift', selectlags = 'BIC')
summary(constante)

# H0: Raiz unitaria.
# HA: Estacionaria alrededor de cero.
cero <- ur.df(y = IPP, type = 'none', selectlags = 'BIC')
summary(cero)

# 3.2| Después de diferenciar ---------------------------------------------
plot(diff(log(IPP)), 
     type = 'l',
     main = 'Inflación de Precios al Productor',
     sub  = 'Nota: elaboración propia.',
     xlab = 'Fecha',
     ylab = 'Valor del índice',
     col  = 'red')

# H0: Raiz unitaria.
# HA: Estacionaria alrededor de una tendencia.
tendencia <- ur.df(y = diff(log(IPP)), type = 'trend', selectlags = 'BIC')
summary(tendencia)

# H0: Raiz unitaria.
# HA: Estacionaria alrededor de una constante diferente de cero.
constante <- ur.df(y = diff(log(IPP)), type = 'drift', selectlags = 'BIC')
summary(constante)

# H0: Raiz unitaria.
# HA: Estacionaria alrededor de cero.
cero <- ur.df(y = diff(log(IPP)), type = 'none', selectlags = 'BIC')
summary(cero)

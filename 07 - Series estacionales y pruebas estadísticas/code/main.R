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
library(lubridate)
library(RJDemetra)
library(forecast)
library(urca)
library(aTSA)
library(tsoutliers)
library(seasonal)
library(rvest)

# Directorios donde se almacena la información del proyecto:
directorioPrincipal         <- enc2native(here())
directorioCodigo            <- paste0(directorioPrincipal, '/code/')
directorioDatosCrudos       <- paste0(directorioPrincipal, '/input/raw/')
directorioDatosLimpios      <- paste0(directorioPrincipal, '/input/cleaned/')
directorioResultadosFiguras <- paste0(directorioPrincipal, '/output/figures/')
directorioResultadosTablas  <- paste0(directorioPrincipal, '/output/tables/')
setwd(directorioPrincipal)

# 1.2| Bases de datos -----------------------------------------------------
# En el ejercicio se toma los resultados observados para series nominales,
# no reales. Sin embargo, es factible modelar cualquiera de las dos.
EMC <- read_xls(path  = paste0(directorioDatosCrudos, 
                               '(DANE, 2022) EMC hasta Enero de 2022.xls'),
                sheet = '1.1',
                range = 'B7:W116')

numeroDeMeses  <- nrow(EMC)
fechaInicial   <- ymd('2013-02-01') # Añadir un mes al verdadero mes inicial.
todasLasFechas <- seq.Date(from       = fechaInicial, 
                           by         = 'month', 
                           length.out = numeroDeMeses)
todasLasFechas <- todasLasFechas %m-% days(x = 1) # La observación se da en el
                                                  # último día del mes.

EMC <- EMC %>% 
  select(-c('Año', 'Mes')) %>% 
  mutate(Fecha = todasLasFechas, .before = 1)

# 2| Análisis Exploratorio de los Datos -----------------------------------
# 2.1| ¿Cómo se comportan las series? -------------------------------------
datosVisualizacion <- EMC %>% 
  pivot_longer(cols = !Fecha, names_to = 'Variable', values_to = 'Valor') %>% 
  filter(str_detect(string = Variable, pattern = '^[1-9]')) # Expresión regular.

datosVisualizacion %>% 
  ggplot(aes(x = Fecha, y = Valor)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(. ~ Variable, scales = 'free_y') +
  labs(title    = 'Encuesta Mensual de Comercio al por Menor y Vehículos', 
       subtitle = 'Análisis desglosado por índice',
       caption  = paste0('Elaboración propia. \n', 
                         'Información extraida de: Departamento ', 
                         'Administrativo Nacional de Estadística'),
       x        = 'Fecha',
       y        = 'Índice') + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(date_labels = '%b %Y') +
  theme_classic() +
  theme(plot.title            = element_text(face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        axis.text.x           = element_text(angle = 45, hjust = 1),
        legend.position       = 'bottom')

# 2.2| ¿Qué información podemos extraer de una sola? ----------------------
# En el ejercicio se toma la serie de bebidas alcohólicas y cigarrillos, pues
# la estacionalidad presentada por la serie es bastante clara.
EMC[, 7, drop = TRUE] %>% 
  ts(start = c(2003, 1), frequency = 12) %>% 
  ggseasonplot(main = paste0('Gráfica estacional: Niveles por años\n',
                             'Discriminando por meses'),
               xlab = 'Meses') +
  scale_color_brewer(palette = 'Paired') +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.05))) +
  theme_classic() +
  theme(plot.title            = element_text(face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        legend.position       = 'right') +
  guides(color = guide_legend(title = ''))

EMC[, 7, drop = TRUE] %>% 
  ts(start = c(2003, 1), frequency = 12) %>% 
  ggseasonplot(main = paste0('Gráfica estacional: Diseño polar\n', 
                             'Discriminando por años'),
               xlab = 'Meses',
               polar = T) +
  scale_color_brewer(palette = 'Paired') +
  theme(plot.title            = element_text(face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        legend.position       = 'right') +
  guides(color = guide_legend(title = ''))

EMC[, 7, drop = TRUE] %>% 
  ts(start = c(2003, 1), frequency = 12) %>% 
  ggsubseriesplot(main = paste0('Gráfica estacional: Distribución de los valores\n', 
                                'Discriminado por meses'),
                  xlab = 'Meses') +
  scale_color_brewer(palette = 'Paired') +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.05))) +
  theme_classic() +
  theme(plot.title            = element_text(face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        legend.position       = 'right') +
  guides(color = guide_legend(title = ''))

# 3| Desestacionalización -------------------------------------------------
# Tres de los algoritmos más utilizados son los expuestos a continuación.

# 3.1| STL ----------------------------------------------------------------
# TODO: El procedimiento requiere de calibración de hiperparámetros y los
# resultados no son del todo correctos.
temp <- EMC[, 7, drop = TRUE] %>% 
  ts(start = c(2013, 1), frequency = 12) %>% 
  stl(t.window = 120, s.window = 12, robust = TRUE)

componenteEstacional <- temp$time.series[, 'seasonal', drop = TRUE]
variableOriginal     <- EMC[, 7, drop = TRUE] %>% 
  ts(start = c(2013, 1), frequency = 12)

serieSA <- variableOriginal - componenteEstacional
plot(serieSA)

# 3.2| TRAMO-SEATS --------------------------------------------------------
myspec <- tramoseats_spec(spec = 'RSAfull')
temp   <- tramoseats(series = EMC[, 7, drop = TRUE] %>% 
                       ts(start = c(2013, 1), frequency = 12), 
                     spec = myspec)
serieSA <- temp$final$series[, "sa"]
plot(serieSA)

# 3.3| X13-ARIMA-SEATS ----------------------------------------------------
# El algoritmo se emplea, típicamente, incluyendo los días festivos del
# país sobre el cual se construyó la serie de tiempo.

# Extracción de festivos por mes ------------------------------------------
# Años a scrapear.
# TODO: ¿Por qué es necesario tener 36 meses a futuro?
aux <- seq(from = 2013, to = 2025, by = 1) # Se añade algunos datos más por
                                           # la desestacionalización.

# Bucle que itera a través de la página web extrayendo los días festivos
# para cada uno de los años mencionados en el vector creado previamente.
for (i in seq_along(aux)) {
  # Pagina web de la cual se extrae la información:
  web <- read_html(x = paste0(
    'https://www.cuandoenelmundo.com/calendario/colombia/', aux[i]
    )
  )
  
  # Formato de tabla para los datos:
  table <- web %>% 
    html_table(fill = TRUE)
  firstTable  <- table[[13]] # Tablas anteriores no son valiosas.
  secondTable <- table[[14]]
  table <- bind_rows(firstTable, secondTable)
  
  # Conversion de los datos:
  table <- table %>% 
    select(-c('X3')) %>% 
    mutate(Festivos = ymd(paste0(aux[i], '-', X2, '-', X1)))
  
  # Contatenacion de los datos, año a año:
  if (i == 1) {
    definitive <- table %>% 
      select(c('Festivos'))
  } else {
    definitive <- table %>% 
      select(c('Festivos')) %>% 
      bind_rows(definitive)
  }
}

# Al construir una base de datos de fechas festivas, es necesario colapsar por
# año y mes cuántos días festivos se encontraron.
definitive <- definitive %>% 
  arrange(Festivos) %>% 
  mutate(Ano = year(Festivos),
         Mes = month(Festivos)) %>%
  group_by(Ano, Mes) %>% 
  summarise(Festivos = n()) %>% 
  ungroup() %>% 
  mutate(Fecha = ymd(paste0(Ano, '-', Mes, '-01'))) %>% 
  select(c('Fecha', 'Festivos')) 

fechaInicial   <- ymd('2013-01-01') # En este caso dejamos el mes verdadero.
todasLasFechas <- seq.Date(from       = fechaInicial, 
                           by         = 'month', 
                           length.out = numeroDeMeses + 36)
definitive <- left_join(x = tibble(Fecha = todasLasFechas), 
                        y = definitive, 
                        by = 'Fecha') %>% 
  replace_na(list(Festivos = 0))

serieFestivos <- definitive[, 2, drop = TRUE] %>% 
  ts(start = c(2013, 1), frequency = 12) 

# Estimación de valores desestacionalizados -------------------------------
serieSA <- EMC[, 7, drop = TRUE] %>% 
  ts(start = c(2013, 1), frequency = 12) %>% 
  seas(regression.aictest  = 'easter',
       outlier             = NULL,
       transform.function  = 'none',
       regression.usertype = 'holiday',
       xreg                = serieFestivos)

serieSA <- final(serieSA)
plot(serieSA)

# 4| Estimación -----------------------------------------------------------
# 4.1| Red de búsqueda ----------------------------------------------------
# Numericamente, Box-Cox recomendaría una transformación logarítmica. Sin
# embargo, ¡todavía no hemos visto si es necesario estabilizar la varianza!
lambdaEstimation <- BoxCox.lambda(serieSA, lower = 0)

# Se crea una base de datos vacía a llenar con los valores del criterio
# de información para distintas especificaciones de un modelo ARIMA.
# ¡Ojo! Previamente tiene que determinarse si la serie es estacionaria o 
# requiere de diferenciación.
results <- tibble(model = character(),
                  aic   = numeric(),
                  bic   = numeric())
aux <- 0
for(p in 0:4) {
  for(q in 0:4) {
    aux = aux + 1
    try(model <- serieSA %>% 
      Arima(order = c(p, 1, q),
            method = 'CSS-ML'))
    results[aux, 1] = paste0('(', p, ',1,', q, ')')
    results[aux, 2] = model[['aic']]
    results[aux, 3] = model[['bic']]
  }
}

# Extracción del resultado:
results %>% subset(aic == min(results$aic))
results %>% subset(bic == min(results$bic))

# Estimación del mejor modelo:
model <- serieSA %>% 
  arima(order = c(2, 1, 3)) # Empleamos el BIC en este caso.
summary(model)

# ¡Ojo con las raíces unitarias! En caso de tener una, es necesario diferenciar
# para modelar la parte estacionaria.
autoplot(model)

# 4.2| Evaluación de supuestos --------------------------------------------
# Las pruebas se ejecutan sobre los residuales.
residuales <- residuals(model)

# 4.2.1| No-autocorrelación -----------------------------------------------
numeroDeRezagos <- round(x = length(serieSA)/4, digits = 0)

# H0: No-correlación serial.
# HA: Correlación serial.
Box.test(residuales, lag = numeroDeRezagos, type = c('Box-Pierce'))
Box.test(residuales, lag = numeroDeRezagos, type = c('Ljung-Box'))

# 4.2.2| Homocedasticidad -------------------------------------------------
# H0: Homocedasticidad.
# HA: Heterocedasticidad.
arch.test(model, output = TRUE)

# 4.2.3| Normalidad -------------------------------------------------------
# H0: Normalidad.
# HA: No-normalidad.
JarqueBera.test(residuales)

# Chequeo visual de los anteriores resultados:
checkresiduals(model)

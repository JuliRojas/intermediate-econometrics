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
library(tidyverse)
library(stargazer)
library(texreg)
library(extrafont)
loadfonts(quiet = TRUE) # Primero correr font_import().

# Directorios donde se almacena la información del proyecto:
directorioPrincipal         <- enc2native(here())
directorioCodigo            <- paste0(directorioPrincipal, '/code/')
directorioDatosCrudos       <- paste0(directorioPrincipal, '/input/raw/')
directorioDatosLimpios      <- paste0(directorioPrincipal, '/input/cleaned/')
directorioResultadosFiguras <- paste0(directorioPrincipal, '/output/figures/')
directorioResultadosTablas  <- paste0(directorioPrincipal, '/output/tables/')
setwd(directorioPrincipal)

# 1.2| Bases de datos -----------------------------------------------------
carGenerales <- read.csv(file = paste0(directorioDatosCrudos, 
                                       'Área - Características generales (Personas).csv'), 
                         sep = ';')
carOcupados  <- read.csv(file = paste0(directorioDatosCrudos, 
                                       'Área - Ocupados.csv'), 
                         sep = ';')

# 2| Limpieza -------------------------------------------------------------
# 2.1| Creación de los ID -------------------------------------------------
#      Se procede a cambiar los nombres de las variables consideradas como
#      apropiadas para la creación de los identificadores, a saber:
#      - DIRECTORIO: idVivienda
#      - ORDEN: idPersona
#      - SECUENCIA_P: idHogar

# Para la población en general: 
auxId <- c('idVivienda', 'idHogar', 'idPersona')
colnames(carGenerales)[1:3] <- auxId

colnames(carGenerales)[which(colnames(carGenerales) == 'ï..DIRECTORIO')] <- 'idVivienda'
colnames(carGenerales)[which(colnames(carGenerales) == 'ORDEN')] <- 'idPersona'
colnames(carGenerales)[which(colnames(carGenerales) == 'SECUENCIA_P')] <- 'idHogar'

# Para los individuos ocupados:
carOcupados <- carOcupados %>% 
  rename(idVivienda = `ï..DIRECTORIO`, 
         idPersona  = ORDEN,
         idHogar    = SECUENCIA_P)

# Luego, el ID único vendrá dado por:
carGenerales <- carGenerales %>% 
  mutate(ID = paste0(idVivienda, idHogar, idPersona), .before = 1) %>% 
  select(-all_of(auxId))
  
carOcupados <- carOcupados %>% 
  mutate(ID = paste0(idVivienda, idHogar, idPersona), .before = 1) %>% 
  select(-all_of(auxId))

# Se verifica que las llaves sean únicas:
length(carGenerales$ID) == length(unique(carGenerales$ID))
length(carOcupados$ID)  == length(unique(carOcupados$ID))

# 2.2| Unión de las bases de datos ----------------------------------------
#      Se revisa los nombres que son idénticos en ambas bases de datos y se 
#      deja, solamente, los de 'carGenerales', pues es el documento más completo.
duplicados  <- intersect(names(carGenerales), names(carOcupados))
duplicados  <- duplicados[-1]  # Se guarda el ID en ambas bases de datos.
carOcupados <- carOcupados %>% 
  select(-all_of(duplicados))

# Cualquiera de las uniones de los datos listadas a continuación es válida.
datos <- inner_join(x  = carGenerales, 
                    y  = carOcupados,
                    by = 'ID')
# datos <- merge(x  = carGenerales, 
#                y  = carOcupados,
#                by = 'ID')

# 2.3| Selección de las variables -----------------------------------------
#      Finalmente, se selecciona las variables a trabajar, a saber:
#      -   p6800: Horas que trabaja a la semana.
#      -   p7045: Horas trabajadas en una labor secundaria a la semana.
#      - inglabo: Ingresos laborales mensuales.
#      -   p6040: Edad.
#      -     esc: Escolaridad.
#      -   p6020: Sexo.
datos <- datos %>% 
  select(c('P6800', 'P7045', 'INGLABO', 'P6040', 'ESC', 'P6020', 'fex_c_2011')) %>% 
  rowwise() %>% 
  mutate(P6800 = sum(c_across(P6800:P7045), na.rm = TRUE)) %>% 
  mutate(fex_c_2011 = str_replace(string      = fex_c_2011, 
                                  pattern     = ',', 
                                  replacement = '.')) %>% 
  mutate(fex_c_2011 = as.numeric(fex_c_2011)) %>% 
  select(-c('P7045'))

colnames(datos) <- c('Trabajo semanal', 'Ingreso mensual', 'Edad', 
                     'Escolaridad', 'Sexo', 'Factor de expansion')

# Hay dos opciones para trabajar con el sexo, a saber:
# A| Factores: Trabajemos con esta.
datos <- datos %>% 
  mutate(Sexo = case_when(Sexo == 1 ~ 'Hombre', 
                          Sexo == 2 ~ 'Mujer'),
         Sexo = factor(Sexo, levels = c('Hombre', 'Mujer')))

# B| Binarias: 
# datos = datos %>% 
#   mutate(Sexo = case_when(Sexo == 1 ~ 1, 
#                           Sexo == 2 ~ 0))

# Se procede a generar las variables de salario por hora y experiencia potencial.
datos <- datos %>%
  mutate(`Salario por hora` = (`Ingreso mensual`*12)/(`Trabajo semanal`*52),
         `Experiencia Potencial` = Edad - Escolaridad - 5)

# Pueden analizarse un poco los datos, por ejemplo:
max(datos$`Salario por hora`, na.rm = T)
min(datos$`Salario por hora`, na.rm = T)
max(datos$Escolaridad, na.rm = T)
min(datos$Escolaridad, na.rm = T)

# Para no operar con logaritmo valores de cero (0), el logaritmo del salario
# irá con un 'case_when', como se mostraba previamente.
datos <- datos %>% 
  mutate(`Ln(Salario por hora)` = case_when(
    `Ingreso mensual`  > 1000 ~ log(`Salario por hora`), 
    `Ingreso mensual` <= 1000 ~ NA_real_
  ))

datos <- datos %>% 
  drop_na()

# 3| Modelo econométrico --------------------------------------------------
# 3.1| Estimación ---------------------------------------------------------
#      Ya con todas las variables necesarias creadas, se procede a realizar la
#      estimación.
primerRegresion <- lm(`Ln(Salario por hora)` ~ Escolaridad + 
                        `Experiencia Potencial` + I(`Experiencia Potencial`^2), 
                      data = datos, weights = `Factor de expansion`)
summary(primerRegresion)

segundaRegresion <- lm(`Ln(Salario por hora)` ~ Escolaridad + 
                         `Experiencia Potencial` + I(`Experiencia Potencial`^2) +
                         Sexo, data = datos)
summary(segundaRegresion)

# 3.2| Presentación -------------------------------------------------------
#      Un poco de estadística descriptiva podría ser:
stargazer(datos %>% 
            select(c('Trabajo semanal', 'Ingreso mensual', 'Edad', 'Escolaridad', 'Sexo')) %>% 
            as.data.frame(), 
          type = 'text', title = 'Estadística descriptiva', digits = 2)

#      Y la presentación en LaTeX se genera con:
screenreg(l = list(primerRegresion, segundaRegresion))
texreg(l            = list(primerRegresion, segundaRegresion),
       file         = paste0(directorioResultadosTablas, 'regresiones.tex'),
       label        = 'tab:regresiones',
       caption      = 'Comparación entre regresiones con y sin discriminación de sexo.',
       custom.coef.names = c('Intercepto', 'Escolaridad', 'Experiencia potencial',
                             'Experiencia potencial cuadrática'),
       dcolumn      = TRUE, 
       booktabs     = TRUE,
       float.pos    = 'h')

# 4| Visualización de patrones --------------------------------------------
options(scipen = 999) # Sin notación científica.

# 4.1| Gráfica de caja  ---------------------------------------------------
datosGrafica <- datos %>%
  rowid_to_column(var = 'ID')

datosGrafica <- datosGrafica %>% 
  pivot_longer(cols      = !c('ID', 'Sexo'), 
               names_to  = 'Variables', 
               values_to = 'Valores')

datosGrafica <- datosGrafica %>% 
  filter(Variables %in% c('Trabajo semanal', 'Ingreso mensual', 'Edad', 
                          'Escolaridad'))

datosGrafica %>% 
  ggplot(aes(x = factor(1), y = Valores, color = Sexo)) +
  geom_boxplot() + 
  facet_wrap(vars(Variables), scales = 'free') +
  labs(title    = 'Gráfica de caja por variable', 
       subtitle = 'Comparación entre sexos',
       x        = '',
       y        = '',
       caption  = 'Elaboración propia. \n Datos extraídos del DANE.') + 
  theme_classic() +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(family = 'Georgia'),
        axis.text.y     = element_text(family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, family = 'Georgia'),
        legend.text     = element_text(family = 'Georgia'),
        strip.text      = element_text(family = 'Georgia'),
        plot.title      = element_text(face = 'bold', family = 'Georgia'),
        plot.subtitle   = element_text(family = 'Georgia'),
        plot.caption    = element_text(family = 'Georgia'), 
        panel.background      = element_rect(fill = 'transparent'),
        plot.background       = element_rect(fill = 'transparent', color = NA), 
        panel.grid.major      = element_blank(), 
        panel.grid.minor      = element_blank(), 
        legend.background     = element_rect(fill = 'transparent', color = NA), 
        legend.box.background = element_rect(fill = 'transparent', color = NA))

# 4.2| Distribución salarial diferenciada por sexo ------------------------
#      TODO: ¿Cómo se graficaría la función de densidad del salario,
#      diferenciando por sexo?

# 4.3| Impacto de la educación en el salario ------------------------------
datosGrafica <- datos %>%
  mutate(Nada         = if_else(Escolaridad >=  0, 1, 0),
         Primaria     = if_else(Escolaridad >=  5, 1, 0),
         Secundaria   = if_else(Escolaridad >=  8, 1, 0),
         Bachillerato = if_else(Escolaridad >= 11, 1, 0),
         Pregrado     = if_else(Escolaridad >= 16, 1, 0),
         Posgrado     = if_else(Escolaridad >= 18, 1, 0))

datosGrafica <- datosGrafica %>%
  mutate(Logros = case_when(Posgrado     == 1 ~ 'Posgrado',
                            Pregrado     == 1 ~ 'Pregrado',
                            Bachillerato == 1 ~ 'Bachillerato',
                            Secundaria   == 1 ~ 'Secundaria', 
                            Primaria     == 1 ~ 'Primaria',
                            Nada         == 1 ~ 'Nada'),
         Logros = factor(Logros, 
                         levels = c('Posgrado', 'Pregrado', 'Bachillerato', 
                                    'Secundaria', 'Primaria', 'Nada')))

datosGrafica %>% 
  ggplot(aes(x = Escolaridad, y = `Ln(Salario por hora)`, color = Logros,
             alpha = `Factor de expansion`)) + 
  geom_point() + 
  geom_smooth(show.legend = FALSE, method = 'lm', aes(weight = `Factor de expansion`)) +
  labs(title    = 'Relación entre los años de estudio y la remuneración laboral', 
       subtitle = 'Comparación por logros educativos',
       x        = 'Años de educación',
       y        = 'Remuneración recibida por cada hora de trabajo\n(Medida en logaritmo)',
       caption  = 'Fuente: elaboración propia.\nDatos extraídos del DANE.') + 
  theme_classic() +
  scale_x_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05)),
                     labels = scales::label_number(scale = 1, accuracy = 1, big.mark = ',')) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05)),
                     labels = scales::label_number(scale = 1, accuracy = 0.1, big.mark = ',')) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(family = 'Georgia'),
        axis.text.y     = element_text(family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, family = 'Georgia'),
        legend.text     = element_text(family = 'Georgia'),
        strip.text      = element_text(family = 'Georgia'),
        plot.title      = element_text(face = 'bold', family = 'Georgia'),
        plot.subtitle   = element_text(family = 'Georgia'),
        plot.caption    = element_text(family = 'Georgia'), 
        panel.background      = element_rect(fill = 'transparent'),
        plot.background       = element_rect(fill = 'transparent', color = NA), 
        panel.grid.major      = element_blank(), 
        panel.grid.minor      = element_blank(), 
        legend.background     = element_rect(fill = 'transparent', color = NA), 
        legend.box.background = element_rect(fill = 'transparent', color = NA)) +
  guides(color = guide_legend(title = 'Logros educativos'),
         alpha = guide_legend(title = 'Factor de expansión'))

# 4.4| Impacto de la experiencia en el salario ----------------------------
#      TODO: ¿Cómo se graficaría el impacto de la experiencia sobre el salario,
#      diferenciando por el sexo del empleado o empleada?

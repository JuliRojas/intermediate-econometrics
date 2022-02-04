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

# 1| R como calculadora ---------------------------------------------------
# Note que el resultado aparece en la consola.
(90/4)^3                    # Los paréntesis indican orden de operaciones.
24%%7                       # Operación módulo.
pi*100                      # Uso del número pi.
abs(x = -4.8)               # Valor absoluto.
round(x = 6.72, digits = 1) # Redondeo, con 'n' decimales.
trunc(x = 7.888)            # Eliminar decimales.
sqrt(x = 81)                # Raíz cuadrada.
sin(x = pi)                 # Funciones trigonométricas.
log(x = 15, base = exp(1))  # Funciones logarítmicas .
round(log(abs(-97)))        # Operaciones compuestas.

# 2| Creación de objetos/variables ----------------------------------------
# Note que los objetos creados aparecen en el 'Environment' del WorkSpace.
# Para crear objetos se usa el operador '=' o el operador '<-', aunque
# este ultimo es más correcto.
a = 5     # Se crea un objeto llamado 'a' que sea igual a 5.
a         # Se imprime o muestra el objeto creado.
b <- 4; b # Se crea un objeto llamado 'b' que sea igual a 4 y, 
          # a su vez, se imprime.

# Se opera con los objetos creados. Esto es, se crea un nuevo objeto 'c' 
# que sea la multiplicación de 'a' y 'b', y se imprime 'c'.
c = a*b + b^2
c

# 2.1| Tipos de variables -------------------------------------------------
# Se crea un objeto llamado 'Carácteres' y que contenga la palabra 'Manzana'.
caracteres = 'Manzana'
caracteres # Se muestra o imprime Carácteres.

# Crear un objeto llamado 'Booleanos', que sea verdadera.
booleanos = TRUE
booleanos

# 2.2| Comparaciones/Operaciones lógicas ----------------------------------
# El resultado de estas operaciones será verdadero o falso.
a = 4 ; b = 3
a <= 5 # Se prueba que 'a' es mayor o igual a 5.
a == 5 # O que es igual a 5.
a != 5 # O que es diferente a 5.

# Con argumentos booleanos también puede operarse.
a>0 & b>5 # Y (ambos deben ser verdaderos).
a>0 | b>1 # O (uno debe ser verdadero).
! a<0     # Negación.

# 3| Vectores -------------------------------------------------------------
# 3.1| Creación de vectores -----------------------------------------------
# Se crea un vector 'd' con los números 4, 7, 11 y 16 con la función 
# concatenar, c().
d = c(4, 7, 1, 16)
d
e = 0:100 # Se crea un vector 'e' como una secuencia que avanza de uno en uno.
e = seq(from = 0, to = 100, by = 1)
e

# Para más información, introduzca 'help(seq)' o '?seq' en la consola.
f = seq(from = 1, to = 8, by = 0.5) 
f

# Se crea un vector de datos que se repiten un dado número de veces con la 
# función rep().
rep(x = 14, times = 8)  # Vector en el que el 14 se repite 8 veces.
rep(x = 'A', times = 5) # Vector en el que la 'A' se repite 5 veces.
dummy = c(rep(x = 0, times = 10), 
          rep(x = 1, times = 10),
          rep(x = 0, times = 10)) # Una aplicación interesante es...
dummy

# 3.2| Funciones en vectores ----------------------------------------------
h = 6:10 
h+h      
h+10
h*4      
h^2           # Elevar al cuadrado cada una de las entradas de un vector.
sum(x = h)    # Suma de los elementos de un vector.
prod(x = h)   # Producto de los elementos de un vector.
max(x = h)    # Máximo de un vector.
min(x = h)    # Mínimo de un vector.
mean(x = h)   # Media de un vector.
length(x = h) # Número total de elementos del vector.

# Mostrar sólo ciertos elementos de un vector.
# Note que en R la indexación empieza desde el 1.
h[3]          # Se extrae el elemento de la posición 3.
h[length(h)]  # O el de la última posición
h[c(2,3,5)]   # Se muestra los elementos de la posición 2, 3 y 5.
h[-c(1,5)]    # Se eliminó los datos de la posición 1 y 5.

# 3.3| Otras funciones de vectores ----------------------------------------
j <- c(-1, 2, 6, 7, 4, 5, 2, 9, 3, 6, 4, 3); j  # Se crea otro vector.
sort(j)                     # Ordena un vector de menor a mayor.
sort(j, decreasing = TRUE)  # Ordena un vector de mayor a menor.
unique(j)                   # Muestra los datos únicos del vector.

# Vectores lógicos.
j > 0         # Se ve qué elementos de j son positivos.
(k = log(j))  # Se crea un vector k que es el logaritmo natural de j.

# 3.4| Modificar valor(es) de un vector -----------------------------------
k[1]                      # Ver el elmento 1 del vector k.
mean(x = k)               # Hallar la media del vector k
mean(x = k, na.rm = TRUE) # Hallar la media del vector k excluyendo NA's.
k[1] = 99                 # Editar el NA de k
k

# 4| Matrices -------------------------------------------------------------
# 4.1| Crear matrices -----------------------------------------------------
primerMatriz = matrix(data = 1:12, nrow = 3, byrow = T)

# 4.2| Operaciones con matrices -------------------------------------------
primerMatriz + primerMatriz    # Suma de matrices.
primerMatriz + 20              # Sumar una constante.
primerMatriz*0.5               # Multiplicar por constante.
primerMatriz*primerMatriz      # Multiplicación de elementos uno a uno.
primerMatriz%*%t(primerMatriz) # Típica multiplicación de matrices.
t(primerMatriz)                # Transpuesta.

dim(primerMatriz)  # Dimensiones de una matriz
nrow(primerMatriz) # Número de filas de una matriz
ncol(primerMatriz) # Número de columnas de una matriz

# 4.3| Manipulación de matrices -------------------------------------------
primerMatriz[3,4] # Mostrar el elemento de determinada posición.
                  # En este caso, tercera fila, cuarta columna.
primerMatriz[2,]  # Mostrar elementos de una fila
primerMatriz[,3]  # Mostrar elementos de una columna

# Crear matrices a partir de vectores.
k <- c(3, 7, 1, 0, 2, 11)

# Bind traduce 'unir', y r indicaría una unión de vectores filas (row), mientras
# que c permitiría una unión de vectores columna (column).
rbind(k, k^2)    # Une vectores uno debajo de otro.
cbind(k, k^3)    # Une vectores uno al lado de otro.

# A gregar nombres a una matriz.
segundaMatriz <- rbind(1:4,5:8, 9:12)
segundaMatriz
rownames(segundaMatriz) = c('Auto', 'Motocicleta', 'Bici')
colnames(segundaMatriz) = c('Azul', 'Verde', 'Rojo', 'Amarillo')
segundaMatriz

# 5| Muestreo -------------------------------------------------------------
# El imponer una semilla garantiza que los resultados de las simulaciones
# sean replicables.
set.seed(123)

# El muestreo se realiza con reemplazo y esto implica que la probabilidad
# de obtener determinado número no varía en función del turno.
vectorMuestreo = seq(from = 1, to = 100, by = 1)
sample(x = vectorMuestreo, size = 10, replace = TRUE)

# 6| For loop -------------------------------------------------------------
for (i in 1:10) {
  print(paste0('Esta es la iteración número ', i))
}

# 7| Funciones ------------------------------------------------------------
sumarDosNumeros <- function(a, b) {
  resultado <- paste0('El resultado de la suma entre ', 
                      a, ' y ', b, ' es ', a + b, '.')
  return(resultado)
}

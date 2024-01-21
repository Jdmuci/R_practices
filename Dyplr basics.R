
#Ejemplos Dyplr

library(dplyr)

lirios <- iris[c(1:5,51:55,101:105),]
lirios

# SELECCIONAR FILAS: filter()
#Para seleccionar las filas que necesitamos

#Ejemplo 1: selecciona todos los lirios de la especie setosa
filter(lirios, Species=='setosa')

#Ejemplo 2: selecciona los lirios de la especie setosa o virginica
filter(lirios, Species=="setosa"| Species=="virginica")

#Ejemplo 3: selecciona los lirios de la especie setosa cuya longitud de sépalo es inferior a 5 mm
filter(lirios, Species=='setosa', Sepal.Length < 5)



# SELECCIONAR COLUMNAS: select()
# Para seleccionar las variables (clumnas) que necesites

#Ejemplo 1: seleccionar sólo las variables sépalo length y width
select(lirios, Sepal.Length, Sepal.Width)

#Ejemplo 2: seleccionar columanas desde Petal.Length hasta Sepal.Length
select(lirios, Petal.Length:Sepal.Length)

#Ejemplo 3: seleeccionar todas las columnas, excepto Species
select(lirios, -Species)

#Ejemplo 4: seleeccionar columnas cuyo nombre tenga cierto término, ejemplo: "Petal" 
select(lirios, contains("Petal"))
#En lugar de contains, se puede hacer un uso similar con las siguientes expresiones: starts_with, ends_with o matches.



# ORDENAR: arrange()
#Ordena las filas de - a + valor de la variable elegida. Si escribimos un signo menos, ordena de + a -

#Ejemplo 1: ordenar todo el DF de acuerdo con la Longitud Sépalo(de - a +)
arrange(lirios, Sepal.Length)

#Ejemplo 2: ordenar todo el DF de acuerdo con Longitud Sépalo (de + a -)
arrange(lirios, -Sepal.Length)

#Para resolver empates:
#Ejepmplo 3: ordenar conforme la Especie (alfabeticamente) y dentro de la especie, ordenar conforme Longitud Sépalo (de - a +)
arrange(lirios, Species, Sepal.Length)




#SINTAXIS EN CADENA: pipe %>% operator (cmd + shift + M)
#Para utilizar varias funciones al mismo tiempo
#El término a la izq de cada operador %>% es el primer argumento del término de la der

#Ejemplo 1: 
 #(1)seleccionar las variables que contienen las medidas del pétalo, 
 #(2)seleccionar los que la longitud del pétalo es mayor de 4 mm y,
 #(3)ordenarlos de menor a mayor longitud del pétalo

lirios %>%
  select(contains("Petal")) %>%
  filter(Petal.Length > 4) %>%
  arrange(Petal.Length)

#El código anidado habitual de este ejemplo sería:
arrange(filter(select(lirios, contains('Petal')), Petal.Length > 4), Petal.Length)


# Si cargas Dyplr se puede usar la sintaxis en cadena %>% con otros comandos

#Ejemplo 2: calcula la distancia euclídea entre dos vectores
x1 <- 1:6; x2 <- 7:12
sqrt(sum((x1-x2)^2)) #sintaxis anidada
(x1-x2)^2 %>% sum() %>% sqrt() #sintaxis en cadena




# AGREGAR NUEVAS VARIABLES: mutate()

#Ejemplo 1: Crear variable "Forma" que sea el cociente entre anchura y la longitud del pétalo:
lirios %>%
  mutate(Forma = Petal.Width/Petal.Length)




# RESUMIR (SUBCONJUNTOS DE) VARIABLES: group_by() + summarise()
# summarise() para aplicar comandos (estadísticas) a variables en conjunto con group_by() 

#Ejemplo 1: calcula la media de la longitud del pétalo de cada especie
lirios %>% 
  group_by(Species) %>% 
  summarise(mean(Petal.Length))

#Ejemplo 2: calcular las medias de cada medida del pétalo de cada especie
# summarise_each() para varias variables a la vez
lirios %>% 
  group_by(Species) %>% 
  summarise_each(funs(mean), contains("Petal"))

lirios %>% 
  group_by(Species) %>% 
  summarise_each(funs = mean, contains("Petal"))



# OTRAS FUNCIONES

# Eejmplo 1: extraer observaciones aleatoriamente
lirios %>% sample_n(4)

# Ejemplo 2: Extrae un 25% de obs con reemplazamiento
lirios %>% sample_frac(0.25, rep = TRUE)

# Ejemplo 3: Ver estructura
glimpse(lirios)



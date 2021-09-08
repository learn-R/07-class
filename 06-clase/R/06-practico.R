# Practico 6: Análisis descriptivo ----------------------------------------

# 1. Cargar paquetes ------------------------------------------------------
pacman::p_load(sjmisc,
               sjPlot,
               tidyverse,
               magrittr)

# 2. Cargar datos ---------------------------------------------------------
load("output/data/datos_proc.RData")

# 3. Explorar datos -------------------------------------------------------
names(datos_proc)
head(datos_proc)
sjPlot::view_df(datos_proc)                                           

# 4. Descripción de variables -------------------------------------
# Una vez conocidas las variables que incluye nuestros datos procesados, ¿cómo podemos realizar un análisis descriptivo para algún informe o reporte? Veamos algunas de las más comunes

## 4.1. Medidas de tendencia central
# Para conocer las medidas de tendencia central de las variables hay dos opciones. En la **primera** se puede pedir el estadístico manualmente, en la **segunda** se puede pedir una tabla resumen. 

### Media
#Para conocer la media de una variable se utiliza la función `mean()`, su estructura es:
#mean(datos$variable, na.rm=TRUE)
#El argumento `na.rm=TRUE` excluye del cálculo a los casos perdidos. Esto aplicado a nuestra variable `ingreso_percapita` se ve así:
  
mean(datos_proc$ingreso_percapita, na.rm=TRUE)

### Media recortada
# Pero, ¿qué pasa si la variable `ingreso_percapita` esta influenciada por casos influyentes? 
#Para eso puedo pedir la media recortada agregando el argumento `trim` para excluir al 5% de cada extremo

mean(datos_proc$ingreso_percapita, na.rm=TRUE, trim = 0.025)

### Mediana
  
median(datos_proc$ingreso_percapita, na.rm =TRUE)

# Ya tenemos los estadísticos principales, pero ¿cómo los reportamos? ¿tenemos que sacar el promedio de cada variable una por una? 
# **¡No!**, para ello `sjmisc` tiene diferentes funciones, que veremos a continuación:

## Un resumen
#Por ello ocuparemos `descr` de `sjmisc`, esta función nos entrega un resumen de los estadísticos básicos, incluyendo las etiquetas de cada variable

sjmisc::descr(datos_proc$ingreso_percapita,
              show = "all",
              out = "viewer",
              encoding = "UTF-8",
              file = "output/figures/tabla-ingreso.doc")
 

# Como `sjmisc` dialoga con el universo de `tidyverse`, es fácil de complementar con funciones como `select` de `dplyr` y añadir más variables. 
# Además con la función `file` se puede exportar automáticamente la tabla para los reportes.

datos_proc %>%
  select(ingreso_percapita, ife, horas_mens) %>% 
  sjmisc::descr(
    show = "all",
    out = "viewer",
    encoding = "UTF-8",
    file = "output/figures/tabla1.doc")

## 4.3. Frecuencias 
### Frecuencias absolutas
# Para conocer las frecuencias absolutas de una variable se podría usar la función `table`, esta nos arroja la frecuencia por cada categoría de respuesta

table(datos_proc$sexo) 
 

# También podríamos usar la función `flat_table`, esta puede agrupar más variables y agruparlas.

flat_table(datos_proc, sexo, ocupacion, ife)
 

# El problema es ¿cómo podríamos reportarla en nuestros informes? Si queremos una tabla general usaremos la función `frq`. Esta función devuelve una tabla de frecuencias de vectores etiquetados, como marco de datos.

sjmisc::frq(datos_proc$sexo,
            out = "viewer",
            title = "Frecuencias",
            encoding = "UTF-8",
            file = "output/figures/tabla2.doc") 

# 5. Visualización -----------------------------------------

# Ahora que ya sabemos como tener todos los estadísticos necesarios para escribir nuestros reportes, viene el segundo paso *visualizar los estadísticos*. Esto lo haremos con `sjPlot`
# Para visualizar las frecuencias usaremos la función `plot_frq`, su estructura es la siguiente:
  
# plot_frq(datos,  #base
#          ...,          #variable
#          title = "",   # título
#          type = c("bar", "dot", "histogram", "line", "density", "boxplot", "violin") #tipo de gráfico a especificar 
         
          
#1. Gráfico de barras de frecuencias simple

plot_frq(datos_proc, edad_tramo,
         title = "Gráfico de frecuencias, barras",
         type = c("bar"))


save_plot("/output/img/tab.png", fig = last_plot())

#2. Gráfico de puntos
#Si tenemos más categorías y queremos mejorar el reporte, podemos usar este código:
  
plot_frq(datos_proc, edad_tramo,
         title = "Gráfico de frecuencias, puntos",
         type = c("dot"))

#También podemos cambiar el orden del eje x e y

plot_frq(datos_proc$edad_tramo, type = "dot", show.ci = TRUE, sort.frq = "desc",
         coord.flip = TRUE, expand.grid = TRUE, vjust = "bottom", hjust = "left", title = "Gráfico de frecuencias, puntos cambiado")

#3. Histogramas        

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>% 
  plot_frq(., ingreso_percapita,
           title = "Histograma",
           type = c("histogram"))

# 4. Densidad

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
  plot_frq(., ingreso_percapita,
           title = "Gráfico de densidad",
           type = c("density"))

# 5. Gráfico de líneas

plot_frq(datos_proc, ife,
         title = "Gráfico de líneas",
         type = c("line"))

# 6. Gráfico de cajas

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
  plot_frq(., ingreso_percapita,
           title = "Gráfico de caja",
           type = c("boxplot"))

#7. Gráfico de violín

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
  plot_frq(., ingreso_percapita,
           title = "Gráfico de violín",
           type = c("violin"))

# Como pueden ver, el único argumento que se modificaba era `type = `, es decir, para hacer diversos gráficos, sólo se debe especificar el tipo de gráfico que queremos.

#8. Gráfico de nube de puntos

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
  plot_scatter(., horas_mens, ingreso_percapita)

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
  plot_scatter(., horas_mens, ingreso_percapita, ocupacion)

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
  plot_scatter(., horas_mens, ingreso_percapita, sexo)

datos_proc %>%  filter(ingreso_percapita <= 2000000 & horas_mens <= 450) %>%
  plot_scatter(., horas_mens, ingreso_percapita, sexo,
               fit.grps = "lm", grid = TRUE)

# Ahora que ya hemos graficado las frecuencias de las variables, vamos a graficar frecuencias 
# agrupadas, para ello usaremos la función `plot_grpfrq` de `sjPlot`, su estructura es la siguiente

# plot_grpfrq(
#   var.cnt,
#   var.grp,
#   type = c("bar", "dot", "line", "boxplot", "violin")
  
  
#1. Gráfico de barras
  
plot_grpfrq(datos_proc$sexo, datos_proc$ocupacion,
            type = c("bar"), title = "Gráfico de barras")


# Podemos ver que no solo nos muestra la frecuencia absoluta, sino que también la relativa en porcentaje
# Pero además podemos ver agregar una tercera categoría, que es el total de ambas categorías. Para este ejercicio conoceremos que tramo de edad trabajo la semana pasada.
# Para este ejercicio usaremos la función `plot_xtab`, de la misma librería

plot_xtab(datos_proc$edad_tramo, datos_proc$ocupacion, title = "Gráfico de barras")

# 1.1 Gráfico de barras horizontales

plot_xtab(datos_proc$edad_tramo, datos_proc$ocupacion, margin = "row", 
          bar.pos = "stack",
          title = "Gráfico de barras horizontales",
          show.summary = TRUE, coord.flip = TRUE)

datos_proc %>% select("ocupacion","o2", "o3", "o4", "o6") %>% 
  plot_stackfrq(., title = "Gráfico de barras proporcional")

#1.2 Escalas likert

datos_proc %>% select("ocupacion","o2", "o3", "o4", "o6") %>% 
  sjPlot::plot_likert(., title = "Gráfico de escalas likert")

#1.3 Tablas de proporción cruzadas

plot_gpt(datos_proc, ife, ocupacion, sexo,
         shapes = c(15, 21), 
         title = "Gráfico de proporción agrupada")

#2. Gráfico de puntos

plot_grpfrq(datos_proc$sexo, datos_proc$ocupacion,
            title = "Gráfico de puntos",
            type = c("dot"))

#3. Gráfico de líneas

plot_grpfrq(datos_proc$edad_tramo, datos_proc$ife,
            title = "Gráfico de línea",
            type = c("line"))

plot_grpfrq(datos_proc$edad_tramo, datos_proc$ocupacion, 
            title = "Gráfico de línea",
            type = "line")

# 4. Gráfico de cajas

plot_grpfrq(datos_proc$horas_mens, datos_proc$edad_tramo,
            title = "Gráfico de caja",
            type = c("boxplot"))

# Además, se puede incorporar una tercera variable, en este caso lo haremos con la variable `sexo` y el argumento `intr.var`

plot_grpfrq(datos_proc$horas_mens, datos_proc$edad_tramo, intr.var = datos_proc$sexo, 
            title = "Gráfico de cajas",
            type = "box")

# 5. Gráfico de violín

plot_grpfrq(datos_proc$horas_mens, datos_proc$edad_tramo,
            title = "Gráfico de violín",
            type = c("violin"))

# Nuevamente, la función nos permite la creación de múltiples gráficos, sólo se debe cambiar el argumento `type =`

# 6. Tablas de contingencia ---------------------------------------------------

sjt.xtab(datos_proc$sexo, datos_proc$ife,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8", 
         title = "Tabla de contingencia",
         file = "output/figures/tabla3.doc")


# Si además se quieren añadir variables con las mismas categorías de respuesta, se puede utilizar la función 
#`tab_stackfrq`, para ello utilizaremos a las variables añadidas para este práctico `o3`, `o4` y `o6`. 
#Le añadiremos etiquetas con `value.labels` y le pediremos que nos entregue la frecuencia absoluta (`show.n = TRUE`) y el total (`show.total = T`)

tab_stackfrq(as.data.frame(datos_proc %>% select("o3", "o4", "o6")),
             value.labels=c('1'='Si', '2'='No'),
             show.n = TRUE, show.total = T,
             file = "output/figures/tabla4.doc")

# 7. Test de independencia Chi2 -----------------------------------------------------------

# A la hora de graficar Chi2, debemos asegurarnos con la función `as.factor` que las variables a utilizar sean de tipo factor, 
# luego utilizamos la función `sjp.chi2` de `sjPlot` para crear la tabla, finalmente con el argumento `axis.labels` asignamos la etiqueta de cada variable

data.frame(as.factor(sample(datos_proc$ocupacion, replace = TRUE)),
           as.factor(sample(datos_proc$o2, replace = TRUE)),
           as.factor(sample(datos_proc$o3, replace = TRUE)),
           as.factor(sample(datos_proc$o4, replace = TRUE)),
           as.factor(sample(datos_proc$o6, replace = TRUE))) %>% 
  sjp.chi2(., 
           title = "Gráfico de Chi2",
           axis.labels  = c("Trabajó al menos una hora", "realizó alguna actividad", "Tenía algún empleo", "Ha trabajado alguna vez", "Busco empleo"))

# 8. Correlación --------------------------------------------

# Previamente debemos seleccionar las variables a utilizar, ya que no tiene sentido incluir en el análisis variables nominales

datos_proc %>%
  select(ingreso_percapita, horas_mens) %>% 
  tab_corr(.,
           triangle = "lower",   
           title = "Tabla de correlación",
           encoding = "UTF-8", 
           file = "output/figures/tabla4.doc")



# 9. Anova ----------------------------------------------------
# Finalmente, si queremos reportar un análisis de Anova, no podemos dejar de lado este gráfico que nos otorga la función `sjp.aov1` del paquete `sjPlot`

sjp.aov1(datos_proc$ingreso_percapita, datos_proc$sexo, title = "Anova")





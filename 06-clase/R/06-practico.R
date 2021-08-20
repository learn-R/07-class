# Practico 4: Análisis descriptivo ----------------------------------------


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


# 4. Descripción de variables


## 4.1. Medidas de tendencia central

### Media

mean(datos_proc$ingreso_percapita, na.rm=TRUE)

### Media recortada

mean(datos_proc$ingreso_percapita, na.rm=TRUE, trim = 0.025)

### Mediana
  
median(datos_proc$ingreso_percapita, na.rm =TRUE) # Ahora ya sabemos que al menos un 50% de las familias en Chile tienen por ingreso `$229.184`.

# Ya tenemos los estadísticos principales, pero ¿cómo los reportamos? ¿tenemos que sacar el promedio de cada variable una por una? **¡No!**, para ello `sjmisc` tiene diferentes funciones, que veremos a continuación

## Un resumen

#Podemos obtener un resumen de todos estadísticos a partir de la función summary(). El argumento puede ser tanto una columna en particular como `ingreso_percapita` 

summary(datos_proc$ingreso_percapita)


# También podemos hacerlo con todos los datos. Sólo que no tendrá mucho sentido para las variables nominales.


summary(datos_proc)

# summary() es una función muy potente, dado que no solo permite resúmenes de data.frames (datos), sino que también de otros objetos en R (como los modelos).

# Ahora bien tiene limitantes para interactuar con `dplyr` y generar archivos de salida. Por ello ocuparemos `descr` de `sjmisc`


sjmisc::descr(datos_proc$ingreso_percapita,
              show = "all",
              out = "viewer",
              encoding = "UTF-8",
              file = "output/figures/tabla-ingreso.doc")

# De más variables


datos_proc %>%
  select(ingreso_percapita, ife, horas_mens) %>% 
  sjmisc::descr(
    show = "all",
    out = "viewer",
    encoding = "UTF-8",
    file = "output/figures/tabla1.doc")


# Acá podemos ver la variable `var`, el tipo de variable `type`, su etiqueta `label`, los casos válidos `n`, los casos perdidos `NA.prc`, las medidas de tendencia central y las de dispersión. `sjmisc` tiene muchos beneficios, ya que cómo interactúa con el mundo `tidyverse` es fácil de complementar con funciones como `select` de `dplyr`. Además con la función `file` se puede exportar automáticamente la tabla para los reportes.


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

# 5. Visualización

# Para visualizar las frecuencias usaremos la función `plot_frq`, su estructura es la siguiente:
  
# 1. Gráfico de barras de frecuencias simple
         
plot_frq(datos_proc, edad_tramo,
        title = "Gráfico de frecuencias, barras",
        type = c("bar"))

save_plot(last_plot("/output/img/tab.png"))

#2. Gráfico de puntos
         
plot_frq(datos_proc, edad_tramo,
                  title = "Gráfico de frecuencias, puntos",
                  type = c("dot"))

#También podemos cambiar el orden del eje x e y
         
plot_frq(datos_proc$edad_tramo, type = "dot", show.ci = TRUE, sort.frq = "desc",
         coord.flip = TRUE, expand.grid = TRUE, vjust = "bottom", hjust = "left", title = "Gráfico de frecuencias, puntos cambiado"
         )

#3. Histogramas        
         
datos_proc %>%  filter(ingreso_percapita <= 2000000) %>% 
           plot_frq(., ingreso_percapita,
                    title = "Histograma",
                    type = c("histogram"))

#4. Densidad
         
datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
           plot_frq(., ingreso_percapita,
                    title = "Gráfico de densidad",
                    type = c("density"))

#5. Gráfico de líneas
         
plot_frq(datos_proc, ife,
                  title = "Gráfico de líneas",
                  type = c("line"))
         
#6. Gráfico de cajas

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
           plot_frq(., ingreso_percapita,
                    title = "Gráfico de caja",
                    type = c("boxplot"))

# 7. Gráfico de violín
         
datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
           plot_frq(., ingreso_percapita,
                    title = "Gráfico de violín",
                    type = c("violin"))

# 8. Gráfico de nube de puntos
         
datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
           plot_scatter(., sexo, ingreso_percapita)

#Además, se puede agregar líneas ajustadas para cada grupo.
         
datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
           plot_scatter(., sexo, ingreso_percapita, jitter = .4)

#También es posible agregar una variable de ocupación al diagrama de dispersión.
         
datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
           plot_scatter(., sexo, ingreso_percapita, ocupacion, jitter = .4)

#Pero ¿si quiero una mejor visualización? para eso usamos este código que nos permite hacer dos gráficos para cada categoría de sexo

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
           plot_scatter(., ingreso_percapita, ocupacion, sexo,
                        fit.grps = "loess", grid = TRUE)

datos_proc %>%  filter(ingreso_percapita <= 2000000) %>%
           plot_scatter(., ingreso_percapita, edad_tramo, sexo,
                        fit.grps = "loess", grid = TRUE)

#Ahora que ya hemos graficado las frecuencias de las variables, vamos a graficar frecuencias agrupadas, para ello usaremos la función `plot:grpfrq` de `sjPlot`, su estructura es la siguiente
         
#1. Gráfico de barras
           

plot_grpfrq(datos_proc$sexo, datos_proc$ocupacion,
                       type = c("bar"), title = "Gráfico de barras")

# además podemos ver agregar una tercera categoría, que es el total de ambas categorías. Para este ejercicio conoceremos que tramo de edad trabajo la semana pasada.
           
plot_xtab(datos_proc$edad_tramo, datos_proc$ocupacion, title = "Gráfico de barras")
          
# 1.1 Gráfico de barras horizontales
           

plot_xtab(datos_proc$edad_tramo, datos_proc$ocupacion, margin = "row", 
                     bar.pos = "stack",
                     title = "Gráfico de barras horizontales",
                     show.summary = TRUE, coord.flip = TRUE)

# 2. Gráfico de puntos
           
plot_grpfrq(datos_proc$sexo, datos_proc$ocupacion,
                       title = "Gráfico de puntos",
                       type = c("dot"))
           
# 3. Gráfico de líneas

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

# Además, se puede incorporar una tercera variable, en este caso lo haremos con la variable `sexo`
           
plot_grpfrq(datos_proc$horas_mens, datos_proc$edad_tramo, intr.var = datos_proc$sexo, 
                       title = "Gráfico de cajas",
                       type = "box")

# 5. Gráfico de violín
           
plot_grpfrq(datos_proc$horas_mens, datos_proc$edad_tramo,
                       title = "Gráfico de violín",
                       type = c("violin"))
 # 6. Tablas de contingencia
           
 sjt.xtab(datos_proc$sexo, datos_proc$ife,  title = "Tabla de contingencias",
                    show.col.prc=TRUE,
                    show.summary=FALSE)

# ¿Qué pasó? ¿por qué salen esos símbolos raros en la tabla? 
             
# ¡Es por la codificación!, para ello le agregamos el argumento `encoding = "UTF-8"` y ya tenemos nuestra tabla de frecuencias cruzadas
           
sjt.xtab(datos_proc$sexo, datos_proc$ife,
                    show.col.prc=TRUE,
                    show.summary=FALSE, 
                    encoding = "UTF-8", 
                    title = "Tabla de contingencia",
                    file = "output/figures/tabla3.doc")

# 7. Correlación
           
 datos_proc %>%
             select(ingreso_percapita, horas_mens) %>% 
             tab_corr(.,
                      triangle = "lower",   
                      title = "Tabla de correlación",
                      encoding = "UTF-8", 
                      file = "output/figures/tabla4.doc")

# 8. Anova
           
sjp.aov1(datos_proc$ingreso_percapita, datos_proc$sexo, title = "Anova")

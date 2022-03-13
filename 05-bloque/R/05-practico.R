
# 5: Descriptivos univariados ---------------------------------------------


# 1. Cargar librerías -----------------------------------------------------


pacman::p_load(sjmisc,
               sjPlot,
               tidyverse,
               magrittr)


# 2. Cargar datos ---------------------------------------------------------

datos_proc <- readRDS("input/data/datos_proc.rds")


# 3. Explorar datos -------------------------------------------------------

names(datos_proc)
head(datos_proc)
sjPlot::view_df(datos_proc,
                encoding = "UTF-8")


# 4. Descriptivos ---------------------------------------------------------


## 4.1 Medidas de tendencia central ----------------------------------------


### a) Media -------------------------------------------------------------

mean(datos_proc$ing_pc, na.rm=TRUE)

#### Recortada

mean(datos_proc$ing_pc, na.rm=TRUE, trim = 0.025)

### b) Mediana -----------------------------------------------------------

median(datos_proc$ing_pc, na.rm =TRUE)

### c) Estadísticos resumen de variables cuantitativas con sjmisc::descr() ----------------------------------

sjmisc::descr(datos_proc$ing_pc)

#### Interactuando con dplyr::select()

datos_proc %>% 
  select(ing_pc, ytoth, tot_per) %>% 
  sjmisc::descr()


## 4.2 Análisis de frecuencias ----------------------------------------------


### a) table() para tablas de frecuencia absoluta ---------------------------------

table(datos_proc$sexo) 

### b) flat_table() para tablas de contingencia ------------------------------------

flat_table(datos_proc, sexo, ocupacion, ife)

### c) Frecuencias para variables categóricas con sjmisc::frq() -------------------------------------

sjmisc::frq(datos_proc$sexo,
            out = "viewer",
            title = "Frecuencias",
            encoding = "UTF-8")


# 5. Gráficos univariados -------------------------------------------------


## a) Gráfico de barras ---------------------------------------------------

plot_frq(datos_proc, sexo_sexo_edad_tramo,
         title = "Gráfico de frecuencias, barras",
         type = "bar")


### Exportar gráfico --------------------------------------------------------

save_plot("output/figures/tab.png", fig = last_plot())


## b) Gráfico de puntos ----------------------------------------------------

plot_frq(datos_proc, sexo,
         title = "Gráfico de frecuencias, puntos",
         type = "dot")

### Personalizando

plot_frq(datos_proc$sexo_edad_tramo, type = "dot", show.ci = TRUE, sort.frq = "desc",
         coord.flip = TRUE, expand.grid = TRUE, vjust = "bottom", hjust = "left", 
         title = "Gráfico de frecuencias, puntos cambiado")


## c) Histograma  ---------------------------------------------------------------------

datos_proc %>%  filter(ing_pc <= 2000000) %>% 
  plot_frq(., ing_pc,
           title = "Histograma",
           type = "histogram")


## d) Gráfico de densidad -------------------------------------------------------------

datos_proc %>%  filter(ing_pc <= 2000000) %>%
  plot_frq(., ing_pc,
           title = "Gráfico de densidad",
           type = "density")



## e) Gráfico de líneas ---------------------------------------------------------------

plot_frq(datos_proc, tot_per,
         title = "Gráfico de líneas",
         type = "line")


## f) Gráfico de cajas -----------------------------------------------------

datos_proc %>%  filter(ing_pc <= 2000000) %>%
  plot_frq(., ing_pc,
           title = "Gráfico de caja",
           type = "boxplot")


## g) Gráfico de violín ----------------------------------------------------


datos_proc %>%  filter(ing_pc <= 2000000) %>%
  plot_frq(., ing_pc,
           title = "Gráfico de violín",
           type = "violin")

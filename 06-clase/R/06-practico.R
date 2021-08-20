# Practico 4: Analisis descriptivo ----------------------------------------

# Te toca a ti: ingresa los comentarios explicativos
# Eso permitirá ir reforzando conceptos

# 1. Cargar paquetes ------------------------------------------------------
pacman::p_load(sjmisc,
               sjPlot)

# 2. Cargar datos ---------------------------------------------------------
load("output/data/datos_proc.RData")
# 3. Explorar datos -------------------------------------------------------
names(datos_proc)
head(datos_proc)
sjPlot::view_df(datos_proc)




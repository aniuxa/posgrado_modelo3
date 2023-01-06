# =============================================================================-
# Fecha: 2022-01-10 
# Paquetes a utilizar
# Autora: Ana Escoto
# =============================================================================-

# Introducción: `{dplyr}` y `{ggplot2}` ----

## Paquetes ----

if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere
pacman::p_load(tidyverse,
               readxl,
               writexl, 
               haven,
               sjlabelled, 
               janitor,
               infer, 
               ggpubr,
               magrittr,
               gt)

## Cargando los datos ----

# Desde STATA 

tlaxt322<- haven::read_dta("./datos/tlaxt322.dta")


# Desde Excel:
  
  
ICI_2021 <- readxl::read_excel("./datos/ICI_2021.xlsx",
                               sheet = "para_importar")



## Un poquito de `{dplyr}` y limpieza ----

### Primero, los pipes ----
# 
# R utiliza dos pipes el nativo `|>` y el pipe que está en `{dplyr}` `%>%`. 
# Algunas de las diferencias las puedes checar acá 
# <https://eliocamp.github.io/codigo-r/2021/05/r-pipa-nativa/>
  
# En estas prácticas utilizaremos el segundo, pero son muy parecidos y para que esta instructora recicle algunos de sus códigos viejos. Pero funcionan igual:
  
  
tlaxt322|> #pipe nativo, no necesita instalación
  head()



tlaxt322 %>%  #pipe de dplyr, necesita instalación de dplyr en tidyverse
  head()


### Limpieza de nombres con `{janitor}` ---
# 
# Este paso también nos permitirá enseñar otro *pipe* que está en el paquete `{magrittr}`.
# 
# Los nombres de una base de datos son los nombres de las columnas.

names(tlaxt322)
names(ICI_2021)

# Como vemos en las bases hay mayúsculas, caracteres especiales y demás. Esto lo podemos cambiar


ICI_2021<-ICI_2021 %>% 
  janitor::clean_names()

names(ICI_2021)


# Si quisiéramos que la acción quedará de un solo, podemos usar un pipe diferente:
  
  
tlaxt322%<>% 
  clean_names()

names(tlaxt322)


# Más de otros *pipes* <https://r4ds.had.co.nz/pipes.html>

## `select()` y `filter()` ----
  
#   Este es un recordatorio de que en `{dplyr}`:
#   se filtran CASOS, es decir, líneas o renglones
#   y se seleccionan VARIABLES.
# 
# Por ejemplo:
  
  
tlaxt322%>% 
  dplyr::select(sex, eda) %>% 
  dplyr::filter(eda>11)


# En la documentación de la base de datos de la ENOE se nos señala que debemos quedarnos con quienes tienen entrevista completa `r_def==0` y con quiénes son habitante habituales `(c_res!=2")`
# 
# Hagamos estos cambios:
  
  
tlaxt322%<>%
  filter(r_def==0) %>% 
  filter(!c_res==2)


## Tabulados con `tabyl()`

# El comando tabyl del paquete `{janitor}` nos sirve para hacer tabulados. Para que sean más bonitas,
# necesitaremos cambiar algunas de nuestras variables a sus datos etiquetados


tlaxt322%>%
  dplyr::mutate(sex=sjlabelled::as_label(sex))  %>%
  janitor::tabyl(sex)

# 
# Para ver que esto es una distribución de frecuencias sería muy útil ver la proporción total, 
# ello se realiza agregando un elemento más en nuestro código con una "tubería":
#   
  
tlaxt322%>%
  mutate(sex=as_label(sex)) %>% 
  tabyl(sex) %>%
  adorn_totals() #primer enchulamiento


# Ahora, las proporciones son raras, y preferimos por los porcentajes.


tlaxt322%>%
  mutate(sex=as_label(sex)) %>% # cambia los valores de la variable a sus etiquetas
  tabyl(sex) %>% # para hacer la tabla
  adorn_totals() %>% # añade totales
  adorn_pct_formatting()  # nos da porcentaje en lugar de proporción



# Vamos a darle una "ojeada" a esta variable


glimpse(tlaxt322$niv_ins)


# Hoy hacemos la tabla, con las etiquetas:
  
  
tlaxt322%>%
  mutate(niv_ins=as_label(niv_ins)) %>% #esto sólo si hay etiquetas declaradas, recuerda
  tabyl(niv_ins)


# Para que no nos salgan las categorías sin datos podemos poner una opción dentro del comando "tabyl()"


tlaxt322%>% 
  mutate(niv_ins=as_label(niv_ins)) %>% 
  tabyl(niv_ins, 
        show_missing_levels=F ) %>% # esta opción elimina los valores con 0
  adorn_totals()  


### Cálculo de frecuencias ----

  
tlaxt322%>% 
  mutate(niv_ins=as_label(niv_ins)) %>% # para que las lea como factor
  mutate(sex=as_label(sex)) %>% # para que las lea como factor
  tabyl(niv_ins, sex, show_missing_levels=F ) %>% # incluimos aquí
  adorn_totals()  


# Observamos que en cada celda confluyen los casos que comparten las mismas características:
  
  
tlaxt322%>%   
  count(niv_ins==1 & sex==1) # nos da la segunda celda de la izquierda



### Totales y porcentajes ----

# De esta manera se colocan todos los datos. Si observa al poner la función "adorn_totals()" lo agregó como una nueva fila de totales, pero también podemos pedirle que agregue una columna de totales.


tlaxt322%>% 
  mutate(niv_ins=as_label(niv_ins)) %>% # para que las lea como factor
  mutate(sex=as_label(sex)) %>% # para que las lea como factor
  tabyl(niv_ins, sex, show_missing_levels=F ) %>% # incluimos aquí sex
  adorn_totals("col")  


  
tlaxt322%>% 
  mutate(niv_ins=as_label(niv_ins)) %>% # para que las lea como factor
  mutate(sex=as_label(sex)) %>% # para que las lea como factor
  tabyl(niv_ins, sex, show_missing_levels=F ) %>% # incluimos aquí sexo
  adorn_totals(c("col", "row")) 

  
  
tlaxt322%>% 
  mutate(niv_ins=as_label(niv_ins)) %>% # para que las lea como factor
  mutate(sex=as_label(sex)) %>% # para que las lea como factor
  tabyl(niv_ins, sex, show_missing_levels=F ) %>% # incluimos aquí sexo
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("col") %>% # Divide los valores entre el total de la columna
  adorn_pct_formatting() # lo vuelve porcentaje


tlaxt322%>% 
  mutate(niv_ins=as_label(niv_ins)) %>% # para que las lea como factor
  mutate(sex=as_label(sex)) %>% # para que las lea como factor
  tabyl(niv_ins, sex, show_missing_levels=F ) %>% 
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("row") %>% # Divide los valores entre el total de la fila
  adorn_pct_formatting() # lo vuelve porcentaje




tlaxt322%>% 
  mutate(niv_ins=as_label(niv_ins)) %>% # para que las lea como factor
  mutate(sex=as_label(sex)) %>% # para que las lea como factor
  tabyl(niv_ins, sex, show_missing_levels=F ) %>% # incluimos aquí sexo
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("all") %>% # Divide los valores entre el total de la población
  adorn_pct_formatting() # lo vuelve porcentaje


## *Grammar of tables: gt* ----



mi_tabla<-tlaxt322%>% 
  mutate(niv_ins=as_label(niv_ins)) %>% # para que las lea como factor
  mutate(sex=as_label(sex)) %>% # para que las lea como factor
  tabyl(niv_ins, sex, show_missing_levels=F ) %>% # incluimos aquí sexo
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("all") %>% # Divide los valores entre el total de la población
  adorn_pct_formatting() # lo vuelve porcentaje




gt_tabla<-gt(mi_tabla)
gt_tabla


# Con este formato será bastante sencillo agregar títulos y demás:
  
  
gt_tabla<-gt_tabla %>%
  tab_header(
    title = "Distribución del sexo de la población según nivel de escolaridad",
    subtitle = "Tlaxcala, trimestre III de 2022"
  )

gt_tabla


Agreguemos la fuente a nuestra tabla:
  
  
gt_tabla<-gt_tabla %>%
  tab_source_note(
    source_note = "Fuente: Cálculos propios con datos de INEGI"
  )

gt_tabla


# Checa más de este paquete por aquí <https://gt.rstudio.com/articles/intro-creating-gt-tables.html>

## Descriptivos para variables cuantitativas ----
  
  # Vamos a empezar a revisar los gráficos para variables cuantitativas.

### Medidas numéricas básicas ----

# 5 números


summary(tlaxt322$ing_x_hrs) ## ingreso por horas 


# Con pipes se pueden crear "indicadores" de nuestras variables es un tibble


tlaxt322%>% 
  summarise(nombre_indicador=mean(ing_x_hrs, na.rm=T))


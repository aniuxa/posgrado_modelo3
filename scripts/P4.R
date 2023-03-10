# =============================================================================-
# Fecha: 2022-01-10 
# Introducción a la inferencia (II)
# Autora: Ana Escoto
# =============================================================================-


## Paquetes

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
               gt,
               GGally,
               broom,
               DescTools,
               wesanderson,
               gtsummary,
               srvyr,
               car)


## Cargando los datos

Desde STATA


tlaxt322 <- read_dta("./datos/tlaxt322.dta") %>% 
  clean_names() %>% 
  filter(r_def==0) %>% 
  filter(!c_res==2)



Desde Excel:
  
  
ICI_2021 <- read_excel("./datos/ICI_2021.xlsx",
                       sheet = "para_importar") %>% 
  clean_names()



## Factores de expansión y diseño muestral

### La función tally
# 
# El comando `tabyl()` del paquete `{janitor}` es muy útil pero no es compatible con 
# los factores del expansión. En realidad, `tabyl()` nos ahorra un poco el hecho 
# de tener que agrupar nuestra base en categorías y luego hacer un conteo para cada una de ellas.
# `tally()` es un comando que nos hace ese conteo y `group_by()`
# nos agrupa las observaciones de nuestra base de datos para hacer cualquier operación.


tlaxt322  %>% 
  group_by(as_label(sex)) %>% 
  tally(fac_tri) %>% #nombre del factor
  adorn_totals()  # Agrega total


# Podemos usar funciones de `adorn_...` de `{janitor}`


tlaxt322  %>% 
  group_by(as_label(sex)) %>% 
  tally(fac_tri) %>% #nombre del factor
  adorn_totals() %>% # Agrega total
  adorn_percentages("all")  %>% 
  adorn_pct_formatting()


### Otras formas

# La función `count()` también permite dar pesos


tlaxt322  %>% 
  dplyr::count(sex, niv_ins,  wt = fac_tri) 



# Es compatible con etiquetas


tlaxt322  %>% 
  count(as_label(sex), as_label(niv_ins),  wt = fac_tri) 


# Podemos mover un poquito con pivot_wider para que se vea más a lo que acostumbramos a una tabla de frecuencias


tlaxt322  %>% 
  mutate_at(vars(sex, niv_ins), as_label) %>% 
  count(sex, niv_ins,  wt = fac_tri) %>% 
  tidyr::pivot_wider(names_from = sex, 
                     values_from = n)



tlaxt322  %>% 
  mutate_at(vars(sex, niv_ins), as_label) %>% # otra forma de mutate y as_label
  count(sex, niv_ins,  wt = fac_tri) %>% 
  pivot_wider(names_from = sex, 
              values_from = n) %>%
  adorn_totals() %>% # Agrega total
  adorn_percentages("col")  %>% 
  adorn_pct_formatting()


### Diseño complejo

# Hay muchos diseños muestrales, asumiremos el diseño simple, pero hay que revisar la documentación de la base


# Muestreo aleatorio
tlax_srvy <- tlaxt322  %>%
  as_survey_design(weights = fac_tri)



# Si revisamos las encuestas tiene un diseño complejo, hay estratos y unidades primarias de muestreo



# Muestreo estratificado
tlax_srvy <- tlaxt322  %>%
  as_survey_design(
    upm,
    strata = est_d_tri,
    weights = fac_tri,
    nest = TRUE)



# Como vemos esto es un archivo bien grande, por lo que mejor vamos a seleccionar un par de variables:
  
  
# simple random sample
tlax_srvy <- tlaxt322  %>%
  select(upm,est_d_tri, fac_tri, starts_with("clase"),
         sex, eda, anios_esc, ing_x_hrs, fac_tri) %>% 
  as_survey_design(
    upm,
    strata = est_d_tri,
    weights = fac_tri,
    nest = TRUE)



# Para una media ponderada


tlax_srvy %>%
  filter(eda>14 & eda<99) %>% #filtro de edad para tabulados
  filter(clase2==1) %>% # sólo ocupados
  filter(ing_x_hrs>0) %>% # sólo con ingresos
  summarise(
    media_ponderada = survey_mean(ing_x_hrs, na.rm=T))


# 
# Este valor coincide con los datos publicados por INEGI,
# incluso el error estándar. Si queremos los intervalos de confianza:
  
  
tlax_srvy %>%
  filter(eda>14 & eda<99) %>% #filtro de edad para tabulados
  filter(clase2==1) %>% # sólo ocupados
  filter(ing_x_hrs>0) %>% # sólo con ingresos
  summarize(
    media_ponderada = survey_mean(ing_x_hrs,
                                  vartype = "ci") )




tlax_srvy %>%
  filter(eda>14 & eda<99) %>% #filtro de edad para tabulados
  filter(clase2==1) %>% # sólo ocupados
  filter(ing_x_hrs>0) %>% # sólo con ingresos
  summarize(
    mediana_ponderada = survey_median(ing_x_hrs,
                                      vartype = "ci") )




tlax_srvy %>%
  mutate(sex=as_label(sex)) %>% 
  group_by(sex) %>% #variables cuali
  summarize(proportion = survey_mean(), # proporción
            total = survey_total() ) # totales




## Estimación de varianzas y sus pruebas de hipótesis

# Para poder hacer inferencia sobre la varianza utilizamos el comando `varTest()`
# del paquete `{DescTools}`


tlaxt322%>% 
  filter(clase2==1) %>% 
  with(
    DescTools::VarTest(ing_x_hrs)
  )



# Podemos también decir algo sobre el valor objetivo de nuestra hipótesis


tlaxt322%>% 
  filter(clase2==1) %>% 
  with(
    VarTest(ing_x_hrs, sigma.squared = 100)
  )



$$ H_o:\sigma=100 $$
  
  
## Análisis de varianza
  
# Análisis de varianza. Haremos la versión más simple.
# Para ver el efecto de un factor sobre una variable cualitativa (oneway). 
# Revisaremos si la región de residencia de los trabajadores tiene un efecto 
# en la distribución de los ingresos por trabajo.

### Primero un gráfico

# la ANOVA se basa en que nuestra variable es normal. Quitaremos los outliers


lienzo_bi <-tlaxt322  %>% 
  filter(clase2==1  & !ing_x_hrs==0) %>% 
  ggplot(aes(x=log(ing_x_hrs), fill=as_factor(t_loc_tri), 
             color=as_factor(t_loc_tri),
             alpha=I(0.5)))

lienzo_bi + geom_density()


# La prueba ANOVA o análisis de varianza, nos dice cuánto de nuestra variable se ve explicado por un factor.


anova<-tlaxt322  %>% 
  filter(clase2==1) %>% 
  with(aov(ing_x_hrs ~ as_factor(t_loc_tri)))

summary(anova)


# Con tidy:
  
  
tidy(anova)


### Comparación entre grupos


  
TukeyHSD(anova)


### Supuestos de ANOVA

# -   Las observaciones se obtienen de forma independiente y aleatoria de la población definida por los niveles del factor
# -   Los datos de cada nivel de factor se distribuyen normalmente.
# -   Estas poblaciones normales tienen una varianza común.


#Prueba Bartlett para ver si las varianzas son iguales

tlaxt322  %>% 
  filter(clase2==1) %>% 
  with(bartlett.test(ing_x_hrs ~ as_factor(t_loc_tri)))


# La prueba tiene una Ho "Las varianzas son iguales"


# Test Normalidad 
tlaxt322  %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    ks.test(ing_x_hrs, "pnorm", mean=mean(ing_x_hrs), sd=sd(ing_x_hrs))
  )


# La prueba tiene una Ho "La variable es normal"

# **¿Qué hacer?**
  
  ### Kruskal-Wallis test
  
  # Hay una prueba muy parecida que se basa en el orden de las observaciones, y se lee muy parecida a la ANOVA


kruskal<-tlaxt322  %>% 
  filter(clase2==1) %>% 
  with(
    kruskal.test(ing_x_hrs ~ as_factor(t_loc_tri))
  )

kruskal


# Para ver las comparaciones tenemos que usar el `DunnTest()`, del paquete `{DescTools}`



tlaxt322  %>% 
  filter(clase2==1) %>% 
  with(
    DescTools::DunnTest(ing_x_hrs ~ as_factor(t_loc_tri))
  )



#### Un gráfico coqueto:

# Se hace con ggpubr



tlaxt322  %>% 
  filter(clase2==1) %>% 
  ggpubr::ggviolin(x = "t_loc_tri", y = "ing_x_hrs", fill = "t_loc_tri",
                   add = "boxplot", add.params = list(fill = "white")) +
  stat_compare_means(label.y = 600)  # Add the p-value 

comparaciones <- list( c("1", "2"), c("2", "3"), c("1", "3"),
                       c("1", "4"), c("2", "4"), c("3", "4") )



#Un gráfiquito accesorio 2:

tlaxt322  %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  ggpubr::ggviolin(x = "t_loc_tri", y = "ing_x_hrs", fill = "t_loc_tri",
                   palette = wesanderson::wes_palette("GrandBudapest1", 4, type="discrete"),
                   add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = comparaciones, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 500)     # Add global the p-value 




## Introducción a la regresión lineal 


tlaxt322 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  ggplot() +
  aes(x=anios_esc, y=log(ing_x_hrs)) + 
  geom_point()



# Cuando tenemos muchos casos es útil la opción "jitter"



tlaxt322 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  ggplot() +
  aes(x=anios_esc, y=log(ing_x_hrs)) + 
  geom_jitter()


# También cambiar un poquito la transparencia...


tlaxt322 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  ggplot() +
  aes(x=anios_esc, y=log(ing_x_hrs), alpha=I(0.5)) + 
  geom_jitter()



# ¿Cómo se ve la línea MCO ajustada por estos elementos?
  
  
tlaxt322 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  ggplot() +
  aes(x=anios_esc, y=log(ing_x_hrs), alpha=I(0.5)) + 
  geom_jitter()+
  geom_smooth(method = lm)


# ¿cómo se ajusta esa línea?
  
  
  
model<-tlaxt322 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  with(
    lm(log(ing_x_hrs)~ anios_esc)
  )

model


# Guardarlo en un objeto sirve de mucho porque le podemos "preguntar" cosas

summary(model) # da todo menos la anova de la regresión
confint(model) # da los intervalos de confianza
anova(model) # esto sí da la anova de la regresión.


# Para ver esto más guapo:
  
  
model %>%
  gtsummary::tbl_regression() 
#%>% 
# add_significance_stars() %>% 
# add_n() %>% 
# add_glance_table()


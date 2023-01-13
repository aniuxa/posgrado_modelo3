# =============================================================================-
# Fecha: 2022-01-12 
# Práctica 3: Introducción a la inferencia
# Autora: Ana Escoto
# =============================================================================-

# Paquetes ----

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



# Datos ----

tlaxt322 <- read_dta("./datos/tlaxt322.dta") %>% 
  clean_names() %>% 
  filter(r_def==0) %>% 
  filter(!c_res==2)


ICI_2021 <- read_excel("./datos/ICI_2021.xlsx",
                       sheet = "para_importar") %>% 
  clean_names()


# Intervalos de confianza y prueba de hipótesis ----

t.test(tlaxt322$ing_x_hrs)

tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  with(
   t.test(ing_x_hrs) 
  )


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, # esta es mi variable
           conf.level = 0.99) # establezco mi nivel de confianza
  )

test0<-tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, # esta es mi variable
           conf.level = 0.99) # establezco mi nivel de confianza
  )

test0$conf.int # intervalo de confianza
test0$statistic # valor del estadístico de prueba
test0$parameter # grados de libertad
test0$estimate # x-barra
test0$stderr # error estándar


# t.test para pruebas de hipótesis

tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, # esta es mi variable
           mu=40) # valor objetivo 
      )


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, # esta es mi variable
           mu=40, # valor objetivo de la H0
           alternative = "less") # modifico el tipo de prueba de hipótesis
  )


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, # esta es mi variable
           mu=40, # valor objetivo de la H0
           alternative = "greater") # modifico el tipo de prueba de hipótesis
  )


# broom: un paquete para volver tidy cualquier elemento

broom::tidy(test0)

# con el paquete infer, el comando t_test nos permite
# tener resultados en formato tidy y sin utilizar with()


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  infer::t_test(response = ing_x_hrs)


# Una proporción ----

tlaxt322 %>% 
  mutate(clase1=as_label(clase1)) %>% 
  tabyl(clase1)

tlaxt322 %>% 
  filter(clase1>0) %>% 
  mutate(clase1=as_label(clase1)) %>% 
  tabyl(clase1)

prop.test(x = 5374,
          n = 5374 + 3827)


#prop.test(table(tlaxt322$clase1)) # con formato base

tlaxt322 %>% 
  filter(clase1>0) %>% 
  with(
    table(clase1)
  ) %>% 
  prop.test()

# comando infer

tlaxt322 %>% 
  filter(clase1 > 0) %>% 
  mutate(clase1 = as_label(clase1) ) %>% 
  infer::prop_test(response = clase1)


get_labels(tlaxt322$clase1)


tlaxt322 %>% 
  filter(clase1 > 0) %>% 
  mutate(clase1 = as_label(clase1) ) %>% 
  infer::prop_test(response = clase1,
                   success = "Población no económicamente activa",
                   z = TRUE)


# Vamos a modificar el valor objetivo para la prueba de hipótesis

# p = 0.7 dentro de la pea a nivel nacional 

tlaxt322 %>% 
  filter(clase1 > 0) %>% 
  mutate(clase1 = as_label(clase1) ) %>% 
  infer::prop_test(response = clase1,
                   z = TRUE, # para que me dé resultados con Z y no con chi
                   p = 0.7) # establezco mi valor objetivo de la H0

# H0: p = 0.7
# alternativa = desigualdad
# Ha: p ≠ 0.7

# ¿Cómo reetiqueto una variable?"

etiqueta_pea<-c("PEA", "PNEA") # etiqueta para dos categorías

tlaxt322 %>% 
  filter(clase1>0) %>% 
  sjlabelled::set_labels(clase1, labels = etiqueta_pea) %>% 
  select(clase1)

tlaxt322 %>% 
  filter(clase1>0) %>% 
  sjlabelled::set_labels(clase1, labels = etiqueta_pea) %>% 
  mutate(clase1=as_label(clase1)) %>% 
  tabyl(clase1)


# Diferencias de medias ----

tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  group_by(sex) %>% 
  summarise(avg_hrs=mean(ing_x_hrs))


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs ~ sex)
  )


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs ~ sex,
           alternative = "greater")
  )


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  mutate(sex=as_label(sex)) %>% 
  infer::t_test(response = ing_x_hrs,
                explanatory = sex)


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  mutate(sex=as_label(sex)) %>% 
  infer::t_test(response = ing_x_hrs,
                explanatory = sex,
                order = c("Mujer","Hombre"))



# Diferencia de proporciones ----

tlaxt322 %>% 
  filter(clase1>0) %>% 
  set_labels(clase1, labels=etiqueta_pea) %>% 
  mutate(clase1=as_label(clase1)) %>% # respuesta en formato factor
  mutate(sex=as_label(sex)) %>% # explicativa en formato factor
  infer::prop_test(response = clase1 ,
                   explanatory = sex,
                   success = "PEA",
                   z = TRUE,
                   order = c("Mujer", "Hombre"))

# ¿Cómo puedo escribir una prueba de hipótesis que compruebe
# que las mujeres tienen mayor participación dentro de la PNEA 
# que los varones?


# Ha: p_pnea_muj> p_pnea_var
# H0: p_pnea_muj<= p_pnea_var

tlaxt322 %>% 
  filter(clase1 > 0) %>% 
  set_labels(clase1, labels = etiqueta_pea) %>% 
  mutate(clase1 = as_label(clase1)) %>% #respuesta en formato factor
  mutate(sex = as_label(sex)) %>% #explicativa en formato factor
  infer::prop_test(response = clase1,
                   explanatory = sex,
                   alternative = "greater",
                   success = "PNEA",
                   z = T,
                   order = c("Mujer", "Hombre"))

# =============================================================================-
# Fecha: 2022-01-16 
# Introducción a los modelos de regresión lineal
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
               car,# Companion of Applied Regression
               sjPlot,
               performance, see,
               jtools, #ojo
               sandwich, 
               huxtable)


# Datos ----

tlaxt322 <- read_dta("./datos/tlaxt322.dta") %>% 
  clean_names() %>% 
  filter(r_def==0) %>% 
  filter(!c_res==2) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(clase2==1) %>% 
  filter(anios_esc<99) %>% 
  mutate(log_ing_x_hrs=log(ing_x_hrs))

# Relación entre ingresos y años de escolaridad

tlaxt322 %>% 
  ggplot() +
  aes(x=anios_esc, 
      y=log_ing_x_hrs) + 
  geom_point()


tlaxt322 %>% 
  ggplot() +
  aes(x=anios_esc, 
      y=log_ing_x_hrs) + 
  geom_jitter()



tlaxt322 %>% 
  ggplot() +
  aes(x=anios_esc, 
      y=log_ing_x_hrs,
      alpha=I(0.3)) + 
  geom_jitter() +
  geom_smooth(method = lm)


# Correlación y su prueba de hipótesis ----

cor(y=tlaxt322$log_ing_x_hrs,
    x=tlaxt322$anios_esc)

tlaxt322 %>% 
  with(
    cor(log_ing_x_hrs,
        anios_esc)
  )

cor.test(y=tlaxt322$log_ing_x_hrs,
         x=tlaxt322$anios_esc)

# Una correlación puede ser de magnitud pequeña
# o media y ser estadísticamente significativa
# Porque la prueba de hipótesis nos dice
# que cero no es un valor un posible en la población

# con pipes

tlaxt322 %>% 
  with(
    cor.test(anios_esc, log_ing_x_hrs)
  )

cor_test<-tlaxt322 %>% 
  with(
    cor.test(anios_esc, log_ing_x_hrs)
  )

# H0: rho = 0 "No hay relación lineal"
# Ha: rho ≠ 0 "Hay relación lineal"
broom::tidy(cor_test)

# Ajustar la línea recta ----

tlaxt322 %>% 
  with(
    lm(log_ing_x_hrs ~ anios_esc)
  )

modelo<-tlaxt322 %>% 
  with(
    lm(log_ing_x_hrs ~ anios_esc)
  )

summary(modelo)

confint(modelo)

# ajuste global

anova(modelo)

broom::tidy(modelo) # ajuste individual por coeficiente
broom::glance(modelo) #ajuste global

modelo %>% 
  gtsummary::tbl_regression() %>% 
  gtsummary::add_significance_stars() %>% 
  gtsummary::add_n() %>% 
  gtsummary::add_glance_table()

# Diagnósticos ----

# Los ejemplos gráficos para revisar supuestos
plot(modelo)
performance::check_model(modelo)

## Pruebas de hipótesis

# Identificación de valores atípicos

car::outlierTest(modelo)

# Varianza no constante
car::ncvTest(modelo)
# H0: Los errores tienen varianza constante
# Ha: Los errores no tienen varianza constante.

performance::check_heteroscedasticity(modelo)
performance::check_normality(modelo)

# Regresión lineal múltiple ----
## Una variable categórica ----

tlaxt322 %>% 
  ggplot()+
  aes(x=anios_esc,
      y=log_ing_x_hrs,
      alpha=I(0.3),
      color=as_label(sex))+ 
  geom_jitter()+
  geom_smooth(method=lm) 


modelo1<-tlaxt322 %>% 
  mutate(sex=as_label(sex)) %>% 
  with(
    lm(log_ing_x_hrs ~ anios_esc + sex)
  )

summary(modelo1)
pruebaf0<-anova(modelo, modelo1) # comparar modelos en el ajuste global

performance::compare_performance(modelo, modelo1)
performance::test_performance(modelo, modelo1)

## Incluyendo una variable cuantitativa

modelo2 <-tlaxt322 %>% 
  mutate(sex=as_label(sex)) %>% 
  with(
    lm(log_ing_x_hrs ~ anios_esc + sex + eda)
  )

summary(modelo2)
anova(modelo1, modelo2)

performance::test_performance(modelo, modelo1, modelo2)

## Las otras pruebas se hacen igual

car::vif(modelo2)>5 # tenemos un problema de multicolinealidad

## Paquete jtools 

jtools::summ(modelo2, digits=5)
jtools::summ(modelo2, robust="HC3", digits=5)

jtools::summ(modelo2, scale = T, vifs=T) # para comparar diferentes variables
# con diferentes unidades de medida, 
# se lee por cada desviación estándar que se aumente en x...

# Veamos gráficos y tablas para presentar ----

jtools::export_summs(modelo, modelo1, modelo2)

sjPlot::plot_model(modelo2, scale = T)

sjPlot::plot_models(modelo, modelo1, modelo2)
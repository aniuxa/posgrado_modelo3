# Práctica 2

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
               wesanderson)

# Datos ----

tlaxt322 <- read_dta("./datos/tlaxt322.dta") %>% 
  clean_names()


ICI_2021 <- read_excel("./datos/ICI_2021.xlsx",
                       sheet = "para_importar") %>% 
  clean_names()


tlaxt322 %<>%
  filter(r_def==0) %>% 
  filter(!c_res==2)


# Grammar of tables ----

# primero vamos a hacer una tabla con tabyl()

mi_tabla<-tlaxt322 %>% 
  filter(eda>4) %>% 
  mutate(niv_ins=as_label(niv_ins)) %>% 
  mutate(sex=as_label(sex)) %>% 
  tabyl(niv_ins, sex, show_missing_levels = FALSE) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=2)

writexl::write_xlsx(mi_tabla, path = "mi_tabla1.xlsx")

mi_tabla %>% 
  clean_names()


gt_tabla<- gt::gt(mi_tabla)

gt_tabla<-gt_tabla %>%
  tab_header(
    title = "Distribución del sexo de la población según nivel de escolaridad",
    subtitle = "Tlaxcala, trimestre III de 2022"
  )

gt_tabla

class(gt_tabla)
class(mi_tabla) 
class(tlaxt322)
class(tlaxt322$sex)

rm(gt_tabla)
remove(mi_tabla)

# Análisis descriptivo de variables cuantitativas ----

summary(tlaxt322$ing_x_hrs)
summary(as_label(tlaxt322$sex))
summary(tlaxt322$sex)

# Summary con filtros

tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    summary(ing_x_hrs)
  )

# tidy y summarise()

tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  summarise(indicador1=mean(ing_x_hrs),
            indicador2=sd(ing_x_hrs),
            indicador3=mean(anios_esc))



tlaxt322 %>% 
  filter(ing_x_hrs>0) %>%
  group_by(sex) %>% 
  summarise(indicador1=mean(ing_x_hrs),
            indicador2=sd(ing_x_hrs),
            indicador3=mean(anios_esc))


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>%
  group_by(sex, t_loc_tri) %>% 
  summarise(indicador1=mean(ing_x_hrs),
            indicador2=sd(ing_x_hrs),
            indicador3=mean(anios_esc))

# Visualización de datos ----


plot(as_label(tlaxt322$niv_ins))

plot(tlaxt322$niv_ins)


barplot(table(as_label(tlaxt322$niv_ins)))

hist(ICI_2021$esperanza_de_vida)
boxplot(ICI_2021$esperanza_de_vida)

box<-boxplot(ICI_2021$esperanza_de_vida)

box$stats
box$group

# Gráficos con ggplot2 ----

## Para variable cualitativa ----

tlaxt322 %>% 
  ggplot() +
  aes(y=as_label(niv_ins)) +
  geom_bar()


ggplot(data=tlaxt322) +
  aes(y=as_label(niv_ins)) +
  geom_bar()



tlaxt322 %>%
  filter(eda>4) %>% 
  ggplot() +
  aes(y=as_label(niv_ins)) +
  geom_bar()



tlaxt322 %>%
  filter(eda>4) %>% 
  ggplot() +
  aes(y=as_label(niv_ins), fill=as_label(niv_ins)) +
  geom_bar()



tlaxt322 %>%
  filter(eda>4) %>% 
  ggplot() +
  aes(y=as_label(niv_ins)) +
  geom_bar(fill="blue")



tlaxt322 %>%
  filter(eda>4) %>% 
  ggplot() +
  aes(y=as_label(niv_ins), fill=as_label(sex)) +
  geom_bar()

## Para variable cuantitativa ----

tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  ggplot() +
  aes(x=ing_x_hrs) +
  geom_histogram(bins=32)

tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  ggplot() +
  aes(x=ing_x_hrs) +
  geom_density()


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  ggplot() +
  aes(x=log(ing_x_hrs)) + # transformé a logaritmo mi variable
  geom_density()



# Gráficos de dos variables cuantitativas

tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  ggplot()+
  aes(x=anios_esc,
      y=ing_x_hrs)+
  geom_point()
  


tlaxt322 %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  select(eda, ing_x_hrs, anios_esc) %>% 
  GGally::ggpairs()

########### BASE DE DATOS > EDUCACION > Base de Datos por Escuela 2024 > 2024 Base usuaria 5 - Caracteristicas del establecimiento #########
library(tidyverse)
library(readxl)
library(scales)     

caracteristicas <- read_excel("2024 Caracteristicas - agregada.xlsx")
caracteristicas

##  Armado de la tabla
porcentaje_laboratorios <- caracteristicas |> 
  group_by(ambito, sector) |> 
  summarize(
    Total_escuelas = sum(Localizacion, na.rm = TRUE),
    Labs_si = sum(DisponedesalaolaboratoriodeinformáticaSi, na.rm = TRUE),
    Porcentaje_labs_si = Labs_si / Total_escuelas) |> 
  mutate(
    Labs_no = Total_escuelas - Labs_si,
    Porcentaje_labs_no = Labs_no / Total_escuelas)
porcentaje_laboratorios

## Transformar la tabla a formato largo
datos_grafico <- porcentaje_laboratorios |> 
  pivot_longer(
    cols = c(Porcentaje_labs_si, Porcentaje_labs_no),
    names_to = "Laboratorio",
    values_to = "Porcentaje") |> 
  mutate(
    ## Leyenda
    Laboratorio = if_else(Laboratorio == "Porcentaje_labs_si", "Sí", "No"),
    ## Generar las 4 columnas para que aparezcan en el eje x
    Categoria = paste(ambito, sector, sep = "\n"))

## Armado del grafico
ggplot(datos_grafico, aes(x = Categoria, y = Porcentaje, fill = Laboratorio)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = percent(Porcentaje, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold") +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Ámbito y Sector",
    y = "Proporción (%)",
    fill = "Laboratorio",
    title = "Disponibilidad de laboratorio de informática por zona")
ggsave("Disponibilidad_de_laboratorio_de_informatica_por_zona.png")






















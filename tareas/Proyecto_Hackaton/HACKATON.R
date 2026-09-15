########### BASE DE DATOS > EDUCACION > Base de Datos por Escuela 2024 > 2024 Base usuaria 5 - Caracteristicas del establecimiento #########

library(readxl)
library(tidyverse)
library(scales)

# Cargar la base de características
caracteristicas <- read_excel("2024 Caracteristicas - agregada.xlsx")
caracteristicas

## Armado de la tabla
porcentaje_laboratorios <- caracteristicas |> 
  group_by(ambito, sector) |> 
  summarize(
    Total_escuelas = sum(Localizacion, na.rm = TRUE),
    Labs_si = sum(DisponedesalaolaboratoriodeinformáticaSi, na.rm = TRUE),
    Porcentaje_labs_si = Labs_si / Total_escuelas,
    Labs_no = Total_escuelas - Labs_si,
    Porcentaje_labs_no = Labs_no / Total_escuelas
  )
porcentaje_laboratorios

## Transformar la tabla a formato largo
datos_grafico <- porcentaje_laboratorios |> 
  pivot_longer(
    cols = c(Porcentaje_labs_si, Porcentaje_labs_no),
    names_to = "Laboratorio",
    values_to = "Porcentaje"
  ) |> 
  mutate(
    ## Leyenda
    Laboratorio = if_else(Laboratorio == "Porcentaje_labs_si", "Sí", "No"),
    ## Generar las 4 columnas para que aparezcan en el eje x
    Categoria = paste(ambito, sector, sep = "\n")
  )

## Armado del grafico
grafico_cobertura <- ggplot(datos_grafico, aes(x = Categoria, y = Porcentaje, fill = Laboratorio)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = percent(Porcentaje, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold"
  ) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Ámbito y Sector",
    y = "Proporción (%)",
    fill = "Laboratorio",
    title = "Disponibilidad de laboratorio de informática por zona")

print(grafico_cobertura)
#ggsave("Disponibilidad_de_laboratorio_de_informatica_por_zona.png", grafico_cobertura)


########### Cruce con Trayectoria Educativa y Eficiencia de Egreso

# Cargar la planilla de trayectoria
trayectoria <- read_excel("2024 Trayectoria - agregada.xlsx")

# Verificar que se cargaron bien las dimensiones
dim(caracteristicas)  
dim(trayectoria)     

# Cruzar los datasets
dataset_unido <- trayectoria %>%
  left_join(
    caracteristicas, 
    by = c("provincia", "Departamento", "sector", "ambito")
  )

# Confirmar las dimensiones del dataset cruzado
dim(dataset_unido)
glimpse(dataset_unido)

# Limpieza y cálculo de la métrica de salida instantánea
df_analisis <- dataset_unido |>  
  filter(
    !is.na(secundaria_egresados), 
    !is.na(ultimo_12), 
    ultimo_12 > 0,
    # Filtramos las celdas vacías para que no entren como "sin lab"
    !is.na(DisponedesalaolaboratoriodeinformáticaSi)
  ) |>  
  mutate(
    ratio_salida = (secundaria_egresados / ultimo_12) * 100,
    sector = factor(sector),
    ambito = factor(ambito),
    
    # mayor a 0 es que tiene, 0 es que no tiene 
    tiene_lab = if_else(
      DisponedesalaolaboratoriodeinformáticaSi > 0,
      "Con Lab. IT",
      "Sin Lab. IT"
    )
  ) |>  
  filter(ratio_salida <= 100)

colnames(trayectoria)
colnames(caracteristicas)

# Resumen estadístico simple agrupado
resumen_stats <- df_analisis |> 
  group_by(sector, tiene_lab) |> 
  summarise(
    promedio_salida = mean(ratio_salida, na.rm = TRUE),
    total_escuelas  = n(),
    .groups = "drop"
  )

print(resumen_stats)

# Visualización limpia y directa para la presentación (Boxplot)
grafico_final <- ggplot(df_analisis, aes(x = tiene_lab, y = ratio_salida, fill = tiene_lab)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ sector) +
  scale_fill_manual(values = c("Con Lab. IT" = "#2b83ba", "Sin Lab. IT" = "#d7191c")) +
  labs(
    title = "Infraestructura Informática y Eficiencia de Egreso Secundario",
    subtitle = "Ratio de Salida Instantáneo (Egresados / Matrícula Último Año)",
    x = "Dispone de Laboratorio de Informática",
    y = "Ratio de Salida (%)",
    caption = "Fuente: Elaboración propia en base a datos de Educación (2024)"
  ) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14, face = "bold"))

print(grafico_final)
#ggsave("grafico-join.png", grafico_final)





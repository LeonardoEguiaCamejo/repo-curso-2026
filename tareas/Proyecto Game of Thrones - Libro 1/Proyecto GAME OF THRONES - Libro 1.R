
# Game of Thrones - Libro 1 -----------------------------------------------

library(readr)     
library(tidyverse) 
library(tidygraph) 
library(ggraph)  
library(igraph)

path <- "C:/Users/Hp/OneDrive/Documents/Curso-E520/Game of Thrones/asoiaf-master/asoiaf-master/data"
files <- list.files(path = path, full.names = TRUE)
files


nodos <- read.csv(file.path(path, "asoiaf-book1-nodes.csv"))
vertices <- read.csv(file.path(path, "asoiaf-book1-edges.csv"))
graph <- graph_from_data_frame(d = vertices, vertices = nodos, directed = FALSE)

vcount(graph)
ecount(graph)

graph <- as_tbl_graph(graph) %>%
  mutate(
    degree = centrality_degree(weights = weight),
    betweenness = centrality_betweenness(weights = weight),
    eigenvector = centrality_eigen(weights = weight)
  )

graph %>%
  as_tibble() %>%
  arrange(desc(degree)) %>%
  select(name, Label, degree, betweenness, eigenvector) %>%
  head(10)


# Personaje mas relevante -------------------------------------------------


main_ch <- vertices %>%
  select(-Type) %>%
  gather(x, name, Source:Target) %>%
  group_by(name) %>%
  summarise(sum_weight = sum(weight)) %>%
  ungroup()

# Filtrar y ordenar los personajes más importantes del Libro 1
main_ch_l <- main_ch %>%
  arrange(desc(sum_weight)) %>%
  top_n(10, sum_weight) # Cambiamos a Top 10 para ver los principales rápidamente

main_ch_l

# cuantas conexiones tiene cada persona? ----------------------------------

# Duplicar las filas para contar tanto las apariciones en 'Source' como en 'Target'
conexiones_por_persona <- vertices %>%
  gather(key = "posicion", value = "personaje", Source:Target) %>%
  group_by(personaje) %>%
  summarise(
    n_conexiones = n(),               # Cantidad de personas distintas con las que interactúa
    peso_total = sum(weight)           # Suma total de interacciones (peso)
  ) %>%
  arrange(desc(n_conexiones))          # Ordenar de mayor a menor número de conexiones

head(conexiones_por_persona, 15)

# Clustering --------------------------------------------------------------

transitivity(graph, type = "global")

# Generar modelos nulos/aleatorios (ajustados a NO dirigidos)
# Modelo Erdős–Rényi: conserva la cantidad de nodos y vértices
red_er <- sample_gnm(n = vcount(graph), m = ecount(graph), directed = FALSE)

# Modelo Configuracional: conserva la secuencia exacta de grados de los nodos
red_conf <- sample_degseq(
  out.deg = degree(graph),
  method  = "vl"
)

# Función de métricas adaptada a redes no dirigidas
metricas <- function(x) {
  c(
    clustering    = transitivity(x, type = "global"),
    asortatividad = assortativity_degree(x, directed = FALSE),
    dist_promedio = mean_distance(x, directed = FALSE)
  )
}

# Tabla comparativa
round(rbind(
  Real              = metricas(graph),
  "Erdős–Rényi"     = metricas(red_er),
  Configuracional   = metricas(red_conf)
), 3)

# cantidad de clusterings

graph_clusters <- as_tbl_graph(graph) %>%
  mutate(
    cluster = as.factor(group_louvain()), 
    degree = centrality_degree()
  )

# Cuántos clusters existen y cuántos personajes tiene cada uno:
graph_clusters %>% 
  as_tibble() %>% 
  count(cluster, name = "cantidad_personajes")


# Visualizacion -----------------------------------------------------------

# Calcular puentes entre distintos clusters
graph_foto <- graph_clusters %>%
  activate(edges) %>%
  mutate(
    cluster_from = .N()$cluster[from],
    cluster_to = .N()$cluster[to],
    es_puente = cluster_from != cluster_to
  )

# Generar el gráfico de la imagen
ggraph(graph_foto, layout = "fr") + 
  geom_edge_link(aes(color = es_puente, width = weight), alpha = 0.5) + 
  scale_edge_color_manual(values = c("TRUE" = "tomato", "FALSE" = "gray80")) + 
  geom_node_point(aes(color = cluster), size = 3) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 2.5, check_overlap = TRUE) + 
  theme_void() + 
  labs(
    title = "Clusters y Puentes en Game of Thrones",
    subtitle = "Líneas rojas = Puentes entre diferentes facciones/clusters",
    color = "Cluster",
    edge_color = "¿Es puente?"
  )

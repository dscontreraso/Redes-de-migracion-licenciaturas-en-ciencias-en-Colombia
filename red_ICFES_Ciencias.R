install.packages("plotly")
install.packages("leaflet.extras")
library(igraph)
library(ggplot2)
library(dplyr)
library(tidygraph)
library(plotly)
library(ggrepel)
library(leaflet)
library(leaflet.extras)


# Cargar los datos de la base de datos "red"
red <- read_excel("C:/Users/DCO20/Desktop/Observatorio UPN/Octavo informe/red.xlsx", 
                  sheet = "redmigra")

# Cargar los datos de la base de datos con coordenadas
coordenadas <- read_excel("C:/Users/DCO20/Desktop/Observatorio UPN/Octavo informe/red.xlsx", 
                          sheet = "coordenadas")

# Depurar filas con valores iguales en ESTU_COD_COLE_MCPIO_TERMINO y ESTU_PRGM_CODMUNICIPIO
red_clean <- red %>%
  distinct(ESTU_COD_COLE_MCPIO_TERMINO, ESTU_PRGM_CODMUNICIPIO, .keep_all = TRUE)


# Obtener una lista única de nodos
nodes <- unique(c(red$ESTU_COD_COLE_MCPIO_TERMINO, red$ESTU_PRGM_CODMUNICIPIO))

# Crear un dataframe con los nodos y sus coordenadas
nodes_df <- data.frame(id = nodes)
nodes_df <- left_join(nodes_df, coordenadas, by = c("id" = "codigo"))

# Crear un grafo dirigido
g <- graph_from_data_frame(red, directed = TRUE, vertices = nodes_df)

# Calcular el grado de los nodos
node_degree <- degree(g, mode = "all", normalized = TRUE)

# Agregar el grado al dataframe de nodos
nodes_df$node_degree <- node_degree

# Convertir el grafo de igraph a tidygraph
tg <- as_tbl_graph(g)

# Crear un dataframe con las coordenadas de inicio y final de las aristas
edges_coords <- red %>%
  inner_join(coordenadas, by = c("ESTU_COD_COLE_MCPIO_TERMINO" = "codigo")) %>%
  rename(x_start = LONGITUD, y_start = LATITUD) %>%
  inner_join(coordenadas, by = c("ESTU_PRGM_CODMUNICIPIO" = "codigo")) %>%
  rename(x_end = LONGITUD, y_end = LATITUD)


# Calcular el número de aristas entrantes para cada nodo
incoming_edges <- edges_coords %>%
  group_by(x_end) %>%
  summarize(num_incoming_edges = n())

# Agregar el número de aristas entrantes al dataframe de nodos
nodes_df <- left_join(nodes_df, incoming_edges, by = c("id" = "x_end"))

# Crear el mapa con leaflet
map <- leaflet(data = nodes_df) %>%
  setView(lng = mean(coordenadas$LONGITUD), lat = mean(coordenadas$LATITUD), zoom = 10) %>%
  addTiles()  # Agregar el fondo del mapa

# Función para asignar colores según la licenciatura
get_color <- function(licenciatura) {
  if (licenciatura == "LICENCIATURA EN FISICA") {
    return("blue")
  } else if (licenciatura == "LICENCIATURA EN QUIMICA") {
    return("red")
  } else if (licenciatura == "LICENCIATURA EN CIENCIAS NATURALES Y EDUCACION AMBIENTAL") {
    return("green")
  } else if (licenciatura == "LICENCIATURA EN BIOLOGIA") {
    return("orange")
  } else {
    return("grey")
  }
}

# Agregar las aristas al mapa con colores de acuerdo a la columna "licenciatura"
for (i in 1:nrow(edges_coords)) {
  color <- get_color(edges_coords$licenciatura[i])
  
  map <- map %>%
    addPolylines(lng = c(edges_coords$x_start[i], edges_coords$x_end[i]),
                 lat = c(edges_coords$y_start[i], edges_coords$y_end[i]),
                 color = color, opacity = 0.5, group = "Polylines")
}

# Agregar los nodos al mapa con valores numéricos en node_degree
for (i in 1:nrow(nodes_df)) {
  radius <- ifelse(is.na(nodes_df$num_incoming_edges[i]), 0, nodes_df$num_incoming_edges[i] * 2)
  color <- ifelse(is.na(nodes_df$num_incoming_edges[i]), "black", "grey")
  
  map <- map %>%
    addCircleMarkers(lng = nodes_df$LONGITUD[i], lat = nodes_df$LATITUD[i],
                     radius = radius, color = color,
                     fillOpacity = 0.8, label = as.character(nodes_df$id[i]), group = "Nodes")
}

# Agregar controles de capas
map <- map %>% 
  addLayersControl(
    baseGroups = c("OpenStreetMap"),
    overlayGroups = c("Polylines", "Nodes"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Agregar la leyenda en la esquina inferior derecha
map <- map %>%
  addLegend(position = "bottomright",
            colors = c("blue", "red", "green", "orange"),
            labels = c("LICENCIATURA EN FISICA", "LICENCIATURA EN QUIMICA", "LICENCIATURA EN CIENCIAS NATURALES Y EDUCACION AMBIENTAL", "LICENCIATURA EN BIOLOGIA"),
            title = "Licenciaturas")

# Imprimir el mapa
map

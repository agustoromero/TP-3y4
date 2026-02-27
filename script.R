#cargo librerías
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(writexl)
library(ggplot2)
library(cluster)
library(tidyverse)

#set general de chunks
knitr::opts_chunk$set(echo = TRUE)
#set directorio de trabajo principal
setwd("C:/Users/agusr/Desktop/Diplo CSCyHD/TP-3y4")
# carpetas base 
dir.create("data", showWarnings = FALSE)
dir.create("scripts", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tablas", showWarnings = FALSE)
dir.create("outputs/graficos", showWarnings = FALSE)
#Leo base y resumo información
m4_atlas_argentina <- read_csv(
  file = "data/m4_atlas_argentina.csv",
  show_col_types = FALSE)

summary(m4_atlas_argentina)


#Separo filas con NA y analizo
filas_con_na <- m4_atlas_argentina %>%
  filter(if_any(everything(), is.na))

nrow(filas_con_na)
nrow(filas_con_na) / nrow(m4_atlas_argentina)

base_sin_na <- m4_atlas_argentina %>%
  filter(if_all(everything(), ~ !is.na(.)))

filas_con_na

#veo 4 filas sin separar en columnas con separador "," arreglo
#exporto en excel y arreglo
write_xlsx(
  m4_atlas_argentina,
  path = "m4_atlas_argentina.xlsx")
#importo corregido
m4_atlas_fixed <- read_xlsx("data/m4_atlas_fixed.xlsx")

##separo de nuevo sobre fixed
filas_con_na <- m4_atlas_fixed %>%
  filter(if_any(everything(), is.na))

nrow(filas_con_na)

# Filtrar solo las filas de provincias / gobierno Regional
filas_provincias <- filas_con_na %>%
  filter(nivel_gobierno == "Regional")

# Verificar
cat("Número de filas de provincias (Regional):", nrow(filas_provincias), "\n")
head(filas_provincias)
#elimino de filas con na

filas_con_na <- filas_con_na %>%
  filter(nivel_gobierno != "Regional")
filas_con_na

resumen_poblacion_excluida_de_base <- filas_con_na %>%
  summarise(
    n_na_poblacion = sum(is.na(poblacion)),            # filas con NA en poblacion
    n_valor_poblacion = sum(!is.na(poblacion)),       # filas con valor
    total_poblacion = sum(poblacion, na.rm = TRUE)    # suma de poblacion disponible
  )

resumen_poblacion_excluida_de_base

#Dejo base limpia sin NA
base_sin_na <- m4_atlas_fixed %>%
  filter(if_all(everything(), ~ !is.na(.)))

summary(base_sin_na)

#reconozco variables numericas en formato character asi que las transformo
#para no tener problemas.
vars_char <- base_sin_na %>%
  select(where(is.character)) %>%
  names()

vars_char

vars_continuas <- c(
  "tics_computadora", "tics_celular",
  "etnia_indigena", "etnia_afro",
  "recoleccion_residuos",
  "electricidad", "agua_mejorada", "saneamiento_mejorado",
  "desempleo_adulto", "desempleo_adulto_mujer",
  "participacion_mujeres", "participacion_hombres",
  "analfabetismo", "fin_secundaria_adultos", "empleo_publico_categoria"
)

base_sin_na <- base_sin_na %>%
  mutate(
    across(
      all_of(vars_continuas),
      ~ parse_double(.x)
    )
  )

summary(base_sin_na)
dicc_atlas_clasif <- read_excel(
  "data/dicc_atlas_jurisdiccion/dicc_atlas_clasif.xlsx",
  vars_demografica <- dicc_atlas_clasif %>%
    filter(dimension == "demografica") %>%
    pull(variable)
)


DT::datatable(
  dicc_atlas_clasif,
  caption = "Anexo A. Diccionario Atlas Argentina- con clasificación",
  options = list(
    scrollY = "500px",
    scrollX = TRUE,
    paging = FALSE,
    dom = 't'
  ),
  rownames = FALSE
)
vars_educacion <- dicc_atlas_clasif %>%
  filter(dimension == "educacion") %>%
  pull(variable)
vars_laboral <- dicc_atlas_clasif %>%
  filter(dimension == "laboral") %>%
  pull(variable)
vars_servicios <- dicc_atlas_clasif %>%
  filter(dimension == "servicios") %>%
  pull(variable)
vars_etnia <- dicc_atlas_clasif %>%
  filter(dimension == "etnia") %>%
  pull(variable)
vars_tics <- dicc_atlas_clasif %>%
  filter(dimension == "tics") %>%
  pull(variable)
vars_tamano <- dicc_atlas_clasif %>%
  filter(dimension == "cant_hogares") %>%
  pull(variable)

vars_excluir_pca <- c("poblacion", vars_tamano)
base_pca <- base_sin_na %>%
  select(all_of(c("codigo_unico", vars_pca))) %>%
  drop_na()

nrow(base_sin_na)
nrow(base_pca)
# 1. Convertir todas las columnas de vars_pca a numeric
base_pca <- base_pca %>%
  mutate(across(all_of(vars_pca), ~ as.numeric(.)))

# 2. Verificar que todas sean numéricas
sapply(base_pca %>% select(all_of(vars_pca)), class)


vars_educacion <- c(
  "analfabetismo",
  "fin_secundaria_adultos"
)

pca_educacion <- base_pca %>%
  select(all_of(vars_educacion)) %>%
  scale() %>%
  prcomp(center = TRUE, scale. = TRUE)

summary(pca_educacion)
pca_educacion$rotation
vars_laboral <- c(
  "participacion_mujeres",
  "participacion_hombres",
  "desempleo_adulto",
  "desempleo_adulto_mujer",
  "desempleo_adulto_hombre",
  "empleo_publico_categoria"
)

pca_laboral <- base_pca %>%
  select(all_of(vars_laboral)) %>%
  scale() %>%
  prcomp(center = TRUE, scale. = TRUE)

summary(pca_laboral)
pca_laboral$rotation
vars_servicios <- c(
  "agua_mejorada",
  "saneamiento_mejorado",
  "electricidad",
  "recoleccion_residuos"
)

pca_servicios <- base_pca %>%
  select(all_of(vars_servicios)) %>%
  scale() %>%
  prcomp(center = TRUE, scale. = TRUE)

summary(pca_servicios)
pca_servicios$rotation
vars_tics <- c(
  "tics_computadora",
  "tics_celular"
)

pca_tics <- base_pca %>%
  select(all_of(vars_tics)) %>%
  scale() %>%
  prcomp(center = TRUE, scale. = TRUE)

summary(pca_tics)
pca_tics$rotation
vars_etnia <- c(
  "etnia_indigena",
  "etnia_afro"
)

pca_etnia <- base_pca %>%
  select(all_of(vars_etnia)) %>%
  scale() %>%
  prcomp(center = TRUE, scale. = TRUE)

summary(pca_etnia)
pca_etnia$rotation
# -----------------------------
# Scores de PCA por dimensión
# -----------------------------

# Educación (solo PC1)
scores_educacion <- as.data.frame(pca_educacion$x[, 1])
colnames(scores_educacion) <- "educacion_nivel"

# Laboral (PC1, PC2, PC3)
scores_laboral <- as.data.frame(pca_laboral$x[, 1:3])
colnames(scores_laboral) <- c(
  "laboral_desempeno",
  "laboral_participacion",
  "laboral_empleo_publico"
)

# Servicios (PC1 y PC2)
scores_servicios <- as.data.frame(pca_servicios$x[, 1:2])
colnames(scores_servicios) <- c(
  "servicios_acceso_general",
  "servicios_composicion"
)

# TIC (PC1 y PC2)
scores_tics <- as.data.frame(pca_tics$x[, 1:2])
colnames(scores_tics) <- c(
  "tics_acceso_general",
  "tics_tipo_acceso"
)

# Etnia (PC1 y PC2)
scores_etnia <- as.data.frame(pca_etnia$x[, 1:2])
colnames(scores_etnia) <- c(
  "etnia_presencia",
  "etnia_composicion"
)

# -----------------------------
# Base final de perfiles
# -----------------------------

base_perfiles <- base_pca %>%
  select(
    codigo_unico,
    poblacion,
    urbano,
    tiene_ods
  ) %>%
  bind_cols(
    scores_educacion,
    scores_laboral,
    scores_servicios,
    scores_tics,
    scores_etnia
  )

# Chequeo rápido
head(base_perfiles)
vars_clustering <- c(
  "educacion_nivel",
  "laboral_desempeno", "laboral_participacion", "laboral_empleo_publico",
  "servicios_acceso_general", "servicios_composicion",
  "tics_acceso_general", "tics_tipo_acceso",
  "etnia_presencia", "etnia_composicion"
)

matriz_clustering <- base_perfiles %>%
  select(all_of(vars_clustering)) %>%
  drop_na()

#Estandarizamos

matriz_clustering_scaled <- scale(matriz_clustering)

#determinamos k

set.seed(123)

wss <- map_dbl(
  2:10,
  ~ kmeans(matriz_clustering_scaled, centers = .x, nstart = 25)$tot.withinss
)

quiebre_df <- tibble(
  k = 2:10,
  wss = wss
)

ggplot(quiebre_df, aes(k, wss)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Método para K-means",
    x = "Número de clusters (k)",
    y = "Suma de cuadrados intra-cluster"
  ) +
  theme_minimal()

calidad_prom <- map_dbl(
  2:10,
  function(k) {
    km <- kmeans(matriz_clustering_scaled, centers = k, nstart = 25)
    sil <- silhouette(km$cluster, dist(matriz_clustering_scaled))
    mean(sil[, 3])
  }
)

sil_df <- tibble(
  k = 2:10,
  calidad = calidad_prom
)

ggplot(sil_df, aes(k, calidad)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Análisis de calidad",
    x = "Número de clusters (k)",
    y = "calidad promedio"
  ) +
  theme_minimal()

set.seed(123)

k_optimo <- 6

kmeans_final <- kmeans(
  matriz_clustering_scaled,
  centers = k_optimo,
  nstart = 50
)

base_perfiles <- base_perfiles %>%
  mutate(cluster_kmeans = factor(kmeans_final$cluster))
perfiles_clusters <- base_perfiles %>%
  group_by(cluster_kmeans) %>%
  summarise(
    across(
      c(
        educacion_nivel,
        laboral_desempeno,
        laboral_participacion,
        laboral_empleo_publico,
        servicios_acceso_general,
        servicios_composicion,
        tics_acceso_general,
        tics_tipo_acceso,
        etnia_presencia,
        etnia_composicion
      ),
      mean,
      na.rm = TRUE
    ),
    n_municipios = n(),
    .groups = "drop"
  )
perfiles_clusters <- perfiles_clusters %>%
  left_join(
    base_perfiles %>%
      group_by(cluster_kmeans) %>%
      summarise(
        poblacion_promedio = mean(poblacion, na.rm = TRUE),
        proporcion_urbano = mean(urbano, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "cluster_kmeans"
  )
#muestro que el 6 tiene muy pocas unidades territoriales
base_perfiles %>%
  count(cluster_kmeans) %>%
  mutate(porcentaje = n / sum(n))

#veo si alguno esta muy dominado por grandes ciudades
base_perfiles %>%
  group_by(cluster_kmeans) %>%
  summarise(
    poblacion_mediana = median(poblacion, na.rm = TRUE),
    poblacion_max = max(poblacion, na.rm = TRUE)
  )
#veo proporcion urbana vs rural
base_perfiles %>%
  group_by(cluster_kmeans) %>%
  summarise(
    prop_urbano = mean(urbano, na.rm = TRUE)
  )


#grafico
perfiles_clusters %>%
  select(-n_municipios, -poblacion_promedio, -proporcion_urbano) %>%
  pivot_longer(
    cols = -cluster_kmeans,
    names_to = "dimension",
    values_to = "score"
  ) %>%
  ggplot(aes(dimension, score, fill = factor(cluster_kmeans))) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    fill = "Cluster",
    x = "",
    y = "Score promedio (PCA)"
  ) +
  theme_minimal()


scores_educacion <- c("educacion_nivel")

scores_laboral <- c(
  "laboral_desempeno",
  "laboral_participacion",
  "laboral_empleo_publico"
)

scores_servicios <- c(
  "servicios_acceso_general",
  "servicios_composicion"
)

scores_tics <- c(
  "tics_acceso_general",
  "tics_tipo_acceso"
)

scores_etnia <- c(
  "etnia_presencia",
  "etnia_composicion"
)


# Distancia euclídea
distancias <- dist(matriz_clustering_scaled, method = "euclidean")

# Clustering jerárquico (Ward)
hclust_ward <- hclust(distancias, method = "ward.D2")

# Cortamos en k = 6 (mismo k que k-means)
base_perfiles <- base_perfiles %>%
  mutate(cluster_hclust = factor(cutree(hclust_ward, k = 6)))

var_intra <- function(data, cluster_var, vars_dimension) {
  data %>%
    group_by(.data[[cluster_var]]) %>%
    summarise(
      across(
        all_of(vars_dimension),
        ~ var(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      var_intra_media = rowMeans(
        select(., all_of(vars_dimension)),
        na.rm = TRUE
      )
    )
}

################ K-means

var_educ_kmeans <- var_intra(base_perfiles, "cluster_kmeans", scores_educacion)
var_laboral_kmeans <- var_intra(base_perfiles, "cluster_kmeans", scores_laboral)
var_servicios_kmeans <- var_intra(base_perfiles, "cluster_kmeans", scores_servicios)
var_tics_kmeans <- var_intra(base_perfiles, "cluster_kmeans", scores_tics)
var_etnia_kmeans <- var_intra(base_perfiles, "cluster_kmeans", scores_etnia)


###################### Jerarquico

var_educ_hclust <- var_intra(base_perfiles, "cluster_hclust", scores_educacion)
var_laboral_hclust <- var_intra(base_perfiles, "cluster_hclust", scores_laboral)
var_servicios_hclust <- var_intra(base_perfiles, "cluster_hclust", scores_servicios)
var_tics_hclust <- var_intra(base_perfiles, "cluster_hclust", scores_tics)
var_etnia_hclust <- var_intra(base_perfiles, "cluster_hclust", scores_etnia)

#################### Tabla comparativa

tabla_varianza <- bind_rows(
  var_educ_kmeans %>% mutate(dimension = "Educación", metodo = "K-means"),
  var_laboral_kmeans %>% mutate(dimension = "Laboral", metodo = "K-means"),
  var_servicios_kmeans %>% mutate(dimension = "Servicios", metodo = "K-means"),
  var_tics_kmeans %>% mutate(dimension = "TICs", metodo = "K-means"),
  var_etnia_kmeans %>% mutate(dimension = "Etnia", metodo = "K-means"),
  
  var_educ_hclust %>% mutate(dimension = "Educación", metodo = "Jerárquico (Ward)"),
  var_laboral_hclust %>% mutate(dimension = "Laboral", metodo = "Jerárquico (Ward)"),
  var_servicios_hclust %>% mutate(dimension = "Servicios", metodo = "Jerárquico (Ward)"),
  var_tics_hclust %>% mutate(dimension = "TICs", metodo = "Jerárquico (Ward)"),
  var_etnia_hclust %>% mutate(dimension = "Etnia", metodo = "Jerárquico (Ward)")
) %>%
  group_by(dimension, metodo) %>%
  summarise(
    var_intra_media = mean(var_intra_media, na.rm = TRUE),
    .groups = "drop"
  )

DT::datatable(
  tabla_varianza,
  rownames = FALSE,
  options = list(
    dom = "t",
    pageLength = 10
  ),
  caption = "Varianza intra-cluster media por dimensión y método de clustering"
)

ggplot(tabla_varianza,
       aes(x = dimension, y = var_intra_media, fill = metodo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparación de varianza intra-cluster por dimensión",
    x = "",
    y = "Varianza intra-cluster media",
    fill = "Método"
  ) +
  theme_minimal()

# ------------------------------------------------------
# 1. VARIABLES DE SCORES PCA UTILIZADAS EN CLUSTERING
# ------------------------------------------------------

vars_scores <- c(
  "educacion_nivel",
  "laboral_desempeno",
  "laboral_participacion",
  "laboral_empleo_publico",
  "servicios_acceso_general",
  "servicios_composicion",
  "tics_acceso_general",
  "tics_tipo_acceso",
  "etnia_presencia",
  "etnia_composicion"
)

# ------------------------------------------------------
# 2. CENTROIDES KMEANS
# ------------------------------------------------------

centroides_kmeans <- base_perfiles %>%
  group_by(cluster_kmeans) %>%
  summarise(
    across(all_of(vars_scores), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(metodo = "K-means") %>%
  rename(cluster = cluster_kmeans)

# ------------------------------------------------------
# 3. CENTROIDES JERARQUICOS
# ------------------------------------------------------

centroides_ward <- base_perfiles %>%
  group_by(cluster_hclust) %>%
  summarise(
    across(all_of(vars_scores), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(metodo = "Jerárquico") %>%
  rename(cluster = cluster_hclust)

# ------------------------------------------------------
# 4. TABLA COMPARATIVA FINAL
# ------------------------------------------------------

tabla_centroides <- bind_rows(
  centroides_kmeans,
  centroides_ward
)

print(tabla_centroides)

# ------------------------------------------------------
# 5. FORMATO LARGO PARA GRAFICO
# ------------------------------------------------------

tabla_long <- tabla_centroides %>%
  pivot_longer(
    cols = all_of(vars_scores),
    names_to = "dimension",
    values_to = "valor"
  )

# ------------------------------------------------------
# 6. GRAFICO COMPARATIVO DE PERFILES PROMEDIO
# ------------------------------------------------------

ggplot(tabla_long,
       aes(x = dimension,
           y = valor,
           fill = metodo)) +
  geom_col(position = "dodge") +
  facet_wrap(~cluster) +
  coord_flip() +
  labs(
    title = "Comparación de Perfiles Promedio por Cluster",
    x = "Dimensión",
    y = "Score promedio"
  ) +
  theme_minimal()

# ======================================================
# FASE 5 – VALIDACIÓN E INTERPRETACIÓN CON RANDOM FOREST
# ======================================================

# ------------------------------------------------------
# 1. Preparamos la base para RF
#    - Variable dependiente: cluster_kmeans
#    - Predictoras: scores PCA (mismas que en clustering)
# ------------------------------------------------------

rf_data <- base_perfiles %>%
  select(
    cluster_kmeans,
    educacion_nivel,
    laboral_desempeno,
    laboral_participacion,
    laboral_empleo_publico,
    servicios_acceso_general,
    servicios_composicion,
    tics_acceso_general,
    tics_tipo_acceso,
    etnia_presencia,
    etnia_composicion
  ) %>%
  drop_na()

rf_data$cluster_kmeans <- as.factor(rf_data$cluster_kmeans)

# ------------------------------------------------------
# 2. Ajuste del modelo Random Forest
# ------------------------------------------------------

set.seed(123)

rf_model <- randomForest(
  cluster_kmeans ~ .,
  data = rf_data,
  importance = TRUE,
  ntree = 500
)

# ------------------------------------------------------
# 3. Importancia de variables – Mean Decrease Gini
# ------------------------------------------------------

importancia_rf <- importance(rf_model, type = 2) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("dimension") %>%
  arrange(desc(MeanDecreaseGini))

importancia_rf

# ------------------------------------------------------
# 4. Gráfico de importancia
# ------------------------------------------------------

ggplot(importancia_rf,
       aes(x = reorder(dimension, MeanDecreaseGini),
           y = MeanDecreaseGini)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de dimensiones en la clasificación de clusters",
    subtitle = "Random Forest – Mean Decrease Gini",
    x = "",
    y = "Mean Decrease Gini"
  ) +
  theme_minimal()

# ------------------------------------------------------
# 5. Chequeo territorial: Urbano / Rural por cluster
# ------------------------------------------------------

urbano_cluster <- base_perfiles %>%
  group_by(cluster_kmeans) %>%
  summarise(
    proporcion_urbano = mean(urbano, na.rm = TRUE),
    poblacion_promedio = mean(poblacion, na.rm = TRUE),
    n_municipios = n(),
    .groups = "drop"
  )

urbano_cluster

# ------------------------------------------------------
# 6. Visualización urbano / rural
# ------------------------------------------------------

ggplot(urbano_cluster,
       aes(x = cluster_kmeans, y = proporcion_urbano)) +
  geom_col(fill = "darkorange") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Composición urbano-rural por cluster",
    x = "Cluster",
    y = "Proporción de municipios urbanos"
  ) +
  theme_minimal()

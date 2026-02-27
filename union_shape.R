
# =========================================================
# BLOQUE 2 — CARGAR SHAPEFILE
# =========================================================

library(sf)

mapa_local <- st_read("data/geodata/gobiernos_locales4/gobiernos_locales4.shp")

# ver resumen general
mapa_local

# =========================================================
# ATLAS CAF + CARTOGRAFÍA OFICIAL MUNICIPAL
# Script profesional de integración espacial
# Autor: Spatial Data Science Workflow
# =========================================================

library(sf)
library(dplyr)
library(stringr)
library(readr)
library(janitor)
library(purrr)
library(ggplot2)

options(scipen = 999)

# =========================================================
# FUNCIÓN 1 — CONSTRUCCIÓN DEL codigo_unico (METODOLOGÍA CAF)
# =========================================================
# CAF define:
# codigo_unico = ISO3 + codigo regional + codigo municipal
#
# En Argentina:
# ISO3 = ARG
# codigo regional = provincia (2 o 3 dígitos)
# codigo municipal = cmu (6 dígitos)
#
# Se aplica padding para evitar errores de formato.

construir_codigo_unico <- function(df,
                                   iso3 = "ARG",
                                   var_region,
                                   var_local) {
  
  df %>%
    mutate(
      
      codigo_region = str_pad(
        as.character(.data[[var_region]]),
        width = 3,
        side = "left",
        pad = "0"
      ),
      
      codigo_local = str_pad(
        as.character(.data[[var_local]]),
        width = 6,
        side = "left",
        pad = "0"
      ),
      
      codigo_unico = paste0(
        iso3,
        codigo_region,
        codigo_local
      )
    )
}

# =========================================================
# FUNCIÓN 2 — VALIDACIÓN DE VARIABLES CLAVE
# =========================================================
# Verifica la presencia de variables armonizadas CAF

validar_variables_clave <- function(df) {
  
  vars_clave <- c(
    "analfabetismo",
    "secundaria_completa",
    "participacion_laboral",
    "desempleo",
    "acceso_agua",
    "acceso_saneamiento",
    "acceso_electricidad",
    "acceso_residuos"
  )
  
  faltantes <- setdiff(vars_clave, names(df))
  
  if(length(faltantes) > 0) {
    
    warning("Variables faltantes detectadas:")
    print(faltantes)
    
  } else {
    
    message("Validación OK: todas las variables presentes")
    
  }
  
  return(faltantes)
  
}

# =========================================================
# FUNCIÓN 3 — PROMEDIOS PONDERADOS
# =========================================================
# Necesario cuando municipios intersectan múltiples
# unidades censales

promedio_ponderado <- function(df,
                               grupo,
                               variables,
                               ponderador) {
  
  df %>%
    group_by(.data[[grupo]]) %>%
    summarise(
      
      across(
        all_of(variables),
        
        ~ weighted.mean(
          .x,
          w = .data[[ponderador]],
          na.rm = TRUE
        ),
        
        .names = "{.col}"
      ),
      
      poblacion_total = sum(
        .data[[ponderador]],
        na.rm = TRUE
      ),
      
      .groups = "drop"
      
    )
  
}

# =========================================================
# FUNCIÓN 4 — CARGAR SHAPEFILE Y CONSTRUIR KEY
# =========================================================

cargar_cartografia_municipal <- function(path_shp) {
  
  mapa <- st_read(path_shp, quiet = TRUE)
  
  mapa <- mapa %>%
    construir_codigo_unico(
      var_region = "cpr",
      var_local = "cmu"
    )
  
  return(mapa)
  
}

# =========================================================
# FUNCIÓN 5 — CONTROL DE CALIDAD POST JOIN
# =========================================================

control_calidad_join <- function(mapa, datos) {
  
  total_shape <- nrow(mapa)
  total_datos <- nrow(datos)
  
  mapa_join <- mapa %>%
    left_join(datos, by = "codigo_unico")
  
  sin_datos <- mapa_join %>%
    filter(is.na(poblacion))
  
  reporte <- list(
    
    total_shape = total_shape,
    total_datos = total_datos,
    
    sin_datos_n = nrow(sin_datos),
    
    porcentaje_match = mean(
      mapa$codigo_unico %in%
        datos$codigo_unico
    )
    
  )
  
  print(reporte)
  
  return(mapa_join)
  
}

# =========================================================
# FUNCIÓN 6 — INSPECCIÓN DE COHERENCIA NOMINAL
# =========================================================
# Detecta posibles errores de join

validar_coherencia_nombres <- function(mapa_join,
                                       nombre_mapa = "nam",
                                       nombre_datos = "nombre_subnacional") {
  
  comparacion <- mapa_join %>%
    st_drop_geometry() %>%
    select(
      codigo_unico,
      nombre_shape = all_of(nombre_mapa),
      nombre_datos = all_of(nombre_datos)
    )
  
  comparacion %>%
    mutate(
      
      similitud =
        stringdist::stringsim(
          nombre_shape,
          nombre_datos,
          method = "jw"
        )
      
    ) %>%
    arrange(similitud)
  
}

# =========================================================
# FUNCIÓN 7 — REPORTE DE MUNICIPIOS SIN DATOS
# =========================================================

reporte_sin_datos <- function(mapa_join) {
  
  mapa_join %>%
    st_drop_geometry() %>%
    filter(is.na(poblacion)) %>%
    select(
      codigo_unico,
      nam,
      jur,
      cat_gl
    )
  
}

# =========================================================
# FUNCIÓN 8 — MAPA DE VALIDACIÓN VISUAL
# =========================================================

mapa_validacion <- function(mapa_join,
                            variable = "cluster_kmeans") {
  
  ggplot(mapa_join) +
    
    geom_sf(
      aes(fill = factor(.data[[variable]])),
      color = NA
    ) +
    
    theme_minimal() +
    
    labs(
      fill = "Cluster",
      title = "Validación espacial de tipología municipal"
    )
  
}

# =========================================================
# PIPELINE PRINCIPAL
# =========================================================

ejecutar_pipeline <- function(path_shp,
                              base_atlas,
                              vars_ponderadas,
                              var_peso = "poblacion") {
  
  message("Cargando cartografía...")
  mapa <- cargar_cartografia_municipal(path_shp)
  
  message("Validando variables...")
  validar_variables_clave(base_atlas)
  
  message("Calculando promedios ponderados...")
  base_agregada <- promedio_ponderado(
    base_atlas,
    grupo = "codigo_unico",
    variables = vars_ponderadas,
    ponderador = var_peso
  )
  
  message("Uniendo bases...")
  mapa_join <- control_calidad_join(
    mapa,
    base_agregada
  )
  
  message("Generando reporte de faltantes...")
  reporte <- reporte_sin_datos(mapa_join)
  
  return(list(
    mapa = mapa_join,
    reporte = reporte
  ))
  
}

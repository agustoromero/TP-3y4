# -1)explorar y describir los datos disponibles,
# 
# -2)analizar relaciones y correlaciones entre variables,
# 
# -3)evaluar la viabilidad de reducir la dimensionalidad de las diferentes dimensiones presentes en los indicadores
# 
# -4)identificar grupos de municipios con perfiles similares,
# 
# -5)interpretar los clusters resultantes


library(pdftools)
library(dplyr)
library(stringr)
library(tibble)
library(openxlsx)

pdf_path <- "anexo_codigo_gobiernos_locales_2022.pdf"

texto_pdf <- pdf_text(pdf_path)
lineas <- str_split(paste(texto_pdf, collapse = "\n"), "\n")[[1]]
lineas_datos <- lineas %>%
  str_trim() %>%
  .[str_detect(., "^\\d{6}\\s+\\d{5}")]
head(lineas_datos, 20)
length(lineas_datos)

dicc_atlas <- tibble(linea = lineas_datos) %>%
  mutate(
    codigo_gob_local = str_extract(linea, "^\\d{6}"),
    codigo_departamento = str_extract(linea, "(?<=\\s)\\d{5}(?=\\s)"),
    categoria = str_extract(linea, "(CO|MU|CF|JV|JG|CM)"),
    resto = str_remove(linea, "^\\d{6}\\s+\\d{5}\\s+\\w+\\s+")
  ) %>%
  separate(
    resto,
    into = c("nombre_gobierno_local", "nombre_departamento"),
    sep = "\\s{2,}",
    fill = "right"
  ) %>%
  select(-linea)

glimpse(dicc_atlas)

dicc_atlas$provincia <- NA_character_

#loop
current_prov <- NA
fila_dicc <- 1

for (i in seq_along(lineas)) {
  
  # Detectar encabezado de provincia (ej: "06 Buenos Aires")
  if (str_detect(lineas[i], "^\\d{2}\\s+[A-Za-z]")) {
    current_prov <- str_remove(lineas[i], "^\\d{2}\\s+")
  }
  
  # Detectar fila de datos
  if (str_detect(lineas[i], "^\\s*\\d{6}\\s+\\d{5}")) {
    dicc_atlas$provincia[fila_dicc] <- current_prov
    fila_dicc <- fila_dicc + 1
  }
}

count(dicc_atlas, provincia, sort = TRUE)

dicc_atlas <- dicc_atlas %>%
  mutate(
    codigo_unico = paste0(
      "ARG",
      str_pad(codigo_departamento, 5, pad = "0"),
      codigo_gob_local
    )
  )
head(dicc_atlas$codigo_unico)

dicc_atlas <- dicc_atlas %>%
  mutate(
    nombre_nivel = case_when(
      categoria == "MU" ~ "Municipio",
      categoria == "CO" ~ "Comuna",
      categoria == "CF" ~ "Comisión de fomento",
      categoria == "JG" ~ "Junta de gobierno",
      categoria == "JV" ~ "Junta vecinal",
      categoria == "CM" ~ "Comisión municipal",
      TRUE ~ NA_character_
    )
  )
count(dicc_atlas, nombre_nivel)

write.xlsx(
  dicc_atlas,
  "dicc_jurisdiccion_atlas.xlsx",
  overwrite = TRUE
)

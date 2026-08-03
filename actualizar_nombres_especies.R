#!/usr/bin/env Rscript
# ============================================================
# Actualiza nombres de especies en BeagleChannel_links_standarized.csv
# usando el archivo maestro Links_data_2024_06_24_Time_04_31_40.xlsx
# como fuente de nombres correctos/actualizados.
#
# Lógica:
#   El xlsx tiene, para la red "BeagleChannel", columnas
#   resource / consumer (nombres YA corregidos) y
#   resource_original_name / consumer_original_name (nombre viejo,
#   solo presente cuando hubo un cambio, con comentario del revisor).
#   Se arma un diccionario nombre_viejo -> nombre_nuevo y se aplica
#   a ambas columnas del CSV (que usa "." en vez de espacios).
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(purrr)

# ---- 1. Rutas (ajustar si hace falta) ----------------------
path_xlsx <- "Links_data_2024_06_24_Time_04_31_40.xlsx"
path_csv  <- "BeagleChannel_links_standarized.csv"
path_out  <- "BeagleChannel_links_standarized_actualizado.csv"
path_log  <- "reporte_cambios_nombres.csv"

# ---- 2. Cargar datos -----------------------------------------
links_master <- read_excel(path_xlsx, sheet = "links")

links_csv <- read.csv(path_csv, stringsAsFactors = FALSE)

# ---- 3. Filtrar solo la red Beagle Channel --------------------
beagle <- links_master %>%
  filter(network == "BeagleChannel")

# ---- 4. Construir el diccionario nombre_viejo -> nombre_nuevo --
# Tomamos por separado los pares (original -> actual) desde
# resource y desde consumer, y los unimos en un solo mapa.
map_resource <- beagle %>%
  filter(!is.na(resource_original_name), resource_original_name != "") %>%
  select(old = resource_original_name, new = resource)

map_consumer <- beagle %>%
  filter(!is.na(consumer_original_name), consumer_original_name != "") %>%
  select(old = consumer_original_name, new = consumer)

name_map <- bind_rows(map_resource, map_consumer) %>%
  distinct()

# Chequeo de consistencia: un mismo nombre viejo no debería
# mapear a dos nombres nuevos distintos.
conflictos <- name_map %>%
  count(old) %>%
  filter(n > 1)

if (nrow(conflictos) > 0) {
  warning("Hay nombres viejos con mas de un mapeo posible. Revisar:")
  print(conflictos)
}

# El CSV usa "." en vez de espacio -> convertimos el diccionario
# al mismo formato para poder hacer el reemplazo directo.
name_map <- name_map %>%
  mutate(
    old_dot = str_replace_all(old, " ", "."),
    new_dot = str_replace_all(new, " ", ".")
  )

cat("Nombres a actualizar (", nrow(name_map), "):\n", sep = "")
print(name_map %>% select(old_dot, new_dot))

# ---- 5. Función de reemplazo ------------------------------------
lookup <- setNames(name_map$new_dot, name_map$old_dot)

actualizar_nombre <- function(x) {
  ifelse(x %in% names(lookup), lookup[x], x)
}

# ---- 6. Aplicar el reemplazo y registrar los cambios -------------
links_actualizado <- links_csv %>%
  mutate(
    resource_original = resource,
    consumer_original = consumer,
    resource = actualizar_nombre(resource),
    consumer = actualizar_nombre(consumer)
  )

reporte <- links_actualizado %>%
  filter(resource != resource_original | consumer != consumer_original) %>%
  select(resource_original, resource, consumer_original, consumer)

cat("\nFilas del CSV modificadas:", nrow(reporte), "de", nrow(links_csv), "\n")

# ---- 7. Reemplazar "." por espacio para mejor visualizacion --------
# El CSV original usa "." en vez de espacio entre palabras (y a veces
# un "." final suelto, ej. "Ballia.sp." o "Delesseriaceae."). Lo
# convertimos a espacios y limpiamos espacios repetidos/sobrantes.
limpiar_nombre <- function(x) {
  x %>%
    str_replace_all("\\.", " ") %>%
    str_squish()
}

links_final <- links_actualizado %>%
  mutate(
    resource = limpiar_nombre(resource),
    consumer = limpiar_nombre(consumer)
  ) %>%
  select(resource, consumer)

# ---- 8. Guardar salidas -------------------------------------------
write.csv(links_final, path_out, row.names = FALSE)
write.csv(reporte, path_log, row.names = FALSE)

cat("\nListo:\n")
cat(" - CSV actualizado (nombres con espacios) ->", path_out, "\n")
cat(" - Reporte de cambios ->", path_log, "\n")

# ---- 9. Chequeo rapido de duplicados tras la actualizacion --------
# (dos links distintos podrian colapsar en el mismo par tras
#  la unificacion de nombres, ej. sinonimos)
dups <- links_final %>%
  count(resource, consumer) %>%
  filter(n > 1)

if (nrow(dups) > 0) {
  cat("\nATENCION: hay", nrow(dups), "pares resource-consumer duplicados",
      "tras la actualizacion (posibles sinonimos unificados).\n")
  print(dups)
} else {
  cat("\nSin duplicados nuevos tras la actualizacion.\n")
}

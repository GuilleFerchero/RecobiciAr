#' Obtener recorridos históricos de Ecobici
#'
#' Descarga los archivos ZIP anuales de recorridos, extrae el CSV y procesa
#' las fechas y duraciones.
#'
#' @param year Entero. Año de los datos (ej. 2024).
#' @param month Entero o Nulo. Mes numérico (1-12) para filtrar.
#'
#' @return Un tibble con los recorridos del mes seleccionado.
#' @export
#' @importFrom dplyr mutate filter select %>%
#' @importFrom readr read_csv
#' @importFrom lubridate ymd_hms wday
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom utils download.file unzip
get_recorridos <- function(year = 2024, month = 1) {

  # 1. Construir URL
  base_url <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas/bicicletas-publicas"
  file_url <- glue::glue("{base_url}/recorridos-realizados-{year}.zip")

  message(glue::glue("Descargando y procesando recorridos de {year}... (esto puede tardar debido al tamaño del archivo)"))

  # 2. Usar la función auxiliar para manejar el ZIP
  df <- .download_and_read_zip(file_url)

  # 3. Limpieza de nombres INICIAL (Clave para evitar errores de tipeo después)
  df <- janitor::clean_names(df)

  # 4. Procesamiento
  # Nota: Ecobici a veces cambia nombres de columnas (ej: duration vs duracion_recorrido).
  # Intentamos ser robustos asumiendo nombres estandarizados por clean_names.

  df_procesado <- df %>%
    # Convertir fechas usando lubridate (asume formato YYYY-MM-DD HH:MM:SS)
    dplyr::mutate(
      fecha_origen = lubridate::ymd_hms(fecha_origen_recorrido),
      fecha_destino = lubridate::ymd_hms(fecha_destino_recorrido),
      # Calculamos duración real nosotros (más confiable que la columna que viene)
      duracion_segundos = as.numeric(fecha_destino - fecha_origen),
      mes_origen = lubridate::month(fecha_origen),
      dia_label = lubridate::wday(fecha_origen, label = TRUE, abbr = FALSE)
    ) %>%
    # Filtramos por el mes solicitado
    dplyr::filter(mes_origen == month) %>%
    # Seleccionamos columnas de interés
    dplyr::select(
      id_usuario,
      fecha_origen,
      fecha_destino,
      duracion_segundos,
      dia_label,
      long_estacion_origen,
      long_estacion_destino
    )

  return(df_procesado)
}

# --- FUNCIÓN AUXILIAR (No exportada) ---
# El punto al inicio del nombre es una convención para funciones internas,
# y no agregamos @export para que el usuario final no la vea.

.download_and_read_zip <- function(url) {
  # Guardar opciones actuales y restaurarlas al salir (on.exit)
  old_options <- options(timeout = 300)
  on.exit(options(old_options))

  # Rutas temporales
  temp_dir <- tempdir()
  temp_zip <- file.path(temp_dir, "temp_ecobici.zip")

  # Descarga
  tryCatch({
    utils::download.file(url, temp_zip, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    stop("Error al descargar el archivo ZIP. Verifique la conexión o el año.")
  })

  # Listar archivos dentro del ZIP
  files_in_zip <- utils::unzip(temp_zip, list = TRUE)
  csv_file <- files_in_zip$Name[grep("\\.csv$", files_in_zip$Name)]

  if (length(csv_file) == 0) stop("No se encontró ningún CSV dentro del ZIP.")

  # Extraer solo el CSV
  utils::unzip(temp_zip, files = csv_file[1], exdir = temp_dir)

  # Leer CSV (Usamos readr que es más rápido que read.csv para archivos grandes)
  full_path <- file.path(temp_dir, csv_file[1])
  df <- readr::read_csv(full_path, show_col_types = FALSE)

  # Limpiar basura temporal
  unlink(temp_zip)
  unlink(full_path)

  return(df)
}

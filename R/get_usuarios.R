#' Obtener datos de usuarios de Ecobici
#'
#' Descarga el dataset anual de usuarios desde el portal de datos abiertos,
#' permite filtrar por mes y opcionalmente enriquecer la data con categorías
#' de edad y franjas horarias.
#'
#' @param year Entero. Año de los datos (ej. 2024).
#' @param month Entero o Nulo. Mes numérico (1-12) para filtrar. Si es NULL, devuelve todo el año.
#' @param enriquecer Lógico. Si es TRUE, agrega columnas calculadas (rango etario, franja horaria).
#'
#' @return Un tibble con los datos de usuarios.
#' @export
#' @importFrom dplyr filter mutate case_when select %>%
#' @importFrom readr read_csv
#' @importFrom lubridate month hour wday
#' @importFrom janitor clean_names
#' @importFrom glue glue
get_usuarios <- function(year = 2024, month = NULL, enriquecer = FALSE) {


  base_url <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas/bicicletas-publicas"
  file_url <- glue::glue("{base_url}/usuarios_ecobici_{year}.csv")


  message(glue::glue("Descargando datos de usuarios del año {year}... esto puede tardar unos segundos."))


  df <- tryCatch({
    readr::read_csv(file_url, show_col_types = FALSE)
  }, error = function(e) {
    stop("No se pudo descargar el archivo. Verifica tu conexión o si el año solicitado existe en el portal de datos.")
  })


  df <- janitor::clean_names(df)


  if (!is.null(month)) {
    df <- df %>%
      dplyr::filter(lubridate::month(fecha_alta) == month)
  }


  if (enriquecer) {
    df <- df %>%
      dplyr::mutate(
        genero = dplyr::case_when(
          genero_usuario == "MALE" ~ "Masculino",
          genero_usuario == "FEMALE" ~ "Femenino",
          TRUE ~ "Otro"
        ),
        mes_nombre = lubridate::month(fecha_alta, label = TRUE, abbr = FALSE),
        hora = lubridate::hour(hora_alta),
        momento_dia = dplyr::case_when(
          hora >= 0 & hora < 6 ~ "Madrugada",
          hora >= 6 & hora < 12 ~ "Mañana",
          hora >= 12 & hora < 18 ~ "Tarde",
          TRUE ~ "Noche"
        ),
        dia_semana = lubridate::wday(fecha_alta, label = TRUE, abbr = FALSE),
        rango_etario = dplyr::case_when(
          edad_usuario < 15 ~ "1. Menores de 15",
          edad_usuario < 20 ~ "2. 15 a 20",
          edad_usuario < 25 ~ "3. 20 a 25",
          edad_usuario < 30 ~ "4. 25 a 30",
          edad_usuario < 35 ~ "5. 30 a 35",
          edad_usuario < 40 ~ "6. 35 a 40",
          edad_usuario < 45 ~ "7. 40 a 45",
          edad_usuario < 50 ~ "8. 45 a 50",
          edad_usuario < 55 ~ "9. 50 a 55",
          TRUE ~ "10. Mayores de 55"
        )
      )
  }

  return(df)
}

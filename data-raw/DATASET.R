
### Armo listado de diccionario mundial para:
# --> decimal
# --> thousands
# --> grouping
# --> currency
# --> numerals
locs_num <- list.files("data-raw/locale-numbers", full.names = TRUE)
loc_names_num <- basename(tools::file_path_sans_ext(locs_num))
locs_num <- lapply(locs_num, jsonlite::fromJSON)
names(locs_num) <- loc_names_num

### Armo listado de diccionario mundial para fechas
locs_dtm <- list.files("data-raw/locale-dates", full.names = TRUE)
loc_names_dtm <- basename(tools::file_path_sans_ext(locs_dtm))
locs_dtm <- lapply(locs_dtm, jsonlite::fromJSON)
names(locs_dtm) <- loc_names_dtm


all(loc_names_dtm %in% loc_names_num)
loc_names_dtm[!loc_names_dtm %in% loc_names_num]

### Lista con diccionarios disponibles para números y fechas
available_locales <- list(num = loc_names_num, dtm = loc_names_dtm)

### Lista con diccionario mundial de valores para números y fechas
locales <-  list(num = locs_num, dtm = locs_dtm)
locales <- utils::modifyList(locs_num, locs_dtm)


locales_table <- locales |>
  unlist(recursive = FALSE) |>
  tibble::enframe(x = _) |>
  tidyr::separate(name, c("locale","variable"), sep = "\\.") |>
  tidyr::pivot_wider(names_from = "variable") |>
  dplyr::mutate(lang = substr(locale,1,2)) |>
  dplyr::select(lang, everything())

locales <- purrr::transpose(locales_table)
available_locales <- locales_table$locale
names(locales) <- locales_table$locale

### ¿Qué son? Contenedores?
fallbacks <- list(
  "ar-EG" = "ar-*",
  "pt-BR" = "pt-*",
  "es-MX" = "^(?!.*(?:ES|MX))(?=.*(?:es))",
  "fr-FR" = "fr-*",
  "zh-CN" = "zh-TW"
)

### ¿Qué son?
sys_fallbacks <- list(
  "ar-EG" = "ar-*",
  "es-ES" = "es-*",
  "de-DE" = "de-*",
  "fr-FR" = "fr-*",
  "en-US" = "en-CA"
)

high_weights <- c("en-US", "es-ES")

### Diccionario de meses
locale_month_names <- locales_table |>
  dplyr::select(locale, months, shortMonths) |>
  tidyr::unnest(cols = c(months, shortMonths)) |>
  dplyr::ungroup() |>
  dplyr::group_by(locale) |>
  dplyr::mutate(month_order = 1:dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(weight = ifelse(locale %in% high_weights, 2, 1)) |>
  dplyr::arrange(desc(weight))




# grepl("^(?!.*(?:ES|MX))(?=.*(?:es))","es-CO", perl = TRUE)
# grepl("^(?!.*(?:ES|MX))(?=.*(?:es))","es-MX", perl = TRUE)
# grepl("^(?!.*(?:ES|MX))(?=.*(?:es))","es-ES", perl = TRUE)


usethis::use_data(locales,
                  fallbacks,
                  sys_fallbacks,
                  locale_month_names,
                  #locale_summary,
                  available_locales,
                  internal = TRUE, overwrite = TRUE)


usethis::use_data(available_locales,
                  internal = FALSE, overwrite = TRUE)




get_locale <- function(locale){

  if(is.null(locale)) return()
  # locale <- "es-CO"
  # locale <- "es-BO"
  in_available_locales <- locale %in% available_locales
  fallback <- which_locale_fallback(locale)
  if(!in_available_locales && dstools::is.empty(fallback)){
    stop(paste0("Locale or fallback locale not available, must be one of: ",
                paste(available_locales, collapse = ", ")))
  }
  fallback <- locales[[fallback %||% ""]]
  locale_list <- dstools::removeNulls(locales[[locale]])
  fallback_missing <- fallback[!names(fallback) %in% names(locale_list)]
  utils::modifyList(locale_list %||% list(), fallback_missing %||% list()) # es-BO ok

}

which_locale_fallback <- function(locale){
  fb_match_idx <- unlist(lapply(fallbacks,
                                function(x) grepl(x,locale, perl = TRUE)))
  if(!any(fb_match_idx)) return()
  fallback <- names(fallbacks[fb_match_idx]) %||% NULL
  if(is.null(fallback)) return()
  fallback
}

which_locale_sys_fallback <- function(locale){

  fb_match_idx <- unlist(lapply(sys_fallbacks,
                                function(x) grepl(x,locale, perl = TRUE)))
  if(!any(fb_match_idx)) return()
  fallback <- names(sys_fallbacks[fb_match_idx]) %||% NULL
  if(is.null(fallback)) return()
  fallback
}


guess_locale <- function(separators_decimal, separators_thousands) {

  file_locale <- locale_summary
  if (separators_thousands == " ") {
    file_locale <- file_locale |> dplyr::filter(!.data$thousands %in% c("٬", ".", ",", "\u0027")) # "٬" es non-ASCII character
  } else {
    file_locale <- file_locale |>  dplyr::filter(.data$thousands %in% separators_thousands)
  }

  file_locale <- file_locale |>  dplyr::filter(.data$decimal %in% separators_decimal)
  guess_l <- file_locale$locale
  guess_l
}

guess_specifier <- function(locale = "en-US") {
  if (is.null(locale)) return()
  guess_s <- get_locale(locale)[c("decimal", "thousands")]
  paste0(0, ", '",
         guess_s$decimal, "', '",
         guess_s$thousands, "') + '")
  # guess_s <- paste0(guess_s$thousands, guess_s$decimal, '0f')
  # guess_s
}

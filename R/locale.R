
get_locale <- function(locale){
  # locale <- "es-CO"
  # locale <- "es-BO"
  in_available_locales <- locale %in% makeup::available_locales
  fallback <- which_locale_fallback(locale)
  if(!in_available_locales && is.empty(fallback)){
    stop(paste0("Locale or fallback locale not available, must be one of: ",
                paste(available_locales, collapse = ", ")))
  }
  fallback <- makeup::locales[[fallback %||% ""]]
  locale_list <- removeNulls(makeup::locales[[locale]])
  fallback_missing <- fallback[!names(fallback) %in% names(locale_list)]
  modifyList(locale_list %||% list(), fallback_missing %||% list()) # es-BO ok

}

which_locale_fallback <- function(locale){
  fb_match_idx <- unlist(lapply(makeup::fallbacks,
                                function(x) grepl(x,locale, perl = TRUE)))
  if(!any(fb_match_idx)) return()
  fallback <- names(makeup::fallbacks[fb_match_idx]) %||% NULL
  if(is.null(fallback)) return()
  fallback
}

which_locale_sys_fallback <- function(locale){
  fb_match_idx <- unlist(lapply(makeup::sys_fallbacks,
                                function(x) grepl(x,locale, perl = TRUE)))
  if(!any(fb_match_idx)) return()
  fallback <- names(makeup::sys_fallbacks[fb_match_idx]) %||% NULL
  if(is.null(fallback)) return()
  fallback
}

removeNulls <- function(x){
  if (length(x) == 0 || !is.list(x))
    return(x)
  if(is.empty(x)) return(list())
  x[!unlist(lapply(x,is.null))]
}


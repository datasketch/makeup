
#' @title makeup_dat
#'
#' @description Formats date values for human use. You can set an example of how the output is wanted and a certain locale setting.
#'
#' @param v character value to be formatted
#' @param sample a character value as an example of how the output is wanted. Days and years must be as numbers.
#' @param locale character string naming a locale. For example: "es-MX" for mexico locale setting. You can check your default system locale with Sys.getlocale(). See available values for setting up at makeup::available_locales
#' @param format a character vector of date-time formats. Default is "%-m/%-d/%Y"
#'
#' @return a character value with a specific date format
#' @export
#'
#' @examples
#'
#'   v <- "2020-03-04"
#'   makeup_dat(v, sample = "Ene 3", locale = "es-CO")
#'   makeup_dat(v, sample = "Enero 3", locale = "es-CO")
#'   makeup_dat(v, sample = "Enero 3 2022", locale = "es-CO")
#'   makeup_dat(v, sample = "2022 Enero 3", locale = "es-CO")
#'
#'   ### Sample doesnt' work with day as a text value
#'   makeup_dat(v, sample = "Tres de Enero", locale = "es-CO")
#'
#'
makeup_dat <- function(v, sample = NULL, locale = NULL, format = NULL){
  if(!lubridate::is.Date(v)){
    v <- lubridate::as_date(v)
  }
  if(!is.null(format)){
    return(format_date(v, format, locale = locale))
  } else if (is.null(sample) && is.null(locale)){
    return(format_date(v, NULL))
  } else if(!is.null(sample)){
    # } else if(!is.null(sample) && is.null(locale)){
    locale <- locale %||% guess_date_locale(sample)
    date_fmt <- guess_date_fmt(sample, locale)
    out <- format_date(v, date_fmt, locale = locale)
    return(match_caps(out, sample))
  }
  date_fmt <- get_locale(locale)$date
  format_date(v, date_fmt = date_fmt, locale = locale)
}



format_date <- function(v, date_fmt, locale = NULL){
  original_locale <- locale
  date_fmt <- date_fmt %||% "%-m/%-d/%Y"
  fallback <- which_locale_sys_fallback(locale)
  locale <- gsub("-","_",fallback %||% locale)
  locale <- locale %||% "en_US"
  locale_attempt <- locale
  if(Sys.info()[['sysname']] == "Linux"){
    locale <- paste0(locale,".UTF-8")
  }
  {
    old_lc_time <- Sys.getlocale("LC_TIME")
    if (old_lc_time != locale) {
      on.exit({Sys.setlocale("LC_TIME", old_lc_time)})
      #Sys.setlocale("LC_TIME", locale)
      locale_attempt <- trySetLocale(locale)
    }
  }
  date_fmt <- d3date2lubridate(date_fmt)
  formatted <- format( v, format = date_fmt)
  fmttd <- gsub("###","",gsub("###0","",formatted))

  # If the locale is not available in the machine, try the EN default
  # and rename months
  if(gsub("\\.UTF-8","", locale_attempt) != gsub("\\.UTF-8","",locale) &&
     grepl("B|b", date_fmt)){
    lang <- substr(original_locale,1,5)
    type <- ifelse(grepl("B", date_fmt), "longMonths", "shortMonths")
    fmttd <- rename_months(fmttd, lang, type)
  }
  fmttd
}

trySetLocale <- function(locale){
  setloc <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_TIME", locale),error=function(e) e, warning=function(w) w))
  if(inherits(setloc, "warning"))
    setloc <- suppressWarnings(tryCatch(Sys.setlocale("LC_TIME", paste0(locale,".utf8")), silent = TRUE))
  locale_error_cmd <- paste0("sudo locale-gen ", locale,
                             " ; sudo locale-gen ",locale,"; sudo update-locale",
                             "\n or the following for all country variations:\n",
                             "sudo apt-get install language-pack-", substr(locale,1,2))
  if(inherits(setloc, "warning")) stop("Error in guess_date_fmt. ", setloc, "\n",
                                         "If you are on linux try running: \n",
                                         locale_error_cmd)
  Sys.getlocale("LC_TIME")
}


d3date2lubridate <- function(date_fmt, marker = '###'){
  date_fmt <- gsub("%-m",paste0(marker,"%m"),date_fmt)
  gsub("%-d",paste0(marker,"%d"),date_fmt)
}




#' @title Guess a certain date format
#'
#' @description guesses date format structure for a given sample
#'
#' @param sample a character value for which you want to "guess" the date format
#' @param locale character string naming a locale. For example: "es-MX" for mexico locale setting. You can check your default system locale with Sys.getlocale(). See available values for setting up at makeup::available_locales
#'
#' @examples
#'
#' ### Getting date format for Brazilian locale setting
#' sample <- "24 janeiro 2010"
#' guess_date_fmt(sample, locale = "pt-BR")
#'
#' ### Getting date format for Colombian locale setting
#' sample <- "24 de enero de 2010"
#' guess_date_fmt(sample, locale = "es-CO")
#'
#' @export
#' @importFrom dstools %||%
guess_date_fmt <- function(sample, locale = NULL){
  locale <- locale %||% guess_date_locale(sample)
  fallback <- which_locale_sys_fallback(locale)
  locale <- gsub("-","_",fallback %||% locale) %||% "en_US"
  format_orders <- c("ymd", "mdY", "dmy", "BdY", "Bdy","dBY","dbY", "bdY", "bdy",
                     "Bd","bd", "dB","db")
  fmts <- try(lubridate::guess_formats(sample, format_orders, locale = locale), silent = TRUE)
  if(inherits(fmts, "try-error")){
    fmts <- try(lubridate::guess_formats(sample, format_orders, locale = locale), silent = TRUE)
    fmts <- try(lubridate::guess_formats(sample, format_orders, locale = paste0(locale,".utf8")), silent = TRUE)

    locale_error_cmd <- paste0("sudo locale-gen ", locale,
                               " ; sudo locale-gen ",locale,".UTF-8 ; sudo update-locale",
                               "\n or the following for all country variations:\n",
                               "sudo apt-get install language-pack-", substr(locale,1,2)
    )
    if(inherits(fmts, "try-error")) stop("Error in guess_date_fmt. ", fmts, "\n",
                                         "If you are on linux try running: \n",
                                         locale_error_cmd)

  }
  fmts[1]
}

guess_date_locale <- function(sample){
  sample <- sample[!is.na(sample)]
  stopwords <- c("th","de")
  string <- gsub(paste0("[^a-zA-Z]|",paste0(stopwords,collapse = "|")),"", sample)
  if(all(unlist(lapply(string, dstools::is.empty)))) return()
  months <- locale_month_names
  months_match <- grepl(string, months$months, ignore.case = TRUE)
  # short_months_match <- grepl(string,months$shortMonths, ignore.case = TRUE)
  # months_match <- months_match | short_months_match
  guess <- months$locale[months_match]
  if(dstools::is.empty(guess)) return()
  guess[1]
}

rename_months <- function(fmttd, lang, type){
  months <- locale_month_names
  ms <- months |> dplyr::filter(.data$locale == lang)
  ms_en <- months |>  dplyr::filter(.data$locale == "en-US")
  x <- tibble::tibble(to = ms$months, from = ms_en$months)
  if(type == "shortMonths"){
    x$from <- ms_en$shortMonths
    x$to <- ms$shortMonths
  }
  x <- purrr::transpose(x)
  unlist(lapply(fmttd, function(fm){
    unlist(lapply(x, function(m){
      if(grepl(m$from, fm))
        return(gsub(m$from, m$to, fm))
    }))
  }))

}




makeup_dat_stamp <- function(v, format = NULL, locale = NULL){
  if(!lubridate::is.Date(v)) stop("must be a Date")
  locale <- locale %||% guess_date_locale(format)
  locale <- locale %||% "en-US"

  fallback <- which_locale_sys_fallback(locale)
  locale <- gsub("-","_",fallback %||% locale)

  stamp <- lubridate::stamp_date(format, locale = locale)
  stamp(v)
}

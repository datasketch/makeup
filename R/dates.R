
#' @export
makeup_dat <- function(v, sample = NULL, locale = NULL, format = NULL){
  if(!lubridate::is.Date(v)){
    v <- lubridate::as_date(v)
  }
  if(!is.null(format)){
    return(format_date(v, format))
  } else if (is.null(sample) && is.null(locale)){
    return(format_date(v, NULL))
  } else if(!is.null(sample) && is.null(locale)){
    locale <- guess_date_locale(sample)
    date_fmt <- guess_date_fmt(sample, locale)
    return(format_date(v, date_fmt, locale = locale))
  }
  date_fmt <- get_locale(locale)$date
  format_date(v, date_fmt = date_fmt, locale = locale)
}


format_date <- function(v, date_fmt, locale = NULL){
  date_fmt <- date_fmt %||% "%-m/%-d/%Y"
  fallback <- which_locale_sys_fallback(locale)
  locale <- gsub("-","_",fallback %||% locale)
  locale <- locale %||% "en_US"
  {
    old_lc_time <- Sys.getlocale("LC_TIME")
    if (old_lc_time != locale) {
      on.exit(Sys.setlocale("LC_TIME", old_lc_time))
      Sys.setlocale("LC_TIME", locale)
    }
  }
  date_fmt <- d3date2lubridate(date_fmt)
  formatted <- format( v, format = date_fmt)
  gsub("###","",gsub("###0","",formatted))
}

d3date2lubridate <- function(date_fmt, marker = '###'){
  date_fmt <- gsub("%-m",paste0(marker,"%m"),date_fmt)
  gsub("%-d",paste0(marker,"%d"),date_fmt)
}

guess_date_fmt <- function(sample, locale = NULL){
  fallback <- which_locale_sys_fallback(locale)
  locale <- gsub("-","_",fallback %||% locale) %||% "en_US"
  format_orders <- c("ymd", "mdY", "dmy", "BdY", "Bdy","dBY","dbY", "bdY", "bdy",
                     "Bd","bd", "dB","db")
  fmts <- lubridate::guess_formats(sample, format_orders, locale = locale)
  fmts[1]
}

guess_date_locale <- function(sample){
  stopwords <- c("th","de")
  string <- gsub(paste0("[^a-zA-Z]|",paste0(stopwords,collapse = "|")),"", sample)
  if(is.empty(string)) return()
  months <- makeup::locale_month_names
  guess <- months$locale[months$months %in% c(string, tolower(string))]
  if(is.empty(guess)) return()
  guess[1]
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


#' @export
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
  date_fmt <- date_fmt %||% "%-m/%-d/%Y"
  fallback <- which_locale_sys_fallback(locale)
  locale <- gsub("-","_",fallback %||% locale)
  locale <- locale %||% "en_US"
  if(Sys.info()[['sysname']] == "Linux"){
    locale <- paste0(locale,".UTF-8")
  }
  {
    old_lc_time <- Sys.getlocale("LC_TIME")
    if (old_lc_time != locale) {
      on.exit({Sys.setlocale("LC_TIME", old_lc_time)})
      #Sys.setlocale("LC_TIME", locale)
      trySetLocale(locale)
      Sys.getlocale("LC_TIME")
    }
  }
  date_fmt <- d3date2lubridate(date_fmt)
  formatted <- format( v, format = date_fmt)
  gsub("###","",gsub("###0","",formatted))
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
}


d3date2lubridate <- function(date_fmt, marker = '###'){
  date_fmt <- gsub("%-m",paste0(marker,"%m"),date_fmt)
  gsub("%-d",paste0(marker,"%d"),date_fmt)
}

#' @export
guess_date_fmt <- function(sample, locale = NULL){
  # message("guess_date_fmt")
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
  stopwords <- c("th","de")
  string <- gsub(paste0("[^a-zA-Z]|",paste0(stopwords,collapse = "|")),"", sample)
  if(is.empty(string)) return()
  months <- makeup:::locale_month_names
  months_match <- grepl(string,months$months, ignore.case = TRUE)
  # short_months_match <- grepl(string,months$shortMonths, ignore.case = TRUE)
  # months_match <- months_match | short_months_match
  guess <- months$locale[months_match]
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

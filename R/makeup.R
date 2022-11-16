
#' @title makeup
#'
#' @description Formats numbers, strings or dates values for human use
#'
#' @param v value to be formatted
#' @param sample human format to apply in v value
#' @param locale locale to use, for example "es-MX" for mexican. See posible values at makeup::available_locales
#' @param format ??
#' @param type kind of value to be formatted: ("num", "dat" or "chr")
#' @param suffix Character string to append after formatted value
#' @param prefix Character string to append before formatted value
#'
#' @return a formatted character value
#'
#' @examples
#' x <- c(1234.56, 432141, 0.12)
#'
#' makeup(x, sample = "1'432.1")
#' makeup(x, sample = "1,432.1")
#' makeup(x, sample = "10k")
#' makeup(x, sample = "10%")
#'
#' @export
makeup <- function(v, sample = NULL, format = NULL, locale = NULL,
                   type = NULL, suffix = "", prefix = ""){

  # assertthat::assert_that(!is.null(type), msg = "A type value ('dat' for Date, 'chr' for character or 'num' for number) must be indicated")

  if(is.null(type)){
    if(lubridate::is.Date(v)){
      type <- "dat"
    } else if(is.character(v)){
      type <- "chr"
    } else if(is.numeric(v)){
      type <- "num"
    }
  }

  if(type == "dat"){
    return(makeup_dat(v, sample = sample, locale = locale, format = format))
  }

  if(type == "chr"){
    return(makeup_chr(v, sample = sample, format = format))
  }

  if(type == "num"){
    return(makeup_num(v, sample = sample, locale = locale, format = format, suffix = suffix, prefix = prefix))
  }

}





#' @title makeup_format
#'
#' @description Creates a format sample to be applied to a value
#'
#' @param sample human format to apply in v value
#' @param locale locale to use, for example "es-MX" for mexican. See posible values at makeup::available_locales
#' @param format ??
#' @param type kind of value to be formatted: ("num", "dat" or "chr")
#' @param suffix Character string to append after formatted value
#' @param prefix Character string to append before formatted value
#'
#' @return a formatted character value
#'
#' @examples
#'
#'  f <- makeup_format(sample = "3 de abril 1900")
#'  f(as.Date("2020-04-28"))
#'  f <- makeup_format(sample = "abril 3 1900", type = "dat")
#'  f("2020-04-28")
#'
#' @return a function
#' @export
makeup_format <- function(sample = NULL, format = NULL, locale = NULL,
                          type = "", suffix = "", prefix = ""){
  function(x){
    makeup(x, type = type,
           sample = sample, format = format, locale = locale, suffix = suffix, prefix = prefix)
  }
}




#' @title makeup_format_js
#'
#' @description ??
#'
#' @param sample human format to apply in v value
#' @param locale locale to use, for example "es-MX" for mexican. See posible values at makeup::available_locales
#' @param prefix Character string to append before formatted value
#' @param suffix Character string to append after formatted value
#'
#' @return ??
#'
#' @export
makeup_format_js <- function(sample = NULL, locale = NULL, prefix = "", suffix = "") {

  params <- which_num_format(sample)
  locale <- locale %||% "en-US"
  ..format <- guess_specifier(locale)

  if (!is.null(params)) {
   ..format <- paste0(params$separators$n_decimal, ", '",
                      params$separators$decimal, "', '",
                      params$separators$thousands, "') + '")
  # ..format <-  paste0(params$separators$thousands,
  #                     params$separators$decimal,
  #                     params$separators$n_decimal, "f")
  #locale <- guess_locale(params$separators$decimal, params$separators$thousands)[1]
  }

 # f <- d3.format::d3_format_js(specifier = ..format,
 #                              prefix = prefix, suffix = suffix, locale = locale)
  f <-htmlwidgets::JS(
    paste0("function() {return '", prefix, "' + Highcharts.numberFormat(this.value, ", ..format, suffix, "'}")
    )
 f
}

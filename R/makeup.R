
#' @title makeup
#'
#' @description Formats numbers, strings or dates values for human use
#'
#' @param v value to be formatted
#' @param sample human format to apply in v value
#' @param locale locale to use, for example "es-MX" for mexican. See posible values at makeup::available_locales
#' @param format a character vector of date-time formats. Default is "%-m/%-d/%Y"
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




#' Create a JavaScript function for formatting numbers in Highcharts
#'
#' This function creates a JavaScript function that can be used to format numbers
#' in Highcharts with custom prefixes and suffixes, and optionally with SI prefixes
#' such as "k" for kilo and "M" for mega. The function uses the `which_num_format()`
#' and `guess_specifier()` functions to determine the appropriate formatting settings
#' based on a sample vector and locale settings.
#'
#' @param sample A sample vector with the same data type and range as the data to be plotted, to use as a reference for the formatting settings. If not specified, default formatting settings will be used.
#' @param locale A character string specifying the locale settings to use. If not specified, the default locale will be used.
#' @param prefix A character string to prepend to each formatted value.
#' @param suffix A character string to append to each formatted value.
#' @param si_prefix A logical value indicating whether to use SI prefixes such as "k" and "M" to format the values. If \code{TRUE}, the values will be formatted with SI prefixes. If \code{FALSE}, standard formatting will be used.
#'
#' @return A JavaScript function for formatting numbers in Highcharts.
#'
#' @importFrom htmlwidgets JS
#' @export
#'
#' @examples
#' makeup_format_js(prefix = "$", suffix = " USD")
#'
#' @family utility functions
makeup_format_js <- function(sample = NULL, locale = NULL, prefix = "", suffix = "", si_prefix = FALSE) {

  params <- which_num_format(sample)
  locale <- locale %||% "en-US"
  ..format <- guess_specifier(locale)

  if (si_prefix) {
    f <- JS(paste0("function() {
                    var ret,
                        numericSymbols = ['k', 'M', 'G', 'T', 'P', 'E'],
                        i = 6;
                    if(this.value >=1000) {
                        while (i-- && ret === undefined) {
                            multi = Math.pow(1000, i + 1);
                            if (this.value >= multi && numericSymbols[i] !== null) {
                                ret = (this.value / multi) + numericSymbols[i];
                            }
                        }
                    }
                    return '", prefix,"' + (ret ? ret : this.value) + '", suffix,"';
                }")
    )
  } else {
  if (!is.null(params)) {
   ..format <- paste0(params$separators$n_decimal, ", '",
                      params$separators$decimal, "', '",
                      params$separators$thousands, "') + '")
  }
  f <- htmlwidgets::JS(
    paste0("function() {return '", prefix, "' + Highcharts.numberFormat(this.value, ", ..format, suffix, "'}")
    )
  }
 f
}

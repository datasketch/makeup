
#' @export
makeup <- function(v, sample = NULL, format = NULL, locale = NULL,
                   type = c("num","dat","chr"), suffix = "", prefix = ""){
  if(lubridate::is.Date(v) || type == "dat"){
    return(makeup_dat(v, sample = sample, locale = locale, format = format))
  }
  if(is.character(v) || type == "chr"){
    return(makeup_chr(v, sample = sample, format = format))
  }
  if(is.numeric(v) || type == "num"){
    return(makeup_num(v, sample = sample, locale = locale, format = format, suffix = suffix, prefix = prefix))
  }

}

#' @export
makeup_format <- function(sample = NULL, format = NULL, locale = NULL,
                          type = "", suffix = "", prefix = ""){
  function(x){
    makeup(x, type = type,
           sample = sample, format = format, locale = locale, suffix = suffix, prefix = prefix)
  }
}


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

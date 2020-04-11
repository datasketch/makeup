
#' @export
makeup <- function(v, sample = NULL, format = NULL, locale = NULL,
                   type = c("num","dat","chr")){
  if(lubridate::is.Date(v) || type == "dat"){
    return(makeup_dat(v, sample = sample, locale = locale, format = format))
  }
  if(is.character(v) || type == "chr"){
    return(makeup_chr(v, sample = sample, format = format))
  }
  if(is.numeric(v) || type == "num"){
    return(makeup_num(v, sample = sample, locale = locale, format = format))
  }

}

#' @export
makeup_format <- function(sample = NULL, format = NULL, locale = NULL,
                          type = ""){
  function(x){
    makeup(x, type = type,
           sample = sample, format = format, locale = locale)
  }
}

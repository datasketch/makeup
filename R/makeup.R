

makeup <- function(v, sample = NULL, format = NULL, locale = NULL){
  if(lubridate::is.Date(v)){
    return(makeup_dat(v, sample = sample, locale = locale, format = format))
  }
  if(is.character(v)){
    return(makeup_chr(v, sample = sample, format = format))
  }
  if(is.numeric(v)){
    return(makeup_num(v, sample = sample, locale = locale, format = format))
  }

}

# makeup_locale <- function(){}


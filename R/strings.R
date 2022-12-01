
#' @title makeup_chr
#'
#' @description Format numbers for human use
#'
#' @param v value to be formatted
#' @param sample human format to apply in v value
#' @param format ??
#'
#' @return a formatted character value
#'
#' @examples
#'   makeup_chr("A NAME", sample = "hello")
#'
#'   makeup_chr("A four word sentence", sample = "Hello Hello")
#'   makeup_chr(c("A four", " word sentence"), sample = "Hello Hello")
#'
#'
#'
#' @export
makeup_chr <- function(v, sample = NULL, format = NULL){

  if(!is.null(format)){

    available_formats <- c("title","upper","lower","firstupper")

    if(!format %in% available_formats){
      stop("format must be one of: ", paste(available_formats, collapse = ", "))
    }

    fun <- paste0(to, format)
    return(do.call(fun, list(v)))
  }
  match_caps(v, sample)
}

match_caps <- function(str, sample){
  if(is.null(sample)) return(str)
  if(is_lower(sample)) return(tolower(str))
  if(is_upper(sample)) return(toupper(str))
  if(is_title(sample)) return(totitle(str))
  if(is_firstupper(sample)) return(tofirstupper(str))
}

is_lower <- function(str){
  str == tolower(str)
}

is_upper <- function(str){
  str == toupper(str)
}

is_title <- function(str){
  str == totitle(str)
}

is_firstupper <- function(str){
  str == tofirstupper(str)
}

# Borrowed from
# https://stackoverflow.com/questions/18509527/first-letter-to-upper-case
tofirstupper <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Borrowed from
# https://stackoverflow.com/questions/15776732/how-to-convert-a-vector-of-strings-to-title-case

totitle <- function(str){
  re_from <- "\\b([[:lower:]])([[:lower:]]+)"
  gsub(re_from, "\\U\\1\\L\\2" ,tolower(str), perl=TRUE)
}




#' @export
makeup_month <- function(x, locale = NULL, format = "wide"){
  if(lubridate::is.Date(x)){
    x <- lubridate::month(x)
  }
  if(!is.numeric(x)){
    stop("month must be a date or a number")
  }
  locale <- locale %||% "en"
  months <- cldrr::cldr_months(locale)
  ms <- unname(unlist(months[[format]]))
  ms <- factor(ms, ordered = TRUE, levels = ms)
  ms[x]
}

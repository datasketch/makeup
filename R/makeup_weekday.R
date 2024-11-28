

#' @export
makeup_weekday <- function(x, locale = NULL, format = "wide"){

  locale <- locale %||% "en"
  first_day <- cldrr::cldr_week_first_day(locale = locale)
  # week_start <- ifelse(first_day == "mon", 1, 7)
  week_start <- 7

  if(lubridate::is.Date(x)){
    x <- lubridate::wday(x, label = TRUE, abbr = FALSE,
                         week_start = week_start)
  }
  if(is.numeric(x)){
    x <- lubridate::wday(1:7, label = TRUE, abbr = TRUE,
                    week_start = week_start)
  }

  # if(!is.character(x)){
  #   stop("weekday must be a date or a number")
  # }

  weekdays <- cldrr::cldr_weekdays(locale)
  ws <- unname(unlist(weekdays[[format]]))
  ws <- factor(ws, ordered = TRUE, levels = ws)
  ws[x]
}

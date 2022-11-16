
#' @title makeup_num
#'
#' @description Formats numbers values for human use
#'
#' @param v value to be formatted
#' @param sample human format to apply in v value
#' @param locale locale to use, for example "es-MX" for mexican. See posible values at available_locales
#' @param format ??
#' @param prefix Character string to append before formatted value
#' @param suffix string to append after formatted value
#'
#' @return a formatted character value
#'
#' @examples
#'  makeup_num(c(0.1, 0.5), "100%")
#'
#' @export
makeup_num <- function(v, sample = NULL, locale = NULL, format = NULL, prefix = "", suffix = ""){

  params <- which_num_format(sample) %||% list(specifier = ",")

  if (!params$specifier %in% c(".0%", ".2s") & !is.null(params$separators)) {
    v <- round(v, params$separators$n_decimal)
  }

  if(is.character(locale)){
    locale <- get_locale(locale)[c("decimal", "thousands")]
  }

  locale <- utils::modifyList(params$separators %||% list(), locale %||% list())
  # locale <- locale[c("decimal", "thousands")]
  f <- d3.format::d3.format(params$specifier, locale = locale, prefix = prefix, suffix = suffix)
  f(v)
}

#' @title which_num_format
#' @description Recognizes decimal values and decimal / thopusands separators. Also adds a specifier
#'
#' @param str A number as a character value
#'
#' @return a list with four values: number of decimal values, decimal and thousands separator and specifier value.  The output has the following properties:
#'
#' $n_decimal
#'
#' $specifier
#'
#' $decimal
#'
#' $thousands
#'
#' @examples
#' which_num_format("2135.3")
#'
#' @export
which_num_format <- function(str){
  if(is.null(str)) return()
  separators <- number_separators(str)
  specifier <- ","
  if(is_pct(str)) specifier <- ".0%"
  if(is_si_num(str)) specifier <- ".2s"
  list(
    specifier = specifier,
    separators = separators
  )
}

is_pct <- function(str) grepl(".%$", str)

is_si_num <- function(str){
  si_suffixes <- paste0("yzafpn", "\u00B5", "mkMGTPEZY")

  grepl(pattern = paste0(".[", si_suffixes, "]$"),
        x = str)
}


number_separators <- function(str){
  seps <- gsub("[0-9]", "", str)
  seps_chr <- strsplit(seps, "")[[1]]
  num_blocks <- strsplit(str, "[^0-9]")[[1]]

  has_one_sep <- length(seps_chr) <= 1
  has_many_sep <- length(seps_chr) > 1
  has_decimal_or_at_most_two_blks <- length(num_blocks) <= 2
  all_blocks_size_at_most_3 <- all(length(num_blocks)<=3)

  n_possible_decimals <- ifelse(length(num_blocks) < 2, 0, nchar(rev(num_blocks)[1]))
  if (is.na(num_blocks[3])) n_possible_decimals <- 0

  if(has_one_sep && has_decimal_or_at_most_two_blks){
    thousands <- ""
    decimal <- seps_chr %||% ""
    n_possible_decimals <- ifelse(length(num_blocks) < 2, 0, nchar(rev(num_blocks)[1]))
  }
  if(has_many_sep || !all_blocks_size_at_most_3){
    thousands <- rev(seps_chr)[2]
    decimal <- rev(seps_chr)[1] # take las separator
  }
  list(n_decimal = n_possible_decimals, decimal = decimal, thousands = thousands)
}

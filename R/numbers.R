#' Format a numeric vector with custom prefixes and suffixes
#'
#' This function formats a numeric vector with custom prefixes and suffixes, and optionally with SI prefixes such as "k" for kilo and "M" for mega. The function uses the `scales` and `d3.format` packages to format the values with custom separators, number of decimal places, and locale settings.
#'
#' @param v A numeric vector to format.
#' @param sample A sample vector with the same data type and range as \code{v} to use as a reference for the formatting settings. If not specified, default formatting settings will be used.
#' @param locale A character string specifying the locale settings to use. If not specified, the default locale will be used.
#' @param prefix A character string to prepend to each formatted value.
#' @param suffix A character string to append to each formatted value.
#' @param si_prefix A logical value indicating whether to use SI prefixes such as "k" and "M" to format the values. If \code{TRUE}, the values will be formatted with SI prefixes. If \code{FALSE}, standard formatting will be used.
#' @param scale A numeric value indicating the scale factor to apply when using SI prefixes. For example, a value of 1000 would use "k" for values above 1000, "M" for values above 1,000,000, etc.
#'
#' @return A character vector of formatted values.
#'
#' @importFrom scales label_number cut_short_scale
#' @importFrom d3.format d3.format
#' @export
#'
#' @examples
#' makeup_num(1000000, sample = "1234", si_prefix = TRUE)
makeup_num <- function(v, sample = NULL, locale = NULL,
                       prefix = "", suffix = "",
                       si_prefix = FALSE, scale = 1){

  params <- which_num_format(sample) %||% list(specifier = ",")

  if (!params$specifier %in% c(".0%", ".2s") & !is.null(params$separators)) {
    v <- round(v, params$separators$n_decimal)
  }

  if (si_prefix) {
  ac <- NULL
  if (params$separators$n_decimal != 0) {
      ac <- as.numeric(paste0("0.", paste0(rep(0, params$separators$n_decimal - 1), collapse = ""), 1))
    }
  f <- label_number(si_prefix = si_prefix, accuracy = ac, scale = scale, scale_cut = cut_short_scale())
  } else {
  if(is.character(locale)){
    locale <- get_locale(locale)[c("decimal", "thousands")]
  }
  locale <- utils::modifyList(params$separators %||% list(), locale %||% list())
  f <- d3.format::d3.format(params$specifier, locale = locale, prefix = prefix, suffix = suffix)
  }
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

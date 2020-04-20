
#' @export
makeup_num <- function(v, sample = NULL, locale = NULL, format = NULL){
  params <- which_num_format(sample) %||% list(specifier = ",")
  if(is.character(locale)){
    locale <- get_locale(locale)[c("decimal", "thousands")]
  }
  locale <- modifyList(params$separators %||% list(), locale %||% list())
  # locale <- locale[c("decimal", "thousands")]
  f <- d3.format::d3.format(params$specifier, locale = locale)
  f(v)
}

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
  si_suffixes <- "yzafpnµmkMGTPEZY"
  grepl(paste0(".[",si_suffixes,"]$"), str)
}


number_separators <- function(str){

  seps <- gsub("[0-9]", "", str)
  seps_chr <- strsplit(seps, "")[[1]]
  num_blocks <- strsplit(str, "[^0-9]")[[1]]

  has_one_sep <- length(seps_chr) <= 1
  has_many_sep <- length(seps_chr) > 1
  has_decimal_or_at_most_two_blks <- length(num_blocks) <= 2
  all_blocks_size_at_most_3 <- all(length(num_blocks)<=3)
  n_possible_decimals <- ifelse(length(num_blocks) == 2, 0, nchar(rev(num_blocks)[1]))

  if(has_one_sep && has_decimal_or_at_most_two_blks){
    thousands <- ""
    decimal <- seps_chr %||% ""
  }
  if(has_many_sep || !all_blocks_size_at_most_3){
    thousands <- rev(seps_chr)[2]
    decimal <- rev(seps_chr)[1] # take las separator
  }
  list(n_decimal = n_possible_decimals, decimal = decimal, thousands = thousands)
}

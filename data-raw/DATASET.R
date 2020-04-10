
library(tidyverse)

locs_num <- list.files("data-raw/locale-numbers", full.names = TRUE)
loc_names_num <- basename(tools::file_path_sans_ext(locs_num))
locs_num <- lapply(locs_num, jsonlite::fromJSON)
names(locs_num) <- loc_names_num


locs_dtm <- list.files("data-raw/locale-dates", full.names = TRUE)
loc_names_dtm <- basename(tools::file_path_sans_ext(locs_dtm))
locs_dtm <- lapply(locs_dtm, jsonlite::fromJSON)
names(locs_dtm) <- loc_names_dtm

all(loc_names_dtm %in% loc_names_num)
loc_names_dtm[!loc_names_dtm %in% loc_names_num]

available_locales <- list(num = loc_names_num, dtm = loc_names_dtm)
locales <-  list(num = locs_num, dtm = locs_dtm)

locales <- modifyList(locs_num, locs_dtm)


locales_table <- locales %>%
  unlist(recursive = FALSE) %>%
  enframe() %>%
  separate(name, c("locale","variable"), sep = "\\.") %>%
  pivot_wider(names_from = "variable") %>%
  mutate(lang = substr(locale,1,2)) %>%
  select(lang, everything())

locales <- transpose(locales_table)
available_locales <- locales_table$locale
names(locales) <- locales_table$locale

fallbacks <- list(
  "ar-EG" = "ar-*",
  "pt-BR" = "pt-*",
  "es-MX" = "^(?!.*(?:ES|MX))(?=.*(?:es))",
  "fr-FR" = "fr-*",
  "zh-CN" = "zh-TW"
)


sys_fallbacks <- list(
  "ar-EG" = "ar-*",
  "es-ES" = "es-*",
  "de-DE" = "de-*",
  "fr-FR" = "fr-*"
)

locale_month_names <- locales_table %>%
  select(locale, months, shortMonths) %>%
  unnest(cols = c(months, shortMonths))


# grepl("^(?!.*(?:ES|MX))(?=.*(?:es))","es-CO", perl = TRUE)
# grepl("^(?!.*(?:ES|MX))(?=.*(?:es))","es-MX", perl = TRUE)
# grepl("^(?!.*(?:ES|MX))(?=.*(?:es))","es-ES", perl = TRUE)


usethis::use_data(available_locales, locales,
                  fallbacks, sys_fallbacks,
                  locale_month_names,
                  internal = TRUE, overwrite = TRUE)



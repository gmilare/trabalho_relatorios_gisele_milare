pacotes <-
  c("dplyr",
    "DT",
    "geobr",
    "ggplot2",
    "ggspatial",
    "knitr",
    "lubridate",
    "prettydoc",
    "purrr",
    "readr",
    "scales",
    "sf",
    "tidyr")
knitr::write_bib(pacotes,
                 "bib/packages.bib")

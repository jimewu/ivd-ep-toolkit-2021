# * load pkg
pkg_lst <- c(
    "readODS",
    "dplyr",
    "here"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * set path
setwd(
    paste(
        git = here(),
        category = "analytical_sensitivity_lob_classical",
        sep = "/"
    )
)

# * setting

setting_ep17 <- read_ods(
    "input.ods",
    sheet = "setting_ep17"
)

# * import
## case-specific: 不同樣品 & 不同的 data 日期

data_ep17lob_import <- read_ods(
    "input.ods",
    sheet = "data_ep17_lob"
)

# * data_tidy

## combine
data_ep17lob_tidy_combine <- data_ep17lob_import

## split by reagent_lot
data_ep17lob_tidy_split <- data_ep17lob_tidy_combine %>%
    split(.$reagent_lot)
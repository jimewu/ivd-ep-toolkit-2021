# * Load PKG
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

# * path setting

## 從git root設定路徑

setwd(
    paste(
        git = here(),
        category = "interference",
        sep = "/"
    )
)

# * setting_ep7

setting_ep7 <- read_ods(
    "input.ods",
    sheet = "setting_ep7"
)

# * import
## 讀取資料
data_ep7_import <- read_ods(
    "input.ods",
    sheet = "data_ep7"
)


# * tidy
## 依照interferent, interferent_level, analyte_level分組
data_ep7_tidy_combine <- data_ep7_import %>%
    mutate(
        condition = paste(
            interferent,
            ": ",
            interferent_level,
            " x analyte: ",
            analyte_level,
            sep = ""
        )
    )

data_ep7_tidy_split <- data_ep7_tidy_combine %>%
    split(.$condition)
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
        category = "analytical_sensitivity_lod_classical",
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

data_ep17lod_import <- read_ods(
    "input.ods",
    sheet = "data_ep17_lod"
)

# * data_tidy

## combine
data_ep17lod_tidy_combine <- data_ep17lod_import

## split by reagent_lot
data_ep17lod_tidy_split <- data_ep17lod_tidy_combine %>%
    # 加入分組依據1
    mutate(
        split = paste(
            "Regante Lot",
            reagent_lot
        )
    ) %>%
    split(.$split) %>%
    lapply(
        function(x) {
            result <- x %>%
                # 加入分組依據2
                mutate(
                    split = paste(
                        "Sample",
                        sample
                    )
                ) %>%
                split(.$split) %>%
                # 拿掉各組split欄位
                lapply(
                    function(y) {
                        y$split <- NULL

                        return(y)
                    }
                )

            return(result)
        }
    )
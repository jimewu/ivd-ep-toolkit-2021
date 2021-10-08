# * setting
## create empty lists
ep17lod_setting <- vector(mode = "list")
ep17lod_import <- vector(mode = "list")
ep17lod_tidy <- vector(mode = "list")
ep17lod_analysis <- vector(mode = "list")
ep17lod_report_fig <- vector(mode = "list")
ep17lod_report_tab <- vector(mode = "list")
ep17lod_report_crit <- vector(mode = "list")


## load pkg
ep17lod_setting[["deps"]] <- c(
    "readODS",
    "dplyr",
    "here",
    "flextable",
    "DT",
    "ggplot2"
)

lapply(
    ep17lod_setting[["deps"]],
    library,
    character.only = TRUE
)

## set path
ep17lod_setting[["path"]] <- paste(
    git = here(),
    category = "analytical_sensitivity_lod_classical",
    sep = "/"
) %>%
    setwd()

# * import
## setting
ep17lod_import[["setting"]] <- read_ods(
    "input.ods",
    sheet = "setting_ep17"
)

## data
### case-specific: 不同樣品 & 不同的 data 日期
ep17lod_import[["data"]] <- read_ods(
    "input.ods",
    sheet = "data_ep17_lod"
)

# * tidy
## combine
ep17lod_tidy[["combine"]] <- ep17lod_import[["data"]]

## split by reagent_lot
ep17lod_tidy[["split"]] <- ep17lod_tidy[["combine"]] %>%
    # 加入易讀的分組依據
    mutate(
        split = paste(
            "Regante Lot",
            reagent_lot
        )
    ) %>%
    split(.$split) %>%
    lapply(
        function(x) {
            x$split <- NULL

            return(x)
        }
    )

source("lib_ep17lod.r")
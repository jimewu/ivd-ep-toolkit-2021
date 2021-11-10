# * setting

## create empty lists

ep17lod_setting <- vector(mode = "list") # 相依套件 & 工作路徑
ep17lod_import <- vector(mode = "list") # 設定值 & 資料
ep17lod_tidy <- vector(mode = "list") # 合併 & 拆分
ep17lod_analysis <- vector(mode = "list") # 各種分析
ep17lod_report_fig <- vector(mode = "list") # 圖報告
ep17lod_report_tab <- vector(mode = "list") # 表報告
ep17lod_report_crit <- vector(mode = "list") # 關鍵數據

## load pkg

ep17lod_setting[["deps"]] <- c(
    "readODS",
    "dplyr",
    "here",
    "flextable",
    "gtsummary",
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
    category = "analytical_sensitivity_lod_probit",
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

ep17lod_tidy[["combine"]] <- ep17lod_import[["data"]] %>%
    filter(
        concentration != 0
    ) %>%
    mutate(
        log_conc = log10(concentration)
    )

## split by reagent_lot

ep17lod_tidy[["split"]] <- ep17lod_tidy[["combine"]] %>%
    # 加入易讀的分組依據
    mutate(
        split = paste(
            "Reagent Lot",
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
# * setting

## create empty lists

ep17lob_setting <- vector(mode = "list") # 相依套件 & 工作路徑
ep17lob_import <- vector(mode = "list") # 設定值 & 資料
ep17lob_tidy <- vector(mode = "list") # 合併 & 拆分
ep17lob_analysis <- vector(mode = "list") # 各種分析
ep17lob_report_fig <- vector(mode = "list") # 圖報告
ep17lob_report_tab <- vector(mode = "list") # 表報告
ep17lob_report_crit <- vector(mode = "list") # 關鍵數據


## load pkg

ep17lob_setting[["deps"]] <- c(
    "readODS",
    "dplyr",
    "here",
    "flextable",
    "gtsummary",
    "DT",
    "ggplot2"
)

lapply(
    ep17lob_setting[["deps"]],
    library,
    character.only = TRUE
)

## set path

ep17lob_setting[["path"]] <- paste(
    git = here(),
    category = "analytical_sensitivity_lob_classical",
    sep = "/"
) %>%
    setwd()

# * import

## setting

ep17lob_import[["setting"]] <- read_ods(
    "input.ods",
    sheet = "setting_ep17"
)

## data

### case-specific: 不同樣品 & 不同的 data 日期

ep17lob_import[["data"]] <- read_ods(
    "input.ods",
    sheet = "data_ep17_lob"
)

# * tidy

## combine

ep17lob_tidy[["combine"]] <- ep17lob_import[["data"]]

## split by reagent_lot

ep17lob_tidy[["split"]] <- ep17lob_tidy[["combine"]] %>%
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

source("lib_ep17.r")
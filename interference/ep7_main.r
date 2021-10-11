# * setting

## create empty lists

ep7_setting <- vector(mode = "list") # 相依套件 & 工作路徑
ep7_import <- vector(mode = "list") # 設定值 & 資料
ep7_tidy <- vector(mode = "list") # 合併 & 拆分
ep7_analysis <- vector(mode = "list") # 各種分析
# ep7_report_fig <- vector(mode = "list") # 圖報告
ep7_report_tab <- vector(mode = "list") # 表報告
ep7_report_crit <- vector(mode = "list") # 關鍵數據

## load pkg

ep7_setting[["deps"]] <- c(
    "readODS",
    "dplyr",
    "here",
    "flextable",
    "gtsummary",
    "DT",
    "ggplot2"
)

lapply(
    ep7_setting[["deps"]],
    library,
    character.only = TRUE
)

## set path

ep7_setting[["path"]] <- paste(
    git = here(),
    category = "interference",
    sep = "/"
) %>%
    setwd()

# * import

## setting

ep7_import[["setting"]] <- read_ods(
    "input.ods",
    sheet = "setting_ep7"
)

## data

### case-specific: 不同樣品 & 不同的 data 日期

ep7_import[["data"]] <- read_ods(
    "input.ods",
    sheet = "data_ep7"
)

# * tidy

## combine

ep7_tidy[["combine"]] <- ep7_import[["data"]] %>%
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

## split: 依照interferent, interferent_level, analyte_level分組

ep7_tidy[["split"]] <- ep7_tidy[["combine"]] %>%
    split(.$condition)


source("ep7_lib_replicate_calculator.r")
source("ep7_lib_paried_difference.r")
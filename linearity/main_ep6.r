# * setting

## create empty lists

ep6_setting <- vector(mode = "list") # 相依套件 & 工作路徑
ep6_import <- vector(mode = "list") # 設定值 & 資料
ep6_tidy <- vector(mode = "list") # 合併 & 拆分
ep6_analysis <- vector(mode = "list") # 各種分析
ep6_report_fig <- vector(mode = "list") # 圖報告
ep6_report_tab <- vector(mode = "list") # 表報告
ep6_report_crit <- vector(mode = "list") # 關鍵數據

## load pkg

ep6_setting[["deps"]] <- c(
    "readODS",
    "dplyr",
    "here",
    "flextable",
    "DT",
    "VCA",
    "ggplot2"
)

lapply(
    ep6_setting[["deps"]],
    library,
    character.only = TRUE
)

## set path

ep6_setting[["path"]] <- paste(
    git = here(),
    category = "linearity",
    sep = "/"
) %>%
    setwd()

# * import

## setting

ep6_import[["setting"]] <- read_ods(
    "input.ods",
    sheet = "setting_ep6"
)

## data

### case-specific: 不同樣品 & 不同的 data 日期

ep6_import[["data"]] <- read_ods(
    "input.ods",
    sheet = "data_ep6"
)

# * tidy

## combine

ep6_tidy[["combine"]] <- ep6_import[["data"]]

## split by sample

ep6_tidy[["split"]] <- ep6_tidy[["combine"]] %>%
    split(.$sample)

source("lib_ep6.r")
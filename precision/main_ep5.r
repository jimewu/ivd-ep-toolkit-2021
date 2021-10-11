# * setting

## create empty lists

ep5_setting <- vector(mode = "list") # 相依套件 & 工作路徑
ep5_import <- vector(mode = "list") # 設定值 & 資料
ep5_tidy <- vector(mode = "list") # 合併 & 拆分
ep5_analysis <- vector(mode = "list") # 各種分析
ep5_report_fig <- vector(mode = "list") # 圖報告
ep5_report_tab <- vector(mode = "list") # 表報告
ep5_report_crit <- vector(mode = "list") # 關鍵數據

## load pkg

ep5_setting[["deps"]] <- c(
    "readODS",
    "dplyr",
    "here",
    "flextable",
    "DT",
    "VCA",
    "ggplot2"
)

lapply(
    ep5_setting[["deps"]],
    library,
    character.only = TRUE
)

## set path

ep5_setting[["path"]] <- paste(
    git = here(),
    category = "precision",
    sep = "/"
) %>%
    setwd()

# * import

## setting

ep5_import[["setting"]] <- read_ods(
    "input.ods",
    sheet = "setting_ep5"
)

## data

### case-specific: 不同樣品 & 不同的 data 日期

ep5_import[["data"]] <- read_ods(
    "input.ods",
    sheet = "data_ep5"
)

# * tidy

## combine

ep5_tidy[["combine"]] <- ep5_import[["data"]] %>%
    group_by(sample) %>%
    # add y_lj as Levey-Jennings Scale
    mutate(
        y_lj = (y - mean(y)) / sd(y)
    ) %>%
    ungroup()

## split by sample

ep5_tidy[["split"]] <- ep5_tidy[["combine"]] %>%
    split(.$sample)

source("lib_ep5.r")
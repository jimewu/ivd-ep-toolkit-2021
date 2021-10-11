# * setting

## create empty lists

ep25_setting <- vector(mode = "list") # 相依套件 & 工作路徑
ep25_import <- vector(mode = "list") # 設定值 & 資料
ep25_tidy <- vector(mode = "list") # 合併 & 拆分
ep25_analysis <- vector(mode = "list") # 各種分析
ep25_report_fig <- vector(mode = "list") # 圖報告
ep25_report_tab <- vector(mode = "list") # 表報告
ep25_report_crit <- vector(mode = "list") # 關鍵數據

## load pkg

ep25_setting[["deps"]] <- c(
    "readODS",
    "dplyr",
    "here",
    "flextable",
    "gtsummary",
    "DT",
    "ggplot2"
)

lapply(
    ep25_setting[["deps"]],
    library,
    character.only = TRUE
)

## set path

ep25_setting[["path"]] <- paste(
    git = here(),
    category = "stability",
    sep = "/"
) %>%
    setwd()

# * import

## setting

ep25_import[["setting"]] <- read_ods(
    "input.ods",
    sheet = "setting_ep25"
)

## data

### case-specific: 不同樣品 & 不同的 data 日期

ep25_import[["data"]] <- read_ods(
    "input.ods",
    sheet = "data_ep25"
)

# * tidy

## 得到總表: 每個y都是每個sample的每個day中不同replicates的平均

ep25_tidy[["combine"]] <- ep25_import[["data"]] %>%
    group_by(
        sample,
        day
    ) %>%
    summarise(
        sample,
        day,
        y = mean(y)
    ) %>%
    # 表上加上允收上下限，以作圖
    summarise(
        day,
        y,
        limit_upr = y[1] * (
            100 + ep25_import[["setting"]]$allowable_drift
        ) / 100,
        limit_lwr = y[1] * (
            100 - ep25_import[["setting"]]$allowable_drift
        ) / 100
    ) %>%
    ungroup()

## 從總表切分成分析單位
ep25_tidy[["split"]] <- ep25_tidy[["combine"]] %>%
    split(.$sample)


## * for analysis and report
source("ep25_lib.r")
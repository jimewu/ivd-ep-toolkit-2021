# * setting

## create empty lists

ep9_setting <- vector(mode = "list") # 相依套件 & 工作路徑
ep9_import <- vector(mode = "list") # 設定值 & 資料
ep9_tidy <- vector(mode = "list") # 合併 & 拆分
ep9_analysis <- vector(mode = "list") # 各種分析
ep9_report_fig <- vector(mode = "list") # 圖報告
ep9_report_tab <- vector(mode = "list") # 表報告
ep9_report_crit <- vector(mode = "list") # 關鍵數據

## load pkg

ep9_setting[["deps"]] <- c(
    "readODS",
    "dplyr",
    "here",
    "flextable",
    "gtsummary",
    "DT",
    "ggplot2",
    "EnvStats",
    "mcr"
)

lapply(
    ep9_setting[["deps"]],
    library,
    character.only = TRUE
)

## set path

ep9_setting[["path"]] <- paste(
    git = here(),
    category = "method_comparison",
    sep = "/"
) %>%
    setwd()

# * import

## setting

ep9_import[["setting"]] <- read_ods(
    "input.ods",
    sheet = "setting_ep9"
)

## data

### case-specific: 不同樣品 & 不同的 data 日期

ep9_import[["data"]] <- read_ods(
    "input.ods",
    sheet = "data_ep9"
)

# * tidy

ep9_tidy[["combine"]] <-
    # 去除NA值
    na.omit(ep9_import[["data"]]) %>%
    # 依照y_ref升冪排序
    arrange(y_ref) %>%
    # 計算difference
    mutate(
        diff = y_test - y_ref,
        pctdiff = 100 * diff / y_ref
    ) %>%
    cbind(
        order = seq(
            1, length(.$y_ref)
        )
    )

source("ep9_lib.r")
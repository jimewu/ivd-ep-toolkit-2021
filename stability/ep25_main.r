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
        category = "stability",
        sep = "/"
    )
)

# * setting_ep25

setting_ep25 <- read_ods(
    "input.ods",
    sheet = "setting_ep25"
)

# * import
## 讀取資料
data_ep25_import <- read_ods(
    "input.ods",
    sheet = "data_ep25"
)


# * tidy
## 得到總表: 每個y都是每個sample的每個day中不同replicates的平均
data_ep25_tidy_combine <- data_ep25_import %>%
    group_by(sample, day) %>%
    summarise(
        sample,
        day,
        y = mean(y)
    ) %>%
    # 表上加上允收上下限，以作圖
    summarise(
        day,
        y,
        limit_upr = y[1] * (100 + setting_ep25$allowable_drift) / 100,
        limit_lwr = y[1] * (100 - setting_ep25$allowable_drift) / 100
    )

## 從總表切分成分析單位
data_ep25_tidy_split <- data_ep25_tidy_combine %>%
    split(.$sample)


## * for analysis and report
source("ep25_lib.r")
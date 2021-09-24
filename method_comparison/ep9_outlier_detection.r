# * Load PKG
pkg_lst <- c(
    "readODS",
    "dplyr",
    "here",
    "EnvStats",
    "DT"
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
        category = "method_comparison",
        sep = "/"
    )
)

# * setting_ep9

setting_ep9 <- read_ods(
    "input.ods",
    sheet = "setting_ep9"
)

# * import
## 讀取資料
data_ep9_import <- read_ods(
    "input.ods",
    sheet = "data_ep9"
)

# * tidy
## 去除NA值
data_ep9_tidy <- na.omit(data_ep9_import)

# * analysis: Rosner's Test for Outliers
data_ep9_analysis_rosner <- rosnerTest(
    data_ep9_tidy$y_test,
    k = floor(
        0.05 * length(data_ep9_tidy$y_test)
    )
)$all.stats

data_ep9_analysis_rosner <- data_ep9_analysis_rosner[
    , colnames(data_ep9_analysis_rosner) != "Obs.Num"
]

# * report_critical

data_ep9_report_crit_outlier <- data_ep9_analysis_rosner[
    data_ep9_analysis_rosner$Outlier == TRUE,
]$Value

# * report_tab

data_ep9_report_tab_rosner <- data_ep9_analysis_rosner

data_ep9_report_tab_rosner_dt <- datatable(
    data_ep9_report_tab_rosner,
    rownames = FALSE
)
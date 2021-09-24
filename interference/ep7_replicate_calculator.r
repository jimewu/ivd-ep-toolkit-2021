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
        category = "interference",
        sep = "/"
    )
)

# * setting_ep7

setting_ep7 <- read_ods(
    "input.ods",
    sheet = "setting_ep7"
)

# * analysis
## 計算在不假設candidate干擾物方向(不知為正干擾或負干擾)時所需的replicate
data_analysis_2side <- 2 * (
    (
        qnorm(1 - setting_ep7$alpha / 2) +
            qnorm(1 - setting_ep7$beta)
    ) *
        setting_ep7$repeatability / setting_ep7$allowable_interference
)^2

## 計算在candidate干擾物方向已知時所需的replicate
data_analysis_1side <- 2 * (
    (
        qnorm(1 - setting_ep7$alpha) +
            qnorm(1 - setting_ep7$beta)
    ) *
        setting_ep7$repeatability / setting_ep7$allowable_interference
)^2

# * report_critical
## replicate數值無條件進位為整數，且若計算值低於5則以5取代(CLSI EP07Ed3E要求)
data_report_critical_2side <- ifelse(
    data_analysis_2side >= 5,
    ceiling(data_analysis_2side),
    5
)

## replicate數值無條件進位為整數，且若計算值低於5則以5取代(CLSI EP07Ed3E要求)
data_report_critical_1side <- ifelse(
    data_analysis_1side >= 5,
    ceiling(data_analysis_2side),
    5
)
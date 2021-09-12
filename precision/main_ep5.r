# Load pkg
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

# ! TMP
setwd(
    paste(
        git = here(),
        category = "precision",
        sep = "/"
    )
)

# ! Data_Import
## ! case-specific: 不同樣品 & 不同的 data 日期

data_ep5_import <- list(
    lv1 = read_ods(
        "data.ods",
        sheet = "ep5.lv1.2021.0609"
    ),
    lv23 = read_ods(
        "data.ods",
        sheet = "ep5.lv23.2021.0524"
    )
)

# * data_tidy
## ! case-specific: 如果是標準格式，應該直接是combine版本然後拆成split
data_ep5_tidy_split <- list(
    lv1 = transmute(
        data_ep5_import[["lv1"]],
        Sample = "lv1",
        Factor1 = Day,
        Factor2 = Run,
        y = CONC.lv1
    ),
    lv2 = transmute(
        data_ep5_import[["lv23"]],
        Sample = "lv2",
        Factor1 = Day,
        Factor2 = Run,
        y = CONC.lv2
    ),
    lv3 = transmute(
        data_ep5_import[["lv23"]],
        Sample = "lv3",
        Factor1 = Day,
        Factor2 = Run,
        y = CONC.lv3
    )
)

data_ep5_tidy_combine <- rbind(
    data_ep5_tidy_split[["lv1"]],
    data_ep5_tidy_split[["lv2"]],
    data_ep5_tidy_split[["lv3"]]
)

source("lib_ep5_analysis_report.r")
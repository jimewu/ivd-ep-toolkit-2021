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

data_ep9_tidy <-
    # 去除NA值
    na.omit(data_ep9_import) %>%
    # 依照y_ref升冪排序
    arrange(y_ref) %>%
    # 計算difference
    mutate(
        diff = y_test - y_ref,
        pctdiff = 100 * diff / y_ref
    ) %>% cbind(
        order = seq(
            1, length(.$y_ref)
        )
    )
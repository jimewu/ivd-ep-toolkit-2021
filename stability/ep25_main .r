# * Load PKG
pkg_lst <- c(
    "readODS",
    "dplyr",
    "flextable",
    "ggplot2",
    "here"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * `data_ep25_import`

## * 從git root設定路徑

setwd(
    paste(
        git = here(),
        category = "stability",
        sep = "/"
    )
)

## * 讀取資料
data_ep25_import <- read_ods(
    "data.ods",
    sheet = "ep25.2021-0621.shelf"
)

data_ep25_import <- data_ep25_import %>%
    mutate(
        sample = paste(
            "Conc",
            Sample,
            "Dev_lot",
            Dev.Lot,
            sep = "_"
        )
    )

## `data_ep25_tidy_combine`

data_ep25_tidy_combine <- data.frame(
    sample = rep(
        data_ep25_import$sample,
        length(
            grep(
                pattern = "rep",
                colnames(data_ep25_import)
            )
        )
    ),
    day = rep(
        data_ep25_import$Day,
        length(
            grep(
                pattern = "rep",
                colnames(data_ep25_import)
            )
        )
    ),
    y = c(
        data_ep25_import$y.rep1,
        data_ep25_import$y.rep2,
        data_ep25_import$y.rep3,
        data_ep25_import$y.rep4,
        data_ep25_import$y.rep5
    )
)


## `data_ep25_tidy_split`

data_ep25_tidy_split <- data_ep25_tidy_combine %>%
    split(.$sample)

ggplot(
    data_ep25_tidy_combine,
    aes(
        x = day,
        y = y
    )
) +
    geom_point() +
    facet_wrap(
        ~sample,
        scales = "free_y"
    )
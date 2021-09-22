# * load pkg
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

# * set path
setwd(
    paste(
        git = here(),
        category = "precision",
        sep = "/"
    )
)

# * setting_import

setting_ep5 <- read_ods(
    "input.ods",
    sheet = "setting_ep5"
)

# * data_import
## case-specific: 不同樣品 & 不同的 data 日期

data_ep5_import <- read_ods(
    "input.ods",
    sheet = "data_ep5"
)

# * data_tidy

## combine
data_ep5_tidy_combine <- data_ep5_import

## split by sample
data_ep5_tidy_split <- data_ep5_tidy_combine %>%
    split(.$sample)

## convert y to Levey-Jennings Scale
data_ep5_tidy_split <- lapply(
    data_ep5_tidy_split,
    function(x) {
        result <- x %>%
            mutate(
                y_lj = (y - mean(x$y)) / sd(x$y)
            )

        return(result)
    }
)

## add y_lj back to data_combine
if (
    length(data_ep5_tidy_split) == 1
) {
    data_ep5_tidy_combine <- data_ep5_tidy_split[[1]]
} else if (
    length(data_ep5_tidy_split) > 1
) {
    data_ep5_tidy_combine <- data_ep5_tidy_split[[1]]

    for (
        x in seq(2, length(data_ep5_tidy_split))
    ) {
        data_ep5_tidy_combine <- rbind(
            data_ep5_tidy_combine,
            data_ep5_tidy_split[[x]]
        )
    }
}

source("lib_ep5.r")
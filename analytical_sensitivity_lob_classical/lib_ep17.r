source("main_ep17.r")

# * Load PKG
pkg_lst <- c(
    "dplyr",
    "flextable",
    "DT",
    "ggplot2"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * analysis
## 定義常態資料計算lob算法
get_lob_para <- function(data, sample.lot) {
    mean_blank <- mean(data)
    sd_blank <- sd(data)

    num_test <- length(data)

    # 先算c_blank分母(denominator)
    denom <- 1 - (4 * num_test - 4 * sample.lot)^(-1)

    c_blank <- qnorm(0.95) / denom

    lob <- mean_blank + c_blank * sd_blank

    result <- data.frame(
        mean = mean_blank,
        sd = sd_blank,
        lob = lob
    )

    return(result)
}

## 定義非常態資料計算lob算法
get_lob_nonpara <- function(data) {
    mean_blank <- mean(data)
    sd_blank <- sd(data)

    data <- sort(data)

    rank_position <- 0.5 + 0.95 * length(data)

    upr <- ceiling(rank_position)
    lwr <- floor(rank_position)

    lob <- ifelse(
        upr == lwr,
        data[rank_position],
        mean(
            data[lwr],
            data[upr]
        )
    )

    result <- data.frame(
        mean = mean_blank,
        sd = sd_blank,
        lob = lob
    )

    return(result)
}

# 分析中間值
imd <- vector(mode = "list")

# 資料是否為常態
imd[["spw-p"]] <- shapiro.test(data_ep17lob_tidy_combine$y)$p.value

# reagent_lot數量
imd[["reagent_lot"]] <- data_ep17lob_tidy_combine$reagent_lot %>%
    factor() %>%
    levels() %>%
    length()

# sample_lot數量
imd[["sample_lot"]] <- data_ep17lob_tidy_combine$sample_lot %>%
    factor() %>%
    levels() %>%
    length()


# 區分情況計算lob
if (
    # 條件1:資料為常態
    imd[["spw-p"]] >= 0.05 &
        ## 條件2: reagent_lot ≥4
        imd[["reagent_lot"]] >= 4
) {
    # 則不分批計算常態lob
    data_ep17lob_analysis_lob <- get_lob_para(
        data_ep17lob_tidy_combine$y,
        imd[["sample_lot"]]
    )

    # 紀錄狀況判斷結果
    imd[["crit"]] <- data.frame(
        normality = TRUE,
        by_lot = FALSE
    )
} else if (
    # 條件1:資料為非常態
    imd[["spw-p"]] < 0.05 &
        ## 條件2: reagent_lot ≥4
        imd[["reagent_lot"]] >= 4
) {
    # 則不分批計算非常態lob
    data_ep17lob_analysis_lob <- get_lob_nonpara(
        data_ep17lob_tidy_combine$y
    )

    # 紀錄狀況判斷結果
    imd[["crit"]] <- data.frame(
        normality = FALSE,
        by_lot = FALSE
    )
} else if (
    # 條件1:資料為常態
    imd[["spw-p"]] >= 0.05 &
        ## 條件2: reagent_lot ≤3
        imd[["reagent_lot"]] <= 3
) {
    # 則分批計算常態lob
    imd[["lot_lob"]] <- lapply(
        data_ep17lob_tidy_split,
        function(x) {
            sample_lot <- x$sample_lot %>%
                factor() %>%
                levels() %>%
                length()

            result <- get_lob_para(
                x$y,
                sample_lot
            )
        }
    )

    # 紀錄狀況判斷結果
    imd[["crit"]] <- data.frame(
        normality = TRUE,
        by_lot = TRUE
    )
} else if (
    # 條件1:資料為非常態
    imd[["spw-p"]] < 0.05 &
        ## 條件2: reagent_lot ≤3
        imd[["reagent_lot"]] <= 3
) {
    # 則分批計算非常態lob
    imd[["lot_lob"]] <- lapply(
        data_ep17lob_tidy_split,
        function(x) {
            result <- get_lob_nonpara(x$y)
        }
    )

    # 紀錄狀況判斷結果
    imd[["crit"]] <- data.frame(
        normality = FALSE,
        by_lot = TRUE
    )
}

if (
    # 如果有分批lob
    is.null(imd[["lot_lob"]]) == FALSE
) {
    # 把各批合併
    data_ep17lob_analysis_lob <- imd[["lot_lob"]][[1]]
    for (
        x in seq(
            2, length(imd[["lot_lob"]])
        )
    ) {
        data_ep17lob_analysis_lob <- rbind(
            data_ep17lob_analysis_lob,
            imd[["lot_lob"]][[x]]
        )
    }

    # 加上final lob
    data_ep17lob_analysis_lob <- data_ep17lob_analysis_lob %>%
        mutate(
            final_lob = max(lob)
        )
}

# * report_fig
data_ep5_report_fig_raw <- ggplot(
    data_ep5_tidy_combine,
    aes(
        x = factor1,
        y = y,
        color = factor(factor2)
    )
) +
    geom_point() +
    facet_wrap(
        facets = ~sample,
        scales = "free",
        ncol = 1
    ) +
    xlab(setting_ep5$name_of_factors[1]) +
    ylab(setting_ep5$unit_of_device[1]) +
    labs(
        color = setting_ep5$name_of_factors[2]
    ) +
    scale_x_continuous(
        breaks = seq(1, 20, by = 1)
    )

data_ep5_report_fig_lj <- ggplot(
    data_ep5_tidy_combine,
    aes(
        x = factor1,
        y = y_lj,
        color = factor(factor2)
    )
) +
    geom_point() +
    facet_wrap(
        facets = ~sample,
        scales = "free",
        ncol = 1
    ) +
    geom_hline(
        yintercept = 0,
        color = "black",
        alpha = 0.3
    ) +
    geom_hline(
        yintercept = c(
            1:3,
            (-1):(-3)
        ),
        color = "black",
        alpha = 0.3,
        linetype = 2
    ) +
    xlab(setting_ep5$name_of_factors[1]) +
    ylab("Levey-Jennings Scale") +
    labs(
        color = setting_ep5$name_of_factors[2]
    ) +
    scale_y_continuous(
        breaks = seq(-3, 3, by = 1)
    ) +
    scale_x_continuous(
        breaks = seq(1, 20, by = 1)
    )

# * report_tab
## 加入前置欄位以方便識別
data_ep5_report_tab_split <- lapply(
    seq(1, length(data_ep5_analysis)),
    function(x) {
        # 調整列的順序，total移到最下面
        data <- data_ep5_analysis[[x]][
            c(
                seq(2, nrow(data_ep5_analysis[[x]])),
                1
            ),
        ]

        cbind(
            Sample = names(data_ep5_analysis)[x],
            Item = c(
                setting_ep5$name_of_factors,
                "Error",
                "Total"
            ),
            round(
                data,
                digits = 3
            )
        )
    }
)

# * data_report_tab
if (
    length(data_ep5_report_tab_split) == 1
) {
    data_ep5_report_tab <- data_ep5_report_tab_split[[1]]
} else if (
    length(data_ep5_report_tab_split) > 1
) {
    data_ep5_report_tab <- data_ep5_report_tab_split[[1]]

    for (
        x in seq(2, length(data_ep5_report_tab_split))
    ) {
        data_ep5_report_tab <- rbind(
            data_ep5_report_tab,
            data_ep5_report_tab_split[[x]]
        )
    }
}

## 使用datatable產生動態表格
data_ep5_report_tab <- data_ep5_report_tab %>%
    datatable(
        rownames = FALSE
    )


# data_ep5_report_tab <- data_ep5_report_tab %>%
#     flextable() %>%
#     merge_v(j = "Sample") %>%
#     align(
#         align = "center",
#         part = "all"
#     ) %>%
#     footnote(
#         part = "header",
#         j = c(
#             "DF",
#             "SS",
#             "MS",
#             "VC"
#         ),
#         value = as_paragraph(
#             c(
#                 "Degrees of freedom",
#                 "sum of squares",
#                 "mean squares",
#                 "variance components"
#             )
#         ),
#         ref_symbols = c("a", "b", "c", "d"),
#         inline = TRUE,
#         sep = "; "
#     )
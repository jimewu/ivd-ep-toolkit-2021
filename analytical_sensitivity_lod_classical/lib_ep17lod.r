source("main_ep17lod.r")

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
        cp = c_blank,
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
        lwr = data[lwr],
        upr = data[upr],
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
data_ep17lob_report_fig_raw <- ggplot(
    data_ep17lob_tidy_combine,
    aes(y)
) +
    geom_histogram(bins = 30) +
    xlab(setting_ep17$unit_of_device)


data_ep17lob_report_fig_qq <- ggplot(
    data_ep17lob_tidy_combine,
    aes(
        sample = y
    )
) +
    stat_qq(
        alpha = 0.75
    ) +
    geom_qq_line(
        color = "red",
        linetype = 2
    ) +
    xlab("Normal Distribution (Levey-Jennings Scale)") +
    ylab(
        paste(
            "Sample",
            " (Shapiro-Wilk Normality Test p: ",
            round(
                shapiro.test(data_ep17lob_tidy_combine$y)$p.value,
                digits = 3
            ),
            ")",
            sep = ""
        )
    )

# * report_tab
data_ep17lob_report_tab <- data_ep17lob_analysis_lob %>%
    # 取到小數第3位
    round(digits = 3) %>%
    flextable() %>%
    merge_v() %>%
    set_header_labels(
        mean = "Mean",
        sd = "SD",
        lwr = "Rank Position: Lwr.",
        upr = "Rank Position: Upr.",
        lob = "LoB",
        final_lob = "Final LoB"
    ) %>%
    align(
        part = "header",
        align = "center"
    ) %>%
    align(
        part = "body",
        align = "center"
    )

if (
    is.null(data_ep17lob_analysis_lob$cp) == FALSE
) {
    data_ep17lob_report_tab <- data_ep17lob_report_tab %>%
        compose(
            part = "header",
            j = "cp",
            value = as_paragraph("C", as_sub("P")),
        ) %>%
        # 加上數值單位說明
        add_footer_lines(
            values = paste(
                "unit of Mean, SD, and LoB:",
                setting_ep17$unit_of_device[1],
                sep = " "
            )
        ) %>%
        align(
            part = "footer",
            align = "right"
        )
} else {
    data_ep17lob_report_tab <- data_ep17lob_report_tab %>%
        # 加上數值單位說明
        add_footer_lines(
            values = paste(
                "unit of values:",
                setting_ep17$unit_of_device[1],
                sep = " "
            )
        ) %>%
        align(
            part = "footer",
            align = "right"
        )
}
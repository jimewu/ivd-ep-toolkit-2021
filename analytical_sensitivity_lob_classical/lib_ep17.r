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

## 資料是否為常態

ep17lob_analysis[["spw.p"]] <- shapiro.test(
    ep17lob_tidy[["combine"]]$y
)$p.value

## 取出reagent_lot數量

ep17lob_analysis[["reagent_lot number"]] <-
    ep17lob_tidy[["combine"]]$reagent_lot %>%
    factor() %>%
    levels() %>%
    length()

## 取出sample_lot數量

ep17lob_analysis[["sample_lot number"]] <-
    ep17lob_tidy[["combine"]]$sample_lot %>%
    factor() %>%
    levels() %>%
    length()

## 計算lob

### parametric all in 1
ep17lob_analysis[["lob para 1"]] <- get_lob_para(
    ep17lob_tidy[["combine"]]$y,
    ep17lob_analysis[["sample_lot number"]]
)

### nonparametric all in 1
ep17lob_analysis[["lob nonpara 1"]] <- get_lob_nonpara(
    ep17lob_tidy[["combine"]]$y
)

### parametric lot
ep17lob_analysis[["lob para lot split"]] <- lapply(
    ep17lob_tidy[["split"]],
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

ep17lob_analysis[["lob para lot"]] <- ep17lob_analysis[["lob para lot split"]][[1]]

for (
    x in 2:length(ep17lob_analysis[["lob para lot split"]])
) {
    ep17lob_analysis[["lob para lot"]] <- rbind(
        ep17lob_analysis[["lob para lot"]],
        ep17lob_analysis[["lob para lot split"]][[x]]
    )
}

### nonparametric lot
ep17lob_analysis[["lob nonpara lot split"]] <- lapply(
    ep17lob_tidy[["split"]],
    function(x) {
        result <- get_lob_nonpara(x$y)
    }
)

ep17lob_analysis[["lob nonpara lot"]] <- ep17lob_analysis[["lob nonpara lot split"]][[1]]

for (
    x in 2:length(ep17lob_analysis[["lob nonpara lot split"]])
) {
    ep17lob_analysis[["lob nonpara lot"]] <- rbind(
        ep17lob_analysis[["lob nonpara lot"]],
        ep17lob_analysis[["lob nonpara lot split"]][[x]]
    )
}

# * report_fig

## raw data: 各測量值分佈
ep17lob_report_fig[["raw"]] <- ggplot(
    ep17lob_tidy[["combine"]],
    aes(
        x = factor(reagent_lot),
        y = y,
        color = factor(reagent_lot),
        shape = factor(sample_lot)
    )
) +
    geom_jitter() +
    xlab("Reagnet Lot") +
    ylab(
        paste(
            "Measured Value, ",
            ep17lob_import[["setting"]]$unit_of_device,
            sep = ""
        )
    ) +
    labs(
        shape = "Sample Lot",
        color = "Reagent Lot"
    )

ep17lob_report_fig[["raw hist"]] <- ggplot(
    ep17lob_tidy[["combine"]],
    aes(y)
) +
    geom_histogram(bins = 30) +
    xlab(ep17lob_import[["setting"]]$unit_of_device)

## qqplot

ep17lob_report_fig[["qq"]] <- ggplot(
    ep17lob_tidy[["combine"]],
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
                ep17lob_analysis[["spw.p"]],
                digits = 3
            ),
            ")",
            sep = ""
        )
    )

# * report_tab

ep17lob_report_tab[["raw"]] <- ep17lob_tidy[["combine"]] %>%
    datatable(
        options = list(scrollY = "450px"),
        colnames = c(
            "Sample Lot",
            "Reagent Lot",
            "測量值"
        )
    )


## 敘述統計: parametric

ep17lob_report_tab[["desc parametric"]] <-
    ep17lob_tidy[["combine"]] %>%
    tbl_summary(
        by = "reagent_lot",
        label = list(
            reagent_lot ~ "Reagent Lot",
            sample_lot ~ "Sample Lot",
            y ~ "Measurements"
        ),
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} / {N} ({p}%)"
        ),
        digits = all_continuous() ~ 2,
    ) %>%
    modify_header(label = "**Reagent Lot**") %>%
    bold_labels()

## 敘述統計: non-parametric

ep17lob_report_tab[["desc non-parametric"]] <-
    ep17lob_tidy[["combine"]] %>%
    tbl_summary(
        by = "reagent_lot",
        label = list(
            reagent_lot ~ "Reagent Lot",
            sample_lot ~ "Sample Lot",
            y ~ "Measurements"
        ),
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} / {N} ({p}%)"
        ),
        digits = all_continuous() ~ 2,
    ) %>%
    modify_header(label = "**Reagent Lot**") %>%
    bold_labels()

## lob分析結果

ep17lob_report_tab[["lob para 1"]] <- ep17lob_analysis[["lob para 1"]] %>%
    # 取到小數第3位
    round(digits = 3) %>%
    flextable() %>%
    merge_v() %>%
    set_header_labels(
        mean = "Mean",
        sd = "SD",
        lob = "LoB"
    ) %>%
    align(
        part = "header",
        align = "center"
    ) %>%
    align(
        part = "body",
        align = "center"
    ) %>%
    compose(
        part = "header",
        j = "cp",
        value = as_paragraph("C", as_sub("P")),
    ) %>%
    # 加上數值單位說明
    add_footer_lines(
        values = paste(
            "unit of Mean, SD, and LoB:",
            ep17lob_import[["setting"]]$unit_of_device[1],
            sep = " "
        )
    ) %>%
    align(
        part = "footer",
        align = "right"
    )

ep17lob_report_tab[["lob para lot"]] <- ep17lob_analysis[["lob para lot"]] %>%
    # 取到小數第3位
    round(digits = 3) %>%
    flextable() %>%
    merge_v() %>%
    set_header_labels(
        mean = "Mean",
        sd = "SD",
        lob = "LoB"
    ) %>%
    align(
        part = "header",
        align = "center"
    ) %>%
    align(
        part = "body",
        align = "center"
    ) %>%
    compose(
        part = "header",
        j = "cp",
        value = as_paragraph("C", as_sub("P")),
    ) %>%
    # 加上數值單位說明
    add_footer_lines(
        values = paste(
            "unit of Mean, SD, and LoB:",
            ep17lob_import[["setting"]]$unit_of_device[1],
            sep = " "
        )
    ) %>%
    align(
        part = "footer",
        align = "right"
    )

ep17lob_report_tab[["lob nonpara 1"]] <- ep17lob_analysis[["lob nonpara 1"]] %>%
    # 取到小數第3位
    round(digits = 3) %>%
    flextable() %>%
    merge_v() %>%
    set_header_labels(
        mean = "Mean",
        sd = "SD",
        lwr = "Lwr.",
        upr = "Upr.",
        lob = "LoB"
    ) %>%
    align(
        part = "header",
        align = "center"
    ) %>%
    align(
        part = "body",
        align = "center"
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of values:",
            ep17lob_import[["setting"]]$unit_of_device[1],
            sep = " "
        )
    ) %>%
    align(
        part = "footer",
        align = "right"
    )

ep17lob_report_tab[["lob nonpara lot"]] <- ep17lob_analysis[["lob nonpara lot"]] %>%
    # 取到小數第3位
    round(digits = 3) %>%
    flextable() %>%
    merge_v() %>%
    set_header_labels(
        mean = "Mean",
        sd = "SD",
        lwr = "Lwr.",
        upr = "Upr.",
        lob = "LoB"
    ) %>%
    align(
        part = "header",
        align = "center"
    ) %>%
    align(
        part = "body",
        align = "center"
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of values:",
            ep17lob_import[["setting"]]$unit_of_device[1],
            sep = " "
        )
    ) %>%
    align(
        part = "footer",
        align = "right"
    )
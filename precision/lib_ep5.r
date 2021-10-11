# * analysis

## Nested ANOVA

ep5_analysis[["anova"]] <- lapply(
    ep5_tidy[["split"]],
    function(x) {
        # 計算factor數量(不包含replicate)
        colindex_factor <- colnames(x) %>%
            grep("factor", .)

        num_factor <- length(colindex_factor)

        x <- as.data.frame(x)

        if (num_factor == 2) {
            anova <- anovaVCA(
                y ~ factor1 / factor2,
                x
            )
        } else if (num_factor == 3) {
            anova <- anovaVCA(
                y ~ factor1 / factor2 / factor3,
                x
            )
        } else if (num_factor == 4) {
            anova <- anovaVCA(
                y ~ factor1 / factor2 / factor3 / factor4,
                x
            )
        } else if (num_factor == 5) {
            anova <- anovaVCA(
                y ~ factor1 / factor2 / factor3 / factor4 / factor5,
                x
            )
        }

        anova <- anova %>%
            as.matrix() %>%
            as.data.frame()
    }
)

# * report_fig

## raw data

ep5_report_fig[["raw"]] <- ggplot(
    ep5_tidy[["combine"]],
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
    xlab(ep5_import[["setting"]]$name_of_factors[1]) +
    ylab(ep5_import[["setting"]]$unit_of_device[1]) +
    labs(
        color = ep5_import[["setting"]]$name_of_factors[2]
    ) +
    scale_x_continuous(
        breaks = seq(1, 20, by = 1)
    )

ep5_report_fig[["levey-jennings"]] <- ggplot(
    ep5_tidy[["combine"]],
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
    xlab(ep5_import[["setting"]]$name_of_factors[1]) +
    ylab("Levey-Jennings Scale") +
    labs(
        color = ep5_import[["setting"]]$name_of_factors[2]
    ) +
    scale_y_continuous(
        breaks = seq(-3, 3, by = 1)
    ) +
    scale_x_continuous(
        breaks = seq(1, 20, by = 1)
    )

# * report_tab

## 各個sample的ANOVA結果

ep5_report_tab[["anova split"]] <- lapply(
    seq(
        1, length(ep5_analysis[["anova"]])
    ),
    function(x) {
        # 調整列的順序，total移到最下面
        data <- ep5_analysis[["anova"]][[x]][
            c(
                seq(
                    2, nrow(ep5_analysis[["anova"]][[x]])
                ),
                1
            ),
        ]
        # 增加欄位以增加易讀性
        result <- cbind(
            Sample = names(ep5_analysis[["anova"]])[x],
            Item = c(
                ep5_import[["setting"]]$name_of_factors,
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

## 合併

if (
    length(ep5_report_tab[["anova split"]]) == 1
) {
    ep5_report_tab[["anova combine"]] <- ep5_report_tab[["anova split"]][[1]]
} else if (
    length(ep5_report_tab[["anova split"]]) > 1
) {
    ep5_report_tab[["anova combine"]] <- ep5_report_tab[["anova split"]][[1]]

    for (
        x in seq(
            2, length(ep5_report_tab[["anova split"]])
        )
    ) {
        ep5_report_tab[["anova combine"]] <- rbind(
            ep5_report_tab[["anova combine"]],
            ep5_report_tab[["anova split"]][[x]]
        )
    }
}

## 使用datatable產生動態表格
ep5_report_tab[["anova combine"]] <- ep5_report_tab[["anova combine"]] %>%
    datatable(
        rownames = FALSE
    )
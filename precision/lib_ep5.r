# * Load PKG
pkg_lst <- c(
    "dplyr",
    "flextable",
    "DT",
    "VCA",
    "ggplot2"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * analysis
## Nested ANOVA
data_ep5_analysis <- lapply(
    data_ep5_tidy_split,
    function(x) {
        colindex_factor <- colnames(x) %>%
            grep("factor", .)

        num_factor <- length(colindex_factor)

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

# * Load PKG
pkg_lst <- c(
    "dplyr",
    "flextable",
    "VCA",
    "ggplot2"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * setting_import

setting_ep5_import <- read_ods(
    "setting_ep5.ods",
    sheet = "main"
)

# * data_report_fig

data_ep5_report_fig <- ggplot(
    data_ep5_tidy_combine,
    aes(
        x = Factor1,
        y = y,
        color = factor(Factor2)
    )
) +
    geom_point() +
    facet_wrap(
        facets = ~Sample,
        scales = "free",
        ncol = 1
    ) +
    xlab(setting_ep5_import$name_of_factors[1]) +
    ylab(setting_ep5_import$unit_of_device[1]) +
    labs(
        color = setting_ep5_import$name_of_factors[2]
    )


# * data_analysis: Nested ANOVA

data_ep5_analysis <- lapply(
    data_ep5_tidy_split,
    function(x) {
        colindex_factor <- colnames(x) %>%
            grep("Factor", .)

        num_factor <- length(colindex_factor)

        if (num_factor == 2) {
            anova <- anovaVCA(
                y ~ Factor1 / Factor2,
                x
            )
        } else if (num_factor == 3) {
            anova <- anovaVCA(
                y ~ Factor1 / Factor2 / Factor3,
                x
            )
        } else if (num_factor == 4) {
            anova <- anovaVCA(
                y ~ Factor1 / Factor2 / Factor3 / Factor4,
                x
            )
        } else if (num_factor == 5) {
            anova <- anovaVCA(
                y ~ Factor1 / Factor2 / Factor3 / Factor4 / Factor5,
                x
            )
        }

        anova <- anova %>%
            as.matrix() %>%
            as.data.frame()
    }
)

# * data_report_tab_split

## 加入前置欄位以方便識別
data_ep5_report_tab_split <- lapply(
    seq(1, length(data_ep5_analysis)),
    function(x) {
        cbind(
            Sample = names(data_ep5_analysis)[x],
            Item = c(
                "Total",
                setting_ep5_import$name_of_factors
            ),
            round(
                data_ep5_analysis[[x]],
                digits = 3
            )
        )
    }
)

# * data_report_tab

data_ep5_report_tab <- data_ep5_report_tab_split[[1]]

for (
    x in seq(2, length(data_ep5_report_tab_split))
) {
    data_ep5_report_tab <- rbind(
        data_ep5_report_tab,
        data_ep5_report_tab_split[[x]]
    )
}

data_ep5_report_tab <- data_ep5_report_tab %>%
    flextable() %>%
    merge_v(j = "Sample") %>%
    align(
        align = "center",
        part = "all"
    ) %>%
    footnote(
        part = "header",
        j = c(
            "DF",
            "SS",
            "MS",
            "VC"
        ),
        value = as_paragraph(
            c(
                "Degrees of freedom",
                "sum of squares",
                "mean squares",
                "variance components"
            )
        ),
        ref_symbols = c("a", "b", "c", "d"),
        inline = TRUE,
        sep = "; "
    )
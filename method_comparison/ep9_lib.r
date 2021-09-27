source("ep9_main.r")

# ! tmp
# * Load PKG
pkg_lst <- c(
    "dplyr",
    "DT",
    "flextable",
    "ggplot2",
    "EnvStats"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * analysis
## Rosner's Test for Outliers based on Diff
data_ep9_analysis_rosner_diff <- rosnerTest(
    data_ep9_tidy$diff,
    k = floor(
        0.05 * length(data_ep9_tidy$y_test)
    )
)$all.stats

data_ep9_analysis_rosner_diff <- data_ep9_analysis_rosner_diff[
    , colnames(data_ep9_analysis_rosner_diff) != "Obs.Num"
]

## Rosner's Test for Outliers based on PctDiff
data_ep9_analysis_rosner_pctdiff <- rosnerTest(
    data_ep9_tidy$pctdiff,
    k = floor(
        0.05 * length(data_ep9_tidy$y_test)
    )
)$all.stats

data_ep9_analysis_rosner_pctdiff <- data_ep9_analysis_rosner_pctdiff[
    , colnames(data_ep9_analysis_rosner_pctdiff) != "Obs.Num"
]

# * report_critical
## Candidate Outliers based on Diff
data_ep9_report_crit_outlier_diff <- data_ep9_analysis_rosner_diff[
    data_ep9_analysis_rosner_diff$Outlier == TRUE,
]$Value

## Candidate Outliers based on PctDiff
data_ep9_report_crit_outlier_diff <- data_ep9_analysis_rosner_diff[
    data_ep9_analysis_rosner_diff$Outlier == TRUE,
]$Value


# * report_tab
data_ep9_report_tab_rosner_pctdiff <- data_ep9_analysis_rosner_diff

data_ep9_report_tab_rosner_pctdiff_dt <- datatable(
    data_ep9_report_tab_rosner_pctdiff,
    rownames = FALSE
)


# * report_fig
## 原始結果圖
data_ep9_report_fig_raw <- ggplot(
    data_ep9_tidy,
    aes(
        x = y_ref,
        y = y_test
    )
) +
    geom_point() +
    xlab(setting_ep9$name_of_ref) +
    ylab(setting_ep9$name_of_test)

## difference plot
data_ep9_report_fig_diff <- ggplot(
    data_ep9_tidy,
    aes(
        x = y_ref,
        y = diff
    )
) +
    geom_point() +
    xlab(setting_ep9$name_of_ref) +
    ylab(paste(
        "Difference (",
        setting_ep9$unit_of_device,
        ")",
        sep = ""
    )) +
    geom_hline(
        yintercept = 0,
        alpha = 0.5
    ) +
    geom_hline(
        yintercept = sd(data_ep9_tidy$diff) * c(
            -3, -2, -1, 1, 2, 3
        ),
        alpha = 0.5,
        linetype = 2
    )

## %difference plot
data_ep9_report_fig_pctdiff <- ggplot(
    data_ep9_tidy,
    aes(
        x = y_ref,
        y = pctdiff
    )
) +
    geom_point() +
    xlab(setting_ep9$name_of_ref) +
    ylab("Difference (%)") +
    geom_hline(
        yintercept = 0,
        alpha = 0.5
    ) +
    geom_hline(
        yintercept = sd(data_ep9_tidy$pctdiff) * c(
            -3, -2, -1, 1, 2, 3
        ),
        alpha = 0.5,
        linetype = 2
    )
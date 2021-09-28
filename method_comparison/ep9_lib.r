source("ep9_main.r")

# ! tmp
# * Load PKG
pkg_lst <- c(
    "dplyr",
    "DT",
    "flextable",
    "ggplot2",
    "EnvStats",
    "mcr"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * analysis
## Rosner's Test for Outliers
data_ep9_analysis_rosner <- lapply(
    list(
        diff = "diff",
        pctdiff = "pctdiff"
    ),
    function(x) {
        result <- rosnerTest(
            data_ep9_tidy[, x],
            k = floor(
                0.05 * length(data_ep9_tidy$y_test)
            )
        )$all.stats

        result <- result[
            , colnames(result) != "Obs.Num"
        ]

        return(result)
    }
)

data_ep9_analysis_mcreg <- lapply(
    list(
        LinReg = data.frame(
            method.reg = "LinReg",
            method.ci = "bootstrap"
        ),
        Deming = data.frame(
            method.reg = "Deming",
            method.ci = "bootstrap"
        ),
        WLinReg = data.frame(
            method.reg = "WLinReg",
            method.ci = "bootstrap"
        ),
        WDeming = data.frame(
            method.reg = "WDeming",
            method.ci = "bootstrap"
        ),
        PaBa = data.frame(
            method.reg = "PaBa",
            method.ci = "bootstrap"
        )
    ),
    function(x) {
        result <- vector(mode = "list")

        result[["raw"]] <- mcreg(
            x = data_ep9_tidy$y_ref,
            y = data_ep9_tidy$y_test,
            mref.name = setting_ep9$name_of_ref,
            mtest.name = setting_ep9$name_of_test,
            error.ratio = setting_ep9$repeatability / setting_ep9$repeatability_ref,
            alpha = 0.05,
            method.reg = x$method.reg,
            method.ci = x$method.ci
        )

        result[["coef"]] <- result[["raw"]] %>%
            MCResult.getCoefficients() %>%
            as.data.frame() %>%
            cbind(
                .,
                ideal = c(0, 1)
            ) %>%
            mutate(
                is.ideal = ifelse(
                    (LCI - ideal) * (UCI - ideal) < 0,
                    TRUE,
                    FALSE
                ),
                ci.range = UCI - LCI
            )

        return(result)
    }
)

# * report_critical
## Candidate Outliers based on Diff
data_ep9_report_crit_outlier <- lapply(
    data_ep9_analysis_rosner,
    function(x) {
        result <- x[
            x$Outlier == TRUE,
        ]$Value

        # 如果沒有outlier則回傳NA
        if (
            length(result) == 0
        ) {
            return(NA)
        } else {
            return(result)
        }
    }
)



# * report_tab
data_ep9_report_tab_rosner <- lapply(
    data_ep9_analysis_rosner,
    function(x) {
        # 對數值欄取到小數第三位
        for (
            i in seq(
                1, ncol(x)
            )
        ) {
            if (class(x[, i]) == "numeric") {
                x[, i] <- round(
                    x[, i],
                    digits = 3
                )
            }
        }

        return(x)
    }
)

# * report_tab_dt
data_ep9_report_tab_rosner_dt <- lapply(
    data_ep9_report_tab_rosner,
    function(x) {
        result <- datatable(
            x,
            rownames = FALSE
        )

        return(result)
    }
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
    rbind(
        diff = data.frame(
            type_x = "Measurement Value",
            x = data_ep9_tidy$y_ref,
            y = data_ep9_tidy$diff
        ),
        odr_diff = data.frame(
            type_x = "Ordered Measurement",
            x = data_ep9_tidy$order,
            y = data_ep9_tidy$diff
        )
    ),
    aes(
        x = x,
        y = y
    )
) +
    geom_point() +
    geom_hline(
        yintercept = 0,
        alpha = 0.5
    ) +
    facet_wrap(
        ~type_x,
        scales = "free"
    )

data_ep9_report_fig_pctdiff <- ggplot(
    rbind(
        pctdiff = data.frame(
            type_x = "Measurement Value",
            x = data_ep9_tidy$y_ref,
            y = data_ep9_tidy$pctdiff
        ),
        odr_pctdiff = data.frame(
            type_x = "Ordered Measurement",
            x = data_ep9_tidy$order,
            y = data_ep9_tidy$pctdiff
        )
    ),
    aes(
        x = x,
        y = y
    )
) +
    geom_point() +
    geom_hline(
        yintercept = 0,
        alpha = 0.5
    ) +
    facet_wrap(
        ~type_x,
        scales = "free"
    )





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

# compareFit
data_report_fig_comparefit_fun <-
    function(x = data_ep9_analysis_mcreg) {
        compareFit(
            x[["PaBa"]][["raw"]],
            x[["WDeming"]][["raw"]],
            x[["WLinReg"]][["raw"]],
            x[["Deming"]][["raw"]],
            x[["LinReg"]][["raw"]]
        )
    }
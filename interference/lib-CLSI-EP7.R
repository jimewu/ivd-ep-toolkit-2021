
# Load PKG
pkg_lst <- c(
    "knitr",
    "readODS",
    "dplyr",
    "flextable",
    "here"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

source("lib_ft.R")

# # ! TMP
# setwd(
#     paste(
#         git = here(),
#         category = "A_醫療器材/醫材案件-縱向產品/Z-跨科/Z.IVD",
#         case = "CS1_Leadgene_Indoxyl_Sulfate_ELISA",
#         task = "OUT3.0_Preclinical_Study_Report/RMarkdown",
#         sep = "/"
#     )
# )

# # ! params

# params <- data.frame(
#     dev_unit3 = "μg/ml"
# )

# # ! Import Data
# data_import <- read_ods(
#     "data.ods",
#     sheet = "ep7.2021.0217_del"
# )

# Data Tidy
## Split by Interferent and Sample.Conc
data_import <- data_import %>%
    mutate(
        tmp_split = paste(
            Interferent,
            Sample.Conc,
            sep = " x "
        )
    )

data_splt <- data_import %>%
    split(.$tmp_split)

### Remove tmp col
data_import$tmp_split <- NULL

data_splt <- lapply(
    data_splt,
    function(x) {
        x$tmp_split <- NULL

        return(x)
    }
)

## split後的表格即完成整理，可以進行分析
data_tidy <- data_splt

# Data Analysis: find % mean difference
data_analyze <- lapply(
    data_tidy,
    function(x) {
        mean_test <- mean(x$y.test)
        sd_test <- sd(x$y.test)

        mean_control <- mean(x$y.control)
        sd_control <- sd(x$y.control)

        perc_diff <- 100 * (x$y.test - x$y.control) / x$y.control
        mean_perc_diff <- mean(perc_diff)
        sd_perc_diff <- sd(perc_diff)

        # Calculate 95% CI of perc.diff
        ## reference: https://www.programmingr.com/statistics/confidence-interval-in-r/
        ## sample size
        num <- length(perc_diff)

        ## error
        ### 計算95%信心區間error的公式
        error <- qnorm(0.975) * sd_perc_diff / sqrt(num)

        mean_perc_diff_upr <- mean_perc_diff + error
        mean_perc_diff_lwr <- mean_perc_diff - error

        # 計算t-test p-value，預設paired=FALSE
        p_value <- t.test(x$y.test, x$y.control)$p.value

        result <- data.frame(
            mean_test,
            sd_test,
            mean_control,
            sd_control,
            mean_perc_diff,
            mean_perc_diff_upr,
            mean_perc_diff_lwr,
            p_value
        )

        return(result)
    }
)

# Report Compilation
## Reference: CLSI EP7Ed3E Table 3A-B
data_report <- lapply(
    seq(1, length(data_tidy)),
    function(x) {

        # 依照Table 3左半邊的樣子取出原始數據
        data_raw <- data.frame(
            Control = data_tidy[[x]]$y.control,
            Test = data_tidy[[x]]$y.test
        )
        ## 依照Table 3的樣子讓原始數據橫躺
        data_raw <- t(data_raw)

        ## 以第N個replicate的N作為欄位名稱
        colnames(data_raw) <- seq(
            1, ncol(data_raw)
        )

        mean_control <- data_analyze[[x]]$mean_control
        mean_test <- data_analyze[[x]]$mean_test

        sd_control <- data_analyze[[x]]$sd_control
        sd_test <- data_analyze[[x]]$sd_test

        mean_perc_diff <- data_analyze[[x]]$mean_perc_diff

        mean_perc_diff_lwr <- data_analyze[[x]]$mean_perc_diff_lwr
        mean_perc_diff_upr <- data_analyze[[x]]$mean_perc_diff_upr

        p_value <- data_analyze[[x]]$p_value

        report_pfx <- cbind(
            Interferent = data_tidy[[x]]$Interferent[1],
            Sample_Conc = data_tidy[[x]]$Sample.Conc[1],
            Group = c("Con.", "Test")
        )

        report_num <- cbind(
            data_raw,
            Mean = c(mean_control, mean_test),
            SD = c(sd_control, sd_test),
            Diff = mean_perc_diff,
            CI95_lwr = mean_perc_diff_lwr,
            CI95_upr = mean_perc_diff_upr,
            p_value
        )

        report <- cbind(
            report_pfx,
            round(
                report_num,
                digits = 2
            )
        )

        return(report)
    }
)

data_report_combine <- data_report[[1]]

for (
    x in seq(2, length(data_report))
) {
    data_report_combine <- rbind(
        data_report_combine,
        data_report[[x]]
    )
}

data_report_combine <- data_report_combine %>%
    as.data.frame() %>%
    rename(
        "Sample Conc." = Sample_Conc,
        "Lwr." = CI95_lwr,
        "Upr." = CI95_upr,
        "p-value" = p_value
    )


## 產生flextable表格
data_report_ft <-
    data_report_combine %>%
    flextable() %>%
    set_header_labels(
        Diff = "%Diff"
    ) %>%
    merge_v(
        part = "body",
        j = c(
            "Interferent",
            "Sample Conc.",
            "Diff",
            "Lwr.",
            "Upr.",
            "p-value"
        )
    ) %>%
    add_header_row(
        values = c(
            colnames(data_report_combine)[1:3],
            "Replicates",
            "Mean",
            "SD",
            "%Diff",
            "95% CI",
            "p-value"
        ),
        colwidths = c(
            1, 1, 1,
            nrow(data_tidy[[1]]),
            1, 1, 1, 2, 1
        )
    ) %>%
    ### 垂直合併特定欄位
    merge_v(
        part = "header"
    ) %>%
    theme_ms() %>%
    align(
        align = "center",
        part = "header"
    ) %>%
    align(
        align = "center",
        part = "body"
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of measurement values of replicates, mean, and SD:",
            params$dev_unit3,
            sep = " "
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )
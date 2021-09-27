# Load PKG
pkg_lst <- c(
    "knitr",
    "readODS",
    "dplyr",
    "flextable",
    "here",
    "EnvStats",
    "ggplot2",
    "mcr"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

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

# params <- data.frame(
#     ep9_medical_decision_point = 2.233,
#     ep9_rm_outlier = FALSE,
#     ep9_alpha = 0.05,
#     ep9_err_ratio = 1,
#     ep9_mtest_name = "LC-MS",
#     ep9_mtest_name = "Leadgene (ELISA)",
#     dev_unit2 = "(μg/ml)"
# )

# # ! # Data: Import
# data_import <- read_ods(
#     "data.ods",
#     sheet = "ep9.2021.0714"
# )

# * Data: QC
## 作圖
data_qc_plot <- ggplot(
    data_import,
    aes(
        x = y.ref,
        y = y.test
    )
) +
    geom_point()

## 用Rosner's Test for Outliers檢測outlier
num_outlier_sus <-
    floor(nrow(data_import) * 0.05)

data_qc_rosner_test <-
    rosnerTest(
        data_import$y.test,
        k = num_outlier_sus,
        alpha = 0.05
    )

num_outlier <-
    data_qc_rosner_test[["n.outliers"]]

outlier <-
    data_qc_rosner_test[["all.stats"]]$Value[1:num_outlier]

data_qc <-
    data_import %>%
    arrange(y.test)

for (x in outlier) {
    data_qc <-
        data_qc %>%
        filter(y.test != x)
}

### 作圖：拿掉outlier
data_qc_plot_rosner <- ggplot(
    data_qc,
    aes(
        x = y.ref,
        y = y.test
    )
) +
    geom_point()

# * Data: Tidy
## 無須分組，僅須依照是否拿掉outlier來決定
if (params$ep9_rm_outlier) {
    data_tidy <- data_qc
} else {
    data_tidy <-
        data_import %>%
        arrange(y.test)
}

# * Data: Analysis
## 列出CLSI EP9中的regression方法以及其計算CI的方法
reg_ci <- data.frame(
    LinReg = c(
        "analytical",
        "Ordinary Linear Regression"
    ),
    Deming = c(
        "analytical",
        "Deming Regression"
    ),
    WLinReg = c(
        "analytical",
        "Weighted Ordinary Linear Regression"
    ),
    WDeming = c(
        "jackknife",
        "Weighted Deming Regression"
    ),
    PaBa = c(
        "bootstrap",
        "Passing-Bablok Regression"
    )
)

rownames(reg_ci) <-
    c("CI Method", "Regression Full Name")

## mcreg: 以各種regression方法進行分析
data_analysis <-
    lapply(
        colnames(reg_ci),
        function(x) {
            mcreg(
                x = data_tidy$y.ref,
                y = data_tidy$y.test,
                error.ratio = params$ep9_err_ratio,
                alpha = params$ep9_alpha,
                method.reg = x,
                method.ci = reg_ci$x[1],
                mref.name = params$ep9_mref_name,
                mtest.name = params$ep9_mtest_name,
                na.rm = TRUE
            )
        }
    )

names(data_analysis) <- colnames(reg_ci)

## getcoef: 取出各項regression之結果表
data_analysis_getcoef <-
    lapply(
        data_analysis,
        function(x) {
            result <-
                MCResult.getCoefficients(x) %>%
                as.data.frame()

            return(result)
        }
    )

## if.viable: 比較各種regression結果中
## intercept是否通過0，且
## slope是否通過1
data_analysis_if_viable <-
    sapply(
        data_analysis_getcoef,
        function(x) {
            if_intercept <-
                all(
                    x["Intercept", "LCI"] < 0,
                    x["Intercept", "UCI"] > 0
                )

            if_slope <-
                all(
                    x["Slope", "LCI"] < 1,
                    x["Slope", "UCI"] > 1
                )

            return(
                all(if_intercept, if_slope)
            )
        }
    )

data_analysis_ci_area <-
    sapply(
        data_analysis_getcoef,
        function(x) {
            len_intercept <-
                x["Intercept", "UCI"] - x["Intercept", "LCI"]

            len_slope <-
                x["Slope", "UCI"] - x["Slope", "LCI"]

            ci_area <- len_intercept * len_slope

            return(ci_area)
        }
    )

best_regression <-
    data_analysis_if_viable[order(data_analysis_ci_area)]

best_regression <-
    best_regression[best_regression == TRUE][1]

best_regression <-
    names(best_regression)

data_analysis_best <-
    data_analysis[[best_regression]]

data_analysis_getcoef_best <-
    data_analysis_getcoef[[best_regression]]

## pbias_best: 計算出最佳回歸的proportional percentage bias
data_analysis_pbias_best <-
    MCResult.calcBias(
        data_analysis[[best_regression]],
        type = "proportional",
        percent = TRUE,
        x.levels = params$ep9_medical_decision_point,
        alpha = 0.05
    )

## getfitted: 取出以regression結果反算之數據
data_analysis_getfitted <-
    lapply(
        data_analysis,
        function(x) {
            result <-
                MCResult.getFitted(x) %>%
                as.data.frame() %>%
                arrange(x_hat)

            return(result)
        }
    )

# * Data: Report
## Scatter plot: 與regression
data_report_scatter_fun <-
    function(x = data_import_ep9) {
        plot(
            x$y.ref,
            x$y.test,
            xlab = params$ep9_mref_name,
            ylab = params$ep9_mtest_name
        )
    }

## Residual Plot (在CLSI EP9稱為Difference plot)
data_report_plot_residual_fun <-
    function(x = data_analysis_ep9) {
        MCResult.plotResiduals(
            x[[1]],
            res.type = "y",
            ylab = paste(
                "Difference",
                params$dev_unit2
            ),
            main = "Unit Difference Plot"
        )
    }

## compareFit: 比較各種regression方式的intercept以及slope
## intercept以包含0為佳，slope以包含1為佳，
## CI均為越小越好
## 由於mcr所有圖均無法存為變數，因此以function方式即時呼叫生成
data_report_plot_comparefit_fun <-
    function(x = data_analysis_ep9) {
        compareFit(
            x[["PaBa"]],
            x[["WDeming"]],
            x[["WLinReg"]],
            x[["Deming"]],
            x[["LinReg"]]
        )
    }

## Regression plot
data_report_plot_regression_fun <-
    function(x = data_analysis_best_ep9) {
        plot(
            x,
            add.legend = FALSE
        )
    }

## Regression Chart
data_report_regression <-
    data.frame(
        Item = c("Intercept", "Slope"),
        round(
            data_analysis_getcoef_best,
            digits = 3
        )
    )

### 拿掉空的欄位
data_report_regression$SE <- NULL

data_report_regression_ft <-
    data_report_regression %>%
    flextable() %>%
    set_header_labels(
        EST = "Value",
        LCI = "95% CI Lwr.",
        UCI = "95% CI Upr."
    ) %>%
    align(
        align = "center",
        part = "all"
    )

## Bias Chart
data_report_pbias_ft <-
    data_analysis_pbias_best %>%
    # 轉換為data.frame
    as.data.frame() %>%
    # 保留要的欄位
    select(
        "Prop.bias(%)",
        "LCI",
        "UCI"
    ) %>%
    # 顯示到小數第三位
    round(digits = 3) %>%
    flextable() %>%
    set_header_labels(
        "Prop.bias(%)" = "Proportional Bias (%)",
        LCI = "95% CI Lwr.",
        UCI = "95% CI Upr."
    ) %>%
    align(
        align = "center",
        part = "all"
    )
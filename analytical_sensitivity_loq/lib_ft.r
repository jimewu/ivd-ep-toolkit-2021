pkg_lst <- c(
  "knitr",
  "readODS",
  "dplyr",
  "flextable",
  "officedown",
  "officer"
)

lapply(
  pkg_lst,
  library,
  character.only = TRUE
)

theme_ms <- function(x) {
  x <-
    # 以theme_box為基礎
    theme_box(x) %>%
    # header背景顏色
    bg(
      part = "header",
      bg = "#4F81BD"
    ) %>%
    # header外圍框線顏色&寬度
    border_outer(
      part = "header",
      border = fp_border(
        color = "#4F81BD",
        width = 1
      )
    ) %>%
    # header內側橫向框線顏色&寬度
    border_inner(
      part = "header",
      border = fp_border(
        color = "white",
        width = 1
      )
    ) %>%
    # body框線顏色&寬度
    border(
      part = "body",
      border = fp_border(
        color = "#4F81BD",
        width = 1
      )
    ) %>%
    # header文字顏色
    color(
      part = "header",
      color = "white"
    ) %>%
    autofit()

  return(x)
}

# 多數情況
set_flextable_defaults(
  font.family = "Times New Roman",
  eastasia.family = "DFKai-SB",
  font.size = 14,
  theme_fun = "theme_ms"
)

# # for CER
# set_flextable_defaults(
#   font.family = "Cambria",
#   eastasia.family = "DFKai-SB",
#   theme_fun = "theme_box"
# )

# 依照可視寬度等比例換算欄位寬度
width_ratio <- function(x,
                        width_col_ratio,
                        width_visible = 10) {
  result <-
    width_col_ratio * width_visible / sum(width_col_ratio)

  width(
    x = x,
    width = result
  )
}
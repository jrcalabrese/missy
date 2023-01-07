#' Format a table using APA style.
#'
#' This function can be applied to `gtsummary` and `flextable` objects. It will format the table
#' to be in APA style. You still need to manually add a title and save it as a .docx, though.
#'
#' @param ft A `flextable` or `gtsummary` object. Will convert to `flextable` if not a `flextable` object.
#'
#' @importFrom methods is
#' @importFrom dplyr %>%
#' @importFrom gtsummary as_flex_table
#' @importFrom flextable font fontsize align rotate hline_top hline_bottom border_remove set_table_properties color
#' @importFrom officer fp_border
#' @export

apa_theme <- function (ft)  {

  if (is(ft) == "flextable") {
    ft
  }

  if (!is(ft) == "flextable") {
    ft <- gtsummary::as_flex_table(ft)
  }

  ft %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::font(fontname = "Times New Roman", part = "footer") %>%
    flextable::fontsize(size = 12, part = "all") %>%
    flextable::align(align = "left", part = "body") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::rotate(rotation = "lrtb", align = "top", part = "body") %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = officer::fp_border(width = 2),
                         part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2),
                            part = "all") %>%
    flextable::hline(i = 1, border = officer::fp_border(width = 1), part = "header") %>%
    flextable::set_table_properties(layout = "autofit") %>%
    flextable::color(color = "black", part = "header")
}

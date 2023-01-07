#' Create a descriptive statistics table from imputed data.
#'
#' This takes data from a `mids` object and turns the variables you specify
#' into table of descriptive statistics This is useful when handling imputed data.
#'
#' The output will be a `flextable` formatted in APA style.
#'
#' @param imp A `mids` object.
#' @param vs Character vector, variables from `imp`. E.g., `c("bmi", "chl")`.
#' @param title Character vector, title of your correlation matrix. Optional.
#' @param nm Character vector, preferred variable names. E.g., `c("BMI", "Cholesterol")`. Optional.
#'
#' @importFrom stats setNames
#' @importFrom mice complete
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr %>%
#' @importFrom rrtable df2flextable
#' @importFrom flextable add_header_lines
#' @export

mice_df <- function(imp, vs, title, nm){

  # complete data
  impdat <- mice::complete(imp, action = "long", include = FALSE)

  # this is where this function came from
  # https://bookdown.org/mwheymans/bookmi/data-analysis-after-multiple-imputation.html

  z <- lapply(as.list(vs), function(x){
    x = as.name(x)
    pool_mean <- with(impdat, by(impdat, .imp, function(y) c(
      mean(y[[x]]),
      sd(y[[x]]),
      min(y[[x]]),
      max(y[[x]]),
      ( sd(y[[x]])/sqrt(length(y[[x]])) )
    )))
    Reduce("+", pool_mean)/length(pool_mean)
  }) %>%
    setNames(as.list(vs)) %>%
    as.data.frame()

  if (missing(nm))
    colnames(z) <- vs
  else
    colnames(z) <- nm

    z <- z %>% `rownames<-`(c("Mean", "Standard Deviation", "Minimum",
                   "Maximum", "Standard Error")) %>%
    t() %>% as.data.frame() %>%
    tibble::rownames_to_column("Variable")

    if (missing(title))
      title <- "Descriptive Statistics"
    else
      title <- title

  z <- z %>%
    rrtable::df2flextable(
      vanilla = TRUE,
      add.rownames = FALSE,
      colorheader = FALSE,
      align_body = "left",
      NA2space = TRUE) %>%
    missy::apa_theme() %>%
    flextable::add_header_lines(values = title)

  return(z)
}

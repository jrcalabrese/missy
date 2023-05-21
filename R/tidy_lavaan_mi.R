#' Tidy a `lavaan.mi` object.
#'
#' Put description here later.
#'
#' @param x A `lavaan.mi` object, such as those returned from [semTools::lavaan.mi()].
#'
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr mutate filter rename select %>% across everything
#'
#' @export
tidy_lavaan_mi <- function(x) {

  Beta <- Estimate <- SE <- est <- exo <- label <- lhs <- op <- p <- pvalue <- rhs <- rowname <- se <- term <- NULL

  y <-
    summary(x) %>%
    as_tibble() %>%
    rownames_to_column() %>%
    mutate(term = paste(lhs, op, rhs)) %>%
    filter(!grepl("~~", term)) %>%
    rename(
      Estimate = term,
      Beta = est,
      SE = se,
      `T` = t,
      p = pvalue,
    ) %>%
    select(Estimate, op, everything(), -rowname, -lhs, -rhs) %>%
    as_tibble() %>%
    select(-c(op, exo, label, `df`)) %>%
    mutate(across(c(Beta, SE, `T`), round, 2)) %>%
    mutate(p = format(round(p, digits = 3), nsmall = 3))

  return(y)

}

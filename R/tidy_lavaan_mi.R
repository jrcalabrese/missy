#' Tidy a `lavaan.mi` object.
#'
#' Put description here later.
#'
#' @param x A `lavaan.mi` object, such as those returned from [semTools::lavaan.mi()].
#'
#' @importFrom dplyr mutate filter rename select %>% across everything
#' @importFrom tibble as_tibble rownames_to_column
#'
#' @export
tidy_lavaan_mi <- function(x) {

  Beta <- Estimate <- SE <- est <- exo <- label <- lhs <- df <- op <- p <- pvalue <- rhs <- rowname <- se <- term <- `std.all` <- NULL

  mm <- summary(x,
          rsquare = T,
          standardized = "std.all",
          fit.measures = TRUE,
          modindices = TRUE,
          pool.robust = TRUE,
          test = "D2") %>%
    as.data.frame() %>%
    tibble::as_tibble(.name.repair = "unique") %>%
    tibble::rownames_to_column() %>%
    mutate(term = paste(lhs, op, rhs)) %>%
    filter(!grepl("~~", term)) %>%
    rename(Estimate = term, Beta = est, SE = se, `T` = t, p = pvalue) %>%
    select(Estimate, op, everything(), -rowname, -lhs, -rhs) %>%
    as_tibble() %>%
    select(-c(op, exo, label, `df`, `std.all`)) %>%
    mutate(across(c("Beta", "SE", `T`), ~round(., 2))) %>%
    mutate(p = format(round(p, digits = 3), nsmall = 3))

  print(mm)

}


#' Create a correlation matrix from imputed data.
#'
#' This takes data from a `mids` object and turns the variables you specify
#' into a correlation matrix. This is useful when handling imputed data.
#'
#' The output will be a `flextable` formatted in APA style.
#'
#' @param imp A `mids` object.
#' @param vs Character vector, variables from `imp`. E.g., `c("bmi", "chl")`.
#' @param nm Character vector, preferred variable names. E.g., `c("BMI", "Cholesterol")`. Optional.
#' @param title Character, title of your correlation matrix. Optional.
#'
#' @importFrom miceadds micombine.cor
#' @importFrom dplyr mutate select group_by %>%
#' @importFrom tibble column_to_rownames
#' @importFrom tidyr pivot_longer
#' @importFrom rrtable df2flextable
#' @importFrom flextable compose add_header_lines as_paragraph as_chunk
#' @export
mice_cor <- function(imp, vs, nm, title) {

  # https://nathaneastwood.github.io/2019/08/18/no-visible-binding-for-global-variable/
  p <- r <- value <- variable1 <- variable2 <- NULL

  # use micombine.cor
  res <- miceadds::micombine.cor(mi.res = imp, variables = vs) %>%
    dplyr::select(c(variable1, variable2, r, p))

  # Rename variables
  if (missing(nm))
    nm <- vs
  else
    nm <- nm

  res <- res %>%
    mutate(
      variable1 = ifelse(variable1 %in% vs, nm[match(variable1, vs)], variable1),
      variable2 = ifelse(variable2 %in% vs, nm[match(variable2, vs)], variable2)
    )

  # round digits
  res <- res %>%
    dplyr::mutate(r = round(r, digits = 2)) %>%
    dplyr::mutate(r = sub("^(-?)0.", "\\1.", sprintf("%.2f", r))) %>%
    dplyr::mutate(p = round(p, digits = 3)) %>%
    dplyr::mutate(p = sub("^(-?)0.", "\\1.", sprintf("%.2f", p))) %>%
    # Surround with parentheses
    dplyr::mutate(p = paste0("(",p,")")) %>%
    # Combine
    dplyr::mutate(value = paste0(r," \n ", p))

  # Get rid of old columns
  res <- res %>%
    dplyr::select(-c(r, p))

  # Make wide
  res <- res %>%
    dplyr::group_by(variable2, variable1) %>%
    tidyr::pivot_wider(
      names_from = variable1,
      values_from = value
    ) %>%
    tibble::column_to_rownames("variable2")

  res[is.na(res)] <- "-"

  # Slightly change row order
  res <- res[match(colnames(res), rownames(res)),]

  # Extract lower triangle
  res[upper.tri(res, diag = TRUE)] <- NA
  res <- res[rowSums(is.na(res)) != ncol(res), ]
  res <- res[,colSums(is.na(res)) < nrow(res)]

  if (missing(title))
    title <- "Correlation Matrix"
  else
    title <- title

  res <- res %>%
    rrtable::df2flextable(
    vanilla = TRUE,
    add.rownames = TRUE,
    colorheader = FALSE,
    align_body = "left",
    NA2space = TRUE) %>%
    missy::apa_theme() %>%
    flextable::compose(i = 1, j = 1,
                       part = "header",
                       flextable::as_paragraph(flextable::as_chunk(" "))) %>%
    flextable::add_header_lines(values = title, top = TRUE)

  return(res)
}

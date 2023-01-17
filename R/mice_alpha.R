#' Calculates Cronbach's alpha with multiply imputed data.
#'
#' This function calculates Cronbach's alpha from variables in a `mids` object.
#' #' Based on Dion Groothof's \href{https://stackoverflow.com/questions/70816175/how-do-i-calculate-cronbachs-alpha-on-multiply-imputed-data}{StackOverflow post}.
#'
#' The output will be a `flextable` formatted in APA style.
#'
#' @param imp A `mids` object.
#' @param varlist Variables that make up a specific subscale, e.g., `c("x1", "x2", "x3")`.
#' @param bnum An integer specifying the number of bootstrap samples to be taken.
#' @param title Character, title of your Cronbach's alpha table Optional.
#'
#' @importFrom mice complete as.mids pool.scalar
#' @importFrom dplyr select %>% rename
#' @importFrom tibble rownames_to_column
#' @importFrom tidyselect all_of
#' @importFrom rrtable df2flextable
#' @importFrom flextable compose add_header_lines as_paragraph as_chunk
#'
#' @export

mice_alpha <- function(imp, varlist, bnum, title){

    var <- sd <- quantile <- qt <- NULL

    Bx <- bnum

  implong <- mice::complete(imp, action = "long")

  implong2 <- mice::complete(imp, action = "long", include = TRUE) %>%
    select(".imp", ".id", all_of(varlist))
  makemids <- as.mids(implong2, .imp = ".imp", .id = ".id")
  imp <- makemids

  cronbach_fun <- function(list_compl_data, boot = TRUE, B = Bx, ci = FALSE) {
    n <- nrow(list_compl_data); p <- ncol(list_compl_data)
    total_variance <- var(rowSums(list_compl_data))
    item_variance <- sum(apply(list_compl_data, 2, sd)^2)
    alpha <- (p/(p - 1)) * (1 - (item_variance/total_variance))
    out <- list(alpha = alpha)
    boot_alpha <- numeric(B)
    if (boot) {
      for (i in seq_len(B)) {
        boot_dat <- list_compl_data[sample(seq_len(n), replace = TRUE), ]
        total_variance <- var(rowSums(boot_dat))
        item_variance <- sum(apply(boot_dat, 2, sd)^2)
        boot_alpha[i] <- (p/(p - 1)) * (1 - (item_variance/total_variance))
      }
      out$var <- var(boot_alpha)
    }
    if (ci){
      out$ci <- quantile(boot_alpha, c(.025,.975))
    }
    return(out)
  }

  m <- length(unique(implong$.imp))
  boot_alpha <- rep(list(NA), m)
  for (i in seq_len(m)) {
    set.seed(i) # fix random number generator
    sub <- implong[implong$.imp == i, -c(1,2)]
    boot_alpha[[i]] <- cronbach_fun(sub)
  }

  # obtain Q and U (see ?pool.scalar)
  Q <- sapply(boot_alpha, function(x) x$alpha)
  U <- sapply(boot_alpha, function(x) x$var)

  # pooled estimates
  pool_estimates <- function(x) {
    out <- c(
      alpha = x$qbar,
      lwr = x$qbar - qt(0.975, x$df) * sqrt(x$t),
      upr = x$qbar + qt(0.975, x$df) * sqrt(x$t)
    )
    return(out)
  }

  zz <- pool_estimates(pool.scalar(Q, U))
  zz <- setNames(object = zz,
                 nm = c("Alpha", "Lower 95%", "Upper 95%")) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(Name = 1) %>% rename(Value = 2)

  if (missing(title))
    title <- "Cronbach's alpha"
  else
    title <- title

  res <- zz %>%
    rrtable::df2flextable(
      vanilla = TRUE,
      add.rownames = FALSE,
      colorheader = FALSE,
      align_body = "left",
      align_header = "left",
      NA2space = TRUE,
      digits = 3
      ) %>%
    apa_theme() %>%
    flextable::compose(i = 1, j = 1, part = "header",
                       flextable::as_paragraph(flextable::as_chunk(" "))) %>%
    flextable::add_header_lines(values = title, top = TRUE)

  return(res)

}

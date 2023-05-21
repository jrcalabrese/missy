#' Convert object from `plotProbe` into a `ggplot` object.
#'
#' This is an updated/modified version of [cookesd](https://stackoverflow.com/users/13716967/cookesd)'s function,
#' which can be found [here on StackOverflow](https://stackoverflow.com/questions/64931105/plotting-interaction-graph-solely-based-on-slope).
#'
#' @param model_res An object produced by [`probe2WayMC` or other related functions](https://www.rdocumentation.org/packages/semTools/versions/0.5-6/topics/plotProbe.html#:~:text=335.%2010.3390/psych3030024-,See%20Also,latent%20interaction%20when%20the%20results%20are%20obtained%20from%20residual%2Dcentering%20approach.,-Examples).
#' @param xlim Your specified `xlim` parameter. E.g., `c(-1, 1)`.
#' @param mycolor Specified colors. Can be one or more colors.
#'
#' @importFrom dplyr mutate select left_join %>% sym
#' @importFrom ggplot2 ggplot geom_segment aes labs scale_color_manual guides
#'
#' @export
ggplotProbe <- function(model_res, xlim = xlim, mycolor){

  Intcept <- est <- x_start <- x_end <- y_start <- y_end <- NULL

  # make slope and intercept matrices dfs to enable merging

  slope_df = data.frame(model_res$SimpleSlope, check.names=F)
  if (is.null(model_res$SimpleIntcept)){
    # set intercepts to 0 if none provided per documentation https://www.rdocumentation.org/packages/semTools/versions/0.5-2/topics/probe2WayMC
    intcept_df = slope_df %>% mutate(Intcept = 0)
  } else{
    intcept_df = data.frame(model_res$SimpleIntcept, check.names = F)
  }
  var_name = colnames(intcept_df)[1] # get the name of the independent variable

  # combine the slope and intercept data and make segment endpoints
  plot_df = intcept_df %>%
    select(c(!!var_name, 'Intcept')) %>%
    left_join(slope_df %>% select(c(!!var_name,'est')), by = var_name) %>%
    # make segment endpoints for geom_segment
    mutate(x_start = xlim[1], x_end = xlim[2],
                  y_start = Intcept + est*x_start,
                  y_end = Intcept + est*x_end)

  # make graph
  g = ggplot(plot_df) +
    geom_segment(aes(x = x_start, y = y_start,
                     xend = x_end, yend = y_end,
                     color = factor(!!sym(var_name)),
                     linetype = factor(!!sym(var_name), levels = intcept_df[[var_name]])
                     ),
                 data = plot_df) +
    labs(linetype = var_name,
         y = "Dependent Variable",
         x = "Independent Variable") +
    scale_color_manual(name = "Moderator",
                       label = c("-1 SD", "Mean", "+1 SD"),
                       values = mycolor) +
    guides(linetype = "none")

  # return graph and df used to plot
  #return(list('graph' = g,'df' = plot_df))
  return(g)
}

multi_anova <- function(data, adjust, title) {
  data_piv <- data |>
    tidyr::pivot_longer(
      !(sample:condition),
      names_to = "var",
      values_to = "value"
    ) |>
    dplyr::group_by(var)

  res <- data_piv |>
    # condense data per group into list-column for aov function
    tidyr::nest() |>
    dplyr::mutate(
      aov = purrr::map(data, ~ rstatix::anova_summary(stats::aov(value ~ condition, data = .x), detailed = TRUE)),
      n = purrr::map(data, dplyr::summarise, n = dplyr::n())
    ) |>
    # pull out ANOVA summary info
    tidyr::unnest(cols = c(aov, n)) |>
    dplyr::select(-c(data, Effect, "p<.05")) |>
    # need to ungroup for p adjustment
    dplyr::ungroup() |>
    rstatix::adjust_pvalue(method = adjust) |>
    rstatix::add_significance()

  boxplots <- list()
  for (i in res$var) {
    boxplots[[i]] <- local({
      # subtitle label
      lab <- res |>
        dplyr::filter(var == i) |>
        glue::glue_data("*F*({DFn}, {DFd}) = {F}, &eta; <sup>2</sup><sub>g</sub> = {ges}, *p.adj*={p.adj}, *n*={n}")

      p1 <- data_piv |>
        dplyr::filter(var == i) |>
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_inorder(condition), y = value)) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = condition)) +
        ggplot2::geom_point(size = 1.5)

      p1 <- p1 +
        ggpubr::geom_pwc(method = "tukey_hsd", hide.ns = TRUE) +
        ggpubr::theme_pubr()

      p1 +
        ggplot2::labs(
          title = i,
          subtitle = lab,
        ) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        viridis::scale_fill_viridis(discrete = TRUE, option = "viridis", begin = .2, end = .8) +
        ggpubr::theme_pubr() +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            face = "bold"
          ),
          plot.subtitle = ggtext::element_markdown(
            hjust = 0.5
          )
        )
    })
  }

  p1 <- cowplot::plot_grid(plotlist = boxplots)

  plot_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      label =
        if (shiny::isTruthy(title)) {
          title
        } else {
          "ANOVA of each variable"
        },
      fontface = "bold",
      hjust = 0.5
    )

  # massage label to reflect actual p.adjust method
  pwc_label <- data_piv |>
    rstatix::tukey_hsd(value ~ condition) |>
    rstatix::get_pwc_label()
  pwc_label[[5]][[2]] <- adjust

  test_stat_caption <- cowplot::ggdraw() +
    cowplot::draw_label(
      pwc_label,
      fontface = "plain",
      hjust = 0.5
    )

  stars <- cowplot::ggdraw() +
    cowplot::draw_label("* p<0.05, ** p<0.01, *** p<0.001",
      hjust = 0.5
    )

  caption <- cowplot::plot_grid(
    test_stat_caption, stars,
    ncol = 2
  )

  p1 <- cowplot::plot_grid(
    plot_title, p1, caption,
    ncol = 1,
    rel_heights = c(0.1, 1)
  )

  y_title <- grid::textGrob("Abundance",
    gp = grid::gpar(fontface = "bold", fontsize = 15),
    rot = 90
  )

  pdf(NULL)
  p1 <- gridExtra::grid.arrange(gridExtra::arrangeGrob(p1, left = y_title))
  invisible(dev.off())

  # make pretty table for display
  table <- res |> dplyr::mutate(
    DFn = format(DFn, digits = 1),
    DFd = format(DFd, digits = 1),
    p = rstatix::p_format(p, digits = 3),
    p.adj = rstatix::p_format(p.adj, digits = 3),
    SSn = format(SSn, digits = 3),
    SSd = format(SSd, digits = 3),
  )

  out <- list(
    "test" = "ANOVA",
    "pretty_title" = "ANOVA",
    "res" = res,
    "table" = table,
    "boxplots" = boxplots,
    "plot_grid" = p1
  )

  return(out)
}

# t-test per column against condition
multi_ttest <- function(data, parametric, var.equal, paired, conf.level, adjust, title) {
  data_piv <- data |>
    tidyr::pivot_longer(
      !(sample:condition),
      names_to = "var",
      values_to = "value"
    ) |>
    dplyr::group_by(var)

  if (shiny::isTruthy(parametric)) {
    res <- data_piv |> rstatix::t_test(
      formula = value ~ condition,
      var.equal = var.equal,
      paired = paired,
      conf.level = conf.level,
      detailed = TRUE
    )
    test <- "ttest"
  } else {
    res <- data_piv |> rstatix::wilcox_test(
      formula = value ~ condition,
      paired = paired,
      conf.level = conf.level,
      detailed = TRUE
    )
    test <- "mwu"
  }

  # make test title
  pretty_title <- test |> dplyr::case_match(
    "mwu" ~ "Mann Whitney U Test",
    "ttest" ~ "Student's T-test"
  )
  if (shiny::isTruthy(paired)) {
    pretty_title <- paste0("Paired ", pretty_title)
    test <- paste0("paired_", test)
  }
  res <- res |>
    rstatix::adjust_pvalue(method = adjust) |>
    rstatix::add_significance() |>
    rstatix::add_xy_position(x = "condition")

  # Boxplot of each item
  boxplots <- list()
  for (i in res$var) {
    stat <- res |> dplyr::filter(var == i)
    boxplots[[i]] <- local({
      p1 <- data_piv |>
        dplyr::filter(var == i) |>
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_inorder(condition), y = value)) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = condition)) +
        ggplot2::geom_point(size = 1.5)

      # add connecting lines for paired data
      if (shiny::isTruthy(paired)) {
        p1 <- p1 + ggplot2::geom_line(ggplot2::aes(group = sample))
      }

      p1 <- p1 +
        ggpubr::geom_bracket(data = stat) +
        ggpubr::theme_pubr()

      p1 +
        ggplot2::labs(
          title = i,
          subtitle = bquote(
            P[adj] == .(format(signif(as.numeric(stat |> dplyr::pull(p.adj), digits = 3)), scientific = -2, digits = 2))
          )
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
          plot.subtitle = ggplot2::element_text(
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
          paste0(pretty_title, " of each variable")
        },
      fontface = "bold",
      hjust = 0.5
    )

  test_stat_caption <- cowplot::ggdraw() +
    cowplot::draw_label(rstatix::get_pwc_label(res),
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
  table <- res |>
    dplyr::relocate(p, .before = p.adj) |>
    dplyr::mutate(
      p = rstatix::p_format(p, digits = 3),
      p.adj = rstatix::p_format(p.adj, digits = 3),
      estimate = format(estimate, digits = 3),
      statistic = format(statistic, digits = 3),
    )
  if (shiny::isTruthy(parametric)) {
    table <- table |>
      dplyr::select(c(var, n1:df, estimate, p:p.adj)) |>
      dplyr::mutate(
        df = format(df, digits = 3)
      )
  } else {
    table <- table |>
      dplyr::select(c(var, n1:statistic, estimate, p:p.adj))
  }


  out <- list(
    "pretty_title" = pretty_title,
    "test" = test,
    "res" = res,
    "table" = table,
    "boxplots" = boxplots,
    "plot_grid" = p1
  )

  return(out)
}

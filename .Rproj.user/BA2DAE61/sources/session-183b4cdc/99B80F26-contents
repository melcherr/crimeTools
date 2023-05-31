#' Plots results of item reponse theory models
#'
#' Returns an object of class 'irtClass'
#' @param data Data frame
#' @param model model formula, must be a formula or string
#' @param itemtype type of IRT model
#' @param dim_name name of dimension, must be a string
#' @param legend_categories name of categories, must be a character vector
#' @param xlim limits of x axis of itemparameter plot, must be a numeric vector
#' @param ylim limits of y axis of itemparameter plot, must be a numeric vector
#' @param ... further arguments for function mirt()
#' @return A-irtClass()-object
#' @export

plot_mirt <-
  function(data,
           itemtype = "gpcm",
           dim_name = "value",
           model,
           legend_categories = NULL,
           xlim = NULL,
           ylim = NULL,
           ...) {

    # setOldClass(c("gg", "ggplot", "trellis", NULL))
    #
    # setClassUnion("SingleGroupClassOrNULL", members = c("SingleGroupClass", "NULL"))
    # setClassUnion("tblOrNULL", members = c("tbl", "NULL"))
    # setClassUnion("trellisOrNULL", members = c("trellis", "NULL"))
    # setClassUnion("ggOrNULL", members = c("gg", "NULL"))
    #
    #
    # set_irtClass <- setClass(Class = "irtClass",
    #                          slots = c(model = "SingleGroupClassOrNULL",
    #                                    fit_table = "tblOrNULL",
    #                                    itemfit_plot = "ggOrNULL",
    #                                    itemtrace_plot = "trellisOrNULL",
    #                                    itemparameter_plot = "ggOrNULL"))

    data <- na.omit(data)

    model_irt <- mirt::mirt(data = data,
                            itemtype = itemtype,
                            model = model,
                            SE = TRUE,
                            ...)

    if ((ncol(data) > 3) & !is.double(model)) {

      fit_table <-
        mirt::anova(model_irt) %>%
        bind_cols(mirt::M2(model_irt, type = "C2", calcNULL = FALSE)) %>%
        bind_cols(model_irt@Data[c("N", "nitems")]) %>%
        dplyr::mutate(model = dim_name) %>%
        dplyr::select(model, N, nitems, AIC, BIC, logLik, RMSEA, CFI, TLI) %>%
        dplyr::rename("Items" = "nitems") %>%
        tidyr::pivot_longer(-model, names_to = "Fitindex") %>%
        tidyr::pivot_wider(names_from = model, values_from = value)

    } else {

      fit_table <-
        model_irt@Data[c("N", "nitems")] %>%
        append(model_irt@Fit[c("AIC", "BIC", "logLik", "RMSEA")]) %>%
        tibble::as_tibble_row() %>%
        dplyr::rename("Items" = "nitems") %>%
        tidyr::pivot_longer(everything(),
                            names_to = "Fitindex",
                            values_to = dim_name)

    }

    nudge <- max(mirt::itemfit(model_irt)[,2]) * 0.07

    itemfit_plot <-
      mirt::itemfit(model_irt)[,1:2] %>%
      ggplot(data = ., aes(y = reorder(item, S_X2), x = S_X2)) +
      geom_bar(stat = "identity", alpha = 0.5, width = 0.6) +
      geom_text(aes(label = sprintf(S_X2, fmt = "%.1f")),
                nudge_x = nudge,
                family = "sans") +
      geom_crossbar(aes(xmin = S_X2, xmax = S_X2, y = reorder(item, S_X2)), width = 0.6) +
      ylab("") +
      xlab(expression(paste("Generalisiertes S-", chi^2))) +
      theme_tufte() +
      theme(text = element_text(family = "sans"),
            axis.line.x = element_line(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 13),
            axis.text.y = element_text(face = "bold"),
            panel.grid.major.x = element_line(colour = "grey80"),
            plot.title = element_text(face = "bold", size = 18)) +
      lemon::coord_capped_cart(bottom = "both")


    if (!is.double(model)) {

      itemtrace_plot <- mirt::plot(model_irt,
                                   type = "trace",
                                   main = "Charakteristikkurven der Itemkategorien (CCC)")

      itemtrace_plot$par.settings$strip.background$col <- "lightgrey"
      itemtrace_plot$x.scales$alternating <- 3
      itemtrace_plot$y.scales$alternating <- 3
      itemtrace_plot$par.strip.text <- list("cex" = 1, "fontface" = "bold")
      itemtrace_plot$legend <- list(bottom = itemtrace_plot$legend$right)
      itemtrace_plot$legend$bottom$args$columns <- 5
      itemtrace_plot$par.settings$layout.heights <- list(xlab.key.padding = 2)

      if (!is.null(legend_categories)) itemtrace_plot$legend$bottom$args$text <- legend_categories

      data <- mirt::coef(model_irt, IRTpars = TRUE) %>%
        purrr::map_dfr(.,
                       ~dplyr::as_tibble(t(.x),
                                         rownames = "coeff"),
                       .id = "var") %>%
        dplyr::filter(var %in% colnames(model_irt@Data$data)) %>%
        dplyr::mutate(coeff = stringr::str_extract(string = coeff, "[:lower:]")) %>%
        dplyr::group_by(var, coeff) %>%
        dplyr::summarise(dplyr::across(par:CI_97.5,
                                       ~mean(.))) %>%
        tidyr::pivot_wider(names_from = coeff,
                           values_from = c(par, CI_2.5, CI_97.5))

      a_scale <- (max(data$par_a) - min(data$par_a)) * 0.02
      b_scale <- (max(data$par_b) - min(data$par_b)) * 0.02

      nudge <- abs(max(data[,"CI_97.5_b"]) - min(data[,"CI_2.5_b"])) * 0.07

      itemparameter_plot <-
        ggplot(data = data) +
        geom_point(aes(x = par_a, y = par_b), color = "darkred") +
        geom_text(aes(x = par_a, y = CI_97.5_b, label = var), nudge_y = nudge) +
        geom_linerange(aes(xmin = par_a, xmax = CI_97.5_a, y = par_b), alpha = 0.5) +
        geom_linerange(aes(ymin = par_b, ymax = CI_97.5_b, x = par_a), alpha = 0.5) +
        geom_linerange(aes(xmin = CI_2.5_a, xmax = par_a, y = par_b), alpha = 0.5) +
        geom_linerange(aes(ymin = CI_2.5_b, ymax = par_b, x = par_a), alpha = 0.5) +
        scale_x_continuous(limits = xlim, breaks = scales::pretty_breaks(6)) +
        scale_y_continuous(limits = ylim, breaks = scales::pretty_breaks(6)) +
        ylab("Itemschwierigkeit") +
        xlab("Itemdiskriminanz") +
        ggthemes::theme_tufte() +
        theme(text = element_text(family = "sans"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 13),
              axis.line = element_line(),
              plot.title = element_text(face = "bold", size = 18)) +
        lemon::coord_capped_cart(bottom = "both", left = "both")

    } else {

      itemtrace_plot <- NULL
      itemparameter_plot <- NULL

    }

    results <- list(model = model_irt,
                    fit_table = fit_table,
                    itemfit_plot = itemfit_plot,
                    itemtrace_plot = itemtrace_plot,
                    itemparameter_plot = itemparameter_plot)

    return(results)

  }

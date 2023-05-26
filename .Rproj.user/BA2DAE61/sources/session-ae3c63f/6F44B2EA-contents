to_string_pct <- function(x, dec = "0", fac = 1) paste0(stringr::str_replace(sprintf(x * fac, fmt = paste0("%.", dec, "f")), "\\.", ","), "%")

`%nin%` <- function(x, y) !match(x, y, nomatch = FALSE)

split_formula <- function(formula) {

  stringr::str_split(formula,
            pattern = " \\+ | \\~ |\\|") %>%
    unlist %>%
    stringr::str_remove_all("\\(\\||\\)") %>%
    stringr::str_subset("\\*|\\(", negate = TRUE) %>%
    stringr::str_subset("^[0-9]*$", negate = TRUE)

}

min_calc <- function(x, method = mean, min_valid = 3) {

  x_na <- is.na(x)

  if (length(x) - sum(x_na) < min_valid) return(NA_real_)

  else {{method}}(x, na.rm = TRUE)


}

is_binary <- function(x) {

  unique_values <- unique(x)
  length(unique_values) == 2 && all(unique_values %in% c(0,1))

}

get_fit <- function(model) {

  if (inherits(model, "clmm")) {

    cluster <- model$dims[["q"]]

    icc_clmm <- round(performance::icc(model)[[1]], 3)
    r2_conditional <- round(performance::r2(model)[[1]], 2) %>%
      unname()
    r2_marginal <- round(performance::r2(model)[[2]], 2) %>%
      unname()

  }

  else if (inherits(model, "lmerMod")){

    cluster <- model@devcomp$dims[["q"]]

  }

  n <- nobs(model)

  modellfit <-
    performance::model_performance(model) %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = everything()) %>%
    dplyr::filter(name %in% c("AIC", "BIC", "ICC", "R2_conditional", "R2_marginal")) %>%
    dplyr::mutate(name = dplyr::case_when(name == "R2_conditional" ~ "R<sup>2</sup><sub>Konditional</sub>",
                                          name == "R2_marginal" ~ "R<sup>2</sup><sub>Marginal</sub>",
                                          TRUE ~ name),
                  value = dplyr::if_else(name == "ICC", round(value, 3), round(value, 2)),
                  text = stringr::str_replace(paste0("**", name, "**", " = ", value), "\\.", ",")) %>%
    dplyr::pull(text) %>%
    c(glue::glue("**Befragte** = ", n),
      glue::glue("**Gemeindecluster** = ", cluster),
      .)

  if (inherits(model, "clmm")) {

    modellfit <-
      modellfit %>%
      c(.,
        glue::glue("**R<sup>2</sup><sub>Konditional</sub>** = ", r2_conditional),
        glue::glue("**R<sup>2</sup><sub>Marginal</sub>** = ", r2_marginal),
        glue::glue("**ICC** = ", icc_clmm)
      )

  }

  modellfit %>%
    paste(collapse = "<br>")

}



plot_density <- function(data,
                         x,
                         area_limits = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         title = NULL,
                         subtitle = NULL) {

  g <- data %>%
    ggplot(data = ., aes(x = {{x}})) +
    geom_density(fill = '#829cb2',
                 alpha = 0.6,
                 color = '#3e6487') +
    geom_vline(xintercept = area_limits[[1]],
               color = "darkred",
               linetype = "dashed") +
    scale_x_continuous(breaks = 1:5,
                       labels = c("1\nsehr gering", "2", "3", "4", "5\nsehr hoch")) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
    crimeTools:::theme_crimeTools() +
    theme(axis.line.x.bottom = element_line(),
          axis.line.y.left = element_line(),
          axis.ticks.y.left = element_line(),
          axis.text.y.left = element_text(size = 16),
          axis.title = element_text(size = 16, face = "bold")) +
    lemon::coord_capped_cart(bottom = "both", left = "both") +
    xlab(xlab) +
    ylab(ylab) +
    labs(title = title, subtitle = subtitle)

  if (!is.null(area_limits)) {

    area_prop <-
      data %>%
      dplyr::mutate(in_area = dplyr::if_else(dplyr::between({{x}},
                                                            area_limits[[1]],
                                                            area_limits[[2]]),
                                             1,
                                             0,
                                             NA_real_)) %>%
      dplyr::pull(in_area) %>%
      sjmisc::frq(show.na = FALSE) %>%
      as.data.frame() %>%
      dplyr::filter(val == 1) %>%
      dplyr::pull(raw.prc) %>%
      sprintf("%.1f", .) %>%
      stringr::str_replace("\\.", ",") %>%
      paste0(., "%")

    g_build <- ggplot_build(g)
    g_build <- g_build$data[[1]]
    g_build <- g_build[between(g_build$x,
                               left = area_limits[[1]],
                               right = area_limits[[2]]),]

    g +
      geom_area(data = g_build, aes(x = x, y = y), fill = "#f6d2c1", color = "#e36c33") +
      geom_text(aes(x = mean(area_limits),
                    y = 0,
                    label = area_prop),
                nudge_y = 0.025,
                size = 5)

  } else g

}

plot_kriging <- function(data,
                         variogram_formula,
                         krige_formula,
                         new_data,
                         return_variogram = FALSE,
                         contour = TRUE,
                         low = "#3e6487",
                         high = "#F6D2C1", ...) {

  auto_fit <- automap::autofitVariogram(as.formula(variogram_formula), data)

  kriged <- gstat::krige(formula = as.formula(krige_formula),
                         locations = data,
                         newdata = new_data,
                         model = auto_fit$var_model) %>%
    as.data.frame()

  fill_limits <- range(kriged[["var1.pred"]])

  g <- ggplot(data = kriged, aes(x = coords.x1, y = coords.x2)) +
    geom_tile(aes(fill = var1.pred))

  if (contour) g <- g + stat_contour(aes(z = var1.pred), color = "black")

  g <- g +
    geom_point(data = as.data.frame(data),
               aes(x = coords.x1, y = coords.x2)) +
    coord_quickmap() +
    scale_fill_gradient("",
                        low = low,
                        high = high,
                        labels = scales::percent_format(scale = 1, big.mark = ".", decimal.mark = ",")) +
    theme_void() +
    theme(legend.position = "right",
          legend.key.height = unit(3.5, "cm"),
          legend.key.width = unit(0.25, "cm"),
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 16),
          legend.text = element_text(size = 14)
    ) +
    labs(...)

  if (return_variogram) list("variogram" = plot(auto_fit), "map" = g)

  else g

}



plot_mirt <- function(data, model, dim_name, legend_categories) {

  c_cluster <- parallel::makeCluster(parallel::detectCores())

  mirt_results <- parallel::clusterMap(c_cluster, function(data, model, dim_name, legend_categories){

    library(tidyverse)
    library(ggthemes)
    library(lemon)
    library(sjmisc)
    library(mirt)

    setOldClass(c("gg", "ggplot", "trellis", NULL))

    setClassUnion("SingleGroupClassOrNULL", members = c("SingleGroupClass", "NULL"))
    setClassUnion("tblOrNULL", members = c("tbl", "NULL"))
    setClassUnion("trellisOrNULL", members = c("trellis", "NULL"))
    setClassUnion("ggOrNULL", members = c("gg", "NULL"))

    set_irtClass <- setClass(Class = "irtClass",
                             slots = c(model = "SingleGroupClassOrNULL",
                                       fit_table = "tblOrNULL",
                                       itemfit_plot = "ggOrNULL",
                                       itemtrace_plot = "trellisOrNULL",
                                       itemparameter_plot = "ggOrNULL"))

    compare_models <- function(data,
                               itemtype = "gpcm",
                               dim_name = "value",
                               model,
                               legend_categories = NULL,
                               xlim = NULL,
                               ylim = NULL) {

      model_irt <- mirt::mirt(data = data, itemtype = itemtype, model = model, SE = TRUE)

      if ((ncol(data) > 3) & !is.double(model)) {

        fit_table <- anova(model_irt) %>%
          bind_cols(M2(model_irt, type = "C2", calcNULL = FALSE)) %>%
          bind_cols(model_irt@Data[c("N", "nitems")]) %>%
          dplyr::mutate(model = dim_name) %>%
          dplyr::select(model, N, nitems, AIC, BIC, logLik, RMSEA, CFI, TLI) %>%
          dplyr::rename("Items" = "nitems") %>%
          tidyr::pivot_longer(-model, names_to = "Fitindex") %>%
          tidyr::pivot_wider(names_from = model, values_from = value)

      } else {

        fit_table <- model_irt@Data[c("N", "nitems")] %>%
          append(model_irt@Fit[c("AIC", "BIC", "logLik", "RMSEA")]) %>%
          tibble::as_tibble_row() %>%
          dplyr::rename("Items" = "nitems") %>%
          tidyr::pivot_longer(everything(),
                              names_to = "Fitindex",
                              values_to = dim_name)

      }

      nudge <- max(itemfit(model_irt)[,2]) * 0.07

      itemfit_plot <-
        mirt::itemfit(model_irt)[,1:2] %>%
        ggplot(data = ., aes(y = reorder(item, S_X2), x = S_X2)) +
        geom_bar(stat = "identity", alpha = 0.5, width = 0.6) +
        geom_text(aes(label = sprintf(S_X2, fmt = "%.1f")),
                  nudge_x = nudge,
                  family = "sans") +
        geom_crossbar(aes(xmin = S_X2, xmax = S_X2, y = reorder(item, S_X2)), width = 0.6) +
        ylab("") + xlab(expression(paste("Generalisiertes S-", chi^2))) +
        theme_tufte() +
        theme(text = element_text(family = "sans"),
              axis.line.x = element_line(),
              axis.ticks.y = element_blank(),
              axis.text = element_text(size = 12),
              axis.title.x = element_text(size = 13),
              axis.text.y = element_text(face = "bold"),
              panel.grid.major.x = element_line(colour = "grey80"),
              plot.title = element_text(face = "bold", size = 18)) +
        coord_capped_cart(bottom = "both")


      if (!is.double(model)) {

        itemtrace_plot <- plot(model_irt,
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

        data <- coef(model_irt, IRTpars = TRUE) %>%
          purrr::map_dfr(., ~as_tibble(t(.x), rownames = "coeff"), .id = "var") %>%
          dplyr::filter(var %in% colnames(model_irt@Data$data)) %>%
          dplyr::mutate(coeff = str_extract(string = coeff, "[:lower:]")) %>%
          dplyr::group_by(var, coeff) %>%
          dplyr::summarise(across(par:CI_97.5, ~mean(.))) %>%
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
          ylab("Itemschwierigkeit") + xlab("Itemdiskriminanz") +
          theme_tufte() +
          theme(text = element_text(family = "sans"),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 13),
                axis.line = element_line(),
                plot.title = element_text(face = "bold", size = 18)) +
          coord_capped_cart(bottom = "both", left = "both")

      } else {

        itemtrace_plot <- NULL
        itemparameter_plot <- NULL

      }

      results <- set_irtClass(model = model_irt,
                              fit_table = fit_table,
                              itemfit_plot = itemfit_plot,
                              itemtrace_plot = itemtrace_plot,
                              itemparameter_plot = itemparameter_plot)

      return(results)

    }

    compare_models(data = na.omit(data),
                   model = model,
                   dim_name = dim_name,
                   legend_categories = legend_categories)

  },
  data = data,
  model = model,
  dim_name = dim_name,
  legend_categories = legend_categories)

  return(mirt_results)

  stopCluster(c_cluster)

}

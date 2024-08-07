to_string_pct <- function(x, dec = "0", fac = 1) paste0(stringr::str_replace(sprintf(x * fac, fmt = paste0("%.", dec, "f")), "\\.", ","), "%")

`%nin%` <- function(x, y) !match(x, y, nomatch = FALSE)

split_formula <-
  function(formula) {

      stringr::str_split(formula, pattern = "\\+| \\~ |\\||\\=~|\n|\\*|\\:=") %>%
        unlist %>%
        stringr::str_remove_all("\\(\\||\\)|\\s") %>%
        stringr::str_subset("\\*|\\(", negate = TRUE) %>%
        stringr::str_subset("^[0-9]*$", negate = TRUE) %>%
        unique()

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

    modellfit <- modellfit %>%
      c(.,
        glue::glue("**ICC** = ", icc_clmm))

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

get_loadings <- function(x, names) {

  if (!all(class(x) %in% c("fa", "psych"))) {

    stop("x must be an object of class 'fa'")

  }

  unclass(x[["loadings"]]) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::bind_cols("h2" = x[["communalities"]],
                     "u2" = x[["uniquenesses"]],
                     "com" = x[["complexity"]]) %>%
    dplyr::bind_rows(x[["Vaccounted"]] %>%
                       as.data.frame() %>%
                       tibble::rownames_to_column()) %>%
    dplyr::left_join(names, "rowname")

}

get_summary <- function(data, vars, w = NULL) {

  data %>%
    dplyr::select({{vars}}, {{w}}) %>%
    dplyr::mutate(dplyr::across({{vars}}, ~ifelse(. %in% -76:-99, NA_real_, .))) %>%
    tidyr::pivot_longer({{vars}}) %>%
    na.omit() %>%
    srvyr::as_survey_design(.data = .,
                            ids = 1,
                            weights = {{w}})  %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(Min = min(value, na.rm = TRUE),
              Max = max(value, na.rm = TRUE),
              Median = srvyr::survey_median(value, na.rm = TRUE)[[1]],
              Mittelwert = str_replace(sprintf("%.1f", srvyr::survey_mean(value, na.rm = TRUE)[[1]]), "\\.", ","),
              SD = stringr::str_replace(sprintf("%.1f", srvyr::survey_sd(value, na.rm = TRUE)[[1]]), "\\.", ","),
              N = n(),
              Fehlwerte = nrow(data) - N) %>%
    dplyr::left_join(sjlabelled::get_label(dplyr::select(data, {{vars}})) %>%
                tibble::enframe(value = "label"))


}

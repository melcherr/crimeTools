library(testthat)

skisax_weighted <- sjlabelled::read_stata("E:/Documents/Arbeitsordner/SIPS/SKiSAX/Datenanalysen/01_Daten/01_Befragungsdaten/infas_bereinigter_Rohdatensatz_7529SKiSAX_20221124.dta",
                                          convert.factors = FALSE,
                                          encoding = "utf8")
skisax_weighted <- srvyr::as_survey_design(.data = skisax_weighted, ids = 1, weights = weight)

color <- list("1" = "#3e6487",
              "2" = c("#e36c33", "#3e6487"),
              "3" = c('#3e6487', '#c7cdd1', '#e36c33'),
              "4" = c("#3e6487", "#c7cdd1", "#f6d2c1", "#e36c33"),
              "5" = c("#3e6487", "#829cb2", "#c7cdd1", "#edad88", "#e36c33"),
              "6" = c("#3e6487", "#829cb2", "#c7cdd1", "#f6d2c1", "#edad88", "#e36c33"),
              "conf_colors" = c('#3e6487', '#829cb2'))

crimeTools::plot_likert(data = skisax_weighted,
                        vars = q10a:q10m,
                        group = q43,
                        bar_color_manual = color$`5`,
                        grouping = "group_by_vars",
                        grouping_title = TRUE,
                        na_values = -88:-99,
                        na_group = -88:-99,
                        text_position = "outside_colored",
                        alpha = 0.6,
                        sort = FALSE,
                        legend.position = "bottom",
                        show_n = TRUE,
                        na_drop = TRUE)

 crimeTools::plot_likert(data = skisax_weighted,
                        vars = q10a:q10m,
                        group = q43,
                        bar_color_manual = color$`5`,
                        grouping = "group_by_vars",
                        grouping_title = TRUE,
                        na_values = -88:-99,
                        na_group = c(-88:-99, 3),
                        text_position = "outside",
                        sort_val = c(4, 5),
                        show_n = FALSE,
                        na_drop = FALSE,
                        na_label = "Fehlwerte",
                        na_plot = "graph",
                        legend = "caption")

library(testthat)
library(crimeTools)

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

plot_prevalence(data = skisax_weighted,
                vars = q20_1a:q20_1g,
                group = q52,
                grouping = "group_by_vars",
                na_group = -88:-99,
                prop_values = 1,
                sort = "desc",
                text_label_align = "right",
                legend = "caption")

plot_prevalence(data = skisax_weighted,
                vars = q10a:q10h,
                show_n = FALSE,
                group = q43,
                grouping = "vars_by_group",
                grouping_title = TRUE,
                na_group = c(-88:-99),
                na_values = c(-88:-99),
                prop_values = c(4, 5),
                sort = "desc",
                group_sort_val = 1,
                flip = FALSE,
                axis_text_width = 30,
                show_confint = FALSE,
                legend = "caption",
                graph_type = "bar")

plot_prevalence(data = skisax_weighted,
                vars = q10a:q10h,
                show_n = TRUE,
                group = q44_a2,
                na_group = c(-88:-99),
                prop_values = c(4, 5),
                sort = "desc",
                flip = FALSE,
                group_sort_val = 8,
                axis_text_width = 30,
                legend = "caption",
                graph_type = "bar")

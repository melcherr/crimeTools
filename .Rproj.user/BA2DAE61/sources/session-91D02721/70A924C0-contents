library(testthat)

skisax_weighted <- sjlabelled::read_stata("E:/Documents/Arbeitsordner/SIPS/SKiSAX/Datenanalysen/Daten/01_Befragungsdaten/infas_bereinigter_Rohdatensatz_7529SKiSAX_20221124.dta",
                                          convert.factors = FALSE,
                                          encoding = "utf8")
skisax_weighted <- srvyr::as_survey_design(.data = skisax_weighted, ids = 1, weights = weight)

plot_prevalence(data = skisax_weighted, vars = q20_1a:q20_1m, prop_values = 1, sort = "none")
plot_prevalence(data = skisax_weighted,
                vars = q10a:q10h,
                show_n = TRUE,
                group = q43,
                na_group = c(-88:-99),
                prop_values = c(4, 5),
                sort = "desc",
                flip = FALSE,
                axis_text_width = 30,
                legend = "caption",
                graph_type = "lollipop")

library(testthat)

skisax_weighted <- sjlabelled::read_stata("E:/Documents/Arbeitsordner/SIPS/SKiSAX/Datenanalysen/Daten/01_Befragungsdaten/infas_bereinigter_Rohdatensatz_7529SKiSAX_20221124.dta",
                                          convert.factors = FALSE,
                                          encoding = "utf8")
skisax_weighted <- srvyr::as_survey_design(.data = skisax_weighted, ids = 1, weights = weight)

crimeTools::plot_likert(data = skisax_weighted, vars = q10a:q10h, na_values = -88:-99, na_group = c(-88:-99, 3), na_drop = FALSE)
crimeTools::plot_likert(data = skisax_weighted,
                        vars = q10a:q10h,
                        group = q44_a2,
                        na_values = -88:-99,
                        na_group = -88:-99,
                        na_drop = FALSE,
                        na_plot = "graph",
                        show_n = TRUE,
                        sort = FALSE,
                        reverse_coding = TRUE)

#' Wrapper around tidy()
#'
#' Returns as tibble with multiple confidence intervals.
#' @param model Object of class lm, lmer or similar
#' @param conf.level confidence levels
#' @return A-tibble()-object
#' @examples
#' wrap_tidy(model) ;
#' @export

wrap_tidy <- function (model, conf.level = c(0.90, 0.95, 0.99)) {

  map(.x = conf.level,
      function(x) {

        low <- rlang::parse_expr(paste0("conf.low_", x))
        high <- rlang::parse_expr(paste0("conf.high_", x))

        if (inherits(model, "lmerMod")) {

          broom::tidy(model, conf.int = TRUE, conf.level = x) %>%
            rename({{low}} := 7, {{high}} := 8)

        } else {

          broom::tidy(model, conf.int = TRUE, conf.level = x) %>%
            dplyr::rename({{low}} := 6, {{high}} := 7)

        }

      }) %>%
    purrr::reduce(dplyr::left_join)

}

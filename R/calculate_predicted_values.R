#' Calculates Predicted Values
#'
#' Calculates Predicted Values from lmerMod and clmm objects
#' @param model, object of type lmerMod or clmm
#' @param type, whether to use link or probabilites
#' @param newata, must be a data.frame with matching variable names
#' @param add_input, logical
#' @return a Dataframe
#' @export

calculate_predicted_values <- function(model,
                                       type = "link",
                                       newdata = NULL,
                                       add_input = FALSE) {

  if (inherits(model, "lmerMod")) {

    data <- as.matrix(model@frame[,-c(1, length(model@frame))]) %>%
      cbind(rep(1, nrow(model@frame)), .)

    group <- as.character(model@frame[, length(model@frame)])

    beta <- lme4::fixef(model)
    alpha <- lme4::ranef(model) %>%
      as.data.frame() %>%
      dplyr::select(grp, condval) %>%
      dplyr::mutate(grp = as.character(grp))

    Xb <- data %*% beta

    Xb %>%
      cbind(., group) %>%
      as.data.frame() %>%
      setNames(c("pred", "grp")) %>%
      dplyr::left_join(alpha, by = "grp") %>%
      dplyr::mutate(pred = as.numeric(pred) + condval)

  }

  if (inherits(model, "clmm")) {

    data <- as.matrix(model$model[,-c(1, length(model$model))])

    alpha <- model$alpha

    beta <- model$beta


    if (!is.null(newdata)) {

      names_newdata <- names(newdata)
      names_model <- colnames(data)

      if (!any(names_newdata %in% names_model)) {

        stop("Column names in 'newdata' must match at least one column name in model formula")

      }

      if (is.data.frame(newdata)) newdata <- as.matrix(newdata)

      beta <- beta[names_newdata]

      pred <- purrr::map_dfc(.x = alpha,
                             function(x) {

                               Xb <- newdata %*% beta

                               data.frame(pred = (x - Xb)) %>%
                                 dplyr::pull(pred)

                             }) %>%
        setNames(names(alpha))

    }


    else {

      group <- as.character(model$model[, length(model$model)])

      intercepts <- ordinal::ranef.clmm(model) %>%
        as.data.frame() %>%
        dplyr::rownames_to_column() %>%
        setNames(c("grp", "condval"))

      pred <- map_dfc(.x = alpha,
                      function(x) {

                        Xb <- data %*% beta

                        Xb %>%
                          cbind(., group) %>%
                          as.data.frame() %>%
                          setNames(c("pred", "grp")) %>%
                          dplyr::left_join(intercepts, by = "grp") %>%
                          dplyr::mutate(pred = x - condval - as.numeric(pred)) %>%
                          dplyr::pull(pred)

                      }) %>%
        setNames(names(alpha))

    }

    if (type == "prob") {

      pred <- pred %>%
        sapply(pnorm) %>%
        cbind(0, ., 1) %>%
        apply(1, function(x) {

          zoo::rollapplyr(x,
                          FUN = function(y) diff(y),
                          width = 2)

        }) %>%
        t()

      colnames(pred) <- 1:(length(alpha) + 1)

      if (!is.null(newdata) & add_input) pred <- pred %>% bind_cols(newdata)

      return (pred)

    } else if (type == "link"){

      if (!is.null(newdata) & add_input) pred <- pred %>% bind_cols(newdata)

      return (pred)

    }

  }

}

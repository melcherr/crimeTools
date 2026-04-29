#' Cross-tabulation for Survey Data with srvyr
#'
#' This function creates a labelled cross-tabulation (crosstable) of survey data
#' using the `srvyr` package. It allows flexible specification of variables and
#' grouping variables, supports multiple variables and groups, and integrates
#' variable and value labels from `sjlabelled`.
#'
#' @param data A data frame or a `srvyr` survey design object.
#'   If a regular data frame is provided, it will be converted to a survey design
#'   using `srvyr::as_survey_design(ids = 1)`.
#' @param variables The variable(s) to summarize (as unquoted variable names).
#'   Can be a single variable or multiple variables (using `c(var1, var2)` or
#'   range syntax such as `var1:var3`).
#' @param group The grouping variable(s) (as unquoted variable names).
#'   Can include one or more grouping variables (e.g., `c(gender, region)`).
#' @param na_values Optional vector of values in `variables` to treat as missing (`NA`).
#' @param na_group Optional vector of values in `group` variables to treat as missing (`NA`).
#' @param fun The summary function to use for estimation, default is `srvyr::survey_prop`.
#'   Can also be functions like `srvyr::survey_mean` or `srvyr::survey_total`.
#' @param vartype Type of variance estimate to include, passed to `fun`.
#'   Common options are `"ci"` (confidence interval) or `"se"` (standard error).
#' @param envir The environment for evaluating expressions (default is the caller environment).
#' @param ... Additional arguments passed to the summary function `fun`.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Converts input data to a survey design if needed.
#'   \item Supports multiple variables and grouping factors.
#'   \item Replaces specified missing values with `NA`.
#'   \item Produces weighted proportions or other summary statistics using `srvyr`.
#'   \item Preserves variable and value labels from `sjlabelled`, and attaches them to the output.
#' }
#'
#' The resulting table includes the variable name, variable label, grouped proportions,
#' confidence intervals (if `vartype = "ci"`), and unweighted counts (`n`).
#'
#' @return A tibble with survey-weighted cross-tabulation results, including:
#' \itemize{
#'   \item `var_name` — variable name
#'   \item `var_label` — variable label
#'   \item Grouping variable columns
#'   \item `var_value` — levels of the analyzed variable
#'   \item `proportion` — estimated proportion (or other statistic)
#'   \item `n` — unweighted count
#' }
#'
#' @importFrom srvyr as_survey_design select mutate group_by summarise ungroup survey_prop survey_total
#' @importFrom rlang enquo get_expr is_call call_args expr expr_text quo_is_null sym syms ensym as_name caller_env eval_tidy
#' @importFrom dplyr rename relocate
#' @importFrom stringr str_detect
#' @importFrom purrr map map2 map_dfr
#' @importFrom sjlabelled get_label get_labels to_character set_labels drop_labels
#'
#' @examples
#' \dontrun{
#' library(srvyr)
#' library(sjlabelled)
#'
#' # Example survey data
#' data(api)
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' # Single variable crosstable by one group
#' srvyr_crosstable(
#'   data = dstrata,
#'   variables = awards,
#'   group = stype
#' )
#'
#' # Multiple variables and groups
#' srvyr_crosstable(
#'   data = dstrata,
#'   variables = c(awards, sch.wide),
#'   group = c(stype, comp.imp)
#' )
#' }
#'
#' @export

srvyr_crosstable <- function(data,
                             variables,
                             group,
                             na_values = NULL,
                             na_group = NULL,
                             fun = srvyr::survey_prop,
                             vartype = "ci",
                             envir = rlang::caller_env(),
                             ...) {

  # --- Convert data to survey design if needed ---
  if (is.data.frame(data))
    data <- srvyr::as_survey_design(.data = data, ids = 1)

  # --- Allow multiple grouping variables ---
  group_syms <- {
    grp_expr <- rlang::enquo(group)
    if (rlang::is_call(rlang::get_expr(grp_expr), "c")) {

      rlang::call_args(grp_expr)

    } else {

      list(rlang::get_expr(grp_expr))

    }
  }

  # --- Handle multi-variable expressions for 'variables' argument ---
  if (stringr::str_detect(rlang::expr_text(rlang::expr({{ variables }})), ":|,") &&
      !rlang::quo_is_null(rlang::enquo(group))) {

    parameter <- as.list(match.call())
    parameter <- parameter[which(names(parameter) %nin% c("variables"))[-1]]

    if (any(stringr::str_detect(sjlabelled::to_character(parameter$group), "\\{"))) {
      parameter <- purrr::map(parameter, ~eval(.x, envir = envir))
    }

    vars_syms <- rlang::syms(colnames(srvyr::select(data, {{ variables }})))
    names(vars_syms) <- sjlabelled::get_label(srvyr::select(data, {{ variables }}))

    purrr::map_dfr(vars_syms, function(x) {
      parameter <- append(parameter, list(variables = x))
      rlang::expr(srvyr_crosstable(!!!parameter)) %>% rlang::eval_tidy()
    })

  } else {

    # --- Select variables and groups ---
    data <- data %>%
      srvyr::select({{ variables }}, !!!group_syms)

    var_label <- sjlabelled::get_label(data$variables[[1]])
    value_labels <- sjlabelled::get_labels(data$variables, values = "n")

    data <- data %>%
      srvyr::mutate({{ variables }} := ifelse({{ variables }} %in% na_values, NA_real_, {{ variables }}),
                    srvyr::across(c(!!!group_syms), ~ ifelse(.x %in% na_group, NA_real_, .x))) %>%
      na.omit() %>%
      srvyr::group_by(!!!group_syms, {{ variables }}) %>%
      srvyr::summarise(proportion = {{ fun }}(vartype = vartype),
                       n = srvyr::survey_total()) %>%
      srvyr::ungroup() %>%
      dplyr::mutate(
        !!!unlist(purrr::map2(.x = exprs( {{ variables }}, !!!group_syms),
                              .y = value_labels,
                              \(group, value_set) {

                                rlang::quo(dplyr::across({{group}},
                                                         ~sjlabelled::set_labels(.,
                                                                                 labels = {{value_set}},
                                                                                 drop.na = TRUE)) %>%
                                             sjlabelled::as_labelled(.))

                              })
        ),
        n = round(n, 0),
        var_label = var_label,
        var_name = rlang::as_name(rlang::ensym(variables))) %>%
      sjlabelled::drop_labels() %>%
      dplyr::rename(var_value = {{ variables }}) %>%
      dplyr::relocate(var_name:var_label,
                      .before = c(!!!group_syms))

    if (length(group_syms) == 1) {

      data %>%
        dplyr::rename(group_value = !!group_syms[[1]]) %>%
        dplyr::mutate(group_label = as.character(sjlabelled::as_label(group_value)))


    } else data

  }
}

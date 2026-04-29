# ───────────────────────────────────────────────────────────────
# test-plot_likert.R
# Comprehensive test suite for plot_likert()
# ───────────────────────────────────────────────────────────────

library(testthat)
library(dplyr)
library(srvyr)
library(ggplot2)

# ---------------------------------------------------------------------
# Helper: make a minimal reproducible Likert dataset
# ---------------------------------------------------------------------

set.seed(123)

df_likert <- tibble::tibble(
  gender = factor(sample(c("Male", "Female"), 100, TRUE)),
  age_group = factor(sample(c("Young", "Middle", "Old"), 100, TRUE)),
  item1 = factor(sample(1:5, 100, TRUE), labels = paste("Level", 1:5)),
  item2 = factor(sample(1:5, 100, TRUE), labels = paste("Level", 1:5)),
  item3 = factor(sample(1:5, 100, TRUE), labels = paste("Level", 1:5))
)

survey_likert <- srvyr::as_survey_design(df_likert, ids = 1)

# ---------------------------------------------------------------------
# Begin tests
# ---------------------------------------------------------------------

test_that("plot_likert() runs on a simple data frame", {
  p <- plot_likert(data = df_likert, vars = c(item1, item2, item3))
  expect_s3_class(p, "ggplot")
})

test_that("plot_likert() runs on srvyr object", {
  p <- plot_likert(data = survey_likert, vars = c(item1, item2))
  expect_s3_class(p, "ggplot")
})

test_that("grouping by variable works", {
  p <- plot_likert(data = df_likert,
                   vars = c(item1, item2),
                   group = gender)
  expect_s3_class(p, "ggplot")
})

test_that("grouping modes (group_by_vars / vars_by_group) work", {
  p1 <- plot_likert(data = df_likert, vars = c(item1, item2),
                    group = gender, grouping = "group_by_vars")
  p2 <- plot_likert(data = df_likert, vars = c(item1, item2),
                    group = gender, grouping = "vars_by_group")
  expect_s3_class(p1, "ggplot")
  expect_true(inherits(p2, c("patchwork", "ggplot", "list")))
})

test_that("NA handling (drop vs. bar vs. graph) works", {
  tmp <- df_likert
  tmp$item1[sample(1:10)] <- NA

  # drop missing
  p_drop <- plot_likert(tmp, vars = item1, na_drop = TRUE)
  expect_s3_class(p_drop, "ggplot")

  # show as bar
  p_bar <- plot_likert(tmp, vars = item1, na_drop = FALSE, na_plot = "bar")
  expect_s3_class(p_bar, "ggplot")

  # show as graph
  p_graph <- plot_likert(tmp, vars = item1, na_drop = FALSE, na_plot = "graph")
  expect_true(inherits(p_graph, c("patchwork", "ggplot", "list")))
})

test_that("manual and auto color options work", {
  p_manual <- plot_likert(df_likert, vars = item1,
                          bar_color_manual = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))
  expect_s3_class(p_manual, "ggplot")

  p_auto <- plot_likert(df_likert, vars = item1,
                        bar_color_auto = c("red", "white", "blue"))
  expect_s3_class(p_auto, "ggplot")
})

test_that("reverse coding works", {
  p <- plot_likert(df_likert, vars = item1, reverse_coding = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("different text positions and colors work", {
  expect_s3_class(plot_likert(df_likert, vars = item1, text_position = "inside"), "ggplot")
  expect_s3_class(plot_likert(df_likert, vars = item1, text_position = "outside"), "ggplot")
  expect_s3_class(plot_likert(df_likert, vars = item1, text_position = "outside_colored"), "ggplot")
})

test_that("sorting options work", {
  expect_s3_class(plot_likert(df_likert, vars = c(item1, item2), sort = TRUE), "ggplot")
  expect_s3_class(plot_likert(df_likert, vars = c(item1, item2), sort = FALSE), "ggplot")
})

test_that("legend options work", {
  expect_s3_class(plot_likert(df_likert, vars = item1, legend = "standard"), "ggplot")
  expect_s3_class(plot_likert(df_likert, vars = item1, legend = "caption"), "ggplot")
  expect_s3_class(plot_likert(df_likert, vars = item1, legend = "none"), "ggplot")
})

test_that("invalid inputs throw clear errors", {
  expect_error(plot_likert("not_a_df", vars = item1))
  expect_error(plot_likert(df_likert, vars = "item1")) # quoted vars
  expect_error(plot_likert(df_likert, vars = item1, text_position = "weird"))
  expect_error(plot_likert(df_likert, vars = item1, legend = "banana"))
  expect_error(plot_likert(df_likert, vars = item1, grouping = "banana"))
})

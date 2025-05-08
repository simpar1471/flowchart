
test_that("errors with neither filter nor N", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_filter(fc), error = TRUE)
})

test_that("errors with both filter and N", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_filter(fc, filter = TRUE, N = 5), error = TRUE)
})

test_that("errors when text_padding is zero", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_filter(fc, filter = TRUE, text_padding = 0), error = TRUE)
  expect_snapshot(fc_filter(fc, filter = TRUE, text_padding_exc = 0), error = TRUE)
})

test_that("errors when N is too large", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_filter(fc, N = 20), error = TRUE)
})

test_that("errors with invalid label type", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_filter(fc, filter = TRUE, label = 1), error = TRUE)
})

test_that("errors with invalid label_exc type", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_filter(fc, filter = TRUE, show_exc = TRUE, label_exc = 1), error = TRUE)
})

test_that("errors when sel_group used without groups", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_filter(fc, filter = TRUE, sel_group = "A"), error = TRUE)
})

test_that("accepts valid filter expression", {
  df <- data.frame(x = 1:10, y = c(rep(TRUE, 5), rep(FALSE, 5)))
  fc <- as_fc(df)
  result <- fc_filter(fc, filter = y)
  expect_equal(nrow(result$data), 5)
  expect_equal(result$fc$n[2], 5)
})

test_that("handles show_exc parameter", {
  df <- data.frame(x = 1:10, y = c(rep(TRUE, 5), rep(FALSE, 5)))
  fc <- as_fc(df)
  result <- fc_filter(fc, filter = y, show_exc = TRUE)
  exc_rows <- result$fc |> dplyr::filter(type == "exclude")
  expect_equal(nrow(exc_rows), 1)
  expect_equal(exc_rows$n, 5)
})

test_that("preserves styling parameters", {
  fc <- as_fc(N = 10)
  result <- fc_filter(fc, N = 5,
                      text_color = "red",
                      text_fs = 12,
                      bg_fill = "yellow",
                      border_color = "blue"
  )
  new_box <- result$fc |> dplyr::filter(type == "filter")
  expect_equal(new_box$text_color, "red")
  expect_equal(new_box$text_fs, 12)
  expect_equal(new_box$bg_fill, "yellow")
  expect_equal(new_box$border_color, "blue")
})

test_that("errors informatively with nonexistent group", {
  fc <- as_fc(N = 10)
  x <- tibble::tibble(
    group = c("A", "B")
  )
  attr(fc$fc, "group") <- x$group
  expect_snapshot(
    fc_filter(fc, filter = TRUE, sel_group = "C"),
    error = TRUE
  )
})



test_that("can produce a flowchart with formatted 'N =' portions", {
  testthat::skip(message = "Skipping as only here to demonstrate changes")
  # OLD
  fc <- rbind(safo, safo) |>
    as_fc(label = expression(paste("Patients ", italic("assessed"), " for ", bold("eligibility"))),
          bold_n = TRUE) |>
    fc_filter(filter = chronic_heart_failure == "No",
              label = "No chronic heart failure",
              label_exc = "Chronic heart failure",
              text_pattern = "{n} ({perc}%)",
              text_pattern_exc = "{n} ({perc}%)",
              show_exc = TRUE) |>
    fc_draw()
  # NEW
  devtools::load_all()
  fc <- rbind(safo, safo) |>
    as_fc(label = expression(paste("Patients ", italic("assessed"), " for ", bold("eligibility"))),
          bold_n = TRUE) |>
    fc_filter(filter = chronic_heart_failure == "No",
              label = expression(paste(italic("No chronic"), bold(" heart failure"))),
              label_exc = expression(paste(bold("Chronic"), " heart failure")),
              text_pattern = expression(paste(N, " = ", bold("{n}"), "/{N}: ", italic("{perc}"), "%")),
              text_pattern_exc = expression(paste(bold("{n}"), "({perc})%")),
              show_exc = TRUE) |>
    fc_draw()
})

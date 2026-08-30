test_that("crude rates and exact Poisson intervals are calculated", {
  expect_equal(calculate_crude_rate(10, 10000), 100)
  interval <- poisson_rate_interval(10, 10000)
  expect_true(interval$lower < 100)
  expect_true(interval$upper > 100)
  expect_equal(poisson_rate_interval(0, 10000)$lower, 0)
})

test_that("invalid denominators remain missing", {
  expect_true(all(is.na(calculate_crude_rate(c(1, 1, NA), c(0, NA, 100)))))
})

test_that("non-count measures are not eligible for rates", {
  expect_true(is_count_measure("Internações"))
  expect_true(is_count_measure("Qtd.aprovada"))
  expect_false(is_count_measure("Valor total"))
  expect_false(is_count_measure("Taxa mortalidade"))
})

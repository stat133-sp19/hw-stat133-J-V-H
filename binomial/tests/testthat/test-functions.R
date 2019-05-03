context("Checking the functions of the package: binomial")

# check prob
test_that("check prob is valid and length 1", {
  expect_true(check_prob(0.4))
  expect_true(check_prob(0.95))
  expect_length(check_prob(0.5), 1)
})

# check trials
test_that("check trials is valid", {
  expect_true(check_trials(10))
  expect_true(check_trials(500000))
  expect_true(check_trials(10000))
})

# check success
test_that("check success is valid", {
  expect_true(check_success(10, 3))
  expect_true(check_success(10, 9))
  expect_length(check_success(10, 3), 1)
})

# -----------------------------------

# aux mean
test_that("aux mean valid", {
  expect_equal(aux_mean(5,0.1), 0.5)
  expect_length(aux_mean(5,0.1), 1)
  expect_equal(aux_mean(5,0.5), 2.5)
})

# aux mode
test_that("aux mode is valid", {
  expect_equal(aux_mode(5,0.1), 1)
  expect_length(aux_mean(5,0.1), 1)
  expect_equal(aux_mode(5,0.5), 3)
})

# aux var
test_that("aux variance is valid", {
  expect_equal(aux_variance(10,0.3), 2.1)
  expect_length(aux_variance(10,0.3), 1)
  expect_equal(aux_variance(11,0.4), 2.64)
})

# aux skew
test_that("aux skewness is valid", {
  expect_equal(aux_skewness(100,0.5), 0)
  expect_length(aux_skewness(10,0.3), 1)
  expect_equal(aux_skewness(10000,0.5), 0)
})

# aux kurt
test_that("aux kurtosis is valid", {
  expect_equal(aux_kurtosis(1000,0.5), -0.002)
  expect_length(aux_kurtosis(10,0.3), 1)
  expect_equal(aux_kurtosis(100,0.5), -0.02)
})

# -----------------------------------

# bin choose
test_that("bin choose works", {
  expect_equal(bin_choose(5,2), 10)
  expect_equal(bin_choose(10, 4), 210)
  expect_length(bin_choose(10, 4), 1)
})

# bin prob
test_that("bin probability works", {
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability(success = 1, trials = 2, prob = 0.01), 0.0198)
  expect_length(bin_probability(success = 2, trials = 5, prob = 0.5), 1)
})


# bin dist
test_that("bin distribution is valid ", {
  expect_length(bin_distribution(trials=10,prob=0.7), 2)
  expect_length(bin_distribution(trials=95,prob=0.5), 2)
  expect_length(bin_distribution(trials=5,prob=0.5), 2)
})

# bin cumul
test_that("bin cumulative is valid", {
  expect_length(bin_cumulative(10,0.7), 3)
  expect_length(bin_cumulative(95,0.5), 3)
  expect_length(bin_cumulative(5,0.5), 3)
})



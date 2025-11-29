# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

# Prevent the creation of Rplots.pdf file
pdf(NULL)

test_that('pbc works', {
  expect_no_error(pbc(csection$avg_delay))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection,
                      chart = 'i'))
  expect_no_error(pbc(month, ontime, cases,
                      data = ontime_ct,
                      chart = 'i'))
  expect_no_error(pbc(month, ontime, cases,
                      data = ontime_ct,
                      chart = 'ms'))
})

test_that('Faceting works', {
  # one-way faceting
  expect_no_error(pbc(month, deaths, cases, hospital,
                      data = bacteremia_mortality))
  expect_no_error(pbc(month, deaths, cases, hospital,
                      data = bacteremia_mortality,
                      yfixed = F))
  expect_no_error(pbc(month, deaths, cases, hospital,
                      data = bacteremia_mortality,
                      chart = 'i'))
  expect_no_error(pbc(month, deaths, cases, hospital,
                      data = bacteremia_mortality,
                      chart = 'i',
                      title = 'Bacteremia mortality',
                      multiply = 100,
                      ylab = '%',
                      xlab = 'Month',
                      ylim = c(0, NA)))
  # two-way faceting
  expect_no_error(pbc(qrt, avg_days,
                      facet = list(region, operation),
                      data = waiting_times))
})

test_that('freeze argument works', {
  expect_no_error(pbc(month, deaths, facet = hospital,
                      data = bacteremia_mortality,
                      chart = 'i',
                      freeze = 12,
                      yfixed = FALSE))

  expect_no_error(pbc(month, ontime,
                      data = ontime_ct,
                      chart = 'i',
                      freeze = 12))
  expect_no_error(pbc(month, ontime, cases,
                      data = ontime_ct,
                      chart = 'i',
                      freeze = 12))
  expect_message(pbc(month, ontime, cases,
                     data = ontime_ct,
                     chart = 'i',
                     freeze = 24))
  expect_message(pbc(month, ontime, cases,
                     data = ontime_ct,
                     chart = 'i',
                     freeze = 1))
})

test_that('split argument works', {
  expect_no_error(pbc(month, deaths, facet = hospital,
                      data = bacteremia_mortality,
                      chart = 'i',
                      split = 12,
                      yfixed = FALSE))

  expect_no_error(pbc(month, ontime,
                      data = ontime_ct,
                      chart = 'i',
                      split = 12))
  expect_no_error(pbc(month, ontime, cases,
                      data = ontime_ct,
                      chart = 'i',
                      split = 12))
  expect_message(pbc(month, ontime, cases,
                     data = ontime_ct,
                     chart = 'i',
                     split = 24))
  expect_message(pbc(month, ontime, cases,
                     data = ontime_ct,
                     chart = 'i',
                     split = 1))
})

test_that('signals and summary work', {
  expect_equal(
    summary(pbc(1:12, chart = 'i'))$sigma.signal, 6)
  expect_equal(
    summary(pbc(1:12, chart = 'i'))$runs.signal, 1)
  expect_equal(
    summary(pbc(1:11, chart = 'i'))$n.obs, 11)
  expect_equal(
    summary(pbc(1:11, chart = 'i'))$n.useful, 10)
  expect_equal(
    summary(pbc(-6:6, chart = 'i'))$cl, 0)
  expect_equal(
    summary(pbc(-6:6, chart = 'i'))$avg_ucl, 2.66,
    tolerance = 0.005)
  expect_equal(
    summary(pbc(-6:6, chart = 'i'))$sigma.signal, 8,
    tolerance = 0.005)
  expect_equal(
    summary(pbc(-6:6, chart = 'i', exclude = c(1, 13)))$sigma.signal, 6,
    tolerance = 0.005)
  expect_equal(summary(pbc(month, avg_hba1c,
                           data = hba1c,
                           chart = 'i'))$avg_ucl, 65.66,
               tolerance = 0.005)
  expect_equal(summary(pbc(month, avg_hba1c,
                           data  = hba1c,
                           chart = 'i'))$sigma.signal, 1,
               tolerance = 0.005)
  expect_equal(summary(pbc(month, avg_hba1c * n, n,
                           data  = hba1c,
                           chart = 'i'))$avg_ucl, 65.15,
               tolerance = 0.005)
  expect_equal(summary(pbc(month, avg_hba1c * n, n,
                           data  = hba1c,
                           chart = 'i'))$sigma.signal, 0,
               tolerance = 0.005)
})

test_that('screenms argument works', {
  set.seed(44)
  y <- rnorm(24)

  expect_equal(
    summary(pbc(y, chart = 'ms'))$sigma.signal, 1)
  expect_equal(
    summary(pbc(y, chart = 'i'))$sigma.signal, 0)
  expect_equal(
    summary(pbc(y, chart = 'i', screenms = TRUE))$sigma.signal, 1)
})

test_that('cl and sd arguments work', {
  expect_no_error(pbc(rnorm(12), chart = 'i', cl = 0, sd = 1))
})

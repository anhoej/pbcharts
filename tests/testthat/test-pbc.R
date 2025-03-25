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
                     spli = 24))
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
    summary(pbc(1:11, chart = 'i'))$n, 11)
  expect_equal(
    summary(pbc(1:11, chart = 'i'))$n.useful, 10)
  expect_equal(
    summary(pbc(-6:6, chart = 'i'))$cl, 0)
  expect_equal(
    summary(pbc(-6:6, chart = 'i'))$avg_ucl, 2.66,
    tolerance = 0.005)
})

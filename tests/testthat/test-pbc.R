# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

# Prevent the creation of Rplots.pdf file
pdf(NULL)

test_that('pbc works', {
  expect_no_error(pbc(csection$avg_delay))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection))
  expect_no_error(pbc(month, avg_delay, n,
                      data = csection,
                      chart = 'i'))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection,
                      chart = 'i'))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection,
                      freeze = 6))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection,
                      freeze = 6,
                      chart = 'i'))
  expect_no_error(pbc(month, ontime, cases,
                      data = ontime_ct,
                      chart = 'i'))
  expect_no_error(pbc(month, deaths, cases, hospital,
                      data = bacteremia_mortality))
  expect_no_error(pbc(month, deaths, cases, hospital,
                      data = bacteremia_mortality,
                      fixedscales = F))
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

test_that('signals work', {
  expect_equal(
    summary(pbc(1:12, chart = 'i'))$sigma.signal, 6)
  expect_equal(
    summary(pbc(1:12, chart = 'i'))$runs.signal, 1)
})

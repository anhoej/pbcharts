# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that('pbc works', {
  expect_no_error(pbc(csection$avg_delay))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection))
  expect_no_error(pbc(month, avg_delay, n,
                      data = csection, chart = 'i'))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection, chart = 'i'))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection, freeze = 6))
  expect_no_error(pbc(month, avg_delay * n, n,
                      data = csection, freeze = 6, chart = 'i'))
}
)

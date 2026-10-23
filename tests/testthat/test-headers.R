test_that("token is sent on every request, nothing else leaks", {
  opal <- list(csrf = "c", token = NULL)
  expect_equal(opalr:::.headers(opal)$headers, c('X-XSRF-TOKEN' = "c"))
  opal$token <- "t"
  expect_equal(opalr:::.headers(opal)$headers, c('X-XSRF-TOKEN' = "c", 'X-Opal-Auth' = "t"))
})

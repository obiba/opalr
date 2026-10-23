# .opal.login with no credentials warns on the url check then stops on the auth check: no network involved.
test_that("plain http is refused unless localhost or insecure.http", {
  login <- function(url, opts = list()) opalr:::.opal.login(NA, NA, NA, url, opts = opts, profile = NULL)
  expect_error(login("http://proxy:8000"), "secure http is required")
  expect_error(expect_warning(login("http://localhost:8080"), "non-secure http"), "authentication strategy")
  expect_error(expect_warning(login("http://proxy:8000", list(insecure.http = TRUE)), "non-secure http"), "authentication strategy")
  old <- options(opal.insecure.http = TRUE); on.exit(options(old))
  expect_error(expect_warning(login("http://proxy:8000"), "non-secure http"), "authentication strategy")
})

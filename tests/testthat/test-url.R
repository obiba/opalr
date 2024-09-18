test_that("URL are cleaned", {
  expected <- "https://opal.example.org/ws/files/some/path/to/file"
  expect_equal(opalr:::.cleanUrl("https://opal.example.org/ws/files/some///path/to//file"), expected)
  expected <- "http://localhost:8080/ws/files/some/path/to/file"
  expect_equal(opalr:::.cleanUrl("http://localhost:8080/ws/files/some///path/to//file"), expected)
})
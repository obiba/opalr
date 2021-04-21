# Setup
#options(opal.url = "http://localhost:8080")
#options(opal.url = "https://opal-demo.obiba.org")

check_skip <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if(is.null(getOption("opal.url")), "Skipping tests because Opal url is not defined")
}

make_test_dataset <- function() {
  df <- mtcars
  df$id <- 1:nrow(df)
  df$name <- rownames(df)
  df <- tibble::as_tibble(df)
  df
}

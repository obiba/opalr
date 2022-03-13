# Setup
#options(opal.url = "https://localhost:8443")
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

make_test_dataset_with_repeatables <- function() {
  data <- tibble::tibble(
    id = c(1,2,3,1,1,2,2,3,3,4),
    id2 = c(1,2,3,4,5,6,7,8,9,NA),
    sex = c(NA,"M","F","M","M",NA,"F","F","M",NA),
    var1 = c(7.3,1, NA, 2.3, 1.4,NA, 2.4, 5.4, -99,NA)
  )
}

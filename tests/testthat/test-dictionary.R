.makeTestDataset <- function() {
  data <- tibble::as_tibble(mtcars)
  data$id <- as.character(1:nrow(data))
  variables <- tibble::tribble(
    ~name, ~valueType, ~`label:en`,  ~`description:en`, ~`Namespace::Name`, ~`Namespace::Name2:en`, ~unit, ~repeatable, ~index,
    "mpg", "decimal", "Mpg label",  "Mpg description", "Value1", "ValueA", "years", 0, 1,
    "cyl", "decimal", "Cyl label",  "Cyl description", "Value2", "ValueB", "kg/m2", 0, 2,
    "disp", "decimal", "Disp label", "Disp description", NA, NA, NA, 1, 3
  )
  categories <- tibble::tribble(
    ~variable, ~name, ~missing, ~`label:en`, ~`label:fr`,
    "cyl", "4", 0, "Four", "Quatre",
    "cyl", "6", 0, "Six", "Six",
    "cyl", "8", 1, "Height", "Huit"
  )
  list(data = data, variables = variables, categories = categories)
}

test_that("Dico applied to tibble", {
  dataset <- .makeTestDataset()
  data <- dataset$data
  variables <- dataset$variables
  categories <- dataset$categories
  
  expect_warning(data <- dictionary.apply(data, variables, categories))
  attrs <- attributes(data$cyl)
  attrs
  expect_equal(attrs$label, "(en) Cyl label")
  expect_equal(attrs$description, "(en) Cyl description")
  expect_equal(attrs$`Namespace::Name`, "Value2")
  expect_equal(attrs$`Namespace::Name2:en`, "ValueB")
  expect_equal(attrs$opal.unit, "kg/m2")
  expect_equal(attrs$opal.repeatable, 0)
  expect_equal(attrs$opal.index, 2)
  expect_true("double" %in% class(data$cyl))
  expect_true("haven_labelled" %in% class(data$cyl))
  expect_false(is.null(attrs$labels))
  expect_equal(length(attrs$labels), 3)
  expect_equal(attrs$labels, c("(en) Four"=4, "(en) Six"=6, "(en) Height"=8))
})

test_that("Dico to JSON", {
  dataset <- .makeTestDataset()
  json <- .toJSONVariables(variables = dataset$variables, categories = dataset$categories)
  #print(json)
  expect_equal(as.character(json), '[{"name":"mpg","valueType":"decimal","entityType":"Participant","unit":"years","isRepeatable":false,"index":1,"attributes":[{"name":"label","locale":"en","value":"Mpg label"},{"name":"description","locale":"en","value":"Mpg description"},{"namespace":"Namespace","name":"Name","value":"Value1"},{"namespace":"Namespace","name":"Name2","locale":"en","value":"ValueA"}]},{"name":"cyl","valueType":"decimal","entityType":"Participant","unit":"kg/m2","isRepeatable":false,"index":2,"attributes":[{"name":"label","locale":"en","value":"Cyl label"},{"name":"description","locale":"en","value":"Cyl description"},{"namespace":"Namespace","name":"Name","value":"Value2"},{"namespace":"Namespace","name":"Name2","locale":"en","value":"ValueB"}],"categories":[{"name":"4","isMissing":false,"attributes":[{"name":"label","locale":"en","value":"Four"},{"name":"label","locale":"fr","value":"Quatre"}]},{"name":"6","isMissing":false,"attributes":[{"name":"label","locale":"en","value":"Six"},{"name":"label","locale":"fr","value":"Six"}]},{"name":"8","isMissing":true,"attributes":[{"name":"label","locale":"en","value":"Height"},{"name":"label","locale":"fr","value":"Huit"}]}]},{"name":"disp","valueType":"decimal","entityType":"Participant","isRepeatable":true,"index":3,"attributes":[{"name":"label","locale":"en","value":"Disp label"},{"name":"description","locale":"en","value":"Disp description"}]}]')
})

test_that("Dico inspection: IDs", {
  patients <- tibble::tribble(
    ~id, ~visit_id, ~sex, ~visit_date,
    1, 1, "M", as.Date("2020-01-01"),
    2, 2, "F", as.Date("2020-01-02"),
    3, 3, "M", as.Date("2020-01-03"))
  expect_true(dictionary.inspect(patients, id.name = "id"))
  expect_error(dictionary.inspect(patients, id.name = "x"))
  # multilines
  patients <- tibble::tribble(
    ~id, ~visit_id, ~sex, ~visit_date,
    1, 1, "M", as.Date("2020-01-01"),
    2, 2, "F", as.Date("2020-01-02"),
    3, 3, "M", as.Date("2020-01-03"),
    3, 4, "M", as.Date("2020-01-04"))
  expect_true(dictionary.inspect(patients, id.name = "id"))
  # missing ids
  patients <- tibble::tribble(
    ~id, ~visit_id, ~sex, ~visit_date,
    1, 1, "M", as.Date("2020-01-01"),
    2, 2, "F", as.Date("2020-01-02"),
    3, 3, "M", as.Date("2020-01-03"),
    NA, 4, "M", as.Date("2020-01-04"))
  expect_error(dictionary.inspect(patients, id.name = "id"))
  patients <- tibble::tribble(
    ~id, ~visit_id, ~sex, ~visit_date,
    1, 1, "M", as.Date("2020-01-01"),
    2, 2, "F", as.Date("2020-01-02"),
    3, 3, "M", as.Date("2020-01-03"),
    NULL, 4, "M", as.Date("2020-01-04"))
  expect_error(dictionary.inspect(patients, id.name = "id"))
  patients <- tibble::tribble(
    ~id, ~visit_id, ~sex, ~visit_date,
    "1", 1, "M", as.Date("2020-01-01"),
    "2", 2, "F", as.Date("2020-01-02"),
    "3", 3, "M", as.Date("2020-01-03"),
    "", 4, "M", as.Date("2020-01-04"))
  expect_error(dictionary.inspect(patients, id.name = "id"))
})

test_that("Dico inspection: multilines", {
  # multilines
  patients <- tibble::tribble(
    ~id, ~visit_id, ~sex, ~visit_date,
    1, 1, "M", as.Date("2020-01-01"),
    2, 2, "F", as.Date("2020-01-02"),
    3, 3, "M", as.Date("2020-01-03"),
    3, 4, "M", as.Date("2020-01-04"))
  attributes(patients$visit_date)$opal.repeatable <- 0  
  expect_warning(dictionary.inspect(patients, id.name = "id"))
  # no multilines
  visits <- tibble::tribble(
    ~id, ~patient_id, ~sex, ~visit_date,
    1, 1, "M", as.Date("2020-01-01"),
    2, 2, "F", as.Date("2020-01-02"),
    3, 3, "M", as.Date("2020-01-03"),
    4, 3, "M", as.Date("2020-01-04"))
  expect_true(dictionary.inspect(visits))
  attributes(visits$visit_date)$opal.repeatable <- 0
  expect_true(dictionary.inspect(visits))
})

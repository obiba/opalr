test_that("Token Datashield creation", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  #opal.tokens(o)
  name <- "ds-xxx"
  opal.token_delete(o, name)
  val <- opal.token_datashield_create(o, name, projects = c("CNSIM1", "CNSIM2"))
  expect_true(nchar(val) == 32)
  expect_error(opal.token_datashield_create(o, name))
  tk <- opal.token(o, name)
  expect_equal(tk$access, 'READ_NO_VALUES')
  expect_null(tk$commands)
  expect_true(tk$useDatashield)
  expect_false(tk$useR)
  expect_false(tk$useSQL)
  opal.token_delete(o, name)
  expect_null(opal.token(o, name))
  
  opal.logout(o)
})

test_that("Token R creation", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  name <- "r-xxx"
  opal.token_delete(o, name)
  val <- opal.token_r_create(o, name, access = 'READ')
  expect_true(nchar(val) == 32)
  expect_error(opal.token_r_create(o, name))
  tk <- opal.token(o, name)
  expect_equal(tk$commands, list('export'))
  expect_false(tk$useDatashield)
  expect_true(tk$useR)
  expect_false(tk$useSQL)
  opal.token_delete(o, name)
  expect_null(opal.token(o, name))
  
  opal.logout(o)
})

test_that("Token SQL creation", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  name <- "sql-xxx"
  opal.token_delete(o, name)
  val <- opal.token_sql_create(o, name)
  expect_true(nchar(val) == 32)
  expect_error(opal.token_sql_create(o, name))
  tk <- opal.token(o, name)
  expect_null(tk$commands)
  expect_false(tk$useDatashield)
  expect_false(tk$useR)
  expect_true(tk$useSQL)
  opal.token_delete(o, name)
  expect_null(opal.token(o, name))
  
  opal.logout(o)
})
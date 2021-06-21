
test_that("Project permissions", {
  check_skip()
  #skip("Temporary skip")
  
  name <- "testthat"
  o <- opal.login("administrator", "password")
  
  if (opal.project_exists(o, name))
    opal.project_delete(o, name)
  opal.project_create(o, name)
  expect_true(opal.project_exists(o, name))
  project <- opal.project(o, name)
  expect_null(project$database)
  expect_equal(project$datasourceStatus, "NONE")
  opal.project_delete(o, name)
  expect_error(opal.project(o, name))
  
  dbs <- opal.projects_databases(o)
  expect_true(length(dbs)>0)
  expect_error(opal.project_create(o, name, database = "dummy"))
  opal.project_create(o, name, database = dbs[1])
  expect_true(opal.project_exists(o, name))
  project <- opal.project(o, name)
  expect_equal(project$database, dbs[1])
  expect_equal(project$datasourceStatus, "READY")
  opal.project_delete(o, name)
  
  opal.logout(o)
  
})

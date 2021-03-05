test_that("Table export", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  # check export of each supported formats
  expect_true(opal.table_exists(o, "CNSIM", "CNSIM1"))
  for (format in c("rds", "sav", "zsav", "sas7bdat", "xpt", "dta")) {
    localfile <- tempfile(fileext = paste0(".", format))
    opalfile <- paste0("/tmp/", basename(localfile))
    opal.table_export(o, "CNSIM", "CNSIM1", file = opalfile)
    opal.file_download(o, opalfile, localfile)
    opal.file_rm(o, opalfile)
    expect_true(file.exists(localfile))
    expect_true(file.size(localfile)>0)
    unlink(localfile) 
  }
  
  # check no R session created
  expect_null(o$rid)
  
  opal.logout(o)
})

test_that("Table import", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  ds <- make_test_dataset()
  localfile <- tempfile(fileext = ".rds")
  saveRDS(ds, localfile)
  opalfile <- paste0("/tmp/", basename(localfile))
  opal.file_upload(o, source = localfile, destination = "/tmp")
  unlink(localfile)
  if (opal.table_exists(o, "RSRC", "mtcars")) {
    opal.table_delete(o, "RSRC", "mtcars")
  }
  opal.table_import(o, file = opalfile, project = "RSRC", table = "mtcars")
  opal.file_rm(o, opalfile)
  expect_true(opal.table_exists(o, "RSRC", "mtcars"))
  tinfo <- opal.table(o, "RSRC", "mtcars", counts = TRUE)
  expect_equal(tinfo$variableCount, 12)
  expect_equal(tinfo$valueSetCount, 32)
  opal.table_delete(o, "RSRC", "mtcars")
  
  # check no R session created
  expect_null(o$rid)
  
  opal.logout(o)
})


test_that("Table save and get", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  ds <- make_test_dataset()
  opal.table_save(o, ds, "RSRC", "mtcars", force = TRUE)
  expect_true(opal.table_exists(o, "RSRC", "mtcars"))
  ds2 <- opal.table_get(o, "RSRC", "mtcars")
  expect_equal(nrow(ds2), nrow(ds))
  expect_equal(ncol(ds2), ncol(ds))
  expect_equal(summary(ds2$cyl), summary(ds$cyl))
  expect_equal(summary(as.factor(ds2$vs)), summary(as.factor(ds$vs)))
  expect_true(all(colnames(ds) %in% colnames(ds2)))
  opal.table_delete(o, "RSRC", "mtcars")
  
  # check no R session created
  expect_null(o$rid)
  
  opal.logout(o)
})
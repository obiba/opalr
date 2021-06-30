test_that("Project creation", {
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

test_that("Project backup and restore", {
  check_skip()
  #skip("Temporary skip")
  
  name <- "testthat"
  project <- "GREENSPACE"
  archive <- "/home/administrator/backup-testthat/"
  o <- opal.login("administrator", "password")
  
  # backup
  opal.file_rm(o, archive)
  opal.project_backup(o, project, archive = paste0(archive, project), override = TRUE, wait = TRUE)
  ls <- opal.file_ls(o, archive)
  expect_true(nrow(ls)>0)
  expect_equal(ls[1,]$name, project)
  expect_equal(ls[1,]$type, "FOLDER")
  ls <- opal.file_ls(o, paste0(archive, project))
  expect_true(nrow(ls)>0)
  expect_equal(ls$name, c("files", "resources", "tables"))
  expect_equal(ls$type, replicate(3, "FOLDER"))
  # error when cannot override
  expect_error(opal.project_backup(o, project, archive = paste0(archive, project), override = FALSE, wait = TRUE))
  # download as a zip
  tmp <- tempfile(fileext = ".zip")
  opal.file_download(o, paste0(archive, project), tmp)
  expect_true(file.exists(tmp))
  # upload zip
  opal.file_upload(o, tmp, "/tmp")
  ls <- opal.file_ls(o, paste0("/tmp/", basename(tmp)))
  expect_equal(nrow(ls), 1)
  expect_true(ls[1,]$size>0)
  unlink(tmp)
  
  # restore
  archive <- paste0("/tmp/", basename(tmp))
  if (opal.project_exists(o, name))
    opal.project_delete(o, name)
  opal.project_create(o, name, database = TRUE)
  opal.project_restore(o, name, archive)
  expect_equal(opal.tables(o, name)$name, opal.tables(o, project)$name)
  expect_equal(opal.resources(o, name)$name, opal.resources(o, project)$name)
  opal.project_delete(o, name)
  # error when restore in a project without database
  opal.project_create(o, name)
  expect_error(opal.project_restore(o, name, archive))
  
  # clean up
  opal.project_delete(o, name)
  opal.file_rm(o, archive)
  
  opal.logout(o)
})

test_that("System permissions", {
  check_skip()
  #skip("Temporary skip")
  
  name <- "testthat"
  o <- opal.login("administrator", "password")
  
  test_permission <- function(permission) {
    oadmin.system_perm_add(o, name, permission = permission)
    perms <- oadmin.system_perm(o)
    perm <- perms[perms$subject == name, ]
    expect_equal(perm$subject, name)
    expect_equal(perm$type, "user")
    expect_equal(perm$permission, permission)
    oadmin.system_perm_delete(o, name)
    perms <- oadmin.system_perm(o)
    expect_false(name %in% perms$subject)
  }

  test_permission("project_add")
  test_permission("administrate")
  expect_error(test_permission("dummy"))
    
  opal.logout(o)
  
})

test_that("R permissions", {
  check_skip()
  #skip("Temporary skip")
  
  name <- "testthat"
  o <- opal.login("administrator", "password")
  
  test_permission <- function(permission) {
    oadmin.r_perm_add(o, name, permission = permission)
    perms <- oadmin.r_perm(o)
    perm <- perms[perms$subject == name, ]
    expect_equal(perm$subject, name)
    expect_equal(perm$type, "user")
    expect_equal(perm$permission, permission)
    oadmin.r_perm_delete(o, name)
    perms <- oadmin.r_perm(o)
    expect_false(name %in% perms$subject)
  }
  
  test_permission("use")
  expect_error(test_permission("dummy"))
  
  opal.logout(o)
  
})

test_that("Project permissions", {
  check_skip()
  #skip("Temporary skip")
  
  name <- "testthat"
  o <- opal.login("administrator", "password")
  
  if (opal.project_exists(o, name))
    opal.project_delete(o, name)
  opal.project_create(o, name)
  
  # creator has permission to administrate
  perms <- opal.project_perm(o, name)
  expect_equal(nrow(perms), 1)
  expect_equal(perms[1,]$subject, "administrator")
  expect_equal(perms[1,]$type, "user")
  expect_equal(perms[1,]$permission, "administrate")
  
  test_permission <- function(subject, permission) {
    opal.project_perm_add(o, name, subject = subject, permission = permission)
    perms <- opal.project_perm(o, name)
    perm <- perms[perms$subject == subject, ]
    expect_equal(perm$subject, subject)
    expect_equal(perm$type, "user")
    expect_equal(perm$permission, permission)
    opal.project_perm_delete(o, name, subject = subject)
    perms <- opal.project_perm(o, name)
    expect_false(subject %in% perms$subject)
  }
  
  test_permission(name, "administrate")
  expect_error(test_permission("dummy"))

  opal.project_delete(o, name)
  expect_error(opal.project_perm(o, name))
  
  opal.logout(o)
  
})

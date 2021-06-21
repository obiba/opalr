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


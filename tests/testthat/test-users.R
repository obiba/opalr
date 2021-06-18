test_that("Users management", {
  check_skip()
  #skip("Temporary skip")
  
  name <- "testthat"
  o <- opal.login("administrator", "password")
  if (oadmin.user_exists(o, name))
    oadmin.user_rm(o, name)
  pwd <- oadmin.user_add(o, name, groups = c("a", "b"))
  expect_false(is.null(pwd))
  expect_true(nchar(pwd)>10)
  expect_true(oadmin.user_exists(o, name))
  users <- oadmin.users(o)
  user <- as.list(users[users$name == name,])
  expect_equal(user$groups, paste0(c("a", "b"), collapse = ", "))
  oadmin.user_rm(o, name)
  pwd <- oadmin.user_add(o, name)
  users <- oadmin.users(o)
  user <- as.list(users[users$name == name,])
  expect_equal(user$groups, "")
  opal.logout(o)
  
  o <- opal.login(name, pwd)
  expect_equal(o$uprofile$principal, name)
  expect_equal(o$uprofile$realm, "opal-user-realm")
  opal.logout(o)

  # reset password
  o <- opal.login("administrator", "password")
  expect_error(oadmin.user_reset_password(o, name, password = "123"))
  pwd2 <- oadmin.user_reset_password(o, name)
  expect_false(pwd == pwd2)
  opal.logout(o)
  
  o <- opal.login(name, pwd2)
  expect_equal(o$uprofile$principal, name)
  expect_equal(o$uprofile$realm, "opal-user-realm")
  opal.logout(o)
  
  # disable
  o <- opal.login("administrator", "password")
  oadmin.user_enable(o, name, FALSE)
  opal.logout(o)
  
  expect_error(opal.login(name, pwd2))
  
  # clean up
  o <- opal.login("administrator", "password")
  oadmin.user_rm(o, name)
  expect_false(oadmin.user_exists(o, name))
  opal.logout(o)
  
})

test_that("DataSHIELD profile permissions", {
  check_skip()
  #skip("Temporary skip")
  
  o <- opal.login("administrator", "password")
  expect_true(dsadmin.profile_exists(o, 'default'))
  
  name <- 'testthat'
  if (dsadmin.profile_exists(o, name))
    dsadmin.profile_delete(o, name)
  expect_false(dsadmin.profile_exists(o, name))
  dsadmin.profile_create(o, name)
  expect_true(dsadmin.profile_exists(o, name))
  profile <- dsadmin.profile(o, name)
  expect_false(profile$enabled)
  expect_false(profile$restrictedAccess)
  
  perms <- dsadmin.profile_perm(o, name)
  expect_equal(nrow(perms), 0)
  
  dsadmin.profile_perm_add(o, name, c('andrei', 'valentina'), 'user', 'use')
  profile <- dsadmin.profile(o, name)
  expect_true(profile$restrictedAccess)
  perms <- dsadmin.profile_perm(o, name)
  expect_equal(nrow(perms), 2)
  
  dsadmin.profile_perm_delete(o, name, c('andrei'), 'user')
  perms <- dsadmin.profile_perm(o, name)
  expect_equal(nrow(perms), 1)
  profile <- dsadmin.profile(o, name)
  expect_true(profile$restrictedAccess)
  
  dsadmin.profile_enable(o, name, enabled = TRUE)
  dsadmin.profile_access(o, name, FALSE)
  profile <- dsadmin.profile(o, name)
  expect_true(profile$enabled)
  expect_false(profile$restrictedAccess)
  perms <- dsadmin.profile_perm(o, name)
  expect_equal(nrow(perms), 0)
  
  dsadmin.profile_delete(o, name)
  
  opal.logout(o)
})
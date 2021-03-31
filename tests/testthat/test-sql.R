test_that("Table SQL", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  df <- opal.sql(o, 'CNSIM', 'select avg(LAB_HDL) as HDL_AVG, GENDER from CNSIM1 where LAB_HDL is not null group by GENDER')
  expect_equal(df$HDL_AVG, c(1.5170, 1.6223))
  expect_equal(df$GENDER, c("0", "1"))
  
  # table not found
  expect_error(opal.sql(o, 'CNSIM', 'select * from CNSIM4'))
  # SQL syntax error
  expect_error(opal.sql(o, 'CNSIM', 'select from CNSIM1'))
  
  # check no R session created
  expect_null(o$rid)
  
  opal.logout(o)
})

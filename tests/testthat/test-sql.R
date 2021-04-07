test_that("Table SQL with project context", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  df <- opal.sql(o, 'select avg(LAB_HDL) as HDL_AVG, GENDER from CNSIM1 where LAB_HDL is not null group by GENDER', 'CNSIM')
  expect_equal(df$HDL_AVG, c(1.517015, 1.622344), tolerance = 0.000001)
  expect_equal(df$GENDER, as.factor(c("0", "1")))
  
  # table not found
  expect_error(opal.sql(o, 'select * from CNSIM4', 'CNSIM'))
  # SQL syntax error
  expect_error(opal.sql(o, 'select from CNSIM1', 'CNSIM'))
  
  # check no R session created
  expect_null(o$rid)
  
  opal.logout(o)
})

test_that("Table SQL without project context", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  df <- opal.sql(o, 'select avg(LAB_HDL) as HDL_AVG, GENDER from `CNSIM.CNSIM1` where LAB_HDL is not null group by GENDER')
  expect_equal(df$HDL_AVG, c(1.517015, 1.622344), tolerance = 0.000001)
  expect_equal(df$GENDER, as.factor(c("0", "1")))
  
  # table not found
  expect_error(opal.sql(o, 'select * from `CNSIM.CNSIM4`'))
  # SQL syntax error
  expect_error(opal.sql(o, 'select from `CNSIM.CNSIM1`'))
  
  # check no R session created
  expect_null(o$rid)
  
  opal.logout(o)
})
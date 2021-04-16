test_that("Table SQL with project context", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")

  q <- 'select avg(LAB_HDL) as HDL_AVG, GENDER from CNSIM1 where LAB_HDL is not null group by GENDER'
  df <- opal.sql(o, q, 'CNSIM')
  expect_equal(df$HDL_AVG, c(1.517015, 1.622344), tolerance = 0.000001)
  expect_equal(df$GENDER, as.factor(c("0", "1")))
  h <- opal.sql_history(o, limit = 1)
  expect_equal(h[[1,1]], "administrator")
  expect_equal(h[[1,2]], q)
  expect_equal(h[[1,3]], 'CNSIM')
  expect_true(is.na(h[[1,4]]))
  
  # table not found
  q <- 'select * from CNSIM4'
  expect_error(opal.sql(o, q, 'CNSIM'))
  h <- opal.sql_history(o, limit = 1)
  expect_equal(h[[1,1]], "administrator")
  expect_equal(h[[1,2]], q)
  expect_equal(h[[1,3]], 'CNSIM')
  expect_false(is.na(h[[1,4]]))
  # SQL syntax error
  q <- 'select from CNSIM1'
  expect_error(opal.sql(o, q, 'CNSIM'))
  h <- opal.sql_history(o, limit = 1)
  expect_equal(h[[1,1]], "administrator")
  expect_equal(h[[1,2]], q)
  expect_equal(h[[1,3]], 'CNSIM')
  expect_false(is.na(h[[1,4]]))
  
  # check no R session created
  expect_null(o$rid)
  
  opal.logout(o)
})

test_that("Table SQL without project context", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")
  
  q <- 'select avg(LAB_HDL) as HDL_AVG, GENDER from `CNSIM.CNSIM1` where LAB_HDL is not null group by GENDER'
  df <- opal.sql(o, q)
  expect_equal(df$HDL_AVG, c(1.517015, 1.622344), tolerance = 0.000001)
  expect_equal(df$GENDER, as.factor(c("0", "1")))
  h <- opal.sql_history(o, limit = 1)
  expect_equal(h[[1,1]], "administrator")
  expect_equal(h[[1,2]], q)
  expect_true(is.na(h[[1,3]]))
  expect_true(is.na(h[[1,4]]))
  
  # table not found
  q <- 'select * from `CNSIM.CNSIM4`'
  expect_error(opal.sql(o, q))
  h <- opal.sql_history(o, limit = 1)
  expect_equal(h[[1,1]], "administrator")
  expect_equal(h[[1,2]], q)
  expect_true(is.na(h[[1,3]]))
  expect_false(is.na(h[[1,4]]))
  # SQL syntax error
  q <- 'select from `CNSIM.CNSIM1`'
  expect_error(opal.sql(o, q))
  h <- opal.sql_history(o, limit = 1)
  expect_equal(h[[1,1]], "administrator")
  expect_equal(h[[1,2]], q)
  expect_true(is.na(h[[1,3]]))
  expect_false(is.na(h[[1,4]]))
  
  # check no R session created
  expect_null(o$rid)
  
  opal.logout(o)
})

test_that("Table SQL unsupported operations", {
  check_skip()
  #skip("Temporary skip")
  o <- opal.login("administrator", "password")

  expect_error(opal.sql(o, 'CREATE TABLE CNSIM0(x INTEGER PRIMARY KEY ASC, y, z);', 'CNSIM'))
  expect_error(opal.sql(o, 'DROP TABLE CNSIM1', 'CNSIM'))
  expect_error(opal.sql(o, 'ALTER TABLE CNSIM1 RENAME TO CNSIM0', 'CNSIM'))

  opal.logout(o)
})
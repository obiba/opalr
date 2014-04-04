#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

source("setup.R")

#
# Tests
#

context("dsbase::meandDS")

rids <- datashield.aggregate(opals, quote(meanDS(D$LAB_TSC)), async=TRUE)
stat.mean <- datashield.command_result(opals, rids, wait=TRUE)
#print(stat.mean)

test_that("mean values", { 
  expect_false(is.na(stat.mean$sim1))
  expect_equal(stat.mean$sim1, 5.87211344770338, tolerance = .000000000000001)
  expect_false(is.na(stat.mean$sim2))
  expect_equal(stat.mean$sim2, 5.84526388341867, tolerance = .000000000000001)
  expect_false(is.na(stat.mean$sim3))
  expect_equal(stat.mean$sim3, 5.84630008623168, tolerance = .000000000000001)
})

context("dsbase::quantilemean.ds")

rids <- datashield.aggregate(opals, quote(quantilemean.ds(D$LAB_TSC)), async=TRUE)
quantiles <- datashield.command_result(opals, rids, wait=TRUE)
#print(quantiles)

test_that("quantile mean values", { 
  expect_equal(as.numeric(quantiles$sim1["Mean"]), 5.87211344770338, tolerance = .000000000000001)
  expect_equal(as.numeric(quantiles$sim2["Mean"]), 5.84526388341867, tolerance = .000000000000001)
  expect_equal(as.numeric(quantiles$sim3["Mean"]), 5.84630008623168, tolerance = .000000000000001)
})

#
# Tear down
#

source("teardown.R")
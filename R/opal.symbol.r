#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the R symbols available in the remote R session.
#' 
#' @title List R symbols
#' 
#' @param opal Opal object.
#' @export
opal.symbols <- function(opal) {
  .get(opal, "r", "session", "current", "symbols")
}

#' Remove a symbol from the current R session.
#' 
#' @title Remove a R symbol
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @export
opal.symbol_rm <- function(opal, symbol) {
  tryCatch(.delete(opal, "r", "session", "current", "symbol", symbol), error=function(e){})
}

#' Remove a symbol from the current R session. Deprecated: see opal.symbol_rm function instead.
#' 
#' @title Remove a R symbol (deprecated)
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @export
opal.rm <- function(opal, symbol) {
  opal.symbol_rm(opal, symbol)
}
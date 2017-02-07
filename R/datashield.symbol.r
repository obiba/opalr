#-------------------------------------------------------------------------------
# Copyright (c) 2017 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the R symbols available after the datashield.assign calls in the current Datashield session.
#' 
#' @title List R symbols
#' 
#' @param opal Opal object or list of opal objects.
#' @rdname datashield.symbols
#' @export
datashield.symbols=function(opal, ...) {
  UseMethod('datashield.symbols');
}

#' @rdname datashield.symbols
#' @method datashield.symbols opal
#' @S3method datashield.symbols opal
datashield.symbols.opal=function(opal) {
  ignore <- .getDatashieldSessionId(opal)
  .get(opal, "datashield", "session", opal$rid, "symbols")
}

#' @rdname datashield.symbols
#' @method datashield.symbols list
#' @S3method datashield.symbols list
datashield.symbols.list=function(opals) {
  lapply(opals, FUN=datashield.symbols.opal)
}

#' Remove a symbol from the current Datashield session.
#' 
#' @title Remove a R symbol
#' 
#' @param opals Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @export
datashield.rm=function(opals, symbol) {
  if (missing(symbol) || length(symbol) == 0) stop("symbol to remove is required")
  UseMethod('datashield.rm');
}

#' @rdname datashield.rm
#' @method datashield.rm opal
#' @S3method datashield.rm opal
datashield.rm.opal=function(opal, symbol) {
  ignore <- .getDatashieldSessionId(opal)
  res <- .delete(opal, "datashield", "session", opal$rid, "symbol", symbol)
}

#' @rdname datashield.rm
#' @method datashield.rm list
#' @S3method datashield.rm list
datashield.rm.list=function(opals, symbol) {
  res <- lapply(opals, FUN=datashield.rm.opal, symbol)
}
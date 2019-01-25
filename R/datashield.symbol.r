#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
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
datashield.symbols=function(opal) {
  UseMethod('datashield.symbols');
}

#' @rdname datashield.symbols
#' @export
datashield.symbols.opal=function(opal) {
  ignore <- .getDatashieldSessionId(opal)
  opal.get(opal, "datashield", "session", opal$rid, "symbols")
}

#' @rdname datashield.symbols
#' @export
datashield.symbols.list=function(opal) {
  lapply(opal, FUN=datashield.symbols.opal)
}

#' Remove a symbol from the current Datashield session.
#' 
#' @title Remove a R symbol
#' 
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @export
datashield.rm=function(opal, symbol) {
  if (missing(symbol) || length(symbol) == 0) stop("symbol to remove is required")
  UseMethod('datashield.rm');
}

#' @rdname datashield.rm
#' @export
datashield.rm.opal=function(opal, symbol) {
  ignore <- .getDatashieldSessionId(opal)
  res <- opal.delete(opal, "datashield", "session", opal$rid, "symbol", symbol)
}

#' @rdname datashield.rm
#' @export
datashield.rm.list=function(opal, symbol) {
  res <- lapply(opal, FUN=datashield.rm.opal, symbol)
}
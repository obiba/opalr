#-------------------------------------------------------------------------------
# Copyright (c) 2016 OBiBa. All rights reserved.
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
  ignore <- .getRSessionId(opal)
  .get(opal, "r", "session", opal$rid, "symbols")
}

#' Remove a symbol from the current R session.
#' 
#' @title Remove a R symbol
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @export
opal.symbol_rm <- function(opal, symbol) {
  ignore <- .getRSessionId(opal)
  tryCatch(.delete(opal, "r", "session", opal$rid, "symbol", symbol), error=function(e){})
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

#' Save the tibble identified by the symbol in a file of the R session workspace 
#' 
#' @title Data assignment
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @param destination The path of the file in the Opal file system. Supported file extensions are: .sav (SPSS), .sas7bdat (SAS), .dta (Stata). 
#' @export
opal.symbol_save <- function(opal, symbol, destination) {
  ignore <- .getRSessionId(opal)
  if (!is.na(opal$version) && opal.version_compare(opal,"2.8")<0) {
    warning("Saving tibble in a file not available for opal ", opal$version, " (2.8.0 or higher is required)")
  } else {
    if (is.null(destination)) {
      stop("Destination file path is missing or empty.")
    }
    query <- list(destination=destination)
    res <- .put(opal, "r", "session", opal$rid, "symbol", symbol, "_save", query=query)
  }
}
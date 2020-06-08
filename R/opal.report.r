#-------------------------------------------------------------------------------
# Copyright (c) 2020 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Helper function for generating reports.
#' 
#' @title Opal report
#' 
#' @param input Path to the R markdown report file
#' @param output Directory path where to ouput the html report file. Default is the current working directory.
#' @param progress Knitr progress option
#' @param verbose Knitr verbose option
#' @param boot_style Deprecated, directives can be integrated in the YAML header of the R markdown document.
#' @examples 
#' \dontrun{
#' opal.report('input.Rmd', 'report', progress=TRUE)
#' }
#' @export
opal.report <- function(input, output=NULL, progress=FALSE, verbose=FALSE, boot_style=NULL) {
  if (is.null(input) | !grepl(pattern="\\.Rmd$", input)) {
    stop("R markdown input file is required.")
  }
  
  outputDir <- output
  if (is.null(output)) {
    outputDir <- getwd()
  }
  dir.create(outputDir, showWarnings=FALSE)
  outputDir <- normalizePath(outputDir)
  
  # prepare working directory for knitr
  inputFile <- normalizePath(input)
  if (outputDir != dirname(inputFile)) {
    file.copy(from=list.files(dirname(inputFile), full.names=TRUE), 
              to=outputDir, 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
    inputFile <- file.path(outputDir, basename(inputFile))
  }
  message("output directory: ", dirname(inputFile))
  originalWd <- getwd()
  setwd(dirname(inputFile))
  
  # make markdown, then make html
  opal.report_md(inputFile, progress, verbose)
  
  # restore working directory
  setwd(originalWd)
}

#' Helper function for turning an array into its Markdown representation.
#' 
#' @title Array to Markdown
#' 
#' @param table An array, including a matrix or a data.frame.
#' @param icons Turn logicals to icons (requires bootstrap style). Default is TRUE.
#' @param digits The maximum number of digits for numeric columns (passed to round()); it can also be a vector of length ncol(table) to set the number of digits for individual columns.
#' @param col.names A character vector of column names to be used in the table
#' @param align The alignment of columns: a character vector consisting of 'l' (left), 'c' (center) and/or 'r' (right); by default, numeric columns are right-aligned, and other columns are left-aligned; if align = NULL, the default alignment is used.
#' @param caption	The table caption.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' opal.as_md_table(opal.variables(o, 'datashield', 'CNSIM1'))
#' opal.logout(o)
#' }
#' @export
opal.as_md_table <- function(table, icons=TRUE, digits=getOption("digits"), col.names=colnames(table), align, caption=NULL) {
  if (!icons) {
    return(knitr::kable(table, format="markdown", digits=digits, col.names=col.names, align=align, caption=caption))
  }
  asIcons <- function(x) {
    unlist(lapply(x, function(a) {
      if(a %in% c("TRUE","FALSE") || is.na(a)) {
        if(is.na(a) || !as.logical(a)) {
          return('<span class="glyphicon glyphicon-remove alert-error"></span>')
        }
        else if(as.logical(a)){
          return('<span class="glyphicon glyphicon-ok alert-success"></span>')
        }
      }
      return(a)
    }))
  }
  return(knitr::kable(as.data.frame(lapply(table,asIcons)), format="markdown", digits=digits, col.names=col.names, align=align, caption=caption))
}

#' Turn a R markdown file to html.
#' @keywords internal
opal.report_md <- function(inputFile, progress, verbose) {
  knitr::opts_knit$set(progress=progress, verbose=verbose)
  knitr::opts_chunk$set(tidy = FALSE, highlight = FALSE)
  rmarkdown::render(inputFile)
  message("output file: ", gsub("\\.Rmd", "\\.html", inputFile))
}
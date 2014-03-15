#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
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
#' @param ouput Directory path where to ouput the html report file. Default is the current working directory.
#' @param boot_style The Bootstrap style to use if character, if NULL uses the default markdown to HTML converter (no Bootstrap style), if TRUE a menu is shown with the available styles.
#' @param progress Knitr progress option
#' @param verbose Knitr verbose option
#' @export
opal.report <- function(input, output=NULL, boot_style=getOption("opal.report.style"), progress=FALSE, verbose=FALSE) {
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
  opal.report_md(inputFile, boot_style, progress, verbose)
  
  # restore working directory
  setwd(originalWd)
}

#' Turn a R markdown file to html.
#' @keywords internal
opal.report_md <- function(inputFile, boot_style, progress, verbose) {
  require(knitr)
  opts_knit$set(progress=progress, verbose=verbose)
  opts_chunk$set(tidy = FALSE, highlight = FALSE)
  mdFile = knit(inputFile)
  message("output file: ", gsub("\\.md", "\\.html", mdFile))
  if (is.null(boot_style)) {
    require(markdown)
    markdownToHTML(mdFile, gsub("\\.md", "\\.html", mdFile))
  } else {
    require(knitrBootstrap)
    knit_bootstrap_md(mdFile, chooser="boot", boot_style=boot_style)
  }
}
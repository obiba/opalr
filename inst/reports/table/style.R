#
# Source this file in Rstudio before generating the report.
# http://nsaunders.wordpress.com/2012/08/27/custom-css-for-html-generated-using-rstudio/
#

#
# custom.css file is expected to be in the same directory as the input file.
#
#options(rstudio.markdownToHTML = 
#  function(inputFile, outputFile) {      
#    require(markdown)
#    markdownToHTML(inputFile, outputFile, stylesheet='custom.css')   
#  }
#)

#
# https://github.com/jimhester/knitrBootstrap
#
# See for the list of boot_styles: https://github.com/jimhester/knitrBootstrap/blob/master/R/knit_bootstrap.R
#
options(rstudio.markdownToHTML =
  function(inputFile, outputFile) {
    require(knitrBootstrap)
    knit_bootstrap_md(input=inputFile, output=outputFile, boot_style="flatly", chooser="boot")
  }
)
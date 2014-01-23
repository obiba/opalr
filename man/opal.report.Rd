\name{opal.report}
\alias{opal.report}
\title{Opal report}
\usage{
opal.report(input, output = NULL, chooser = "boot", boot_style = "flatly",
  progress = FALSE, verbose = FALSE)
}
\arguments{
  \item{input}{Path to the R markdown report file}

  \item{ouput}{Directory path where to ouput the html
  report file. Default is the current working directory.}

  \item{chooser}{A character vector, if contains "boot",
  adds a bootstrap style chooser to the HTML, if contains
  "code" adds the bootstrap code chooser.}

  \item{boot_style}{The bootstrap style to use if
  character, if NULL uses the default markdown to HTML
  converter (no Bootstrap style), if TRUE a menu is shown
  with the available styles.}

  \item{progress}{Knitr progress option}

  \item{verbose}{Knitr verbose option}
}
\description{
Helper function for generating reports.
}


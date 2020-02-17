# Analysis of Norwegian survey of IHC concepts (2019)

## Introduction

This repository contains an R package for the analysis of a survey of adult
Norwegians' understanding of the
[Informed Health Choices](https://www.informedhealthchoices.org) concepts, and
a comparison Norwegians to Ugandan children, parents, and teachers. The package
contains code and data.

## Setup

### 1. Clone this repository

If you are viewing this file on GitHub's website, you clone this 
repository using the "Clone or download" button above. Otherwise use
whatever mechanism you like to obtain a local copy of this repository).

### 2. Obtain the Norwegian survey data

Due to privacy concerns, the Norwegian survey data cannot be distributed
alongside the R code in this repository. You need to obtain the Norwegian
survey data and place it in the correct location. The files you need are called:

* `Quiz-1-data.xlsx`
* `Quiz-2-data.xlsx`
* `Quiz-3-data.xlsx`
* `Quiz-4-data.xlsx`

and they can be obtained from Harvard Dataverse at
[https://doi.org/10.7910/DVN/R3DHA5](https://doi.org/10.7910/DVN/R3DHA5).

Place these files in the `inst/extdata/` directory of your local clone of this git
repository (i.e., the result of step 1, above). Please follow whatever
restrictions are placed on your use of those files. You can then proceed by
installing the package and running the analysis, as described below.

### 3. Installing the package and running the analysis

If you use RStudio, ensure you have the `devtools` package installed, open the
`fhi.ihc.norway.2019.Rproj` file and use the `devtools::install_deps` function
to install the dependencies. (If you do not use RStudio, follow whatever steps
are necessary to install a local R package.)

### 4. Running the analysis

You should then be able to run the analysis by knitting the
`ihc-norway-analysis.Rmd` file in the `vignettes` directory. This will generate
a Word document with all results, and will also save publication-ready figures
to that directory. Note that some figures in the generated Word file may not
display nicely due to the typical annoyances of scaling RMarkdown graphics.
For such figures, simply refer to the files that are saved to disk. (Figures
from the most recent run of the analysis are included in the repository.)

## Support

If you have questions about this analysis, please contact
[Chris Rose](<mailto:cjro@fhi.no> "Help with the Norway IHC Survey analysis") at
the Norwegian Institute of Public Health.

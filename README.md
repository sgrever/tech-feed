# TechFeed
News API technology articles, refreshed weekly. 

This Quarto dashboard is compiled by Github Actions and hosted on Github Pages. Dependencies are managed using `renv`, making it reproducible for anyone with a recent version of R and a [News API](https://newsapi.org/) key. 

In addition to Quarto and Github Actions/Pages, this project utilizes the following R tools: 

* HTTP client and JSON parser: [httr2](https://httr2.r-lib.org/), [jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)  
* HTML outputs: [reactable](https://glin.github.io/reactable/), [htmltools](https://rstudio.github.io/htmltools/index.html) 
* Data wrangling: [dplyr](https://dplyr.tidyverse.org/), [tidyr](https://tidyr.tidyverse.org/), [janitor](https://cran.r-project.org/web/packages/janitor/index.html)
* String manipulation: [glue](https://glue.tidyverse.org/), [stringr](https://stringr.tidyverse.org/)
* Date manipulation: [lubridate](https://lubridate.tidyverse.org/)


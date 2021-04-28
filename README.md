# CJOR-SDI-SOR 2011-2021 Review

This repository contains `R`` code to reproduce the results and figures from the CJOR-SDI-SOR 2021 paper submitted to the _Central European Journal of Operations Research_.

## How to reproduce the results and figures?

1. First, install the appropiate version of the `renv` package from GitHub with:
    ``` r
    install.packages("remotes")
    remotes::install_github("rstudio/renv")
    ```
2. We use `renv` to manage the packages used in this project. Make sure that the `renv.lock`, `.Rprofile`, and `renv/` files/folders are present. You can restore all packages in your machine, in their appropriate versions. by command:
    ``` r
    renv::restore()
    ```
3. Explore and/or run the source code with:
    ``` r
    source("analysis.R")
    ```


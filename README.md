# GARCH-RAC

This is the repository for R code associated with the paper "Garch Tutorial in R", submitted to RAC in march 2020.


# Instructions

1) Install R (link [here](https://cloud.r-project.org/) )

2) Install RStudio (link [here](https://rstudio.com/products/rstudio/download/))

3) Run script _00-Prepare_Computer.R_ to install all dependencies of the code (open script file in RStudio and press _control + shift + enter_)

4) Execute other scripts in root folder in order to reproduce all results from the paper


# Available R scripts

| Filename                         | Description                                                                                        |
|----------------------------------|----------------------------------------------------------------------------------------------------|
| 00-Prepare_computer.R            | Setup computer by installing all required R packages. This is a mandatory step.                    |
| 01-Get_Index_Data.R              | Using the internet and package BatchGetSymbols, imports a dataset of prices of the Ibovespa index. |
| 02-Do_Descriptive_Figures.R      | Creates and saves all descriptive figures presented in the paper.                                  |
| 03-Do_ARCH_Test.R                | Performs the arch test in the data.                                                                |
| 04-Estimate_Simple_Garch_Model.R | Estimate an introductory Garch model and present results.                                          |
| 04-Find_Best_Garch_Model.R       | Finds the best ARMA(ar, ma)-GARCH(p,q) model for the dataset.                                      |
| 05-Simulate_Garch_Model.R        | Simulates the previous GARCH model and plots the index.                                            |

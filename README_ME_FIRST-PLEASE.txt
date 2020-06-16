# Description

This is the R code associated with paper "Garch Tutorial in R", avaliable at <link_here>. 

# Instructions

1) Install R: https://cloud.r-project.org/

2) Install RStudio: https://rstudio.com/products/rstudio/download/

3) Run script _00-Prepare_Computer.R_ to install all R dependencies. For that, open script file in RStudio (or other IDE) and execute (in RStudio, press _control + shift + enter_)

4) Executescripts in root folder in order to reproduce all results from the paper


# Available R scripts

| Filename                         | Description                                                                                        |
|----------------------------------|----------------------------------------------------------------------------------------------------|
| 00-Prepare_computer.R            | Setup computer by installing all required R packages. **This is a mandatory step.**                |
| 01-Get_Index_Data.R              | Using the internet, imports a dataset of prices of the Ibovespa index.                             |
| 02-Do_Descriptive_Figures.R      | Creates and saves all descriptive figures presented in the paper.                                  |
| 03-Do_ARCH_Test.R                | Performs the arch test in the data.                                                                |
| 04-Estimate_Simple_Garch_Model.R | Estimate an introductory Garch model and present results.                                          |
| 05-Find_Best_Garch_Model.R       | Finds the best ARMA(ar, ma)-GARCH(p,q) model for the dataset.                                      |
| 06-Simulate_Garch_Model.R        | Simulates the previous GARCH model and plots paths and probabilities                               |

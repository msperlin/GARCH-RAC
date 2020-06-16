# GARCH-RAC

This is the repository for R code associated with the paper "Garch Tutorial in R", submitted to RAC in march 2020. The full paper is available at [RAC](link_to_paper). 

## Available R scripts

| Filename                         | Description                                                                                        |
|----------------------------------|----------------------------------------------------------------------------------------------------|
| 00-Prepare_computer.R            | Setup computer by installing all required R packages. **This is a mandatory step.**                |
| 01-Get_Index_Data.R              | Using the internet, imports a dataset of prices of the Ibovespa index.                             |
| 02-Do_Descriptive_Figures.R      | Creates and saves all descriptive figures presented in the paper.                                  |
| 03-Do_ARCH_Test.R                | Performs the arch test in the data.                                                                |
| 04-Estimate_Simple_Garch_Model.R | Estimate an introductory Garch model and present results.                                          |
| 05-Find_Best_Garch_Model.R       | Finds the best Garch/eGarch/gjcGarch model for the dataset.                                      |
| 06-Simulate_Garch_Model.R        | Simulates the previous GARCH model and plots paths and probabilities.                              |


## Instructions

1) Install latest version of R ([link](https://cloud.r-project.org/) )

2) Install latest version of RStudio ([link](https://rstudio.com/products/rstudio/download/))

3) Download [zip file](https://github.com/msperlin/GARCH-RAC/archive/master.zip) from this repository.

4) Unzip the content of zip file in a personal folder

5) Run first script _00-Prepare_Computer.R_ to install all dependencies of the code. For that, in RStudio, open R script file and press _control + shift + enter_

6) Execute other scripts in root folder in order to reproduce all results from the paper

If these steps don't work for you, please [let us know](https://github.com/msperlin/GARCH-RAC/issues) by reporting OS (windows/linux/mac), R version and error code (if any). 


## Issues and bugs

If you've found an issue within the code, please use [Git issue system](https://github.com/msperlin/GARCH-RAC/issues). That way everyone will be able to see the history of issues and corrections.

### Tested platforms:

- [Windows 10](https://www.microsoft.com/pt-br/software-download/windows10ISO)
- [Linux Mint 20](https://linuxmint.com/release.php?id=38)
- [Ubuntu 20.04](https://ubuntu.com/download/desktop/thank-you?version=20.04&architecture=amd64)
- [RStudio Cloud](https://rstudio.cloud/)


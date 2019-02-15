install.packages("caper")
library(caper)
## Your comparative data
comp_data <- comparative.data(phy = my_tree, data = my_data,
                              names.col = species, vcv = TRUE)
Note that you can have a specimen column that can look like this:
read.excel <- function(header=TRUE,...) {
    read.table("clipboard",sep="\t",header=header,...)
  }

dat=read.excel()


colnames(dat)
## Your formula
  my_formula <- variable1 ~ variable2
And your MCMC settings:
  
  ## Setting the prior list (see the MCMCglmm course notes for details)
  prior <- list(R = list(V=1, nu=0.002),
                G = list(G1 = list(V=1, nu=0.002)))

## Setting the MCMC parameters
## Number of interations
nitt <- 12000

## Length of burnin
burnin <- 2000

## Amount of thinning
thin <- 5
And you should then be able to run a default MCMCglmm:
  
  ## Extracting the comparative data
  mcmc_data <- comp_data$data


## As MCMCglmm requires a colume named animal for it to identify it as a phylo
## model we include an extra colume with the species names in it.
mcmc_data <- cbind(animal = rownames(mcmc_data), mcmc_data)
mcmc_tree <- comp_data$phy

## The MCMCglmmm
mod_mcmc <- MCMCglmm(fixed = my_formual, 
                     random = ~ animal + specimen, 
                     family = "gaussian",
                     pedigree = mcmc_tree, 
                     data = mcmc_data,
                     nitt = nitt,
                     burnin = burnin,
                     thin = thin,
                     prior = prior)
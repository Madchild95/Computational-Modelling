#checking if Toolchain C++ works
pkgbuild::has_build_tools(debug = TRUE) 

#Configuration of the toolchain
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
    if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else 
      if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=corei7 -mtune=corei7" else
        "CXX14FLAGS += -fPIC",
    file = M, sep = "\n", append = TRUE)

#If you need to change the configuration type
#M <- file.path(Sys.getenv("HOME"), ".R", ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
#file.edit(M)

#To be on the safe side, remove first
remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

#Restart R

#Install RStan
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#Install rethinking
remove.packages("rethinking")
install.packages(c('devtools','coda','mvtnorm','StanHeaders','ggplot2','rstan','pacman','parallel'))
library(pacman)
p_load(pacman, devtools, coda, mvtnorm, StanHeaders, ggplot2, rstan, parallel)

options(repos=c(getOption('repos'),rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')

library(rethinking)
help(package=rethinking)

#Install BMRS
if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("paul-buerkner/brms", build_vignettes = TRUE) #latest version from Github
install.packages("brms")

library(brms)
help('brms')

installed.packages('rstan')
my_packages <- library()$results[,1]
head(my_packages, 10)
"dplyr" %in% tolower(my_packages)
library(rethinking)
(.packages())

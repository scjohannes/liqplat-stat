# renv's lockfile check at startup parses every notebook (about 15 s), and
# Quarto starts R once per notebook; scripts/run-all.R checks once per run.
Sys.setenv(RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE")
source("renv/activate.R")

# Personal options, such as the cmdstanr model directory, which the project
# profile would otherwise hide.
if (file.exists("~/.Rprofile")) source("~/.Rprofile")

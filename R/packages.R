# Activate reproducible package environment (renv) -----------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::activate()

# renv::status()

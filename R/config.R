# config.R ---------------------------------------------------------------------

SEED <- 123

IN <- list(

  full_paper_sample              = file.path(PATHS$IN, "full_paper_sample.xlsx"),
  coded_reli                     = file.path(PATHS$IN, "df_sample_coded_reli.xlsx"),
  coded_full_sample              = file.path(PATHS$IN, "full_paper_sample_coded.xlsx"),
  dupes_checked                  = file.path(PATHS$IN, "dupes_checked.xlsx"),
  wos_abstracts                  = file.path(PATHS$IN, "wos_abstracts_2009_2023.xlsx"),
  wos_legacy                     = file.path(PATHS$IN, c("WoS_savedrecs_1-1000.xls", "WoS_savedrecs_1001-2000.xls", "WoS_savedrecs_2001-2765.xls")
                                          
  )
)

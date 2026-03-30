# config.R ---------------------------------------------------------------------

SEED <- 123

IN <- list(

  full_paper_sample              = file.path(PATHS$IN, "03a_full_paper_sample.xlsx"),
  coded_reli                     = file.path(PATHS$IN, "04_df_sample_coded_reli.xlsx"),
  coded_full_sample              = file.path(PATHS$IN, "05_full_paper_sample_coded.xlsx"),
  dupes_checked                  = file.path(PATHS$IN, "05_dupes_checked.xlsx"),
  wos_abstracts                  = file.path(PATHS$IN, "01_wos_abstracts_2009_2023.xlsx"),
  wos_legacy                     = file.path(PATHS$IN, c("01_WoS_savedrecs_1-1000.xls", "01_WoS_savedrecs_1001-2000.xls", "01_WoS_savedrecs_2001-2765.xls")
                                          
  )
)

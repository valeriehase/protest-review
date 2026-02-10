#
# Data Cleaning of Final Coded Data
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Setup ------------------------------------------------------------------------

library(here)

source(here("R/paths.R"))
source(here("R/config.R"))
source(here("R/logging.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(openxlsx)

log_df <- init_log()

# Load Input -----------------------------------------------------

input_file <- if (exists("IN") && !is.null(IN$coded_full_sample_deduplicated)) {
  IN$coded_full_sample_deduplicated
} else {
  here("data", "in", "full_paper_sample_coded_deduplicated.xlsx")
}
stopifnot(file.exists(input_file))
df <- readxl::read_excel(input_file) 

df_clean <- df %>%
  rename_with(
    ~ str_extract(.x, "^V\\d+"),  # anpassung variablennamen
    starts_with("V")) 

# 6.1 Check Values ----------------------------------------

# sanity check
n_distinct(df_clean$id_unique) == nrow(df_clean)

# --- V6 ------------------------------------

v6_long <- df_clean %>%
  mutate(.row = dplyr::row_number()) %>%
  filter(!is.na(V6)) %>%
  tidyr::separate_rows(V6, sep = ";\\s*") %>%
  mutate(V6 = stringr::str_trim(V6))

v6_counts <- v6_long %>% count(.row, name = "n_tokens")
v6_empty <- v6_long %>% filter(V6 == "")
v6_num_weird <- v6_long %>%
  filter(stringr::str_detect(V6, "^[0-9]+$"), V6 != "1")

invalid_v6_ids <- df_clean %>%
  mutate(.row = dplyr::row_number()) %>%
  left_join(v6_counts, by = ".row") %>%
  mutate(n_tokens = dplyr::coalesce(n_tokens, 0L)) %>%  
  filter(n_tokens > 3) %>%                              
  select(id_unique) %>%
  bind_rows(v6_empty %>% select(id_unique)) %>%
  bind_rows(v6_num_weird %>% select(id_unique)) %>%
  distinct() %>%
  arrange(id_unique)

invalid_v6 <- df_clean %>%
  filter(id_unique %in% invalid_v6_ids$id_unique)

invalid_v6

df_clean <- df_clean %>%
  mutate(V6 = if_else(id_unique == "ID413", "18 Million Rising (18MR); Asian American Feminist Collective (AAFC); Black Women Radicals (BWR)", V6), # too many strings, reduced to first three
         V6 = if_else(id_unique == "ID44", "Euromaidan; Occupy Wall Street (OWS; unionsq); Gezi park", V6), # too many strings, reduced to first three  
         V6 = if_else(id_unique == "ID94", "1", V6), # protest general coded as protest cases
         ) %>%
  mutate(V6 = if_else(is.na(V6), V6, V6 %>%
        str_squish() %>%                         
        str_replace_all("\\s*[,/|]+\\s*", "; ") %>% # andere trenner
        str_replace_all("\\s*;\\s*", "; ") %>%   # spaces
        str_replace_all("^(;\\s*)+", "") %>%     # führende/trailing semikolons 
        str_replace_all("(;\\s*)+$", "") %>% 
        str_replace_all("(;\\s*){2,}", "; ")     # doppelte semikolons
    )
  )

log_event(
  step   = "03_V6_value_fix",
  action = "manual_edit_and_normalize",
  note   = "id_unique ID413, ID44, ID94: V6 manuell angepasst (zu viele Protest Cases bzw. Recode); gesamte Spalte V6 anschließend normalisiert (Trenner vereinheitlicht, führende/abschließende Semikolons entfernt)"
)


# --- V7 ------------------------------------

allowed_v7 <- as.character(1:10) |> c("NA")  

invalid_v7 <- df_clean %>%
  filter(!is.na(V7)) %>%                        
  separate_rows(V7, sep = ";\\s*") %>%         
  mutate(V7 = str_trim(V7)) %>%
  filter(!(V7 %in% allowed_v7))  

print(invalid_v7)
# Typo bei ID366

df_clean <- df_clean %>%
  mutate(V7 = if_else(id_unique == "ID366", "1; 3", V7))

log_event(
  step   = "03_V7_value_fix",
  action = "manual_edit",
  note   = "id_unique ID366: V7 geändert von '1.3' zu '1; 3'"
)

# --- V8 ------------------------------------

v8_long <- df_clean %>%
  mutate(.row = dplyr::row_number()) %>%
  filter(!is.na(V8)) %>%
  tidyr::separate_rows(V8, sep = ";\\s*") %>%
  mutate(V8 = stringr::str_trim(V8))

v8_counts <- v8_long %>% count(.row, name = "n_tokens")
v8_empty <- v8_long %>% filter(V8 == "")
v8_num_weird <- v8_long %>%
  filter(stringr::str_detect(V8, "^[0-9]+$"), V8 != "NA")

invalid_v8_ids <- df_clean %>%
  mutate(.row = dplyr::row_number()) %>%
  left_join(v8_counts, by = ".row") %>%
  mutate(n_tokens = dplyr::coalesce(n_tokens, 0L)) %>%
  filter(n_tokens > 3) %>%                        
  select(id_unique) %>%
  bind_rows(v8_empty %>% select(id_unique)) %>%
  bind_rows(v8_num_weird %>% select(id_unique)) %>%
  distinct() %>%
  arrange(id_unique)

invalid_v8 <- df_clean %>%
  filter(id_unique %in% invalid_v8_ids$id_unique) 

print(invalid_v8)

df_clean <- df_clean %>%
  mutate(V8 = if_else(id_unique == "ID1224", "Iraq; Egypt; Yemen", V8), # too many strings, reduced to first three
         V8 = if_else(id_unique == "ID202", "Ireland; Malta; Netherlands", V8), # too many strings, reduced to first three  
         V8 = if_else(id_unique == "ID44", "Turkey; Ukraine; United States of America", V8) # too many strings, reduced to first three
         ) %>%
  mutate(
    V8 = if_else(
      is.na(V8), V8,
      V8 %>%
        str_squish() %>%                         
        str_replace_all("\\s*[,/|]+\\s*", "; ") %>% # alternative trenner
        str_replace_all("\\s*;\\s*", "; ") %>%   # semikolons vereinheitlichen
        str_replace_all("^(;\\s*)+", "") %>%     # führende/trailing semikolons 
        str_replace_all("(;\\s*)+$", "") %>% 
        str_replace_all("(;\\s*){2,}", "; ")     # doppelte semikolons 
    )
  )

log_event(
  step   = "03_V8_value_fix",
  action = "manual_edit_and_normalize",
  note   = "id_unique ID1224, ID202, ID44: V8 manuell angepasst (zu viele Strings, jeweils auf drei reduziert); gesamte Spalte V8 anschließend normalisiert (Trenner vereinheitlicht, führende/abschließende Semikolons entfernt)"
)

# --- V9 ------------------------------------

v9_long <- df_clean %>%
  mutate(.row = dplyr::row_number()) %>%
  filter(!is.na(V9)) %>%
  tidyr::separate_rows(V9, sep = ";\\s*") %>%
  mutate(V9 = stringr::str_trim(V9))

v9_counts <- v9_long %>% count(.row, name = "n_tokens")
v9_empty  <- v9_long %>% filter(V9 == "")
v9_num_weird <- v9_long %>%
  filter(stringr::str_detect(V9, "^[0-9]+$"), V9 != "NA")

invalid_v9_ids <- df_clean %>%
  mutate(.row = dplyr::row_number()) %>%
  left_join(v9_counts, by = ".row") %>%
  mutate(n_tokens = dplyr::coalesce(n_tokens, 0L)) %>%
  filter(n_tokens > 3) %>%
  select(id_unique) %>%
  bind_rows(v9_empty %>% select(id_unique)) %>%
  bind_rows(v9_num_weird %>% select(id_unique)) %>%
  distinct() %>%
  arrange(id_unique)

invalid_v9 <- df_clean %>%
  filter(id_unique %in% invalid_v9_ids$id_unique) %>%
  select(id_unique, V9, dplyr::everything())

print(invalid_v9)

df_clean <- df_clean %>%
  mutate(
    V9 = if_else(
      is.na(V9), V9,
      V9 %>%
        str_squish() %>%                        
        str_to_lower() %>%                      
        str_replace_all("\\s*[,/|]+\\s*", "; ") %>% # andere trenner zwischen strings
        str_replace_all("\\s*;\\s*", "; ") %>% # semikolon spaces vereinheitlichen
        str_replace_all("^(;\\s*)+", "") %>% # führende/trailing semikolons entfernen
        str_replace_all("(;\\s*)+$", "") %>% 
        str_replace_all("(;\\s*){2,}", "; ") # doppelte semikolons
    )
  )

log_event(
  step   = "03_V9_normalize",
  action = "format_standardization",
  note   = "V9 automatisch normalisiert: lowercase, Trenner auf '; ' vereinheitlicht, führende/abschließende Semikolons entfernt"
)


# --- V10 ------------------------------------

allowed_v10 <- as.character(c(100, 110:114, 120:124, 130:134, 140:142, 150:153, 160:162, 200:204, 300, 400, "NA"))

invalid_v10 <- df_clean %>%
  filter(!is.na(V10)) %>%                        
  separate_rows(V10, sep = ";\\s*") %>%          
  mutate(V10 = str_trim(V10)) %>%
  filter(!(V10 %in% allowed_v10))

print(invalid_v10)

df_clean <- df_clean %>% 
  mutate(V10 = if_else(id_unique == "ID1680", "100", V10), # survey on use of ICT
         V10 = if_else(id_unique == "ID822", "300; 113; 112; 111; 100; 110", V10), # media types included traditional media (or affiliates), online partisan media, online nonpartisan media, activism/advocacy media, social media, and ephemeral websites
         V10 = if_else(id_unique == "ID98", "141; 131; 132; 124", V10)) # Twitter, Facebook, Instagram, and Reddit data were collected using Synthesio

#log
log_event(
  step   = "03_V10_value_fix",
  action = "manual_edit",
  note   = "id_unique ID1680: V10 geändert von '99' zu '100' (survey on use of ICT)"
)
log_event(
  step   = "03_V10_value_fix",
  action = "manual_edit",
  note   = "id_unique ID822: V10 geändert von ' ' zu '300; 113; 112; 111; 100; 110' (fehlende Kodierung)"
)
log_event(
  step   = "03_V10_value_fix",
  action = "manual_edit",
  note   = "id_unique ID98: V10 geändert von ' ' zu '141; 131; 132; 124' (fehlende Kodierung)"
)


# --- V11 ------------------------------------

allowed_v11 <- as.character(c(10:18, 20:26, 99, "NA"))

invalid_v11 <- df_clean %>%
  filter(!is.na(V11)) %>%                 
  separate_rows(V11, sep = ";\\s*") %>%   
  mutate(V11 = str_trim(V11)) %>%         
  filter(!(V11 %in% allowed_v11))         

print(invalid_v11)

df_clean <- df_clean %>% 
  mutate(V11 = if_else(id_unique == "ID1494", "24", V11), 
         V11 = if_else(id_unique == "ID2437", "21; 22", V11),
         V11 = if_else(id_unique == "ID83", "18; 12", V11)) 

#log
log_event(
  step   = "03_V11_value_fix",
  action = "manual_edit",
  note   = "id_unique ID1494: V11 geändert von '24.' zu '24'"
)
log_event(
  step   = "03_V11_value_fix",
  action = "manual_edit",
  note   = "id_unique ID2437: V11 geändert von ' ' zu '21; 22'"
)
log_event(
  step   = "03_V11_value_fix",
  action = "manual_edit",
  note   = "id_unique ID83: V11 geändert von '18, 12' zu '18; 12'"
)


# --- V12 ------------------------------------

allowed_v12 <- c("0", "1")

invalid_v12 <- df_clean %>%           
  mutate(V12 = str_trim(as.character(V12))) %>%
  filter(!(V12 %in% allowed_v12))       

print(invalid_v12)

# --- V13 ------------------------------------

allowed_v13 <- c("0", "1")

invalid_v13 <- df_clean %>%
  mutate(V13 = str_trim(as.character(V13))) %>%
  filter(!(V13 %in% allowed_v13))      

print(invalid_v12)


# 6.2 Check Consistency ----------------------------------------------------

### Wenn method == "0", dürfen in V11 keine 10:18 stehen

method_col <- "method"

invalid_v11_method0_ids <- df_clean %>%
  filter(.data[[method_col]] == "0") %>%    
  separate_rows(V11, sep = ";\\s*") %>%                   
  mutate(V11 = str_trim(V11)) %>%                         
  filter(V11 %in% as.character(10:18)) %>%                
  distinct(id_unique) %>%                                 
  arrange(id_unique)

invalid_v11_method0 <- df_clean %>%
  filter(id_unique %in% invalid_v11_method0_ids$id_unique)

print(invalid_v11_method0)

df_clean <- df_clean %>% 
  mutate(V11 = if_else(id_unique == "ID1462", "21", V11), 
         V11 = if_else(id_unique == "ID1656", "21", V11),
         V11 = if_else(id_unique == "ID19", "21; 22", V11),
         V11 = if_else(id_unique == "ID1956", "21", V11),
         V11 = if_else(id_unique == "ID2327", "21; 22", V11),
         V11 = if_else(id_unique == "ID239", "24", V11),
         V11 = if_else(id_unique == "ID413", "21", V11),
         V11 = if_else(id_unique == "ID94", "24", V11)
         ) 

log_event(
  step   = "04_consistency_CSS_in_method0",
  action = "manual_edit",
  note   = "id_unique ID1462: V11 geändert von '13; 21' zu '21' (kein Automated Content Analysis nachweisbar)"
)
log_event(
  step   = "04_consistency_CSS_in_method0",
  action = "manual_edit",
  note   = "id_unique ID1656: V11 geändert von '13; 21' zu '21' (kein Automated Content Analysis nachweisbar)"
)
log_event(
  step   = "04_consistency_CSS_in_method0",
  action = "manual_edit",
  note   = "id_unique ID19: V11 geändert von '21; 22; 18' zu '21; 22' (Datensammlung ohne Spezifikation; Code 18 entfernt)"
)
log_event(
  step   = "04_consistency_CSS_in_method0",
  action = "manual_edit",
  note   = "id_unique ID1956: V11 geändert von '17; 21' zu '21' (Tracking-Code missverstanden)"
)
log_event(
  step   = "04_consistency_CSS_in_method0",
  action = "manual_edit",
  note   = "id_unique ID2327: V11 geändert von '21; 22; 18' zu '21; 22' (kein Scraping belegt; Code 18 entfernt)"
)
log_event(
  step   = "04_consistency_CSS_in_method0",
  action = "manual_edit",
  note   = "id_unique ID239: V11 geändert von '12; 24' zu '24' (Archivnutzung; API nicht eigenständig; Code 12 entfernt)"
)
log_event(
  step   = "04_consistency_CSS_in_method0",
  action = "manual_edit",
  note   = "id_unique ID413: V11 geändert von '21; 22; 13; 16; 18' zu '21' (ausschließlich qualitative Instagram-Analyse; übrige Codes entfernt)"
)
log_event(
  step   = "04_consistency_CSS_in_method0",
  action = "manual_edit",
  note   = "id_unique ID94: V11 geändert von '12; 18; 24' zu '24' (quantitative Inhaltsanalyse bestehender Website-Daten; 12/18 entfernt)"
)

log_event(
  step   = "04_consistency_CSS_in_method0",
  action = "no_change_documented",
  note   = "IDs ID1199, ID202, ID2152, ID267, ID366, ID109: V11 überprüft – keine Änderungen; CSS-Methoden erscheinen hier fälschlich bei method == 0"
)

ids_method_css <- c("ID1199", "ID202", "ID2152", "ID267", "ID366", "ID109")

df_clean <- df_clean %>%
  mutate(
    method = if_else(id_unique %in% ids_method_css, 1, method)
  )

set.seed(SEED)
ids_to_delete <- df_clean %>%
  filter(method == 1) %>%
  distinct(id_unique) %>%
  slice_sample(n = 12) %>%
  pull(id_unique)

df_clean <- df_clean %>%
  filter(!id_unique %in% ids_to_delete)

df_clean %>% count(method)

log_event(
  step   = "04_consistency_correction",
  action = "random_deletion",
  note   = paste0(
    "Randomly removed 12 cases from method == 1 to restore equal group sizes ",
    "(n = 220 each). Draw reproducible via predefined seed. IDs removed: ",
    paste(ids_to_delete, collapse = ", ")
  )
)

# 6.3 Check Comments --------------------------------------------------------

comments_long <- df_clean %>%
  filter(!is.na(Comment_CODER)) %>%
  mutate(Comment = str_squish(Comment_CODER)) %>%
  separate_rows(Comment, sep = ";\\s*") %>%
  extract(Comment, into = c("variable", "comment_text"),
          regex = "^(V\\d{1,2}):\\s*(.*)$", remove = TRUE) %>%
  mutate(
    variable = str_trim(variable),
    comment_text = str_trim(comment_text)
  ) %>%
  filter(!is.na(variable), variable != "")

print(comments_long)


# --- Manual adjustments based on comments_long review -------------------------

df_clean <- df_clean %>%
  mutate(
    V13 = if_else(id_unique == "ID11",   "1",        as.character(V13)),
    V10 = if_else(id_unique == "ID1072", "100",      as.character(V10)),
    V7  = if_else(id_unique == "ID1703", "10",       as.character(V7)),
    V8  = if_else(id_unique == "ID1703", "NA",       as.character(V8)),
    V11 = if_else(id_unique == "ID248",  "13; 16",   as.character(V11)),
    V11 = if_else(id_unique == "ID83",   "13; 16; 99", as.character(V11)),
    V11 = if_else(id_unique == "ID131",  "20",       as.character(V11)),
    V11 = if_else(id_unique == "ID1355", "20",       as.character(V11)),
    V11 = if_else(id_unique == "ID1390", "20",       as.character(V11))
  )

log_event(
  step   = "05_comment_review",
  action = "review_comments",
  note   = "Kommentare aus Comment_CODER extrahiert und manuell geprüft."
)

# --- Logging: changes ---------------------------------------------------------

log_event("05_comment_review", "manual_edit",
          "ID11: V13 0 -> 1 | Grund: quasi-experimentelles Design erfüllt Experiment-Kriterium")

log_event("05_comment_review", "manual_edit",
          "ID1072: V10 NA -> 100 | Grund: Fokus auf digitale Kommunikation (Memes), keine plattformspezifische Analyse")

log_event("05_comment_review", "manual_edit",
          "ID1703: V7 8 -> 10; V8 Israel/Palästina -> NA | Grund: global/Diaspora-Fokus, keine eindeutige Länderzuordnung")

log_event("05_comment_review", "manual_edit",
          "ID248: V11 13 -> 13; 16 | Grund: semantisches Netzwerk mit Gephi, Netzwerkanalyse ergänzt")

log_event("05_comment_review", "manual_edit",
          "ID83: V11 18; 12 -> 13; 16; 99 | Grund: Datenbank/Archiv + Netzwerkanalyse; Scraping/API-Codes entfernt")

log_event("05_comment_review", "manual_edit",
          "ID131: V11 NA -> 20 | Grund: theoretische Diskussion von Case Studies")

log_event("05_comment_review", "manual_edit",
          "ID1355: V11 NA -> 20 | Grund: theoretische Case-Study-Diskussion")

log_event("05_comment_review", "manual_edit",
          "ID1390: V11 21 -> 20 | Grund: theoretische Case-Study-Modell-Diskussion")


# --- Logging: reviewed, no change --------------------------------------------

log_event("05_comment_review", "no_change_documented",
          "ID1033: method geprüft | Grund: Methodenteil vorhanden, Kodierung bleibt")

log_event("05_comment_review", "no_change_documented",
          "ID1092: experiment geprüft | Grund: Beleg im Text vorhanden, Kodierung bleibt")

log_event("05_comment_review", "no_change_documented",
          "ID1174: method geprüft | Grund: Methodik ausreichend beschrieben, Kodierung bleibt")

log_event("05_comment_review", "no_change_documented",
          "ID12: method geprüft | Grund: qualitative Frame-Analyse klar beschrieben, Kodierung bleibt")

log_event("05_comment_review", "no_change_documented",
          "ID1273: plattform geprüft | Grund: Plattformzuordnung ausreichend belegt, Kodierung bleibt")

log_event("05_comment_review", "no_change_documented",
          "ID2054: V7 geprüft | Grund: MEA-/Regionenbezug passt zu Code 10, Kodierung bleibt")

log_event("05_comment_review", "no_change_documented",
          "ID2449: V12 geprüft | Grund: nationaler Fall (UK), nicht cross-national, Kodierung bleibt")

log_event("05_comment_review", "no_change_documented",
          "ID391: V10 geprüft | Grund: Website-Analyse passt zu Code 111, Kodierung bleibt")

log_event("05_comment_review", "no_change_documented",
          "ID13: V7 geprüft | Grund: pan-european passt, Kodierung bleibt")

log_event("05_comment_review", "no_change_documented",
          "ID1463: V7 geprüft | Grund: global/diaspora passt, Kodierung bleibt")


# --- Clear reviewed comments --------------------------------------------------

checked_ids <- c(
  "ID11","ID1072","ID1703","ID248","ID83",
  "ID131","ID1355","ID1390",
  "ID1033","ID1092","ID1174","ID12","ID1273","ID2054","ID2449","ID391",
  "ID13","ID1463"
)

df_clean <- df_clean %>%
  mutate(
    Comment_CODER = if_else(id_unique %in% checked_ids, NA_character_, Comment_CODER)
  )

log_event(
  step   = "05_comment_review",
  action = "clear_reviewed_comments",
  note   = paste0("Comment_CODER geleert für geprüfte Fälle: ", toString(checked_ids))
)

# 6.4 Check code distributions -----------------------------------------

coder_col <- "V5"   
vars_to_check <- paste0("V", 10:13)

code_distribution_by_coder <- purrr::map_dfr(vars_to_check, function(var) {
  
  df_clean %>%
    select(id_unique, all_of(coder_col), all_of(var)) %>%
    mutate(
      variable = var,
      value = as.character(.data[[var]]),
      coder = as.character(.data[[coder_col]])
    ) %>%
    filter(!is.na(coder)) %>%
    filter(!is.na(value), value != "") %>%
    separate_rows(value, sep = ";\\s*") %>%
    mutate(
      code = str_trim(value)
    ) %>%
    filter(code != "") %>%
    count(variable, coder, code, name = "n") %>%
    arrange(variable, code, coder)
})

print(code_distribution_by_coder)

# Inspect specific V11 codes

v11_codes_of_interest <- c("14", "17", "25", "99")

v11_flagged <- df_clean %>%
  select(id_unique, V5, V11, everything()) %>%
  mutate(V11_chr = as.character(V11)) %>%
  filter(
    is.na(V11_chr) |
      str_detect(
        V11_chr,
        paste0("(^|;\\s*)(", paste(v11_codes_of_interest, collapse = "|"), ")(;|$)")
      )
  )

print(v11_flagged)

log_event(
  step   = "06_code_distributions_by_coder",
  action = "check_code_frequencies",
  note   = paste0(
    "Code-Verteilungen V10–V13 getrennt nach Kodiererin auf Auffälligkeiten geprüft."
  )
)

# Output --------------------------------------------------------------------

write_log(log_df, PATHS$log_cleaning)


path <- "data/processed/full_paper_sample_coded_clean.xlsx"
openxlsx::write.xlsx(df_clean, path)

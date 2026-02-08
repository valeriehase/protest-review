levels_V7 <- tibble(
  V7 = c(as.character(1:10), "NA"),
  V7_label = c(
    "North America",
    "South America",
    "Europe",
    "Russia and former Soviet Republics",
    "Middle East and North Africa",
    "East Asia",
    "Central and South (East) Asia",
    "Sub-Saharan Africa",
    "Oceania",
    "Global",
    "Not mentioned"
  )
)

levels_V10 <- tibble(
  V10 = c(
    "100", "110", "111", "112", "113", "114",
    "120", "121", "122", "124",
    "130", "131", "132", "133", "134",
    "140", "141", "142",
    "150", "151", "152", "153",
    "160", "161", "162",
    "200", "201", "202", "203", "204",
    "300", "400", "NA"
  ),
  V10_label = c(
    "Internet / online / social media (general)", "Websites (general)", "Websites of activist groups / NGOs / parties / companies", "News Media (online)", "Partisan news outlets", "Blogs",
    "Knowledge communities & discussion", "Specific discussion forum(s)", "Discord", "Reddit",
    "Social networking sites (general)", "Facebook", "Instagram", "TikTok", "VKontakte",
    "Microblogs", "X/Twitter", "Weibo", 
    "Video-sharing sites", "YouTube", "Vimeo", "Twitch",
    "Photo-sharing sites", "Flickr", "Tumblr",
    "Messengers (general)", "WhatsApp", "Telegram", "Signal", "WeChat",
    "News Media (offline)", "Other", "Not mentioned"
  )
)

df_V10agg <- df %>%
  tidyr::separate_rows(V10, sep = ";") %>%
  dplyr::mutate(V10 = stringr::str_trim(as.character(V10))) %>%
  dplyr::mutate(
    V10_agg = dplyr::case_when(
      V10 == "100" ~ "100",
      V10 %in% c("110","111","112","113","114") ~ "110",
      V10 %in% c("120","121","122","124") ~ "120",
      V10 %in% c("130","131","132","133","134") ~ "130",
      V10 %in% c("140","141","142") ~ "140",
      V10 %in% c("150","151","152","153") ~ "150",
      V10 %in% c("160","161","162") ~ "160",
      V10 %in% c("200","201","202","203","204") ~ "200",
      V10 == "300" ~ "300",
      V10 == "400" ~ "400",
      V10 %in% c("NA", NA) ~ "NA",
      TRUE ~ "Other"
    )
  ) %>%
  dplyr::mutate(V10_agg = as.character(V10_agg))

levels_V10_agg <- tibble(
  V10_agg = c("100", "110", "120", "130", "140", "150", "160", "200", "300", "400", "NA"),
  V10_agg_label = c(
    "Internet / online / social media (general)",
    "Websites generally",
    "Knowledge communities & discussion",
    "Social networking sites",
    "Microblogs",
    "Video-sharing sites",
    "Photo-sharing sites",
    "Messengers",
    "News Media (offline)",
    "Other",
    "Not mentioned"
  )
)


levels_V11 <- tibble(
  V11 = c(
    "10", "11","12","13","14","15","16","17","18",
    "20", "21","22","23","24","25","26",
    "99","NA"
  ),
  V11_label = c(
    "CSS-related (general)", 
    "Agent-based models / simulations", "API access", "Automated content analysis", "Data donation", "Eye-tracking", "Network analysis", "Tracking", "Web scraping",
    "Not-CSS-related (general)",
    "Qualitative content analysis", "Qualitative interviews / focus groups", "Qualitative observation", "Quantitative content analysis", "Quantitative observation", "Quantitative survey",
    "Other", "Not mentioned"
  )
)

levels_V12 <- tibble(
  V12 = c(
    "1",
    "0"
  ),
  V12_label = c(
    "cross-national",
    "not cross-national"
  )
)

levels_V13 <- tibble(
  V13 = c(
    "1",
    "0"
  ),
  V13_label = c(
    "experiment",
    "not experiment"
  )
)

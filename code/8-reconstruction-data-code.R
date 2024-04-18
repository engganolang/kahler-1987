# for Daniel's question on Etymological reconstruction data
# scripts needed to be run:
## - 5-1. checking loanword
## - 6-check-stems
## - 7-transl..

## then..

library(googledrive)
library(googlesheets4)

ketym <- stem_all2 |> 
  filter(!is.na(stem_etymological_form)) |> 
  select(stem_id, kms_Alphabet, kms_page, kms_entry_no, stem_form, stem_DE, stem_etymological_form, stem_etymological_language_donor) |> 
  left_join(lang_df |> 
              mutate(sw_id = as.character(sw_id)) |> 
              rename(stem_etymological_language_donor = sw_id)) |> 
  mutate(sw_name = replace(sw_name, sw_name == "PAN", "PAN (Proto-Austronesian)"),
         sw_name = replace(sw_name, sw_name == "UAN (Proto-Austronesian)", "PAN (Proto-Austronesian)"),
         sw_name = replace(sw_name, sw_name == "SMtw", "SMtw (Sudmentawai [South Mentawai])"))
 
ketym1 <- ketym |> 
  mutate(sw_name = str_replace_all(sw_name, "\\s\\(.+\\)", "")) |> 
  extract(stem_etymological_form, into = c("sw_name1", "sw_name2"), regex = "^[^(].+?([(][A-Z][^)]+?[)])[^(]+?([(][A-Z][^)]+?[)])", remove = FALSE) |> 
  mutate(etymlang = if_else(!is.na(sw_name1), str_c(sw_name1, sw_name2, sep = "_"), sw_name)) |> 
  mutate(etymlang = str_split(etymlang, "_")) |> 
  unnest_longer(etymlang) |> 
  mutate(etymlang = str_replace_all(etymlang, "(\\(|\\))", "")) |> 
  select(-sw_name1, -sw_name2, -stem_etymological_language_donor, -sw_name) |> 
  arrange(kms_Alphabet, kms_page, kms_entry_no)

ketym_n <- ketym1 |> 
  count(etymlang, sort = TRUE) |> 
  filter(!is.na(etymlang))

# create an empty gsheet to store the translation to be checked
# drive_create(name = 'stem_etym',
#              path = kahler_dict_folder, # this vector needs the running of code file no. 0-source
#              type = 'spreadsheet')

# Created Drive file:
#   • stem_etym <id: 1YNQYgpdRZ0B5oNavL8C3JwQ4CJiebGJjDnjZZIHN8pM>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet

# now save the stem translation tibble to the created spreadsheet
# sheet_write(ketym1, ss = '1YNQYgpdRZ0B5oNavL8C3JwQ4CJiebGJjDnjZZIHN8pM', sheet = 'Sheet1')
sheet_write(ketym_n, ss = '1YNQYgpdRZ0B5oNavL8C3JwQ4CJiebGJjDnjZZIHN8pM', sheet = 'CountEtymLang')

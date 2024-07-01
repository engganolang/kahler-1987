# NOTE: This code file processes the checked translation data for the stems and example forms.

# library(googledrive)
# library(googlesheets4)
library(tidyverse)

# Access google drive folder in the Enggano shared drive
# source("code/0-directory.R")

# list the files in the Kahler dictionary translation folder id
# kahler_dict_files <- drive_ls(path = as_id(kahler_dict_folder))
# kahler_dict_files

# get the stem translation from the Google Spreadsheet that has been checked
# stems_translation_checked <- drive_get("1_stem_german_translation-to-check") |>
#   read_sheet() |>
#   write_rds("data-raw/1_stem_german_translation-to-check.rds")
stems_translation_checked <- read_rds("data-raw/1_stem_german_translation-to-check.rds")

stems_translation_checked1 <- stems_translation_checked |> # combine the corrected with the original German columns
  mutate(German_corrected = if_else(stem_id == "8_1688391595",
                          German,
                          German_corrected)) |> 
  mutate(German_all = if_else(is.na(German_corrected), German, German_corrected)) |> 
  select(-German, -German_corrected) |> # combine the corrected with the original English columns
  mutate(English_all = if_else(is.na(English_corrected), English, English_corrected)) |> 
  select(-English, -English_corrected) |> 
  # change the category for id "12_1688366718" from REMARK to CROSSREF
  mutate(category = replace(category, stem_id == "12_1688366718" & 
                              category == "stem_crossref",
                            "stem_remark")) |> 
  # change typo
  mutate(English_all = str_replace(English_all, "^(see)ee\\s", "\\1")) |> 
  
  # combine the stem_GermanTranslationVariant with the stem_GermanTranslation for stem ID 12_1684853273
  # because it is more appropriate
  mutate(German_all = if_else(stem_id == "12_1684853273" & category == "stem_GermanTranslation",
                          str_replace(German_all, "^(.)", "(\"d Eingeschlossene\" =) \\1"),
                          German_all),
         English_all = if_else(stem_id == "12_1684853273" & category == "stem_GermanTranslation",
                           str_replace(English_all, "^(.)", "(\"the trapped\" (m/n/f) =) \\1"),
                           English_all),
         Indonesian = if_else(stem_id == "12_1684853273" & category == "stem_GermanTranslation",
                           str_replace_all(Indonesian, "^(.)", "(\"yang terjebak\" =) \\1"),
                           Indonesian),
         Indonesian = if_else(stem_id == "12_1684853273" & category == "stem_GermanTranslation",
                              str_replace_all(Indonesian, "\\s\\(er\\)", ""),
                              Indonesian)) |>  
  
  # other editing
  mutate(English_all = str_replace(English_all, "\\(change expression for\\b", "(alternative expression for")) |> 
  
  # change the category for id "11_1684291849" from REMARK to CROSSREF
  mutate(category = replace(category, stem_id == "11_1684291849" & 
                              category == "stem_remark",
                            "stem_crossref")) |> 
  mutate(German_all = replace(German_all, stem_id == "11_1684291849" &
                                category == "stem_crossref",
                              "DEC 345 kadodo 'Artocarpus Gomeziana Wall.', DEC 355 kadodotok 'Artocarpus rigilda bl.', Heyne 64/1946 kadodohok 'Artocarpus rigida'")) |> 
  mutate(English_all = replace(English_all, stem_id == "11_1684291849" & 
                                 category == "stem_crossref",
                               "DEC 345 kadodo 'Artocarpus Gomeziana Wall.', DEC 355 kadodotok 'Artocarpus rigilda bl.', Heyne 64/1946 kadodohok 'Artocarpus rigida'"))

# get the concept column from the example translation GSheet
# ex_concept <- drive_get("2_example_german_translation") |>
#   read_sheet() |>
#   filter(!is.na(Concept), Concept != "DUPLICATE") |>
#   mutate(category = replace(category,
#                             example_id == "12_1683688193_0" &
#                               category == "example_GermanTranslationVariant",
#                             "example_GermanTranslation"))
# ex_concept |> write_rds("data-raw/ex_concept.rds")
ex_concept <- read_rds("data-raw/ex_concept.rds")

# get the example translation Google Spreadsheet that has been checked
# ex_translation_checked1 <- drive_get("2_example_german_translation-to-check-BATCH1") |>
#   read_sheet() |>
#   mutate(batch = 1)
# ex_translation_checked1 |> write_rds("data-raw/2_example_german_translation-to-check-BATCH1.rds")
ex_translation_checked1 <- read_rds("data-raw/2_example_german_translation-to-check-BATCH1.rds")

# ex_translation_checked2 <- drive_get("2_example_german_translation-to-check-BATCH2") |>
#   read_sheet() |>
#   mutate(batch = 2)
# ex_translation_checked2 |> write_rds("data-raw/2_example_german_translation-to-check-BATCH2.rds")
ex_translation_checked2 <- read_rds("data-raw/2_example_german_translation-to-check-BATCH2.rds")

ex_all_translation_checked <- bind_rows(ex_translation_checked1, ex_translation_checked2)
ex_all_translation_checked1 <- ex_all_translation_checked |> # combine the correction with the original German columns
  mutate(German_all = if_else(is.na(German_correction), German, German_correction)) |> 
  select(-German, -German_correction) |> # combine the correction with the original English columns
  mutate(English_all = if_else(is.na(English_correction), English, English_correction)) |> 
  select(-English, -English_correction) |>
  # filter out duplicates
  filter(str_detect(English_all, "DUPLICATE", negate = TRUE), stem_id != "12_1684999160") |> 
  
  # edit some example_GermanTranslation combined from example_GermanTranslationVariant
  mutate(category = replace(category, example_id == "12_1683688193_0", "example_GermanTranslation")) |> 
  mutate(German_all = if_else(str_detect(German_all, "die Früchte sitzen dicht"),
                              str_replace(German_all, "(sitzen)", "(sind gut angeordnet =) \\1"),
                              German_all)) |> 
  mutate(English_all = replace(English_all, English_all == "the fruits sit close together", "the fruits (are well arranged =) sit close together")) |> 
  mutate(German_all = if_else(example_id == "12_1684293179_2" & German_all == "Messer",
                              str_replace(German_all, "(Messer)", "(Anspitzensmittel =) \\1"),
                              German_all),
         English_all = replace(English_all, example_id == "12_1684293179_2" & English_all == "knife", "(sharpener =) knife")) |> 
  mutate(German_all = if_else(example_id == "12_1684393396_3" & German_all == "Begräbnisstätte",
                              str_replace(German_all, "(^.+$)", "(\"Verschwindensort\" =) \\1"),
                              German_all),
         English_all = replace(English_all, 
                               example_id == "12_1684393396_3" & English_all == "place of burial", 
                               "(\"place of disappearance\" =) burial site")) |> 
  mutate(German_all = if_else(example_id == "11_1684834366_4" & German_all == "das Vermengte der Leute sind Waren",
                              str_replace(German_all, "(^.+$)", "das Vermengte (= Getauschte) der Leute sind Waren"),
                              German_all),
         English_all = replace(English_all, example_id == "11_1684834366_4" & category == "example_GermanTranslation", "what people mix (= exchange) are goods")) |> 
  mutate(German_all = replace(German_all, example_id == "8_1685083404_2" & category == "example_GermanTranslation", "(\"Zuckerrohrblatt\" =) Schwert, damasziertes Haumesser"),
         English_all = replace(English_all, example_id == "8_1685083404_2" & category == "example_GermanTranslation", "(\"sugar cane leaf\" =) Sword, damascened cleaver")) |> 
  mutate(example_form = replace(example_form, example_id == "15_1684204179_6", "edohəao kipakũkũ hii ekẽpũ"))
  
## add the missing data for "ũmãhã́ũ .... ũmãhã́ũ"
ex_all_translation_checked2 <- ex_all_translation_checked1 |> 
  add_row(tibble_row(stem_id = "9_1687614424",
                     kms_Alphabet = "u",
                     kms_page = 285, 
                     kms_entry_no = 9, 
                     stem_form = "ũmãhã́ũ [a-]",
                     example_id = "9_1687614424_0",
                     example_entry_no = "00",
                     example_form = "ũmãhã́ũ .... ũmãhã́ũ", 
                     category = "example_GermanTranslation", 
                     batch = 2, 
                     German_all = "entweder ... oder",
                     English_all = "either ... or"))


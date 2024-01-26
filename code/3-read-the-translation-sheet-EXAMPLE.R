# Access google drive folder in the Enggano shared drive
library(googledrive)
library(googlesheets4)
library(tidyverse)

source("code/0-directory.R")

# list the files in the Kahler dictionary translation folder id
kahler_dict_files <- drive_ls(path = as_id(kahler_dict_folder))
kahler_dict_files

# read the example translation sheet using the id
ex_translation <- drive_get("2_example_german_translation") |> 
  read_sheet()

# prepare the translation file to be checked by Barnaby =====
source('code/1-pre-processing.R')
ex_to_check <- stems4 |> 
  select(1:5) |> 
  left_join(ex_translation) |> 
  filter(!is.na(example_id)) |> 
  arrange(kms_page, kms_Alphabet, kms_entry_no) |> 
  mutate(example_entry_no = str_extract(example_id, "(?<=_)\\d+$"),
         example_entry_no = if_else(nchar(example_entry_no) == 1, paste("0", example_entry_no, sep = ""), example_entry_no),
         category = factor(category, levels = c("example_GermanTranslation", 
                                                "example_GermanTranslationVariant",
                                                "example_crossref",
                                                "example_remark"))) |> 
  arrange(kms_page, kms_Alphabet, kms_entry_no, example_entry_no, category)

## merge the German translation into one column
ex_to_check1 <- ex_to_check |> 
  mutate(German_all = if_else(is.na(German_corrected) & 
                                is.na(German_noun_and_all), 
                              German, 
                              German_noun_and_all))

## BATCH 1: filter the to-check only and add the example form =============
# ex_to_check2 <- ex_to_check1 |> 
#   filter(TO_CHECK) |> 
#   left_join(examples3 |> select(1:3)) |> 
#   mutate(German_correction = NA, English, English_correction = NA) |> 
#   select(1:6, example_entry_no, example_form, category, German = German_all, German_correction, English, English_correction)
# ex_to_check2

## create an empty gsheet to store the translation to be checked
# drive_create(name = '2_example_german_translation-to-check-BATCH1', 
#              path = 'https://drive.google.com/drive/folders/1KY-XunZ0mzkXxCJbbrOFDH1vgmCBTpSV',
#              type = 'spreadsheet')
# Created Drive file:
#   • 2_example_german_translation-to-check-BATCH1 <id: 15yvHhEQaTTHg_9eR0IimizhDtAHbX27EwOJacdjHvHs>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet

## now save the stem translation tibble to the created spreadsheet (BATCH 1)
# sheet_write(ex_to_check2, ss = '15yvHhEQaTTHg_9eR0IimizhDtAHbX27EwOJacdjHvHs', sheet = 'Sheet1')


## BATCH 2: filter the to-check only and add the example form =============
ex_to_check2 <- ex_to_check1 |> 
  filter(TO_CHECK, BATCH_CHECK == 2) |> 
  left_join(examples3 |> select(1:3)) |> 
  mutate(German_correction = NA, English, English_correction = NA) |> 
  select(1:6, example_entry_no, example_form, category, German = German_all, German_correction, English, English_correction) |> 
  arrange(kms_page, kms_Alphabet, kms_entry_no, example_entry_no, category)
ex_to_check2

## create an empty gsheet to store the BATCH 2 translation to be checked
# drive_create(name = '2_example_german_translation-to-check-BATCH2',
#              path = 'https://drive.google.com/drive/folders/1KY-XunZ0mzkXxCJbbrOFDH1vgmCBTpSV',
#              type = 'spreadsheet')
# Created Drive file:
#   • 2_example_german_translation-to-check-BATCH2
# <id: 1eAW-zXLHVfgHkDfTkCr3GcH19wDKwe6P8unaW3e6kPc>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet
 
## now save the stem translation tibble to the created spreadsheet (BATCH 2)
# sheet_write(ex_to_check2, ss = '1eAW-zXLHVfgHkDfTkCr3GcH19wDKwe6P8unaW3e6kPc', sheet = 'Sheet1')

# NOTE: This code file records the process to prepare the STEM/ROOT data which English translation from German to be checked.

library(googledrive)
library(googlesheets4)

# Access google drive folder in the Enggano shared drive
source("code/0-directory.R")

# list the files in the Kahler dictionary translation folder id
kahler_dict_files <- drive_ls(path = as_id(kahler_dict_folder))
kahler_dict_files

# read the stem translation sheet using the id
stem_translation <- drive_get("1_stem_german_translation") |> read_sheet()

# prepare the translation file for the stem to be checked by Barnaby
source('code/1-pre-processing.R')
stems4_mini <- stems4 |> 
  select(stem_id, kms_Alphabet, kms_page, kms_entry_no, stem_form)
stems4_mini
stem_translation_to_check <- stem_translation |> 
  left_join(stems4_mini, by = join_by(stem_id)) |> 
  mutate(category = factor(category, levels = c('stem_GermanTranslation', 'stem_GermanTranslationVariant',
                                                'stem_crossref', 'stem_remark')),
         German_corrected = NA) |> 
  select(stem_id, kms_Alphabet, kms_page, kms_entry_no, stem_form, German, German_corrected, everything()) |> 
  arrange(category, kms_page, kms_entry_no)
stem_translation_to_check

# create an empty gsheet to store the translation to be checked
# drive_create(name = '1_stem_german_translation-to-check', 
#              path = 'https://drive.google.com/drive/folders/1KY-XunZ0mzkXxCJbbrOFDH1vgmCBTpSV',
#              type = 'spreadsheet')
# Created Drive file:
#   • 1_stem_german_translation-to-check <id: 1Xnd7hqZAv3-zuWcOCmbDpLOPOJrSIojSsM5blycI4Lw>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet

# now save the stem translation tibble to the created spreadsheet
# sheet_write(stem_translation_to_check, ss = '1Xnd7hqZAv3-zuWcOCmbDpLOPOJrSIojSsM5blycI4Lw', sheet = 'Sheet1')

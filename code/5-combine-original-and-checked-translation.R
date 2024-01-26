source("code/4-pre-processing-the-checked-translation.R")
source('code/1-pre-processing.R')

# Custom fixing
stems_translation_checked1 <- stems_translation_checked1 |> 
  mutate(English_all = str_replace_all(English_all,
                                       "(§+)(\\d)",
                                       "\\1 \\2"))

# 1. ROOT data and its translations that have been checked =======
stems_translation_root <- stems_translation_checked1 |> 
  filter(category == "stem_GermanTranslation") |> 
  select(-category) |> 
  rename(stem_GermanTranslation_DE = German_all) |> 
  rename(stem_English = English_all)
# CHECK if the stem_id of the original database is the same with (i.e. present in) the checked translation
all(sort(pull(filter(stems4, !is.na(stem_GermanTranslation)), stem_id)) ==
    sort(stems_translation_root$stem_id))
# [1] TRUE

# 2. ROOT VARIANT data and its translations that have been checked =======
stems_translation_variant <- stems_translation_checked1 |> 
  filter(category == "stem_GermanTranslationVariant") |> 
  select(-category) |> 
  rename(stem_GermanTranslationVariant_DE = German_all) |> 
  rename(stem_EnglishVariant = English_all)
# CHECK if the stem_id of the original database is the same with (i.e. present in) the checked translation
all(sort(pull(filter(stems4, !is.na(stem_GermanTranslationVariant)), stem_id)) ==
      sort(stems_translation_variant$stem_id))
# [1] TRUE

# 3. ROOT REMARK data and its translations that have been checked =======
stems_translation_remark <- stems_translation_checked1 |> 
  filter(category == "stem_remark") |> 
  select(-category) |> 
  rename(stem_remark_DE = German_all) |> 
  rename(stem_remark_EN = English_all) |> 
  ## REMARK custom editing and fixing ======
  mutate(stem_remark_EN = replace(stem_remark_EN, 
                                  stem_remark_DE == "Makassar tamataj d Tote",
                                  "Makassar \"tamataj\" 'dead person'")) |> 
  mutate(stem_remark_EN = str_replace(stem_remark_EN,
                                      "^arch\\;",
                                      "archaic ;")) |> 
  mutate(stem_remark_EN = str_replace(stem_remark_EN,
                                      "\\b(now)[ː: ]",
                                      "\\1:")) |> 
  mutate(stem_remark_EN = str_replace(stem_remark_EN,
                                      "^(MOD) (209)\\.(211)",
                                      "\\1:\\2,\\3")) |> 
  mutate(stem_remark_EN = str_replace(stem_remark_EN, 
                                      "MOD 219", 
                                      "MOD:219")) |> 
  mutate(stem_remark_EN = str_replace(stem_remark_EN,
                                      "(?<=known\\sas\\s)(cohia)",
                                      "<form><w xml:lang=\"eno\">\\1</w></form>")) |> 
  mutate(stem_remark_EN = str_replace(stem_remark_EN,
                                      "\\sauf\\s",
                                      " on ")) |> 
  mutate(stem_remark_EN = str_replace(stem_remark_EN,
                                      "'([(]fig\\.12.+?)(?=cap\\sadorned)",
                                      "\\1'")) |> 
  mutate(stem_remark_EN = if_else(str_detect(stem_remark_EN, "MOD"),
                                  str_replace_all(stem_remark_EN, 
                                                  "\\b(èuba cahába|comaú|èkanoeoenoe|canuúnu|èkietohè|echitoè|ecobá|Ecoè|ecoè|eupurúhui|eakõmãʔã:õ|èkoekjou)\\b",
                                                  "<form><w xml:lang=\"eno\">\\1</w></form>"),
                                  stem_remark_EN)) |> 
  mutate(stem_remark_EN = str_replace_all(stem_remark_EN,
                                          "<\\/form\\> 'bird's claw'", 
                                          " <gloss>bird's claw</gloss></form>"),
         stem_remark_EN = if_else(str_detect(stem_remark_EN, "MOD"),
                                  str_replace_all(stem_remark_EN,
                                                  "(<\\/form\\>)\\s'([^']+?)'", 
                                                  " <gloss>\\2</gloss>\\1"),
                                  stem_remark_EN),
         stem_remark_EN = if_else(str_detect(stem_remark_EN, "MOD"),
                                  str_replace_all(stem_remark_EN,
                                                  "(<\\/form>) \\(fig\\.12 on S\\.152\\) '(cap adorned with feathers)'",
                                                  " (fig.12 on S.152) <gloss>\\2</gloss>\\1"),
                                  stem_remark_EN))
# CHECK if the stem_id of the original database is the same with (i.e. present in) the checked translation
all(sort(pull(filter(stems4, !is.na(stem_remark)), stem_id)) ==
      sort(stems_translation_remark$stem_id))
# [1] FALSE
# Warning message:
#   In sort(pull(filter(stems4, !is.na(stem_remark)), stem_id)) == sort(stems_translation_remark$stem_id) :
#   longer object length is not a multiple of shorter object length
# CHECK which id is not in the translation database
missing_id_remark <- setdiff(sort(pull(filter(stems4, !is.na(stem_remark)), stem_id)), 
        sort(stems_translation_remark$stem_id))
missing_id_remark
# [1] "19_1683781496"
missing_remark_df <- stems4 |> 
  filter(stem_id %in% missing_id_remark) |> 
  mutate(Indonesian = NA, Remark = NA, stem_remark_EN = NA) |> 
  select(stem_id, kms_Alphabet, kms_page, kms_entry_no, stem_form, 
         Indonesian, Remark, stem_remark_DE = stem_remark, stem_remark_EN)
stems_translation_remark <- stems_translation_remark |> 
  bind_rows(missing_remark_df)
# CHECK AGAIN if the stem_id of the original database is the same with (i.e. present in) the checked translation
all(sort(pull(filter(stems4, !is.na(stem_remark)), stem_id)) ==
      sort(stems_translation_remark$stem_id))
# [1] TRUE


# 4. ROOT CROSSREF data and its translations that have been checked =======
stems_translation_crossref <- stems_translation_checked1 |> 
  filter(category == "stem_crossref") |> 
  select(-category) |> 
  rename(stem_crossref_DE = German_all) |> 
  rename(stem_crossref_EN = English_all) |> 
  ## CROSSREF custom editing and fixing ======
  mutate(stem_crossref_EN = str_replace_all(stem_crossref_EN,
                                            "(?<=\\d)Z(?=\\d)",
                                            "z")) |> 
  mutate(stem_crossref_EN = str_replace_all(stem_crossref_EN,
                                            "(?<=\\d)o",
                                            "0"))
# CHECK if the stem_id of the original database is the same with the checked translation
all(sort(pull(filter(stems4, !is.na(stem_crossref)), stem_id)) ==
      sort(stems_translation_crossref$stem_id))
# [1] FALSE
# Warning message:
#   In sort(pull(filter(stems4, !is.na(stem_crossref)), stem_id)) ==  :
#   longer object length is not a multiple of shorter object length
# CHECK which id is not in the translation database
missing_id_crossref <- setdiff(sort(pull(filter(stems4, !is.na(stem_crossref)), stem_id)), 
                               sort(stems_translation_crossref$stem_id))
missing_id_crossref
# [1] "10_1689608120"
missing_crossref_df <- stems4 |> 
  filter(stem_id %in% missing_id_crossref) |> 
  mutate(Indonesian = NA, Remark = NA, stem_crossref_EN = NA) |> 
  select(stem_id, kms_Alphabet, kms_page, kms_entry_no, stem_form, 
         Indonesian, Remark, stem_crossref_DE = stem_crossref, stem_crossref_EN)
stems_translation_crossref <- stems_translation_crossref |> 
  bind_rows(missing_crossref_df)
# CHECK AGAIN if the stem_id of the original database is the same with the checked translation
all(sort(pull(filter(stems4, !is.na(stem_crossref)), stem_id)) ==
      sort(stems_translation_crossref$stem_id))
# [1] TRUE
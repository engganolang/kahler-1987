stem_df <- stem_main_tb |> 
  mutate(stem_form_original = stem_form)

# pure vowel repetition (long vowel) ====
stem_df |> 
  select(stem_id, stem_form) |> 
  mutate(repeated = str_extract_all(stem_form, "([aiueoəɔ]+)\\1")) |> 
  unnest_longer(repeated) |> 
  select(repeated) |> 
  distinct()
# # A tibble: 5 × 1
# repeated
# <chr>   
#   1 aa      
# 2 oo      
# 3 uu      
# 4 ee      
# 5 ii
# 6 əə

# vowel with diacritics ====
## STEM FORM ====
stem_df |> 
  select(stem_id, stem_form) |> 
  mutate(repeated = str_extract_all(stem_form, "([aiueoɔə][́ ̀ ̃ ][̀ ́ ̃ ]?)\\1")) |>
  unnest_longer(repeated) |> 
  select(repeated) |> 
  distinct() |> 
  arrange(repeated)
# A tibble: 13 × 1
# repeated
# <chr>   
#   1 ãã      
# 2 ã́ã́      
# 3 ẽẽ      
# 4 íí      
# 5 ĩĩ      
# 6 ĩ́ĩ́      
# 7 óó      
# 8 õõ      
# 9 ṍṍ      
# 10 úú      
# 11 ũũ      
# 12 ṹṹ      
# 13 ə̃ə̃

## STEM VARIANT ====
stem_main_tb |> 
  select(stem_id, stem_formVarian) |> 
  mutate(repeated = str_extract_all(stem_formVarian, "([aiueoɔə][́ ̀ ̃ ][̀ ́ ̃ ]?)\\1")) |>
  unnest_longer(repeated) |> 
  select(repeated) |> 
  distinct() |> 
  arrange(repeated)
# A tibble: 9 × 1
# repeated
# <chr>   
#   1 áá      
# 2 ãã      
# 3 ẽẽ      
# 4 õõ      
# 5 ṍṍ      
# 6 úú      
# 7 ũũ      
# 8 ə̃ə̃      
# 9 NA


# START FROM HERE ====

# stem_df |> 
#   select(stem_id, stem_form, stem_form_original) |> 
#   mutate(stem_form_original = str_replace_all(stem_form_original, "([aiueoə]+)\\1", "\\1̄")) 

# qlcData::write.profile(stem_main_tb$stem_form,
#                        editing = TRUE,
#                        info = TRUE,
#                        file.out = "data-raw/stem_form_profile_to_original_skeleton.tsv")
# qlcData::write.profile(stem_main_tb$stem_form,
#                        editing = TRUE,
#                        info = TRUE,
#                        file.out = "data-raw/stem_form_profile_to_common_skeleton.tsv")
# qlcData::write.profile(stem_main_tb$stem_formVarian,
#                        editing = TRUE,
#                        info = TRUE,
#                        file.out = "data-raw/stem_variant_profile_to_original_skeleton.tsv")

## STEM FORM ====
stem_main_tb <- stem_main_tb |> 
  mutate(ID = row_number()) |> 
  select(ID, everything())

stem_forms_orig <- qlcData::tokenize(stem_main_tb$stem_form,
                                     profile = "data-raw/stem_form_profile_to_original_skeleton.tsv",
                                     method = "global",
                                     transliterate = "Replacement",
                                     ordering = NULL,
                                     normalize = "NFC",
                                     sep.replace = "#",
                                     regex = TRUE)
as_tibble(stem_forms_orig$strings)
as_tibble(stem_forms_orig$strings) |> 
  # join characters of diphthong ========
  mutate(transliterated = str_replace_all(transliterated, " : ", "͜")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated, "\\s", "")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated_nontokenized, "\\#", " "))

### CHECK THE CAPITAL GRAPHEME FOR VOWEL IN THE FORM ====
filter(stem_main_tb, stem_form %in% filter(as_tibble(stem_forms_orig$strings), str_detect(originals, "[AIUEO]"))$originals) |> 
  select(stem_form, stem_EN)
### CHECK THE CAPITAL GRAPHEME FOR CONSONANT IN THE FORM ====
filter(stem_main_tb, stem_form %in% filter(as_tibble(stem_forms_orig$strings), str_detect(originals, "[A-Z]"))$originals) |> 
  select(stem_form, stem_EN) |> 
  filter(str_detect(stem_form, "[AIUEO]", negate = TRUE))
#### [IMPORTANT] no need to change the capitals here into underlined letters except for the accented vowel in capital

stem_forms_comm <- qlcData::tokenize(stem_main_tb$stem_form,
                                     profile = "data-raw/stem_form_profile_to_common_skeleton.tsv",
                                     method = "global",
                                     transliterate = "Replacement",
                                     ordering = NULL,
                                     normalize = "NFC",
                                     sep.replace = "#",
                                     regex = TRUE)

as_tibble(stem_forms_comm$strings) |> 
  # join characters of diphthong ========
  mutate(transliterated = str_replace_all(transliterated, " : ", ":")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated, "\\s", "")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated_nontokenized, "\\#", " "))


## STEM VARIANT FORM ====
stem_variant_orig <- qlcData::tokenize(stem_main_tb$stem_formVarian,
                                       profile = "data-raw/stem_variant_profile_to_original_skeleton.tsv",
                                       method = "global",
                                       transliterate = "Replacement",
                                       ordering = NULL,
                                       normalize = "NFC",
                                       sep.replace = "#",
                                       regex = TRUE)

### CHECK THE CAPITAL GRAPHEME FOR VOWEL IN THE FORM ====
filter(stem_main_tb, stem_formVarian %in% filter(as_tibble(stem_variant_orig$strings), str_detect(originals, "[AIUEO]"))$originals) |> 
  select(stem_formVarian)

# stem_variant_orig$missing |> 
#   write_tsv("data-raw/stem_variant_profile_to_original_missing.tsv")
as_tibble(stem_variant_orig$strings)
as_tibble(stem_variant_orig$strings) |> 
  # join characters of diphthong ========
mutate(transliterated = str_replace_all(transliterated, " : ", "͜")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated, "\\s", "")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated_nontokenized, "[#]", " "))

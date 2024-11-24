library(tidyverse)
library(qlcData)
# stem_main_tb <- read_rds("data-main/stem_main_tb.rds")
# ex_main_tb <- read_rds("data-main/examples_main_tb.rds")
stem_df <- stem_main_tb |> 
  mutate(stem_form_original = stem_form)


# pure vowel repetition (long vowel) ====
## STEM FORM ====
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

## EXAMPLE FORM ====
ex_main_tb |> 
  select(stem_id, example_id, example_form) |> 
  mutate(repeated = str_extract_all(example_form, "([aiueoəɔ]+)\\1")) |> 
  unnest_longer(repeated) |> 
  select(repeated) |> 
  distinct()
# A tibble: 7 × 1
# repeated
# <chr>   
#   1 oo      
# 2 ee      
# 3 uu      
# 4 aa      
# 5 ii      
# 6 eooeoo  
# 7 əə   


## EXAMPLE VARIANT =====
ex_main_tb |> 
  select(stem_id, example_id, example_variant) |> 
  filter(!is.na(example_variant)) |> 
  mutate(repeated = str_extract_all(example_variant, "([aiueoəɔ]+)\\1")) |> 
  unnest_longer(repeated) |> 
  select(repeated) |> 
  distinct()
# # A tibble: 5 × 1
# repeated
# <chr>   
#   1 aa      
# 2 oo      
# 3 ii      
# 4 ee      
# 5 uu



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

## EXAMPLE FORM =====
ex_main_tb |> 
  select(stem_id, example_id, example_form) |> 
  mutate(repeated = str_extract_all(example_form, "([aiueoɔə][́ ̀ ̃ ][̀ ́ ̃ ]?)\\1")) |>
  unnest_longer(repeated) |> 
  select(repeated) |> 
  distinct() |> 
  arrange(repeated)
# # A tibble: 10 × 1
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
# 9 ũũ      
# 10 ə̃ə̃ 


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

## EXAMPLE VARIANT =====
ex_main_tb |> 
  select(stem_id, example_id, example_variant) |> 
  filter(!is.na(example_variant)) |> 
  mutate(repeated = str_extract_all(example_variant, "([aiueoɔə][́ ̀ ̃ ][̀ ́ ̃ ]?)\\1")) |>
  unnest_longer(repeated) |> 
  select(repeated) |> 
  distinct() |> 
  arrange(repeated)
# # A tibble: 4 × 1
# repeated
# <chr>   
#   1 ãã      
# 2 ĩĩ      
# 3 õõ      
# 4 ũũ


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
# qlcData::write.profile(stem_main_tb$stem_formVarian,
#                        editing = TRUE,
#                        info = TRUE,
#                        file.out = "data-raw/stem_variant_profile_to_common_skeleton.tsv")
# qlcData::write.profile(ex_main_tb$example_form,
#                        editing = TRUE,
#                        info = TRUE,
#                        file.out = "data-raw/ex_form_profile_to_original_skeleton.tsv")
# qlcData::write.profile(ex_main_tb$example_variant,
#                        editing = TRUE,
#                        info = TRUE,
#                        file.out = "data-raw/ex_variant_profile_to_original_skeleton.tsv")
# qlcData::write.profile(ex_main_tb$example_form,
#                        editing = TRUE,
#                        info = TRUE,
#                        file.out = "data-raw/ex_form_profile_to_common_skeleton.tsv")
# qlcData::write.profile(ex_main_tb$example_variant,
#                        editing = TRUE,
#                        info = TRUE,
#                        file.out = "data-raw/ex_variant_profile_to_common_skeleton.tsv")


## STEM FORM ====
stem_main_tb <- stem_main_tb |> 
  mutate(ID = row_number()) |> 
  select(ID, everything())

### Tracing into the original forms ====
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


### Tracing into the common transcription forms =====
stem_forms_comm <- qlcData::tokenize(stem_main_tb$stem_form,
                                     profile = "data-raw/stem_form_profile_to_common_skeleton.tsv",
                                     method = "global",
                                     transliterate = "Replacement",
                                     ordering = NULL,
                                     normalize = "NFC",
                                     sep.replace = "#",
                                     regex = TRUE)

stem_forms_comm_tb <- as_tibble(stem_forms_comm$strings) |> 
  # join characters of diphthong ========
mutate(transliterated = str_replace_all(transliterated, " : ", ":")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated, "\\s", "")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated_nontokenized, "\\#", " ")) |> 
  select(stem_form_comm_tokenised = transliterated,
         stem_form_comm_untokenised = transliterated_nontokenized)
stem_main_tb <- stem_main_tb |> 
  bind_cols(stem_forms_comm_tb)

## EXAMPLE FORM =====

### Tracing into the common transcription forms ====
ex_forms_comm <- qlcData::tokenize(ex_main_tb$example_form,
                                   profile = "data-raw/ex_form_profile_to_common_skeleton.tsv",
                                   method = "global",
                                   transliterate = "Replacement",
                                   ordering = NULL,
                                   normalize = "NFC",
                                   sep.replace = "#",
                                   regex = TRUE)

ex_form_comm_tb <- as_tibble(ex_forms_comm$strings) |> 
  # join characters of diphthong ========
  mutate(transliterated = str_replace_all(transliterated, " : ", ":")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated, "\\s", "")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated_nontokenized, "\\#", " ")) |> 
  select(example_form_comm_tokenised = transliterated,
         example_form_comm_untokenised = transliterated_nontokenized)
ex_main_tb <- ex_main_tb |> 
  bind_cols(ex_form_comm_tb)


## STEM VARIANT FORM ====
### Tracing into the original form =====
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


### Tracing into the common form =====
stem_variant_comm <- qlcData::tokenize(stem_main_tb$stem_formVarian,
                                       profile = "data-raw/stem_form_profile_to_common_skeleton.tsv",
                                       method = "global",
                                       transliterate = "Replacement",
                                       ordering = NULL,
                                       normalize = "NFC",
                                       sep.replace = "#",
                                       regex = TRUE)

# write_tsv(stem_variant_comm$missing, "data-raw/stem_variant_profile_to_common_missing.tsv")

### CHECK THE CAPITAL GRAPHEME FOR VOWEL IN THE FORM ====
filter(stem_main_tb, stem_formVarian %in% filter(as_tibble(stem_variant_comm$strings), str_detect(originals, "[AIUEO]"))$originals) |> 
  select(stem_formVarian, stem_variant_EN)

as_tibble(stem_variant_comm$strings)
stem_formVarian_comm <- as_tibble(stem_variant_comm$strings) |> 
  # join characters of diphthong ========
  mutate(transliterated = str_replace_all(transliterated, " : ", ":")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated, "\\s", "")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated_nontokenized, "\\#", " ")) |> 
  select(stem_formVarian_tokenised = transliterated,
         stem_formVarian_untokenised = transliterated_nontokenized)
stem_main_tb <- stem_main_tb |> 
  bind_cols(stem_formVarian_comm) |> 
  relocate(stem_form_comm_untokenised, .after = stem_form) |> 
  relocate(stem_form_comm_tokenised, .after = stem_form_comm_tokenised) |> 
  relocate(stem_formVarian_tokenised, .after = stem_formVarian) |> 
  relocate(stem_formVarian_untokenised, .after = stem_formVarian)

## EXAMPLE VARIANT FORM =====
ex_variant_comm <- qlcData::tokenize(ex_main_tb$example_variant,
                                     profile = "data-raw/ex_form_profile_to_common_skeleton.tsv",
                                     method = "global",
                                     transliterate = "Replacement",
                                     ordering = NULL,
                                     normalize = "NFC",
                                     sep.replace = "#",
                                     regex = TRUE)
# ex_variant_comm$missing |> write_tsv("data-raw/ex_variant_profile_to_commong_missing.tsv")
ex_variant_comm_tb <- as_tibble(ex_variant_comm$strings) |> 
  # join characters of diphthong ========
mutate(transliterated = str_replace_all(transliterated, " : ", ":")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated, "\\s", "")) |> 
  mutate(transliterated_nontokenized = str_replace_all(transliterated_nontokenized, "\\#", " ")) |> 
  select(ex_variant_comm_tokenised = transliterated,
         ex_variant_comm_untokenised = transliterated_nontokenized)
ex_main_tb <- ex_main_tb |> 
  bind_cols(ex_variant_comm_tb) |> 
  relocate(ex_variant_comm_tokenised, .after = example_variant) |> 
  relocate(ex_variant_comm_untokenised, .after = example_variant) |> 
  relocate(example_form_comm_tokenised, .after = example_form) |> 
  relocate(example_form_comm_untokenised, .after = example_form)

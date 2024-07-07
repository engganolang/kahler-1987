# NOTE: This code file check entries that originally have no German translation to be checked.

# source("code/4-pre-processing-the-checked-translation.R")
# source('code/1-pre-processing.R')

### STEMS ========

# stems that have no German
stem_no_german <- stems4 |> 
  filter(is.na(stem_GermanTranslation)) # |> 
  # select(matches("German|remark|cross")) |> 
  # filter(if_any(where(is.character), ~!is.na(.)))
stem_no_german_id <- stem_no_german$stem_id

## check if the stem id whose form has no German also appears in the checked translation for the stem
stems_translation_checked2 |> 
  filter(stem_id %in% stem_no_german_id)
# # A tibble: 263 × 10
### the output contains 263 entries and these are NOT from the German translation category, but from Cross-Ref, Remark, and German of the stem_variant (This has been fixed as this stem_GermanTranslationVariant should be in the remark)


## check which stem id comes from the form without German translation that are not in the checked translation
stem_no_german_in_checked_file <- setdiff(stem_no_german_id, stems_translation_checked2$stem_id)

# 1. stem forms that has no German translation in the checked file.
stem_no_german_in_checked_file_df <- stems4 |> 
  filter(stem_id %in% stem_no_german_in_checked_file)
## the entries also have no remark, and no cross-reference
stem_no_german_in_checked_file_df |> filter(!is.na(stem_remark))
# A tibble: 0 × 25
stem_no_german_in_checked_file_df |> filter(!is.na(stem_crossref))
# A tibble: 0 × 25

# 2. stem form in the original `stems4` but missing from `stems_translation_checked2`
filter(stems4, stem_id %in% stems4$stem_id[!stems4$stem_id %in% stems_translation_checked2$stem_id])
## the entries also have no remark, and no cross-reference
filter(stems4, stem_id %in% stems4$stem_id[!stems4$stem_id %in% stems_translation_checked2$stem_id]) |> 
  filter(!is.na(stem_remark))
# A tibble: 0 × 25
filter(stems4, stem_id %in% stems4$stem_id[!stems4$stem_id %in% stems_translation_checked2$stem_id]) |> 
  filter(!is.na(stem_crossref))
# A tibble: 0 × 25
# 3. code in 1. and 2. above converges, since the IDs all appear in the two data frame as indicated by the following code:
code1_id <- filter(stems4, stem_id %in% stems4$stem_id[!stems4$stem_id %in% stems_translation_checked2$stem_id])$stem_id
code2_id <- stem_no_german_in_checked_file_df$stem_id
all(code1_id %in% code2_id)
## the data frame `stem_no_german_in_checked_file_df` contains ~
## entries that ORIGINALLY (i.e., before being checked) have NO GERMAN TRANSLATION for the stem, NO REMARK, and NO CROSSREFERENCE.


# 4. Check which entries in `stems4` that:
## a. has NO GERMAN TRANSLATION of the stem, BUT

## b. has entries for the remark 
stem_in_stem4_without_german_but_with_remark <- stems4 |> 
  filter(is.na(stem_GermanTranslation)) |> 
  filter(!is.na(stem_remark)) 
stem_in_stem4_without_german_but_with_remark 
### there are only 51 entries for the REMARK
# A tibble: 51 × 25

## c. then, check if these REMARK entries also appear in the `stems_translation_checked2`
stems_translation_checked2 |> 
  filter(stem_id %in% stem_in_stem4_without_german_but_with_remark$stem_id) |> 
  filter(category == "stem_remark")
### # A tibble: 51 × 10
### GREAT, the number matches: 51 entries of the REMARK are also in the `stem_translation_checked1`

# 5. Check which entries in `stems4` that:
## a. has NO GERMAN TRANSLATION of the stem, BUT

## b. has entries for the crossref 
stem_in_stem4_without_german_but_with_crossref <- stems4 |> 
  filter(is.na(stem_GermanTranslation)) |> 
  filter(!is.na(stem_crossref))
stem_in_stem4_without_german_but_with_crossref
### there are only 212 entries
# A tibble: 212 × 25

## c. then, check if these CROSSREFERENCE entries also appear in the `stems_translation_checked2`
stems_translation_checked2 |> 
  filter(stem_id %in% stem_in_stem4_without_german_but_with_crossref$stem_id) |> 
  filter(category == "stem_crossref")
# A tibble: 212 × 10
### GREAT, the number matches: 212 entries of the CROSSREF are also in the `stem_translation_checked1`

# NOTE: the output of codes point 4 and 5 above indicates that we need to combine, from `stems4`,
# only entries that have no German for the stem, no entries for remark and no entries for crossreference.
# these entries are available in `stem_no_german_in_checked_file_df` from code point 1.
stem_no_german_in_checked_file_df


## D. STEMS REMAINING COLUMNS ==============
stems4_rests <- stems4 |> 
  select(1:5,
         stem_homonymID,
         stem_formVarian,
         stem_dialectVariant,
         stem_etymological_form,
         stem_etym_form_German,
         stem_etymological_language_donor,
         stem_loanword_form,
         stem_loanword_language_donor,
         stem_source_form,
         stem_source_form_homonymID)

### D1. combining `stems4` with `stems_translation_root/variant/remark/crossref`
stem_all <- stems4 |> 
  left_join(stems_translation_root) |> 
  left_join(stems_translation_variant) |> 
  left_join(stems_translation_remark) |> 
  left_join(stems_translation_crossref) |> 
  select(!matches("^create|update|used_time|complete")) |> 
  arrange(kms_Alphabet, kms_page, kms_entry_no) |> 
  mutate(stem_etym_DE = if_else(!is.na(stem_etym_form_German), stem_etym_form_German, NA),
         stem_etym_EN = if_else(stem_etym_DE == "\"der zu Angelnde\"", "the one to be fished", NA))


### EXAMPLES ========
#### IMPORTANT: some of the following codes can be run after first running codes in `5-combine-original-and-checked...`


#### retrieve entries of example_forms that have no German translation
ex_no_german_for_form <- examples3 |> filter(is.na(example_GermanTranslation))
ex_no_german_for_form_id <- ex_no_german_for_form$example_id

ex_no_german_for_form |> 
  filter(is.na(example_GermanTranslationVariant), 
         is.na(example_remark), is.na(example_crossref)) -> ex_no_german_for_form_variant_remark_crossref



#### 1. from `ex_no_german_for_form`, check if their IDs are absent from the example_form having German translation that is checked
##### IMPORTANT: the following code can be run after codes in `5-combine-original-and-checked...` have been run!
##### NOTE ON RESULTS: so it is correct that IDs in `ex_no_german_for_form` are absent from the checked translation of the example_form having German translation
# > ex_form_translation |> filter(example_id %in% ex_no_german_for_form_id)
# A tibble: 0 × 10
# ℹ 10 variables: stem_id <chr>, kms_Alphabet <chr>, kms_page <dbl>, kms_entry_no <dbl>, stem_form <chr>, example_id <chr>, example_entry_no <chr>,
#   example_form <chr>, ex_GermanTranslation_DE <chr>, ex_English <chr>
##### CHECK IF THE EXAMPLE_ID from `examples3` (that has example_GermanTranslation) and `ex_form_translation` matches
ex_id_that_has_german_translation <- pull(filter(examples3, !is.na(example_GermanTranslation)), example_id)
ex_id_that_has_german_translation_checked <- ex_form_translation$example_id
setdiff(ex_id_that_has_german_translation, ex_id_that_has_german_translation_checked)
# character(0)
all(sort(ex_id_that_has_german_translation) == sort(ex_id_that_has_german_translation_checked))
# [1] TRUE

#### 2. from `ex_no_german_for_form`, check if there are entries for the GermanVariant and crosscheck with the checked translation file
ex_no_german_has_germanvariant <- filter(ex_no_german_for_form, !is.na(example_GermanTranslationVariant))
ex_no_german_has_germanvariant
# A tibble: 0 × 20 <- zero case
filter(ex_all_translation_checked3, example_id  %in% ex_no_german_has_germanvariant$example_id) |> filter(category == "example_GermanTranslationVariant")
# A tibble: 0 × 12 <- zero case match! YES, the GermanTransVariant in `ex_no_german_has_germanvariant` has been checked


#### 3. from `ex_no_german_for_form`, check if there are entries for the Remark and crosscheck with the checked translation file
ex_no_german_has_remark <- filter(ex_no_german_for_form, !is.na(example_remark))
ex_no_german_has_remark
# A tibble: 6 × 20 <- only six cases
filter(ex_all_translation_checked3, example_id  %in% ex_no_german_has_remark$example_id) |> filter(category == "example_remark")
# A tibble: 6 × 12 <- six cases match! YES, the example_remark in `ex_no_german_has_remark` has been checked


#### 4. from `ex_no_german_for_form`, check if there are entries for the CROSSREF and crosscheck with the checked translation file
ex_no_german_has_crossref <- filter(ex_no_german_for_form, !is.na(example_crossref))
ex_no_german_has_crossref
# A tibble: 14 × 20 <- fourteen cases
filter(ex_all_translation_checked3, example_id  %in% ex_no_german_has_crossref$example_id) |> filter(category == "example_crossref")
# A tibble: 14 × 12 <- fourteen cases match! YES, the example_crossref in `ex_no_german_has_crossref` has been checked

### SUMMARY NOTES
#### the data in `ex_form_translation` (example_form that has German translation) from output of codes in `5-combine-original-...`
#### needs to be combined with `ex_no_german_for_form_variant_remark_crossref` (example_form that has NO German translation, No German for variant, NO Remark, and NO Crossreferences) from the current code file.
ex_no_german_for_form_variant_remark_crossref1 <- ex_no_german_for_form_variant_remark_crossref |> 
  select(stem_id, example_id, example_form) |> 
  left_join(stems4[, 1:5]) |> 
  mutate(example_entry_no = str_extract(example_id, "(?<=_)\\d+$"),
         example_entry_no = if_else(nchar(example_entry_no) == 1, paste("0", example_entry_no, sep = ""), example_entry_no)) |> 
  mutate(ex_DE = NA, ex_EN = NA, ex_concept = NA)



## D. EXAMPLES REMAINING COLUMNS ===========
examples3_rests <- examples3 |> 
  select(example_id, 
         stem_id, 
         example_form, 
         example_variant, 
         example_etymological_form, 
         example_etymological_language_donor, 
         example_loanword_form, 
         example_loanword_language_donor, 
         example_source_form, 
         example_source_form_homonymID, 
         example_dialect_variant)

### D1. combining `examples3` with `ex_form/remark/crossref/variant_translation`
example_all <- examples3 |> 
  left_join(ex_form_translation |> 
              select(-kms_Alphabet, -kms_page, -kms_entry_no, -stem_form, -example_entry_no)) |> 
  left_join(ex_variant_translation |> 
              select(-kms_Alphabet, -kms_page, -kms_entry_no, -stem_form, -example_entry_no)) |> 
  left_join(ex_remark_translation |> 
              select(-kms_Alphabet, -kms_page, -kms_entry_no, -stem_form, -example_entry_no)) |> 
  left_join(ex_crossref_translation |> 
              select(-kms_Alphabet, -kms_page, -kms_entry_no, -stem_form, -example_entry_no)) |> 
  select(!matches("^create|update|used_time|complete"))

### D2. select relevant columns
example_all |> select(1:3, matches("^kms_|^stem_form|entry_no|^ex_|etym|loan|source|homon|^example_variant"))

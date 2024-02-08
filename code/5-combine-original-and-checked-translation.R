# NOTE: This code file aims to record the steps in combining the original data (before checking) with the data which English translation has been checked.

source("code/4-pre-processing-the-checked-translation.R")
source('code/1-pre-processing.R')

# Custom fixing
stems_translation_checked1 <- stems_translation_checked1 |> 
  mutate(English_all = str_replace_all(English_all,
                                       "(§+)(\\d)",
                                       "\\1 \\2")) |> 
  mutate(German_all = if_else(stem_id == "11_1684827868" & stem_form == "eakaruba" & category == "stem_crossref",
                              str_replace(German_all, "^\\§ 10e\\: \\-r\\-", "§ 10e: -r- < n(i)- (UAN) ; warum nicht < Rumaq? (PAN)"),
                              German_all),
         English_all = if_else(stem_id == "11_1684827868" & stem_form == "eakaruba" & category == "stem_crossref",
                              str_replace(English_all, "^\\§ 10e\\: \\-r\\-", "§ 10e: -r- < n(i)- (UAN) ; why not < Rumaq? (PAN)"),
                              English_all))

# A. ROOT/STEM =============
## 1. ROOT data and its translations that have been checked =======
stems_translation_root <- stems_translation_checked1 |> 
  filter(category == "stem_GermanTranslation") |> 
  select(-category) |> 
  rename(stem_DE = German_all) |> 
  rename(stem_EN = English_all)
### CHECK if the stem_id of the original database is the same with (i.e. present in) the checked translation ======
all(sort(pull(filter(stems4, !is.na(stem_GermanTranslation)), stem_id)) ==
    sort(stems_translation_root$stem_id))
# [1] TRUE

## 2. ROOT VARIANT data and its translations that have been checked =======
stems_translation_variant <- stems_translation_checked1 |> 
  filter(category == "stem_GermanTranslationVariant") |> 
  # remove the stem_GermanTranslationVariant for "pudu" (ID 19_1685198085)
  # because that translation should be for dialectal usage remark, of rarely used dialectal variant
  # and also for stem ID 12_1684853273 ("d Eingeschlossene" 'the trapped'), which has been combined into the stem_GermanTranslation
  filter(!Indonesian %in% c("yang terjebak", "jarang")) |> 
  select(-category) |> 
  rename(stem_variant_DE = German_all) |> 
  rename(stem_variant_EN = English_all)
### CHECK if the stem_id of the original database is the same with (i.e. present in) the checked translation ======
all(sort(pull(filter(stems4, !is.na(stem_GermanTranslationVariant)), stem_id)) ==
      sort(stems_translation_variant$stem_id))
# [1] TRUE


## 3. ROOT REMARK data and its translations that have been checked =======
stems_translation_remark <- stems_translation_checked1 |> 
  filter(category == "stem_remark") |> 
  select(-category) |> 
  rename(stem_remark_DE = German_all) |> 
  rename(stem_remark_EN = English_all) |> 
  ### REMARK custom editing and fixing ======
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
### CHECK if the stem_id of the original database is the same with (i.e. present in) the checked translation ======
all(sort(pull(filter(stems4, !is.na(stem_remark)), stem_id)) ==
      sort(stems_translation_remark$stem_id))
# [1] FALSE
# Warning message:
#   In sort(pull(filter(stems4, !is.na(stem_remark)), stem_id)) == sort(stems_translation_remark$stem_id) :
#   longer object length is not a multiple of shorter object length
### CHECK which id is not in the translation database ============
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
### CHECK AGAIN if the stem_id of the original database is the same with (i.e. present in) the checked translation =========
all(sort(pull(filter(stems4, !is.na(stem_remark)), stem_id)) ==
      sort(stems_translation_remark$stem_id))
# [1] TRUE


## 4. ROOT CROSSREF data and its translations that have been checked =======
stems_translation_crossref <- stems_translation_checked1 |> 
  filter(category == "stem_crossref") |> 
  select(-category) |> 
  rename(stem_crossref_DE = German_all) |> 
  rename(stem_crossref_EN = English_all) |> 
  ### CROSSREF custom editing and fixing ======
  mutate(stem_crossref_EN = str_replace_all(stem_crossref_EN,
                                            "(?<=\\d)Z(?=\\d)",
                                            "z")) |> 
  mutate(stem_crossref_EN = str_replace_all(stem_crossref_EN,
                                            "(?<=\\d)o",
                                            "0"))
### CHECK if the stem_id of the original database is the same with the checked translation ======
all(sort(pull(filter(stems4, !is.na(stem_crossref)), stem_id)) ==
      sort(stems_translation_crossref$stem_id))
# [1] FALSE
# Warning message:
#   In sort(pull(filter(stems4, !is.na(stem_crossref)), stem_id)) ==  :
#   longer object length is not a multiple of shorter object length
### CHECK which id is not in the translation database ===========
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
### CHECK AGAIN if the stem_id of the original database is the same with the checked translation ============
all(sort(pull(filter(stems4, !is.na(stem_crossref)), stem_id)) ==
      sort(stems_translation_crossref$stem_id))
# [1] TRUE

# B. EXAMPLES, DERIVED FORMS ================
ex_to_check <- read_rds("data-raw/ex_to_check.rds") # ex_to_check.rds is generated once from the script in `3-read-the-translation-sheet-EXAMPLE.R`
ex_to_check_missing <- ex_to_check |> 
  filter(is.na(BATCH_CHECK)) |> # retrieve only data that I did not include in the checked items
  select(1:6, example_entry_no, example_form, category, batch = BATCH_CHECK, German_all = German, English_all = English) |> 
  filter(!is.na(category)) # exclude the error for the 'either ... or' entry (i.e. "epanãũ ubaha:u")

#### Add English translation for some common German abbreviation ======
ex_to_check_missing1 <- ex_to_check_missing |> 
  mutate(English_all = if_else(str_detect(German_all, "\\bdgl\\b") & is.na(English_all),
                               str_replace(German_all, "\\bdgl\\b", "and the like"),
                               English_all),
         English_all = if_else(str_detect(German_all, "\\bvgl\\b") & is.na(English_all),
                               str_replace(German_all, "\\bvgl\\b", "cf. "),
                               English_all)) |> 
  mutate(English_all = if_else(is.na(English_all) & str_detect(German_all, "^Pl von"),
                               str_replace(German_all, "\\bPl von", "Plural of"),
                               English_all),
         English_all = if_else(is.na(English_all) & str_detect(German_all, "^s "),
                               str_replace(German_all, "^s ", "see "),
                               English_all),
         German_all = str_replace(German_all, "(?<=H88\\:28)of über", "0")) |> 
  mutate(English_all = if_else(str_detect(German_all, "^intr$") & is.na(English_all),
                               "intransitive",
                               English_all),
         English_all = if_else(str_detect(German_all, "^tr$") & is.na(English_all),
                               "transitive",
                               English_all),
         English_all = if_else(German_all == "ohne -ha-?",
                               "without -ha-?",
                               English_all),
         English_all = if_else(German_all == "DIA" & is.na(English_all),
                               "Dialectal variation",
                               English_all)) |> 
  mutate(English_all = if_else(str_detect(German_all, "feest bij sterfgevallen"),
                               str_replace(German_all, "feest bij sterfgevallen", "celebration of/in case of death"),
                               English_all),
         English_all = if_else(str_detect(German_all, "Wurfnetzǃ") & is.na(English_all),
                               str_replace(German_all, "Wurfnetzǃ", "cast/throw the net!"),
                               English_all),
         English_all = if_else(str_detect(German_all, "^aber\\b\\s"),
                               str_replace(English_all, "^aber ", "but "),
                               English_all)) |> 
  
  ##### put spaces around commas
  mutate(German_all = if_else(str_detect(German_all, "[^, ]\\,[^, ]"),
                              str_replace_all(German_all, "([^, ])(\\,)([^, ])", "\\1 \\2 \\3"),
                              German_all)) |> 
  
  ##### transfer over the remaining German crossrefs and remarks (that have no German words to translate) into the English_all
  mutate(English_all = if_else(is.na(English_all),
                               German_all,
                               English_all)) |> 
  
  ##### replacing the misrepresentation of "0" that the transcriber thought as "o".
  mutate(English_all = if_else(str_detect(English_all, "((?<=[0-9])o|o(?=[0-9]))"),
                               str_replace_all(English_all, "((?<=[0-9])o|o(?=[0-9]))", "0"),
                               English_all),
         German_all = if_else(str_detect(German_all, "((?<=[0-9])o|o(?=[0-9]))"),
                               str_replace_all(German_all, "((?<=[0-9])o|o(?=[0-9]))", "0"),
                               German_all))

ex_to_check_missing1 |> 
  filter(is.na(English_all)) |> 
  select(German_all, English_all)
#### IMPORTANT: `ex_to_check_missing1` needs to be added to `ex_all_translation_checked2` into `ex_all_translation_checked3` 
#### (esp. for remark and crossref categories) because
#### there are cross-references and remarks that are not checked 
#### (because they are not having German translation content)
ex_all_translation_checked3 <- ex_to_check_missing1 |> 
  bind_rows(ex_all_translation_checked2) |> 
  distinct() |> 
  ##### put spaces around semicolons
  mutate(across(matches("^(German_all|English_all)$"), ~ str_replace_all(.x, "([^\\s])([;,])", "\\1 \\2"))) |> 
  ##### replace the left out "vgl"
  mutate(English_all = str_replace_all(English_all, "\\bvgl\\b", "cf.")) |> 
  ##### don't put space after colon in H88 and MOD
  mutate(English_all = str_replace_all(English_all, regex("\\b(H88\\:|MOD\\:)\\s", ignore_case = FALSE), "\\1")) |> 
  ##### add the missing German in the crossref for example_form "eadeda"
  mutate(English_all = if_else(example_form == "eadeda" & category == "example_crossref",
                               str_replace(English_all, "(?<=112z18)(\\:)", "\\1 'happiness'"),
                               English_all),
         German_all = if_else(example_form == "eadeda" & category == "example_crossref",
                               str_replace(German_all, "(?<=112z18)(\\:)", "\\1 \"Glück\""),
                               German_all))

ex_all_translation_checked3

## 1. EXAMPLES data and their translations that have been checked =======
ex_form_translation <- ex_all_translation_checked3 |> 
  filter(category == "example_GermanTranslation") |> 
  select(-category, -batch) |> 
  rename(ex_DE = German_all, 
         ex_EN = English_all)
### CHECK if the example_id of the original database is the same with (i.e. present in) the checked translation ======
ex_id_checked <- sort(ex_form_translation$example_id)
ex_id_orig <- examples3 |> 
  filter(!is.na(example_GermanTranslation)) |> 
  pull(example_id) |> 
  sort()
all(ex_id_orig == ex_id_checked)
# [1] FALSE
# Warning message:
#   In ex_id_orig == ex_id_checked :
#   longer object length is not a multiple of shorter object length
### CHECK if the example_form of the original database is the same with (i.e. present in) the checked translation ======
ex_form_checked <- sort(ex_form_translation$example_form)
ex_form_orig <- examples3 |> 
  filter(!is.na(example_GermanTranslation)) |> 
  pull(example_form) |> 
  sort()
all(ex_form_orig == ex_form_checked) # (FIXED)
# [1] FALSE
# Warning message:
#   In ex_form_orig == ex_form_checked :
#   longer object length is not a multiple of shorter object length
### CHECK which example IDs in the original is not in the translation database ============
missing_id_ex <- setdiff(ex_id_orig, ex_id_checked)
missing_id_ex
# character(0)

### retrieve the example_form whose ID is missing in the checked translation database and check if these forms available in the checked translation database
example_form_whose_id_not_in_the_checked_translation <- filter(examples3, example_id %in% missing_id_ex)$example_form
example_form_whose_id_not_in_the_checked_translation # (FIXED; they are entered into as duplicates)
# character(0)

### CHECK which example_form in the original is not in the translation database ==========
missing_form_ex <- setdiff(ex_form_orig, ex_form_checked)
missing_form_ex
# [1] "ũmãhã́ũ .... ũmãhã́ũ" (FIXED!)
# VERY GOOD NEWS! Only one form and that is expected given this 'either ... or' form is added later. (FIXED)
### CHECK which form from the translation database is not in the original ===============
setdiff(ex_form_checked, ex_form_orig)
# character(0) VERY GOOD NEWS! All example form which translation has been checked is available in the original, except the form for 'either ... or' above (which is also FIXED already).
#### (NO NEED) get the example word-forms which are not in the checked translation (to be rechecked in the check translation)
#### the rationale is that the missing form could be the results of editing the original example form (e.g., misplacement, or later addition, etc.)
# missing_id_ex_form <- examples3 |> 
#   filter(example_id %in% missing_id_ex) |> 
#   pull(example_form)
#### re-check the missing_id_ex_form in the checked translation
# ex_all_translation_checked1 |> filter(example_form %in% missing_id_ex_form)

## 2. EXAMPLE VARIANT data and its translations that have been checked =======
ex_variant_translation <- ex_all_translation_checked3 |> 
  filter(category == "example_GermanTranslationVariant") |> 
  filter(!German_all %in% c("sind gut angeordnet", "Anspitzensmittel", "Verschwindensort", "das Getauschte der Leute sind Waren", "Zuckerrohrblatt")) |> 
  select(-category, -batch) |> 
  rename(ex_variant_DE = German_all) |> 
  rename(ex_variant_EN = English_all)
### CHECK if the example_form of the original database is the same with (i.e. present in) the checked translation ======
ex_var_form_orig <- examples3 |> 
  filter(!is.na(example_GermanTranslationVariant)) |> 
  pull(example_form)
ex_var_form_checked <- ex_variant_translation |> 
  pull(example_form)
setdiff(ex_var_form_orig, ex_var_form_checked)
# [1] "epanãũ ubaha:u" <- (FIXED) this one has been moved to example_GermanTranslation because the German refers to the translation of this form, but the transcriber put the variant form in the example_GermanTranslationVariant
# character(0) <- after being FIXED

## 3. EXAMPLE REMARK data and its translations that have been checked =======
ex_remark_translation <- ex_all_translation_checked3 |> 
  filter(category == "example_remark") |> 
  select(-category, -batch) |> 
  rename(ex_remark_DE = German_all) |> 
  rename(ex_remark_EN = English_all)
### CHECK if the example_form of the original database is the same with (i.e. present in) the checked translation ======
ex_remark_form_orig <- examples3 |> 
  filter(!is.na(example_remark)) |> 
  pull(example_form)
ex_remark_form_checked <- ex_all_translation_checked3 |> 
  filter(category == "example_remark") |> 
  pull(example_form)
setdiff(ex_remark_form_orig, ex_remark_form_checked)
# [1] "enãẽ uʔapo"

### below are codes to check that the remark for "enãẽ uʔapo" are absent from the checked translation
# ex_all_translation_checked |> filter(if_any(where(is.character), ~str_detect(., "nai afo")))
# ex_all_translation_checked1 |> filter(if_any(where(is.character), ~str_detect(., "nai afo")))
# ex_all_translation_checked2 |> filter(if_any(where(is.character), ~str_detect(., "nai afo")))
# ex_all_translation_checked3 |> filter(if_any(where(is.character), ~str_detect(., "nai afo")))

### below is code to show that the remark for "enãẽ uʔapo" is only present in `examples3`, which I missed to include in the checked translation file.
examples |> filter(if_any(where(is.character), ~str_detect(., "nai afo")))

## add the missing data for remark of "enãẽ uʔapo"
misdat <- ex_all_translation_checked3 |> 
  filter(example_form %in% setdiff(ex_remark_form_orig, ex_remark_form_checked)) |> 
  mutate(category = "example_remark",
         German_all = "Francis \"nai afo\"",
         English_all = "Francis \"nai afo\"")
ex_all_translation_checked3 <- bind_rows(ex_all_translation_checked3, misdat)
ex_remark_translation <- bind_rows(ex_remark_translation, 
                                   misdat |> 
                                     select(-category, -batch) |> 
                                     rename(ex_remark_DE = German_all,
                                            ex_remark_EN = English_all))

### CHECK AGAIN the example_form of the original database for REMARK is the same with (i.e. present in) the checked translation ======
ex_remark_form_orig <- examples3 |> 
  filter(!is.na(example_remark)) |> 
  pull(example_form)
ex_remark_form_checked <- ex_all_translation_checked3 |> 
  filter(category == "example_remark") |> 
  pull(example_form)
setdiff(ex_remark_form_orig, ex_remark_form_checked)
# character(0) <- all match now!

## 4. EXAMPLE CROSSREF data and its translations that have been checked =======
ex_crossref_translation <- ex_all_translation_checked3 |> 
  filter(category == "example_crossref") |> 
  select(-category, -batch) |> 
  rename(ex_crossref_DE = German_all) |> 
  rename(ex_crossref_EN = English_all)
### CHECK if the example_form of the original database is the same with (i.e. present in) the checked translation ======
ex_crossref_form_orig <- examples3 |> 
  filter(!is.na(example_crossref)) |> 
  pull(example_form)
ex_crossref_form_checked <- ex_all_translation_checked3 |> 
  filter(category == "example_crossref") |> 
  pull(example_form)
setdiff(ex_crossref_form_orig, ex_crossref_form_checked)
# [1] "kadede"

### below are codes to check that the crossref value for the example_form "kadede" is absent from the checked translation file
# ex_all_translation_checked |> filter(if_any(where(is.character), ~str_detect(., "IVz15")))
# ex_all_translation_checked1 |> filter(if_any(where(is.character), ~str_detect(., "IVz15")))
# ex_all_translation_checked2 |> filter(if_any(where(is.character), ~str_detect(., "IVz15")))
# ex_all_translation_checked3 |> filter(if_any(where(is.character), ~str_detect(., "IVz15"))) <- the value is also the crossref for example_form "kadea"

### below is code to show that the crossref value for "kadede" is only present in `examples3`, which I missed to include in the checked translation file.
examples3 |> filter(if_any(where(is.character), ~ str_detect(., "IVz15")))

## add the missing data for crossref of "kadede"
misdat <- ex_all_translation_checked3 |> 
  filter(example_form %in% setdiff(ex_crossref_form_orig, ex_crossref_form_checked)) |> 
  mutate(category = "example_crossref",
         German_all = "IVz15",
         English_all = "IVz15")
ex_all_translation_checked3 <- bind_rows(ex_all_translation_checked3, misdat)
ex_crossref_translation <- bind_rows(ex_crossref_translation, 
                                   misdat |> 
                                     select(-category, -batch) |> 
                                     rename(ex_crossref_DE = German_all,
                                            ex_crossref_EN = English_all))

### CHECK AGAIN the example_form of the original database for CROSSREF is the same with (i.e. present in) the checked translation ======
ex_crossref_form_orig <- examples3 |> 
  filter(!is.na(example_crossref)) |> 
  pull(example_form)
ex_crossref_form_checked <- ex_all_translation_checked3 |> 
  filter(category == "example_crossref") |> 
  pull(example_form)
setdiff(ex_crossref_form_orig, ex_crossref_form_checked)
# character(0) <- all match now!


# C. EXAMPLES REMAINING COLUMNS ===========
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

# D. STEMS REMAINING COLUMNS ==============
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

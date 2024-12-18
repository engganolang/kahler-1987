# this script can be run after the `6-check-stems...`
# IMPORTANT: script no 5 and 5-1 still need to be run to update contents of `stem_all`

# script to run the translation into Indonesian

## Test script
# stem_all
# 
# x <- stem_all |> 
#   filter(!is.na(stem_remark_EN)) |> 
#   select(stem_remark_EN) |> 
#   mutate(EN_to_deepl = str_extract_all(stem_remark_EN, "\\'[^']+?\\'")) |> 
#   filter(str_detect(stem_remark_EN, "\\'[^']+?\\'")) |> 
#   mutate(EN_to_deepl = map_chr(EN_to_deepl, function(x) paste(x, collapse = "__"))) |> 
#   select(EN_to_deepl)
# x
# 
# x <- x |> mutate(IDN = deeplr::translate2(EN_to_deepl, target_lang = "ID", source_lang = "EN", preserve_formatting = TRUE, auth_key = deeplkey))
# x |> select(IDN)
stem_all |> 
  select(stem_crossref_EN) |> 
  filter(!is.na(stem_crossref_EN), str_detect(stem_crossref_EN, "aux"))

stem_all1 <- stem_all |> mutate(stem_crossref_IDN = stem_crossref_EN,
                                stem_crossref_IDN = str_replace_all(stem_crossref_IDN, "\\bcompare\\b", "bandingkan"),
                                stem_crossref_IDN = str_replace_all(stem_crossref_IDN, "\\b(however|but)\\b", "namun"),
                                stem_crossref_IDN = str_replace_all(stem_crossref_IDN, "\\b[fF]ig\\.", "Gambar "),
                                stem_crossref_IDN = str_replace_all(stem_crossref_IDN, "\\btable\\b", "Tabel "),
                                stem_crossref_IDN = str_replace_all(stem_crossref_IDN, "\\bon p\\.", "di halaman "),
                                stem_crossref_IDN = str_replace_all(stem_crossref_IDN, "^see ", "lihat "),
                                stem_crossref_IDN = str_replace_all(stem_crossref_IDN, "\\band the like\\b", "dan sejenisnya"),
                                stem_crossref_IDN = str_replace_all(stem_crossref_IDN, "^auxiliary count word", "kata bantu bilangan/numeralia"),
                                
                                stem_remark_IDN = stem_remark_EN,
                                stem_remark_IDN = str_replace_all(stem_remark_IDN, "\\barchaic\\b", "bentuk arkais"),
                                stem_remark_IDN = str_replace_all(stem_remark_IDN, "\\b[Aa]lternate expression for\\b", "Ungkapan/bentuk alternatif dari"),
                                stem_remark_IDN = str_replace_all(stem_remark_IDN, "^loanword\\b", "kata pinjaman"),
                                stem_remark_IDN = str_replace_all(stem_remark_IDN, "^outdated\\b", "bentuk lama/tidak digunakan lagi"),
                                stem_remark_IDN = str_replace_all(stem_remark_IDN, "^[Dd]ialect\\b", "variasi dialek"),
                                stem_remark_IDN = str_replace_all(stem_remark_IDN, "Bengkulu dialect", "dialek Bengkulu"),
                                stem_remark_IDN = str_replace_all(stem_remark_IDN, "Pulau Dua dialect", "dialek Pulau Dua"),
                                stem_remark_IDN = str_replace_all(stem_remark_IDN, "^auxiliary count word", "kata bantu bilangan/numeralia"),
                                
                                stem_IDN2 = stem_EN,
                                stem_IDN2 = str_replace_all(stem_IDN2, "\\bauxil+iary (count(ing|er)?( words?\\b)?|measure)", "kata bantu bilangan/numeralia"))

# run this to source the deepl API key
# source("code/0-directory.R")

# create unique ID
stem_all1 <- stem_all1 |> mutate(ID = row_number())

# A. to translate: the STEM =====
## for the Word form =====
df_stem <- stem_all1 |> 
  select(ID, stem_EN, stem_IDN2) |> 
  # exclude NAs
  filter(!is.na(stem_IDN2))
df_stem

# translating the stem form (1,607 rows, started at 18.04 pm stopped at around 18.18, 12 Feb. 2024) using the deeplr R package
# df_stem <- df_stem |> 
#   mutate(stem_IDN2 = deeplr::translate2(text = stem_IDN2, 
#                                         target_lang = "ID",
#                                         source_lang = "EN",
#                                         split_sentences = FALSE,
#                                         preserve_formatting = TRUE,
#                                         auth_key = deeplkey))
# write_rds(df_stem, file="data-raw/stem_form_IDN.rds")
# stem_form_IDN <- read_rds("data-raw/stem_form_IDN.rds")
# stem_form_IDN |> left_join(stem_all1 |> select(ID, stem_EN, stem_DE, stem_form, stem_id, matches("^kms_"))) |>
#   write_csv2("../kahler-idn-translation-to-check/stem_form_IDN-added-cols.csv") <- this is the file checked for the Indonesian translation from deeplr and tracked in https://github.com/engganolang/kahler-idn-translation-checking

## for the Remark =====
df_remark <- stem_all1 |> 
  select(ID, stem_remark_IDN, stem_remark_EN, stem_remark_DE, stem_form, stem_id, matches("^kms_")) |> 
  # exclude NAs
  filter(!is.na(stem_remark_IDN))
df_remark
### translating the remark of the stem form (277 rows; started from 21.21 pm - 21.23 pm) using deeplr R package
# df_remark <- df_remark |>
#   mutate(stem_remark_IDN2 = deeplr::translate2(text = stem_remark_IDN,
#                                         target_lang = "ID",
#                                         source_lang = "EN",
#                                         split_sentences = FALSE,
#                                         preserve_formatting = TRUE,
#                                         auth_key = deeplkey))
# df_remark |> write_csv2("../kahler-idn-translation-to-check/stem_remark_IDN-added-cols.csv") <- this is the file checked for the Indonesian translation from deeplr and tracked in https://github.com/engganolang/kahler-idn-translation-checking

## for the Crossreference =====
df_crossref <- stem_all1 |> 
  select(ID, stem_crossref_IDN, stem_crossref_EN, stem_crossref_DE, stem_form, stem_id, matches("^kms_")) |> 
  # exclude NAs
  filter(!is.na(stem_crossref_IDN))
### translating the crossref of the stem form (552 rows; started from 12.20 pm - 12.24 pm) using deeplr R package
# df_crossref <- df_crossref |> 
#   mutate(stem_crossref_IDN2 = deeplr::translate2(text = stem_crossref_IDN,
#                                                  target_lang = "ID",
#                                                  source_lang = "EN",
#                                                  split_sentences = FALSE,
#                                                  preserve_formatting = TRUE,
#                                                  auth_key = deeplkey))
# df_crossref |> 
#   select(-stem_crossref_IDN) |> 
#   select(ID, stem_crossref_IDN = stem_crossref_IDN2, everything()) |> 
#   write_csv2("../kahler-idn-translation-to-check/stem_crossref_IDN-added-cols.csv") <- this is the file checked for the Indonesian translation from deeplr and tracked in https://github.com/engganolang/kahler-idn-translation-checking

# B. to translate: the EXAMPLE ===========

## for the Example form ====
### 1. create unique ID =====
example_all1 <- example_all |> 
  mutate(ID = row_number()) |> 
  select(ID, everything())

### 2. select the translated items =====
ex_form_to_translate <- example_all1 |> 
  select(ID, ex_EN) |> 
  filter(!is.na(ex_EN))

### 3. translating the Example Form =====
#### (started from 11:59, not finished yet at 12.44 pm, but it did at 12:58) using deeplr R package
# ex_form_IDN <- deeplr::translate2(text = ex_form_to_translate$ex_EN,
#                                   target_lang = "ID",
#                                   source_lang = "EN",
#                                   split_sentences = FALSE,
#                                   preserve_formatting = TRUE,
#                                   auth_key = deeplkey)
# write_rds(ex_form_IDN, file = "data-raw/ex_form_IDN.rds")
ex_form_IDN <- read_rds("data-raw/ex_form_IDN.rds")
# ex_form_to_translate1 <- ex_form_to_translate |> 
#   mutate(ex_form_IDN,
#          ex_form_IDN_edited = "")
# ex_form_to_translate1 |> 
#   left_join(example_all1 |> 
#               select(ID, example_id, stem_id, example_form, ex_DE, ex_EN)) |> 
#   select(ID, example_id, stem_id, example_form, 
#          ex_DE, ex_EN, ex_form_IDN, ex_form_IDN_edited) |>
#   write_csv2("../kahler-idn-translation-to-check/example_form_IDN-added-cols.csv") <- this is the file checked for the Indonesian translation from deeplr and tracked in https://github.com/engganolang/kahler-idn-translation-checking

## for the Example Variant form ====
# ex_variant_IDN <- example_all1 |> 
#   select(ID, ex_variant_EN) |> 
#   filter(!is.na(ex_variant_EN)) |> 
#   mutate(ex_variant_IDN = deeplr::translate2(text = ex_variant_EN,
#                                              target_lang = "ID",
#                                              source_lang = "EN",
#                                              split_sentences = FALSE,
#                                              preserve_formatting = TRUE,
#                                              auth_key = deeplkey))
# ex_variant_IDN |> write_rds("data-raw/ex_variant_IDN.rds")
# ex_variant_IDN <- read_rds("data-raw/ex_variant_IDN.rds")
# ex_variant_IDN |> write_csv2("../kahler-idn-translation-to-check/example_variant_IDN-added-cols.csv")
ex_variant_IDN_checked <- read_csv2("../kahler-idn-translation-to-check/example_variant_IDN-added-cols.csv") |> 
  select(ID, ex_variant_IDN)

## for the Example Remark form ====
# ex_remark_IDN <- example_all1 |> 
#   select(ID, ex_remark_EN) |> 
#   filter(!is.na(ex_remark_EN)) |> 
#   mutate(ex_remark_IDN = deeplr::translate2(text = ex_remark_EN,
#                                              target_lang = "ID",
#                                              source_lang = "EN",
#                                              split_sentences = FALSE,
#                                              preserve_formatting = TRUE,
#                                              auth_key = deeplkey))
# ex_remark_IDN |> write_rds("data-raw/ex_remark_IDN.rds")
# ex_remark_IDN <- read_rds("data-raw/ex_remark_IDN.rds")
# ex_remark_IDN |> write_csv2("../kahler-idn-translation-to-check/example_remark_IDN-added-cols.csv")
ex_remark_IDN_checked <- read_csv2("../kahler-idn-translation-to-check/example_remark_IDN-added-cols.csv") |> 
  mutate(ex_remark_IDN = if_else(!is.na(ex_remark_IDN_edited), 
                                 ex_remark_IDN_edited,
                                 ex_remark_IDN)) |> 
  select(ID, ex_remark_IDN)

## for the Example Crossref. form ====
# ex_crossref_IDN <- example_all1 |> 
#   select(ID, ex_crossref_EN) |> 
#   filter(!is.na(ex_crossref_EN)) |> 
#   mutate(ex_crossref_IDN = deeplr::translate2(text = ex_crossref_EN,
#                                               target_lang = "ID",
#                                               source_lang = "EN",
#                                               split_sentences = FALSE,
#                                               preserve_formatting = TRUE,
#                                               auth_key = deeplkey))
# ex_crossref_IDN |> write_rds("data-raw/ex_crossref_IDN.rds")
# ex_crossref_IDN <- read_rds("data-raw/ex_crossref_IDN.rds")
# ex_crossref_IDN |> write_csv2("../kahler-idn-translation-to-check/example_crossref_IDN-added-cols.csv")
ex_crossref_IDN_checked <- read_csv2("../kahler-idn-translation-to-check/example_crossref_IDN-added-cols.csv") |> 
  mutate(ex_crossref_EN = if_else(!is.na(ex_crossref_EN_edited),
                                  ex_crossref_EN_edited,
                                  ex_crossref_EN),
         ex_crossref_IDN = if_else(!is.na(ex_crossref_IDN_edited),
                                   ex_crossref_IDN_edited,
                                   ex_crossref_IDN)) |> 
  select(ID, ex_crossref_EN2 = ex_crossref_EN,
         ex_crossref_IDN)

# C. Read-in the IDN translation for STEM that has been checked ======
# See the GitHub repo for the checked Indonesian translation at https://github.com/engganolang/kahler-idn-translation-checking
## C-1. for word-form: combine the IDN translation into the main table ======
stem_form_IDN <- read_csv2("../kahler-idn-translation-to-check/stem_form_IDN-added-cols.csv") |> 
  rename(stem_IDN = stem_IDN2) |> 
  select(ID, stem_IDN)
stem_all2 <- stem_all1 |>
  select(-stem_IDN2, -stem_IDN) |>
  left_join(stem_form_IDN)

## C-2. for remark: combine the IDN translation into the main table ======
stem_remark_IDN <- read_csv2("../kahler-idn-translation-to-check/stem_remark_IDN-added-cols.csv") |> 
  select(-stem_remark_IDN) |> 
  rename(stem_remark_IDN = stem_remark_IDN2) |> 
  select(ID, stem_remark_IDN)
stem_all3 <- stem_all2 |>
  select(-stem_remark_IDN) |>
  left_join(stem_remark_IDN)


## C-3. for cross-reference: combine the IDN translation into the main table ======
stem_crossref_IDN <- read_csv2("../kahler-idn-translation-to-check/stem_crossref_IDN-added-cols.csv") |> 
  select(ID, stem_crossref_IDN)
stem_all4 <- stem_all3 |>
  select(-stem_crossref_IDN) |>
  left_join(stem_crossref_IDN)

## C-4 SELECT THE RELEVANT COLUMNS FOR THE FINAL "STEM" DATA TO SAVE AND UPLOAD/SHARE ON GITHUB ====
stem_main_tb <- stem_all4 |> 
  select(1:5, stem_homonymID, stem_DE, stem_EN, stem_IDN, 
         stem_formVarian, stem_variant_DE, stem_variant_EN, stem_variant_IDN,
         stem_dialectVariant,
         stem_etymological_form,
         stem_etym_form_German,
         stem_etymological_language_donor,
         stem_loanword_form,
         stem_loanword_language_donor,
         stem_source_form,
         stem_source_form_homonymID,
         stem_remark_DE,
         stem_remark_EN,
         stem_remark_IDN,
         stem_crossref_DE,
         stem_crossref_EN,
         stem_crossref_IDN) |> 
  # change the glottal stop to superscript
  mutate(across(where(is.character), ~str_replace_all(., "ʔ", "ˀ")))

## C-5 CHECK WHICH COLUMNS STILL CONTAIN VALUES with long-vowel marker ======
stem_main_tb |> 
  select(where(function(x) any(grepl("̄", x)))) |>
  filter(if_any(where(is.character), ~str_detect(., "̄")))
# stem_form stem_DE                                                                                 
# <chr>     <chr>                                                                                   
#   1 eˀahã́ĩ    grūner Papagei                                                                          
# 2 ka-       er, sie, es; wir (in) pronominales Prāfix (3.Sg und 1.Pl in) vor Verben mit b(u)-, m(ũ)-
#   3 ka-       eins (vor Hilfszāhlwörtern)                                                             
# 4 ekixudo   1. Blattrippe, trockene Kokospalmenblātter ; 2. Besen                                   
# 5 kõˀõ kapũ̄ Mitternacht

### C-5-1 Replace long-vowel markers with diarisis in stem_DE =====
stem_main_tb <- stem_main_tb |> 
  mutate(stem_DE = str_replace_all(stem_DE, "̄", "̈"))
### C-5-2 Replace reduplicate vowels with long vowel markers in stem_form ====
stem_main_tb <- stem_main_tb |> 
  mutate(stem_form = str_replace_all(stem_form, "(.̃)̄", "\\1\\1"))


# D. Read-in the IDN translation for EXAMPLE FORMS that has been checked ====
# See the GitHub repo for the checked Indonesian translation at https://github.com/engganolang/kahler-idn-translation-checking
## D-1. for example-form: combine the IDN translation into the main table ====
### on my Dell Windows
ex_form_IDN_edited <- read_tsv("../kahler-idn-translation-to-check/example_form_IDN-added-cols.csv") 

### on my Mac laptop
# ex_form_IDN_edited <- read_tsv("../kahler-idn-translation-to-check/example_form_IDN-added-cols.csv")

ex_form_IDN_edited <- ex_form_IDN_edited |> 
  # merge the edited translation with those that are OK from DeepL
  mutate(ex_IDN = if_else(is.na(ex_form_IDN_edited), ex_form_IDN, ex_form_IDN_edited)) |> 
  # rename(ex_IDN_old = ex_form_IDN) |> 
  select(ID, example_id, stem_id, example_form, ex_DE, ex_EN, ex_IDN)

example_all2 <- example_all1 |> 
  left_join(ex_form_IDN_edited |> 
              select(ID, ex_IDN)) |> 
  select(ID, example_id, stem_id, example_form, ex_DE, ex_EN, ex_IDN, 
         example_variant, ex_variant_DE, ex_variant_EN, ex_remark_DE, ex_remark_EN, example_etymological_form, example_etymological_language_donor,
         example_loanword_form,
         everything()) |> 
  mutate(ex_IDN = if_else(str_detect(ex_IDN, "\\(\\bsg\\b\\)"),
                               str_replace_all(ex_IDN, "\\(\\bsg\\b\\)", "(bentuk tunggal)"),
                               ex_IDN),
         ex_IDN = if_else(str_detect(ex_IDN, "\\(\\bpl\\b\\)"),
                               str_replace_all(ex_IDN, "\\(\\bpl\\b\\)", "(bentuk jamak)"),
                               ex_IDN),
         ex_IDN = if_else(str_detect(ex_IDN, "(\\(\\bsg\\b\\/\\bpl\\b\\)|\\(\\bpl\\b\\/\\bsg\\b\\))"),
                               str_replace_all(ex_IDN, "(\\(\\bsg\\b\\/\\bpl\\b\\)|\\(\\bpl\\b\\/\\bsg\\b\\))", "(bentuk tunggal/jamak)"),
                               ex_IDN),
         ex_IDN = str_replace_all(ex_IDN, "\\bimperatif sg\\b", "imperatif tunggal"),
         ex_IDN = str_replace_all(ex_IDN, "\\s\\bf\\.sg\\.\\)", " feminin tunggal)"),
         ex_IDN = str_replace_all(ex_IDN, "\\(sg\\,\\str\\)", "(bentuk tunggal , transitif)"),
         ex_IDN = str_replace_all(ex_IDN, "\\(tr\\)", "(bentuk transitif)")) |> 
  # strip off all translation beginning with "untuk" that translates English "to-infinitive".
  mutate(ex_IDN = str_replace_all(ex_IDN, "^untuk (?=me)", ""))

## D-2. for example-variant: combine the IDN translation into the main table ====
example_all3 <- example_all2 |> 
  left_join(ex_variant_IDN_checked)

## D-3. for example-remark: combine the IDN translation into the main table ====
example_all4 <- example_all3 |> 
  left_join(ex_remark_IDN_checked)

## D-4. for example-crossreference: combine the IDN translation into the main table ====
example_all5 <- example_all4 |> 
  left_join(ex_crossref_IDN_checked) |> 
  mutate(ex_crossref_EN = if_else(ex_crossref_EN != ex_crossref_EN2,
                                  ex_crossref_EN2,
                                  ex_crossref_EN)) |> 
  select(-ex_crossref_EN2)

## D-5 SELECT THE RELEVANT COLUMNS FOR THE FINAL "EXAMPLES" DATA TO SAVE AND UPLOAD/SHARE ON GITHUB ====
ex_main_tb <- example_all5 |> 
  select(stem_id,
         example_id,
         example_form,
         ex_DE,
         ex_EN,
         ex_IDN,
         example_variant,
         ex_variant_DE,
         ex_variant_EN,
         ex_variant_IDN,
         example_dialect_variant,
         example_etymological_form,
         example_etymological_language_donor,
         example_loanword_form,
         example_loanword_language_donor,
         example_source_form,
         example_source_form_homonymID,
         # example_remark,
         ex_remark_DE,
         ex_remark_EN,
         ex_remark_IDN,
         # example_crossref,
         ex_crossref_DE,
         ex_crossref_EN,
         ex_crossref_IDN,
         matches("concept")) |> 
  # change the glottal stop to superscript
  mutate(across(where(is.character), ~str_replace_all(., "ʔ", "ˀ")))

## D-6 CHECK WHICH COLUMNS STILL CONTAIN VALUES with long-vowel marker ======
ex_main_tb |> 
  select(where(function(x) any(grepl("̄", x)))) |>
  filter(if_any(where(is.character), ~str_detect(., "̄")))

### D-6-1 Replace long-vowel markers with diarisis in ex_DE =====
ex_main_tb <- ex_main_tb |> 
  mutate(ex_DE = str_replace_all(ex_DE, "̄", "̈"))
### C-5-2 Replace reduplicate vowels with long vowel markers in example_form, example_variant, and example_source_form ====
ex_main_tb <- ex_main_tb |> 
  mutate(across(matches("example_(form|variant|source_form)"), 
                ~str_replace_all(., "([aiueo]̃)̄", "\\1\\1")))

## C-5-3 Below add script to process orthography before saving ======
source("code/10-orthography.R")

# E. Combine stem_main_tb with ex_main_tb by the "stem_id" column =====
kahler_dict <- stem_main_tb |> 
  left_join(ex_main_tb)
kahler_dict

### E.1 add the name of the etymological language donor =====
#### [IMPORTANT] - need to run code `1-....`
kahler_dict <- kahler_dict |> 
  left_join(lang_df |> 
              mutate(sw_id = as.character(sw_id)) |> 
              rename(stem_etymological_language_donor = sw_id)) |> 
  mutate(stem_etymological_language_donor = if_else(!is.na(sw_name),
                                                    sw_name,
                                                    stem_etymological_language_donor)) |> 
  select(-sw_name)
kahler_dict

### E.2 deal with the loanword data =====
#### [IMPORTANT] - need to run code `5-1-checking-loanword`
loan_df <- stem_loanword_form |> 
  select(stem_id, loan_form) |> 
  left_join(stem_loanword_lang |> 
              select(-lld_id)) |> 
  left_join(lang_df |> 
              rename(stem_loanword_language_donor = sw_id)) |> 
  select(-stem_loanword_language_donor) |> 
  rename(stem_loan_lang = sw_name,
         stem_loan_form = loan_form)

### E.3 combine kahler_dict with loan_df =====
kahler_dict <- kahler_dict |> 
  left_join(loan_df) |> 
  select(-stem_loanword_form,
         -stem_loanword_language_donor) |> 
  distinct()

## SAVE THE COMBINED DATA TO SHARE TO GITHUB ====
#### Check at https://github.com/engganolang/kahler-1987/tree/main/data-main
write_csv(kahler_dict, file = "data-main/kahler_dict.csv") 
write_tsv(kahler_dict, file = "data-main/kahler_dict.tsv")
write_rds(kahler_dict, file = "data-main/kahler_dict.rds")

## SAVE THE EXAMPLE DATA TO SHARE TO GITHUB =====
#### Check at https://github.com/engganolang/kahler-1987/tree/main/data-main
write_csv(ex_main_tb, file = "data-main/examples_main_tb.csv") 
write_tsv(ex_main_tb, file = "data-main/examples_main_tb.tsv")
write_rds(ex_main_tb, file = "data-main/examples_main_tb.rds")

## SAVE THE STEM DATA TO SHARE TO GITHUB =====
#### Check at https://github.com/engganolang/kahler-1987/tree/main/data-main
write_csv(stem_main_tb, file = "data-main/stem_main_tb.csv") 
write_tsv(stem_main_tb, file = "data-main/stem_main_tb.tsv")
write_rds(stem_main_tb, file = "data-main/stem_main_tb.rds")


## Create a skeleton data frame for variable description ====
# kahler_dicts <- readr::read_rds("data-main/kahler_dict.rds")
# tibble(VAR = colnames(kahler_dicts), DESC = "") |> 
#   write_tsv("data-main/vardesc.tsv")

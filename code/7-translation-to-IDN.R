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

# translating the stem form (1,607 rows, started at 18.04 pm stopped at around 18.18, 12 Feb. 2024)
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
#   write_csv2("../kahler-idn-translation-to-check/stem_form_IDN-added-cols.csv")

## for the Remark =====
df_remark <- stem_all1 |> 
  select(ID, stem_remark_IDN, stem_remark_EN, stem_remark_DE, stem_form, stem_id, matches("^kms_")) |> 
  # exclude NAs
  filter(!is.na(stem_remark_IDN))
df_remark
### translating the remark of the stem form (277 rows; started from 21.21 pm - 21.23 pm)
# df_remark <- df_remark |>
#   mutate(stem_remark_IDN2 = deeplr::translate2(text = stem_remark_IDN,
#                                         target_lang = "ID",
#                                         source_lang = "EN",
#                                         split_sentences = FALSE,
#                                         preserve_formatting = TRUE,
#                                         auth_key = deeplkey))
# df_remark |> write_csv2("../kahler-idn-translation-to-check/stem_remark_IDN-added-cols.csv")

## for the Crossreference =====
df_crossref <- stem_all1 |> 
  select(ID, stem_crossref_IDN, stem_crossref_EN, stem_crossref_DE, stem_form, stem_id, matches("^kms_")) |> 
  # exclude NAs
  filter(!is.na(stem_crossref_IDN))
### translating the crossref of the stem form (552 rows; started from 12.20 pm - 12.24 pm)
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
#   write_csv2("../kahler-idn-translation-to-check/stem_crossref_IDN-added-cols.csv")

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
#### (started from 11:59, not finished yet at 12.44 pm, but it did at 12:58)
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
#   write_csv2("../kahler-idn-translation-to-check/example_form_IDN-added-cols.csv")

## for the Example Variant form ====
ex_variant_to_translate <- example_all1 |> 
  select(ID, ex_variant_EN) |> 
  filter(!is.na(ex_variant_EN))

## for the Example Remark form ====
ex_remark_to_translate <- example_all1 |> 
  select(ID, ex_remark_EN) |> 
  filter(!is.na(ex_remark_EN))

## for the Example Crossref. form ====
ex_crossref_to_translate <- example_all1 |> 
  select(ID, ex_crossref_EN) |> 
  filter(!is.na(ex_crossref_EN))


# C. Read-in the IDN translation for STEM that has been checked ======
## C-1. for word-form: combine the IDN translation into the main table ======
stem_form_IDN <- read_csv2("../kahler-idn-translation-to-check/stem_form_IDN-added-cols.csv") |> 
  rename(stem_IDN = stem_IDN2) |> 
  select(ID, stem_IDN)
stem_all2 <- stem_all1 |>
  select(-stem_IDN2, -stem_IDN) |>
  left_join(stem_form_IDN)

## C-2. for remark: combine the IDN translation into the main table ======


# D. Read-in the IDN translation for EXAMPLE that has been checked ====
## D-1. for example-form: combine the IDN translation into the main table ====
ex_form_IDN_edited <- read_csv2("../kahler-idn-translation-to-check/example_form_IDN-added-cols.csv") |> 
  
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
         ex_IDN = str_replace_all(ex_IDN, "\\(sg\\,\\str\\)", "(bentuk tunggal , transitif)"))

library(tidyverse)
library(readxl)

# read the stem ====
# kstem <- read_xlsx("data-raw/primary/kahler-done-2-replaced.xlsx", range = "A30:X3358") |> 
#   mutate(kms_Alphabet = str_to_lower(kms_Alphabet)) |> 
#   arrange(as.numeric(kms_page), kms_Alphabet, as.numeric(kms_entry_no))

stems <- read_csv2(file = "data-raw/primary/20230719-kahler-done-master.csv",
                   skip = 29, n_max = 3328, quote = "\"", comment = "",
                   col_types = "cciiccccccccccccccccccc",
                   locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ",")) |> 
  mutate(kms_Alphabet = str_to_lower(kms_Alphabet)) |> 
  arrange(as.numeric(kms_page), kms_Alphabet, as.numeric(kms_entry_no))

# convert the incorrect characters into byte in UTF-8
colmatches <- "_form|formVarian|dialect|crossref|remark|Translation"
stems1 <- stems |> 
  mutate(across(matches(colmatches), 
                ~iconv(., from = "UTF-8", to = "UTF-8", sub = "byte")))

# replace the incorrect characters
stems2 <- stems1 |> 
  mutate(across(where(is.character), ~str_replace_all(., "ə\\<cc\\>\\?", "ə́")),
         across(where(is.character), ~str_replace_all(., "ə̃\\<cc\\>\\?", "ə̃́")),
         across(where(is.character), ~str_replace_all(., "\\<cb\\>\\?", ":")),
         across(where(is.character), ~str_replace_all(., "(.̃)\\<cc\\>\\?", "\\1́")),
         across(where(is.character), ~str_replace_all(., "(.)\\<cc\\>\\?", "\\1́")),
         across(where(is.character), ~str_replace_all(., "(ö|ö)", "ö")),
         across(where(is.character), ~str_replace_all(., "(õ|õ)", "õ")),
         across(where(is.character), ~str_replace_all(., "(ó|ó)", "ó")),
         across(where(is.character), ~str_replace_all(., "(á|á)", "á")),
         across(where(is.character), ~str_replace_all(., "(ä|ä)", "ä")),
         across(where(is.character), ~str_replace_all(., "(í|í)", "í")),
         across(where(is.character), ~str_replace_all(., "(é|é)", "é")),
         across(where(is.character), ~str_replace_all(., "(ú|ú)", "ú")),
         across(where(is.character), ~str_replace_all(., "(ü|ü)", "ü")),
         across(where(is.character), ~str_replace_all(., "(ß|ẞ)", "ß")))

## checking the replacement
stems2 |> select(where(is.character)) |> filter(if_any(where(is.character), ~str_detect(., "ə̃́")))

## move the mis-placed German translation
stems2 <- stems2 |> 
  mutate(stem_GermanTranslation = if_else(stem_id == "12_1683687686", stem_GermanTranslationVariant, stem_GermanTranslation),
         stem_GermanTranslationVariant = if_else(stem_id == "12_1683687686", NA, stem_GermanTranslationVariant),
         stem_formVarian = if_else(stem_id == "12_1683687686", paste(stem_formVarian, " (O!)", sep = ""), stem_formVarian),
         
         stem_GermanTranslation = if_else(stem_id == "12_1683692043", stem_GermanTranslationVariant, stem_GermanTranslation),
         stem_GermanTranslationVariant = if_else(stem_id == "12_1683692043", NA, stem_GermanTranslationVariant),
         
         stem_GermanTranslation = if_else(stem_id %in% c("12_1683690710", "12_1683690823"), stem_GermanTranslationVariant, stem_GermanTranslation),
         stem_GermanTranslationVariant = if_else(stem_id  %in% c("12_1683690710", "12_1683690823"), NA, stem_GermanTranslationVariant),
         )

## count the number of characters to get the trimmed entry ======
char_count <- stems2 |> 
  mutate(across(matches("German|crossref|remark"), nchar, .names = "nchar_{.col}"))
char_count |> 
  select(1:5, matches("nchar_")) |> 
  arrange(desc(nchar_stem_crossref))
char_count |> 
  select(1:5, matches("nchar_")) |> 
  arrange(desc(nchar_stem_remark))
char_count |> 
  select(1:5, matches("nchar_")) |> 
  arrange(desc(nchar_stem_GermanTranslation))

# in the stems, there are no trimmed entries for the stem_GermanTranslation

# in the stems, the trimmed entries for "REMARK" are:
# 'kms_page==117 & kms_entry_no==10' and 'kms_page==180 & kms_entry_no==4' (max characters are 249 that were trimmed)
kms_117_entry_10_part <- " geworfen, und nach etwa einer Stunde konnte man die betäubten Fische herausnehmen."
kms_180_entry_04_part <- "(eakõmãʔã:õ) darstellen sollte, geschmück. Er war Symbol der Schnelligkeit. H88:307 ... zijn de sampans versierd ... aan den achtersteven met de èkoekjou, een houten vogel met oogen var paarlemoer ..."

# in the stems, the trimmed entries for "CROSSREF" are:
# 'kms_page==144 & kms_entry_no==16', 'kms_page==164, kms_entry_no==6', 'kms_page==108, kms_entry_no==5' (characters from 248-252)
kms_144_entry_16_part <- "lla sotto gli scogli e nelle acque basse."
kms_164_entry_6_part <- "36 auf S.21o) con gli occhi di madreperla, che somigliano indubbiamente alle ardee, ai picchioni ed ai pappagalli, tutti animali volatori. ...Euciá eloha (= ekuʔiʔiau udahao) sono detti gli uccelli e le teste umane che stanno sulla prua."
kms_108_entry_5_part <- "ewöhnlich jagte man sie jedoch mit Speeren. Vgl MOD fig.26 auf S.171"

### integrate the missing parts of the REMARK and CROSSREFERENCE =====
stems3 <- stems2 |> 
  mutate(stem_remark = if_else(kms_page == 117 & kms_entry_no == 10,
                               paste(stem_remark, kms_117_entry_10_part, sep = ""),
                               stem_remark),
         stem_remark = if_else(kms_page == 180 & kms_entry_no == 4,
                               str_replace_all(stem_remark, "(?<=der einen fliegenden Reiher )\\(.+$", ""),
                               stem_remark),
         stem_remark = if_else(kms_page == 180 & kms_entry_no == 4,
                               paste(stem_remark, kms_180_entry_04_part, sep = ""),
                               stem_remark),
         stem_crossref = if_else(kms_page==144 & kms_entry_no==16,
                                 paste(stem_crossref, kms_144_entry_16_part, sep = ""),
                                 stem_crossref),
         stem_crossref = if_else(kms_page==164 & kms_entry_no==6,
                                 paste(stem_crossref, kms_164_entry_6_part, sep = ""),
                                 stem_crossref),
         stem_crossref = if_else(kms_page==108 & kms_entry_no==5,
                                 paste(stem_crossref, kms_108_entry_5_part, sep = ""),
                                 stem_crossref)
         )



# read the example ====
# kex <- read_xlsx("data-raw/primary/kahler-done-2-replaced.xlsx", range = "A3359:T9070")

examples <- read_csv2(file = "data-raw/primary/20230719-kahler-done-master.csv",
                      col_types = "cccccccccccccccccccc",
                      skip = 3358, n_max = 5711, quote = "\"", comment = "",
                      locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ",")) |> 
  arrange(stem_id)
examples

# convert the incorrect characters into byte in UTF-8
colmatches <- "_form|variant|dialect|crossref|remark|Translation|etymological|loanword"
examples1 <- examples |> 
  mutate(across(matches(colmatches), 
                ~iconv(., from = "UTF-8", to = "UTF-8", sub = "byte")))

# replace the incorrect characters
examples2 <- examples1 |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\<cb\\>\\?", ":")),
         across(where(is.character), ~str_replace_all(., "(.̃)\\<cc\\>\\?", "\\1́")),
         across(where(is.character), ~str_replace_all(., "(.)\\<cc\\>\\?", "\\1́")),
         across(where(is.character), ~str_replace_all(., "(ö|ö)", "ö")),
         across(where(is.character), ~str_replace_all(., "(õ|õ)", "õ")),
         across(where(is.character), ~str_replace_all(., "(ó|ó)", "ó")),
         across(where(is.character), ~str_replace_all(., "(á|á)", "á")),
         across(where(is.character), ~str_replace_all(., "(ä|ä)", "ä")),
         across(where(is.character), ~str_replace_all(., "(í|í)", "í")),
         across(where(is.character), ~str_replace_all(., "(é|é)", "é")),
         across(where(is.character), ~str_replace_all(., "(ú|ú)", "ú")),
         across(where(is.character), ~str_replace_all(., "(ü|ü)", "ü")),
         across(where(is.character), ~str_replace_all(., "(ß|ẞ)", "ß")))

# testing the replacement
examples2 |> select(where(is.character)) |> filter(if_any(where(is.character), ~str_detect(., "ə̃́")))

## count the number of characters to get the trimmed entry ======
char_count <- examples2 |> 
  mutate(across(matches("German|crossref|remark"), nchar, .names = "nchar_{.col}"))

char_count |> 
  select(1:4, matches("nchar_.*cross")) |> 
  arrange(desc(nchar_example_crossref))
# in the examples, the trimmed entries for "crossref" are:
## 'stem_id=='8_1684998925', example_id=='8_1684998925_8'' -- part 1
crossref_ex_part_1 <- "ij hoofdpijn [kafoehoe èoeloe] wordt het hoofd van den lijder eerst kunstmatig behandeld [pihi] en daarna stijf met een doek aangebonden"

## 'stem_id=='11_1685071495', example_id=='11_1685071495_0'' -- part 2
crossref_ex_part_2 <- "tyleerd mensenfiguurtje (MOD: Hund!) in hurkende of kruipende houding is uitgebeeld. Het werd tijdens het landbouwritueel bij de vrouwendans op het hoofd gedragen, versierd met opstaande veren. Zulke kapjes kwamen in Indonesie alleen op Enggano voor."

## 'stem_id=='12_1684292048', example_id=='12_1684292048_0'' -- part 3
crossref_ex_part_3 <- "D fig.XVII auf S.197"

## 'stem_id == "15_1685324586", example_id == "15_1685324586_3"' -- part 4
crossref_ex_part_4 <- "e ungefähr alle zehn Jahre einmal dröhnen, als ob sie abgefeuert würde. Heirdurch wird dann das Folgen einer Zeit der Krankheits- und Sterbefälle eingeleitet."

char_count |> 
  select(1:4, matches("nchar_.*German")) |> 
  arrange(desc(nchar_example_GermanTranslation))
# in the examples, the trimmed entries for "German translations" are:
## (stem_id=="17_1685520127", example_id=="17_1685520127_0"); - part 1
example_german_trans_part_1 <- " auf Enggano anzutreffen"
## (stem_id=="10_1685081633", example_id=="10_1685081633_0"); - part 2
example_german_trans_part_2 <- "d bleibe!\""
## (stem_id=="11_1685071495", example_id=="11_1685071495_0"); - part 3
example_german_trans_part_3 <- "teckt" # need to check the database for this entry, which part is put in the cross-ref and/or remark compared to the German translation

examples3 <- examples2 |> 
  mutate(example_crossref = if_else(stem_id == "8_1684998925" & example_id == "8_1684998925_8",
                                    paste(example_crossref, crossref_ex_part_1, sep = ""),
                                    example_crossref),
         example_crossref = if_else(stem_id == "11_1685071495" & example_id == "11_1685071495_0",
                                    paste(example_crossref, crossref_ex_part_2, sep = ""),
                                    example_crossref),
         example_crossref = if_else(stem_id == "12_1684292048" & example_id == "12_1684292048_0",
                                    paste(example_crossref, crossref_ex_part_2, sep = ""),
                                    example_crossref),
         example_crossref = if_else(stem_id == "15_1685324586" & example_id == "15_1685324586_3",
                                    paste(example_crossref, crossref_ex_part_2, sep = ""),
                                    example_crossref),
         example_GermanTranslation = if_else(stem_id == "17_1685520127" & example_id == "17_1685520127_0",
                                             paste(example_GermanTranslation, example_german_trans_part_1, sep = ""),
                                             example_GermanTranslation),
         example_GermanTranslation = if_else(stem_id == "10_1685081633" & example_id == "10_1685081633_0",
                                             paste(example_GermanTranslation, example_german_trans_part_1, sep = ""),
                                             example_GermanTranslation),
         example_GermanTranslation = if_else(stem_id == "11_1685071495" & example_id == "11_1685071495_0",
                                             paste(example_GermanTranslation, example_german_trans_part_1, sep = ""),
                                             example_GermanTranslation))

char_count |> 
  select(1:4, matches("nchar_.*remark|remark")) |> 
  arrange(desc(nchar_example_remark)) %>%
  .[1:3, ] |> 
  pull(example_remark)

# to translate ====
## German translation of the stem table =====
tr1 <- stems3 |> 
  select(stem_id, stem_GermanTranslation) |> 
  filter(!is.na(stem_GermanTranslation)) |> 
  rename(German = stem_GermanTranslation) |> 
  mutate(category = "stem_GermanTranslation")
tr2 <- stems3 |> 
  select(stem_id, stem_GermanTranslationVariant) |> 
  filter(!is.na(stem_GermanTranslationVariant)) |> 
  rename(German = stem_GermanTranslationVariant) |> 
  mutate(category = "stem_GermanTranslationVariant")
tr3 <- stems3 |> 
  select(stem_id, stem_crossref) |> 
  filter(!is.na(stem_crossref)) |> 
  rename(German = stem_crossref) |> 
  mutate(category = "stem_crossref")
tr4 <- stems3 |> 
  select(stem_id, stem_remark) |> 
  filter(!is.na(stem_remark)) |> 
  rename(German = stem_remark) |> 
  mutate(category = "stem_remark")
tr_stem <- bind_rows(tr1, tr2, tr3, tr4)
tr_stem |> 
  writexl::write_xlsx("to-translate/1_stem_german_translation.xlsx")

## German translation of the example table =====
tr1 <- examples2 |> 
  select(example_id, stem_id, example_GermanTranslation) |> 
  filter(!is.na(example_GermanTranslation)) |> 
  rename(German = example_GermanTranslation) |> 
  mutate(category = "example_GermanTranslation")
tr2 <- examples2 |> 
  select(example_id, stem_id, example_GermanTranslationVariant) |> 
  filter(!is.na(example_GermanTranslationVariant)) |> 
  rename(German = example_GermanTranslationVariant) |> 
  mutate(category = "example_GermanTranslationVariant")
tr3 <- examples2 |> 
  select(example_id, stem_id, example_crossref) |> 
  filter(!is.na(example_crossref)) |> 
  rename(German = example_crossref) |> 
  mutate(category = "example_crossref")
tr4 <- examples2 |> 
  select(example_id, stem_id, example_remark) |> 
  filter(!is.na(example_remark)) |> 
  rename(German = example_remark) |> 
  mutate(category = "example_remark")
tr_ex <- bind_rows(tr1, tr2, tr3, tr4) 
tr_ex |> 
  writexl::write_xlsx("to-translate/2_example_german_translation.xlsx")


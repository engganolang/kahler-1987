library(tidyverse)
library(readxl)

# read the stem ====
## IMPORTANT: Re-check the `range` when the transcription is done!
kstem <- read_xlsx("data-raw/kahler1.xlsx", range = "A29:X2117") |> 
  mutate(kms_Alphabet = str_to_lower(kms_Alphabet)) |> 
  arrange(as.numeric(kms_page), kms_Alphabet, as.numeric(kms_entry_no))

lang_abbr <- read_xlsx("data-raw/kahler1.xlsx", range = "A5:B27") |> 
  mutate(sw_name = replace(sw_name, sw_name %in% c("PAN", "UAN (Proto-Austronesian)"), "PAN (Proto-Austronesian)"),
         sw_name = replace(sw_name, sw_name == "MKB", "MKB (Minangkabau [Austronesian, Sumatra])"),
         sw_name = replace(sw_name, sw_name == "SMtw", "SMtw (Sudmentawai [South Mentawai])"))

# read the example ====
## IMPORTANT: Re-check the `range` when the transcription is done!
kex <- read_xlsx("data-raw/kahler1.xlsx", range = "A2119:T5500")


# inspect the encoding
(inspects <- kstem[1795:1820, "stem_form"] |> pull(stem_form))

# get the characters - TESTING
all_chars <- inspects |> 
  str_split("\\W") |> 
  map(function(x) x[nzchar(x)]) |> 
  unlist() |> 
  str_split("") |> 
  unlist() |> 
  sort() |> 
  unique()
all_chars

# get the characters - STEM =====
all_chars_stem <- kstem |> 
  select(!matches("create|date|update|used"), -kms_Alphabet, -kms_page, -kms_entry_no) |> 
  pivot_longer(-stem_id, names_to = "cols_name") |> 
  pull(value) |> 
  str_split("\\W") |> 
  map(function(x) x[nzchar(x)]) |> 
  unlist() |> 
  str_split("") |> 
  unlist() |> 
  sort()
all_chars_stem_tb <- tibble(char = all_chars_stem) |> 
  count(char) |> 
  arrange(char, desc(n))
all_chars_stem_tb$char |> unique()

## replace incorrect character - STEM ====
kstem <- kstem |> 
  mutate(across(where(is.character), ~str_replace_all(.x, "ə̿", "ə́")),
         across(where(is.character), ~str_replace_all(.x, "ə̃̿", "ə̃́")),
         across(where(is.character), ~str_replace_all(.x, "ĩ̿", "ĩ́")),
         across(where(is.character), ~str_replace_all(.x, "i̿", "í")),
         across(where(is.character), ~str_replace_all(.x, "(í|í)", "í")),
         across(where(is.character), ~str_replace_all(.x, "(ã̿̿|ã̿)", "ã́")),
         across(where(is.character), ~str_replace_all(.x, "a̿", "á")),
         across(where(is.character), ~str_replace_all(.x, "(á|á)", "á")),
         across(where(is.character), ~str_replace_all(.x, "(ä|ä)", "ä")),
         across(where(is.character), ~str_replace_all(.x, "(é|é)", "é")),
         across(where(is.character), ~str_replace_all(.x, "ẽ̿", "ẽ́")),
         across(where(is.character), ~str_replace_all(.x, "e̿", "é")),
         across(where(is.character), ~str_replace_all(.x, "õ̿", "ṍ")),
         across(where(is.character), ~str_replace_all(.x, "o̿", "ó")),
         across(where(is.character), ~str_replace_all(.x, "(ö|ö)", "ö")),
         across(where(is.character), ~str_replace_all(.x, "(õ|õ)", "õ")),
         across(where(is.character), ~str_replace_all(.x, "(ó|ó)", "ó")),
         across(where(is.character), ~str_replace_all(.x, "ɔ̿", "ɔ́")),
         across(where(is.character), ~str_replace_all(.x, "ũ̿", "ṹ")),
         across(where(is.character), ~str_replace_all(.x, "u̿", "ú")),
         across(where(is.character), ~str_replace_all(.x, "(ú|ú)", "ú")),
         across(where(is.character), ~str_replace_all(.x, "(ü|ü)", "ü")),
         across(where(is.character), ~str_replace_all(.x, "(ß|ẞ)", "ß")),
         across(where(is.character), ~str_replace_all(.x, "u̿", "ú"))
  )
### re-check the replaced characters
all_chars_stem <- kstem |> 
  select(!matches("create|date|update|used"), -kms_Alphabet, -kms_page, -kms_entry_no) |> 
  pivot_longer(-stem_id, names_to = "cols_name") |> 
  pull(value) |> 
  str_split("\\W") |> 
  map(function(x) x[nzchar(x)]) |> 
  unlist() |> 
  str_split("") |> 
  unlist() |> 
  sort()
all_chars_stem_tb <- tibble(char = all_chars_stem) |> 
  count(char) |> 
  arrange(char, desc(n))
all_chars_stem_tb$char |> unique()

### check entries with a given character
kstem |> 
  filter(if_any(.cols = everything(), ~str_detect(., unique(all_chars_stem_tb$char)[115])))


### test to tokenise the separator of the variant providing whitespace in either side
kstem <- kstem |> 
  mutate(stem_formVarian = str_replace_all(stem_formVarian, "(?<!\\s)([;,])", " \\1"))

### test to count variant of the stems =====
kstem |> 
  mutate(variant_forms = str_count(stem_formVarian, "\\b[^,;]+\\b")) |> 
  select(stem_form, stem_formVarian, variant_forms) |> 
  filter(!is.na(variant_forms)) |> 
  arrange(desc(variant_forms)) |> 
  print(n=20)

### test to measure mean of variant forms of the stem =====
kstem |> 
  mutate(variant_forms = str_count(stem_formVarian, "\\b[^,;]+\\b")) |> 
  select(stem_form, stem_formVarian, variant_forms) |> 
  filter(!is.na(variant_forms)) |> 
  arrange(desc(variant_forms)) |> summarise(mean_var = mean(variant_forms))

### test count the source language marked as etymology =====
kstem |> 
  select(matches("etym.*donor")) |> 
  filter(!is.na(stem_etymological_language_donor)) |> 
  left_join(lang_abbr |> 
              rename(stem_etymological_language_donor = sw_id)) |> 
  count(sw_name, sort = TRUE)


### test finding any appearance of Helfrich's 1916 or Helfrich's 1888 =====
kstem |> 
  filter(if_any(everything(), ~str_detect(., "\\b[Hh]16\\b"))) |> 
  select(stem_crossref)

kstem |> 
  filter(if_any(everything(), ~str_detect(., "\\b[Hh]8"))) |> 
  select(stem_crossref)










# get the characters - EXAMPLES ======
# all_chars_stem <- kex |> 
#   select(!matches("create|date|update|used"), -kms_Alphabet, -kms_page, -kms_entry_no) |> 
#   pivot_longer(-stem_id, names_to = "cols_name") |> 
#   pull(value) |> 
#   str_split("\\W") |> 
#   map(function(x) x[nzchar(x)]) |> 
#   unlist() |> 
#   str_split("") |> 
#   unlist() |> 
#   sort()
# all_chars_stem_tb <- tibble(char = all_chars_stem) |> 
#   count(char) |> 
#   arrange(char, desc(n))
# all_chars_stem_tb$char |> unique()
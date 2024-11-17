# This code file checks the loanword data

library(tidyverse)

### IMPORTANT
source("code/5-combine-original-and-checked-translation.R") # [IMPORTANT!] If this file has been run independently, don't run it here.
source("code/1-pre-processing.R") # [IMPORTANT!] If this file has been run independently, don't run it here.

### The code files no. 2 and 3 are skipped because they are preparation scripts ... (cont.) 
#### ... to retrieve the German-English translation Google Spreadsheet to be further checked by Barnaby 
#### (the checking is DONE and the checked English translation files are processed and brought into R in script 4-)

stem_loanword_form <- read_csv2(file = "data-raw/primary/20230719-kahler-done-master.csv", 
                                skip = 9076, 
                                n_max = 75, 
                                locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ","), 
                                col_names = c("stf_id", "stem_id", "loan_form")) |> 
  mutate(loan_form = replace(loan_form, 
                             loan_form == "genggang;" & 
                               stem_id == "8_1683871912", 
                             "genggang ; ginggung")) |> 
  mutate(loan_form = str_replace(loan_form, "^g(?=i\\(m)", "q"))

stem_loanword_lang <- read_csv2(file = "data-raw/primary/20230719-kahler-done-master.csv", 
                                skip = 9161, 
                                n_max = 76, 
                                locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ","), 
                                col_names = c("lld_id", # stands for loanword language donor ID 
                                              "stem_id", "stem_loanword_language_donor"))

# check stem id appearing in the loanword language data but not in the loanword form
setdiff(stem_loanword_lang$stem_id, stem_loanword_form$stem_id)
# [1] "17_1687350573"
## for this ID, there is no source form given as the loanword, only the donor language.
## the loanword form appears as the stem "Bukixi"
missloan <- setdiff(stem_loanword_lang$stem_id, stem_loanword_form$stem_id)
stems4 |> filter(stem_id %in% missloan)
stem_loanword_lang |> 
  filter(stem_id == missloan)








# check stem id appearing in the loanword form data but not in the loanword language data
setdiff(stem_loanword_form$stem_id, stem_loanword_lang$stem_id)
# [1] "9_1684988647"
missloan <- setdiff(stem_loanword_form$stem_id, stem_loanword_lang$stem_id)
stems4 |> filter(stem_id %in% missloan) |> as.data.frame()
stem_loanword_form |> filter(stem_id %in% missloan)

# add the misplaced loanword in "9_1684988647" into the original `stems4` for the etymological_form column
etym_form <- stem_loanword_form |> 
  filter(stem_id == missloan) |> 
  pull(loan_form) |> 
  str_replace("\\;.+$", "")
remark_form <- stem_loanword_form |> 
  filter(stem_id == missloan) |> 
  pull(loan_form) |> 
  str_replace("^[^;]+;\\s+", "")
stem_loanword_form <- stem_loanword_form |> 
  mutate(loan_form = if_else(stem_id %in% missloan,
                                       etym_form,
                                       loan_form))
stem_loanword_form |> filter(stem_id %in% missloan)
stems4 <- stems4 |> 
  mutate(stem_etymological_form = if_else(stem_id == missloan,
                                          etym_form,
                                          stem_etymological_form),
         stem_remark = if_else(stem_id == missloan,
                               remark_form,
                               stem_remark))
stems4 |> 
  filter(stem_id == missloan) |> 
  select(matches("remark|etym|loan"))

df <- stems_translation_root |> 
  filter(stem_id == missloan) |> 
  select(-stem_IDN) |> 
  mutate(stem_remark_IDN = NA) |> 
  rename(stem_remark_RMK = stem_RMK,
         stem_remark_DE = stem_DE,
         stem_remark_EN = stem_EN) |> 
  mutate(stem_remark_DE = remark_form,
         stem_remark_EN = remark_form)

df

stems_translation_remark <- bind_rows(stems_translation_remark, df)

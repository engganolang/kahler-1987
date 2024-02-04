# This code file checks the loanword data

### IMPORTANT: run first codes in `5-combine-original-and-...`

# check stem id appearing in the loanword language data but not in the loanword form
setdiff(stem_loanword_lang$stem_id, stem_loanword_form$stem_id)
# [1] "17_1687350573"
missloan <- setdiff(stem_loanword_lang$stem_id, stem_loanword_form$stem_id)
stems4 |> filter(stem_id %in% missloan)
stem_loanword_lang |> 
  filter(stem_id == missloan)








# check stem id appearing in the loanword form data but not in the loanword language data
setdiff(stem_loanword_form$stem_id, stem_loanword_lang$stem_id)
# [1] "9_1684988647"
missloan <- setdiff(stem_loanword_form$stem_id, stem_loanword_lang$stem_id)
stems4 |> filter(stem_id %in% missloan)
stem_loanword_form |> filter(stem_id %in% missloan)

# add the misplaced loanword in "9_1684988647" into the original `stems4` for the etymological_form column
etym_form <- stem_loanword_form |> 
  filter(stem_id == missloan) |> 
  pull(stem_loandword_form) |> 
  str_replace("\\;.+$", "")
remark_form <- stem_loanword_form |> 
  filter(stem_id == missloan) |> 
  pull(stem_loandword_form) |> 
  str_replace("^[^;]+;\\s+", "")
stem_loanword_form <- stem_loanword_form |> 
  mutate(stem_loandword_form = if_else(stem_id %in% missloan,
                                       etym_form,
                                       stem_loandword_form))
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
  mutate(Indonesian = NA) |> 
  rename(stem_remark_DE = stem_GermanTranslation_DE,
         stem_remark_EN = stem_English) |> 
  mutate(stem_remark_DE = remark_form,
         stem_remark_EN = remark_form)
stems_translation_remark <- bind_rows(stems_translation_remark,
                                      df)

# To get the count of entries for Kähler (1987) for the EnoLEX AsiaLEX proceedings, run the source code-files mentioned in `8-reconstruction-...`
# Pay attention where to run (on Mac or Windows) so that we need to adjust the code in `7-translation` for reading the edited translation for the example form
# After running these source codes, the relevant data are as follows:
## - stem_all2 contains data for the stem form only
## - example_all2 contains data for the example form only (foreign key here to match with the stem_all2 is stem_id)

# Asumming I have run codes mentioned in `8-reconstruction-...`, here is how to get the count for the EnoLEX proceeding Table 1
(stem_counts <- stem_all2 |> select(stem_form, stem_homonymID) |> distinct() |> nrow())
(example_counts <- example_all2 |> select(example_form, ex_DE) |> distinct() |> nrow())
(kahler_total_entries <- sum(stem_counts, example_counts))

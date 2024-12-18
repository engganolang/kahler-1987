Retro-digitised Enggano-German dictionary derived from Kähler’s (1987)
“Enggano-Deutsches Wörterbuch”
================
[Gede Primahadi Wijaya
Rajeg](https://www.ling-phil.ox.ac.uk/people/gede-rajeg)
<a itemprop="sameAs" content="https://orcid.org/0000-0002-2047-8621" href="https://orcid.org/0000-0002-2047-8621" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>,
Cokorda Pramartha
<a itemprop="sameAs" content="https://orcid.org/0000-0002-2835-3989" href="https://orcid.org/0000-0002-2835-3989" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>,
Ida Bagus Gede Sarasvananda
<a itemprop="sameAs" content="https://orcid.org/0000-0002-2020-5148" href="https://orcid.org/0000-0002-2020-5148" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>,
Putu Wahyu Widiatmika, Ida Bagus Made Ari Segara, Yul Fulgensia Rusman
Pita, Fitriani Putri Koemba, I Gede Semara Dharma Putra
<a itemprop="sameAs" content="https://orcid.org/0009-0000-6711-0878" href="https://orcid.org/0009-0000-6711-0878" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>,
Putu Dea Indah Kartini, Ni Putu Wulan Lestari, Barnaby Burleigh
<a itemprop="sameAs" content="https://orcid.org/0009-0009-2295-9048" href="https://orcid.org/0009-0009-2295-9048" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[<img
src="https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/file-oxweb-logo.gif"
width="84" alt="The University of Oxford" />](https://www.ox.ac.uk/)
[<img
src="https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/file-lingphil.png"
width="83"
alt="Faculty of Linguistics, Philology and Phonetics, the University of Oxford" />](https://www.ling-phil.ox.ac.uk/)
[<img
src="https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/file-ahrc.png"
width="325" alt="Arts and Humanities Research Council (AHRC)" />](https://www.ukri.org/councils/ahrc/)
</br>*This work is part of the [AHRC-funded
project](https://gtr.ukri.org/projects?ref=AH%2FW007290%2F1) on the
lexical resources for Enggano, led by the Faculty of Linguistics,
Philology and Phonetics at the University of Oxford, UK. Visit the
[central webpage of the Enggano
project](https://enggano.ling-phil.ox.ac.uk/)*.

<p xmlns:cc="http://creativecommons.org/ns#">
This work is licensed under
<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC
BY-NC-SA 4.0
<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" alt=""></a>
</p>
<!-- badges: end -->

# Overview

This document provides description of the variables/columns names in the
main tables of the Enggano-German Dictionary ([Kähler
1987](#ref-kähler1987)) in this `data-main` sub-directory[^1].

``` r
library(vtable)
#> Loading required package: kableExtra
```

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.2     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.0
#> ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter()     masks stats::filter()
#> ✖ dplyr::group_rows() masks kableExtra::group_rows()
#> ✖ dplyr::lag()        masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
vardesc <- readr::read_tsv("vardesc.tsv")
#> Rows: 62 Columns: 2
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (2): VAR, DESC
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
kahler_dictionary <- readr::read_rds("kahler_dict.rds")
vtable::vt(kahler_dictionary, labels = vardesc, data.title = "Kähler's (1987) Enggano-German dictionary", desc = "The description of variables/column names in the data table.")
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Kähler’s (1987) Enggano-German dictionary
</caption>
<thead>
<tr>
<th style="text-align:left;">
Name
</th>
<th style="text-align:left;">
Class
</th>
<th style="text-align:left;">
Label
</th>
<th style="text-align:left;">
Values
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ID
</td>
<td style="text-align:left;">
integer
</td>
<td style="text-align:left;">
Row IDs
</td>
<td style="text-align:left;">
Num: 1 to 3325
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_id
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
ID of the stem/headword generated by the database entry system
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
kms_Alphabet
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
The alphabetic entry of the stem/headword
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
kms_page
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Page number of the stem
</td>
<td style="text-align:left;">
Num: 1 to 292
</td>
</tr>
<tr>
<td style="text-align:left;">
kms_entry_no
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Entry number of the stem in a given page; manually marked in the printed
page before entering to the database entry system
</td>
<td style="text-align:left;">
Num: 1 to 94
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_form
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
The form of the stem/headword
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_form_comm_untokenised
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
The stem in common transcription and NOT-YET tokenised/segmented
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_homonymID
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Homonym ID attached to the stem in the original dictionary
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_DE
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
German gloss of the stem
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_EN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
English translation of the stem’s German gloss
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_IDN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Indonesian translation of the English translation
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_formVarian
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Variant forms given for the stem in the dictionary (original
transcription)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_formVarian_untokenised
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Variant forms given for the stem in the dictionary (common
transcription)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_formVarian_tokenised
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Variant forms given for the stem in the dictionary (tokenised/segmented
in common transcription)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_variant_DE
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
German gloss of the variant form
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_variant_EN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
English translation of the variant form’s German gloss
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_variant_IDN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Indonesian translation of the English translation
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_dialectVariant
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Dialectal variant of the stem; forms marked with DIA in the original
dictionary
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_etymological_form
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Reconstructed etymological form of the stem
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_etym_form_German
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
German gloss of the reconstructed etymological form
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_etymological_language_donor
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Language source of the etymological form
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_source_form
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
This is a form marked with ‘\<’ in the dictionary, indicating that the
stem/headword is derived from this source_form
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_source_form_homonymID
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Homonym ID attached to the stem’s/headword’s source form in the original
dictionary
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_remark_DE
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
German gloss of what we classified as remark
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_remark_EN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
English translation of the remark’s German
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_remark_IDN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Indonesian translation of the English translation of the remark
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_crossref_DE
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
German gloss of what we classified as cross-reference
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_crossref_EN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
English translation of the crossreference’s German
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_crossref_IDN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Indonesian translation of the English translation of the crossreference
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_form_comm_tokenised
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
The stem in common transcription and tokenised/segmented
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_id
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
ID of the example (i.e., sub-entries of the stem/headword) generated by
the database entry system
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_form
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
The form of the sub-entries/examples of the stem/headword
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_form_comm_untokenised
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
The sub-entries/examples in common transcription and NOT-YET
tokenised/segmented
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_form_comm_tokenised
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
The sub-entries/examples in common transcription and tokenised/segmented
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_DE
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
German gloss of the examples/sub-entries
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_EN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
English translation of the examples/sub-entries German gloss
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_IDN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Indonesian translation of the English translation
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_variant
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Variant forms given for the examples/sub-entries in the dictionary
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_variant_comm_untokenised
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Variant forms given for the examples/sub-entries in the dictionary
(common transcription unsegmented/non-tokensied)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_variant_comm_tokenised
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Variant forms given for the examples/sub-entries in the dictionary
(common transcription segmented/tokensied)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_variant_DE
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
German gloss of the variant form
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_variant_EN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
English translation of the German gloss of the variant form
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_variant_IDN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Indonesian translation of the English translation
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_dialect_variant
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Dialectal variant of the examples/sub-entries; forms marked with DIA in
the original dictionary
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_etymological_form
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Reconstructed etymological form of the examples/sub-entries
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_etymological_language_donor
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Language source of the etymological form
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_loanword_form
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
If the examples/sub-entries are marked as loanword, they will be stored
here
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_loanword_language_donor
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Information on the language donor of the loanword
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_source_form
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
This is a form marked with “\<” in the dictionary, indicating that the
examples/sub-entries are derived from this source_form
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
example_source_form_homonymID
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Homonym ID attached to the examples/sub-entries source form in the
original dictionary
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_remark_DE
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
German gloss of what we classified as remark for the
examples/sub-entries
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_remark_EN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
English translation of the remark’s German
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_remark_IDN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Indonesian translation of the English translation of the remark
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_crossref_DE
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
German gloss of what we classified as cross-reference for the
examples/sub-entries
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_crossref_EN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
English translation of the crossreference’s German
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_crossref_IDN
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Indonesian translation of the English translation of the crossreference
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_concept
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
qualitative categorisation (like semantic domain, but not consistently
pursued)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_variant_concept
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
qualitative categorisation (like semantic domain, but not consistently
pursued)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_remark_concept
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
qualitative categorisation (like semantic domain, but not consistently
pursued)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ex_crossref_concept
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
qualitative categorisation (like semantic domain, but not consistently
pursued)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_loan_form
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
If the stem/headword is marked as loanword, they will be stored here
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
stem_loan_lang
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Information on the language donor of the loanword
</td>
<td style="text-align:left;">
</td>
</tr>
</tbody>
</table>

The work on the manual checking of the Indonesian translation from the
English translation is available
[here](https://github.com/engganolang/kahler-idn-translation-checking).
Some of the words in the dictionary are included in the Shiny app of the
[*EnoLEX*](https://enggano.shinyapps.io/enolex/) database ([Krauße et
al. 2024](#ref-krausse_enolex_2024); [Rajeg, Krauße & Pramartha
2024](#ref-rajeg_enolex_2024)).

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-kähler1987" class="csl-entry">

Kähler, Hans. 1987. *Enggano-Deutsches wörterbuch* (Veröffentlichungen
Des Seminars Für Indonesische Und Südseesprachen Der Universität Hamburg
14). Berlin; Hamburg: Dietrich Reimer Verlag.

</div>

<div id="ref-krausse_enolex_2024" class="csl-entry">

Krauße, Daniel, Gede Primahadi Wijaya Rajeg, Cokorda Pramartha, Erik
Zobel, Charlotte Hemmings, I Wayan Arka & Mary Dalrymple. 2024. EnoLEX:
A diachronic lexical database of the Enggano language. Online database.
GitHub & Shiny web application: https://github.com/engganolang/enolex.
<https://enggano.shinyapps.io/enolex/>.

</div>

<div id="ref-rajeg_enolex_2024" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Daniel Krauße & Cokorda Pramartha. 2024.
EnoLEX: A diachronic lexical database for the Enggano language. In Ai
Inoue, Naho Kawamoto & Makoto Sumiyoshi (eds.), *AsiaLex 2024
proceedings: Asian Lexicography - Merging cutting-edge and established
approaches*, 123–132. Toyo University, Tokyo, Japan.
<https://doi.org/10.25446/oxford.27013864>.

</div>

</div>

[^1]: News related to the transcription of the Enggano-German dictionary
    are published
    [here](https://www.ling-phil.ox.ac.uk/news/2023/05/28/retro-digitisation-work-enggano-german-dictionary-udayana-university-indonesia)
    and
    [here](https://sasing.unud.ac.id/posts/boel-students-involved-in-research-project-led-by-researchers-from-the-university-of-oxford-uk)

Transcription note for the digitised Enggano-German Dictionary by Hans
Kähler (1987)
================
[Gede Primahadi Wijaya
Rajeg](https://www.ling-phil.ox.ac.uk/people/gede-rajeg)
<a itemprop="sameAs" content="https://orcid.org/0000-0002-2047-8621" href="https://orcid.org/0000-0002-2047-8621" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>

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
project](https://gtr.ukri.org/project/8AB0C3DC-F1C9-4CFA-BB4D-5BE748213372)
on the lexical resources for Enggano, led by the Faculty of Linguistics,
Philology and Phonetics at the University of Oxford, UK. Visit the
[central webpage of the Enggano
project](https://enggano.ling-phil.ox.ac.uk/)*.

<!-- badges: end -->

# Character encoding notes

### Character space for the remark, German Translation, and crossreference

- some remarks, crossref, and translation texts are trimmed (perhaps due
  to character limitation?)
  - YES, it is due to character limitationǃ FIXED

### Characters error in the Kähler’s transcription

- diphtong symbol, which is the long sound symbol (ː), is not
  recognised, but turned into subscript arrow (e.g., koʔa˿ixa˿, page
  156, K entry)
- BUT, normal colon (:) is recognised!
- acute accent ( ́) is not recognised and appears as double straight
  line (e.g., epa̿ra, page 239, P entry)

### Characters error in reading in into R with read_delim

- long sound/vowel symbol becomes “\xcb?”
- IMPORTANT & GOOD NEWS: nasal is maintained when we turn the tibble
  into as.data.frame()!!
- nasal is messed up whenever there is long sound/vowel symbol in the
  characters BEFORE turning tibble into as.data.frame()
- acute is lost and rendered as “\xcc?”

### Characters that are read correctly

- glottal stop (ʔ)
- diarisis (i.e., ö)
- nasal (e.g., ñ, m̃, ã)
- German sharp “s” (i.e., ß)
- the middle dot (e.g., peno·hoyo)
- Schwa (i.e., ə)
- ɔ

# Preliminary steps to handle the Kahler export in .csv

- open in MS Excel as UTF-8 encoded file
- select all columns as “Text”
- save again as .xlsx file (kahler1.xlsx)
- search and replace the subscript arrow (˿) (i.e., the diphthong
  character) as colon (:)
- insert space to separate stem table and example table, and example
  table with “stf_id”
- get the sheet range for stem and example tables
  - A29:X2117 (stem table) \## IMPORTANT: *Re-check the range when the
    transcription is done*!
  - A2119:T5500 (example table) \## IMPORTANT: *Re-check the range when
    the transcription is done*!
- try load them into R
  - THIS IS A MUCH BETTER SOLUTION!

# Notes on retrieving the truncated entries

- The truncated entries (based on the first batch of the transcribed
  entries) are found in the Remark and Crossreferences **of the Stem**,
  especially the entries whose characters count are over 200 characters.
  The steps to retrieve this are as follows:

  - read the stem csv entries (e.g., the `kstem` data frame)

  - select the columns that potentially contain large characters (i.e.,
    the German translation, remark, and crossreferences) (code:
    `select(matches("German|crossref|remark"))`)

  - count the characters in those columns using `across()` (code:
    `mutate(across(matches("German|crossref|remark"), nchar, .names = "nchar_{.col}"))`)
    and save the new data frame to say `char_count` object

  - arrange the `char_count` in descending order by the column with the
    character counts (code example:
    `arrange(desc(nchar_stem_crossref))`)

  - pull the relevant column as character vector and check if indeed the
    value(s) of that column is truncated (say the first ten values which
    have high character numbers) (code example:
    `pull(stem_crossref) %>% .[1:10]`). Double check which stem, and on
    which page.

    - In the stems, the trimmed entries for “**remark**” are as follows
      (the max. character count is 249 that got trimmed):

      - `'kms_page==117 & kms_entry_no==10'`

      - `'kms_page==180 & kms_entry_no==4'`

    - In the stems, the trimmed entries for “**crossreference**” are as
      follows (characters count ranging from 248-252):

      - `'kms_page==144 & kms_entry_no==16'`

      - `'kms_page==164, kms_entry_no==6'`

      - `'kms_page==108, kms_entry_no==5'`

- Do the same for the **Example** file (DONE; see the pre-processing
  code file).

# Character encoding notes

### Character space for the remark, German Translation, and crossreference

- some remarks, crossref, and translation texts are trimmed (perhaps due to character limitation?)
  - YES, it is due to character limitationǃ FIXED

### Characters error in the Kähler's transcription

- diphtong symbol, which is the long sound symbol (ː), is not recognised, but turned into subscript arrow (e.g., koʔa˿ixa˿, page 156, K entry) 
- BUT, normal colon (:) is recognised!
- acute accent ( ́) is not recognised and appears as double straight line (e.g., epa̿ra, page 239, P entry)

### Characters error in reading in into R with read_delim

- long sound/vowel symbol becomes "\\xcb?"
- IMPORTANT & GOOD NEWS: nasal is maintained when we turn the tibble into as.data.frame()!!
- nasal is messed up whenever there is long sound/vowel symbol in the characters BEFORE turning tibble into as.data.frame()
- acute is lost and rendered as "\\xcc?"

### Characters that are read correctly

- glottal stop (ʔ)
- diarisis (i.e., ö)
- nasal (e.g., ñ, m̃, ã)
- German sharp "s" (i.e., ß)
- the middle dot (e.g., peno·hoyo)
- Schwa (i.e., ə)
- ɔ

# Preliminary steps to handle the Kahler export in .csv

- open in MS Excel as UTF-8 encoded file
- select all columns as "Text"
- save again as .xlsx file (kahler1.xlsx)
- search and replace the subscript arrow (˿) (i.e., the diphthong character) as colon (:) 
- insert space to separate stem table and example table, and example table with "stf_id"
- get the sheet range for stem and example tables
  - A29:X2117 (stem table) ## IMPORTANT: _Re-check the range when the transcription is done_!
  - A2119:T5500 (example table) ## IMPORTANT: _Re-check the range when the transcription is done_!
- try load them into R
  - THIS IS A MUCH BETTER SOLUTION!

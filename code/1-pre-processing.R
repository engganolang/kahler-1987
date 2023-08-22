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
                                 stem_crossref))

### fixing typos =====
stems4 <- stems3 |> 
  mutate(stem_GermanTranslation = if_else(stem_id == "9_1686458720",
                                          str_replace(stem_GermanTranslation,
                                                      "^Baumstmpf",
                                                      "Baumstumpf"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "11_1684220509",
                                          str_replace(stem_GermanTranslation,
                                                      "\\,\\s",
                                                      " "),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "11_1684285480",
                                          str_replace_all(stem_GermanTranslation,
                                                      "\\s(\\([ri])",
                                                      "\\1"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "12_1683777738",
                                          str_replace_all(stem_GermanTranslation,
                                                          "P1-Praafix",
                                                          "Pl-Praafix"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "12_1684221429",
                                          str_replace_all(stem_GermanTranslation,
                                                          "^vor Wortstāmmen",
                                                          "^vor Wortstämmen"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "12_1684222515",
                                          str_replace_all(stem_GermanTranslation,
                                                          "frūher",
                                                          "früher"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "12_1684293286",
                                          str_replace_all(stem_GermanTranslation,
                                                          "prāfix",
                                                          "präfix"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "12_1684391149",
                                          str_replace_all(stem_GermanTranslation,
                                                          "\\'(jetzt)\\' (epoo)",
                                                          '\\1 "\\2"'),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "12_1684394399",
                                          paste(stem_GermanTranslation, ")", sep = ""),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "12_1684827147",
                                          paste(stem_GermanTranslation, ")", sep = ""),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "12_1684832859",
                                          paste(stem_GermanTranslation, ")", sep = ""),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "12_1684834734",
                                          str_replace_all(stem_GermanTranslation, 
                                                          "(Schatten)\\s",
                                                          "\\1"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "8_1684207650",
                                          str_replace_all(stem_GermanTranslation,
                                                          "vorhaden",
                                                          "vorhanden"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "8_1684213149",
                                          str_replace_all(stem_GermanTranslation,
                                                          "tritt ein",
                                                          "tritt ein!"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "8_1684419143",
                                          str_replace_all(stem_GermanTranslation,
                                                          "sie - pronominales Präfix (3 . P1) vor Verben mit b (u)-, m(ũ)-",
                                                          "sie - pronominales Präfix (3.Pl) vor Verben mit b(u)-, m(ũ)-"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id %in% c("9_1683686106", "9_1683692560"), "Männername"),
         stem_GermanTranslation = replace(stem_GermanTranslation, str_detect(stem_GermanTranslation, "^Mānnername$"), "Männername"),
         stem_GermanTranslation = str_replace_all(stem_GermanTranslation, "\\bMānnern\\b", "\\bMännern\\b"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "9_1684162360", "das Hochsein"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "15_1685974417", "Herkunft(sort), Herkommen"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "9_1684251022", "versammelt, ingesammelt, beisammen sein"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "11_1684915330", "ein Teil, eine Schnitte, halbe(r,s)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_GermanTranslation == "Nachkommen (schaft)", "Nachkommen(schaft)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "11_1685430762", "jene(r,s) - korrelat zur 3.P"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1684234186", "(herum)gehen"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1685082716", "Manggisbaum, Mangosteen"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1684234902", "öffne, laß losǃ"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id %in% c("15_1683514040", "9_1685072125"), "Männername"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '11_1684988020', 'dämmern, abends sein'),
         stem_crossref = replace(stem_crossref, stem_id == "8_1683688104", "H16 èkèh(è) 'korte breede bijl'"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '17_1683983407', 'Rückseite des Oberschenkels'),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '17_1684155768', 'Zaun(pfahl), Palisade'),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '17_1684243719', 'Käfer'),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "15_1688124911", "Öl"),
         stem_formVarian = replace(stem_formVarian, stem_id == "12_1684853273", "ekidá:uʔuo"),
         stem_crossref = replace(stem_crossref, stem_id == "9_1683946018", "DEC931 Ctenolophon parvifolius Oliv."),
         stem_crossref = if_else(stem_id == "10_1684235787",
                                 str_replace_all(stem_crossref,
                                                 "ā",
                                                 "ä"),
                                 stem_crossref),
         kms_entry_no = replace(kms_entry_no, stem_id == "9_1687835693" & kms_page == 289, 5),
         stem_crossref = replace(stem_crossref, stem_id == "15_1684276172", "G50 ; DEC 2575 Pangium edule Reinw."),
         stem_crossref = replace(stem_crossref, stem_id == "8_1684292694", "s hekoʔo ; kəʔəa⁴ ; kõʔõʔã"),
         stem_crossref = replace(stem_crossref, stem_id == "15_1685072233", "DEC 1462 pidjoe 'Ficus elastica Roxb'.; 'Gummifeigenbaum'; 'Kautschuk'; epitoe; efitoe 'Gummi'"),
         stem_crossref = replace(stem_crossref, stem_id == "15_1687838004", "DEC 601 Cananga odorata"),
         stem_remark = replace(stem_remark, stem_id == "12_1684221758", "ML alang-alang, DEC 1883 kahoie 'Imperata cylindrica beauv.', Heyne 19/109 ekahione 'Imperata spp.',  'Echetes Silberhaargras'"),
         stem_crossref = replace(stem_crossref, stem_id == "11_1686195257", "aber DEC 2181 und Heyne 147/4444 èbaè 'Manihot utilissima, also Cassava oder Maniok'"),
         stem_crossref = replace(stem_crossref, stem_id == "15_1686537974", "DEC 2557 inima 'Pandanus sp div'"),
         stem_crossref = replace(stem_crossref, stem_id == "11_1684291849", "DEC 345 kadodo 'Artocarpus Gomeziana Wall.', DEC 355 kadodotok 'Artocarpus rigilda bl.', Heyne 64/1946 kadodohok 'Artocarpus rigida'"),
         stem_crossref = replace(stem_crossref, stem_id == "17_1684764893", "DEC 3313 und Heyne 221/5544 kila oeloe 'Terminalia Catappa L., Etagenbaum'; 51z8: Mandelbaum"),
         stem_crossref = replace(stem_crossref, stem_id == "8_1688432302", "DEC 584 ejobe, eroba 'Calophyllum spectabile Willd.', Heyne 187/5178 ejobe 'Calophyllum Inophyllum'; MOD:174f due pezzi di legno che chiamavano eióba dai quali mi dissero si poteva subito ottenere il fuoco"),
         stem_dialectVariant = replace(stem_dialectVariant, stem_id == "12_1684853273", "ekidá:uʔuo (DIA)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "15_1688197962", "andere(r,s) ; außerdem"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1689178924", "Art und Weise"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1689603451", "Schnur, Stück (Hilfszählwort beim Zählen von Glasperlenschnüren)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1689484258", "sie (pronominales Präffix der 3.Pl.)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1689484729", "ihr(e) (Possessivsuffix der 3.Pl)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1689606013", "ausspannen (ein Netz)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "12_1687754541", "euer (Possessivsuffix 2.Pl)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '17_1684675601', '1. Augenfalte; 2. Bananenart'),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "12_1687754773", "Art und Weise; alsob, gleichsam"),
         stem_crossref = if_else(stem_id == "10_1685056239",
                                 str_replace_all(stem_crossref, "ekumãkũ", "ekumãkũũ"),
                                 stem_crossref),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "12_1687821624", 'euer (Possessivsuffix 2.Pl)'),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "12_1687821467", "mit Blättern überdeckte Fallgrube für den Wildschweinfang"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1685071684", '("Schößling des Anhängsels an der Halskette" =) Muschelart'),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1684293365", "Lautnachahmung für das Gackern von Hühnern"),
         stem_GermanTranslation = if_else(str_detect(stem_GermanTranslation, "\\bschlūrfen\\b"),
                                          str_replace_all(stem_GermanTranslation,
                                                          "\\bschlūrfen\\b",
                                                          "schlürfen"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = if_else(stem_id == "9_1683686558",
                                          str_replace_all(stem_GermanTranslation,
                                                          "frūheren",
                                                          "früheren"),
                                          stem_GermanTranslation),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '9_1684988081', 'bezeichnet Verstärkung oder Mehrzahl'),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "19_1688022882", "das Schwindligsein, Schwindel, Wanken"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1689608120", "Musang, Zibetkatze"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "19_1685158316", "ausgehöhlte Kokosnuß als Trinkgefäß"),
         stem_crossref = replace(stem_crossref, stem_id == "10_1689608120", "H88:277 nafie-nafie"),
         stem_crossref = if_else(stem_id == "9_1683946343",
                                 str_replace_all(stem_crossref, 
                                                 "\\brittan\\b", 
                                                 "rottan"),
                                 stem_crossref),
         stem_crossref = if_else(stem_id == "9_1684250436",
                                 str_replace_all(stem_crossref, "lengkoewas", "langkoewas"),
                                 stem_crossref),
         stem_formVarian = replace(stem_formVarian, stem_id == "12_1684850663", "ekabá:édo"),
         stem_remark = replace(stem_remark, stem_id == "12_1684850663", "Papaja?, H16 èkapèlo 'een papegaaisoort'"),
         stem_crossref = replace(stem_crossref, stem_id == "8_1685086297", "vgl eupurúhui; G37 ML 'burung elang'; H88:277 paeroehoe 'burung lang'"),
         stem_crossref = replace(stem_crossref, stem_id == "10_1689606013", "vgl nẽnẽkĩ; nə̃nə̃kĩ 'einkreisen'"),
         stem_remark = replace(stem_remark, stem_id == "8_1685086297", NA),
         stem_remark = replace(stem_remark, stem_id == "12_1683690091", "vgl piʔo 'umwickeln'"),
         stem_crossref = replace(stem_crossref, stem_id == "10_1689182298", "DEC 546 èoewa 'Calamus didymophyllus Becc' ; H88:276 èoewa 'rottan getah'"),
         stem_crossref = replace(stem_crossref, stem_id == "8_1688391208", "s iʔia; H88:276 èije 'de rottan manou met eetbare vruchten', d.h. DEC 557 Calamus Manan"),
         stem_crossref = replace(stem_crossref, stem_id == "17_1685194033", "H16 èpèkoeqwaq 'maag, krop'; èpèkoe(w)aq 'lever'"),
         stem_crossref = replace(stem_crossref, stem_id == "19_1686349301", "s Erz XIV; H16 hoeko(q)ä 'eene heilige plaats, waar de meeste geesten verblijven'; H88:281 Hoekok² '... waar zich de meeste èkowèks moeten ophouden'"),
         stem_remark = replace(stem_remark, stem_id == "12_1684835372", "H16 èkanoeoenoe 'vogelklauw' ; MOD canuúnu 'unghia'"),
         stem_remark = replace(stem_remark, stem_id == "12_1683692043", "H16 èkahaoe 'buiten eene omheinde plaats'"),
         stem_crossref = replace(stem_crossref, stem_id == "10_1684206125", "H16 kohèaq 'hut', èkohèaq 'zich met bladeren, inz. van den pandanus, beschutten tegen den regen; regenscherm.'"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "12_1687845906", "hin und wieder, ab und zu"),
         stem_homonymID = replace(stem_homonymID, stem_id == "12_1687847379", 2),
         stem_formVarian = replace(stem_formVarian, stem_id == "12_1687847379", NA),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "19_1688085511", "gehe!"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "9_1687838176", "(hebt das folgende Wort hervor)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1689179683", "wie (beim Vergleich)"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1689180816", "husch!l, hopp!, los!"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "10_1689180940", "Attributanzeiger vor dem Rectum; es ersetzt den Singular-Artikel e-"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "9_1684987873", "Bestandteil des Präfixes des Nomen causativum mit kip(a)-"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '11_1687148518', "Unkraut, Gras"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '11_1688357328', '(Blatt)Unterlage (fürs Essen)'),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "12_1688366718", "Sehne des Bogens, aus Bast"),
         stem_remark = replace(stem_remark, stem_id == "12_1688366718", "(Keuning 192 Fußnote 11: Deze 'schichten ende boghen' [im Journal der Reise von de Houtman und de Keyser, 1596] moeten wel op een vergissing berusten. In geen enkel ander bericht wordt ooit voor Enggano van pijl en boog gerept. Vermoedelijk heeft men op een afstand de werpsperen, waarvan de Engganees meestal enige met zich meedroeg, voor pijl en boog aangezien. Dgl bei MOD 243"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '12_1687834812', 'dann (kann den einem irrealen Konditionalsatz nachgestellten Hauptsatz einleiten)'),
         stem_remark = if_else(stem_id == '19_1683781496',
                               str_extract(stem_GermanTranslation, 'Sim.+(?=\\)\\sBruchs)'),
                               stem_remark),
         stem_remark = replace(stem_remark, stem_id == "15_1684804708", "ML kapur-kapur ; 'Ophiocephalus striatus Bl.' ?"),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == '19_1683781496', 'Bruchstück'),
         stem_remark = replace(stem_remark, stem_id == '19_1683781496', 'Sim təpi[x]'),
         stem_GermanTranslation = replace(stem_GermanTranslation, stem_id == "9_1683692494", "Überschuß, Rest"),
         stem_GermanTranslation = if_else(stem_id == "9_1683773300",
                                          str_replace_all(stem_GermanTranslation,
                                                          "Rüsselkāfers",
                                                          "Rüsselkäfers"),
                                          stem_GermanTranslation))



















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
tr1 <- stems4 |> 
  select(stem_id, stem_GermanTranslation) |> 
  filter(!is.na(stem_GermanTranslation)) |> 
  rename(German = stem_GermanTranslation) |> 
  mutate(category = "stem_GermanTranslation")
tr2 <- stems4 |> 
  select(stem_id, stem_GermanTranslationVariant) |> 
  filter(!is.na(stem_GermanTranslationVariant)) |> 
  rename(German = stem_GermanTranslationVariant) |> 
  mutate(category = "stem_GermanTranslationVariant")
tr3 <- stems4 |> 
  select(stem_id, stem_crossref) |> 
  filter(!is.na(stem_crossref)) |> 
  rename(German = stem_crossref) |> 
  mutate(category = "stem_crossref")
tr4 <- stems4 |> 
  select(stem_id, stem_remark) |> 
  filter(!is.na(stem_remark)) |> 
  rename(German = stem_remark) |> 
  mutate(category = "stem_remark")
tr_stem <- bind_rows(tr1, tr2, tr3, tr4) |> 
  mutate(wordcount = str_count(German, "\\b([^(),; !-=]+)\\b"))
stem_german_only <- tr_stem |> 
  filter(str_detect(category, "German")) |> 
  mutate(wordcount = str_count(German, "\\b([^(),; !-=]+)\\b"))
sum(stem_german_only$wordcount)
sum(tr_stem$wordcount)

stems4 <- stems4 |> 
  mutate(stem_etym_form_German = if_else(stem_id == "9_1685625844",
                                         str_extract(stem_GermanTranslation,
                                                     "\"der zu Angelnde\""),
                                         NA),
         stem_GermanTranslation = if_else(stem_id == "9_1685625844",
                                          str_replace_all(stem_GermanTranslation,
                                                          "^\\(\"der zu Angelnde\"\\s\\=\\)\\s",
                                                          ""),
                                          stem_GermanTranslation))

stems4 |> 
  writexl::write_xlsx("data-main/stem4.xlsx")
# stem_german_only |> 
#   writexl::write_xlsx("to-translate/1_stem_german_translation.xlsx")



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
tr_ex <- bind_rows(tr1, tr2, tr3, tr4) |> 
  mutate(wordcount = str_count(German, "\\b([^(),; !-=]+)\\b"))
example_german_only <- tr_ex |> 
  filter(str_detect(category, "German")) |> 
  mutate(wordcount = str_count(German, "\\b([^(),; !-=]+)\\b"))
sum(example_german_only$wordcount)
sum(tr_ex$wordcount)
# example_german_only |> 
#   writexl::write_xlsx("to-translate/2_example_german_translation.xlsx")


## total data for translations =====
(rows_numbers_german_only <- sum(nrow(stem_german_only), nrow(example_german_only)))
(rows_numbers_all <- sum(nrow(tr_stem), nrow(tr_ex)))
(wordcount_german_only <- sum(example_german_only$wordcount, stem_german_only$wordcount))
(wordcount_all <- sum(tr_ex$wordcount, tr_stem$wordcount))

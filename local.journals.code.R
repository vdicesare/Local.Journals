library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(readr)
library(stringr)

### MEGA JOURNALS DATAFRAME CONSTRUCTION
# OpenAlex upload
openalex_data <- read.csv("~/Desktop/Local.Journals/OpenAlex.csv")

# MJL upload and data mining
mjl_data <- read.csv("~/Desktop/Local.Journals/MJL.csv")
mjl_data <- mjl_data %>% distinct()
mjl_data <- mjl_data %>% group_by(journal_name, issn, eissn) %>%
                         summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")

# Scopus upload and data mining
scopus_data <- readxl::read_excel("~/Desktop/Local.Journals/Scopus.xlsx")
scopus_data <- scopus_data %>% group_by(journal_name, issn, eissn) %>%
                               summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
scopus_data$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_data$issn)
scopus_data$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_data$eissn)

# DOAJ upload and data mining
doaj_data <- read.csv("~/Desktop/Local.Journals/DOAJ.csv")
doaj_data$issn <- ifelse(!is.na(doaj_data$issn) & doaj_data$issn != "", 
                         str_pad(doaj_data$issn, width = 8, side = "left", pad = "0"), 
                         doaj_data$issn)
doaj_data$eissn <- ifelse(!is.na(doaj_data$eissn) & doaj_data$eissn != "", 
                          str_pad(doaj_data$eissn, width = 8, side = "left", pad = "0"), 
                          doaj_data$eissn)
doaj_data$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_data$issn)
doaj_data$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_data$eissn)

# SJR upload and data mining
sjr_data <- readxl::read_excel("~/Desktop/Local.Journals/SJR.xlsx")
sjr_data <- sjr_data %>% separate(eissn, into = c("eissn", "issn"), sep = ", ", extra = "merge", fill = "right") %>%
                         mutate(issn = ifelse(is.na(issn), NA, issn))
sjr_data <- sjr_data %>% group_by(journal_name, issn, eissn) %>%
                         summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
sjr_data$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", sjr_data$issn)
sjr_data$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", sjr_data$eissn)

# JCR upload and data mining
jcr_data <- list.files(path = "~/Desktop/Local.Journals/JCR", pattern = "VictoriaDi.*JCR_JournalResults.*", full.names = TRUE)
jcr_data <- rbindlist(lapply(jcr_data, fread, sep = ","), fill = TRUE)
jcr_data <- jcr_data %>% group_by(`Journal name`, `JCR Abbreviation`, Publisher, ISSN, eISSN) %>%
                         summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
jcr_data <- jcr_data %>% rename(journal_name = `Journal name`, issn = ISSN, eissn = eISSN)
jcr_data[jcr_data == "N/A"] <- NA

# add unique identifiers to each dataframe
openalex_data <- openalex_data %>% mutate(OpenAlex_ID = paste0("OpenAlex", row_number())) %>%
                                   relocate(OpenAlex_ID)
mjl_data <- mjl_data %>% mutate(MJL_ID = paste0("MJL", row_number())) %>%
                         relocate(MJL_ID)
scopus_data <- scopus_data %>% mutate(Scopus_ID = paste0("Scopus", row_number())) %>%
                               relocate(Scopus_ID)
doaj_data <- doaj_data %>% mutate(DOAJ_ID = paste0("DOAJ", row_number())) %>%
                           relocate(DOAJ_ID)
sjr_data <- sjr_data %>% mutate(SJR_ID = paste0("SJR", row_number())) %>%
                          relocate(SJR_ID)
jcr_data <- jcr_data %>% mutate(JCR_ID = paste0("JCR", row_number())) %>%
                         relocate(JCR_ID)

# create variable to unify all ISSN codes per dataframe
openalex_data$issn_codes <- apply(openalex_data[, c("issn", "issn_l")], 1, function(x) {
                                  unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                  paste(unique_values, collapse = ";")})
mjl_data$issn_codes <- apply(mjl_data[, c("issn", "eissn")], 1, function(x) {
                             unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                             paste(unique_values, collapse = ";")})
scopus_data$issn_codes <- apply(scopus_data[, c("issn", "eissn")], 1, function(x) {
                                unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                paste(unique_values, collapse = ";")})
doaj_data$issn_codes <- apply(doaj_data[, c("issn", "eissn")], 1, function(x) {
                              unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                              paste(unique_values, collapse = ";")})
sjr_data$issn_codes <- apply(sjr_data[, c("issn", "eissn")], 1, function(x) {
                             unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                             paste(unique_values, collapse = ";")})
jcr_data$issn_codes <- apply(jcr_data[, c("issn", "eissn")], 1, function(x) {
                             unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                             paste(unique_values, collapse = ";")})


# TRATAMIENTO DE LOS DISTINTOS ISSNS, TAL VEZ LO MEJOR SEA CONSERVARLOS TODOS, INDEPENDIENTEMENTE DE SU TIPO, Y PROGRAMAR EL MATCHEO POR CUALQUIERA DE ELLOS
## SEGUIR TRABAJANDO POR ACÁ, ARMANDO ESA PRIMERA RONDA DE MATCHEO POR ISSN_CODES Y LUEGO PENSAR LA SEGUNDA RONDA A PARTIR DE LOS TÍTULOS.

# standardize all journal_name variables to ensure accurate comparisons
mjl_data <- mjl_data %>% mutate(journal_name = journal_name %>%
                                  toupper() %>%
                                  str_replace_all("[[:punct:]]", "") %>%
                                  str_replace_all(" ", "") %>%
                                  stringi::stri_trans_general("Latin-ASCII") %>%
                                  iconv(from = "UTF-8", to = "ASCII", sub = ""))
scopus_data <- scopus_data %>% mutate(journal_name = journal_name %>%
                                        toupper() %>%
                                        str_replace_all("[[:punct:]]", "") %>%
                                        str_replace_all(" ", "") %>%
                                        stringi::stri_trans_general("Latin-ASCII") %>%
                                        iconv(from = "UTF-8", to = "ASCII", sub = ""))
doaj_data <- doaj_data %>% mutate(journal_name = journal_name %>%
                                    toupper() %>%
                                    str_replace_all("[[:punct:]]", "") %>%
                                    str_replace_all(" ", "") %>%
                                    stringi::stri_trans_general("Latin-ASCII") %>%
                                    iconv(from = "UTF-8", to = "ASCII", sub = ""))
sjr_data <- sjr_data %>% mutate(journal_name = journal_name %>%
                                  toupper() %>%
                                  str_replace_all("[[:punct:]]", "") %>%
                                  str_replace_all(" ", "") %>%
                                  stringi::stri_trans_general("Latin-ASCII") %>%
                                  iconv(from = "UTF-8", to = "ASCII", sub = ""))
jcr_data <- jcr_data %>% mutate(journal_name = journal_name %>%
                                  toupper() %>%
                                  str_replace_all("[[:punct:]]", "") %>%
                                  str_replace_all(" ", "") %>%
                                  stringi::stri_trans_general("Latin-ASCII") %>%
                                  iconv(from = "UTF-8", to = "ASCII", sub = ""))





### LOCAL VARIABLE PUBS_PROP COMPUTING
pubs <- read.csv("~/Desktop/Local.Journals/local_variable_pubs.csv", col.names = c("journal_id", "journal_name", "article_id", "references_prop", "country"))
pubs <- within(pubs, rm(references_prop))
pubs <- pubs %>% separate_rows(country, sep = "&/&")
pubs <- pubs %>% group_by(journal_id) %>%
                 mutate(pubs_total = n_distinct(article_id)) %>%
                 ungroup()

pubs <- pubs %>% group_by(journal_id, country) %>%
                 mutate(pubs_count = n()) %>%
                 ungroup()

pubs <- within(pubs, rm(article_id))
pubs <- pubs %>% distinct()
pubs <- pubs %>% group_by(journal_id) %>%
                 filter(pubs_count == max(pubs_count)) %>%
                 ungroup()

pubs <- pubs %>% mutate(pubs_prop = round(pubs_count / pubs_total, 2))


### LOCAL VARIABLE REFS_PROP COMPUTING
refs <- list.files(path = "~/Desktop/Local.Journals/local_variable_refs", pattern = "^userdb_VDC\\.local_research_OA2410_articles_journals_export-\\d{12}\\.csv$", full.names = TRUE)
refs <- rbindlist(lapply(refs, fread))

refs <- refs %>% group_by(journal_id) %>%
                 mutate(refs_total = n_distinct(reference_id)) %>%
                 ungroup()

refs <- refs %>% group_by(journal_id, country) %>%
                 mutate(refs_count = n()) %>%
                 ungroup()

refs <- within(refs, rm(article_id, reference_id))
refs <- refs %>% distinct()
refs <- refs %>% group_by(journal_id) %>%
                 filter(refs_count == max(refs_count)) %>%
                 ungroup()

refs <- refs %>% mutate(refs_prop = round(refs_count / refs_total, 2))


### LOCAL VARIABLE CITS_PROP COMPUTING
cits <- read.csv("~/Desktop/Local.Journals/local_variable_cits.csv", col.names = c("journal_id", "journal_name", "article_id", "citing_work_id", "country"))

cits <- cits %>% group_by(journal_id) %>%
                 mutate(cits_total = n_distinct(citing_work_id)) %>%
                 ungroup()

cits <- cits %>% group_by(journal_id, country) %>%
                 mutate(cits_count = n()) %>%
                 ungroup()

cits <- within(cits, rm(article_id, citing_work_id))
cits <- cits %>% distinct()
cits <- cits %>% group_by(journal_id) %>%
                 filter(cits_count == max(cits_count)) %>%
                 ungroup()

cits <- cits %>% mutate(cits_prop = round(cits_count / cits_total, 2))


### LOCAL VARIABLE LANGUAGES COMPUTING
# open WOS, Scopus and DOAJ datasets
wos_lang <- read.csv("~/Desktop/Local.Journals/MJL.csv")[, c("journal_name", "issn", "eissn", "language")]
scopus_lang <- readxl::read_excel("~/Desktop/Local.Journals/Scopus.xlsx")[, c("journal_name", "issn", "eissn", "language")]
doaj_lang <- read.csv("~/Desktop/Local.Journals/DOAJ.csv")[, c("journal_name", "issn", "eissn", "language")]

langs <- read.csv("~/Desktop/Local.Journals/local_variable_langs.csv")[, c("journal_id", "journal_name", "issn", "eissn", "language")]

# standardize all journal_name variables to ensure accurate comparisons
wos_lang <- wos_lang %>% mutate(journal_name = journal_name %>%
                                  toupper() %>%
                                  str_replace_all("[[:punct:]]", "") %>%
                                  str_replace_all(" ", "") %>%
                                  stringi::stri_trans_general("Latin-ASCII"))

scopus_lang <- scopus_lang %>% mutate(journal_name = journal_name %>%
                                        toupper() %>%
                                        str_replace_all("[[:punct:]]", "") %>%
                                        str_replace_all(" ", "") %>%
                                        stringi::stri_trans_general("Latin-ASCII"))

doaj_lang <- doaj_lang %>% mutate(journal_name = journal_name %>%
                                    toupper() %>%
                                    str_replace_all("[[:punct:]]", "") %>%
                                    str_replace_all(" ", "") %>%
                                    stringi::stri_trans_general("Latin-ASCII"))

langs <- langs %>% mutate(journal_name = journal_name %>%
                            toupper() %>%
                            str_replace_all("[[:punct:]]", "") %>%
                            str_replace_all(" ", "") %>%
                            stringi::stri_trans_general("Latin-ASCII"))

# make sure all language variables present the same format, which is a single cell per journal with separator ", "
langs <- langs %>% group_by(journal_id, journal_name, issn, eissn) %>%
                   summarise(language = paste(unique(trimws(language[language != ""])), collapse = ", "), 
                             .groups = "drop")

# journal_name trials: identify unique common journals between WOS, Scopus and DOAJ, and OpenAlex
cat("Common journal names between WOS & OpenAlex: ", length(intersect(unique(wos_lang$journal_name), unique(langs$journal_name))), "\n") # 15345
cat("Common journal names between Scopus & OpenAlex: ", length(intersect(unique(scopus_lang$journal_name), unique(langs$journal_name))), "\n") # 18480
cat("Common journal names between DOAJ & OpenAlex: ", length(intersect(unique(doaj_lang$journal_name), unique(langs$journal_name))), "\n") # 10679

# issn trials
cat("Common ISSN codes between WOS & OpenAlex: ", length(intersect(unique(wos_lang$issn), unique(langs$issn))), "\n") # 15143
cat("Common issn codes between Scopus & OpenAlex: ", length(intersect(unique(scopus_lang$issn), unique(langs$issn))), "\n") # 10402
cat("Common issn codes between DOAJ & OpenAlex: ", length(intersect(unique(doaj_lang$issn), unique(langs$issn))), "\n") # 6649

# eissn trials
cat("Common eissn codes between WOS & OpenAlex: ", length(intersect(unique(wos_lang$eissn), unique(langs$eissn))), "\n") # 12808
cat("Common eissn codes between Scopus & OpenAlex: ", length(intersect(unique(scopus_lang$eissn), unique(langs$eissn))), "\n") # 12883
cat("Common eissn codes between DOAJ & OpenAlex: ", length(intersect(unique(doaj_lang$eissn), unique(langs$eissn))), "\n") # 6687

# journal_name & issn intersections
cat("Number of unique cases where both journal_name and ISSN match between WOS & OpenAlex: ", 
    length(intersect(paste(wos_lang$journal_name, wos_lang$issn), paste(langs$journal_name, langs$issn))), "\n") # 12852
cat("Number of unique cases where both journal_name and ISSN match between Scopus & OpenAlex: ", 
    length(intersect(paste(scopus_lang$journal_name, scopus_lang$issn), paste(langs$journal_name, langs$issn))), "\n") # 8413
cat("Number of unique cases where both journal_name and ISSN match between Scopus & OpenAlex: ", 
    length(intersect(paste(doaj_lang$journal_name, doaj_lang$issn), paste(langs$journal_name, langs$issn))), "\n") # 4829



### LOCAL VARIABLE DATABASES COMPUTING
ddbb <- read.csv("~/Desktop/Local.Journals/local_variable_ddbb.csv")[, c("journal_id", "journal_name", "issn", "eissn")]


### LOCAL VARIABLE TOPONYMS COMPUTING
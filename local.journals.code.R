library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(readr)
library(stringr)
library(bit64)
options(scipen = 999)


### MEGA JOURNALS DATAFRAME CONSTRUCTION
# OpenAlex upload and data mining
openalex_journals <- read.csv("~/Desktop/Local.Journals/OpenAlex+50.csv")
openalex_journals$APC_prices <- ifelse(is.na(openalex_journals$price) | openalex_journals$price == "" |
                                         is.na(openalex_journals$currency) | openalex_journals$currency == "", NA, 
                                       paste(openalex_journals$price, openalex_journals$currency))
openalex_journals <- openalex_journals %>% group_by(journal_id) %>%
                                           summarise(across(everything(), ~ first(.)),
                                                     APC_prices = paste(unique(APC_prices[!is.na(APC_prices) & APC_prices != ""]), collapse = "; ")) %>%
                                           ungroup()
###### SEGUIR
### agregar más variables a los journals desde el archivo desagregado por artículos. trabajar desde ahí los topics, etc. para los journals, y dejar los títulos para la variable local
## hay que agrupar todos los topics, etc para cada journal, separándolos con ;
openalex_articles <- list.files(path = "~/Desktop/Local.Journals/OAarticles", pattern = "ArticlesTitlesTopics-.*", full.names = TRUE)
openalex_articles <- rbindlist(lapply(openalex_articles, fread, sep = ","), fill = TRUE)
openalex_articles$journal_id <- as.numeric(as.character(openalex_articles$journal_id))
openalex_articles$article_id <- as.numeric(as.character(openalex_articles$article_id))

concat_unique <- function(x) {x <- unique(na.omit(x))
                              x <- x[x != ""]
                              if (length(x) > 0) paste(x, collapse = ";") else NA}
openalex_topics_aggregated <- openalex_articles %>% group_by(journal_id) %>%
                                                    summarise(topic_display_name = concat_unique(topic_display_name),
                                                              subfield = concat_unique(subfield),
                                                              field = concat_unique(field),
                                                              domain = concat_unique(domain),
                                                              primary_topic_display_name = concat_unique(primary_topic_display_name))

# replace the subfield, field and domain codes with the corresponding tags from openalex_topics
openalex_topics <- readxl::read_excel("~/Desktop/Local.Journals/OAtopics.xlsx")
subfield_lookup <- openalex_topics %>% select(subfield_id, subfield_name) %>%
                                       distinct()
field_lookup <- openalex_topics %>% select(field_id, field_name) %>%
                                    distinct()
domain_lookup <- openalex_topics %>% select(domain_id, domain_name) %>%
                                     distinct()
replace_codes_with_tags <- function(code_column, lookup_table, code_name, tag_name) {sapply(code_column, function(codes) {
                                                                                     code_list <- strsplit(codes, ";")[[1]]
                                                                                     tags <- sapply(code_list, function(code) {
                                                                                       match_code <- lookup_table %>% filter(!!sym(code_name) == code)
                                                                                       if (nrow(match_code) > 0) match_code[[tag_name]] else NA})
                                                                                     paste(tags, collapse = ";")})}
openalex_topics_aggregated <- openalex_topics_aggregated %>% mutate(subfield = replace_codes_with_tags(subfield, subfield_lookup, "subfield_id", "subfield_name"),
                                                                    field = replace_codes_with_tags(field, field_lookup, "field_id", "field_name"),
                                                                    domain = replace_codes_with_tags(domain, domain_lookup, "domain_id", "domain_name"))


### LUEGO DE REARMAR OPENALEX, VOLVER A CORRER EL CÓDIGO DE CADA BBDD Y MATCHEAR POR ISSN

openalex_journals_50 <- read.csv("~/Desktop/Local.Journals/OpenAlex-50.csv")


# MJL upload and data mining
mjl_data <- read.csv("~/Desktop/Local.Journals/MJL.csv")
mjl_data <- mjl_data %>% distinct()
mjl_data <- mjl_data %>% group_by(journal_name, issn, eissn) %>%
                         summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")


# JCR upload and data mining
jcr_data <- list.files(path = "~/Desktop/Local.Journals/JCR", pattern = "VictoriaDi.*JCR_JournalResults.*", full.names = TRUE)
jcr_data <- rbindlist(lapply(jcr_data, fread, sep = ","), fill = TRUE)
jcr_data <- jcr_data %>% group_by(`Journal name`, `JCR Abbreviation`, Publisher, ISSN, eISSN) %>%
  summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
jcr_data <- jcr_data %>% rename(journal_name = `Journal name`, issn = ISSN, eissn = eISSN)
jcr_data[jcr_data == "N/A"] <- NA
jcr_data <- bind_rows(jcr_data, read.csv("~/Desktop/Local.Journals/JCR/JCR_missing_journals.csv", check.names = FALSE) %>%
                        mutate(across(everything(), as.character)))


# Scopus upload and data mining
scopus_data <- readxl::read_excel("~/Desktop/Local.Journals/Scopus.xlsx")
scopus_data <- scopus_data %>% group_by(journal_name, issn, eissn) %>%
                               summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
scopus_data[scopus_data == "NA"] <- NA
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


# CWTS upload
cwts_data <- readxl::read_excel("~/Desktop/Local.Journals/CWTS.xlsx")


# add unique identifiers to each dataframe
openalex_journals <- openalex_journals %>% mutate(OA_ID = paste0("OA", row_number())) %>%
                                   relocate(OA_ID)
openalex_journals_50 <- openalex_journals_50 %>% mutate(OA_ID = paste0("OA", row_number())) %>%
                                         relocate(OA_ID)
mjl_data <- mjl_data %>% mutate(MJL_ID = paste0("MJL", row_number())) %>%
                         relocate(MJL_ID)
jcr_data <- jcr_data %>% mutate(JCR_ID = paste0("JCR", row_number())) %>%
                         relocate(JCR_ID)
scopus_data <- scopus_data %>% mutate(SCOP_ID = paste0("SCOP", row_number())) %>%
                               relocate(SCOP_ID)
doaj_data <- doaj_data %>% mutate(DOAJ_ID = paste0("DOAJ", row_number())) %>%
                           relocate(DOAJ_ID)
sjr_data <- sjr_data %>% mutate(SJR_ID = paste0("SJR", row_number())) %>%
                          relocate(SJR_ID)
cwts_data <- cwts_data %>% mutate(CWTS_ID = paste0("CWTS", row_number())) %>%
                           relocate(CWTS_ID)


# create variable to unify all ISSN codes per dataframe
openalex_journals$issn_codes <- apply(openalex_journals[, c("issn", "issn_l")], 1, function(x) {
                                  unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                  paste(unique_values, collapse = ";")})
openalex_journals_50$issn_codes <- apply(openalex_journals_50[, c("issn", "issn_l")], 1, function(x) {
                                     unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                     paste(unique_values, collapse = ";")})
mjl_data$issn_codes <- apply(mjl_data[, c("issn", "eissn")], 1, function(x) {
                             unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                             paste(unique_values, collapse = ";")})
jcr_data$issn_codes <- apply(jcr_data[, c("issn", "eissn")], 1, function(x) {
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


# create variable to unify all journal names variants in Scopus
scopus_data <- scopus_data %>% rowwise() %>%
                               mutate(journal_name_variants = paste(unique(unlist(strsplit(c_across(`Related Title 1`:`Other Related Title 4`)[!is.na(c_across(`Related Title 1`:`Other Related Title 4`))], ";"))), collapse = ";")) %>%
                               ungroup()


# select, rename and organize all variables per dataframe
#### COMPLETAR CON LAS NUEVAS VARIABLES PARA OPENALEX_JOURNALS
openalex_journals <- openalex_journals %>% select(OA_ID, OA_source_ID = journal_id, OA_ISSN_codes = issn_codes, OA_journal_name = journal_name)
openalex_journals_50 <- openalex_journals_50 %>% select(OA_ID, OA_source_ID = journal_id, OA_ISSN_codes = issn_codes, OA_journal_name = journal_name)

mjl_data <- mjl_data %>% select(MJL_ID, MJL_ISSN_codes = issn_codes, MJL_journal_name = journal_name, MJL_publisher = Publisher.name,
                                MJL_publisher_country = Publisher.address, MJL_language = Languages, MJL_categories = Web.of.Science.Categories)

jcr_data <- jcr_data %>% select(JCR_ID, JCR_ISSN_codes = issn_codes, JCR_journal_name = journal_name, JCR_journal_name_variants = `JCR Abbreviation`,
                                JCR_publisher = Publisher, JCR_categories = Category, JCR_edition = Edition, JCR_JCI_2023 = `2023 JCI`,
                                JCR_JCI_rank = `JCI Rank`, JCR_JCI_quartile = `JCI Quartile`, JCR_JCI_percentile = `JCI Percentile`,
                                JCR_JIF_2023 = `2023 JIF`, JCR_JIF_rank = `JIF Rank`, JCR_JIF_quartile = `JIF Quartile`, JCR_JIF_percentile = `JIF Percentile`,
                                JCR_JIF_5_years = `5 Year JIF`, JCR_JIF_5_years_quartile = `5 Year JIF Quartile`, JCR_JIF_no_self_cites = `JIF Without Self Cites`,
                                JCR_immediacy_index = `Immediacy Index`, JCR_AIS = `Article Influence Score`, JCR_AIS_quartile = `AIS Quartile`,
                                JCR_eigenfactor = Eigenfactor, JCR_eigenfactor_normalized = `Normalized Eigenfactor`, JCR_citing_half_life = `Citing Half-Life`,
                                JCR_cited_half_life = `Cited Half-Life`, JCR_percent_articles_citable_items = `% of Articles in Citable items`,
                                JCR_percent_citable_open_access = `% of Citable OA`, JCR_total_articles = `Total Articles`, JCR_total_citations = `Total Citations`, JCR_citable_articles = `Citable Items`)

scopus_data <- scopus_data %>% select(SCOP_ID, SCOP_source_ID = `Sourcerecord ID`, SCOP_ISSN_codes = issn_codes, SCOP_journal_name = journal_name,
                                      SCOP_journal_name_variants = journal_name_variants, SCOP_publisher = Publisher, SCOP_main_publisher = `Publisher Imprints Grouped to Main Publisher`,
                                      SCOP_language = language, SCOP_ASJC_codes = `All Science Journal Classification Codes (ASJC)`, SCOP_coverage = Coverage, SCOP_open_access = `Open Access Status`,
                                      SCOP_discontinued = `Titles Discontinued by Scopus Due to Quality Issues`, SCOP_medline_sourced = `Medline-sourced Title? (See additional details under separate tab.)`)

doaj_data <- doaj_data %>% select(DOAJ_ID, DOAJ_ISSN_codes = issn_codes, DOAJ_continues_ISSN = Continues, DOAJ_continued_by_ISSN = Continued.By,
                                  DOAJ_journal_name = journal_name, DOAJ_journal_name_variants = Alternative.title, DOAJ_publisher = Publisher,
                                  DOAJ_publisher_country = Country.of.publisher, DOAJ_other_organization = Other.organisation, DOAJ_other_organization_country = Country.of.other.organisation,
                                  DOAJ_language = language, DOAJ_LCC_codes = LCC.Codes, DOAJ_subjects = Subjects, DOAJ_keywords = Keywords,
                                  DOAJ_open_access = Does.the.journal.comply.to.DOAJ.s.definition.of.open.access., DOAJ_open_license_since = When.did.the.journal.start.to.publish.all.content.using.an.open.license.,
                                  DOAJ_license = Journal.license, DOAJ_license_attributes = License.attributes, DOAJ_author_unrestricted_rights = Author.holds.copyright.without.restrictions,
                                  DOAJ_open_citations = Journal.complies.with.I4OC.standards.for.open.citations, DOAJ_machine_readable_license = Machine.readable.CC.licensing.information.embedded.or.displayed.in.articles,
                                  DOAJ_review_process = Review.process, DOAJ_average_weeks_for_publication = Average.number.of.weeks.between.article.submission.and.publication,
                                  DOAJ_APC_prices = APC.amount, DOAJ_other_fees = Has.other.fees, DOAJ_waiver_policy = Journal.waiver.policy..for.developing.country.authors.etc.,
                                  DOAJ_deposit_policy = Deposit.policy.directory, DOAJ_plagiarism_policy = Journal.plagiarism.screening.policy,
                                  DOAJ_persistent_identifiers = Persistent.article.identifiers, DOAJ_preservation_services = Preservation.Services,
                                  DOAJ_national_library_preservation_services = Preservation.Service..national.library, DOAJ_website = Journal.URL)

sjr_data <- sjr_data %>% select(SJR_ID, SJR_source_ID = Sourceid, SJR_ISSN_codes = issn_codes, SJR_journal_name = journal_name, SJR_publisher = Publisher,
                                SJR_publisher_country = Country, SJR_region = Region, SJR_categories = Categories, SJR_areas = Areas, SJR_coverage = Coverage,
                                SJR_SJR = SJR, SJR_rank = Rank, SJR_best_quartile = `SJR Best Quartile`, SJR_h_index = `H index`, SJR_total_articles_2023 = `Total Docs. (2023)`,
                                SJR_total_articles_3_years = `Total Docs. (3years)`, SJR_total_references = `Total Refs.`, SJR_references_per_articles = `Ref. / Doc.`,
                                SJR_total_citations_3_years = `Total Cites (3years)`, SJR_citations_per_articles_2_years = `Cites / Doc. (2years)`,
                                SJR_citable_articles_3_years = `Citable Docs. (3years)`, SJR_percent_female = `%Female`, SJR_SDG = SDG, SJR_overton = Overton)

cwts_data <- cwts_data %>% select(CWTS_ID, CWTS_ISSN_codes = issn_codes, CWTS_journal_name = `Source title`, CWTS_percent_self_citations = `% self cit`,
                                  CWTS_SNIP = SNIP, CWTS_SNIP_lower_bound = `SNIP (lower bound)`, CWTS_SNIP_upper_bound = `SNIP (upper bound)`,
                                  CWTS_IPP = IPP, CWTS_IPP_lower_bound = `IPP (lower bound)`, CWTS_IPP_upper_bound = `IPP (upper bound)`)


# prepare dataframes for matching
openalex_journals_match <- subset(openalex_journals, select = c("OA_ID", "OA_ISSN_codes"))
openalex_journals_match <- openalex_journals_match %>% mutate(OA_ISSN_codes = strsplit(as.character(OA_ISSN_codes), ";")) %>%
                                               unnest(OA_ISSN_codes) %>%
                                               mutate(OA_ISSN_codes = gsub("\\s+", "", OA_ISSN_codes)) %>%
                                               filter(OA_ISSN_codes != "") %>%
                                               distinct(OA_ID, OA_ISSN_codes)

openalex_journals_50_match <- subset(openalex_journals_50, select = c("OA_ID", "OA_ISSN_codes"))
openalex_journals_50_match <- openalex_journals_50_match %>% mutate(OA_ISSN_codes = strsplit(as.character(OA_ISSN_codes), ";")) %>%
                                                     unnest(OA_ISSN_codes) %>%
                                                     mutate(OA_ISSN_codes = gsub("\\s+", "", OA_ISSN_codes)) %>%
                                                     filter(OA_ISSN_codes != "") %>%
                                                     distinct(OA_ID, OA_ISSN_codes)

mjl_data_match <- subset(mjl_data, select = c("MJL_ID", "MJL_ISSN_codes"))
mjl_data_match <- mjl_data_match %>% mutate(MJL_ISSN_codes = strsplit(as.character(MJL_ISSN_codes), ";")) %>%
                                     unnest(MJL_ISSN_codes) %>%
                                     mutate(MJL_ISSN_codes = gsub("\\s+", "", MJL_ISSN_codes)) %>%
                                     filter(MJL_ISSN_codes != "") %>%
                                     distinct(MJL_ID, MJL_ISSN_codes)

jcr_data_match <- subset(jcr_data, select = c("JCR_ID", "JCR_ISSN_codes"))
jcr_data_match <- jcr_data_match %>% mutate(JCR_ISSN_codes = strsplit(as.character(JCR_ISSN_codes), ";")) %>%
                                     unnest(JCR_ISSN_codes) %>%
                                     mutate(JCR_ISSN_codes = gsub("\\s+", "", JCR_ISSN_codes)) %>%
                                     filter(JCR_ISSN_codes != "") %>%
                                     distinct(JCR_ID, JCR_ISSN_codes)

scopus_data_match <- subset(scopus_data, select = c("SCOP_ID", "SCOP_ISSN_codes"))
scopus_data_match <- scopus_data_match %>% mutate(SCOP_ISSN_codes = strsplit(as.character(SCOP_ISSN_codes), ";")) %>%
                                           unnest(SCOP_ISSN_codes) %>%
                                           mutate(SCOP_ISSN_codes = gsub("\\s+", "", SCOP_ISSN_codes)) %>%
                                           filter(SCOP_ISSN_codes != "") %>%
                                           distinct(SCOP_ID, SCOP_ISSN_codes)

doaj_data_match <- subset(doaj_data, select = c("DOAJ_ID", "DOAJ_ISSN_codes"))
doaj_data_match <- doaj_data_match %>% mutate(DOAJ_ISSN_codes = strsplit(as.character(DOAJ_ISSN_codes), ";")) %>%
                                       unnest(DOAJ_ISSN_codes) %>%
                                       mutate(DOAJ_ISSN_codes = gsub("\\s+", "", DOAJ_ISSN_codes)) %>%
                                       filter(DOAJ_ISSN_codes != "") %>%
                                       distinct(DOAJ_ID, DOAJ_ISSN_codes)

sjr_data_match <- subset(sjr_data, select = c("SJR_ID", "SJR_ISSN_codes"))
sjr_data_match <- sjr_data_match %>% mutate(SJR_ISSN_codes = strsplit(as.character(SJR_ISSN_codes), ";")) %>%
                                     unnest(SJR_ISSN_codes) %>%
                                     mutate(SJR_ISSN_codes = gsub("\\s+", "", SJR_ISSN_codes)) %>%
                                     filter(SJR_ISSN_codes != "") %>%
                                     distinct(SJR_ID, SJR_ISSN_codes)

cwts_data_match <- subset(cwts_data, select = c("CWTS_ID", "CWTS_ISSN_codes"))
cwts_data_match <- cwts_data_match %>% mutate(CWTS_ISSN_codes = strsplit(as.character(CWTS_ISSN_codes), ";")) %>%
                                       unnest(CWTS_ISSN_codes) %>%
                                       mutate(CWTS_ISSN_codes = gsub("\\s+", "", CWTS_ISSN_codes)) %>%
                                       filter(CWTS_ISSN_codes != "") %>%
                                       distinct(CWTS_ID, CWTS_ISSN_codes)


# match all dataframes by the journals' ISSN codes to OpenAlex >= 50% threshold data
ddff_OA_match <- openalex_journals_match %>% left_join(mjl_data_match, by = c("OA_ISSN_codes" = "MJL_ISSN_codes"), relationship = "many-to-many") %>%
                                         left_join(jcr_data_match, by = c("OA_ISSN_codes" = "JCR_ISSN_codes"), relationship = "many-to-many") %>%
                                         left_join(scopus_data_match, by = c("OA_ISSN_codes" = "SCOP_ISSN_codes"), relationship = "many-to-many") %>%
                                         left_join(doaj_data_match, by = c("OA_ISSN_codes" = "DOAJ_ISSN_codes"), relationship = "many-to-many") %>%
                                         left_join(sjr_data_match, by = c("OA_ISSN_codes" = "SJR_ISSN_codes"), relationship = "many-to-many") %>%
                                         left_join(cwts_data_match, by = c("OA_ISSN_codes" = "CWTS_ISSN_codes"), relationship = "many-to-many") %>%
                                         select(OA_ID, MJL_ID, JCR_ID, SCOP_ID, DOAJ_ID, SJR_ID, CWTS_ID, OA_ISSN_codes) %>%
                                         rename(ISSN_code = OA_ISSN_codes)


# remove ISSN code variable, duplicated rows, cases with only one ID, and group rows by the OpenAlex ID
ddff_OA_match <- subset(ddff_OA_match, select = c("OA_ID", "MJL_ID", "JCR_ID", "SCOP_ID", "DOAJ_ID", "SJR_ID", "CWTS_ID"))
ddff_OA_match <- ddff_OA_match %>% distinct()
ddff_OA_match <- ddff_OA_match[rowSums(!is.na(ddff_OA_match)) > 1, ]
ddff_OA_match <- ddff_OA_match %>% group_by(OA_ID) %>%
                                   summarise(across(everything(), ~ unique(na.omit(.))[1]), .groups = "drop")


# match all dataframes by the journals' ISSN codes to OpenAlex < 50% threshold data
ddff_OA_50_match <- openalex_journals_50_match %>% left_join(mjl_data_match, by = c("OA_ISSN_codes" = "MJL_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(jcr_data_match, by = c("OA_ISSN_codes" = "JCR_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(scopus_data_match, by = c("OA_ISSN_codes" = "SCOP_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(doaj_data_match, by = c("OA_ISSN_codes" = "DOAJ_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(sjr_data_match, by = c("OA_ISSN_codes" = "SJR_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(cwts_data_match, by = c("OA_ISSN_codes" = "CWTS_ISSN_codes"), relationship = "many-to-many") %>%
                                               select(OA_ID, MJL_ID, JCR_ID, SCOP_ID, DOAJ_ID, SJR_ID, CWTS_ID, OA_ISSN_codes) %>%
                                               rename(ISSN_code = OA_ISSN_codes)


# remove ISSN code variable, duplicated rows, cases with only one ID, and group rows by the OpenAlex ID
ddff_OA_50_match <- subset(ddff_OA_50_match, select = c("OA_ID", "MJL_ID", "JCR_ID", "SCOP_ID", "DOAJ_ID", "SJR_ID", "CWTS_ID"))
ddff_OA_50_match <- ddff_OA_50_match %>% distinct()
ddff_OA_50_match <- ddff_OA_50_match[rowSums(!is.na(ddff_OA_50_match)) > 1, ]
ddff_OA_50_match <- ddff_OA_50_match %>% group_by(OA_ID) %>%
                                         summarise(across(everything(), ~ unique(na.omit(.))[1]), .groups = "drop")


# store separately the rows where there's only one OpenAlex >= 50 ID and incorporate the corresponding journals' titles
ddff_OA_no_match <- ddff_OA_match[rowSums(!is.na(ddff_OA_match)) == 1, ]
ddff_OA_no_match <- ddff_OA_no_match %>% left_join(select(openalex_journals, OA_ID, OA_ISSN_codes, OA_journal_name), by = "OA_ID")
ddff_OA_no_match <- ddff_OA_no_match %>% select(OA_ID, OA_ISSN_codes, OA_journal_name) ### FALTARÍA AGREGAR LA VARIABLE JN VARIANTS


# store separately the rows where there's only one OpenAlex < 50 ID and incorporate the corresponding journals' titles
ddff_OA_50_no_match <- ddff_OA_50_match[rowSums(!is.na(ddff_OA_50_match)) == 1, ]
ddff_OA_50_no_match <- ddff_OA_50_no_match %>% left_join(select(openalex_journals_50, OA_ID, OA_ISSN_codes, OA_journal_name), by = "OA_ID")
ddff_OA_50_no_match <- ddff_OA_50_no_match %>% select(OA_ID, OA_ISSN_codes, OA_journal_name) ### FALTARÍA AGREGAR LA VARIABLE JN VARIANTS


### DESCARGAR DDFF_IDS_NO_MATCH PARA CAMRYN
### tal vez no sea necesario estandarizar los títulos como sigue acá abajo, eso es parte del paso a paso que podría desarrollar Camryn

# standardize all journal_name variables to ensure accurate comparisons
mjl_data <- mjl_data %>% mutate(journal_name = journal_name %>%
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
## ya tengo los títulos de cada artículo para identificar los toponyms
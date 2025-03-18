library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(readr)
library(stringr)
library(bit64)
options(scipen = 999)


# OpenAlex march 2025 version upload
openalex_journals <- read.csv("~/Desktop/Local.Journals/local_journals_OA2503_journals_functions.csv")

# data mining
openalex_journals$issn <- gsub(",", "; ", openalex_journals$issn)
openalex_journals$issn <- gsub("\\[|\\]", "", openalex_journals$issn)
openalex_journals$issn <- gsub('"', "", openalex_journals$issn)

# replace the subfield, field and domain codes with the corresponding tags from OpenAlex topics
openalex_topics <- readxl::read_excel("~/Desktop/Local.Journals/OAtopics.xlsx")
subfield_lookup <- openalex_topics %>% select(subfield_id, subfield_name) %>% distinct()
field_lookup <- openalex_topics %>% select(field_id, field_name) %>% distinct()
domain_lookup <- openalex_topics %>% select(domain_id, domain_name) %>% distinct()

replace_codes_with_tags <- function(code_column, lookup_table, code_name, tag_name) {
                                    sapply(code_column, function(codes) {
                                      code_list <- strsplit(codes, "; ")[[1]]
                                      tags <- sapply(code_list, function(code) {
                                        match_code <- lookup_table %>% filter(!!sym(code_name) == code)
                                        if (nrow(match_code) > 0) match_code[[tag_name]] else NA})
                                      paste(tags, collapse = "; ")})}

openalex_journals$subfield <- replace_codes_with_tags(openalex_journals$subfield, subfield_lookup, "subfield_id", "subfield_name")
openalex_journals$field <- replace_codes_with_tags(openalex_journals$field, field_lookup, "field_id", "field_name")
openalex_journals$domain <- replace_codes_with_tags(openalex_journals$domain, domain_lookup, "domain_id", "domain_name")


### references local variable
# read files and split into 20 dataframes for processing
references_files <- list.files(path = "~/Desktop/Local.Journals/references_local_variable", pattern = "local_journals_OA2503_references_local_variable_.*", full.names = TRUE)
num_parts <- 20  
num_files <- length(references_files)
chunk_size <- ceiling(num_files / num_parts)
for (i in 1:num_parts) {chunk_files <- references_files[((i - 1) * chunk_size + 1):min(i * chunk_size, num_files)]
                        chunk_files <- chunk_files[!is.na(chunk_files)]
                        chunk_data <- rbindlist(lapply(chunk_files, fread), fill = TRUE)
                        assign(paste0("references_part_", i), chunk_data, envir = .GlobalEnv)
                        rm(chunk_data)
                        gc()}

# compute the refs_count and refs_total variables recurrently per each dataframe partition (references_part_1 until references_part_20)
references_part_1 <- references_part_1 %>% group_by(journal_id, journal_name, country) %>%
                                           mutate(refs_count = n()) %>%
                                           ungroup()
references_part_1 <- within(references_part_1, rm(article_id, reference_id))
references_part_1 <- references_part_1 %>% distinct()
references_part_1 <- references_part_1 %>% group_by(journal_id, journal_name) %>%
                                           mutate(refs_total = sum(refs_count, na.rm = TRUE)) %>%
                                           ungroup()

# merge all parts together without loosing rows
references_local_variable <- rbind(references_part_1, references_part_2, references_part_3, references_part_4, references_part_5, references_part_6,
                                   references_part_7, references_part_8, references_part_9, references_part_10, references_part_11, references_part_12,
                                   references_part_13, references_part_14, references_part_15, references_part_16, references_part_17, references_part_18,
                                   references_part_19, references_part_20, fill = TRUE)

# compute variables refs_count, refs_total and refs_prop per unique combination of journal and its most referenced country
references_local_variable <- references_local_variable %>% group_by(journal_id, journal_name, country) %>%
                                                           summarise(refs_count = sum(refs_count, na.rm = TRUE), .groups = "drop")
references_local_variable <- references_local_variable %>% filter(!(journal_id == 1 & journal_name == "TRUE" & country == "TRUE" & refs_count == 1))
references_local_variable <- references_local_variable %>% group_by(journal_id, journal_name) %>%
                                                           mutate(refs_total = sum(refs_count, na.rm = TRUE)) %>%
                                                           ungroup()
references_local_variable <- references_local_variable %>% group_by(journal_id, journal_name) %>%
                                                           filter(refs_count == max(refs_count)) %>%
                                                           ungroup()
references_local_variable <- references_local_variable %>% mutate(refs_prop = round(refs_count / refs_total, 2))


# incorporate cits local variable
### sacar código del otro archivo










# MJL upload and data mining LANGUAGE
mjl_journals <- read.csv("~/Desktop/Local.Journals/MJL.csv")
mjl_journals <- mjl_journals %>% distinct()
mjl_journals <- mjl_journals %>% group_by(journal_name, issn, eissn) %>%
                                 summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")

# Scopus upload and data mining LANG
scopus_journals <- readxl::read_excel("~/Desktop/Local.Journals/Scopus.xlsx")
scopus_journals <- scopus_journals %>% group_by(journal_name, issn, eissn) %>%
                                       summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
scopus_journals[scopus_journals == "NA"] <- NA
scopus_journals$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_journals$issn)
scopus_journals$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_journals$eissn)


# DOAJ upload and data mining LANG
doaj_journals <- read.csv("~/Desktop/Local.Journals/DOAJ.csv")
doaj_journals$issn <- ifelse(!is.na(doaj_journals$issn) & doaj_journals$issn != "", 
                             str_pad(doaj_journals$issn, width = 8, side = "left", pad = "0"), 
                             doaj_journals$issn)
doaj_journals$eissn <- ifelse(!is.na(doaj_journals$eissn) & doaj_journals$eissn != "", 
                              str_pad(doaj_journals$eissn, width = 8, side = "left", pad = "0"), 
                              doaj_journals$eissn)
doaj_journals$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_journals$issn)
doaj_journals$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_journals$eissn)









# add unique identifiers to each dataframe
openalex_journals <- openalex_journals %>% mutate(OA_ID = paste0("OA", row_number())) %>%
                                           relocate(OA_ID)
openalex_journals_50 <- openalex_journals_50 %>% mutate(OA_ID = paste0("OA", row_number())) %>%
                                                 relocate(OA_ID)
mjl_journals <- mjl_journals %>% mutate(MJL_ID = paste0("MJL", row_number())) %>%
                                 relocate(MJL_ID)
jcr_journals <- jcr_journals %>% mutate(JCR_ID = paste0("JCR", row_number())) %>%
                                 relocate(JCR_ID)
scopus_journals <- scopus_journals %>% mutate(SCOP_ID = paste0("SCOP", row_number())) %>%
                                       relocate(SCOP_ID)
doaj_journals <- doaj_journals %>% mutate(DOAJ_ID = paste0("DOAJ", row_number())) %>%
                                   relocate(DOAJ_ID)
sjr_journals <- sjr_journals %>% mutate(SJR_ID = paste0("SJR", row_number())) %>%
                                 relocate(SJR_ID)
cwts_journals <- cwts_journals %>% mutate(CWTS_ID = paste0("CWTS", row_number())) %>%
                                   relocate(CWTS_ID)


# create variable to unify all ISSN codes per dataframe
openalex_journals$issn_codes <- apply(openalex_journals[, c("issn", "issn_l")], 1, function(x) {
                                      unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                      paste(unique_values, collapse = ";")})
openalex_journals_50$issn_codes <- apply(openalex_journals_50[, c("issn", "issn_l")], 1, function(x) {
                                         unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                         paste(unique_values, collapse = ";")})
mjl_journals$issn_codes <- apply(mjl_journals[, c("issn", "eissn")], 1, function(x) {
                                 unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                 paste(unique_values, collapse = ";")})
jcr_journals$issn_codes <- apply(jcr_journals[, c("issn", "eissn")], 1, function(x) {
                                 unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                 paste(unique_values, collapse = ";")})
scopus_journals$issn_codes <- apply(scopus_journals[, c("issn", "eissn")], 1, function(x) {
                                    unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                    paste(unique_values, collapse = ";")})
doaj_journals$issn_codes <- apply(doaj_journals[, c("issn", "eissn")], 1, function(x) {
                                  unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                  paste(unique_values, collapse = ";")})
sjr_journals$issn_codes <- apply(sjr_journals[, c("issn", "eissn")], 1, function(x) {
                                 unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                 paste(unique_values, collapse = ";")})


# create variable to unify all journal names variants in Scopus
scopus_journals <- scopus_journals %>% rowwise() %>%
                                       mutate(journal_name_variants = paste(unique(unlist(strsplit(c_across(`Related Title 1`:`Other Related Title 4`)[!is.na(c_across(`Related Title 1`:`Other Related Title 4`))], ";"))), collapse = ";")) %>%
                                       ungroup()


# select, rename and organize all variables per dataframe
openalex_journals <- openalex_journals %>% select(OA_ID, OA_source_ID = journal_id, OA_ISSN_codes = issn_codes, OA_journal_name = journal_name, OA_journal_name_variants = abbreviated_title,
                                                  OA_publisher = publisher, OA_publisher_country = country_code, OA_topics = topic_display_name, OA_primary_topics = primary_topic_display_name,
                                                  OA_subfields = subfield, OA_fields = field, OA_domains = domain, OA_open_access = is_oa, OA_APC_prices = APC_prices, OA_website = homepage_url,
                                                  OA_total_articles = works_count, OA_total_citations = cited_by_count)
openalex_journals_50 <- openalex_journals_50 %>% select(OA_ID, OA_source_ID = journal_id, OA_ISSN_codes = issn_codes, OA_journal_name = journal_name)

mjl_journals <- mjl_journals %>% select(MJL_ID, MJL_ISSN_codes = issn_codes, MJL_journal_name = journal_name, MJL_publisher = Publisher.name,
                                        MJL_publisher_country = Publisher.address, MJL_language = Languages, MJL_categories = Web.of.Science.Categories)

jcr_journals <- jcr_journals %>% select(JCR_ID, JCR_ISSN_codes = issn_codes, JCR_journal_name = journal_name, JCR_journal_name_variants = `JCR Abbreviation`,
                                        JCR_publisher = Publisher, JCR_categories = Category, JCR_edition = Edition, JCR_JCI_2023 = `2023 JCI`,
                                        JCR_JCI_rank = `JCI Rank`, JCR_JCI_quartile = `JCI Quartile`, JCR_JCI_percentile = `JCI Percentile`,
                                        JCR_JIF_2023 = `2023 JIF`, JCR_JIF_rank = `JIF Rank`, JCR_JIF_quartile = `JIF Quartile`, JCR_JIF_percentile = `JIF Percentile`,
                                        JCR_JIF_5_years = `5 Year JIF`, JCR_JIF_5_years_quartile = `5 Year JIF Quartile`, JCR_JIF_no_self_cites = `JIF Without Self Cites`,
                                        JCR_immediacy_index = `Immediacy Index`, JCR_AIS = `Article Influence Score`, JCR_AIS_quartile = `AIS Quartile`,
                                        JCR_eigenfactor = Eigenfactor, JCR_eigenfactor_normalized = `Normalized Eigenfactor`, JCR_citing_half_life = `Citing Half-Life`,
                                        JCR_cited_half_life = `Cited Half-Life`, JCR_percent_articles_citable_items = `% of Articles in Citable items`,
                                        JCR_percent_citable_open_access = `% of Citable OA`, JCR_total_articles = `Total Articles`, JCR_total_citations = `Total Citations`, JCR_citable_articles = `Citable Items`)

scopus_journals <- scopus_journals %>% select(SCOP_ID, SCOP_source_ID = `Sourcerecord ID`, SCOP_ISSN_codes = issn_codes, SCOP_journal_name = journal_name,
                                              SCOP_journal_name_variants = journal_name_variants, SCOP_publisher = Publisher, SCOP_main_publisher = `Publisher Imprints Grouped to Main Publisher`,
                                              SCOP_language = language, SCOP_ASJC_codes = `All Science Journal Classification Codes (ASJC)`, SCOP_coverage = Coverage, SCOP_open_access = `Open Access Status`,
                                              SCOP_discontinued = `Titles Discontinued by Scopus Due to Quality Issues`, SCOP_medline_sourced = `Medline-sourced Title? (See additional details under separate tab.)`)

doaj_journals <- doaj_journals %>% select(DOAJ_ID, DOAJ_ISSN_codes = issn_codes, DOAJ_continues_ISSN = Continues, DOAJ_continued_by_ISSN = Continued.By,
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

sjr_journals <- sjr_journals %>% select(SJR_ID, SJR_source_ID = Sourceid, SJR_ISSN_codes = issn_codes, SJR_journal_name = journal_name, SJR_publisher = Publisher,
                                        SJR_publisher_country = Country, SJR_region = Region, SJR_categories = Categories, SJR_areas = Areas, SJR_coverage = Coverage,
                                        SJR_SJR = SJR, SJR_rank = Rank, SJR_best_quartile = `SJR Best Quartile`, SJR_h_index = `H index`, SJR_total_articles_2023 = `Total Docs. (2023)`,
                                        SJR_total_articles_3_years = `Total Docs. (3years)`, SJR_total_references = `Total Refs.`, SJR_references_per_articles = `Ref. / Doc.`,
                                        SJR_total_citations_3_years = `Total Cites (3years)`, SJR_citations_per_articles_2_years = `Cites / Doc. (2years)`,
                                        SJR_citable_articles_3_years = `Citable Docs. (3years)`, SJR_percent_female = `%Female`, SJR_SDG = SDG, SJR_overton = Overton)

cwts_journals <- cwts_journals %>% select(CWTS_ID, CWTS_ISSN_codes = issn_codes, CWTS_journal_name = `Source title`, CWTS_percent_self_citations = `% self cit`,
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

mjl_journals_match <- subset(mjl_journals, select = c("MJL_ID", "MJL_ISSN_codes"))
mjl_journals_match <- mjl_journals_match %>% mutate(MJL_ISSN_codes = strsplit(as.character(MJL_ISSN_codes), ";")) %>%
                                             unnest(MJL_ISSN_codes) %>%
                                             mutate(MJL_ISSN_codes = gsub("\\s+", "", MJL_ISSN_codes)) %>%
                                             filter(MJL_ISSN_codes != "") %>%
                                             distinct(MJL_ID, MJL_ISSN_codes)

jcr_journals_match <- subset(jcr_journals, select = c("JCR_ID", "JCR_ISSN_codes"))
jcr_journals_match <- jcr_journals_match %>% mutate(JCR_ISSN_codes = strsplit(as.character(JCR_ISSN_codes), ";")) %>%
                                             unnest(JCR_ISSN_codes) %>%
                                             mutate(JCR_ISSN_codes = gsub("\\s+", "", JCR_ISSN_codes)) %>%
                                             filter(JCR_ISSN_codes != "") %>%
                                             distinct(JCR_ID, JCR_ISSN_codes)

scopus_journals_match <- subset(scopus_journals, select = c("SCOP_ID", "SCOP_ISSN_codes"))
scopus_journals_match <- scopus_journals_match %>% mutate(SCOP_ISSN_codes = strsplit(as.character(SCOP_ISSN_codes), ";")) %>%
                                                   unnest(SCOP_ISSN_codes) %>%
                                                   mutate(SCOP_ISSN_codes = gsub("\\s+", "", SCOP_ISSN_codes)) %>%
                                                   filter(SCOP_ISSN_codes != "") %>%
                                                   distinct(SCOP_ID, SCOP_ISSN_codes)

doaj_journals_match <- subset(doaj_journals, select = c("DOAJ_ID", "DOAJ_ISSN_codes"))
doaj_journals_match <- doaj_journals_match %>% mutate(DOAJ_ISSN_codes = strsplit(as.character(DOAJ_ISSN_codes), ";")) %>%
                                               unnest(DOAJ_ISSN_codes) %>%
                                               mutate(DOAJ_ISSN_codes = gsub("\\s+", "", DOAJ_ISSN_codes)) %>%
                                               filter(DOAJ_ISSN_codes != "") %>%
                                               distinct(DOAJ_ID, DOAJ_ISSN_codes)

sjr_journals_match <- subset(sjr_journals, select = c("SJR_ID", "SJR_ISSN_codes"))
sjr_journals_match <- sjr_journals_match %>% mutate(SJR_ISSN_codes = strsplit(as.character(SJR_ISSN_codes), ";")) %>%
                                             unnest(SJR_ISSN_codes) %>%
                                             mutate(SJR_ISSN_codes = gsub("\\s+", "", SJR_ISSN_codes)) %>%
                                             filter(SJR_ISSN_codes != "") %>%
                                             distinct(SJR_ID, SJR_ISSN_codes)

cwts_journals_match <- subset(cwts_journals, select = c("CWTS_ID", "CWTS_ISSN_codes"))
cwts_journals_match <- cwts_journals_match %>% mutate(CWTS_ISSN_codes = strsplit(as.character(CWTS_ISSN_codes), ";")) %>%
                                               unnest(CWTS_ISSN_codes) %>%
                                               mutate(CWTS_ISSN_codes = gsub("\\s+", "", CWTS_ISSN_codes)) %>%
                                               filter(CWTS_ISSN_codes != "") %>%
                                               distinct(CWTS_ID, CWTS_ISSN_codes)


# match all dataframes by the journals' ISSN codes to OpenAlex >= 50% threshold data
ddff_OA_ISSNs_match <- openalex_journals_match %>% left_join(mjl_journals_match, by = c("OA_ISSN_codes" = "MJL_ISSN_codes"), relationship = "many-to-many") %>%
                                                   left_join(jcr_journals_match, by = c("OA_ISSN_codes" = "JCR_ISSN_codes"), relationship = "many-to-many") %>%
                                                   left_join(scopus_journals_match, by = c("OA_ISSN_codes" = "SCOP_ISSN_codes"), relationship = "many-to-many") %>%
                                                   left_join(doaj_journals_match, by = c("OA_ISSN_codes" = "DOAJ_ISSN_codes"), relationship = "many-to-many") %>%
                                                   left_join(sjr_journals_match, by = c("OA_ISSN_codes" = "SJR_ISSN_codes"), relationship = "many-to-many") %>%
                                                   left_join(cwts_journals_match, by = c("OA_ISSN_codes" = "CWTS_ISSN_codes"), relationship = "many-to-many") %>%
                                                   select(OA_ID, MJL_ID, JCR_ID, SCOP_ID, DOAJ_ID, SJR_ID, CWTS_ID, OA_ISSN_codes) %>%
                                                   rename(ISSN_code = OA_ISSN_codes)


# remove ISSN code variable, duplicated rows, cases with only one ID, and group rows by the OpenAlex ID
ddff_OA_ISSNs_match <- subset(ddff_OA_ISSNs_match, select = c("OA_ID", "MJL_ID", "JCR_ID", "SCOP_ID", "DOAJ_ID", "SJR_ID", "CWTS_ID"))
ddff_OA_ISSNs_match <- ddff_OA_ISSNs_match %>% distinct()
ddff_OA_ISSNs_match <- ddff_OA_ISSNs_match[rowSums(!is.na(ddff_OA_ISSNs_match)) > 1, ]
ddff_OA_ISSNs_match <- ddff_OA_ISSNs_match %>% group_by(OA_ID) %>%
                                               summarise(across(everything(), ~ unique(na.omit(.))[1]), .groups = "drop")


# store separately the rows where there's only one OpenAlex >= 50 ID and incorporate the corresponding journals' titles
ddff_OA_ISSNs_no_match <- ddff_OA_ISSNs_match[rowSums(!is.na(ddff_OA_ISSNs_match)) == 1, ]
ddff_OA_ISSNs_no_match <- ddff_OA_ISSNs_no_match %>% left_join(select(openalex_journals, OA_ID, OA_ISSN_codes, OA_journal_name, OA_journal_name_variants), by = "OA_ID")
ddff_OA_ISSNs_no_match <- ddff_OA_ISSNs_no_match %>% select(OA_ID, OA_ISSN_codes, OA_journal_name, OA_journal_name_variants)
write.csv(ddff_OA_ISSNs_no_match, "~/Desktop/Local.Journals/OA_titles_matching.csv")


# isolate the journals from the rest of the ddbb that don't match with OpenAlex through their ISSN codes in order to match via titles
mjl_journals_no_match <- mjl_journals %>% anti_join(ddff_OA_ISSNs_match, by = "MJL_ID") %>%
                                          select(MJL_ID, MJL_ISSN_codes, MJL_journal_name)
write.csv(mjl_journals_no_match, "~/Desktop/Local.Journals/MJL_titles_matching.csv")

jcr_journals_no_match <- jcr_journals %>% anti_join(ddff_OA_ISSNs_match, by = "JCR_ID") %>%
                                          select(JCR_ID, JCR_ISSN_codes, JCR_journal_name)
write.csv(jcr_journals_no_match, "~/Desktop/Local.Journals/JCR_titles_matching.csv")

scopus_journals_no_match <- scopus_journals %>% anti_join(ddff_OA_ISSNs_match, by = "SCOP_ID") %>%
                                                select(SCOP_ID, SCOP_ISSN_codes, SCOP_journal_name)
write.csv(scopus_journals_no_match, "~/Desktop/Local.Journals/SCOP_titles_matching.csv")

doaj_journals_no_match <- doaj_journals %>% anti_join(ddff_OA_ISSNs_match, by = "DOAJ_ID") %>%
                                            select(DOAJ_ID, DOAJ_ISSN_codes, DOAJ_journal_name)
write.csv(doaj_journals_no_match, "~/Desktop/Local.Journals/DOAJ_titles_matching.csv")

sjr_journals_no_match <- sjr_journals %>% anti_join(ddff_OA_ISSNs_match, by = "SJR_ID") %>%
                                          select(SJR_ID, SJR_ISSN_codes, SJR_journal_name)
write.csv(sjr_journals_no_match, "~/Desktop/Local.Journals/SJR_titles_matching.csv")

cwts_journals_no_match <- cwts_journals %>% anti_join(ddff_OA_ISSNs_match, by = "CWTS_ID") %>%
                                            select(CWTS_ID, CWTS_ISSN_codes, CWTS_journal_name)
write.csv(cwts_journals_no_match, "~/Desktop/Local.Journals/CWTS_titles_matching.csv")


# match all dataframes by the journals' ISSN codes to OpenAlex < 50% threshold data
ddff_OA_50_ISSNs_match <- openalex_journals_50_match %>% left_join(mjl_journals_match, by = c("OA_ISSN_codes" = "MJL_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(jcr_journals_match, by = c("OA_ISSN_codes" = "JCR_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(scopus_journals_match, by = c("OA_ISSN_codes" = "SCOP_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(doaj_journals_match, by = c("OA_ISSN_codes" = "DOAJ_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(sjr_journals_match, by = c("OA_ISSN_codes" = "SJR_ISSN_codes"), relationship = "many-to-many") %>%
                                               left_join(cwts_journals_match, by = c("OA_ISSN_codes" = "CWTS_ISSN_codes"), relationship = "many-to-many") %>%
                                               select(OA_ID, MJL_ID, JCR_ID, SCOP_ID, DOAJ_ID, SJR_ID, CWTS_ID, OA_ISSN_codes) %>%
                                               rename(ISSN_code = OA_ISSN_codes)


# remove ISSN code variable, duplicated rows, cases with only one ID, and group rows by the OpenAlex ID
ddff_OA_50_ISSNs_match <- subset(ddff_OA_50_ISSNs_match, select = c("OA_ID", "MJL_ID", "JCR_ID", "SCOP_ID", "DOAJ_ID", "SJR_ID", "CWTS_ID"))
ddff_OA_50_ISSNs_match <- ddff_OA_50_ISSNs_match %>% distinct()
ddff_OA_50_ISSNs_match <- ddff_OA_50_ISSNs_match[rowSums(!is.na(ddff_OA_50_ISSNs_match)) > 1, ]
ddff_OA_50_ISSNs_match <- ddff_OA_50_ISSNs_match %>% group_by(OA_ID) %>%
                                                     summarise(across(everything(), ~ unique(na.omit(.))[1]), .groups = "drop")


# store separately the rows where there's only one OpenAlex < 50 ID and incorporate the corresponding journals' titles
#ddff_OA_50_ISSNs_no_match <- ddff_OA_50_ISSNs_match[rowSums(!is.na(ddff_OA_50_ISSNs_match)) == 1, ]
#ddff_OA_50_ISSNs_no_match <- ddff_OA_50_ISSNs_no_match %>% left_join(select(openalex_journals_50, OA_ID, OA_ISSN_codes, OA_journal_name), by = "OA_ID")
#ddff_OA_50_ISSNs_no_match <- ddff_OA_50_ISSNs_no_match %>% select(OA_ID, OA_ISSN_codes, OA_journal_name) ### FALTARÍA AGREGAR LA VARIABLE JN VARIANTS


### MEGA MERGE by ISSNs
ddff_OA_megamerge <- ddff_OA_ISSNs_match %>% left_join(openalex_journals, by = "OA_ID") %>%
                                             left_join(mjl_journals, by = "MJL_ID") %>%
                                             left_join(jcr_journals, by = "JCR_ID") %>%
                                             left_join(scopus_journals, by = "SCOP_ID") %>%
                                             left_join(doaj_journals, by = "DOAJ_ID") %>%
                                             left_join(sjr_journals, by = "SJR_ID") %>%
                                             left_join(cwts_journals, by = "CWTS_ID")

ddff_OA_megamerge <- ddff_OA_megamerge %>% select(OA_ID, MJL_ID, JCR_ID, SCOP_ID, DOAJ_ID, SJR_ID, CWTS_ID,
                                                  OA_source_ID, SCOP_source_ID, SJR_source_ID,
                                                  OA_ISSN_codes, MJL_ISSN_codes, JCR_ISSN_codes, SCOP_ISSN_codes, DOAJ_ISSN_codes, DOAJ_continues_ISSN, DOAJ_continued_by_ISSN, SJR_ISSN_codes, CWTS_ISSN_codes,
                                                  OA_journal_name, MJL_journal_name, JCR_journal_name, SCOP_journal_name, DOAJ_journal_name, SJR_journal_name, CWTS_journal_name,
                                                  OA_journal_name_variants, JCR_journal_name_variants, SCOP_journal_name_variants, DOAJ_journal_name_variants,
                                                  OA_publisher, MJL_publisher, JCR_publisher, SCOP_publisher, DOAJ_publisher, SJR_publisher,
                                                  OA_publisher_country, MJL_publisher_country, DOAJ_publisher_country, SJR_publisher_country,
                                                  SCOP_main_publisher, DOAJ_other_organization, DOAJ_other_organization_country, SJR_region,
                                                  MJL_language, SCOP_language, DOAJ_language,
                                                  OA_topics, OA_primary_topics, OA_subfields, OA_fields, OA_domains, MJL_categories, JCR_categories, SJR_categories, SJR_areas, SCOP_ASJC_codes, DOAJ_LCC_codes, DOAJ_subjects, DOAJ_keywords,
                                                  JCR_edition, SCOP_coverage, SJR_coverage,
                                                  OA_open_access, SCOP_open_access, DOAJ_open_access, DOAJ_open_license_since, DOAJ_license, DOAJ_license_attributes, DOAJ_author_unrestricted_rights, DOAJ_open_citations, DOAJ_machine_readable_license,
                                                  DOAJ_review_process, DOAJ_average_weeks_for_publication, OA_APC_prices, DOAJ_APC_prices, DOAJ_other_fees, DOAJ_waiver_policy, DOAJ_deposit_policy, DOAJ_plagiarism_policy,
                                                  DOAJ_persistent_identifiers, DOAJ_preservation_services, DOAJ_national_library_preservation_services, SCOP_medline_sourced, OA_website, DOAJ_website,
                                                  JCR_JCI_2023, JCR_JCI_rank, JCR_JCI_quartile, JCR_JCI_percentile, JCR_JIF_2023, JCR_JIF_rank, JCR_JIF_quartile, JCR_JIF_percentile, JCR_JIF_5_years, JCR_JIF_5_years_quartile, JCR_JIF_no_self_cites,
                                                  JCR_immediacy_index, JCR_AIS, JCR_AIS_quartile, JCR_eigenfactor, JCR_eigenfactor_normalized, JCR_citing_half_life, JCR_cited_half_life, JCR_percent_articles_citable_items, JCR_percent_citable_open_access,
                                                  SJR_SJR, SJR_rank, SJR_best_quartile, SJR_h_index,
                                                  OA_total_articles, JCR_total_articles, SJR_total_articles_2023, SJR_total_articles_3_years, SJR_total_references, SJR_references_per_articles, OA_total_citations, JCR_total_citations, SJR_total_citations_3_years, SJR_citations_per_articles_2_years, JCR_citable_articles, SJR_citable_articles_3_years,
                                                  CWTS_percent_self_citations, CWTS_SNIP, CWTS_SNIP_lower_bound, CWTS_SNIP_upper_bound, CWTS_IPP, CWTS_IPP_lower_bound, CWTS_IPP_upper_bound, SJR_percent_female, SJR_SDG, SJR_overton)

ddff_OA_megamerge <- ddff_OA_megamerge %>% mutate(across(where(is.character), ~ na_if(.x, "")))
write.csv(ddff_OA_megamerge, "~/Desktop/Local.Journals/mega_merge.csv")






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


### LOCAL VARIABLE DATABASES COMPUTING
ddbb <- read.csv("~/Desktop/Local.Journals/local_variable_ddbb.csv")[, c("journal_id", "journal_name", "issn", "eissn")]


### LOCAL VARIABLE TOPONYMS COMPUTING
## ya tengo los títulos de cada artículo para identificar los toponyms


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
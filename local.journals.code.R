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

# remove duplicates and store second issn code in issn_l 
openalex_journals <- openalex_journals %>% mutate(issn_cleaned = str_split(issn, "; "),
                                                  issn_l = if_else(issn_l %in% unlist(issn_cleaned), NA_character_, issn_l)) %>%
                                                  select(-issn_cleaned)

openalex_journals <- openalex_journals %>% mutate(issn_split = str_split(issn, "; "),
                                                  issn_l = if_else(lengths(issn_split) > 1, map_chr(issn_split, ~ .x[2]), issn_l),
                                                  issn = map_chr(issn_split, ~ .x[1])) %>%
                                                  select(-issn_split)

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

# incorporate these refs variables to the main journals dataframe
openalex_journals <- openalex_journals %>% left_join(references_local_variable %>%
                                                       mutate(journal_id = as.numeric(journal_id)) %>%
                                                       rename(refs_country = country),
                                                     by = c("journal_id", "journal_name"))


### citations local variable
# read files
citations_local_variable <- list.files(path = "~/Desktop/Local.Journals/citations_local_variable", pattern = "local_journals_OA2503_citations_local_variable_.*", full.names = TRUE)
citations_local_variable <- rbindlist(lapply(citations_local_variable, fread, sep = ","), fill = TRUE)

# compute variables cits_count, cits_total and cits_prop per unique combination of journal and its most citing country
citations_local_variable <- citations_local_variable %>% group_by(journal_id, journal_name, country) %>%
                                                         mutate(cits_count = n()) %>%
                                                         ungroup()
citations_local_variable <- within(citations_local_variable, rm(article_id, citing_work_id))
citations_local_variable <- citations_local_variable %>% distinct()
citations_local_variable <- citations_local_variable %>% group_by(journal_id, journal_name) %>%
                                                         mutate(cits_total = sum(cits_count, na.rm = TRUE)) %>%
                                                         ungroup()
citations_local_variable <- citations_local_variable %>% group_by(journal_id, journal_name) %>%
                                                         filter(cits_count == max(cits_count)) %>%
                                                         ungroup()
citations_local_variable <- citations_local_variable %>% mutate(cits_prop = round(cits_count / cits_total, 2))

# incorporate these cits variables to the main journals dataframe
openalex_journals <- openalex_journals %>% left_join(citations_local_variable %>%
                                                       mutate(journal_id = as.numeric(journal_id)) %>%
                                                       rename(cits_country = country),
                                                     by = c("journal_id", "journal_name"))


### languages local variable
# MJL language data upload (mainstream = English & Multi-Language)
mjl_language <- read.csv("~/Desktop/Local.Journals/languages_local_variable/MJL.csv") %>% select(issn, eissn, Languages) %>%
                                                                                          rename(mjl_lang = Languages)
mjl_language <- mjl_language %>% distinct(issn, eissn, mjl_lang, .keep_all = TRUE) %>%
                                 filter(mjl_lang != "" & !is.na(mjl_lang))

# Scopus language data upload and mining
scopus_language <- readxl::read_excel("~/Desktop/Local.Journals/languages_local_variable/Scopus.xlsx") %>% select(issn, eissn, language) %>%
                                                                                                           rename(scopus_lang = language)
scopus_language$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_language$issn)
scopus_language$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_language$eissn)
scopus_language <- scopus_language %>% distinct(issn, eissn, scopus_lang, .keep_all = TRUE) %>%
                                       filter(scopus_lang != "" & !is.na(scopus_lang))

# DOAJ language data upload and mining
doaj_language <- read.csv("~/Desktop/Local.Journals/languages_local_variable/DOAJ.csv") %>% select(issn, eissn, language) %>%
                                                                                            rename(doaj_lang = language)
doaj_language$issn <- ifelse(!is.na(doaj_language$issn) & doaj_language$issn != "", 
                             str_pad(doaj_language$issn, width = 8, side = "left", pad = "0"), 
                             doaj_language$issn)
doaj_language$eissn <- ifelse(!is.na(doaj_language$eissn) & doaj_language$eissn != "", 
                              str_pad(doaj_language$eissn, width = 8, side = "left", pad = "0"), 
                              doaj_language$eissn)
doaj_language$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_language$issn)
doaj_language$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_language$eissn)
doaj_language <- doaj_language %>% distinct(issn, eissn, doaj_lang, .keep_all = TRUE) %>%
                                   filter(doaj_lang != "" & !is.na(doaj_lang))

# match to OpenAlex by issn and issn_l
merge_language_data <- function(main_df, lang_df, lang_var) {main_df %>%
                                left_join(lang_df, by = c("issn" = "issn")) %>%
                                left_join(lang_df, by = c("issn" = "eissn")) %>%
                                left_join(lang_df, by = c("issn_l" = "issn")) %>%
                                left_join(lang_df, by = c("issn_l" = "eissn")) %>%
                                mutate(!!lang_var := coalesce(!!sym(paste0(lang_var, ".x")), 
                                       !!sym(paste0(lang_var, ".y")),
                                       !!sym(paste0(lang_var, ".x.x")),
                                       !!sym(paste0(lang_var, ".y.y")))) %>%
                                select(-ends_with(".x"), -ends_with(".y"))}

openalex_journals <- merge_language_data(openalex_journals, mjl_language, "mjl_lang")
openalex_journals <- merge_language_data(openalex_journals, scopus_language, "scopus_lang")
openalex_journals <- merge_language_data(openalex_journals, doaj_language, "doaj_lang")








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
mjl_journals$issn_codes <- apply(mjl_journals[, c("issn", "eissn")], 1, function(x) {
                                 unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                 paste(unique_values, collapse = ";")})
scopus_journals$issn_codes <- apply(scopus_journals[, c("issn", "eissn")], 1, function(x) {
                                    unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                    paste(unique_values, collapse = ";")})
doaj_journals$issn_codes <- apply(doaj_journals[, c("issn", "eissn")], 1, function(x) {
                                  unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                  paste(unique_values, collapse = ";")})





# prepare dataframes for matching
openalex_journals_match <- subset(openalex_journals, select = c("OA_ID", "OA_ISSN_codes"))
openalex_journals_match <- openalex_journals_match %>% mutate(OA_ISSN_codes = strsplit(as.character(OA_ISSN_codes), ";")) %>%
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

scopus_journals_no_match <- scopus_journals %>% anti_join(ddff_OA_ISSNs_match, by = "SCOP_ID") %>%
                                                select(SCOP_ID, SCOP_ISSN_codes, SCOP_journal_name)
write.csv(scopus_journals_no_match, "~/Desktop/Local.Journals/SCOP_titles_matching.csv")

doaj_journals_no_match <- doaj_journals %>% anti_join(ddff_OA_ISSNs_match, by = "DOAJ_ID") %>%
                                            select(DOAJ_ID, DOAJ_ISSN_codes, DOAJ_journal_name)
write.csv(doaj_journals_no_match, "~/Desktop/Local.Journals/DOAJ_titles_matching.csv")




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


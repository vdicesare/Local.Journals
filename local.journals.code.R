library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(readr)
library(stringr)
library(bit64)
library(ggplot2)
library(eulerr)
library(countrycode)
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


### languages local variable
# MJL language data upload and mining (mainstream = English & Multi-Language)
mjl_language <- read.csv("~/Desktop/Local.Journals/languages_local_variable/MJL.csv") %>% select(issn, eissn, Languages) %>%
  rename(mjl_lang = Languages)
mjl_language <- mjl_language %>% distinct(issn, eissn, mjl_lang, .keep_all = TRUE) %>%
  filter(mjl_lang != "" & !is.na(mjl_lang))
mjl_language <- mjl_language %>% mutate(eissn = if_else(eissn %in% issn, NA_character_, eissn))
mjl_language <- mjl_language %>% mutate(issn = na_if(trimws(issn), ""))

# Scopus language data upload and mining (mainstream = ENG)
scopus_language <- readxl::read_excel("~/Desktop/Local.Journals/languages_local_variable/Scopus.xlsx") %>% select(issn, eissn, language) %>%
  rename(scopus_lang = language)
scopus_language$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_language$issn)
scopus_language$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_language$eissn)
scopus_language <- scopus_language %>% distinct(issn, eissn, scopus_lang, .keep_all = TRUE) %>%
  filter(scopus_lang != "" & !is.na(scopus_lang))

# DOAJ language data upload and mining (mainstream = English)
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
doaj_language <- doaj_language %>% mutate(issn = na_if(trimws(issn), ""))

# add unique identifiers to each dataframe
openalex_journals <- openalex_journals %>% mutate(OA_ID = paste0("OA", row_number())) %>%
                                           relocate(OA_ID)
mjl_language <- mjl_language %>% mutate(MJL_ID = paste0("MJL", row_number())) %>%
                                 relocate(MJL_ID)
scopus_language <- scopus_language %>% mutate(SCOP_ID = paste0("SCOP", row_number())) %>%
                                       relocate(SCOP_ID)
doaj_language <- doaj_language %>% mutate(DOAJ_ID = paste0("DOAJ", row_number())) %>%
                                   relocate(DOAJ_ID)

# create variable to unify all ISSN codes per dataframe
openalex_journals$OA_issn <- apply(openalex_journals[, c("issn", "issn_l")], 1, function(x) {
                                   unique_values <- unique(na.omit(x))
                                   paste(unique_values, collapse = ";")})
mjl_language$MJL_issn <- apply(mjl_language[, c("issn", "eissn")], 1, function(x) {
                               unique_values <- unique(na.omit(x))
                               paste(unique_values, collapse = ";")})
scopus_language$SCOP_issn <- apply(scopus_language[, c("issn", "eissn")], 1, function(x) {
                                   unique_values <- unique(na.omit(x))
                                   paste(unique_values, collapse = ";")})
doaj_language$DOAJ_issn <- apply(doaj_language[, c("issn", "eissn")], 1, function(x) {
                                 unique_values <- unique(na.omit(x))
                                 paste(unique_values, collapse = ";")})

# prepare dataframes for matching
openalex_journals_match <- subset(openalex_journals, select = c("OA_ID", "OA_issn"))
openalex_journals_match <- openalex_journals_match %>% mutate(OA_issn = strsplit(as.character(OA_issn), ";")) %>%
                                                       unnest(OA_issn) %>%
                                                       mutate(OA_issn = gsub("\\s+", "", OA_issn)) %>%
                                                       filter(OA_issn != "") %>%
                                                       distinct(OA_ID, OA_issn)

mjl_language_match <- subset(mjl_language, select = c("MJL_ID", "MJL_issn"))
mjl_language_match <- mjl_language_match %>% mutate(MJL_issn = strsplit(as.character(MJL_issn), ";")) %>%
                                             unnest(MJL_issn) %>%
                                             mutate(MJL_issn = gsub("\\s+", "", MJL_issn)) %>%
                                             filter(MJL_issn != "") %>%
                                             distinct(MJL_ID, MJL_issn)

scopus_language_match <- subset(scopus_language, select = c("SCOP_ID", "SCOP_issn"))
scopus_language_match <- scopus_language_match %>% mutate(SCOP_issn = strsplit(as.character(SCOP_issn), ";")) %>%
                                                   unnest(SCOP_issn) %>%
                                                   mutate(SCOP_issn = gsub("\\s+", "", SCOP_issn)) %>%
                                                   filter(SCOP_issn != "") %>%
                                                   distinct(SCOP_ID, SCOP_issn)

doaj_language_match <- subset(doaj_language, select = c("DOAJ_ID", "DOAJ_issn"))
doaj_language_match <- doaj_language_match %>% mutate(DOAJ_issn = strsplit(as.character(DOAJ_issn), ";")) %>%
                                               unnest(DOAJ_issn) %>%
                                               mutate(DOAJ_issn = gsub("\\s+", "", DOAJ_issn)) %>%
                                               filter(DOAJ_issn != "") %>%
                                               distinct(DOAJ_ID, DOAJ_issn)

# match all dataframes by the journals' ISSN codes to OpenAlex
ddff_ISSNs_match <- openalex_journals_match %>% left_join(mjl_language_match, by = c("OA_issn" = "MJL_issn"), relationship = "many-to-many") %>%
                                                left_join(scopus_language_match, by = c("OA_issn" = "SCOP_issn"), relationship = "many-to-many") %>%
                                                left_join(doaj_language_match, by = c("OA_issn" = "DOAJ_issn"), relationship = "many-to-many") %>%
                                                select(OA_ID, MJL_ID, SCOP_ID, DOAJ_ID, OA_issn) %>%
                                                rename(issn = OA_issn)

# remove ISSN code variable, duplicated rows, cases with only one ID, and group rows by the OpenAlex ID
ddff_ISSNs_match <- subset(ddff_ISSNs_match, select = c("OA_ID", "MJL_ID", "SCOP_ID", "DOAJ_ID"))
ddff_ISSNs_match <- ddff_ISSNs_match %>% distinct()
ddff_ISSNs_match <- ddff_ISSNs_match[rowSums(!is.na(ddff_ISSNs_match)) > 1, ]
ddff_ISSNs_match <- ddff_ISSNs_match %>% group_by(OA_ID) %>%
                                         summarise(across(everything(), ~ unique(na.omit(.))[1]), .groups = "drop")

# merge the language variables from MJL, Scopus and DOAJ into OpenAlex journals
openalex_journals <- openalex_journals %>% left_join(ddff_ISSNs_match, by = c("OA_ID" = "OA_ID"))
openalex_journals <- openalex_journals %>% left_join(mjl_language %>% select(MJL_ID, mjl_lang), by = "MJL_ID") %>%
                                           left_join(scopus_language %>% select(SCOP_ID, scopus_lang), by = "SCOP_ID") %>%
                                           left_join(doaj_language %>% select(DOAJ_ID, doaj_lang), by = "DOAJ_ID")
openalex_journals <- openalex_journals %>% select(-OA_issn, -MJL_ID, -SCOP_ID, -DOAJ_ID)

# create a new variable to identify if a journal publishes in mainstream language (= English & Multi-Language in MJL, ENG in Scopus, English in DOAJ and en in OpenAlex)
openalex_journals <- openalex_journals %>% mutate(mainstream_lang = case_when(!is.na(scopus_lang) & str_detect(scopus_lang, "\\bENG\\b") ~ 1,
                                                                              !is.na(scopus_lang) ~ 0,
                                                                              !is.na(mjl_lang) & (str_detect(mjl_lang, "English") | str_detect(mjl_lang, "Multi-Language")) ~ 1,
                                                                              !is.na(mjl_lang) ~ 0,
                                                                              !is.na(doaj_lang) & str_detect(doaj_lang, "English") ~ 1,
                                                                              !is.na(doaj_lang) ~ 0,
                                                                              !is.na(language) & str_detect(language, "\\ben\\b") ~ 1,
                                                                              !is.na(language) ~ 0,
                                                                              TRUE ~ NA_real_))


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
                                                     by = c("journal_id"))


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
                                                     by = c("journal_id"))


# clean country names
unique_countries <- openalex_journals %>% select(refs_country, cits_country, langs_country) %>%
                                          pivot_longer(everything()) %>%
                                          distinct(value)
non_matching_countries <- unique_countries %>% anti_join(world, by = c("value" = "region"))

openalex_journals <- openalex_journals %>% mutate(across(c(refs_country, cits_country), 
                                           ~ case_when(. == "Türkiye" ~ "Turkey",
                                                       . == "Réunion" ~ "Reunion",
                                                       . == "Czechia" ~ "Czech Republic",
                                                       . == "The Netherlands" ~ "Netherlands",
                                                       . == "Hong Kong" ~ "China",
                                                       . == "The Gambia" ~ "Gambia",
                                                       . == "DR Congo" ~ "Democratic Republic of the Congo",
                                                       . == "Congo Republic" ~ "Republic of Congo",
                                                       . == "Macao" ~ "China",
                                                       . == "Curaçao" ~ "Curacao",
                                                       . == "Eswatini" ~ "Swaziland",
                                                       . == "U.S. Virgin Islands" ~ "Virgin Islands",
                                                       . == "Vatican City" ~ "Vatican",
                                                       TRUE ~ .)))


# count the number of articles produced by each country, per journal and in total, as well as the number of articles produced by each journal
articles_per_country <- list.files(path = "~/Desktop/Local.Journals/articles_per_country", pattern = "local_journals_OA2503_articles_per_country_.*", full.names = TRUE)
articles_per_country <- rbindlist(lapply(articles_per_country, fread, sep = ","), fill = TRUE)
articles_per_country <- articles_per_country %>% mutate(journal_id = as.numeric(journal_id))

articles_per_country <- articles_per_country %>% mutate(across(country, 
                                                               ~ case_when(. == "Türkiye" ~ "Turkey",
                                                                           . == "Réunion" ~ "Reunion",
                                                                           . == "Czechia" ~ "Czech Republic",
                                                                           . == "The Netherlands" ~ "Netherlands",
                                                                           . == "Hong Kong" ~ "China",
                                                                           . == "The Gambia" ~ "Gambia",
                                                                           . == "DR Congo" ~ "Democratic Republic of the Congo",
                                                                           . == "Congo Republic" ~ "Republic of Congo",
                                                                           . == "Macao" ~ "China",
                                                                           . == "Curaçao" ~ "Curacao",
                                                                           . == "Eswatini" ~ "Swaziland",
                                                                           . == "U.S. Virgin Islands" ~ "Virgin Islands",
                                                                           . == "Vatican City" ~ "Vatican",
                                                                           TRUE ~ .)))

articles_per_country <- articles_per_country %>% group_by(journal_id, journal_name) %>%
                                                 mutate(arts_count_journal = n_distinct(article_id)) %>%
                                                 ungroup()

articles_per_country <- articles_per_country %>% group_by(country) %>%
                                                 mutate(arts_count_country = n_distinct(article_id)) %>%
                                                 ungroup()

articles_per_country <- articles_per_country %>% group_by(country, journal_id, journal_name) %>%
                                                 mutate(arts_country_journal = n_distinct(article_id)) %>%
                                                 ungroup()

# clean the dataframe by removing article_id variable and dropping duplicated rows
articles_per_country <- articles_per_country %>% select(-article_id) %>%
                                                 distinct()


# compute descriptive measures with variables refs_prop, cits_prop and mainstream_lang per unique journal combination
print(quantile(openalex_journals %>% distinct(journal_id, .keep_all = TRUE) %>% pull(refs_prop),
                                                                                probs = 0.75, na.rm = TRUE))

print(quantile(openalex_journals %>% distinct(journal_id, .keep_all = TRUE) %>% pull(cits_prop),
                                                                                probs = 0.75, na.rm = TRUE))

print(openalex_journals %>% distinct(journal_id, .keep_all = TRUE) %>%
                            summarise(total_zeros = sum(mainstream_lang == 0, na.rm = TRUE)))

print(openalex_journals %>% filter(refs_prop < 0.42) %>%
                            summarise(total_unique_journals = n_distinct(journal_id)))

print(openalex_journals %>% filter(cits_prop >= 0.86) %>%
                            summarise(total_unique_journals = n_distinct(journal_id)))


# isolate the knowledge bridging journals
knowledge_bridging_journals <- openalex_journals %>% filter(refs_prop < 0.42, cits_prop >= 0.86, mainstream_lang == 0)


### Figure 1A: Intersections of conditions that allow for the identification of knowledge bridging journals (n = 1,461) within a larger OpenAlex dataset (N = 59,230)
# compute journals sets dynamically
langs <- openalex_journals %>% filter(mainstream_lang == 0) %>% pull(journal_id) %>% unique()
refs <- openalex_journals %>% filter(refs_prop < 0.42) %>% pull(journal_id) %>% unique()
cits <- openalex_journals %>% filter(cits_prop >= 0.86) %>% pull(journal_id) %>% unique()

# compute their intersections
langs_refs <- intersect(langs, refs)
langs_cits <- intersect(langs, cits)
refs_cits <- intersect(refs, cits)
langs_refs_cits <- Reduce(intersect, list(langs, refs, cits))

# create an Euler diagram input data
venn_data <- c("Non-English" = length(langs),
               "References" = length(refs),
               "Citations" = length(cits),
               "Non-English&References" = length(langs_refs),
               "Non-English&Citations" = length(langs_cits),
               "References&Citations" = length(refs_cits),
               "Non-English&References&Citations" = length(langs_refs_cits))
fit <- euler(venn_data)

# plot the diagram
figure_1A <- plot(fit, 
                 fills = c("#F4FAFE", "#BED7F2", "#4981BF"),
                 labels = c("Non-English\nlanguages", "Global references", "Local citations"),
                 edges = FALSE, 
                 quantities = list(font = ifelse(venn_data == length(langs_refs_cits), 2, 1)))
ggsave("~/Desktop/Local.Journals/figure_1A.png", plot = figure_1A, width = 6.5, height = 6, dpi = 300)

### Figure 1B: Publications average of knowledge bridging journals and other related subsets
# compute average publications for knowledge bridging journals, global refs + local cits journals, global refs + non-English lang journals, and local cits + non-English lang journals
knowledge_bridging_pubs_avg <- articles_per_country %>% filter(journal_id %in% knowledge_bridging_journals$journal_id) %>%
                                                        distinct(journal_id, .keep_all = TRUE) %>%
                                                        summarize(avg_pubs = mean(arts_count_journal, na.rm = TRUE))

refs_cits_pubs_avg <- openalex_journals %>% filter(refs_prop < 0.42, cits_prop >= 0.86) %>%
                                            pull(journal_id)
refs_cits_pubs_avg <- articles_per_country %>% filter(journal_id %in% refs_cits_pubs_avg) %>%
                                               distinct(journal_id, .keep_all = TRUE) %>%
                                               summarize(avg_pubs = mean(arts_count_journal, na.rm = TRUE))

refs_lang_pubs_avg <- openalex_journals %>% filter(refs_prop < 0.42, mainstream_lang == 0) %>%
                                            pull(journal_id)
refs_lang_pubs_avg <- articles_per_country %>% filter(journal_id %in% refs_lang_pubs_avg) %>%
                                               distinct(journal_id, .keep_all = TRUE) %>%
                                               summarize(avg_pubs = mean(arts_count_journal, na.rm = TRUE))

cits_lang_pubs_avg <- openalex_journals %>% filter(cits_prop >= 0.86, mainstream_lang == 0) %>%
                                            pull(journal_id)
cits_lang_pubs_avg <- articles_per_country %>% filter(journal_id %in% cits_lang_pubs_avg) %>%
                                               distinct(journal_id, .keep_all = TRUE) %>%
                                               summarize(avg_pubs = mean(arts_count_journal, na.rm = TRUE))

# combine the average publications values into one dataframe for plotting
figure_1B <- data.frame(Subset = c("Knowledge bridging journals", "Global references + local citations journals", "Global references + non-English language journals", "Local citations + non-English language journals"),
                        Avg_Pubs = c(knowledge_bridging_pubs_avg$avg_pubs, refs_cits_pubs_avg$avg_pubs, refs_lang_pubs_avg$avg_pubs, cits_lang_pubs_avg$avg_pubs))

figure_1B <- ggplot(figure_1B, aes(x = Subset, y = Avg_Pubs, fill = Subset)) +
                    geom_bar(stat = "identity") +
                    theme_minimal() +
                    labs(x = "Journals subsets", y = "Publications average") +
                    scale_fill_manual(values = c("#D35400", "#7BA9D9", "#F1C40F", "#4981BF")) +
                    theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text.y = element_text(size = 16),
                          axis.title.x = element_text(size = 18, face = "bold"),
                          axis.title.y = element_text(size = 18, face = "bold"),
                          legend.text = element_text(size = 14),
                          legend.title = element_text(size = 16, face = "bold"),
                          legend.position = "bottom")
ggsave("~/Desktop/Local.Journals/figure_1B.png", width = 14, height = 10, dpi = 300)

### Figure 1C: Citations average of knowledge bridging journals and other related subsets
# compute average citations for knowledge bridging journals, global refs + local cits journals, global refs + non-English lang journals, and local cits + non-English lang journals
knowledge_bridging_cits_avg <- knowledge_bridging_journals %>% distinct(journal_id, .keep_all = TRUE) %>%
                                                               summarize(avg_cits = mean(cits_total, na.rm = TRUE))

refs_cits_cits_avg <- openalex_journals %>% filter(refs_prop < 0.42, cits_prop >= 0.86) %>%
                                            distinct(journal_id, .keep_all = TRUE) %>%
                                            summarize(avg_cits = mean(cits_total, na.rm = TRUE))

refs_lang_cits_avg <- openalex_journals %>% filter(refs_prop < 0.42, mainstream_lang == 0) %>%
                                            distinct(journal_id, .keep_all = TRUE) %>%
                                            summarize(avg_cits = mean(cits_total, na.rm = TRUE))

cits_lang_cits_avg <- openalex_journals %>% filter(cits_prop >= 0.86, mainstream_lang == 0) %>%
                                            distinct(journal_id, .keep_all = TRUE) %>%
                                            summarize(avg_cits = mean(cits_total, na.rm = TRUE))

# combine the average citations values into one dataframe for plotting
figure_1C <- data.frame(Subset = c("Knowledge bridging journals", "Global references + local citations journals", "Global references + non-English language journals", "Local citations + non-English language journals"),
                        Avg_Cits = c(knowledge_bridging_cits_avg$avg_cits, refs_cits_cits_avg$avg_cits, refs_lang_cits_avg$avg_cits, cits_lang_cits_avg$avg_cits))

figure_1C <- ggplot(figure_1C, aes(x = Subset, y = Avg_Cits, fill = Subset)) +
                    geom_bar(stat = "identity") +
                    theme_minimal() +
                    labs(x = "Journals subsets", y = "Citations average") +
                    scale_fill_manual(values = c("#D35400", "#7BA9D9", "#F1C40F", "#4981BF")) +
                    theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text.y = element_text(size = 16),
                          axis.title.x = element_text(size = 18, face = "bold"),
                          axis.title.y = element_text(size = 18, face = "bold"),
                          legend.text = element_text(size = 14),
                          legend.title = element_text(size = 16, face = "bold"),
                          legend.position = "bottom")
ggsave("~/Desktop/Local.Journals/figure_1C.png", width = 14, height = 10, dpi = 300)


### Figure 2: Countries publication number and share in knowledge bridging journals controlled by country size
# combine with the knowledge bridging journals their articles count and total variables
knowledge_bridging_journals_countries <- knowledge_bridging_journals %>% mutate(journal_id = as.character(journal_id)) %>%
                                                                         left_join(articles_per_country %>%
                                                                         mutate(journal_id = as.character(journal_id)), by = "journal_id") %>%
                                                                         select(journal_id, country, arts_country_journal, arts_count_country) %>%
                                                                         distinct()

# summarise articles count, total count (within 2023 production) and share per country
knowledge_bridging_journals_countries <- knowledge_bridging_journals_countries %>% group_by(country) %>%
                                                                                   summarise(arts_country_journal = sum(arts_country_journal, na.rm = TRUE),
                                                                                   arts_count_country = first(arts_count_country, na.rm = TRUE),
                                                                                   arts_share = arts_country_journal / arts_count_country) %>%
                                                                                   ungroup()

# add continent variable to group countries when plotting
knowledge_bridging_journals_countries <- knowledge_bridging_journals_countries %>% mutate(continent = countrycode(country, origin = "country.name", destination = "continent"),
                                                                                   region = case_when(country %in% c("United States", "Canada", "Mexico") ~ "North America",
                                                                                                      country %in% c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras",
                                                                                                                     "Nicaragua", "Panama", "Cuba", "Haiti", "Dominican Republic",
                                                                                                                     "Jamaica", "Trinidad and Tobago", "Bahamas", "Barbados") ~ "Central America & the Caribbean",
                                                                                                      country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                                                                                                                     "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname",
                                                                                                                     "Uruguay", "Venezuela") ~ "South America",
                                                                                                      TRUE ~ continent))

# plot figure 2A
ggplot(knowledge_bridging_journals_countries %>%
         filter(!is.na(region)) %>%
         arrange(desc(arts_country_journal)) %>%
         slice_head(n = 25),  
       aes(x = reorder(country, arts_country_journal), y = arts_country_journal, fill = factor(region, 
       levels = c("Asia", "Europe", "North America", "Central America & the Caribbean", "South America")))) +
  geom_col() +
  coord_flip() +  
  labs(x = "Country",
       y = "Publications total",
       fill = "Region") +
  scale_fill_manual(values = c("North America" = "#7BA9D9", "Central America & the Caribbean" = "#F39C12",
                               "South America" = "#F1C40F", "Europe" = "#4981BF", "Asia" = "#D35400")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"))
ggsave("~/Desktop/Local.Journals/figure_2A.png", width = 14, height = 18, dpi = 300)

# plot figure 2B
ggplot(knowledge_bridging_journals_countries %>%
         filter(!is.na(region)) %>%
         arrange(desc(arts_share)) %>%
         slice_head(n = 25),  
       aes(x = reorder(country, arts_share), y = arts_share, fill = factor(region, 
                                                                           levels = c("Africa", "Asia", "Europe", "North America", "Central America & the Caribbean", "South America")))) +
  geom_col() +
  coord_flip() +  
  labs(x = "Country",
       y = "Publications share",
       fill = "Region") +
  scale_fill_manual(values = c("North America" = "#7BA9D9", "Central America & the Caribbean" = "#F39C12",
                               "South America" = "#F1C40F", "Europe" = "#4981BF", "Asia" = "#D35400", "Africa" = "#1F3A64")) + 
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"))
ggsave("~/Desktop/Local.Journals/figure_2B.png", width = 16, height = 20, dpi = 300)


### Figure 3: Distribution of knowledge bridging journals by OpenAlex field and domain categories
# subset the knowledge bridging journals meeting all three conditions for subsequent plotting
knowledge_bridging_journals <- openalex_journals %>% filter(refs_prop < 0.42, cits_prop >= 0.86, mainstream_lang == 0)

# prepare field and domain data for each knowledge bridging journal
knowledge_bridging_journals_fields <- knowledge_bridging_journals %>% select(journal_id, journal_name, field) %>%
                                                                      separate_rows(field, sep = "; ") %>%
                                                                      distinct(journal_id, journal_name, field)
openalex_fields_domains <- openalex_topics %>% select(field_name, domain_name) %>%
                                               distinct()
knowledge_bridging_journals_fields <- knowledge_bridging_journals_fields %>% left_join(openalex_fields_domains, by = c("field" = "field_name"))

# compute journals count and share per field and domain
knowledge_bridging_journals_fields <- knowledge_bridging_journals_fields %>% group_by(domain_name, field) %>%
                                                                             summarise(jours_count = n_distinct(journal_id),
                                                                             jours_share = jours_count / 1271, .groups = 'drop') %>%
                                                                             ungroup()

# plot the grouped barplot
ggplot(na.omit(knowledge_bridging_journals_fields), aes(x = field, y = jours_share, fill = domain_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits = c("Veterinary", "Dentistry", "Nursing", "Medicine", "Health Professions",
                              "Pharmacology, Toxicology and Pharmaceutics", "Immunology and Microbiology", "Neuroscience", "Biochemistry, Genetics and Molecular Biology", "Agricultural and Biological Sciences",
                              "Chemical Engineering", "Mathematics", "Physics and Astronomy", "Chemistry", "Materials Science", "Energy", "Earth and Planetary Sciences", "Engineering", "Environmental Science", "Computer Science",
                              "Decision Sciences", "Arts and Humanities", "Economics, Econometrics and Finance", "Business, Management and Accounting", "Psychology", "Social Sciences")) +
  scale_fill_manual(values = c("Health Sciences" = "#D35400", "Life Sciences" = "#7BA9D9", "Physical Sciences" = "#F1C40F", "Social Sciences" = "#4981BF")) +
  theme_minimal() +
  labs(x = "Field", y = "Journals share", fill = "Domain") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("~/Desktop/Local.Journals/figure_3.png", width = 10, height = 6, dpi = 300)

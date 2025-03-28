library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(readr)
library(stringr)
library(bit64)
library(ggplot2)
library(ggVennDiagram)
library(maps)
library(scales)
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


# count the number of articles produced by each country, per journal and in total
articles_per_country <- list.files(path = "~/Desktop/Local.Journals/articles_per_country", pattern = "local_journals_OA2503_articles_per_country_.*", full.names = TRUE)
articles_per_country <- rbindlist(lapply(articles_per_country, fread, sep = ","), fill = TRUE)

articles_per_country <- articles_per_country %>% group_by(journal_id, journal_name, country) %>%
                                                 mutate(arts_count = n()) %>%
                                                 ungroup()
articles_per_country <- within(articles_per_country, rm(article_id))
articles_per_country <- articles_per_country %>% distinct()

articles_per_country <- articles_per_country %>% group_by(country) %>%
                                                 mutate(arts_total = sum(arts_count, na.rm = TRUE)) %>%
                                                 ungroup()


# clean country names
unique_countries <- openalex_journals %>% select(refs_country, cits_country, langs_country) %>%
                                          pivot_longer(everything()) %>%
                                          distinct(value)
non_matching_countries <- unique_countries %>% anti_join(world, by = c("value" = "region"))

openalex_journals <- openalex_journals %>% mutate(across(c(refs_country, cits_country, langs_country), 
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

# import world coordinates and tweak a few country names
world <- map_data("world")
world <- world %>% mutate(across(region, 
                   ~ case_when(. == "USA" ~ "United States",
                               . == "UK" ~ "United Kingdom",
                               . == "Trinidad" ~ "Trinidad and Tobago",
                               . == "Tobago" ~ "Trinidad and Tobago",
                               . == "Saint Kitts" ~ "St Kitts and Nevis",
                               . == "Nevis" ~ "St Kitts and Nevis",
                               . == "Antigua" ~ "Antigua and Barbuda",
                               . == "Barbuda" ~ "Antigua and Barbuda",
                               TRUE ~ .)))


### Table 2. Descriptive measures of the variables at the journal level
# compute descriptive measures with variables refs_prop, cits_prop and mainstream_lang
print(mean(openalex_journals$refs_prop, na.rm = TRUE))
print(median(openalex_journals$refs_prop, na.rm = TRUE))

print(min(openalex_journals$refs_prop, na.rm = TRUE))
print(max(openalex_journals$refs_prop, na.rm = TRUE))

print(quantile(openalex_journals$refs_prop, probs = c(0.25,0.75), na.rm = TRUE))
print(sd(openalex_journals$refs_prop, na.rm = TRUE))

print(openalex_journals %>% distinct(journal_id, .keep_all = TRUE) %>%
                            summarise(total_zeros = sum(mainstream_lang == 0, na.rm = TRUE)))

print(openalex_journals %>% filter(refs_prop < 0.38) %>%
                            summarise(total_unique_journals = n_distinct(journal_id)))

print(openalex_journals %>% filter(cits_prop >= 0.75) %>%
                            summarise(total_unique_journals = n_distinct(journal_id)))


### Figure 1: Intersections of conditions that allow for the identification of knowledge bridging journals (n = 1,461) within a larger OpenAlex dataset (N = 59,230)
# compute the total number of unique journals that meet all three conditions at once
print(openalex_journals %>% distinct(journal_id, .keep_all = TRUE) %>%
                            filter(mainstream_lang == 0, refs_prop < 0.38, cits_prop >= 0.75) %>%
                            summarise(total_unique_journals = n_distinct(journal_id)))

# compute all three overlaps
print(openalex_journals %>% distinct(journal_id, .keep_all = TRUE) %>%
                            filter(refs_prop < 0.38, cits_prop >= 0.75) %>%
                            summarise(total_unique_journals = n_distinct(journal_id)))

# define intersecting journals subsets
langs <- paste0("langs", 1:1243) # Languages journals alone
refs <- paste0("refs", 1:31184) # References journals alone
cits <- paste0("cits", 1:7084) # Citations journals alone

langs_refs <- paste0("langs_refs", 1:3058)   # Languages & References intersection
langs_cits <- paste0("langs_cits", 1:2222)   # Languages & Citations intersection
refs_cits <- paste0("refs_cits", 1:5834)   # References & Citations intersection
langs_refs_cits <- paste0("langs_refs_cits", 1:1461) # All three intersections

journals_subsets <- list("Non-English publishing" = c(langs, langs_refs, langs_cits, langs_refs_cits),
                         "Referenced proportion" = c(refs, langs_refs, refs_cits, langs_refs_cits),
                         "Citing proportion" = c(cits, langs_cits, refs_cits, langs_refs_cits))

# plot the Venn diagram
figure_1 <- ggVennDiagram(journals_subsets, label_alpha = 0, edge_size = 0.3, edge_color = "gray",
              category.names = c("Non-English\nlanguages", "Global\nreferences", "Local citations")) +
  scale_fill_gradient(low = "#F4FAFE" , high = "#4981BF") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
ggsave("~/Desktop/Local.Journals/figure_1.png", plot = figure_1, width = 6.5, height = 6, dpi = 300)


### Figure 2:
# subset the knowledge bridging journals meeting all three conditions
knowledge_bridging_journals <- openalex_journals %>% filter(refs_prop < 0.38, cits_prop >= 0.75, mainstream_lang == 0)


### Figure 3: Countries publication share in knowledge bridging journals with respect to (A) each country's publication total for 2023, and (B) the knowledge bridging journals' publication total for 2023
# combine with the knowledge bridging journals their articles count and total variables
knowledge_bridging_journals_countries <- knowledge_bridging_journals %>% mutate(journal_id = as.character(journal_id)) %>%
                                                                         left_join(articles_per_country %>% mutate(journal_id = as.character(journal_id)), by = "journal_id") %>%
                                                                         select(journal_id, country, arts_count, arts_total)

# summarise articles count, grand total (articles count per country within their 2023 production) and compute share per country
knowledge_bridging_journals_countries <- knowledge_bridging_journals_countries %>% group_by(country) %>%
                                                                                   summarise(arts_count = sum(arts_count, na.rm = TRUE),
                                                                                             arts_total = first(arts_total, na.rm = TRUE),
                                                                                             arts_share = arts_count / arts_total) %>%
                                                                                   ungroup() %>%
                                                                                   rename(region = country)

# compute articles inner total (articles total count within the knowledge bridging journals) and share per country
knowledge_bridging_journals_countries <- knowledge_bridging_journals_countries %>% mutate(inner_total = sum(arts_count, na.rm = TRUE),
                                                                                   inner_share = arts_count / inner_total)

# incorporate each countries' coordinates from the world dataframe
knowledge_bridging_journals_countries <- knowledge_bridging_journals_countries %>% full_join(world, by = c("region" = "region"))

# reshape share data from wide to long to facilitate faceting within the plot
knowledge_bridging_journals_countries <- knowledge_bridging_journals_countries %>% pivot_longer(cols = c(arts_share, inner_share), 
                                                                                   names_to = "share_type", 
                                                                                   values_to = "share_value")

# plot faceted maps
ggplot(knowledge_bridging_journals_countries) +
  geom_map(aes(map_id = region, fill = share_value), map = world) +
  facet_wrap(~ share_type, scales = "free", , ncol = 1,
             labeller = labeller(share_type = c("arts_share" = "(A)", 
                                                "inner_share" = "(B)"))) +
  expand_limits(x = world$long, y = world$lat) +
  scale_fill_continuous(low = "#FFE5B4", high = "#D35400", na.value = "grey",
                        labels = label_number(accuracy = 0.01)) +
  theme_minimal() +
  labs(fill = "Publication share") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"))
ggsave("~/Desktop/Local.Journals/figure_3.png", width = 10, height = 12, dpi = 300)

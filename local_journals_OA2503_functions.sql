-- Create all journals table from OpenAlex's sources, keeping only the journal document type
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions AS
SELECT DISTINCT
  id,
  type,
  display_name,
  abbreviated_title,
  issn,
  issn_l
FROM insyspo.publicdb_openalex_2025_03_rm.sources
WHERE type = 'journal';

-- Create all articles table from OpenAlex's works, keeping only 2023 articles
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_articles_functions AS
SELECT DISTINCT
  w.id AS article_id,
  w.type,
  w.publication_year AS year,
  w.title,
  w.language,
  t.display_name AS topic_display_name,
  t.subfield,
  t.field,
  t.domain
FROM insyspo.publicdb_openalex_2025_03_rm.works w
LEFT JOIN insyspo.publicdb_openalex_2025_03_rm.works_topics wt
  ON w.id = wt.work_id
LEFT JOIN insyspo.publicdb_openalex_2025_03_rm.topics t
  ON wt.topic_id = t.id
WHERE w.type = 'article'
  AND w.publication_year = 2023;

-- Merge all journals and articles tables through works_locations table to keep one row per unique combination of journal and article
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions AS
SELECT
  j.id AS journal_id,
  j.display_name AS journal_name,
  j.abbreviated_title,
  j.issn,
  j.issn_l,
  p.article_id,
  p.year AS publication_year,
  p.title,
  p.language,
  p.topic_display_name,
  p.subfield,
  p.field,
  p.domain
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions AS j
JOIN insyspo.publicdb_openalex_2025_03_rm.works_locations AS wl
  ON j.id = wl.source_id
JOIN steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_articles_functions AS p
  ON wl.work_id = p.article_id;

-- Erase all deleted journals
DELETE FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions
WHERE journal_id = 4317411217;

-- Add total_articles variable to count the number of unique articles per unique journal
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions AS
SELECT 
  j.*, 
  COUNT(DISTINCT j.article_id) OVER (PARTITION BY j.journal_id) AS total_articles
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions j;

-- Add total_articles_references variable to count the number of unique articles per unique journal that present references
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions AS
SELECT 
  j.*, 
  COALESCE(COUNT(DISTINCT CASE WHEN wrw.referenced_work_id IS NOT NULL THEN j.article_id END) 
           OVER (PARTITION BY j.journal_id), 0) AS total_articles_references
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions j
LEFT JOIN insyspo.publicdb_openalex_2025_03_rm.works_referenced_works wrw
  ON j.article_id = wrw.work_id;

-- Add prop_articles_references variable to compute the proportion total_articles_references / total_articles
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions AS
SELECT 
  j.*, 
  SAFE_DIVIDE(j.total_articles_references, j.total_articles) AS prop_articles_references
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions j;

-- Filter the whole table to keep only those cases where >= 50% of articles per journal present references
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions AS
SELECT *
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions
WHERE prop_articles_references >= 0.5;

-- Group by unique journal, eliminating article_id and title variables
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions AS
SELECT 
  journal_id,
  ANY_VALUE(journal_name) AS journal_name, 
  ANY_VALUE(abbreviated_title) AS abbreviated_title,
  ANY_VALUE(issn) AS issn,
  ANY_VALUE(issn_l) AS issn_l,
  MAX(publication_year) AS publication_year, 
  STRING_AGG(DISTINCT language, '; ') AS language,
  STRING_AGG(DISTINCT topic_display_name, '; ') AS topic_display_name,
  STRING_AGG(DISTINCT CAST(subfield AS STRING), '; ') AS subfield,
  STRING_AGG(DISTINCT CAST(field AS STRING), '; ') AS field,
  STRING_AGG(DISTINCT CAST(domain AS STRING), '; ') AS domain,
  MAX(total_articles) AS total_articles, 
  MAX(total_articles_references) AS total_articles_references, 
  MAX(prop_articles_references) AS prop_articles_references
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions
GROUP BY journal_id;

-- Compute final unique journals count
SELECT COUNT(DISTINCT journal_id) AS unique_journals FROM `steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_functions`;
---- Result: 59230 unique journals
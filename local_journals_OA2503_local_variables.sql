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
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions AS
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
DELETE FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions
WHERE journal_id = 4317411217;

-- Add total_articles variable to count the number of unique articles per unique journal
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions AS
SELECT 
  j.*, 
  COUNT(DISTINCT j.article_id) OVER (PARTITION BY j.journal_id) AS total_articles
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions j;

-- Add total_articles_references variable to count the number of unique articles per unique journal that present references
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions AS
SELECT 
  j.*, 
  COALESCE(COUNT(DISTINCT CASE WHEN wrw.referenced_work_id IS NOT NULL THEN j.article_id END) 
           OVER (PARTITION BY j.journal_id), 0) AS total_articles_references
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions j
LEFT JOIN insyspo.publicdb_openalex_2025_03_rm.works_referenced_works wrw
  ON j.article_id = wrw.work_id;

-- Add prop_articles_references variable to compute the proportion total_articles_references / total_articles
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions AS
SELECT 
  j.*, 
  SAFE_DIVIDE(j.total_articles_references, j.total_articles) AS prop_articles_references
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions j;

-- Filter the whole table to keep only those cases where >= 50% of articles per journal present references
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions AS
SELECT *
FROM steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions
WHERE prop_articles_references >= 0.5;

-- export 2023 publications per country
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_articles_per_country AS
SELECT DISTINCT
  a.journal_id,
  a.journal_name,
  a.article_id,
  i.country
FROM `steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions` a
LEFT JOIN `insyspo.publicdb_openalex_2025_03_rm.works_authorships` wa
  ON a.article_id = wa.work_id
LEFT JOIN `insyspo.publicdb_openalex_2025_03_rm.institutions` i
  ON wa.institution_id = i.id
WHERE i.country IS NOT NULL;

-- compute the references local variable for export
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_references_local_variable AS
SELECT DISTINCT
  a.journal_id,
  a.journal_name,
  a.article_id,
  r.referenced_work_id AS reference_id,
  i.country
FROM `steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions` a
JOIN `insyspo.publicdb_openalex_2025_03_rm.works_referenced_works` r
  ON a.article_id = r.work_id
LEFT JOIN `insyspo.publicdb_openalex_2025_03_rm.works_authorships` wa
  ON r.referenced_work_id = wa.work_id
LEFT JOIN `insyspo.publicdb_openalex_2025_03_rm.institutions` i
  ON wa.institution_id = i.id
WHERE i.country IS NOT NULL;

-- compute the citations local variable for export
CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_citations_local_variable AS
SELECT DISTINCT
  a.journal_id,
  a.journal_name,
  a.article_id,
  r.work_id AS citing_work_id,
  i.country
FROM `steadfast-task-437611-f3.userdb_VDC.local_journals_OA2503_journals_and_articles_functions` a
JOIN `insyspo.publicdb_openalex_2025_03_rm.works_referenced_works` r
  ON a.article_id = r.referenced_work_id
LEFT JOIN `insyspo.publicdb_openalex_2025_03_rm.works_authorships` wa
  ON r.work_id = wa.work_id
LEFT JOIN `insyspo.publicdb_openalex_2025_03_rm.institutions` i
  ON wa.institution_id = i.id
WHERE i.country IS NOT NULL;

-- produce export data to compute the pubs_prop local variable
--- CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_research_OA2410_articles_journals_export AS
--- SELECT
---   journal_id,
---   journal_name,
---   article_id,
---   ARRAY_TO_STRING(country, '&/&') AS country
--- FROM `steadfast-task-437611-f3.userdb_VDC.local_research_OA2410_articles_table`;

-- produce export data to compute the databases local variable
---  CREATE OR REPLACE TABLE steadfast-task-437611-f3.userdb_VDC.local_research_OA2410_articles_journals_export AS
---  SELECT DISTINCT
---    journal_id,
---    journal_name,
---    issn
---  FROM
---  steadfast-task-437611-f3.userdb_VDC.local_research_OA2410_articles_journals_export;

-- produce export data to compute the toponyms local variable
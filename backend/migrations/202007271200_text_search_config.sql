-- from https://www.postgresql.org/docs/12/textsearch-dictionaries.html#TEXTSEARCH-SIMPLE-DICTIONARY
-- and: https://dba.stackexchange.com/questions/145016/finding-the-most-commonly-used-non-stop-words-in-a-column/186754#186754

CREATE TEXT SEARCH DICTIONARY english_simple_dict (
    TEMPLATE = pg_catalog.simple
  , STOPWORDS = english
);

CREATE TEXT SEARCH CONFIGURATION english_simple (COPY = simple);
ALTER  TEXT SEARCH CONFIGURATION english_simple
   ALTER MAPPING FOR asciiword WITH english_simple_dict;

-- index to speed up the common "stats" search:
CREATE INDEX IF NOT EXISTS word_cloud_index ON dream USING GIN (to_tsvector('english_simple', title || ' ' || description));

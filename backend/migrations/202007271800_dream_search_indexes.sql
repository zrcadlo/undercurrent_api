--
-- DREAM INDICES
--
CREATE INDEX IF NOT EXISTS idx_dreamed_at ON dream(dreamed_at);

-- to ensure usage of this index, operations _must_ explicitly use the `english` config:
-- https://www.postgresql.org/docs/12/textsearch-tables.html#TEXTSEARCH-TABLES-INDEX
CREATE INDEX IF NOT EXISTS idx_dream_ftsearch ON dream USING GIN (to_tsvector('english', title || ' ' || description));
-- optimized for storage and performance of membership/containment queries (via @>) :
-- https://www.postgresql.org/docs/12/datatype-json.html#JSON-INDEXING
CREATE INDEX IF NOT EXISTS idx_dream_emotions ON dream USING GIN (emotions jsonb_path_ops);

-- note: not creating indices for most of the boolean columns, as per:
-- https://stackoverflow.com/questions/42972726/postgresql-create-index-for-boolean-column
-- https://dba.stackexchange.com/questions/131514/postgresql-not-using-partial-index-when-using-boolean-in-where-clause
-- https://devcenter.heroku.com/articles/postgresql-indexes#partial-indexes
-- although we may benefit from partial indexes down the line, especially for private dreams?

-- 
-- USER INDICES
--
-- if we see that the distribution of private dreams is low, we could add this:
-- CREATE INDEX IF NOT EXISTS idx_private_dreams ON dream((1)) where is_private

CREATE INDEX IF NOT EXISTS idx_user_location ON user_account(location);
CREATE INDEX IF NOT EXISTS idx_user_gender ON user_account(gender);
CREATE INDEX IF NOT EXISTS idx_user_zodiac ON user_account(zodiac_sign);

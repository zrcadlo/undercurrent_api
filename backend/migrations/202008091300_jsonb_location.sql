ALTER TABLE user_account
    DROP COLUMN IF EXISTS location,
    ADD COLUMN location jsonb;

ALTER TABLE dream
    ADD COLUMN IF NOT EXISTS location jsonb;

-- see:
-- https://www.postgresql.org/docs/12/datatype-json.html#JSON-INDEXING
CREATE INDEX IF NOT EXISTS idx_user_location  ON user_account USING GIN (location jsonb_path_ops);
CREATE INDEX IF NOT EXISTS idx_dream_location ON dream USING GIN (location jsonb_path_ops);

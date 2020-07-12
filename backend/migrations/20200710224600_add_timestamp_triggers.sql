CREATE OR REPLACE FUNCTION create_timestamps()   
        RETURNS TRIGGER AS $$
        BEGIN
            NEW.created_at = now();
            NEW.updated_at = now();
            RETURN NEW;   
        END;
        $$ language 'plpgsql';

CREATE OR REPLACE FUNCTION update_timestamps()   
        RETURNS TRIGGER AS $$
        BEGIN
            NEW.updated_at = now();
            RETURN NEW;   
        END;
        $$ language 'plpgsql';

CREATE TRIGGER user_account_insert BEFORE INSERT ON user_account FOR EACH ROW EXECUTE PROCEDURE create_timestamps();
CREATE TRIGGER user_account_update BEFORE UPDATE ON user_account FOR EACH ROW EXECUTE PROCEDURE update_timestamps();

CREATE TRIGGER dream_insert BEFORE INSERT ON dream FOR EACH ROW EXECUTE PROCEDURE create_timestamps();
CREATE TRIGGER dream_update BEFORE UPDATE ON dream FOR EACH ROW EXECUTE PROCEDURE update_timestamps();
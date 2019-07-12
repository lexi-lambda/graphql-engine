ALTER TABLE hdb_catalog.hdb_version
  ADD COLUMN pg_version text NULL
  ADD COLUMN pg_upgraded_on timestamptz NULL;

CREATE VIEW hdb_catalog.hdb_columns_ordinary_default_values AS
  SELECT table_schema::text, table_name::text, column_name::text, column_default::text
    FROM information_schema.columns
    WHERE column_default IS NOT NULL;

-- Same as hdb_columns_ordinary_default_values on versions of Postgres <10, but
-- includes identity columns on Postgres >=10.
CREATE VIEW hdb_catalog.hdb_columns_default_values AS
  SELECT * FROM hdb_catalog.hdb_columns_ordinary_default_values;

CREATE OR REPLACE FUNCTION
  hdb_catalog.inject_table_defaults(view_schema text, view_name text, tab_schema text, tab_name text) RETURNS void
LANGUAGE plpgsql AS $$
  DECLARE
    r RECORD;
  BEGIN
    FOR r IN SELECT column_name, column_default
               FROM hdb_catalog.hdb_columns_default_values
              WHERE table_schema = tab_schema AND table_name = tab_name
      EXECUTE format('ALTER VIEW %I.%I ALTER COLUMN %I SET DEFAULT %s;', view_schema, view_name, r.column_name, r.column_default);
    END LOOP;
  END $$;

CREATE OR REPLACE PROCEDURE insert_distinct_event(new_site smallint, new_name varchar(16), new_value jsonb)
LANGUAGE plpgsql AS $$
DECLARE
    allow_insert BOOLEAN;
BEGIN
    -- Testaa, onko viimeisin arvo eri kuin uusi
    SELECT value <> new_value
    INTO allow_insert
    FROM misc_event
    WHERE new_site = site AND name = new_name
    ORDER BY ts DESC;

    -- Lisää rivi, jos arvo on uusi (tai ei aiempia rivejä)
    IF COALESCE(allow_insert, TRUE) THEN
        INSERT INTO misc_event (site, name, value)
        VALUES (new_site, new_name, new_value);
    END IF;
END;
$$;

-- Migration from old version where target used to be text
UPDATE misc_event
  SET value=COALESCE(jsonb_set(value, '{"target"}', to_jsonb(substring(value->>'explanation' from '^Target (.*)%$')::numeric)), value)
  WHERE name='decision' AND site=1 AND value->'target' IS NULL;

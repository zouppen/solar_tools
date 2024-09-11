CREATE OR REPLACE PROCEDURE insert_distinct_event(new_name character(16), new_value jsonb)
LANGUAGE plpgsql AS $$
DECLARE
    allow_insert BOOLEAN;
BEGIN
    -- Testaa, onko viimeisin arvo eri kuin uusi
    SELECT value <> new_value
    INTO allow_insert
    FROM event
    WHERE name = new_name
    ORDER BY ts DESC;

    -- Lisää rivi, jos arvo on uusi (tai ei aiempia rivejä)
    IF COALESCE(allow_insert, TRUE) THEN
        INSERT INTO event (name, value)
        VALUES (new_name, new_value);
    END IF;
END;
$$;

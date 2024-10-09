-- Tagged way (newest). This assumes there's a houskeeper app which
-- populates bin and child values accordingly

-- Stores all events in a single table with tag and refernce to
-- "parent" if it is a derived value.
CREATE TABLE event (
    id integer PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
    parent integer REFERENCES event(id) ON UPDATE CASCADE ON DELETE SET NULL,
    tag smallint NOT NULL REFERENCES site_tag(id) ON UPDATE CASCADE,
    ts timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    bin smallint, -- See toBin in Integrator
    value jsonb NOT NULL
);

CREATE INDEX event_bin_all ON event(tag, ts);
CREATE INDEX event_bin_16s ON event(tag, ts) WHERE bin >= 4;
CREATE INDEX event_bin_256s ON event(tag, ts) WHERE bin >= 8;
CREATE INDEX event_unbinned ON event(tag, id) WHERE bin IS NULL;

-- Need to make this cleaner. This tries it for all data, even non-victron.
CREATE INDEX victron_battery_full
    ON event(tag, ts DESC)
    WHERE value->'payload'->'soc' = '100';

-- Populate the bins which have index to the bin list
CREATE TABLE event_bin (
    id smallint NOT NULL,
    size interval NOT NULL
);

CREATE INDEX event_size_idx ON event_bin(size);

INSERT INTO event_bin (id, size) VALUES (4, '16 sec'), (8, '256 sec');

CREATE OR REPLACE PROCEDURE insert_distinct_event(new_tag smallint, new_value jsonb)
LANGUAGE plpgsql AS $$
DECLARE
    allow_insert BOOLEAN;
BEGIN
    -- Testaa, onko viimeisin arvo eri kuin uusi
    SELECT value <> new_value
    INTO allow_insert
    FROM event
    WHERE new_tag = tag
    ORDER BY ts DESC
    LIMIT 1;

    -- Lisää rivi, jos arvo on uusi (tai ei aiempia rivejä)
    IF COALESCE(allow_insert, TRUE) THEN
        INSERT INTO event (tag, value)
        VALUES (new_tag, new_value);
    END IF;
END;
$$;

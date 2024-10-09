-- -*- mode: sql; sql-product: postgres -*-
--
-- It's important to reset bins always when moving element under a
-- different tag, because binning depends on the preceding element.


-- Outlier filter for a battery monitor in a small system
PREPARE small_battery AS
  UPDATE event
  SET bin = DEFAULT, tag = $2
  WHERE tag = $1
    AND (value -> 'payload' -> 'current')::numeric BETWEEN -100 AND 100
    AND (value -> 'payload' -> 'voltage')::numeric BETWEEN 0 AND 100
    AND (value -> 'payload' -> 'consumed_ah')::numeric BETWEEN 0 AND 200;

-- Outlier filter for a panel in a small system
PREPARE small_panel AS
  UPDATE event
    SET bin = DEFAULT, tag = $3
    WHERE tag = $1 AND value ->> 'name' = $2
      AND (value -> 'payload' -> 'solar_power')::numeric BETWEEN 0 AND 1000
      AND (value -> 'payload' -> 'battery_voltage')::numeric BETWEEN 0 AND 40
      AND (value -> 'payload' -> 'battery_charging_current')::numeric BETWEEN -50 AND 50;

PREPARE garbage AS
  UPDATE event
    SET bin = DEFAULT, tag = $2
    WHERE tag = $1;

-- Koti
\set koti_in 5
BEGIN;
EXECUTE small_battery(:koti_in, 3);
EXECUTE small_panel(:koti_in, 'Aurinko', 2);
EXECUTE garbage(:koti_in, 9);
COMMIT;

-- Ranta
\set ranta_in 6
BEGIN;
EXECUTE small_battery(:ranta_in, 13);
EXECUTE small_panel(:ranta_in, 'Lounas', 11);
EXECUTE small_panel(:ranta_in, 'Kaakko', 12);
EXECUTE garbage(:ranta_in, 10);
COMMIT;

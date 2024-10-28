-- -*- mode: sql; sql-product: postgres -*-
--
-- NB! This is not run by psql command line so \set etc. don't work.

-- Outlier filter for a battery monitor in a small system
PREPARE small_battery AS
  UPDATE event
  SET tag = $2
  WHERE tag = $1
    AND (value -> 'payload' -> 'current')::numeric BETWEEN -100 AND 100
    AND (value -> 'payload' -> 'voltage')::numeric BETWEEN 0 AND 100
    AND (value -> 'payload' -> 'consumed_ah')::numeric BETWEEN 0 AND 200;

-- Outlier filter for a panel in a small system
PREPARE small_panel AS
  UPDATE event
    SET tag = $3
    WHERE tag = $1 AND value ->> 'name' = $2
      AND (value -> 'payload' -> 'solar_power')::numeric BETWEEN 0 AND 1000
      AND (value -> 'payload' -> 'battery_voltage')::numeric BETWEEN 0 AND 40
      AND (value -> 'payload' -> 'battery_charging_current')::numeric BETWEEN -50 AND 50;

PREPARE garbage AS
  UPDATE event
    SET tag = $2
    WHERE tag = $1;

-- Koti
BEGIN;
EXECUTE small_battery(5, 3);
EXECUTE small_panel(5, 'Aurinko', 2);
EXECUTE garbage(5, 9);
COMMIT;

-- Ranta
BEGIN;
EXECUTE small_battery(6, 13);
EXECUTE small_panel(6, 'Lounas', 11);
EXECUTE small_panel(6, 'Kaakko', 12);
EXECUTE garbage(6, 10);
COMMIT;

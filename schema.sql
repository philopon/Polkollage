
CREATE TABLE raw_images(
  id           serial           PRIMARY KEY,
  sha1         bytea            NOT NULL UNIQUE,
  data         bytea            NOT NULL,
  width        integer          NOT NULL,
  height       integer          NOT NULL,
  small        bytea            NOT NULL,
  small_width  integer          NOT NULL,
  small_height integer          NOT NULL,
  scale        double precision NOT NULL);

CREATE INDEX raw_images_id ON raw_images (id);

CREATE TYPE color AS (
  red   double precision,
  green double precision,
  blue  double precision,
  alpha double precision);

CREATE TYPE arc AS (
  from_left double precision,
  from_top  double precision,
  radius    double precision,
  angle     double precision,
  scaleX    double precision,
  scaleY    double precision);


CREATE table images (
  id       serial  PRIMARY KEY,
  original integer NOT NULL REFERENCES raw_images(id),
  data     bytea   NOT NULL,
  color    color   NOT NULL,
  circles  arc[]   NOT NULL);

CREATE INDEX images_id ON images (id);


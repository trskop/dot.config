# PostgreSQL interactive terminal -- psql

Configuration of `psql` optimised for:

* Multiple databases in different environments, like *development*, *staging*,
  *production*, etc. History of each DB is stored separately to avoid execution
  of potentially harmful query on production DB. Prompt is set in a way that
  makes user aware of this fact as well.

* Tables with fields that contain (potentially huge) JSON data.

* Unicode aware terminals.

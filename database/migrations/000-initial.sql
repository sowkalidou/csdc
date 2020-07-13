--------------------------------------------------------------------------------
-- Entities

CREATE TABLE persons
  ( id serial PRIMARY KEY
  , name text NOT NULL
  , description text NOT NULL
  , orcid text NOT NULL
  );

CREATE TABLE units
  ( id serial PRIMARY KEY
  , name text NOT NULL
  , description text NOT NULL
  , chair integer NOT NULL REFERENCES persons(id)
  );

--------------------------------------------------------------------------------
-- Relations

CREATE TABLE members
  ( id serial PRIMARY KEY
  , person integer NOT NULL REFERENCES persons(id)
  , unit integer NOT NULL REFERENCES units(id)
  );

CREATE TABLE subparts
  ( id serial PRIMARY KEY
  , child integer NOT NULL REFERENCES units(id)
  , parent integer NOT NULL REFERENCES units(id)
  );

--------------------------------------------------------------------------------
-- Messages

CREATE TYPE message_type AS enum (
  'Invitation',
  'Submission'
  );

CREATE TYPE message_status AS enum (
  'Waiting',
  'Accepted',
  'Rejected'
  );

CREATE TABLE messages_member
  ( id serial PRIMARY KEY
  , mtype message_type NOT NULL
  , mstatus message_status NOT NULL
  , message text NOT NULL
  , person integer NOT NULL REFERENCES persons(id)
  , unit integer NOT NULL REFERENCES units(id)
  );

CREATE TABLE messages_subpart
  ( id serial PRIMARY KEY
  , mtype message_type NOT NULL
  , mstatus message_status NOT NULL
  , message text NOT NULL
  , child integer NOT NULL REFERENCES units(id)
  , parent integer NOT NULL REFERENCES units(id)
  );

--------------------------------------------------------------------------------
-- Replies

CREATE TYPE reply_type AS enum (
  'Accept',
  'Reject'
  );

CREATE TYPE reply_status AS enum (
  'Seen',
  'NotSeen'
  );

CREATE TABLE replies_member
  ( id serial PRIMARY KEY
  , rtype reply_type NOT NULL
  , mtype message_type NOT NULL
  , rstatus reply_status NOT NULL
  , reply text NOT NULL
  , message integer NOT NULL REFERENCES messages_member(id)
  );

CREATE TABLE replies_subpart
  ( id serial PRIMARY KEY
  , rtype reply_type NOT NULL
  , mtype message_type NOT NULL
  , rstatus reply_status NOT NULL
  , reply text NOT NULL
  , message integer NOT NULL REFERENCES messages_subpart(id)
  );

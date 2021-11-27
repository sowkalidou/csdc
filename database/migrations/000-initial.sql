--------------------------------------------------------------------------------
-- Entities

CREATE TABLE persons
  ( id serial PRIMARY KEY
  , name text NOT NULL
  , description text NOT NULL
  , orcid text NOT NULL
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

CREATE TABLE units
  ( id serial PRIMARY KEY
  , name text NOT NULL
  , description text NOT NULL
  , chair integer NOT NULL REFERENCES persons(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

--------------------------------------------------------------------------------
-- Relations

CREATE TABLE members
  ( id serial PRIMARY KEY
  , person integer NOT NULL REFERENCES persons(id)
  , unit integer NOT NULL REFERENCES units(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  , CONSTRAINT member_unique UNIQUE (person, unit)
  );

CREATE TABLE subparts
  ( id serial PRIMARY KEY
  , child integer NOT NULL REFERENCES units(id)
  , parent integer NOT NULL REFERENCES units(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  , CONSTRAINT subpart_unique UNIQUE (child, parent)
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
  , type message_type NOT NULL
  , status message_status NOT NULL DEFAULT 'Waiting'
  , message text NOT NULL
  , person integer NOT NULL REFERENCES persons(id)
  , unit integer NOT NULL REFERENCES units(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

CREATE TABLE messages_subpart
  ( id serial PRIMARY KEY
  , type message_type NOT NULL
  , status message_status NOT NULL DEFAULT 'Waiting'
  , message text NOT NULL
  , child integer NOT NULL REFERENCES units(id)
  , parent integer NOT NULL REFERENCES units(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
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
  , type reply_type NOT NULL
  , status reply_status NOT NULL DEFAULT 'NotSeen'
  , reply text NOT NULL
  , message integer NOT NULL REFERENCES messages_member(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

CREATE TABLE replies_subpart
  ( id serial PRIMARY KEY
  , type reply_type NOT NULL
  , status reply_status NOT NULL DEFAULT 'NotSeen'
  , reply text NOT NULL
  , message integer NOT NULL REFERENCES messages_subpart(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

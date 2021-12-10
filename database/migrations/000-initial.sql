CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

--------------------------------------------------------------------------------
-- Entities

CREATE TABLE persons
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , name text NOT NULL
  , description text NOT NULL
  , orcid text NOT NULL
  , image text NOT NULL
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

CREATE TABLE units
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , name text NOT NULL
  , description text NOT NULL
  , chair uuid NOT NULL REFERENCES persons(id)
  , image text NOT NULL
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

--------------------------------------------------------------------------------
-- Relations

CREATE TABLE members
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , person uuid NOT NULL REFERENCES persons(id)
  , unit uuid NOT NULL REFERENCES units(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  , CONSTRAINT member_unique UNIQUE (person, unit)
  );

CREATE TABLE subparts
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , child uuid NOT NULL REFERENCES units(id)
  , parent uuid NOT NULL REFERENCES units(id)
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
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , type message_type NOT NULL
  , status message_status NOT NULL DEFAULT 'Waiting'
  , message text NOT NULL
  , person uuid NOT NULL REFERENCES persons(id)
  , unit uuid NOT NULL REFERENCES units(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

CREATE TABLE messages_subpart
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , type message_type NOT NULL
  , status message_status NOT NULL DEFAULT 'Waiting'
  , message text NOT NULL
  , child uuid NOT NULL REFERENCES units(id)
  , parent uuid NOT NULL REFERENCES units(id)
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
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , type reply_type NOT NULL
  , status reply_status NOT NULL DEFAULT 'NotSeen'
  , reply text NOT NULL
  , message uuid NOT NULL REFERENCES messages_member(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

CREATE TABLE replies_subpart
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , type reply_type NOT NULL
  , status reply_status NOT NULL DEFAULT 'NotSeen'
  , reply text NOT NULL
  , message uuid NOT NULL REFERENCES messages_subpart(id)
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

--------------------------------------------------------------------------------
-- Files

CREATE TABLE files
  ( folder text NOT NULL
  , name text NOT NULL
  , contents bytea NOT NULL
  , size int8 NOT NULL
  , hash bytea NOT NULL
  , modified_at int8 NOT NULL
  , PRIMARY KEY(folder, name)
  );

--------------------------------------------------------------------------------
-- Forum

CREATE TABLE threads
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , unit uuid NOT NULL REFERENCES units(id)
  , author uuid NOT NULL REFERENCES persons(id)
  , subject text NOT NULL
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

CREATE TABLE posts
  ( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
  , thread uuid NOT NULL REFERENCES threads(id)
  , author uuid NOT NULL REFERENCES persons(id)
  , text text NOT NULL
  , created_at timestamptz NOT NULL DEFAULT NOW()
  );

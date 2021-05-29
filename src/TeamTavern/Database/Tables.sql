create table player
    ( id serial not null primary key
    , nickname varchar(40) not null unique
    , discord_tag varchar(37)
    , birthday date
    , languages text[] not null default '{}'
    , location varchar(100)
    , timezone varchar(50)
    , weekday_from time
    , weekday_to time
    , weekend_from time
    , weekend_to time
    , microphone boolean not null default false
    , about text[] not null default '{}'
    , password_hash character(60) not null
    , registered timestamptz not null default current_timestamp
    );

create unique index player_lower_nickname_key on player (lower(nickname));

create table team
    ( id serial not null primary key
    , owner_id int not null references player(id) on delete cascade
    , handle text not null unique
    , organization text not null -- informal, organized
    , name text
    , website text
    , discord_tag text
    , discord_server text
    , age_from integer
    , age_to integer
    , locations text[] not null default '{}'
    , languages text[] not null default '{}'
    , microphone boolean not null default false
    , timezone text
    , weekday_from time
    , weekday_to time
    , weekend_from time
    , weekend_to time
    , about text[] not null default '{}'
    , created timestamptz not null default current_timestamp
    , updated timestamptz not null default current_timestamp
    );

create table session
    ( id serial not null primary key
    , player_id integer not null references player(id) on delete cascade
    , token character(40) not null
    , revoked boolean not null default false
    , generated timestamptz not null default current_timestamp
    );

create table game
    ( id serial not null primary key
    , title varchar(50) not null unique
    , short_title varchar(50) not null unique
    , handle varchar(50) not null unique
    , description text[] not null
    , platforms text[] not null -- steam, riot, battle.net, playstation, xbox, switch
    , created timestamptz not null default current_timestamp
    );

create table field
    ( id serial not null primary key
    , game_id integer not null references game(id)
    , ilk integer not null -- 1 (url), 2 (single), 3 (multi)
    , key varchar(40) not null
    , label varchar(40) not null
    , icon varchar(40) not null
    , ordinal int not null
    , domain varchar(40)
    );

create table field_option
    ( id serial not null primary key
    , field_id integer not null references field(id)
    , key varchar(40) not null
    , label varchar(40) not null
    , ordinal int not null
    );

create table player_profile
    ( id serial not null primary key
    , player_id integer not null references player(id) on delete cascade
    , game_id integer not null references game(id)
    , platform text not null
    , platform_id text not null
    , new_or_returning boolean not null
    , ambitions text[] not null
    , created timestamptz not null default current_timestamp
    , updated timestamptz not null default current_timestamp
    , unique (game_id, player_id)
    );

create table player_profile_field_value
    ( id serial not null primary key
    , player_profile_id integer not null references player_profile(id) on delete cascade
    , field_id integer not null references field(id)
    , url varchar(200) -- When field is url.
    , field_option_id integer references field_option(id) -- When field is single select.
    );

create table player_profile_field_value_option
    ( id serial not null primary key
    , player_profile_field_value_id integer not null references player_profile_field_value(id) on delete cascade
    , field_option_id integer not null references field_option(id)
    );

create table team_profile
    ( id serial not null primary key
    , team_id integer not null references team(id) on delete cascade
    , game_id integer not null references game(id)
    , size text not null -- party, community
    , platforms text[] not null
    , new_or_returning boolean not null
    , ambitions text[] not null
    , created timestamptz not null default current_timestamp
    , updated timestamptz not null default current_timestamp
    , unique (game_id, team_id)
    );

create table team_profile_field_value
    ( id serial not null primary key
    , team_profile_id integer not null references team_profile(id) on delete cascade
    , field_id integer not null references field(id)
    );

create table team_profile_field_value_option
    ( id serial not null primary key
    , team_profile_field_value_id integer not null references team_profile_field_value(id) on delete cascade
    , field_option_id integer not null references field_option(id)
    );

create table competition
    ( id serial not null primary key
    , game_id integer not null references game(id) on delete cascade
    , name text not null
    , description text[] not null
    , website text
    , discord_server text
    );

create table alert
    ( id serial not null primary key
    , game_id integer not null references game(id)
    , token character(20) not null
    , player_or_team text not null
    , email text not null
    , organizations text[]
    , age_from integer
    , age_to integer
    , locations text[]
    , languages text[]
    , microphone boolean not null
    , timezone text not null
    , weekday_from time
    , weekday_to time
    , weekend_from time
    , weekend_to time
    , sizes text[]
    , platforms text[]
    , fields jsonb[]
    , new_or_returning boolean not null
    );

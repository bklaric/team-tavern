create table player
    ( id serial not null primary key
    , email varchar(254) not null unique
    , nickname varchar(40) not null unique
    , about varchar(2000) not null default ''
    , registered timestamptz not null default current_timestamp
    );

create table session
    ( id serial not null primary key
    , player_id integer not null references player(id)
    , token character(40) not null
    , nonce character(20) not null
    , consumed boolean not null default false
    , revoked boolean not null default false
    , generated timestamptz not null default current_timestamp
    );

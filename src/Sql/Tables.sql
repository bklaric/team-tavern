create table player
    ( id serial not null primary key
    , email varchar(254) not null unique
    , nickname varchar(40) not null unique
    , registered timestamptz not null default current_timestamp
    );

create table token
    ( id serial not null primary key
    , player_id integer not null references player(id)
    , value character(40) not null
    , nonce character(20) not null
    , consumed boolean not null default false
    , revoked boolean not null default false
    , generated timestamptz not null default current_timestamp
    );

create table player
    ( id serial not null primary key
    , email varchar(254) not null unique
    , nickname varchar(40) not null unique
    , password_hash character(60) not null
    , confirmation_nonce character(20) not null
    , email_confirmed boolean not null default false
    , about text[] not null default '{}'
    , registered timestamptz not null default current_timestamp
    );

create table session
    ( id serial not null primary key
    , player_id integer not null references player(id)
    , token character(40) not null
    , revoked boolean not null default false
    , generated timestamptz not null default current_timestamp
    );

create table game
    ( id serial not null primary key
    , administrator_id integer not null references player(id)
    , title varchar(50) not null unique
    , handle varchar(50) not null unique
    , description text[] not null
    , created timestamptz not null default current_timestamp
    );

create table profile
    ( id serial not null primary key
    , player_id integer not null references player(id)
    , game_id integer not null references game(id)
    , summary text[] not null
    , created timestamptz not null default current_timestamp
    , unique (game_id, player_id)
    );

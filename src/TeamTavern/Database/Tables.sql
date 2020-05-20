create table player
    ( id serial not null primary key
    , email varchar(254) not null unique
    , nickname varchar(40) not null unique
    , discord_tag varchar(37)
    , birthday date
    , languages text[] not null default '{}'
    , country varchar(100)
    , timezone varchar(50)
    , weekday_from time
    , weekday_to time
    , weekend_from time
    , weekend_to time
    , has_microphone boolean not null default false
    , password_hash character(60) not null
    , confirmation_nonce character(20) not null
    , email_confirmed boolean not null default false
    , about text[] not null default '{}'
    , notify boolean not null default true
    , confirm_email_notified timestamptz
    , create_profile_notified timestamptz
    , update_profile_notified timestamptz
    , registered timestamptz not null default current_timestamp
    );

create unique index player_lower_email_key on player (lower(email));

create unique index player_lower_nickname_key on player (lower(nickname));

create table password_reset
    ( id serial not null primary key
    , player_id integer not null references player(id)
    , nonce character(20) not null
    , consumed boolean not null default false
    , created timestamptz not null default current_timestamp
    )

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
    , icon_path text not null
    , banner_path text not null
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
    , required boolean not null default false
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
    , player_id integer not null references player(id)
    , game_id integer not null references game(id)
    , summary text[] not null
    , created timestamptz not null default current_timestamp
    , updated timestamptz not null default current_timestamp
    , unique (game_id, player_id)
    );

create table player_profile_field_value
    ( id serial not null primary key
    , player_profile_id integer not null references player_profile(id) on delete cascade
    , field_id integer not null references field(id)
    , url varchar(200) -- When field is url.
    , field_option_id integer references field_option(id) -- When field is singe select.
    );

create table player_profile_field_value_option
    ( id serial not null primary key
    , player_profile_field_value_id integer not null references player_profile_field_value(id) on delete cascade
    , field_option_id integer not null references field_option(id)
    );

create table team_profile
    ( id serial not null primary key
    , player_id integer not null references player(id)
    , game_id integer not null references game(id)
    , age_from integer
    , age_to integer
    , languages text[] not null default '{}'
    , countries text[] not null default '{}'
    , timezone text
    , weekday_from time
    , weekday_to time
    , weekend_from time
    , weekend_to time
    , has_microphone boolean not null default false
    , summary text[] not null
    , created timestamptz not null default current_timestamp
    , updated timestamptz not null default current_timestamp
    , unique (game_id, player_id)
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

create table conversation
    ( id serial not null primary key
    , left_interlocutor_id int not null references player(id) on delete cascade
    , right_interlocutor_id int not null references player(id) on delete cascade
    );

create table message
    ( id serial not null primary key
    , conversation_id int not null references conversation(id)
    , interlocutor_id int not null references player(id)
    , content text[] not null
    , read boolean not null default false
    , created timestamptz not null default current_timestamp
    );

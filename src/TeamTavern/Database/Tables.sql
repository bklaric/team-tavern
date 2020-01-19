create table player
    ( id serial not null primary key
    , email varchar(254) not null unique
    , nickname varchar(40) not null unique
    , discord_tag varchar(37)
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

create table profile
    ( id serial not null primary key
    , player_id integer not null references player(id)
    , game_id integer not null references game(id)
    , summary text[] not null
    , type integer not null -- 1 (player), 2 (team)
    , created timestamptz not null default current_timestamp
    , updated timestamptz not null default current_timestamp
    , unique (game_id, player_id, type)
    );

create table field
    ( id serial not null primary key
    , game_id integer not null references game(id)
    , type integer not null -- 1 (url), 2 (single), 3 (multi)
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
    , option varchar(40) not null
    , ordinal int not null
    -- , icon file_path or binary
    );

create table field_value
    ( id serial not null primary key
    , profile_id integer not null references profile(id)
    , field_id integer not null references field(id)
    , url varchar(200) -- When field is url.
    , field_option_id integer references field_option(id) -- When field is singe select.
    );

create table field_value_option
    ( id serial not null primary key
    , field_value_id integer not null references field_value(id) on delete cascade
    , field_option_id integer not null references field_option(id)
    );

create or replace view fields (game_id, fields) as
select
    game_id,
    coalesce(
        json_agg(
            json_build_object(
                'type', type,
                'label', label,
                'key', key,
                'icon', icon,
                'required', required,
                'domain', domain,
                'options', options
            )
            order by ordinal
        )
        filter (where key is not null),
        '[]'
    )
    as "fields"
from (
    select
        field.id,
        field.game_id,
        field.type,
        field.label,
        field.key,
        field.icon,
        field.ordinal,
        field.required,
        field.domain,
        json_agg(
            json_build_object(
                'key', field_option.key,
                'option', field_option.option
            )
            order by field_option.ordinal
        )
        filter (where field_option.id is not null)
        as options
    from field
        left join field_option on field_option.field_id = field.id
    group by
        field.id
    ) as field
group by game_id;

create or replace view field_values (profile_id, field_values) as
select
    profile.id,
    coalesce(
        json_agg(json_build_object(
            'fieldKey', profile_value.key,
            case
                when profile.type = 1 and profile_value.type = 1 then 'url'
                when profile.type = 1 and profile_value.type = 2 then 'optionKey'
                when profile.type = 2 and profile_value.type = 2 then 'optionKeys'
                when profile.type = 1 and profile_value.type = 3 then 'optionKeys'
                when profile.type = 2 and profile_value.type = 3 then 'optionKeys'
            end,
            case
                when profile.type = 1 and profile_value.type = 1 then url
                when profile.type = 1 and profile_value.type = 2 then single
                when profile.type = 2 and profile_value.type = 2 then multi
                when profile.type = 1 and profile_value.type = 3 then multi
                when profile.type = 2 and profile_value.type = 3 then multi
            end
        )) filter (where profile_value.field_value_id is not null),
        '[]'
    ) as field_values
from (
    select
        field_value.profile_id,
        field.key,
        field.type,
        field_value.id as field_value_id,
        to_json(field_value.url) as url,
        to_json(single.key) as single,
        json_agg(multi.key) as multi
    from field_value
    join field on field.id = field_value.field_id
    left join field_value_option on field_value_option.field_value_id = field_value.id
    left join field_option as single on single.id = field_value.field_option_id
    left join field_option as multi on multi.id = field_value_option.field_option_id
    group by
        field.id,
        field_value.id,
        single.id
) as profile_value
join profile on profile.id = profile_value.profile_id
group by profile.id;

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

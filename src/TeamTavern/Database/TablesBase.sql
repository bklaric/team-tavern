-- The redesigned schema (redesign/brief.md). It replaces player profiles, teams,
-- team profiles and alerts with posts, and adds messaging, notifications and the
-- account rows the brief's account page asks for.
--
-- This is a relaunch, not a migration: there is no dated script in Migrations/
-- that turns TablesCurrent.sql into this one. When it settles it becomes
-- TablesCurrent.sql, and production is rebuilt from it with a one-off import.

-- Regions and countries
--
-- Two levels, no nesting: every country belongs to exactly one region. A region
-- is the set of places close enough to each other to play together, which is
-- what the latency between them decides. Whether two players can talk to each
-- other is the languages field's business, so a boundary that is really about
-- language does not belong here.
--
-- A player gives a country; a group or community gives the regions it is
-- looking in. Matching compares the player's country through its region
-- (brief 7.2), so it is one lookup either way.

create table region
    ( name text not null primary key
    , ordinal integer not null
    );

create table country
    ( name text not null primary key
    , region_name text not null references region(name)
    );

create index country_region_name_idx on country (region_name);

-- Players

create table player
    ( id serial not null primary key
    , nickname varchar(40) not null unique

    -- Where the site writes, whichever way the player signs in. Nullable: a
    -- Discord account may arrive without one, and the account page asks for it.
    , email varchar(254)
    , email_confirmed boolean not null default false

    -- The sign-in identity: a password or a Discord account, never both
    -- (brief 11.5). The sign-in row on the account page moves it between them.
    , password_hash character(60)
    , discord_id text unique

    -- Shown on your posts (brief 11.5). Posts read these live, so changing one
    -- changes every post at once.
    , birthday date
    , languages text[] not null default '{}'
    -- The account page calls this Location; it is a country, and only a
    -- country. The region it matches through is country.region_name.
    , country text references country(name)
    , timezone varchar(50)

    -- Contacts, also shown on the player's posts.
    , discord_tag varchar(37)
    , steam_id text
    , riot_id text
    , battle_tag text
    , ea_id text
    , ubisoft_username text
    , psn_id text
    , gamer_tag text
    , friend_code text

    -- One switch per kind of email (brief 11.5).
    , email_matches boolean not null default true
    , email_messages boolean not null default true
    , email_renewals boolean not null default true

    , registered timestamptz not null default current_timestamp

    , constraint player_identity_check check (num_nonnulls(password_hash, discord_id) = 1)
    );

-- A password player signs in with the email, so it is unique among those only.
-- A Discord account is known by its Discord id, so its address is not a
-- credential and registering with Discord never fails on one.
create unique index player_lower_email_key on player (lower(email)) where password_hash is not null;
create unique index player_lower_nickname_key on player (lower(nickname));

create table session
    ( id serial not null primary key
    , player_id integer not null references player(id) on delete cascade
    , token character(40) not null
    , revoked boolean not null default false
    , generated timestamptz not null default current_timestamp
    );

create index session_player_id_idx on session (player_id);

create table password_reset
    ( id serial not null primary key
    , player_id integer not null references player(id) on delete cascade
    , nonce character(20) not null
    , consumed boolean not null default false
    , created timestamptz not null default current_timestamp
    );

create table email_confirmation
    ( id serial not null primary key
    , player_id integer not null references player(id) on delete cascade
    , email varchar(254) not null
    , nonce character(20) not null
    , consumed boolean not null default false
    , created timestamptz not null default current_timestamp
    );

create index email_confirmation_player_id_idx on email_confirmation (player_id);

-- Games and their fields

create table game
    ( id serial not null primary key
    , title varchar(50) not null unique
    , short_title varchar(50) not null unique
    , handle varchar(50) not null unique
    , description text[] not null
    , created timestamptz not null default current_timestamp
    );

-- The accounts a post in this game may offer, each naming a contact of
-- player's. Today's game.platforms is this list under a name that also had to
-- mean what a player plays on; platform is a game field now, and this is the
-- rest of what that column was doing.

create table game_contact
    ( game_id integer not null references game(id) on delete cascade
    , kind text not null
    , primary key (game_id, kind)
    , constraint game_contact_kind_check check (kind in
        ('discord', 'steam', 'riot', 'battle_tag', 'ea'
        , 'ubisoft', 'psn', 'gamer_tag', 'friend_code'))
    );

create table field
    ( id serial not null primary key
    , game_id integer not null references game(id) on delete cascade
    , key varchar(40) not null
    , label varchar(40) not null

    -- A boolean has no options: a post either says yes, a row in
    -- post_field_flag, or says no. It is never left unanswered, so unlike an
    -- empty single or multi it does not count as a miss, and two posts fit on
    -- it when they agree (brief 7.2).
    , ilk text not null -- 'single', 'multi', 'boolean'

    -- Whether the options have a meaningful order. An ordered field can be
    -- given as a range (post_field_range) and compared by how far apart two
    -- options sit, which is what "a few steps either way" counts (brief 7.2).
    -- The ranks and tiers are every such field; nothing else is.
    , ordered boolean not null default false

    -- Whether the field's options are slots on a team rather than a fact that
    -- has to coincide. Where both posts state it about themselves, two players
    -- fit when between them they cover two different slots, which is all a duo
    -- needs; only two locked to the same single slot cannot. Against a group
    -- nothing changes: it names the slots it needs, and a player fits by
    -- filling one (brief 7.2). Roles and positions are every such field. Rank,
    -- platform and language are facts, and fit by coinciding.
    , slotted boolean not null default false

    -- Which post types ask this field. A community has no rank and needs no
    -- particular role, while Valheim's server type is exactly what a community
    -- is, so this is per field rather than a rule. A field that doesn't apply
    -- to both types counts neither way when they are compared (brief 7.2).
    , applies_to text[] not null default '{player,group,community}'

    -- Whether the field's value leads the card or waits behind Details
    -- (brief 5).
    , on_card boolean not null default false

    , ordinal int not null

    , unique (game_id, key)
    , constraint field_ilk_check check (ilk in ('single', 'multi', 'boolean'))
    , constraint field_applies_to_check check
        (applies_to <@ '{player,group,community}' and applies_to <> '{}')
    );

create index field_game_id_idx on field (game_id);

create table field_option
    ( id serial not null primary key
    , field_id integer not null references field(id) on delete cascade
    , key varchar(40) not null
    , label varchar(40) not null
    -- A rank field's options are ordered worst to best, so the distance between
    -- two ordinals is rank closeness and a pair of them is a range.
    , ordinal int not null
    , unique (field_id, key)
    -- Lets post_field_range tie both ends of a range to the field it is on.
    , unique (id, field_id)
    );

create index field_option_field_id_idx on field_option (field_id);

create table tracker
    ( id serial not null primary key
    , game_id integer not null references game(id) on delete cascade

    -- Which account the template takes: Apex has one tracker per account it
    -- can be played with, so a tracker has to say which id it interpolates.
    , contact_kind text not null

    , title text not null
    , template text not null
    , foreign key (game_id, contact_kind) references game_contact(game_id, kind)
    );

create index tracker_game_id_idx on tracker (game_id);

-- Posts
--
-- One table for all three types (brief 3). A player has at most one post of
-- each type per game. Columns that belong to one type are null on the others,
-- and the check constraints below say which.

create table post
    ( id serial not null primary key
    , player_id integer not null references player(id) on delete cascade
    , game_id integer not null references game(id)
    , ilk text not null -- 'player', 'group', 'community'

    -- Freshness and expiry both come from this: a post is active for 30 days
    -- after it is published, renewed or edited, 90 for communities (brief 9),
    -- so publishing, renewing and editing are one act. Every query that needs
    -- the divider writes the same expression, as the token tables write their
    -- own window:
    --   updated > now() - case when ilk = 'community'
    --       then interval '90 days' else interval '30 days' end
    -- Nothing filters on it: the feed sorts by it and shows both sides
    -- (brief 4), so there is nothing here for an index to do.
    , updated timestamptz not null default current_timestamp
    , created timestamptz not null default current_timestamp

    -- Renews one post from an email without signing in (brief 9). Stable, so
    -- every renewal email for the post carries the same link.
    , renewal_nonce character(20) not null

    -- About you and what you're looking for: about and ambitions joined into
    -- one (brief 5.5). Required for communities, where the text is the product.
    , summary text[] not null default '{}'

    , microphone boolean not null default false

    -- One range in the owner's timezone, which is the account's. May cross
    -- midnight, so online_to < online_from is meaningful (brief 6, step 3).
    , online_from time
    , online_to time

    -- How people reach the post, which sets the card's contact button
    -- (brief 5.6). On-site messaging exists on every post whatever this says.
    , contact_preference text not null

    -- Group and community. A group's name is optional, a community's required.
    , name text
    -- Names from region, never countries: a group says which regions it is
    -- looking in, and a player's country is matched through its own region.
    -- Postgres cannot reference array elements, so this one is the
    -- application's to keep honest.
    , regions text[] not null default '{}'
    , languages text[] not null default '{}'
    , website text
    , discord_server text

    -- Group and community. Production fills an age range on 88% of its
    -- community profiles, so a community carries one too.
    , age_from integer
    , age_to integer

    -- Group only. How many you are, and how many more you want: the card reads
    -- "3 players, wants 2 more", and a range where the group will take either
    -- (brief 5.2).
    , group_size integer
    , group_wanted_from integer
    , group_wanted_to integer

    -- How often the post's contacts or join links were opened, shown to its
    -- owner on the home page (brief 11.2). A counter rather than a log of the
    -- openings: the home page reads it for every post of the player's at once,
    -- and nothing on the site asks who or when.
    , contact_reveals integer not null default 0

    , unique (player_id, game_id, ilk)

    , constraint post_ilk_check check (ilk in ('player', 'group', 'community'))
    -- A player or group says how it is reached; a community says how it is
    -- joined (brief 5.6).
    , constraint post_contact_preference_check check
        (case ilk
            when 'community' then contact_preference in ('discord', 'website', 'message')
            else contact_preference in ('message', 'offsite', 'either')
        end)
    , constraint post_community_name_check check (ilk <> 'community' or name is not null)
    -- A community's words are the product, so it has to have written some
    -- (brief 6, step 3). A backstop behind the API's validation, which is what
    -- says which field is wrong: if this ever fires, the API let something
    -- through.
    , constraint post_community_summary_check check
        (ilk <> 'community' or array_to_string(summary, '') ~ '\S')
    -- A community says how it is joined, so the link it names has to be there
    -- (brief 5.6); a post that says Join Discord and holds no invite renders a
    -- button with nothing behind it.
    , constraint post_community_contact_check check
        (ilk <> 'community'
            or contact_preference = 'message'
            or (contact_preference = 'discord' and discord_server is not null)
            or (contact_preference = 'website' and website is not null))
    -- A player post carries none of the group's or community's own facts: its
    -- location, languages and age come from the account (brief 6, step 3).
    , constraint post_player_has_no_group_columns check
        (ilk <> 'player' or (num_nonnulls
            (name, group_size, group_wanted_from, group_wanted_to
            , age_from, age_to, website, discord_server) = 0
            and regions = '{}' and languages = '{}'))
    -- A community is open-ended and never full (brief 3), so it has no numbers.
    , constraint post_community_has_no_group_numbers check
        (ilk <> 'community' or num_nonnulls
            (group_size, group_wanted_from, group_wanted_to) = 0)
    );

-- The feed spans all three types at once (brief 4), so ilk has no place here.
create index post_game_id_updated_idx on post (game_id, updated desc);
create index post_player_id_idx on post (player_id);
create index post_renewal_nonce_idx on post (renewal_nonce);

-- A post's answers to the game's fields. One row per chosen option: the
-- intermediate per-field row today's schema keeps carries only the field id,
-- which the option already gives, and nothing needs to tell a field left
-- unanswered from one answered with nothing (brief 7.2 counts both as a
-- mismatch).

create table post_field_option
    ( post_id integer not null references post(id) on delete cascade
    , field_option_id integer not null references field_option(id)
    , primary key (post_id, field_option_id)
    );

create index post_field_option_field_option_id_idx on post_field_option (field_option_id);

-- A post's answer to an ordered field given as a range rather than a point: a
-- group's "Platinum 1 - Diamond 3". A game has as many of these as it has
-- ranks, and seven of eleven have more than one, so a pair of columns on post
-- could not hold them. Either end may be null for an open range.

create table post_field_range
    ( post_id integer not null references post(id) on delete cascade
    , field_id integer not null references field(id)
    , from_option_id integer
    , to_option_id integer
    , primary key (post_id, field_id)
    -- Both ends have to be options of this field: a range from Bronze to
    -- Support is not one.
    , foreign key (from_option_id, field_id) references field_option (id, field_id)
    , foreign key (to_option_id, field_id) references field_option (id, field_id)
    , constraint post_field_range_not_empty check
        (num_nonnulls(from_option_id, to_option_id) > 0)
    );

-- A post's yes to a boolean field. The absence of a row is its no.

create table post_field_flag
    ( post_id integer not null references post(id) on delete cascade
    , field_id integer not null references field(id)
    , primary key (post_id, field_id)
    );

-- Messaging

-- One row per block, directional but never read that way: a block hides the two
-- players from each other both ways (brief 10), so every read of this table is
-- two lookups, which is what the second index is for. Unblocking deletes the
-- row and nothing besides, which is how it gives back the posts, conversations
-- and notifications the block hid.

create table block
    ( blocker_id integer not null references player(id) on delete cascade
    , blocked_id integer not null references player(id) on delete cascade
    , created timestamptz not null default current_timestamp
    , primary key (blocker_id, blocked_id)
    , constraint block_not_self check (blocker_id <> blocked_id)
    );

create index block_blocked_id_idx on block (blocked_id);

-- A conversation is between a post's owner and one other player, about that
-- post (brief 10). Two players who each message the other's post have two.
--
-- The owner is post.player_id, so nothing here keeps the owner out of the
-- messager's seat: Postgres cannot reach through the reference to check it, and
-- it is the application's to keep honest, as post.regions is.

create table conversation
    ( id serial not null primary key
    , post_id integer not null references post(id) on delete cascade
    , messager_id integer not null references player(id) on delete cascade

    -- Where each side has read to. The unread line in the thread, the inbox's
    -- dot, the header's count of unread conversations (brief 11.4) and whether
    -- a new message sends an email, which happens only when the recipient has
    -- nothing unread already (brief 10), all ask the same question of these:
    -- has the other side written since. Answering it reads message as well,
    -- since a read mark cannot say who wrote last.
    --
    -- Keeping a last message time here would not spare that read: the inbox row
    -- shows the last message's text, so it joins message anyway, and a mark kept
    -- here drifts the moment someone replies without opening the thread.
    , owner_read_at timestamptz
    , messager_read_at timestamptz

    , unique (post_id, messager_id)
    );

create index conversation_messager_id_idx on conversation (messager_id);

create table message
    ( id serial not null primary key
    , conversation_id integer not null references conversation(id) on delete cascade
    , sender_id integer not null references player(id) on delete cascade
    , content text[] not null
    , created timestamptz not null default current_timestamp
    -- A backstop behind the API's validation, as the community summary's is: an
    -- empty message is a blank turn in the thread.
    , constraint message_content_check check (array_to_string(content, '') ~ '\S')
    );

create index message_conversation_id_created_idx on message (conversation_id, created);

-- The sender is one of the conversation's two players, so this cascade never
-- fires on its own: the owner takes the post and the messager the conversation,
-- and either way the messages are already gone. The index is for the check that
-- proves there is nothing left to do, which without it reads every message in
-- the table whenever a player is deleted.
create index message_sender_id_idx on message (sender_id);

-- A stored copy of what was mailed to the admin (brief 10). A report comes from
-- a post's contact panel or from a conversation, which is about a post itself,
-- so a post is what every report is against, and deleting one takes its reports
-- with it as it takes its conversations.

create table report
    ( id serial not null primary key
    , reporter_id integer not null references player(id) on delete cascade
    , reported_id integer not null references player(id) on delete cascade
    , post_id integer not null references post(id) on delete cascade
    , reason text not null
    , detail text
    , created timestamptz not null default current_timestamp
    , constraint report_reason_check check
        (reason in ('spam', 'harassment', 'selling', 'other'))
    );

create index report_reported_id_idx on report (reported_id);

-- Notifications
--
-- Grouped under the player's own post they are about (brief 11.3). A row is
-- about two posts, the owner's and the one that fits it, and deleting either
-- takes the row with it.

create table notification
    ( id serial not null primary key
    -- The owner's post this is about, which the list groups it under and whose
    -- player_id is the bell it counts on.
    , post_id integer not null references post(id) on delete cascade
    , kind text not null -- 'fit', 'expiry'
    -- When it last fired, which orders the list. A fit that comes back
    -- refreshes this rather than adding a row (notification_fit_key below).
    , created timestamptz not null default current_timestamp
    , read boolean not null default false

    -- The fitting post, for a 'fit' row. What the row is headed with, its type
    -- and its freshness are read from the post itself, so the row keeps no copy
    -- of them and cannot outlive what it describes.
    , fitting_post_id integer references post(id) on delete cascade

    , constraint notification_kind_check check (kind in ('fit', 'expiry'))
    -- A fit row names a fitting post, an expiry row names none.
    , constraint notification_fit_post_check check
        ((kind = 'fit') = (fitting_post_id is not null))
    );

create index notification_post_id_created_idx on notification (post_id, created desc);

-- Deleting a post reads this to take the notifications about it with it. The
-- unique index below cannot serve that: it is partial, and the delete knows
-- only the post.
create index notification_fitting_post_id_idx on notification (fitting_post_id);

-- One expiry row per post, never a second; renewing the post deletes it
-- (brief 11.3), and a firing that finds one still there refreshes it rather
-- than failing on this.
create unique index notification_expiry_key on notification (post_id) where kind = 'expiry';

-- One row per fitting post, however often it fits. A post renewed after it
-- expired fits again (brief 8), and that firing upserts on this index, setting
-- created to now and read back to false: the row returns to the top of the list
-- unread, where a second row would say the same thing twice.
create unique index notification_fit_key on notification (post_id, fitting_post_id) where kind = 'fit';

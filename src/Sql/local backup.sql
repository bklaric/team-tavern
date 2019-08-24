--
-- PostgreSQL database dump
--

-- Dumped from database version 11.5 (Ubuntu 11.5-0ubuntu0.19.04.1)
-- Dumped by pg_dump version 11.5 (Ubuntu 11.5-0ubuntu0.19.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: field; Type: TABLE; Schema: public; Owner: bklaric
--

CREATE TABLE public.field (
    id integer NOT NULL,
    game_id integer NOT NULL,
    type integer NOT NULL,
    label character varying(40) NOT NULL,
    data jsonb NOT NULL,
    key character varying(40) DEFAULT 'aaaa'::character varying NOT NULL
);


ALTER TABLE public.field OWNER TO bklaric;

--
-- Name: field_id_seq; Type: SEQUENCE; Schema: public; Owner: bklaric
--

CREATE SEQUENCE public.field_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.field_id_seq OWNER TO bklaric;

--
-- Name: field_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: bklaric
--

ALTER SEQUENCE public.field_id_seq OWNED BY public.field.id;


--
-- Name: field_value; Type: TABLE; Schema: public; Owner: bklaric
--

CREATE TABLE public.field_value (
    id integer NOT NULL,
    profile_id integer NOT NULL,
    field_id integer NOT NULL,
    data jsonb NOT NULL
);


ALTER TABLE public.field_value OWNER TO bklaric;

--
-- Name: field_value_id_seq; Type: SEQUENCE; Schema: public; Owner: bklaric
--

CREATE SEQUENCE public.field_value_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.field_value_id_seq OWNER TO bklaric;

--
-- Name: field_value_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: bklaric
--

ALTER SEQUENCE public.field_value_id_seq OWNED BY public.field_value.id;


--
-- Name: game; Type: TABLE; Schema: public; Owner: bklaric
--

CREATE TABLE public.game (
    id integer NOT NULL,
    administrator_id integer NOT NULL,
    title character varying(50) NOT NULL,
    handle character varying(50) NOT NULL,
    description text[] NOT NULL,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE public.game OWNER TO bklaric;

--
-- Name: game_id_seq; Type: SEQUENCE; Schema: public; Owner: bklaric
--

CREATE SEQUENCE public.game_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.game_id_seq OWNER TO bklaric;

--
-- Name: game_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: bklaric
--

ALTER SEQUENCE public.game_id_seq OWNED BY public.game.id;


--
-- Name: player; Type: TABLE; Schema: public; Owner: bklaric
--

CREATE TABLE public.player (
    id integer NOT NULL,
    email character varying(254) NOT NULL,
    nickname character varying(40) NOT NULL,
    password_hash character(60) NOT NULL,
    confirmation_nonce character(20) NOT NULL,
    email_confirmed boolean DEFAULT false NOT NULL,
    about text[] DEFAULT '{}'::text[] NOT NULL,
    registered timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE public.player OWNER TO bklaric;

--
-- Name: player_id_seq; Type: SEQUENCE; Schema: public; Owner: bklaric
--

CREATE SEQUENCE public.player_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.player_id_seq OWNER TO bklaric;

--
-- Name: player_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: bklaric
--

ALTER SEQUENCE public.player_id_seq OWNED BY public.player.id;


--
-- Name: profile; Type: TABLE; Schema: public; Owner: bklaric
--

CREATE TABLE public.profile (
    id integer NOT NULL,
    player_id integer NOT NULL,
    game_id integer NOT NULL,
    summary text[] NOT NULL,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE public.profile OWNER TO bklaric;

--
-- Name: profile_id_seq; Type: SEQUENCE; Schema: public; Owner: bklaric
--

CREATE SEQUENCE public.profile_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.profile_id_seq OWNER TO bklaric;

--
-- Name: profile_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: bklaric
--

ALTER SEQUENCE public.profile_id_seq OWNED BY public.profile.id;


--
-- Name: session; Type: TABLE; Schema: public; Owner: bklaric
--

CREATE TABLE public.session (
    id integer NOT NULL,
    player_id integer NOT NULL,
    token character(40) NOT NULL,
    revoked boolean DEFAULT false NOT NULL,
    generated timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE public.session OWNER TO bklaric;

--
-- Name: session_id_seq; Type: SEQUENCE; Schema: public; Owner: bklaric
--

CREATE SEQUENCE public.session_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.session_id_seq OWNER TO bklaric;

--
-- Name: session_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: bklaric
--

ALTER SEQUENCE public.session_id_seq OWNED BY public.session.id;


--
-- Name: field id; Type: DEFAULT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.field ALTER COLUMN id SET DEFAULT nextval('public.field_id_seq'::regclass);


--
-- Name: field_value id; Type: DEFAULT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.field_value ALTER COLUMN id SET DEFAULT nextval('public.field_value_id_seq'::regclass);


--
-- Name: game id; Type: DEFAULT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.game ALTER COLUMN id SET DEFAULT nextval('public.game_id_seq'::regclass);


--
-- Name: player id; Type: DEFAULT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.player ALTER COLUMN id SET DEFAULT nextval('public.player_id_seq'::regclass);


--
-- Name: profile id; Type: DEFAULT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.profile ALTER COLUMN id SET DEFAULT nextval('public.profile_id_seq'::regclass);


--
-- Name: session id; Type: DEFAULT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.session ALTER COLUMN id SET DEFAULT nextval('public.session_id_seq'::regclass);


--
-- Data for Name: field; Type: TABLE DATA; Schema: public; Owner: bklaric
--

COPY public.field (id, game_id, type, label, data, key) FROM stdin;
1	1	1	Steam profile	{}	steam-profile
2	1	2	Rank	{"options": [{"id": 1, "key": "herald", "option": "Herald"}, {"id": 2, "key": "guardian", "option": "Guardian"}, {"id": 3, "key": "crusader", "option": "Crusader"}, {"id": 4, "key": "archon", "option": "Archon"}, {"id": 5, "key": "legend", "option": "Legend"}, {"id": 6, "key": "ancient", "option": "Ancient"}, {"id": 7, "key": "divine", "option": "Divine"}, {"id": 8, "key": "immortal", "option": "Immortal"}]}	rank
6	1	3	Position	{"options": [{"id": 1, "key": "safe-lane", "option": "Safe lane"}, {"id": 2, "key": "mid-lane", "option": "Mid lane"}, {"id": 3, "key": "off-lane", "option": "Off lane"}, {"id": 4, "key": "soft-support", "option": "Soft support"}, {"id": 5, "key": "hard support", "option": "Hard support"}]}	position
\.


--
-- Data for Name: field_value; Type: TABLE DATA; Schema: public; Owner: bklaric
--

COPY public.field_value (id, profile_id, field_id, data) FROM stdin;
24	1	6	{"optionIds": [1, 4]}
25	1	2	{"optionId": 1}
26	1	1	{"url": "https://steamcommunity.com/id/Houndolon/"}
\.


--
-- Data for Name: game; Type: TABLE DATA; Schema: public; Owner: bklaric
--

COPY public.game (id, administrator_id, title, handle, description, created) FROM stdin;
1	1	Dota 2	dota2	{aoeuaoeu}	2019-08-17 16:31:26.444589+02
2	1	Test game	ueueueue	{uueeuue}	2019-08-18 17:08:57.590107+02
4	1	uueueue	ieouioeuioeuioeui	{ueueueue}	2019-08-18 17:24:41.937182+02
\.


--
-- Data for Name: player; Type: TABLE DATA; Schema: public; Owner: bklaric
--

COPY public.player (id, email, nickname, password_hash, confirmation_nonce, email_confirmed, about, registered) FROM stdin;
1	branimir.klaric.bk@gmail.com	bklaric	$2b$10$o5wAu4jQSmGjHo7X4pSfEuGGgcBmzB3uXke9H.2IuEhj1bf9GYRJa	384527e7b0ebf55016b4	t	{}	2019-08-17 16:23:07.558151+02
\.


--
-- Data for Name: profile; Type: TABLE DATA; Schema: public; Owner: bklaric
--

COPY public.profile (id, player_id, game_id, summary, created) FROM stdin;
1	1	1	{"aoeuaeou aoeu aoe ue"}	2019-08-17 17:03:39.232836+02
\.


--
-- Data for Name: session; Type: TABLE DATA; Schema: public; Owner: bklaric
--

COPY public.session (id, player_id, token, revoked, generated) FROM stdin;
1	1	ae6243d660326f83af6f33e7a54212c90f8457b1	f	2019-08-17 16:23:47.749482+02
2	1	fc708aa8a69e23584d17903d3d3e81182ff0c894	f	2019-08-17 16:31:19.764405+02
3	1	629b55da98082228052e380dbd389cbe83b9a80d	f	2019-08-17 17:25:12.002666+02
\.


--
-- Name: field_id_seq; Type: SEQUENCE SET; Schema: public; Owner: bklaric
--

SELECT pg_catalog.setval('public.field_id_seq', 6, true);


--
-- Name: field_value_id_seq; Type: SEQUENCE SET; Schema: public; Owner: bklaric
--

SELECT pg_catalog.setval('public.field_value_id_seq', 26, true);


--
-- Name: game_id_seq; Type: SEQUENCE SET; Schema: public; Owner: bklaric
--

SELECT pg_catalog.setval('public.game_id_seq', 4, true);


--
-- Name: player_id_seq; Type: SEQUENCE SET; Schema: public; Owner: bklaric
--

SELECT pg_catalog.setval('public.player_id_seq', 1, true);


--
-- Name: profile_id_seq; Type: SEQUENCE SET; Schema: public; Owner: bklaric
--

SELECT pg_catalog.setval('public.profile_id_seq', 1, true);


--
-- Name: session_id_seq; Type: SEQUENCE SET; Schema: public; Owner: bklaric
--

SELECT pg_catalog.setval('public.session_id_seq', 3, true);


--
-- Name: field field_pkey; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.field
    ADD CONSTRAINT field_pkey PRIMARY KEY (id);


--
-- Name: field_value field_value_pkey; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.field_value
    ADD CONSTRAINT field_value_pkey PRIMARY KEY (id);


--
-- Name: game game_handle_key; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.game
    ADD CONSTRAINT game_handle_key UNIQUE (handle);


--
-- Name: game game_pkey; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.game
    ADD CONSTRAINT game_pkey PRIMARY KEY (id);


--
-- Name: game game_title_key; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.game
    ADD CONSTRAINT game_title_key UNIQUE (title);


--
-- Name: player player_email_key; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.player
    ADD CONSTRAINT player_email_key UNIQUE (email);


--
-- Name: player player_nickname_key; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.player
    ADD CONSTRAINT player_nickname_key UNIQUE (nickname);


--
-- Name: player player_pkey; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.player
    ADD CONSTRAINT player_pkey PRIMARY KEY (id);


--
-- Name: profile profile_game_id_player_id_key; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.profile
    ADD CONSTRAINT profile_game_id_player_id_key UNIQUE (game_id, player_id);


--
-- Name: profile profile_pkey; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.profile
    ADD CONSTRAINT profile_pkey PRIMARY KEY (id);


--
-- Name: session session_pkey; Type: CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.session
    ADD CONSTRAINT session_pkey PRIMARY KEY (id);


--
-- Name: field field_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.field
    ADD CONSTRAINT field_game_id_fkey FOREIGN KEY (game_id) REFERENCES public.game(id);


--
-- Name: field_value field_value_field_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.field_value
    ADD CONSTRAINT field_value_field_id_fkey FOREIGN KEY (field_id) REFERENCES public.field(id);


--
-- Name: field_value field_value_profile_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.field_value
    ADD CONSTRAINT field_value_profile_id_fkey FOREIGN KEY (profile_id) REFERENCES public.profile(id);


--
-- Name: game game_administrator_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.game
    ADD CONSTRAINT game_administrator_id_fkey FOREIGN KEY (administrator_id) REFERENCES public.player(id);


--
-- Name: profile profile_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.profile
    ADD CONSTRAINT profile_game_id_fkey FOREIGN KEY (game_id) REFERENCES public.game(id);


--
-- Name: profile profile_player_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.profile
    ADD CONSTRAINT profile_player_id_fkey FOREIGN KEY (player_id) REFERENCES public.player(id);


--
-- Name: session session_player_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: bklaric
--

ALTER TABLE ONLY public.session
    ADD CONSTRAINT session_player_id_fkey FOREIGN KEY (player_id) REFERENCES public.player(id);


--
-- PostgreSQL database dump complete
--

-- The regions (brief 5). A region is the set of places close enough to each
-- other to play together; language is the languages field's business, so a
-- split that is really about language is not one of these.
--
-- seed-countries.sql holds the countries that map onto them, one region each.

insert into region (name, ordinal) values
    ('Europe', 1),
    ('Middle East', 2),
    ('North Africa', 3),
    ('Sub-Saharan Africa', 4),
    ('North America', 5),
    ('Central America', 6),
    ('South America', 7),
    ('Central Asia', 8),
    ('South Asia', 9),
    ('East Asia', 10),
    ('Southeast Asia', 11),
    ('Oceania', 12);

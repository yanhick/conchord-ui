CREATE TABLE song (
    id bigserial PRIMARY KEY,
    title varchar(200) NOT NULL,
    artist varchar(200) NOT NULL,
    album varchar(200) NOT NUlL,
    year integer NOT NULL,
    content text NOT NULL
);

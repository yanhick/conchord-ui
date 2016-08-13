// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Data_String = require("../Data.String");
var Data_Traversable = require("../Data.Traversable");
var Data_Array = require("../Data.Array");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Either = require("../Data.Either");
var Data_Generic = require("../Data.Generic");
var Data_Foreign_Class = require("../Data.Foreign.Class");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_String = require("../Text.Parsing.StringParser.String");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var Database_Postgres = require("../Database.Postgres");
var Database_Postgres_SqlValue = require("../Database.Postgres.SqlValue");
var Model = require("../Model");
var Control_Apply = require("../Control.Apply");
var Data_Unit = require("../Data.Unit");
var Control_Bind = require("../Control.Bind");
var Data_Function = require("../Data.Function");
var Control_Applicative = require("../Control.Applicative");
var Data_Show = require("../Data.Show");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_List = require("../Data.List");
var Data_Foreign_Index = require("../Data.Foreign.Index");
var Data_Functor = require("../Data.Functor");
var SongTableRow = function (x) {
    return x;
};
var updateSongQuery = "\n\n    UPDATE song\n    SET title = $1,\n        artist = $2,\n        album = $3,\n        year = $4,\n        content = $5\n    WHERE id = $6\n    RETURNING *\n\n";
var songTableRowToDBSong = function (v) {
    return Control_Bind.bind(Data_Either.bindEither)(Text_Parsing_StringParser.runParser(Model.parseSong)(v.content))(function (v1) {
        return Data_Function.apply(Control_Applicative.pure(Data_Either.applicativeEither))({
            id: v.id, 
            song: v1
        });
    });
};
var mkConnection = (function () {
    var charsToString = function ($70) {
        return Data_String.fromCharArray(Data_Array.fromFoldable(Data_List.foldableList)($70));
    };
    var charsToInt = function ($71) {
        return Data_Int.fromString(charsToString($71));
    };
    return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_String.string("postgres://"))(function () {
        return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_Combinators.manyTill(Text_Parsing_StringParser_String.anyChar)(Text_Parsing_StringParser_String.string(":")))(function (v) {
            return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_Combinators.manyTill(Text_Parsing_StringParser_String.anyChar)(Text_Parsing_StringParser_String.string("@")))(function (v1) {
                return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_Combinators.manyTill(Text_Parsing_StringParser_String.anyChar)(Text_Parsing_StringParser_String.string(":")))(function (v2) {
                    return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_Combinators.manyTill(Text_Parsing_StringParser_String.anyDigit)(Text_Parsing_StringParser_String.string("/")))(function (v3) {
                        return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_Combinators.manyTill(Text_Parsing_StringParser_String.anyChar)(Text_Parsing_StringParser_String.eof))(function (v4) {
                            return Control_Bind.bind(Text_Parsing_StringParser.bindParser)((function () {
                                var $33 = charsToInt(v3);
                                if ($33 instanceof Data_Maybe.Nothing) {
                                    return Text_Parsing_StringParser.fail("port is not a valid integer");
                                };
                                if ($33 instanceof Data_Maybe.Just) {
                                    return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)($33.value0);
                                };
                                throw new Error("Failed pattern match at DB line 42, column 14 - line 44, column 35: " + [ $33.constructor.name ]);
                            })())(function (v5) {
                                return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)({
                                    host: charsToString(v2), 
                                    db: charsToString(v4), 
                                    port: v5, 
                                    user: charsToString(v), 
                                    password: charsToString(v1)
                                });
                            });
                        });
                    });
                });
            });
        });
    });
})();
var localConnectionInfo = {
    host: "localhost", 
    db: "test", 
    port: 5432, 
    user: "testuser", 
    password: "test"
};
var isForeignSongTableRow = new Data_Foreign_Class.IsForeign(function (value) {
    return Control_Bind.bind(Data_Either.bindEither)(Data_Foreign_Class.readProp(Data_Foreign_Class.stringIsForeign)(Data_Foreign_Index.indexString)("id")(value))(function (v) {
        return Control_Bind.bind(Data_Either.bindEither)(Data_Foreign_Class.readProp(Data_Foreign_Class.stringIsForeign)(Data_Foreign_Index.indexString)("title")(value))(function (v1) {
            return Control_Bind.bind(Data_Either.bindEither)(Data_Foreign_Class.readProp(Data_Foreign_Class.stringIsForeign)(Data_Foreign_Index.indexString)("artist")(value))(function (v2) {
                return Control_Bind.bind(Data_Either.bindEither)(Data_Foreign_Class.readProp(Data_Foreign_Class.stringIsForeign)(Data_Foreign_Index.indexString)("album")(value))(function (v3) {
                    return Control_Bind.bind(Data_Either.bindEither)(Data_Foreign_Class.readProp(Data_Foreign_Class.intIsForeign)(Data_Foreign_Index.indexString)("year")(value))(function (v4) {
                        return Control_Bind.bind(Data_Either.bindEither)(Data_Foreign_Class.readProp(Data_Foreign_Class.stringIsForeign)(Data_Foreign_Index.indexString)("content")(value))(function (v5) {
                            return Data_Function.apply(Control_Applicative.pure(Data_Either.applicativeEither))({
                                id: v, 
                                title: v1, 
                                artist: v2, 
                                album: v3, 
                                year: v4, 
                                content: v5
                            });
                        });
                    });
                });
            });
        });
    });
});
var updateSong = function (c) {
    return function (id) {
        return function (v) {
            return Database_Postgres.withConnection(c)(function (client) {
                return Control_Bind.bind(Control_Monad_Aff.bindAff)(Database_Postgres.queryOne(isForeignSongTableRow)(updateSongQuery)([ Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueString)(v.meta.title), Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueString)(v.meta.artist), Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueString)(v.meta.album), Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueInt)(v.meta.year), Data_Function.apply(Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueString))(Model.serializeSong(v)), Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueInt)(id) ])(client))(function (v1) {
                    return Data_Function.apply(Control_Applicative.pure(Control_Monad_Aff.applicativeAff))(Data_Functor.map(Data_Maybe.functorMaybe)(songTableRowToDBSong)(v1));
                });
            });
        };
    };
};
var getSongByIdQuery = "\n\n    SELECT content\n    FROM song\n    WHERE id = $1\n\n";
var getSongById = function (c) {
    return function (id) {
        return Database_Postgres.withConnection(c)(function (client) {
            return Control_Bind.bind(Control_Monad_Aff.bindAff)(Database_Postgres.queryValue(Data_Foreign_Class.stringIsForeign)(getSongByIdQuery)([ Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueInt)(id) ])(client))(function (v) {
                return Data_Function.apply(Control_Applicative.pure(Control_Monad_Aff.applicativeAff))((function () {
                    if (v instanceof Data_Maybe.Just) {
                        return v.value0;
                    };
                    if (v instanceof Data_Maybe.Nothing) {
                        return "";
                    };
                    throw new Error("Failed pattern match at DB line 59, column 9 - line 63, column 1: " + [ v.constructor.name ]);
                })());
            });
        });
    };
};
var getSearchResultsQuery = "\n\n    SELECT id, title, artist, album, year, content\n    FROM\n    (SELECT song.id as id,\n            song.title as title,\n            song.artist as artist,\n            song.album as album,\n            song.year as year,\n            song.content as content,\n            to_tsvector(song.content) as document\n            FROM song) as doc\n    WHERE document @@ to_tsquery($1)\n\n";
var getSearchResults = function (c) {
    return function (q) {
        return Database_Postgres.withConnection(c)(function (client) {
            return Control_Bind.bind(Control_Monad_Aff.bindAff)(Database_Postgres.query(isForeignSongTableRow)(getSearchResultsQuery)([ Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueString)(q) ])(client))(function (v) {
                return Data_Function.apply(Control_Applicative.pure(Control_Monad_Aff.applicativeAff))(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Either.applicativeEither)(songTableRowToDBSong)(v));
            });
        });
    };
};
var genericSongTableRow = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "DB.SongTableRow" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(SongTableRow))((function (r) {
            if (r instanceof Data_Generic.SRecord && r.value0.length === 6) {
                return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (album1) {
                    return function (artist1) {
                        return function (content1) {
                            return function (id1) {
                                return function (title1) {
                                    return function (year1) {
                                        return {
                                            album: album1, 
                                            artist: artist1, 
                                            content: content1, 
                                            id: id1, 
                                            title: title1, 
                                            year: year1
                                        };
                                    };
                                };
                            };
                        };
                    };
                }))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[0]).recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[1]).recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[2]).recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[3]).recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[4]).recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[5]).recValue(Data_Unit.unit)));
            };
            return Data_Maybe.Nothing.value;
        })(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("DB.SongTableRow", [ {
        sigConstructor: "DB.SongTableRow", 
        sigValues: [ function ($dollarq1) {
            return new Data_Generic.SigRecord([ {
                recLabel: "album", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                }
            }, {
                recLabel: "artist", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                }
            }, {
                recLabel: "content", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                }
            }, {
                recLabel: "id", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                }
            }, {
                recLabel: "title", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                }
            }, {
                recLabel: "year", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                }
            } ]);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("DB.SongTableRow", [ function ($dollarq) {
        return new Data_Generic.SRecord([ {
            recLabel: "album", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericString)(v.album);
            }
        }, {
            recLabel: "artist", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericString)(v.artist);
            }
        }, {
            recLabel: "content", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericString)(v.content);
            }
        }, {
            recLabel: "id", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericString)(v.id);
            }
        }, {
            recLabel: "title", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericString)(v.title);
            }
        }, {
            recLabel: "year", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericInt)(v.year);
            }
        } ]);
    } ]);
});
var showSongTableRow = new Data_Show.Show(Data_Generic.gShow(genericSongTableRow));
var deleteSongQuery = "\n\n    DELETE FROM song\n    WHERE id = $1\n\n";
var deleteSong = function (c) {
    return function (id) {
        return Database_Postgres.withConnection(c)(function (client) {
            return Database_Postgres.execute(deleteSongQuery)([ Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueInt)(id) ])(client);
        });
    };
};
var createSongQuery = "\n\n    INSERT into song\n    VALUES(default, $1, $2, $3, $4, $5)\n    RETURNING *\n\n";
var createSong = function (c) {
    return function (v) {
        return Database_Postgres.withConnection(c)(function (client) {
            return Control_Bind.bind(Control_Monad_Aff.bindAff)(Database_Postgres.queryOne(isForeignSongTableRow)(createSongQuery)([ Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueString)(v.meta.title), Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueString)(v.meta.artist), Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueString)(v.meta.album), Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueInt)(v.meta.year), Data_Function.apply(Database_Postgres_SqlValue.toSql(Database_Postgres_SqlValue.isSqlValueString))(Model.serializeSong(v)) ])(client))(function (v1) {
                return Data_Function.apply(Control_Applicative.pure(Control_Monad_Aff.applicativeAff))(Data_Functor.map(Data_Maybe.functorMaybe)(songTableRowToDBSong)(v1));
            });
        });
    };
};
module.exports = {
    SongTableRow: SongTableRow, 
    createSong: createSong, 
    createSongQuery: createSongQuery, 
    deleteSong: deleteSong, 
    deleteSongQuery: deleteSongQuery, 
    getSearchResults: getSearchResults, 
    getSearchResultsQuery: getSearchResultsQuery, 
    getSongById: getSongById, 
    getSongByIdQuery: getSongByIdQuery, 
    localConnectionInfo: localConnectionInfo, 
    mkConnection: mkConnection, 
    songTableRowToDBSong: songTableRowToDBSong, 
    updateSong: updateSong, 
    updateSongQuery: updateSongQuery, 
    genericSongTableRow: genericSongTableRow, 
    isForeignSongTableRow: isForeignSongTableRow, 
    showSongTableRow: showSongTableRow
};

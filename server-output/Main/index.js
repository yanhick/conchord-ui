// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Int = require("../Data.Int");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Tuple = require("../Data.Tuple");
var Data_Foreign_EasyFFI = require("../Data.Foreign.EasyFFI");
var Data_Foreign_Generic = require("../Data.Foreign.Generic");
var Data_Either = require("../Data.Either");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Unsafe = require("../Control.Monad.Eff.Unsafe");
var Database_Postgres = require("../Database.Postgres");
var Node_Express_App = require("../Node.Express.App");
var Node_Express_Types = require("../Node.Express.Types");
var Node_Express_Handler = require("../Node.Express.Handler");
var Node_Express_Request = require("../Node.Express.Request");
var Node_Express_Response = require("../Node.Express.Response");
var Node_HTTP = require("../Node.HTTP");
var Network_HTTP_Affjax = require("../Network.HTTP.Affjax");
var DOM = require("../DOM");
var Signal_Channel = require("../Signal.Channel");
var Pux = require("../Pux");
var Signal = require("../Signal");
var Route = require("../Route");
var App = require("../App");
var Action = require("../Action");
var View = require("../View");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Model = require("../Model");
var DB = require("../DB");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Bind = require("../Control.Bind");
var Data_Function = require("../Data.Function");
var Data_Show = require("../Data.Show");
var Data_Generic = require("../Data.Generic");
var Control_Applicative = require("../Control.Applicative");
var Data_Foreign_Class = require("../Data.Foreign.Class");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Category = require("../Control.Category");
var songApiHandler = function (c) {
    var err = function ($139) {
        return Node_Express_Handler.nextThrow(Control_Monad_Eff_Exception.error($139));
    };
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getRouteParam(Node_Express_Types.requestParamString)("id"))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return err("Id is required");
        };
        if (v instanceof Data_Maybe.Just) {
            var $29 = Data_Int.fromString(v.value0);
            if ($29 instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Aff_Class.liftAff(Node_Express_Handler.monadAffHandlerM))(DB.getSongById(c)($29.value0)))(function (v1) {
                    var s$prime = Text_Parsing_StringParser.runParser(Model.parseSong)(v1);
                    if (s$prime instanceof Data_Either.Right) {
                        return Data_Function.apply(Node_Express_Response.send)(Data_Foreign_Generic.toJSONGeneric(Model.genericSong)(Data_Foreign_Generic.defaultOptions)(s$prime.value0));
                    };
                    if (s$prime instanceof Data_Either.Left) {
                        return err(Data_Show.show(Text_Parsing_StringParser.showParseError)(s$prime.value0));
                    };
                    throw new Error("Failed pattern match at Main line 324, column 13 - line 326, column 37: " + [ s$prime.constructor.name ]);
                });
            };
            if ($29 instanceof Data_Maybe.Nothing) {
                return err("Id is not a valid integer");
            };
            throw new Error("Failed pattern match at Main line 320, column 9 - line 328, column 5: " + [ $29.constructor.name ]);
        };
        throw new Error("Failed pattern match at Main line 317, column 5 - line 328, column 5: " + [ v.constructor.name ]);
    });
};
var searchApiHandler = function (c) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getQueryParam("q"))(function (v) {
        if (v instanceof Data_Maybe.Just) {
            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Aff_Class.liftAff(Node_Express_Handler.monadAffHandlerM))(DB.getSearchResults(c)(v.value0)))(function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                        return Data_Function.apply(Node_Express_Response.send)(Data_Show.show(Text_Parsing_StringParser.showParseError)(v1.value0));
                    });
                };
                if (v1 instanceof Data_Either.Right) {
                    return Data_Function.apply(Node_Express_Response.send)(Data_Function.apply(Data_Foreign_Generic.toJSONGeneric(Data_Generic.genericArray(Model.genericDBSong))(Data_Foreign_Generic.defaultOptions))(v1.value0));
                };
                throw new Error("Failed pattern match at Main line 306, column 11 - line 310, column 65: " + [ v1.constructor.name ]);
            });
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("missing query param"));
        };
        throw new Error("Failed pattern match at Main line 303, column 5 - line 314, column 1: " + [ v.constructor.name ]);
    });
};
var renderApp = function (s) {
    return function __do() {
        var v = Pux.start({
            initialState: s, 
            update: Action.update, 
            view: View.view, 
            inputs: [  ]
        })();
        return Pux.renderToString(v.html)();
    };
};
var renderAppHandler = function (s) {
    return Data_Function.apply(Control_Monad_Eff_Unsafe.unsafePerformEff)(Control_Monad_Eff_Exception.catchException(function (v) {
        return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)("");
    })(renderApp(s)));
};
var putUpdateSongPageHandler = function (c) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getRouteParam(Node_Express_Types.requestParamString)("id"))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is required"));
        };
        if (v instanceof Data_Maybe.Just) {
            var $47 = Data_Int.fromString(v.value0);
            if ($47 instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getBodyParam(Data_Foreign_Class.stringIsForeign)("song"))(function (v1) {
                    if (v1 instanceof Data_Maybe.Just) {
                        var $50 = Text_Parsing_StringParser.runParser(Model.parseSong)(v1.value0);
                        if ($50 instanceof Data_Either.Left) {
                            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                                return Node_Express_Response.send($50.value0);
                            });
                        };
                        if ($50 instanceof Data_Either.Right) {
                            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Aff_Class.liftAff(Node_Express_Handler.monadAffHandlerM))(DB.updateSong(c)($47.value0)($50.value0)))(function (v2) {
                                if (v2 instanceof Data_Maybe.Nothing) {
                                    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                                        return Node_Express_Response.send("No song");
                                    });
                                };
                                if (v2 instanceof Data_Maybe.Just) {
                                    if (v2.value0 instanceof Data_Either.Left) {
                                        return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                                            return Node_Express_Response.send(v2.value0.value0);
                                        });
                                    };
                                    if (v2.value0 instanceof Data_Either.Right) {
                                        return Data_Function.apply(Node_Express_Response.redirect)("/song/" + v2.value0.value0.id);
                                    };
                                    throw new Error("Failed pattern match at Main line 144, column 44 - line 149, column 77: " + [ v2.value0.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Main line 140, column 27 - line 149, column 77: " + [ v2.constructor.name ]);
                            });
                        };
                        throw new Error("Failed pattern match at Main line 134, column 21 - line 149, column 77: " + [ $50.constructor.name ]);
                    };
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                            return Node_Express_Response.send("No Song was sent");
                        });
                    };
                    throw new Error("Failed pattern match at Main line 132, column 17 - line 154, column 13: " + [ v1.constructor.name ]);
                });
            };
            if ($47 instanceof Data_Maybe.Nothing) {
                return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is not a valid integer"));
            };
            throw new Error("Failed pattern match at Main line 129, column 9 - line 156, column 1: " + [ $47.constructor.name ]);
        };
        throw new Error("Failed pattern match at Main line 126, column 5 - line 156, column 1: " + [ v.constructor.name ]);
    });
};
var putUpdateSongApiHandler = function (c) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getRouteParam(Node_Express_Types.requestParamString)("id"))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is required"));
        };
        if (v instanceof Data_Maybe.Just) {
            var $65 = Data_Int.fromString(v.value0);
            if ($65 instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getBodyParam(Data_Foreign_Class.stringIsForeign)("song"))(function (v1) {
                    if (v1 instanceof Data_Maybe.Just) {
                        var $68 = Text_Parsing_StringParser.runParser(Model.parseSong)(v1.value0);
                        if ($68 instanceof Data_Either.Left) {
                            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                                return Node_Express_Response.send($68.value0);
                            });
                        };
                        if ($68 instanceof Data_Either.Right) {
                            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Aff_Class.liftAff(Node_Express_Handler.monadAffHandlerM))(DB.updateSong(c)($65.value0)($68.value0)))(function (v2) {
                                if (v2 instanceof Data_Maybe.Nothing) {
                                    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                                        return Node_Express_Response.send("No song");
                                    });
                                };
                                if (v2 instanceof Data_Maybe.Just) {
                                    if (v2.value0 instanceof Data_Either.Left) {
                                        return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                                            return Node_Express_Response.send(v2.value0.value0);
                                        });
                                    };
                                    if (v2.value0 instanceof Data_Either.Right) {
                                        return Data_Function.apply(Node_Express_Response.send)(Data_Foreign_Generic.toJSONGeneric(Model.genericDBSong)(Data_Foreign_Generic.defaultOptions)(v2.value0.value0));
                                    };
                                    throw new Error("Failed pattern match at Main line 177, column 44 - line 181, column 105: " + [ v2.value0.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Main line 173, column 27 - line 181, column 105: " + [ v2.constructor.name ]);
                            });
                        };
                        throw new Error("Failed pattern match at Main line 167, column 21 - line 181, column 105: " + [ $68.constructor.name ]);
                    };
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                            return Node_Express_Response.send("No Song was sent");
                        });
                    };
                    throw new Error("Failed pattern match at Main line 165, column 17 - line 185, column 13: " + [ v1.constructor.name ]);
                });
            };
            if ($65 instanceof Data_Maybe.Nothing) {
                return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is not a valid integer"));
            };
            throw new Error("Failed pattern match at Main line 162, column 9 - line 188, column 1: " + [ $65.constructor.name ]);
        };
        throw new Error("Failed pattern match at Main line 159, column 5 - line 188, column 1: " + [ v.constructor.name ]);
    });
};
var postNewSongPageHandler = function (c) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getBodyParam(Data_Foreign_Class.stringIsForeign)("song"))(function (v) {
        if (v instanceof Data_Maybe.Just) {
            var $82 = Text_Parsing_StringParser.runParser(Model.parseSong)(v.value0);
            if ($82 instanceof Data_Either.Left) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                    return Node_Express_Response.send($82.value0);
                });
            };
            if ($82 instanceof Data_Either.Right) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Aff_Class.liftAff(Node_Express_Handler.monadAffHandlerM))(DB.createSong(c)($82.value0)))(function (v1) {
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                            return Node_Express_Response.send("No song");
                        });
                    };
                    if (v1 instanceof Data_Maybe.Just) {
                        if (v1.value0 instanceof Data_Either.Left) {
                            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                                return Node_Express_Response.send(v1.value0.value0);
                            });
                        };
                        if (v1.value0 instanceof Data_Either.Right) {
                            return Data_Function.apply(Node_Express_Response.redirect)("/song/" + v1.value0.value0.id);
                        };
                        throw new Error("Failed pattern match at Main line 241, column 32 - line 246, column 62: " + [ v1.value0.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Main line 237, column 15 - line 246, column 62: " + [ v1.constructor.name ]);
                });
            };
            throw new Error("Failed pattern match at Main line 231, column 9 - line 246, column 62: " + [ $82.constructor.name ]);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                return Node_Express_Response.send("No Song was sent");
            });
        };
        throw new Error("Failed pattern match at Main line 229, column 5 - line 252, column 1: " + [ v.constructor.name ]);
    });
};
var postNewSongApiHandler = function (c) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getBodyParam(Data_Foreign_Class.stringIsForeign)("song"))(function (v) {
        if (v instanceof Data_Maybe.Just) {
            var $95 = Text_Parsing_StringParser.runParser(Model.parseSong)(v.value0);
            if ($95 instanceof Data_Either.Left) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                    return Node_Express_Response.send($95.value0);
                });
            };
            if ($95 instanceof Data_Either.Right) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Aff_Class.liftAff(Node_Express_Handler.monadAffHandlerM))(DB.createSong(c)($95.value0)))(function (v1) {
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                            return Node_Express_Response.send("No song");
                        });
                    };
                    if (v1 instanceof Data_Maybe.Just) {
                        if (v1.value0 instanceof Data_Either.Left) {
                            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                                return Node_Express_Response.send(v1.value0.value0);
                            });
                        };
                        if (v1.value0 instanceof Data_Either.Right) {
                            return Data_Function.apply(Node_Express_Response.send)(Data_Foreign_Generic.toJSONGeneric(Model.genericDBSong)(Data_Foreign_Generic.defaultOptions)(v1.value0.value0));
                        };
                        throw new Error("Failed pattern match at Main line 216, column 32 - line 220, column 93: " + [ v1.value0.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Main line 212, column 15 - line 220, column 93: " + [ v1.constructor.name ]);
                });
            };
            throw new Error("Failed pattern match at Main line 206, column 9 - line 220, column 93: " + [ $95.constructor.name ]);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                return Node_Express_Response.send("No Song was sent");
            });
        };
        throw new Error("Failed pattern match at Main line 204, column 5 - line 226, column 1: " + [ v.constructor.name ]);
    });
};
var index = function (s) {
    return "\n    <!doctype html>\n    <html>\n        <head>\n            <link rel=\"stylesheet\" href=\"/style.css\">\n        </head>\n        <body>\n            <div id=\"app\">" + (renderAppHandler(s) + ("</div>\n            <script>window.puxLastState =  JSON.stringify(" + (Data_Foreign_Generic.toJSONGeneric(App.genericState)(Data_Foreign_Generic.defaultOptions)(s) + ");</script>\n            <script src=\"/app.js\"></script>\n        </body>\n    </html>\n    ")));
};
var searchPageHandler = function (c) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getQueryParam("q"))(function (v) {
        if (v instanceof Data_Maybe.Just) {
            return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Aff_Class.liftAff(Node_Express_Handler.monadAffHandlerM))(DB.getSearchResults(c)(v.value0)))(function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
                        return Node_Express_Response.send(v1.value0);
                    });
                };
                if (v1 instanceof Data_Either.Right) {
                    return Data_Function.apply(Node_Express_Response.send)(index({
                        currentPage: Data_Function.apply(Route.SearchResultPage.create)(Data_Maybe.maybe("")(Control_Category.id(Control_Category.categoryFn))(v)), 
                        io: {
                            searchResults: new App.Loaded(v1.value0), 
                            song: App.Empty.value, 
                            newSong: new Data_Tuple.Tuple(Model.serializeSong(Model.exampleSong), new Data_Either.Right(Model.exampleSong)), 
                            updateSong: new Data_Tuple.Tuple(Model.serializeSong(Model.exampleSong), new Data_Either.Right(Model.exampleSong))
                        }, 
                        ui: {
                            searchQuery: Data_Maybe.maybe("")(Control_Category.id(Control_Category.categoryFn))(v), 
                            showSongMeta: true, 
                            showDuplicatedChorus: true
                        }
                    }));
                };
                throw new Error("Failed pattern match at Main line 258, column 11 - line 272, column 21: " + [ v1.constructor.name ]);
            });
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("missing query param"));
        };
        throw new Error("Failed pattern match at Main line 255, column 5 - line 275, column 1: " + [ v.constructor.name ]);
    });
};
var songPageHandler = function (c) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getRouteParam(Node_Express_Types.requestParamString)("id"))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is required"));
        };
        if (v instanceof Data_Maybe.Just) {
            var $114 = Data_Int.fromString(v.value0);
            if ($114 instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Aff_Class.liftAff(Node_Express_Handler.monadAffHandlerM))(DB.getSongById(c)($114.value0)))(function (v1) {
                    return Data_Function.apply(Node_Express_Response.send)(index({
                        currentPage: new Route.SongPage($114.value0), 
                        io: {
                            searchResults: App.Empty.value, 
                            song: Data_Function.apply(Data_Either.either(function ($140) {
                                return App.LoadError.create(Data_Show.show(Text_Parsing_StringParser.showParseError)($140));
                            })(App.Loaded.create))(Text_Parsing_StringParser.runParser(Model.parseSong)(v1)), 
                            newSong: new Data_Tuple.Tuple(Model.serializeSong(Model.exampleSong), new Data_Either.Right(Model.exampleSong)), 
                            updateSong: new Data_Tuple.Tuple(Model.serializeSong(Model.exampleSong), new Data_Either.Right(Model.exampleSong))
                        }, 
                        ui: {
                            searchQuery: "", 
                            showSongMeta: true, 
                            showDuplicatedChorus: true
                        }
                    }));
                });
            };
            if ($114 instanceof Data_Maybe.Nothing) {
                return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is not a valid integer"));
            };
            throw new Error("Failed pattern match at Main line 281, column 9 - line 297, column 1: " + [ $114.constructor.name ]);
        };
        throw new Error("Failed pattern match at Main line 278, column 5 - line 297, column 1: " + [ v.constructor.name ]);
    });
};
var homePageHandler = Data_Function.apply(Node_Express_Response.send)(index(App.init));
var getUpdateSongPageHandler = function (c) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getRouteParam(Node_Express_Types.requestParamString)("id"))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is required"));
        };
        if (v instanceof Data_Maybe.Just) {
            var $120 = Data_Int.fromString(v.value0);
            if ($120 instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Aff_Class.liftAff(Node_Express_Handler.monadAffHandlerM))(DB.getSongById(c)($120.value0)))(function (v1) {
                    var s$prime = (function () {
                        var $122 = Text_Parsing_StringParser.runParser(Model.parseSong)(v1);
                        if ($122 instanceof Data_Either.Left) {
                            return new Data_Tuple.Tuple("", Data_Function.apply(Data_Either.Left.create)(Data_Show.show(Text_Parsing_StringParser.showParseError)($122.value0)));
                        };
                        if ($122 instanceof Data_Either.Right) {
                            return new Data_Tuple.Tuple(Model.serializeSong($122.value0), new Data_Either.Right($122.value0));
                        };
                        throw new Error("Failed pattern match at Main line 94, column 26 - line 96, column 82: " + [ $122.constructor.name ]);
                    })();
                    return Data_Function.apply(Node_Express_Response.send)(index({
                        currentPage: new Route.UpdateSongPage($120.value0), 
                        io: {
                            searchResults: App.Empty.value, 
                            song: Data_Function.apply(Data_Either.either(function ($141) {
                                return App.LoadError.create(Data_Show.show(Text_Parsing_StringParser.showParseError)($141));
                            })(App.Loaded.create))(Text_Parsing_StringParser.runParser(Model.parseSong)(v1)), 
                            newSong: new Data_Tuple.Tuple(Model.serializeSong(Model.exampleSong), new Data_Either.Right(Model.exampleSong)), 
                            updateSong: s$prime
                        }, 
                        ui: {
                            searchQuery: "", 
                            showSongMeta: true, 
                            showDuplicatedChorus: true
                        }
                    }));
                });
            };
            if ($120 instanceof Data_Maybe.Nothing) {
                return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is not a valid integer"));
            };
            throw new Error("Failed pattern match at Main line 91, column 9 - line 109, column 1: " + [ $120.constructor.name ]);
        };
        throw new Error("Failed pattern match at Main line 88, column 5 - line 109, column 1: " + [ v.constructor.name ]);
    });
};
var getNewSongPageHandler = Data_Function.apply(Node_Express_Response.send)(index({
    currentPage: Route.NewSongPage.value, 
    io: {
        searchResults: App.Empty.value, 
        song: App.Empty.value, 
        newSong: new Data_Tuple.Tuple(Model.serializeSong(Model.exampleSong), new Data_Either.Right(Model.exampleSong)), 
        updateSong: new Data_Tuple.Tuple(Model.serializeSong(Model.exampleSong), new Data_Either.Right(Model.exampleSong))
    }, 
    ui: {
        searchQuery: "", 
        showSongMeta: true, 
        showDuplicatedChorus: true
    }
}));
var fileHandler = Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getRouteParam(Node_Express_Types.requestParamString)("file"))(function (v) {
    return Data_Function.apply(Node_Express_Response.sendFile)(Data_Maybe.maybe("index.html")(Control_Category.id(Control_Category.categoryFn))(v));
});
var errorHandler = function (err) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(400))(function () {
        return Node_Express_Response.sendJson({
            error: Control_Monad_Eff_Exception.message(err)
        });
    });
};
var deleteSongPageHandler = function (c) {
    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Request.getRouteParam(Node_Express_Types.requestParamString)("id"))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is required"));
        };
        if (v instanceof Data_Maybe.Just) {
            var $130 = Data_Int.fromString(v.value0);
            if ($130 instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Data_Function.apply(Control_Monad_Eff_Class.liftEff(Node_Express_Handler.monadEffHandlerM))(Data_Function.apply(Control_Monad_Aff.launchAff)(DB.deleteSong(c)($130.value0))))(function () {
                    return Control_Bind.bind(Node_Express_Handler.bindHandlerM)(Node_Express_Response.setStatus(204))(function () {
                        return Node_Express_Response.send("");
                    });
                });
            };
            if ($130 instanceof Data_Maybe.Nothing) {
                return Data_Function.apply(Node_Express_Handler.nextThrow)(Control_Monad_Eff_Exception.error("Id is not a valid integer"));
            };
            throw new Error("Failed pattern match at Main line 115, column 9 - line 123, column 1: " + [ $130.constructor.name ]);
        };
        throw new Error("Failed pattern match at Main line 112, column 5 - line 123, column 1: " + [ v.constructor.name ]);
    });
};
var appSetup = function (c) {
    return Control_Bind.bind(Node_Express_App.bindAppM)(Data_Function.apply(Control_Monad_Eff_Class.liftEff(Node_Express_App.monadEffAppM))(Control_Monad_Eff_Console.log("Setting up")))(function () {
        return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.useExternal($foreign.urlencodedBodyParser))(function () {
            return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.get(Node_Express_Types.routePath)("/search")(searchPageHandler(c)))(function () {
                return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.get(Node_Express_Types.routePath)("/api/search")(searchApiHandler(c)))(function () {
                    return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.get(Node_Express_Types.routePath)("/new")(getNewSongPageHandler))(function () {
                        return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.post(Node_Express_Types.routePath)("/new")(postNewSongPageHandler(c)))(function () {
                            return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.get(Node_Express_Types.routePath)("/update/:id")(getUpdateSongPageHandler(c)))(function () {
                                return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.post(Node_Express_Types.routePath)("/update/:id")(putUpdateSongPageHandler(c)))(function () {
                                    return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App["delete"](Node_Express_Types.routePath)("/api/song/:id")(deleteSongPageHandler(c)))(function () {
                                        return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.get(Node_Express_Types.routePath)("/song/:id")(songPageHandler(c)))(function () {
                                            return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.get(Node_Express_Types.routePath)("/:file")(fileHandler))(function () {
                                                return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.get(Node_Express_Types.routePath)("/")(homePageHandler))(function () {
                                                    return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.get(Node_Express_Types.routePath)("/api/song/:id")(songApiHandler(c)))(function () {
                                                        return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.put(Node_Express_Types.routePath)("/api/song/:id")(putUpdateSongApiHandler(c)))(function () {
                                                            return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App["delete"](Node_Express_Types.routePath)("/api/song/:id")(deleteSongPageHandler(c)))(function () {
                                                                return Control_Bind.bind(Node_Express_App.bindAppM)(Node_Express_App.post(Node_Express_Types.routePath)("/api/song")(postNewSongApiHandler(c)))(function () {
                                                                    return Node_Express_App.useOnError(errorHandler);
                                                                });
                                                            });
                                                        });
                                                    });
                                                });
                                            });
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });
    });
};
var main = function __do() {
    var v = Data_Foreign_EasyFFI.unsafeForeignFunction([ "" ])("process.env.PORT || 8080")();
    var v1 = Data_Foreign_EasyFFI.unsafeForeignFunction([ "" ])("process.env.DATABASE_URL || ''")();
    var connectionInfo = (function () {
        var $135 = Text_Parsing_StringParser.runParser(DB.mkConnection)(v1);
        if ($135 instanceof Data_Either.Right) {
            return $135.value0;
        };
        if ($135 instanceof Data_Either.Left) {
            return DB.localConnectionInfo;
        };
        throw new Error("Failed pattern match at Main line 53, column 26 - line 55, column 44: " + [ $135.constructor.name ]);
    })();
    return Node_Express_App.listenHttp(appSetup(connectionInfo))(v)(function (v2) {
        return Data_Function.apply(Control_Monad_Eff_Console.log)("listening on " + Data_Show.show(Data_Show.showInt)(v));
    })();
};
module.exports = {
    appSetup: appSetup, 
    deleteSongPageHandler: deleteSongPageHandler, 
    errorHandler: errorHandler, 
    fileHandler: fileHandler, 
    getNewSongPageHandler: getNewSongPageHandler, 
    getUpdateSongPageHandler: getUpdateSongPageHandler, 
    homePageHandler: homePageHandler, 
    index: index, 
    main: main, 
    postNewSongApiHandler: postNewSongApiHandler, 
    postNewSongPageHandler: postNewSongPageHandler, 
    putUpdateSongApiHandler: putUpdateSongApiHandler, 
    putUpdateSongPageHandler: putUpdateSongPageHandler, 
    renderApp: renderApp, 
    renderAppHandler: renderAppHandler, 
    searchApiHandler: searchApiHandler, 
    searchPageHandler: searchPageHandler, 
    songApiHandler: songApiHandler, 
    songPageHandler: songPageHandler, 
    urlencodedBodyParser: $foreign.urlencodedBodyParser
};

// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Argonaut_Decode = require("../Data.Argonaut.Decode");
var Data_Argonaut_Encode = require("../Data.Argonaut.Encode");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Unfoldable = require("../Data.Unfoldable");
var Data_Foldable = require("../Data.Foldable");
var Data_Int = require("../Data.Int");
var Data_List = require("../Data.List");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_StrMap = require("../Data.StrMap");
var Data_Tuple = require("../Data.Tuple");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Argonaut_Encode_Class = require("../Data.Argonaut.Encode.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Argonaut_Decode_Class = require("../Data.Argonaut.Decode.Class");
var Data_Function = require("../Data.Function");
var Control_Apply = require("../Control.Apply");
var Data_Functor = require("../Data.Functor");
var Control_Bind = require("../Control.Bind");
var Data_Ring = require("../Data.Ring");
var Control_Applicative = require("../Control.Applicative");
var Data_Semiring = require("../Data.Semiring");
var JsonPrim = function (x) {
    return x;
};
var JCursorTop = (function () {
    function JCursorTop() {

    };
    JCursorTop.value = new JCursorTop();
    return JCursorTop;
})();
var JField = (function () {
    function JField(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    JField.create = function (value0) {
        return function (value1) {
            return new JField(value0, value1);
        };
    };
    return JField;
})();
var JIndex = (function () {
    function JIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    JIndex.create = function (value0) {
        return function (value1) {
            return new JIndex(value0, value1);
        };
    };
    return JIndex;
})();
var showJCursor = new Data_Show.Show(function (v) {
    if (v instanceof JCursorTop) {
        return "";
    };
    if (v instanceof JField) {
        return "." + (v.value0 + Data_Show.show(showJCursor)(v.value1));
    };
    if (v instanceof JIndex) {
        return "[" + (Data_Show.show(Data_Show.showInt)(v.value0) + ("]" + Data_Show.show(showJCursor)(v.value1)));
    };
    throw new Error("Failed pattern match at Data.Argonaut.JCursor line 28, column 3 - line 29, column 3: " + [ v.constructor.name ]);
});
var semigroupJCursor = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v1 instanceof JCursorTop) {
            return v;
        };
        if (v instanceof JCursorTop) {
            return v1;
        };
        if (v instanceof JField) {
            return new JField(v.value0, Data_Semigroup.append(semigroupJCursor)(v.value1)(v1));
        };
        if (v instanceof JIndex) {
            return new JIndex(v.value0, Data_Semigroup.append(semigroupJCursor)(v.value1)(v1));
        };
        throw new Error("Failed pattern match at Data.Argonaut.JCursor line 33, column 3 - line 33, column 26: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var runJsonPrim = function (v) {
    return v;
};
var showJsonPrim = new Data_Show.Show(function (p) {
    return runJsonPrim(p)(Data_Show.show(Data_Argonaut_Core.showJNull))(Data_Show.show(Data_Show.showBoolean))(Data_Show.show(Data_Show.showNumber))(Data_Show.show(Data_Show.showString));
});
var primToJson = function (p) {
    return runJsonPrim(p)(Data_Argonaut_Core.fromNull)(Data_Argonaut_Core.fromBoolean)(Data_Argonaut_Core.fromNumber)(Data_Argonaut_Core.fromString);
};
var primStr = function (v) {
    return function (v1) {
        return function (v2) {
            return function (v3) {
                return function (f) {
                    return f(v);
                };
            };
        };
    };
};
var primNum = function (v) {
    return function (v1) {
        return function (v2) {
            return function (f) {
                return function (v3) {
                    return f(v);
                };
            };
        };
    };
};
var primNull = function (f) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                return f($foreign.exactNull);
            };
        };
    };
};
var primBool = function (v) {
    return function (v1) {
        return function (f) {
            return function (v2) {
                return function (v3) {
                    return f(v);
                };
            };
        };
    };
};
var toPrims = (function () {
    var objFn = function (obj) {
        var f = function (v) {
            return Data_Functor.map(Data_List.functorList)(function (t) {
                return new Data_Tuple.Tuple(new JField(v.value0, Data_Tuple.fst(t)), Data_Tuple.snd(t));
            })(toPrims(v.value1));
        };
        return Control_Bind.bind(Data_List.bindList)(Data_StrMap.toList(obj))(f);
    };
    var mkTop = function (p) {
        return Data_Function.apply(Data_List.singleton)(new Data_Tuple.Tuple(JCursorTop.value, p));
    };
    var nullFn = function (v) {
        return mkTop(primNull);
    };
    var numFn = function (n) {
        return Data_Function.apply(mkTop)(primNum(n));
    };
    var strFn = function (s) {
        return Data_Function.apply(mkTop)(primStr(s));
    };
    var boolFn = function (b) {
        return Data_Function.apply(mkTop)(primBool(b));
    };
    var arrFn$prime = function (v) {
        return Data_List.fromFoldable(Data_List.foldableList)(Data_Functor.map(Data_List.functorList)(function (t) {
            return new Data_Tuple.Tuple(new JIndex(v.value0, Data_Tuple.fst(t)), Data_Tuple.snd(t));
        })(toPrims(v.value1)));
    };
    var arrFn = function (arr) {
        var zipped = Data_List.zipWith(Data_Tuple.Tuple.create)(Data_List.range(0)(Data_Array.length(arr) - 1))(Data_List.fromFoldable(Data_Foldable.foldableArray)(arr));
        return Control_Bind.bind(Data_List.bindList)(zipped)(arrFn$prime);
    };
    return Data_Argonaut_Core.foldJson(nullFn)(boolFn)(numFn)(strFn)(arrFn)(objFn);
})();
var monoidJCursor = new Data_Monoid.Monoid(function () {
    return semigroupJCursor;
}, JCursorTop.value);
var inferEmpty = function (v) {
    if (v instanceof JCursorTop) {
        return Data_Argonaut_Core.jsonNull;
    };
    if (v instanceof JField) {
        return Data_Argonaut_Core.jsonEmptyObject;
    };
    if (v instanceof JIndex) {
        return Data_Argonaut_Core.jsonEmptyArray;
    };
    throw new Error("Failed pattern match at Data.Argonaut.JCursor line 95, column 1 - line 95, column 37: " + [ v.constructor.name ]);
};
var fail = function (dictShow) {
    return function (x) {
        return Data_Function.apply(Data_Either.Left.create)("Expected String or Number but found: " + Data_Show.show(dictShow)(x));
    };
};
var eqJCursor = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof JCursorTop && y instanceof JCursorTop) {
            return true;
        };
        if (x instanceof JField && y instanceof JField) {
            return x.value0 === y.value0 && Data_Eq.eq(eqJCursor)(x.value1)(y.value1);
        };
        if (x instanceof JIndex && y instanceof JIndex) {
            return x.value0 === y.value0 && Data_Eq.eq(eqJCursor)(x.value1)(y.value1);
        };
        return false;
    };
});
var ordJCursor = new Data_Ord.Ord(function () {
    return eqJCursor;
}, function (x) {
    return function (y) {
        if (x instanceof JCursorTop && y instanceof JCursorTop) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof JCursorTop) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof JCursorTop) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof JField && y instanceof JField) {
            var $98 = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if ($98 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($98 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordJCursor)(x.value1)(y.value1);
        };
        if (x instanceof JField) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof JField) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof JIndex && y instanceof JIndex) {
            var $107 = Data_Ord.compare(Data_Ord.ordInt)(x.value0)(y.value0);
            if ($107 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($107 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordJCursor)(x.value1)(y.value1);
        };
        throw new Error("Failed pattern match: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var encodeJsonJCursor = new Data_Argonaut_Encode_Class.EncodeJson((function () {
    var loop = function (v) {
        if (v instanceof JCursorTop) {
            return [  ];
        };
        if (v instanceof JField) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonJString)(v.value0) ])(loop(v.value1));
        };
        if (v instanceof JIndex) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonInt)(v.value0) ])(loop(v.value1));
        };
        throw new Error("Failed pattern match at Data.Argonaut.JCursor line 42, column 3 - line 45, column 49: " + [ v.constructor.name ]);
    };
    return function ($153) {
        return Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonArray(Data_Argonaut_Encode_Class.encodeJsonJson))(loop($153));
    };
})());
var downIndex = function (i) {
    var downIndex$prime = function (v) {
        if (v instanceof JCursorTop) {
            return new JIndex(i, JCursorTop.value);
        };
        if (v instanceof JField) {
            return new JField(v.value0, downIndex$prime(v.value1));
        };
        if (v instanceof JIndex) {
            return new JIndex(v.value0, downIndex$prime(v.value1));
        };
        throw new Error("Failed pattern match at Data.Argonaut.JCursor line 84, column 1 - line 87, column 54: " + [ v.constructor.name ]);
    };
    return downIndex$prime;
};
var downField = function (i) {
    var downField$prime = function (v) {
        if (v instanceof JCursorTop) {
            return new JField(i, JCursorTop.value);
        };
        if (v instanceof JField) {
            return new JField(v.value0, downField$prime(v.value1));
        };
        if (v instanceof JIndex) {
            return new JIndex(v.value0, downField$prime(v.value1));
        };
        throw new Error("Failed pattern match at Data.Argonaut.JCursor line 78, column 1 - line 81, column 54: " + [ v.constructor.name ]);
    };
    return downField$prime;
};
var insideOut = function (v) {
    if (v instanceof JCursorTop) {
        return JCursorTop.value;
    };
    if (v instanceof JField) {
        return downField(v.value0)(insideOut(v.value1));
    };
    if (v instanceof JIndex) {
        return downIndex(v.value0)(insideOut(v.value1));
    };
    throw new Error("Failed pattern match at Data.Argonaut.JCursor line 73, column 1 - line 73, column 34: " + [ v.constructor.name ]);
};
var decodeJsonJCursor = new Data_Argonaut_Decode_Class.DecodeJson(function (j) {
    var goNum = function (c) {
        return function ($154) {
            return Data_Maybe.maybe(new Data_Either.Left("Not an Int"))(function ($155) {
                return Data_Either.Right.create(Data_Function.flip(JIndex.create)(c)($155));
            })(Data_Int.fromNumber($154));
        };
    };
    var loop = function (arr) {
        return Data_Function.apply(Data_Maybe.maybe(new Data_Either.Right(JCursorTop.value))(goLoop))(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(Data_Array.head(arr)))(Data_Array.tail(arr)));
    };
    var goLoop = function (v) {
        return Control_Bind.bind(Data_Either.bindEither)(loop(v.value1))(function (v1) {
            return Data_Argonaut_Core.foldJson(fail(Data_Argonaut_Core.showJNull))(fail(Data_Show.showBoolean))(goNum(v1))(function ($156) {
                return Data_Either.Right.create(Data_Function.flip(JField.create)(v1)($156));
            })(fail(Data_Show.showArray(Data_Argonaut_Core.showJson)))(fail(Data_StrMap.showStrMap(Data_Argonaut_Core.showJson)))(v.value0);
        });
    };
    return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeArray(Data_Argonaut_Decode_Class.decodeJsonJson))(j))(loop);
});
var cursorSet = function (v) {
    return function (v1) {
        if (v instanceof JCursorTop) {
            return function ($157) {
                return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Function["const"](v1)($157));
            };
        };
        if (v instanceof JField) {
            var mergeObjs = function (m) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function ($158) {
                    return Data_Argonaut_Core.fromObject(Data_Function.flip(Data_StrMap.insert(v.value0))(m)($158));
                })(cursorSet(v.value1)(v1)(Data_Maybe.fromMaybe(inferEmpty(v.value1))(Data_StrMap.lookup(v.value0)(m))));
            };
            var defaultObj = Data_Functor.map(Data_Maybe.functorMaybe)(function ($159) {
                return Data_Argonaut_Core.fromObject(Data_StrMap.singleton(v.value0)($159));
            })(cursorSet(v.value1)(v1)(inferEmpty(v.value1)));
            return Data_Argonaut_Core.foldJsonObject(defaultObj)(mergeObjs);
        };
        if (v instanceof JIndex) {
            var setArr = function (__copy_xs) {
                return function (__copy_i1) {
                    return function (__copy_v3) {
                        var xs = __copy_xs;
                        var i1 = __copy_i1;
                        var v3 = __copy_v3;
                        tco: while (true) {
                            var len = Data_Array.length(xs);
                            var $140 = i1 < 0;
                            if ($140) {
                                return Data_Maybe.Nothing.value;
                            };
                            if (!$140) {
                                var $141 = i1 >= len;
                                if ($141) {
                                    var __tco_xs = Data_Semigroup.append(Data_Semigroup.semigroupArray)(xs)(Data_Unfoldable.replicate(Data_Unfoldable.unfoldableArray)((i1 - len) + 1 | 0)(Data_Argonaut_Core.jsonNull));
                                    var __tco_i1 = i1;
                                    var __tco_v3 = v3;
                                    xs = __tco_xs;
                                    i1 = __tco_i1;
                                    v3 = __tco_v3;
                                    continue tco;
                                };
                                if (!$141) {
                                    return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Argonaut_Core.fromArray)(Data_Array.updateAt(i1)(v3)(xs));
                                };
                                throw new Error("Failed pattern match at Data.Argonaut.JCursor line 124, column 13 - line 126, column 51: " + [ $141.constructor.name ]);
                            };
                            throw new Error("Failed pattern match at Data.Argonaut.JCursor line 122, column 8 - line 126, column 51: " + [ $140.constructor.name ]);
                        };
                    };
                };
            };
            var mergeArrs = function (a) {
                return Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(setArr(a)(v.value0))(cursorSet(v.value1)(v1)(Data_Maybe.fromMaybe(inferEmpty(v.value1))(Data_Array.index(a)(v.value0))));
            };
            var defaultArr = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Argonaut_Core.fromArray)(Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Data_Function.flip(Data_Array.updateAt(v.value0))(Data_Unfoldable.replicate(Data_Unfoldable.unfoldableArray)(v.value0 + 1 | 0)(Data_Argonaut_Core.jsonNull)))(cursorSet(v.value1)(v1)(inferEmpty(v.value1))));
            return Data_Argonaut_Core.foldJsonArray(defaultArr)(mergeArrs);
        };
        throw new Error("Failed pattern match at Data.Argonaut.JCursor line 100, column 1 - line 100, column 42: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var fromPrims = function (lst) {
    var f = function (j) {
        return function (v) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(j)(cursorSet(v.value0)(primToJson(v.value1)));
        };
    };
    return Data_Foldable.foldl(Data_List.foldableList)(f)(Data_Functor.map(Data_Maybe.functorMaybe)(function ($160) {
        return inferEmpty(Data_Tuple.fst($160));
    })(Data_List.head(lst)))(lst);
};
var cursorGet = function (v) {
    if (v instanceof JCursorTop) {
        return Data_Maybe.Just.create;
    };
    if (v instanceof JField) {
        return Data_Argonaut_Core.foldJsonObject(Data_Maybe.Nothing.value)(Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(cursorGet(v.value1))(Data_StrMap.lookup(v.value0)));
    };
    if (v instanceof JIndex) {
        return Data_Argonaut_Core.foldJsonArray(Data_Maybe.Nothing.value)(Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(cursorGet(v.value1))(function (v1) {
            return Data_Array.index(v1)(v.value0);
        }));
    };
    throw new Error("Failed pattern match at Data.Argonaut.JCursor line 90, column 1 - line 90, column 28: " + [ v.constructor.name ]);
};
module.exports = {
    JCursorTop: JCursorTop, 
    JField: JField, 
    JIndex: JIndex, 
    JsonPrim: JsonPrim, 
    cursorGet: cursorGet, 
    cursorSet: cursorSet, 
    downField: downField, 
    downIndex: downIndex, 
    fail: fail, 
    fromPrims: fromPrims, 
    inferEmpty: inferEmpty, 
    insideOut: insideOut, 
    primBool: primBool, 
    primNull: primNull, 
    primNum: primNum, 
    primStr: primStr, 
    primToJson: primToJson, 
    runJsonPrim: runJsonPrim, 
    toPrims: toPrims, 
    eqJCursor: eqJCursor, 
    ordJCursor: ordJCursor, 
    showJCursor: showJCursor, 
    semigroupJCursor: semigroupJCursor, 
    monoidJCursor: monoidJCursor, 
    encodeJsonJCursor: encodeJsonJCursor, 
    showJsonPrim: showJsonPrim, 
    decodeJsonJCursor: decodeJsonJCursor, 
    exactNull: $foreign.exactNull
};

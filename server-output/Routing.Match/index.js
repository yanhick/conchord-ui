// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Tuple = require("../Data.Tuple");
var Data_Maybe = require("../Data.Maybe");
var Data_List = require("../Data.List");
var Control_Alt = require("../Control.Alt");
var Control_Plus = require("../Control.Plus");
var Control_Alternative = require("../Control.Alternative");
var Global = require("../Global");
var Data_Semiring_Free = require("../Data.Semiring.Free");
var Data_Foldable = require("../Data.Foldable");
var Data_Validation_Semiring = require("../Data.Validation.Semiring");
var Data_Map = require("../Data.Map");
var Routing_Types = require("../Routing.Types");
var Routing_Match_Class = require("../Routing.Match.Class");
var Routing_Match_Error = require("../Routing.Match.Error");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Control_Applicative = require("../Control.Applicative");
var Data_Unit = require("../Data.Unit");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Functor = require("../Data.Functor");
var Data_Semiring = require("../Data.Semiring");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Bind = require("../Control.Bind");
var Data_Ord = require("../Data.Ord");
var Match = function (x) {
    return x;
};
var unMatch = function (v) {
    return v;
};
var runMatch = function (v) {
    return function (route) {
        var foldErrors = function (errs) {
            return Data_Function.apply(Data_Either.Left.create)(Data_Foldable.foldl(Data_List.foldableList)(function (b) {
                return function (a) {
                    return a + ("\n" + b);
                };
            })("")(Control_Bind.bind(Data_List.bindList)(Data_Functor.map(Data_List.functorList)(Data_List.reverse)(Data_Semiring_Free.runFree(errs)))(function (v1) {
                return Data_Function.apply(Control_Applicative.pure(Data_List.applicativeList))(Data_Function.apply(Data_Foldable.foldl(Data_List.foldableList)(function (b) {
                    return function (a) {
                        return a + (";" + b);
                    };
                })(""))(Data_Functor.map(Data_List.functorList)(Routing_Match_Error.showMatchError)(v1)));
            })));
        };
        return Data_Function.apply(Data_Validation_Semiring.unV(foldErrors)(function ($80) {
            return Data_Either.Right.create(Data_Tuple.snd($80));
        }))(v(route));
    };
};
var matchFunctor = new Data_Functor.Functor(function (fn) {
    return function (v) {
        return Data_Function.apply(Match)(function (r) {
            return Data_Function.apply(Data_Validation_Semiring.unV(Data_Validation_Semiring.invalid)(function (v1) {
                return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(v1.value0, fn(v1.value1)));
            }))(v(r));
        });
    };
});
var matchApply = new Control_Apply.Apply(function () {
    return matchFunctor;
}, function (v) {
    return function (v1) {
        var processFnRes = function (v2) {
            return Data_Validation_Semiring.unV(Data_Validation_Semiring.invalid)(function (v3) {
                return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(v3.value0, v2.value1(v3.value1)));
            })(v1(v2.value0));
        };
        var processFnErr = function (r) {
            return function (err) {
                return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Semiring.mul(Data_Semiring_Free.semiringFree)(err)(Data_Validation_Semiring.unV(Control_Category.id(Control_Category.categoryFn))(Data_Function["const"](Data_Semiring.one(Data_Semiring_Free.semiringFree)))(v1(r))));
            };
        };
        return Data_Function.apply(Match)(function (r) {
            return Data_Validation_Semiring.unV(processFnErr(r))(processFnRes)(v(r));
        });
    };
});
var matchApplicative = new Control_Applicative.Applicative(function () {
    return matchApply;
}, function (a) {
    return function (r) {
        return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(r, a));
    };
});
var matchAlt = new Control_Alt.Alt(function () {
    return matchFunctor;
}, function (v) {
    return function (v1) {
        return Data_Function.apply(Match)(function (r) {
            return Control_Alt.alt(Data_Validation_Semiring.altV(Data_Semiring_Free.semiringFree))(v(r))(v1(r));
        });
    };
});
var matchPlus = new Control_Plus.Plus(function () {
    return matchAlt;
}, Data_Function.apply(Match)(Data_Function.apply(Data_Function["const"])(Data_Validation_Semiring.invalid(Data_Semiring.one(Data_Semiring_Free.semiringFree)))));
var matchAlternative = new Control_Alternative.Alternative(function () {
    return matchApplicative;
}, function () {
    return matchPlus;
});
var matchMatchClass = new Routing_Match_Class.MatchClass(function () {
    return matchAlternative;
}, function (route) {
    if (route instanceof Data_List.Cons && (route.value0 instanceof Routing_Types.Path && route.value0.value0 === "true")) {
        return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(route.value1, true));
    };
    if (route instanceof Data_List.Cons && (route.value0 instanceof Routing_Types.Path && route.value0.value0 === "false")) {
        return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(route.value1, false));
    };
    return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Semiring_Free.free(Routing_Match_Error.ExpectedBoolean.value));
}, function (msg) {
    return function (v) {
        return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Function.apply(Data_Semiring_Free.free)(new Routing_Match_Error.Fail(msg)));
    };
}, function (input) {
    return function (route) {
        if (route instanceof Data_List.Cons && (route.value0 instanceof Routing_Types.Path && route.value0.value0 === input)) {
            return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(route.value1, Data_Unit.unit));
        };
        if (route instanceof Data_List.Cons && route.value0 instanceof Routing_Types.Path) {
            return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Function.apply(Data_Semiring_Free.free)(new Routing_Match_Error.UnexpectedPath(input)));
        };
        return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Semiring_Free.free(Routing_Match_Error.ExpectedPathPart.value));
    };
}, function (route) {
    if (route instanceof Data_List.Cons && route.value0 instanceof Routing_Types.Path) {
        var res = Global.readFloat(route.value0.value0);
        var $51 = Global["isNaN"](res);
        if ($51) {
            return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Semiring_Free.free(Routing_Match_Error.ExpectedNumber.value));
        };
        if (!$51) {
            return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(route.value1, res));
        };
        throw new Error("Failed pattern match at Routing.Match line 42, column 9 - line 45, column 30: " + [ $51.constructor.name ]);
    };
    return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Semiring_Free.free(Routing_Match_Error.ExpectedNumber.value));
}, function (key) {
    return function (route) {
        if (route instanceof Data_List.Cons && route.value0 instanceof Routing_Types.Query) {
            var $56 = Data_Map.lookup(Data_Ord.ordString)(key)(route.value0.value0);
            if ($56 instanceof Data_Maybe.Nothing) {
                return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Function.apply(Data_Semiring_Free.free)(new Routing_Match_Error.KeyNotFound(key)));
            };
            if ($56 instanceof Data_Maybe.Just) {
                return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(new Data_List.Cons(Data_Function.apply(function ($81) {
                    return Routing_Types.Query.create(Data_Map["delete"](Data_Ord.ordString)(key)($81));
                })(route.value0.value0), route.value1), $56.value0));
            };
            throw new Error("Failed pattern match at Routing.Match line 68, column 9 - line 72, column 69: " + [ $56.constructor.name ]);
        };
        return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Semiring_Free.free(Routing_Match_Error.ExpectedQuery.value));
    };
}, function (route) {
    if (route instanceof Data_List.Cons && route.value0 instanceof Routing_Types.Query) {
        return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(route.value1, route.value0.value0));
    };
    return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Semiring_Free.free(Routing_Match_Error.ExpectedQuery.value));
}, function (route) {
    if (route instanceof Data_List.Cons && route.value0 instanceof Routing_Types.Path) {
        return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(route.value1, route.value0.value0));
    };
    return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Semiring_Free.free(Routing_Match_Error.ExpectedString.value));
});
var list = function (v) {
    var go = function (accum) {
        return function (r) {
            return Data_Validation_Semiring.unV(Data_Function.apply(Data_Function["const"])(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree))(new Data_Tuple.Tuple(r, Data_List.reverse(accum)))))(function (v1) {
                return go(new Data_List.Cons(v1.value1, accum))(v1.value0);
            })(v(r));
        };
    };
    return Data_Function.apply(Match)(go(Data_List.Nil.value));
};
var eitherMatch = function (v) {
    var runEither = function (v1) {
        if (v1.value1 instanceof Data_Either.Left) {
            return Data_Function.apply(Data_Validation_Semiring.invalid)(Data_Function.apply(Data_Semiring_Free.free)(new Routing_Match_Error.Fail("Nested check failed")));
        };
        if (v1.value1 instanceof Data_Either.Right) {
            return Data_Function.apply(Control_Applicative.pure(Data_Validation_Semiring.applicativeV(Data_Semiring_Free.semiringFree)))(new Data_Tuple.Tuple(v1.value0, v1.value1.value0));
        };
        throw new Error("Failed pattern match at Routing.Match line 158, column 5 - line 160, column 36: " + [ v1.value1.constructor.name ]);
    };
    return Data_Function.apply(Match)(function (r) {
        return Data_Function.apply(Data_Validation_Semiring.unV(Data_Validation_Semiring.invalid)(runEither))(v(r));
    });
};
module.exports = {
    Match: Match, 
    eitherMatch: eitherMatch, 
    list: list, 
    runMatch: runMatch, 
    unMatch: unMatch, 
    matchMatchClass: matchMatchClass, 
    matchFunctor: matchFunctor, 
    matchAlt: matchAlt, 
    matchPlus: matchPlus, 
    matchAlternative: matchAlternative, 
    matchApply: matchApply, 
    matchApplicative: matchApplicative
};
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Alternative = require("../Control.Alternative");
var Control_Plus = require("../Control.Plus");
var Data_Lens_Indexed = require("../Data.Lens.Indexed");
var Data_Lens_Types_1 = require("../Data.Lens.Types");
var Data_Lens_Types_1 = require("../Data.Lens.Types");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Profunctor_Star = require("../Data.Profunctor.Star");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Lens_Internal_Wander = require("../Data.Lens.Internal.Wander");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Category = require("../Control.Category");
var Data_Function = require("../Data.Function");
var Data_Lens_Internal_Indexed = require("../Data.Lens.Internal.Indexed");
var Control_Applicative = require("../Control.Applicative");
var Data_Eq = require("../Data.Eq");
var traversed = function (dictTraversable) {
    return function (dictWander) {
        return Data_Lens_Internal_Wander.wander(dictWander)(function (dictApplicative) {
            return Data_Traversable.traverse(dictTraversable)(dictApplicative);
        });
    };
};
var traverseOf = function (dictApplicative) {
    return function (t) {
        return function ($23) {
            return Data_Profunctor_Star.unStar(t(Data_Profunctor_Star.Star($23)));
        };
    };
};
var sequenceOf = function (dictApplicative) {
    return function (t) {
        return traverseOf(dictApplicative)(t)(Control_Category.id(Control_Category.categoryFn));
    };
};
var itraverseOf = function (dictApplicative) {
    return function (t) {
        return function (f) {
            return Data_Function.apply(Data_Profunctor_Star.unStar)(Data_Function.apply(t)(Data_Function.apply(Data_Lens_Internal_Indexed.Indexed)(Data_Function.apply(Data_Profunctor_Star.Star)(Data_Tuple.uncurry(f)))));
        };
    };
};
var failover = function (dictAlternative) {
    return function (t) {
        return function (f) {
            return function (s) {
                var $14 = Data_Profunctor_Star.unStar(Data_Function.apply(t)(Data_Function.apply(Data_Profunctor_Star.Star)(function ($24) {
                    return Data_Tuple.Tuple.create(true)(f($24));
                })))(s);
                if ($14.value0) {
                    return Control_Applicative.pure(dictAlternative["__superclass_Control.Applicative.Applicative_0"]())($14.value1);
                };
                if (!$14.value0) {
                    return Control_Plus.empty(dictAlternative["__superclass_Control.Plus.Plus_1"]());
                };
                throw new Error("Failed pattern match at Data.Lens.Traversal line 48, column 18 - line 50, column 32: " + [ $14.constructor.name ]);
            };
        };
    };
};
var elementsOf = function (dictWander) {
    return function (tr) {
        return function (pr) {
            return Data_Lens_Indexed.iwander(dictWander)(function (dictApplicative) {
                return function (f) {
                    return Data_Function.apply(Data_Profunctor_Star.unStar)(Data_Function.apply(tr(Data_Lens_Internal_Wander.wanderStar(dictApplicative)))(Data_Function.apply(Data_Lens_Internal_Indexed.Indexed)(Data_Function.apply(Data_Profunctor_Star.Star)(function (v) {
                        var $20 = pr(v.value0);
                        if ($20) {
                            return f(v.value0)(v.value1);
                        };
                        if (!$20) {
                            return Control_Applicative.pure(dictApplicative)(v.value1);
                        };
                        throw new Error("Failed pattern match at Data.Lens.Traversal line 62, column 81 - line 62, column 111: " + [ $20.constructor.name ]);
                    }))));
                };
            });
        };
    };
};
var element = function (dictWander) {
    return function (n) {
        return function (tr) {
            return Data_Function.apply(Data_Lens_Indexed.unIndex((dictWander["__superclass_Data.Profunctor.Choice.Choice_1"]())["__superclass_Data.Profunctor.Profunctor_0"]()))(elementsOf(dictWander)(function (dictWander1) {
                return Data_Lens_Indexed.positions(dictWander1)(function (dictWander2) {
                    return tr(dictWander2);
                });
            })(function (v) {
                return v === n;
            }));
        };
    };
};
module.exports = {
    element: element, 
    elementsOf: elementsOf, 
    failover: failover, 
    itraverseOf: itraverseOf, 
    sequenceOf: sequenceOf, 
    traverseOf: traverseOf, 
    traversed: traversed
};

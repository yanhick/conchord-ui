// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Enum = require("../Data.Enum");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Partial_Unsafe = require("../Partial.Unsafe");
var Test_StrongCheck_Arbitrary = require("../Test.StrongCheck.Arbitrary");
var Test_StrongCheck_Gen = require("../Test.StrongCheck.Gen");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Ring = require("../Data.Ring");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Bounded = require("../Data.Bounded");
var Data_Semiring = require("../Data.Semiring");
var Data_Function = require("../Data.Function");
var Control_Monad_Free = require("../Control.Monad.Free");
var ArbBoundedEnum = function (x) {
    return x;
};
var showArbBoundedEnum = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(ArbBoundedEnum " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var runArbBoundedEnum = function (v) {
    return v;
};
var eqArbBoundedEnum = function (dictEq) {
    return new Data_Eq.Eq(function (x) {
        return function (y) {
            return Data_Eq.eq(dictEq)(x)(y);
        };
    });
};
var ordArbBoundedEnum = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqArbBoundedEnum(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (x) {
        return function (y) {
            return Data_Ord.compare(dictOrd)(x)(y);
        };
    });
};
var coarbArbBoundedEnum = function (dictBoundedEnum) {
    return new Test_StrongCheck_Arbitrary.Coarbitrary(function (v) {
        return Test_StrongCheck_Arbitrary.coarbitrary(Test_StrongCheck_Arbitrary.coarbInt)(Data_Enum.fromEnum(dictBoundedEnum)(v));
    });
};
var boundedArbBoundedEnum = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordArbBoundedEnum(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(dictBounded), Data_Bounded.top(dictBounded));
};
var enumHour = function (dictBoundedEnum) {
    return new Data_Enum.Enum(function () {
        return ordArbBoundedEnum((dictBoundedEnum["__superclass_Data.Enum.Enum_1"]())["__superclass_Data.Ord.Ord_0"]());
    }, function ($33) {
        return Data_Enum.toEnum(enumArbBoundedEnum(dictBoundedEnum))((function (v) {
            return v - 1;
        })(Data_Enum.fromEnum(enumArbBoundedEnum(dictBoundedEnum))($33)));
    }, function ($34) {
        return Data_Enum.toEnum(enumArbBoundedEnum(dictBoundedEnum))((function (v) {
            return v + 1 | 0;
        })(Data_Enum.fromEnum(enumArbBoundedEnum(dictBoundedEnum))($34)));
    });
};
var enumArbBoundedEnum = function (dictBoundedEnum) {
    return new Data_Enum.BoundedEnum(function () {
        return boundedArbBoundedEnum(dictBoundedEnum["__superclass_Data.Bounded.Bounded_0"]());
    }, function () {
        return enumHour(dictBoundedEnum);
    }, Data_Function.apply(Data_Enum.Cardinality)(Data_Enum.runCardinality(Data_Enum.cardinality(dictBoundedEnum))), function (v) {
        return Data_Enum.fromEnum(dictBoundedEnum)(v);
    }, function (v) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(ArbBoundedEnum)(Data_Enum.toEnum(dictBoundedEnum)(v));
    });
};
var arbArbBoundedEnum = function (dictBoundedEnum) {
    return new Test_StrongCheck_Arbitrary.Arbitrary((function () {
        var f = Partial_Unsafe.unsafePartial(function (dictPartial) {
            return function (v) {
                return Data_Functor.map(Test_StrongCheck_Gen.functorGenT(Control_Monad_Free.freeMonad))(function ($35) {
                    return Data_Maybe.fromJust(dictPartial)(Data_Enum.toEnum(dictBoundedEnum)($35));
                })(Test_StrongCheck_Gen.chooseInt(Control_Monad_Free.freeMonad)(0.0)(Data_Int.toNumber(v) - 1.0));
            };
        });
        return Data_Functor.map(Test_StrongCheck_Gen.functorGenT(Control_Monad_Free.freeMonad))(ArbBoundedEnum)(f(Data_Enum.cardinality(dictBoundedEnum)));
    })());
};
module.exports = {
    ArbBoundedEnum: ArbBoundedEnum, 
    runArbBoundedEnum: runArbBoundedEnum, 
    eqArbBoundedEnum: eqArbBoundedEnum, 
    ordArbBoundedEnum: ordArbBoundedEnum, 
    arbArbBoundedEnum: arbArbBoundedEnum, 
    coarbArbBoundedEnum: coarbArbBoundedEnum, 
    showArbBoundedEnum: showArbBoundedEnum, 
    boundedArbBoundedEnum: boundedArbBoundedEnum, 
    enumHour: enumHour, 
    enumArbBoundedEnum: enumArbBoundedEnum
};
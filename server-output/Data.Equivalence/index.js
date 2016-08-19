// Generated by psc version 0.9.3
"use strict";
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Comparison = require("../Data.Comparison");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor_Contravariant = require("../Data.Functor.Contravariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Equivalence = function (x) {
    return x;
};
var semigroupEquivalence = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return function (a) {
            return function (b) {
                return v(a)(b) && v1(a)(b);
            };
        };
    };
});
var runEquivalence = function (v) {
    return v;
};
var monoidEquivalence = new Data_Monoid.Monoid(function () {
    return semigroupEquivalence;
}, function (v) {
    return function (v1) {
        return true;
    };
});
var defaultEquivalence = function (dictEq) {
    return Data_Eq.eq(dictEq);
};
var contravariantEquivalence = new Data_Functor_Contravariant.Contravariant(function (f) {
    return function (v) {
        return Data_Function.on(v)(f);
    };
});
var comparisonEquivalence = function (v) {
    return function (a) {
        return function (b) {
            return Data_Eq.eq(Data_Ordering.eqOrdering)(v(a)(b))(Data_Ordering.EQ.value);
        };
    };
};
module.exports = {
    Equivalence: Equivalence, 
    comparisonEquivalence: comparisonEquivalence, 
    defaultEquivalence: defaultEquivalence, 
    runEquivalence: runEquivalence, 
    contravariantEquivalence: contravariantEquivalence, 
    semigroupEquivalence: semigroupEquivalence, 
    monoidEquivalence: monoidEquivalence
};
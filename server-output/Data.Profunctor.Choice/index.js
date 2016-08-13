// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Profunctor = require("../Data.Profunctor");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Category = require("../Control.Category");
var Choice = function (__superclass_Data$dotProfunctor$dotProfunctor_0, left, right) {
    this["__superclass_Data.Profunctor.Profunctor_0"] = __superclass_Data$dotProfunctor$dotProfunctor_0;
    this.left = left;
    this.right = right;
};
var right = function (dict) {
    return dict.right;
};
var left = function (dict) {
    return dict.left;
};
var splitChoice = function (dictCategory) {
    return function (dictChoice) {
        return function (l) {
            return function (r) {
                return Control_Semigroupoid.composeFlipped(dictCategory["__superclass_Control.Semigroupoid.Semigroupoid_0"]())(left(dictChoice)(l))(right(dictChoice)(r));
            };
        };
    };
};
var fanin = function (dictCategory) {
    return function (dictChoice) {
        return function (l) {
            return function (r) {
                var join = Data_Profunctor.dimap(dictChoice["__superclass_Data.Profunctor.Profunctor_0"]())(Data_Either.either(Control_Category.id(Control_Category.categoryFn))(Control_Category.id(Control_Category.categoryFn)))(Control_Category.id(Control_Category.categoryFn))(Control_Category.id(dictCategory));
                return Control_Semigroupoid.composeFlipped(dictCategory["__superclass_Control.Semigroupoid.Semigroupoid_0"]())(splitChoice(dictCategory)(dictChoice)(l)(r))(join);
            };
        };
    };
};
var choiceFn = new Choice(function () {
    return Data_Profunctor.profunctorFn;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Data_Either.Left) {
            return Data_Function.apply(Data_Either.Left.create)(v(v1.value0));
        };
        if (v1 instanceof Data_Either.Right) {
            return new Data_Either.Right(v1.value0);
        };
        throw new Error("Failed pattern match at Data.Profunctor.Choice line 33, column 3 - line 33, column 36: " + [ v.constructor.name, v1.constructor.name ]);
    };
}, Data_Functor.map(Data_Either.functorEither));
module.exports = {
    Choice: Choice, 
    fanin: fanin, 
    left: left, 
    right: right, 
    splitChoice: splitChoice, 
    choiceFn: choiceFn
};

// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Identity = require("../Data.Identity");
var Data_Profunctor_Choice = require("../Data.Profunctor.Choice");
var Data_Profunctor_Star = require("../Data.Profunctor.Star");
var Data_Profunctor_Strong = require("../Data.Profunctor.Strong");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Wander = function (__superclass_Data$dotProfunctor$dotChoice$dotChoice_1, __superclass_Data$dotProfunctor$dotStrong$dotStrong_0, wander) {
    this["__superclass_Data.Profunctor.Choice.Choice_1"] = __superclass_Data$dotProfunctor$dotChoice$dotChoice_1;
    this["__superclass_Data.Profunctor.Strong.Strong_0"] = __superclass_Data$dotProfunctor$dotStrong$dotStrong_0;
    this.wander = wander;
};
var wanderStar = function (dictApplicative) {
    return new Wander(function () {
        return Data_Profunctor_Star.choiceStar(dictApplicative);
    }, function () {
        return Data_Profunctor_Star.strongStar((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]());
    }, function (t) {
        return function ($1) {
            return Data_Profunctor_Star.Star(t(dictApplicative)(Data_Profunctor_Star.unStar($1)));
        };
    });
};
var wanderFunction = new Wander(function () {
    return Data_Profunctor_Choice.choiceFn;
}, function () {
    return Data_Profunctor_Strong.strongFn;
}, function (t) {
    return function (f) {
        return function ($2) {
            return Data_Identity.runIdentity(t(Data_Identity.applicativeIdentity)(function ($3) {
                return Data_Identity.Identity(f($3));
            })($2));
        };
    };
});
var wander = function (dict) {
    return dict.wander;
};
module.exports = {
    Wander: Wander, 
    wander: wander, 
    wanderFunction: wanderFunction, 
    wanderStar: wanderStar
};
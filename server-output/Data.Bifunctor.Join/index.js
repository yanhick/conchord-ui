// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Functor = require("../Data.Functor");
var Join = function (x) {
    return x;
};
var runJoin = function (v) {
    return v;
};
var bifunctorJoin = function (dictBifunctor) {
    return new Data_Functor.Functor(function (f) {
        return Data_Functor.map(Data_Functor.functorFn)(Join)(function ($9) {
            return Data_Bifunctor.bimap(dictBifunctor)(f)(f)(runJoin($9));
        });
    });
};
var biapplyJoin = function (dictBiapply) {
    return new Control_Apply.Apply(function () {
        return bifunctorJoin(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]());
    }, function (v) {
        return function (v1) {
            return Control_Biapply.biapply(dictBiapply)(v)(v1);
        };
    });
};
var biapplicativeJoin = function (dictBiapplicative) {
    return new Control_Applicative.Applicative(function () {
        return biapplyJoin(dictBiapplicative["__superclass_Control.Biapply.Biapply_0"]());
    }, function (a) {
        return Control_Biapplicative.bipure(dictBiapplicative)(a)(a);
    });
};
module.exports = {
    Join: Join, 
    runJoin: runJoin, 
    bifunctorJoin: bifunctorJoin, 
    biapplyJoin: biapplyJoin, 
    biapplicativeJoin: biapplicativeJoin
};

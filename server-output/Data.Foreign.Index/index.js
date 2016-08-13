// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Function = require("../Data.Function");
var Control_Applicative = require("../Control.Applicative");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Eq = require("../Data.Eq");
var Index = function (errorAt, hasOwnProperty, hasProperty, ix) {
    this.errorAt = errorAt;
    this.hasOwnProperty = hasOwnProperty;
    this.hasProperty = hasProperty;
    this.ix = ix;
};
var unsafeReadProp = function (k) {
    return function (value) {
        return $foreign.unsafeReadPropImpl(new Data_Either.Left(new Data_Foreign.TypeMismatch("object", Data_Foreign.typeOf(value))), Control_Applicative.pure(Data_Either.applicativeEither), k, value);
    };
};
var prop = unsafeReadProp;
var ix = function (dict) {
    return dict.ix;
};
var index = unsafeReadProp;
var hasPropertyImpl = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return false;
        };
        if (Data_Foreign.isUndefined(value)) {
            return false;
        };
        if (Data_Foreign.typeOf(value) === "object" || Data_Foreign.typeOf(value) === "function") {
            return $foreign.unsafeHasProperty(v, value);
        };
        return false;
    };
};
var hasProperty = function (dict) {
    return dict.hasProperty;
};
var hasOwnPropertyImpl = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return false;
        };
        if (Data_Foreign.isUndefined(value)) {
            return false;
        };
        if (Data_Foreign.typeOf(value) === "object" || Data_Foreign.typeOf(value) === "function") {
            return $foreign.unsafeHasOwnProperty(v, value);
        };
        return false;
    };
};
var indexInt = new Index(Data_Foreign.ErrorAtIndex.create, hasOwnPropertyImpl, hasPropertyImpl, Data_Function.flip(index));
var indexString = new Index(Data_Foreign.ErrorAtProperty.create, hasOwnPropertyImpl, hasPropertyImpl, Data_Function.flip(prop));
var hasOwnProperty = function (dict) {
    return dict.hasOwnProperty;
};
var errorAt = function (dict) {
    return dict.errorAt;
};
module.exports = {
    Index: Index, 
    errorAt: errorAt, 
    hasOwnProperty: hasOwnProperty, 
    hasProperty: hasProperty, 
    index: index, 
    ix: ix, 
    prop: prop, 
    indexString: indexString, 
    indexInt: indexInt
};

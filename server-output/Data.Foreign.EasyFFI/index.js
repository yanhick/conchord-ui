// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Function = require("../Data.Function");
var Data_Semigroup = require("../Data.Semigroup");
var unsafeForeignFunction = function (args) {
    return function (expr) {
        return Data_Function.apply($foreign.unsafeForeignProcedure(args))("return " + (expr + ";"));
    };
};
module.exports = {
    unsafeForeignFunction: unsafeForeignFunction, 
    unsafeForeignProcedure: $foreign.unsafeForeignProcedure
};

// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad_Store_Class = require("../Control.Comonad.Store.Class");
var Control_Comonad_Store_Trans = require("../Control.Comonad.Store.Trans");
var Data_Identity = require("../Data.Identity");
var Data_Tuple = require("../Data.Tuple");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var store = function (f) {
    return function (x) {
        return Data_Function.apply(Control_Comonad_Store_Trans.StoreT)(new Data_Tuple.Tuple(f, x));
    };
};
var runStore = function (v) {
    return Data_Tuple.swap(Data_Functor.map(Data_Tuple.functorTuple)(Data_Identity.runIdentity)(Data_Tuple.swap(v)));
};
module.exports = {
    runStore: runStore, 
    store: store
};

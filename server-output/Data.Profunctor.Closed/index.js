// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Profunctor = require("../Data.Profunctor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Closed = function (__superclass_Data$dotProfunctor$dotProfunctor_0, closed) {
    this["__superclass_Data.Profunctor.Profunctor_0"] = __superclass_Data$dotProfunctor$dotProfunctor_0;
    this.closed = closed;
};
var closedFunction = new Closed(function () {
    return Data_Profunctor.profunctorFn;
}, Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn));
var closed = function (dict) {
    return dict.closed;
};
module.exports = {
    Closed: Closed, 
    closed: closed, 
    closedFunction: closedFunction
};

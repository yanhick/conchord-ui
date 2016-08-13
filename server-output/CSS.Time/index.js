// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var CSS_Property = require("../CSS.Property");
var CSS_String = require("../CSS.String");
var Data_Function = require("../Data.Function");
var Data_Semigroup = require("../Data.Semigroup");
var Time = function (x) {
    return x;
};
var valTime = new CSS_Property.Val(function (v) {
    return v;
});
var sec = function (i) {
    return Data_Function.apply(Time)(Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("s")));
};
var ms = function (i) {
    return Data_Function.apply(Time)(Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("ms")));
};
module.exports = {
    Time: Time, 
    ms: ms, 
    sec: sec, 
    valTime: valTime
};

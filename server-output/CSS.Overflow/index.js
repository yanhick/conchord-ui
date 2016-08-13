// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var CSS_Property = require("../CSS.Property");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Data_Function = require("../Data.Function");
var Overflow = function (x) {
    return x;
};
var visible = Data_Function.apply(Overflow)(CSS_String.fromString(CSS_Property.isStringValue)("visible"));
var valOverflow = new CSS_Property.Val(function (v) {
    return v;
});
var scroll = Data_Function.apply(Overflow)(CSS_String.fromString(CSS_Property.isStringValue)("scroll"));
var overflowY = Data_Function.apply(CSS_Stylesheet.key(valOverflow))(CSS_String.fromString(CSS_Property.isStringKey)("overflow-y"));
var overflowX = Data_Function.apply(CSS_Stylesheet.key(valOverflow))(CSS_String.fromString(CSS_Property.isStringKey)("overflow-x"));
var overflowInherit = Data_Function.apply(Overflow)(CSS_String.fromString(CSS_Property.isStringValue)("inherit"));
var overflowAuto = Data_Function.apply(Overflow)(CSS_String.fromString(CSS_Property.isStringValue)("auto"));
var overflow = Data_Function.apply(CSS_Stylesheet.key(valOverflow))(CSS_String.fromString(CSS_Property.isStringKey)("overflow"));
var hidden = Data_Function.apply(Overflow)(CSS_String.fromString(CSS_Property.isStringValue)("hidden"));
module.exports = {
    Overflow: Overflow, 
    hidden: hidden, 
    overflow: overflow, 
    overflowAuto: overflowAuto, 
    overflowInherit: overflowInherit, 
    overflowX: overflowX, 
    overflowY: overflowY, 
    scroll: scroll, 
    visible: visible, 
    valOverflow: valOverflow
};

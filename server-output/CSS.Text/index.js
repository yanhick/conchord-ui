// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var CSS_Property = require("../CSS.Property");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Data_Function = require("../Data.Function");
var TextDecoration = function (x) {
    return x;
};
var valTextDecoration = new CSS_Property.Val(function (v) {
    return v;
});
var underline = Data_Function.apply(TextDecoration)(CSS_String.fromString(CSS_Property.isStringValue)("underline"));
var textDecoration = Data_Function.apply(CSS_Stylesheet.key(valTextDecoration))(CSS_String.fromString(CSS_Property.isStringKey)("text-decoration"));
var overline = Data_Function.apply(TextDecoration)(CSS_String.fromString(CSS_Property.isStringValue)("overline"));
var noneTextDecoration = Data_Function.apply(TextDecoration)(CSS_String.fromString(CSS_Property.isStringValue)("none"));
var lineThrough = Data_Function.apply(TextDecoration)(CSS_String.fromString(CSS_Property.isStringValue)("line-through"));
var blink = Data_Function.apply(TextDecoration)(CSS_String.fromString(CSS_Property.isStringValue)("blink"));
module.exports = {
    TextDecoration: TextDecoration, 
    blink: blink, 
    lineThrough: lineThrough, 
    noneTextDecoration: noneTextDecoration, 
    overline: overline, 
    textDecoration: textDecoration, 
    underline: underline, 
    valTextDecoration: valTextDecoration
};

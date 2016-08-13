// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var CSS_Common = require("../CSS.Common");
var CSS_Property = require("../CSS.Property");
var CSS_String = require("../CSS.String");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Function = require("../Data.Function");
var Size = function (x) {
    return x;
};
var Angle = function (x) {
    return x;
};
var vw = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("vw"));
};
var vmin = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("vmin"));
};
var vmax = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("vmax"));
};
var vh = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("vh"));
};
var valSize = new CSS_Property.Val(function (v) {
    return v;
});
var valAngle = new CSS_Property.Val(function (v) {
    return v;
});
var sym = function (f) {
    return function (a) {
        return f(a)(a)(a)(a);
    };
};
var rem = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("rem"));
};
var rad = function (i) {
    return Data_Function.apply(Angle)(Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("rad")));
};
var px = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("px"));
};
var pt = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("pt"));
};
var pct = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("%"));
};
var nil = Data_Function.apply(Size)(CSS_String.fromString(CSS_Property.isStringValue)("0"));
var isStringSize = new CSS_String.IsString(function ($4) {
    return Size(CSS_String.fromString(CSS_Property.isStringValue)($4));
});
var ex = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("ex"));
};
var em = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("em"));
};
var deg = function (i) {
    return Data_Function.apply(Angle)(Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("deg")));
};
var autoSize = new CSS_Common.Auto(CSS_String.fromString(isStringSize)("auto"));
module.exports = {
    Angle: Angle, 
    Size: Size, 
    deg: deg, 
    em: em, 
    ex: ex, 
    nil: nil, 
    pct: pct, 
    pt: pt, 
    px: px, 
    rad: rad, 
    rem: rem, 
    sym: sym, 
    vh: vh, 
    vmax: vmax, 
    vmin: vmin, 
    vw: vw, 
    isStringSize: isStringSize, 
    valSize: valSize, 
    autoSize: autoSize, 
    valAngle: valAngle
};

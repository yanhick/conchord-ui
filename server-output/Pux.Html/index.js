// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Data_Array = require("../Data.Array");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Prelude = require("../Prelude");
var Pux_Html_Elements_1 = require("../Pux.Html.Elements");
var Pux_Html_Elements_1 = require("../Pux.Html.Elements");
var Data_Function = require("../Data.Function");
var Data_Unit = require("../Data.Unit");
var withChildren = function (f) {
    return function (htmls) {
        return f([  ])(htmls);
    };
};
var withChild = function (f) {
    return function (html) {
        return Data_Function.apply(f([  ]))(Data_Array.singleton(html));
    };
};
var withAttr = function (f) {
    return function (attr) {
        return function (attrs) {
            return function (children) {
                return f(Data_Array.cons(attr)(attrs))(children);
            };
        };
    };
};
var bind = function (x) {
    return function (f) {
        return $foreign.append(x, f(Data_Unit.unit));
    };
};
module.exports = {
    bind: bind, 
    withAttr: withAttr, 
    withChild: withChild, 
    withChildren: withChildren
};
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Alternative = require("../Control.Alternative");
var Data_Map = require("../Data.Map");
var MatchClass = function (__superclass_Control$dotAlternative$dotAlternative_0, bool, fail, lit, num, param, params, str) {
    this["__superclass_Control.Alternative.Alternative_0"] = __superclass_Control$dotAlternative$dotAlternative_0;
    this.bool = bool;
    this.fail = fail;
    this.lit = lit;
    this.num = num;
    this.param = param;
    this.params = params;
    this.str = str;
};
var str = function (dict) {
    return dict.str;
};
var params = function (dict) {
    return dict.params;
};
var param = function (dict) {
    return dict.param;
};
var num = function (dict) {
    return dict.num;
};
var lit = function (dict) {
    return dict.lit;
};
var fail = function (dict) {
    return dict.fail;
};
var bool = function (dict) {
    return dict.bool;
};
module.exports = {
    MatchClass: MatchClass, 
    bool: bool, 
    fail: fail, 
    lit: lit, 
    num: num, 
    param: param, 
    params: params, 
    str: str
};
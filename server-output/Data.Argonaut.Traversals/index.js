// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Lens = require("../Data.Lens");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Category = require("../Control.Category");
var Data_Lens_Fold = require("../Data.Lens.Fold");
var _JsonString = function (dictWander) {
    return function ($6) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander["__superclass_Data.Profunctor.Choice.Choice_1"]())(Data_Argonaut_Core.isString)($6));
    };
};
var _JsonObject = function (dictWander) {
    return function ($7) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander["__superclass_Data.Profunctor.Choice.Choice_1"]())(Data_Argonaut_Core.isObject)($7));
    };
};
var _JsonNumber = function (dictWander) {
    return function ($8) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander["__superclass_Data.Profunctor.Choice.Choice_1"]())(Data_Argonaut_Core.isNumber)($8));
    };
};
var _JsonNull = function (dictWander) {
    return function ($9) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander["__superclass_Data.Profunctor.Choice.Choice_1"]())(Data_Argonaut_Core.isNull)($9));
    };
};
var _JsonBoolean = function (dictWander) {
    return function ($10) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander["__superclass_Data.Profunctor.Choice.Choice_1"]())(Data_Argonaut_Core.isBoolean)($10));
    };
};
var _JsonArray = function (dictWander) {
    return function ($11) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander["__superclass_Data.Profunctor.Choice.Choice_1"]())(Data_Argonaut_Core.isArray)($11));
    };
};
module.exports = {
    _JsonArray: _JsonArray, 
    _JsonBoolean: _JsonBoolean, 
    _JsonNull: _JsonNull, 
    _JsonNumber: _JsonNumber, 
    _JsonObject: _JsonObject, 
    _JsonString: _JsonString
};

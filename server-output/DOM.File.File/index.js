// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var DOM_File_Blob = require("../DOM.File.Blob");
var DOM_File_Types = require("../DOM.File.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_MediaType = require("../Data.MediaType");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var type_ = function ($0) {
    return DOM_File_Blob.type_(Unsafe_Coerce.unsafeCoerce($0));
};
var size = function ($1) {
    return DOM_File_Blob.size(Unsafe_Coerce.unsafeCoerce($1));
};
module.exports = {
    size: size, 
    type_: type_, 
    name: $foreign.name
};

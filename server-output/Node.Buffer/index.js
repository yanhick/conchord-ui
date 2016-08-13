// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Maybe = require("../Data.Maybe");
var Node_Encoding = require("../Node.Encoding");
var Data_Show = require("../Data.Show");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var UInt8 = (function () {
    function UInt8() {

    };
    UInt8.value = new UInt8();
    return UInt8;
})();
var UInt16LE = (function () {
    function UInt16LE() {

    };
    UInt16LE.value = new UInt16LE();
    return UInt16LE;
})();
var UInt16BE = (function () {
    function UInt16BE() {

    };
    UInt16BE.value = new UInt16BE();
    return UInt16BE;
})();
var UInt32LE = (function () {
    function UInt32LE() {

    };
    UInt32LE.value = new UInt32LE();
    return UInt32LE;
})();
var UInt32BE = (function () {
    function UInt32BE() {

    };
    UInt32BE.value = new UInt32BE();
    return UInt32BE;
})();
var Int8 = (function () {
    function Int8() {

    };
    Int8.value = new Int8();
    return Int8;
})();
var Int16LE = (function () {
    function Int16LE() {

    };
    Int16LE.value = new Int16LE();
    return Int16LE;
})();
var Int16BE = (function () {
    function Int16BE() {

    };
    Int16BE.value = new Int16BE();
    return Int16BE;
})();
var Int32LE = (function () {
    function Int32LE() {

    };
    Int32LE.value = new Int32LE();
    return Int32LE;
})();
var Int32BE = (function () {
    function Int32BE() {

    };
    Int32BE.value = new Int32BE();
    return Int32BE;
})();
var FloatLE = (function () {
    function FloatLE() {

    };
    FloatLE.value = new FloatLE();
    return FloatLE;
})();
var FloatBE = (function () {
    function FloatBE() {

    };
    FloatBE.value = new FloatBE();
    return FloatBE;
})();
var DoubleLE = (function () {
    function DoubleLE() {

    };
    DoubleLE.value = new DoubleLE();
    return DoubleLE;
})();
var DoubleBE = (function () {
    function DoubleBE() {

    };
    DoubleBE.value = new DoubleBE();
    return DoubleBE;
})();
var writeString = function ($2) {
    return $foreign.writeStringImpl(Node_Encoding.encodingToNode($2));
};
var toString = function ($3) {
    return $foreign.toStringImpl(Node_Encoding.encodingToNode($3));
};
var showBufferValueType = new Data_Show.Show(function (v) {
    if (v instanceof UInt8) {
        return "UInt8";
    };
    if (v instanceof UInt16LE) {
        return "UInt16LE";
    };
    if (v instanceof UInt16BE) {
        return "UInt16BE";
    };
    if (v instanceof UInt32LE) {
        return "UInt32LE";
    };
    if (v instanceof UInt32BE) {
        return "UInt32BE";
    };
    if (v instanceof Int8) {
        return "Int8";
    };
    if (v instanceof Int16LE) {
        return "Int16LE";
    };
    if (v instanceof Int16BE) {
        return "Int16BE";
    };
    if (v instanceof Int32LE) {
        return "Int32LE";
    };
    if (v instanceof Int32BE) {
        return "Int32BE";
    };
    if (v instanceof FloatLE) {
        return "FloatLE";
    };
    if (v instanceof FloatBE) {
        return "FloatBE";
    };
    if (v instanceof DoubleLE) {
        return "DoubleLE";
    };
    if (v instanceof DoubleBE) {
        return "DoubleBE";
    };
    throw new Error("Failed pattern match at Node.Buffer line 66, column 3 - line 67, column 3: " + [ v.constructor.name ]);
});
var write = function ($4) {
    return $foreign.writeImpl(Data_Show.show(showBufferValueType)($4));
};
var showBuffer = new Data_Show.Show($foreign.showImpl);
var readString = function ($5) {
    return $foreign.readStringImpl(Node_Encoding.encodingToNode($5));
};
var read = function ($6) {
    return $foreign.readImpl(Data_Show.show(showBufferValueType)($6));
};
var getAtOffset = $foreign.getAtOffsetImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var fromString = function (str) {
    return function ($7) {
        return $foreign.fromStringImpl(str)(Node_Encoding.encodingToNode($7));
    };
};
module.exports = {
    UInt8: UInt8, 
    UInt16LE: UInt16LE, 
    UInt16BE: UInt16BE, 
    UInt32LE: UInt32LE, 
    UInt32BE: UInt32BE, 
    Int8: Int8, 
    Int16LE: Int16LE, 
    Int16BE: Int16BE, 
    Int32LE: Int32LE, 
    Int32BE: Int32BE, 
    FloatLE: FloatLE, 
    FloatBE: FloatBE, 
    DoubleLE: DoubleLE, 
    DoubleBE: DoubleBE, 
    fromString: fromString, 
    getAtOffset: getAtOffset, 
    read: read, 
    readString: readString, 
    toString: toString, 
    write: write, 
    writeString: writeString, 
    showBuffer: showBuffer, 
    showBufferValueType: showBufferValueType, 
    concat: $foreign.concat, 
    "concat'": $foreign["concat'"], 
    copy: $foreign.copy, 
    create: $foreign.create, 
    fill: $foreign.fill, 
    fromArray: $foreign.fromArray, 
    setAtOffset: $foreign.setAtOffset, 
    size: $foreign.size, 
    toArray: $foreign.toArray
};

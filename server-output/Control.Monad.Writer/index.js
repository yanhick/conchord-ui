// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans");
var Data_Identity = require("../Data.Identity");
var Data_Tuple = require("../Data.Tuple");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var runWriter = function ($0) {
    return Data_Identity.runIdentity(Control_Monad_Writer_Trans.runWriterT($0));
};
var mapWriter = function (f) {
    return Control_Monad_Writer_Trans.mapWriterT(function ($1) {
        return Data_Identity.Identity(f(Data_Identity.runIdentity($1)));
    });
};
var execWriter = function (m) {
    return Data_Tuple.snd(runWriter(m));
};
module.exports = {
    execWriter: execWriter, 
    mapWriter: mapWriter, 
    runWriter: runWriter
};

// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_String = require("../Data.String");
var Test_StrongCheck_Arbitrary = require("../Test.StrongCheck.Arbitrary");
var Test_StrongCheck_Gen = require("../Test.StrongCheck.Gen");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Control_Applicative = require("../Control.Applicative");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Monad_Free = require("../Control.Monad.Free");
var AlphaNumString = function (x) {
    return x;
};
var runAlphaNumString = function (v) {
    return v;
};
var eqAlphaNumString = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordAlphaNumString = new Data_Ord.Ord(function () {
    return eqAlphaNumString;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordString)(x)(y);
    };
});
var coarbAlphaNumString = new Test_StrongCheck_Arbitrary.Coarbitrary(function (v) {
    return Test_StrongCheck_Arbitrary.coarbitrary(Test_StrongCheck_Arbitrary.coarbString)(v);
});
var arbAlphaNumString = new Test_StrongCheck_Arbitrary.Arbitrary((function () {
    var rest = Data_String.toCharArray("bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");
    var anyChar = Test_StrongCheck_Gen.oneOf(Control_Monad_Free.freeMonad)(Control_Applicative.pure(Test_StrongCheck_Gen.applicativeGenT(Control_Monad_Free.freeMonad))("a"))(Data_Functor.map(Data_Functor.functorArray)(Control_Applicative.pure(Test_StrongCheck_Gen.applicativeGenT(Control_Monad_Free.freeMonad)))(rest));
    return Data_Functor.map(Test_StrongCheck_Gen.functorGenT(Control_Monad_Free.freeMonad))(function ($16) {
        return AlphaNumString(Data_String.fromCharArray($16));
    })(Test_StrongCheck_Gen.arrayOf(Control_Monad_Free.freeMonad)(anyChar));
})());
module.exports = {
    AlphaNumString: AlphaNumString, 
    runAlphaNumString: runAlphaNumString, 
    eqAlphaNumString: eqAlphaNumString, 
    ordAlphaNumString: ordAlphaNumString, 
    arbAlphaNumString: arbAlphaNumString, 
    coarbAlphaNumString: coarbAlphaNumString
};
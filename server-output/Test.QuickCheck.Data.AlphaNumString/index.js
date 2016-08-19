// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_String = require("../Data.String");
var Test_QuickCheck_Gen = require("../Test.QuickCheck.Gen");
var Test_QuickCheck_Arbitrary = require("../Test.QuickCheck.Arbitrary");
var Control_Applicative = require("../Control.Applicative");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Data_Identity = require("../Data.Identity");
var AlphaNumString = function (x) {
    return x;
};
var runAlphaNumString = function (v) {
    return v;
};
var coarbAlphaNumString = new Test_QuickCheck_Arbitrary.Coarbitrary(function (v) {
    return Test_QuickCheck_Arbitrary.coarbitrary(Test_QuickCheck_Arbitrary.coarbString)(v);
});
var arbAlphaNumString = new Test_QuickCheck_Arbitrary.Arbitrary((function () {
    var rest = Data_String.toCharArray("bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");
    var anyChar = Test_QuickCheck_Gen.oneOf(Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))("a"))(Data_Functor.map(Data_Functor.functorArray)(Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity)))(rest));
    return Data_Functor.map(Control_Monad_State_Trans.functorStateT(Data_Identity.functorIdentity))(function ($4) {
        return AlphaNumString(Data_String.fromCharArray($4));
    })(Test_QuickCheck_Gen.arrayOf(anyChar));
})());
module.exports = {
    AlphaNumString: AlphaNumString, 
    runAlphaNumString: runAlphaNumString, 
    arbAlphaNumString: arbAlphaNumString, 
    coarbAlphaNumString: coarbAlphaNumString
};
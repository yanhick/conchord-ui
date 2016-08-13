// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Color = require("../Color");
var Data_Semiring = require("../Data.Semiring");
var Data_Ring = require("../Data.Ring");
var Data_Ord = require("../Data.Ord");
var Data_Boolean = require("../Data.Boolean");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Multiply = (function () {
    function Multiply() {

    };
    Multiply.value = new Multiply();
    return Multiply;
})();
var Screen = (function () {
    function Screen() {

    };
    Screen.value = new Screen();
    return Screen;
})();
var Overlay = (function () {
    function Overlay() {

    };
    Overlay.value = new Overlay();
    return Overlay;
})();
var blendChannel = function (v) {
    return function (b) {
        return function (f) {
            if (v instanceof Multiply) {
                return b * f;
            };
            if (v instanceof Screen) {
                return 1.0 - (1.0 - b) * (1.0 - f);
            };
            if (v instanceof Overlay) {
                if (b < 0.5) {
                    return 2.0 * b * f;
                };
                if (Data_Boolean.otherwise) {
                    return 1.0 - 2.0 * (1.0 - b) * (1.0 - f);
                };
            };
            throw new Error("Failed pattern match at Color.Blending line 14, column 1 - line 14, column 34: " + [ v.constructor.name, b.constructor.name, f.constructor.name ]);
        };
    };
};
var blend = function (mode) {
    return function (c1) {
        return function (c2) {
            var v2 = Color["toRGBA'"](c2);
            var v1 = Color["toRGBA'"](c1);
            var r = blendChannel(mode)(v1.r)(v2.r);
            var g = blendChannel(mode)(v1.g)(v2.g);
            var b = blendChannel(mode)(v1.b)(v2.b);
            var a = (v1.a + v2.a) / 2.0;
            return Color["rgba'"](r)(g)(b)(a);
        };
    };
};
module.exports = {
    Multiply: Multiply, 
    Screen: Screen, 
    Overlay: Overlay, 
    blend: blend
};

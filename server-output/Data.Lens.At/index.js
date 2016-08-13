// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Identity = require("../Data.Identity");
var Data_Map = require("../Data.Map");
var Data_Maybe = require("../Data.Maybe");
var Data_Set = require("../Data.Set");
var Data_StrMap = require("../Data.StrMap");
var Data_Lens = require("../Data.Lens");
var Data_Lens_Index = require("../Data.Lens.Index");
var Data_Lens_Lens = require("../Data.Lens.Lens");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Control_Category = require("../Control.Category");
var Data_Unit = require("../Data.Unit");
var At = function (__superclass_Data$dotLens$dotIndex$dotIndex_0, at) {
    this["__superclass_Data.Lens.Index.Index_0"] = __superclass_Data$dotLens$dotIndex$dotIndex_0;
    this.at = at;
};
var atStrMap = new At(function () {
    return Data_Lens_Index.indexStrMap;
}, function (k) {
    return function (dictStrong) {
        return Data_Lens_Lens.lens(Data_StrMap.lookup(k))(function (m) {
            return Data_Maybe.maybe(Data_StrMap["delete"](k)(m))(function (v) {
                return Data_StrMap.insert(k)(v)(m);
            });
        })(dictStrong);
    };
});
var atSet = function (dictOrd) {
    return new At(function () {
        return Data_Lens_Index.indexSet(dictOrd);
    }, function (x) {
        return function (dictStrong) {
            var update = function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return Data_Set["delete"](dictOrd)(x);
                };
                if (v instanceof Data_Maybe.Just) {
                    return Data_Set.insert(dictOrd)(x);
                };
                throw new Error("Failed pattern match at Data.Lens.At line 29, column 7 - line 32, column 24: " + [ v.constructor.name ]);
            };
            var get = function (xs) {
                var $13 = Data_Set.member(dictOrd)(x)(xs);
                if ($13) {
                    return new Data_Maybe.Just(Data_Unit.unit);
                };
                if (!$13) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.Lens.At line 30, column 9 - line 32, column 24: " + [ $13.constructor.name ]);
            };
            return Data_Lens_Lens.lens(get)(Data_Function.flip(update))(dictStrong);
        };
    });
};
var atMaybe = new At(function () {
    return Data_Lens_Index.indexMaybe;
}, function (v) {
    return function (dictStrong) {
        return Data_Lens_Lens.lens(Control_Category.id(Control_Category.categoryFn))(function (v1) {
            return Control_Category.id(Control_Category.categoryFn);
        })(dictStrong);
    };
});
var atMap = function (dictOrd) {
    return new At(function () {
        return Data_Lens_Index.indexMap(dictOrd);
    }, function (k) {
        return function (dictStrong) {
            return Data_Lens_Lens.lens(Data_Map.lookup(dictOrd)(k))(function (m) {
                return Data_Maybe.maybe(Data_Map["delete"](dictOrd)(k)(m))(function (v) {
                    return Data_Map.insert(dictOrd)(k)(v)(m);
                });
            })(dictStrong);
        };
    });
};
var atIdentity = new At(function () {
    return Data_Lens_Index.indexIdentity;
}, function (v) {
    return function (dictStrong) {
        return Data_Lens_Lens.lens(function ($15) {
            return Data_Maybe.Just.create(Data_Identity.runIdentity($15));
        })(Data_Function.flip(Data_Maybe.maybe)(Data_Identity.Identity))(dictStrong);
    };
});
var at = function (dict) {
    return dict.at;
};
module.exports = {
    At: At, 
    at: at, 
    atIdentity: atIdentity, 
    atMaybe: atMaybe, 
    atSet: atSet, 
    atMap: atMap, 
    atStrMap: atStrMap
};

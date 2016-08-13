// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Foldable = require("../Data.Foldable");
var Data_Int = require("../Data.Int");
var Data_List = require("../Data.List");
var Color = require("../Color");
var Color_Scheme_X11 = require("../Color.Scheme.X11");
var Data_Ord = require("../Data.Ord");
var Data_Functor = require("../Data.Functor");
var Data_Boolean = require("../Data.Boolean");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Ring = require("../Data.Ring");
var Control_Bind = require("../Control.Bind");
var Data_Function = require("../Data.Function");
var Control_Applicative = require("../Control.Applicative");
var Data_Semiring = require("../Data.Semiring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Monoid = require("../Data.Monoid");
var ColorStop = (function () {
    function ColorStop(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ColorStop.create = function (value0) {
        return function (value1) {
            return new ColorStop(value0, value1);
        };
    };
    return ColorStop;
})();
var ColorScale = (function () {
    function ColorScale(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    ColorScale.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new ColorScale(value0, value1, value2, value3);
                };
            };
        };
    };
    return ColorScale;
})();
var stopRatio = function (v) {
    return v.value1;
};
var stopColor = function (v) {
    return v.value0;
};
var ratio = Data_Ord.clamp(Data_Ord.ordNumber)(0.0)(1.0);
var modify = function (f) {
    return function (v) {
        var f$prime = function (v1) {
            return new ColorStop(f(v1.value1)(v1.value0), v1.value1);
        };
        return new ColorScale(v.value0, f(0.0)(v.value1), Data_Functor.map(Data_List.functorList)(f$prime)(v.value2), f(1.0)(v.value3));
    };
};
var colorStop = function (c) {
    return function (r) {
        return new ColorStop(c, ratio(r));
    };
};
var sample = function (v) {
    return function (x) {
        if (x < 0.0) {
            return v.value1;
        };
        if (x > 1.0) {
            return v.value3;
        };
        if (Data_Boolean.otherwise) {
            var go = function (__copy_v1) {
                return function (__copy_v2) {
                    return function (__copy_v3) {
                        var v1 = __copy_v1;
                        var v2 = __copy_v2;
                        var v3 = __copy_v3;
                        tco: while (true) {
                            if (v3 instanceof Data_List.Nil) {
                                return v1;
                            };
                            if (v3 instanceof Data_List.Cons) {
                                var $39 = Data_Ord.between(Data_Ord.ordNumber)(v2)(v3.value0.value1)(x);
                                if ($39) {
                                    var $40 = v2 === v3.value0.value1;
                                    if ($40) {
                                        return v1;
                                    };
                                    if (!$40) {
                                        return Color.mix(v.value0)(v1)(v3.value0.value0)((x - v2) / (v3.value0.value1 - v2));
                                    };
                                    throw new Error("Failed pattern match at Color.Scale line 94, column 14 - line 96, column 65: " + [ $40.constructor.name ]);
                                };
                                if (!$39) {
                                    var __tco_v1 = v3.value0.value0;
                                    var __tco_v2 = v3.value0.value1;
                                    var __tco_v3 = v3.value1;
                                    v1 = __tco_v1;
                                    v2 = __tco_v2;
                                    v3 = __tco_v3;
                                    continue tco;
                                };
                                throw new Error("Failed pattern match at Color.Scale line 93, column 7 - line 97, column 30: " + [ $39.constructor.name ]);
                            };
                            throw new Error("Failed pattern match at Color.Scale line 86, column 1 - line 97, column 30: " + [ v1.constructor.name, v2.constructor.name, v3.constructor.name ]);
                        };
                    };
                };
            };
            return go(v.value1)(0.0)(Data_List.snoc(v.value2)(colorStop(v.value3)(1.0)));
        };
        throw new Error("Failed pattern match at Color.Scale line 86, column 1 - line 97, column 30: " + [ v.constructor.name, x.constructor.name ]);
    };
};
var colors = function (v) {
    return function (v1) {
        if (v1 === 0) {
            return Data_List.Nil.value;
        };
        if (v1 === 1) {
            return Data_List.singleton(v.value1);
        };
        return Control_Bind.bind(Data_List.bindList)(Data_List.range(0)(v1 - 1))(function (v2) {
            return Data_Function.apply(Control_Applicative.pure(Data_List.applicativeList))(sample(v)(Data_Int.toNumber(v2) / Data_Int.toNumber(v1 - 1)));
        });
    };
};
var colorScale = ColorScale.create;
var cool = colorScale(Color.RGB.value)(Color.hsl(180.0)(1.0)(0.6))(Data_List.Nil.value)(Color.hsl(300.0)(1.0)(0.5));
var grayscale = colorScale(Color.RGB.value)(Color.black)(Data_List.Nil.value)(Color.white);
var hot = colorScale(Color.RGB.value)(Color.black)(new Data_List.Cons(colorStop(Color_Scheme_X11.red)(0.5), new Data_List.Cons(colorStop(Color_Scheme_X11.yellow)(0.75), Data_List.Nil.value)))(Color.white);
var spectrum = (function () {
    var stops = Control_Bind.bind(Data_List.bindList)(Data_List.range(1)(35))(function (v) {
        var r = Data_Int.toNumber(v);
        return Data_Function.apply(Control_Applicative.pure(Data_List.applicativeList))(colorStop(Color.hsl(10.0 * r)(1.0)(0.5))(r / 36.0));
    });
    var end = Color.hsl(0.0)(1.0)(0.5);
    return colorScale(Color.HSL.value)(end)(stops)(end);
})();
var spectrumLCh = (function () {
    var end = Color.lch(70.0)(35.0)(0.0);
    var stops = Control_Bind.bind(Data_List.bindList)(Data_List.range(1)(35))(function (v) {
        var r = Data_Int.toNumber(v);
        return Data_Function.apply(Control_Applicative.pure(Data_List.applicativeList))(colorStop(Color.lch(70.0)(35.0)(10.0 * r))(r / 36.0));
    });
    return colorScale(Color.LCh.value)(end)(stops)(end);
})();
var uniformScale = function (dictFoldable) {
    return function (mode) {
        return function (b) {
            return function (middle) {
                return function (e) {
                    var cs = Data_List.fromFoldable(dictFoldable)(middle);
                    var len = Data_List.length(cs);
                    var n = 1 + len | 0;
                    var makeStop = function (i) {
                        return function (col) {
                            return colorStop(col)(Data_Int.toNumber(i) / Data_Int.toNumber(n));
                        };
                    };
                    var stops = Data_List.zipWith(makeStop)(Data_List.range(1)(n))(cs);
                    return colorScale(mode)(b)(stops)(e);
                };
            };
        };
    };
};
var yellowToRed = (function () {
    var yellow = Color.fromInt(16777164);
    var red = Color.fromInt(8388646);
    var orange = Color.fromInt(16616764);
    return uniformScale(Data_List.foldableList)(Color.Lab.value)(yellow)(new Data_List.Cons(orange, Data_List.Nil.value))(red);
})();
var blueToRed = (function () {
    var red = Color.fromInt(11671595);
    var gray = Color.fromInt(16250871);
    var blue = Color.fromInt(2188972);
    return uniformScale(Data_List.foldableList)(Color.Lab.value)(blue)(new Data_List.Cons(gray, Data_List.Nil.value))(red);
})();
var addStop = function (v) {
    return function (c) {
        return function (r) {
            var stop = colorStop(c)(r);
            return new ColorScale(v.value0, v.value1, Data_List.insertBy(Data_Ord.comparing(Data_Ord.ordNumber)(stopRatio))(stop)(v.value2), v.value3);
        };
    };
};
var cssColorStops = function (__copy_v) {
    var v = __copy_v;
    tco: while (true) {
        if (v.value0 instanceof Color.RGB && v.value2 instanceof Data_List.Nil) {
            return Color.cssStringHSLA(v.value1) + (", " + Color.cssStringHSLA(v.value3));
        };
        if (v.value0 instanceof Color.RGB) {
            var percentage = function (r) {
                return Data_Show.show(Data_Show.showNumber)(r * 100.0) + "%";
            };
            var toString = function (v1) {
                return Color.cssStringHSLA(v1.value0) + (" " + percentage(v1.value1));
            };
            return Color.cssStringHSLA(v.value1) + (", " + (Data_Foldable.intercalate(Data_List.foldableList)(Data_Monoid.monoidString)(", ")(Data_Functor.map(Data_List.functorList)(toString)(v.value2)) + (", " + Color.cssStringHSLA(v.value3))));
        };
        var csRGB$prime = new ColorScale(Color.RGB.value, v.value1, v.value2, v.value3);
        var additionalStops = Control_Bind.bind(Data_List.bindList)(Data_List.range(1)(9))(function (v1) {
            var frac = ratio(Data_Int.toNumber(v1) / 10.0);
            return Data_Function.apply(Control_Applicative.pure(Data_List.applicativeList))(new ColorStop(sample(v)(frac), frac));
        });
        var addStop$prime = function (scale1) {
            return function (v1) {
                return addStop(scale1)(v1.value0)(v1.value1);
            };
        };
        var csRGB = Data_Foldable.foldl(Data_List.foldableList)(addStop$prime)(csRGB$prime)(additionalStops);
        v = csRGB;
        continue tco;
    };
};
module.exports = {
    addStop: addStop, 
    blueToRed: blueToRed, 
    colorScale: colorScale, 
    colorStop: colorStop, 
    colors: colors, 
    cool: cool, 
    cssColorStops: cssColorStops, 
    grayscale: grayscale, 
    hot: hot, 
    modify: modify, 
    sample: sample, 
    spectrum: spectrum, 
    spectrumLCh: spectrumLCh, 
    uniformScale: uniformScale, 
    yellowToRed: yellowToRed
};

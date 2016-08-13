// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Enum = require("../Data.Enum");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Control_Apply = require("../Control.Apply");
var Data_Unit = require("../Data.Unit");
var Data_Bounded = require("../Data.Bounded");
var Data_Ring = require("../Data.Ring");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Semiring = require("../Data.Semiring");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Boolean = require("../Data.Boolean");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Ordering = require("../Data.Ordering");
var Year = function (x) {
    return x;
};
var Monday = (function () {
    function Monday() {

    };
    Monday.value = new Monday();
    return Monday;
})();
var Tuesday = (function () {
    function Tuesday() {

    };
    Tuesday.value = new Tuesday();
    return Tuesday;
})();
var Wednesday = (function () {
    function Wednesday() {

    };
    Wednesday.value = new Wednesday();
    return Wednesday;
})();
var Thursday = (function () {
    function Thursday() {

    };
    Thursday.value = new Thursday();
    return Thursday;
})();
var Friday = (function () {
    function Friday() {

    };
    Friday.value = new Friday();
    return Friday;
})();
var Saturday = (function () {
    function Saturday() {

    };
    Saturday.value = new Saturday();
    return Saturday;
})();
var Sunday = (function () {
    function Sunday() {

    };
    Sunday.value = new Sunday();
    return Sunday;
})();
var January = (function () {
    function January() {

    };
    January.value = new January();
    return January;
})();
var February = (function () {
    function February() {

    };
    February.value = new February();
    return February;
})();
var March = (function () {
    function March() {

    };
    March.value = new March();
    return March;
})();
var April = (function () {
    function April() {

    };
    April.value = new April();
    return April;
})();
var May = (function () {
    function May() {

    };
    May.value = new May();
    return May;
})();
var June = (function () {
    function June() {

    };
    June.value = new June();
    return June;
})();
var July = (function () {
    function July() {

    };
    July.value = new July();
    return July;
})();
var August = (function () {
    function August() {

    };
    August.value = new August();
    return August;
})();
var September = (function () {
    function September() {

    };
    September.value = new September();
    return September;
})();
var October = (function () {
    function October() {

    };
    October.value = new October();
    return October;
})();
var November = (function () {
    function November() {

    };
    November.value = new November();
    return November;
})();
var December = (function () {
    function December() {

    };
    December.value = new December();
    return December;
})();
var Day = function (x) {
    return x;
};
var unYear = function (v) {
    return v;
};
var showYear = new Data_Show.Show(function (v) {
    return "(Year " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var showWeekday = new Data_Show.Show(function (v) {
    if (v instanceof Monday) {
        return "Monday";
    };
    if (v instanceof Tuesday) {
        return "Tuesday";
    };
    if (v instanceof Wednesday) {
        return "Wednesday";
    };
    if (v instanceof Thursday) {
        return "Thursday";
    };
    if (v instanceof Friday) {
        return "Friday";
    };
    if (v instanceof Saturday) {
        return "Saturday";
    };
    if (v instanceof Sunday) {
        return "Sunday";
    };
    throw new Error("Failed pattern match at Data.Date.Component line 194, column 3 - line 195, column 3: " + [ v.constructor.name ]);
});
var showMonth = new Data_Show.Show(function (v) {
    if (v instanceof January) {
        return "January";
    };
    if (v instanceof February) {
        return "February";
    };
    if (v instanceof March) {
        return "March";
    };
    if (v instanceof April) {
        return "April";
    };
    if (v instanceof May) {
        return "May";
    };
    if (v instanceof June) {
        return "June";
    };
    if (v instanceof July) {
        return "July";
    };
    if (v instanceof August) {
        return "August";
    };
    if (v instanceof September) {
        return "September";
    };
    if (v instanceof October) {
        return "October";
    };
    if (v instanceof November) {
        return "November";
    };
    if (v instanceof December) {
        return "December";
    };
    throw new Error("Failed pattern match at Data.Date.Component line 109, column 3 - line 110, column 3: " + [ v.constructor.name ]);
});
var showDay = new Data_Show.Show(function (v) {
    return "(Day " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var genericYear = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Year" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Year))(Data_Generic.fromSpine(Data_Generic.genericInt)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Date.Component.Year", [ {
        sigConstructor: "Data.Date.Component.Year", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Date.Component.Year", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var genericWeekday = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Monday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Monday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Tuesday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Tuesday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Wednesday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Wednesday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Thursday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Thursday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Friday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Friday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Saturday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Saturday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Sunday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Sunday.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Date.Component.Weekday", [ {
        sigConstructor: "Data.Date.Component.Monday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Tuesday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Wednesday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Thursday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Friday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Saturday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Sunday", 
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof Monday) {
        return new Data_Generic.SProd("Data.Date.Component.Monday", [  ]);
    };
    if (v instanceof Tuesday) {
        return new Data_Generic.SProd("Data.Date.Component.Tuesday", [  ]);
    };
    if (v instanceof Wednesday) {
        return new Data_Generic.SProd("Data.Date.Component.Wednesday", [  ]);
    };
    if (v instanceof Thursday) {
        return new Data_Generic.SProd("Data.Date.Component.Thursday", [  ]);
    };
    if (v instanceof Friday) {
        return new Data_Generic.SProd("Data.Date.Component.Friday", [  ]);
    };
    if (v instanceof Saturday) {
        return new Data_Generic.SProd("Data.Date.Component.Saturday", [  ]);
    };
    if (v instanceof Sunday) {
        return new Data_Generic.SProd("Data.Date.Component.Sunday", [  ]);
    };
    throw new Error("Failed pattern match at Data.Date.Component line 163, column 1 - line 163, column 50: " + [ v.constructor.name ]);
});
var genericMonth = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.January" && v.value1.length === 0)) {
        return new Data_Maybe.Just(January.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.February" && v.value1.length === 0)) {
        return new Data_Maybe.Just(February.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.March" && v.value1.length === 0)) {
        return new Data_Maybe.Just(March.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.April" && v.value1.length === 0)) {
        return new Data_Maybe.Just(April.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.May" && v.value1.length === 0)) {
        return new Data_Maybe.Just(May.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.June" && v.value1.length === 0)) {
        return new Data_Maybe.Just(June.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.July" && v.value1.length === 0)) {
        return new Data_Maybe.Just(July.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.August" && v.value1.length === 0)) {
        return new Data_Maybe.Just(August.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.September" && v.value1.length === 0)) {
        return new Data_Maybe.Just(September.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.October" && v.value1.length === 0)) {
        return new Data_Maybe.Just(October.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.November" && v.value1.length === 0)) {
        return new Data_Maybe.Just(November.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.December" && v.value1.length === 0)) {
        return new Data_Maybe.Just(December.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Date.Component.Month", [ {
        sigConstructor: "Data.Date.Component.January", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.February", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.March", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.April", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.May", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.June", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.July", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.August", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.September", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.October", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.November", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.December", 
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof January) {
        return new Data_Generic.SProd("Data.Date.Component.January", [  ]);
    };
    if (v instanceof February) {
        return new Data_Generic.SProd("Data.Date.Component.February", [  ]);
    };
    if (v instanceof March) {
        return new Data_Generic.SProd("Data.Date.Component.March", [  ]);
    };
    if (v instanceof April) {
        return new Data_Generic.SProd("Data.Date.Component.April", [  ]);
    };
    if (v instanceof May) {
        return new Data_Generic.SProd("Data.Date.Component.May", [  ]);
    };
    if (v instanceof June) {
        return new Data_Generic.SProd("Data.Date.Component.June", [  ]);
    };
    if (v instanceof July) {
        return new Data_Generic.SProd("Data.Date.Component.July", [  ]);
    };
    if (v instanceof August) {
        return new Data_Generic.SProd("Data.Date.Component.August", [  ]);
    };
    if (v instanceof September) {
        return new Data_Generic.SProd("Data.Date.Component.September", [  ]);
    };
    if (v instanceof October) {
        return new Data_Generic.SProd("Data.Date.Component.October", [  ]);
    };
    if (v instanceof November) {
        return new Data_Generic.SProd("Data.Date.Component.November", [  ]);
    };
    if (v instanceof December) {
        return new Data_Generic.SProd("Data.Date.Component.December", [  ]);
    };
    throw new Error("Failed pattern match at Data.Date.Component line 68, column 1 - line 68, column 46: " + [ v.constructor.name ]);
});
var genericDay = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Day" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Day))(Data_Generic.fromSpine(Data_Generic.genericInt)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Date.Component.Day", [ {
        sigConstructor: "Data.Date.Component.Day", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Date.Component.Day", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var eqYear = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordYear = new Data_Ord.Ord(function () {
    return eqYear;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordInt)(x)(y);
    };
});
var eqWeekday = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Monday && y instanceof Monday) {
            return true;
        };
        if (x instanceof Tuesday && y instanceof Tuesday) {
            return true;
        };
        if (x instanceof Wednesday && y instanceof Wednesday) {
            return true;
        };
        if (x instanceof Thursday && y instanceof Thursday) {
            return true;
        };
        if (x instanceof Friday && y instanceof Friday) {
            return true;
        };
        if (x instanceof Saturday && y instanceof Saturday) {
            return true;
        };
        if (x instanceof Sunday && y instanceof Sunday) {
            return true;
        };
        return false;
    };
});
var ordWeekday = new Data_Ord.Ord(function () {
    return eqWeekday;
}, function (x) {
    return function (y) {
        if (x instanceof Monday && y instanceof Monday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Monday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Monday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Tuesday && y instanceof Tuesday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Tuesday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Tuesday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Wednesday && y instanceof Wednesday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Wednesday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Wednesday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Thursday && y instanceof Thursday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Thursday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Thursday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Friday && y instanceof Friday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Friday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Friday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Saturday && y instanceof Saturday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Saturday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Saturday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Sunday && y instanceof Sunday) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at Data.Date.Component line 162, column 1 - line 162, column 42: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var eqMonth = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof January && y instanceof January) {
            return true;
        };
        if (x instanceof February && y instanceof February) {
            return true;
        };
        if (x instanceof March && y instanceof March) {
            return true;
        };
        if (x instanceof April && y instanceof April) {
            return true;
        };
        if (x instanceof May && y instanceof May) {
            return true;
        };
        if (x instanceof June && y instanceof June) {
            return true;
        };
        if (x instanceof July && y instanceof July) {
            return true;
        };
        if (x instanceof August && y instanceof August) {
            return true;
        };
        if (x instanceof September && y instanceof September) {
            return true;
        };
        if (x instanceof October && y instanceof October) {
            return true;
        };
        if (x instanceof November && y instanceof November) {
            return true;
        };
        if (x instanceof December && y instanceof December) {
            return true;
        };
        return false;
    };
});
var ordMonth = new Data_Ord.Ord(function () {
    return eqMonth;
}, function (x) {
    return function (y) {
        if (x instanceof January && y instanceof January) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof January) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof January) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof February && y instanceof February) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof February) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof February) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof March && y instanceof March) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof March) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof March) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof April && y instanceof April) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof April) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof April) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof May && y instanceof May) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof May) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof May) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof June && y instanceof June) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof June) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof June) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof July && y instanceof July) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof July) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof July) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof August && y instanceof August) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof August) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof August) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof September && y instanceof September) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof September) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof September) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof October && y instanceof October) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof October) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof October) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof November && y instanceof November) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof November) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof November) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof December && y instanceof December) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at Data.Date.Component line 67, column 1 - line 67, column 38: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var eqDay = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordDay = new Data_Ord.Ord(function () {
    return eqDay;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordInt)(x)(y);
    };
});
var boundedYear = new Data_Bounded.Bounded(function () {
    return ordYear;
}, -271820, 275759);
var boundedWeekday = new Data_Bounded.Bounded(function () {
    return ordWeekday;
}, Monday.value, Sunday.value);
var boundedMonth = new Data_Bounded.Bounded(function () {
    return ordMonth;
}, January.value, December.value);
var boundedEnumYear = new Data_Enum.BoundedEnum(function () {
    return boundedYear;
}, function () {
    return enumYear;
}, 547580, function (v) {
    return v;
}, function (n) {
    if (n >= -271821 && n <= 275759) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Date.Component line 43, column 3 - line 45, column 26: " + [ n.constructor.name ]);
});
var enumYear = new Data_Enum.Enum(function () {
    return ordYear;
}, function ($136) {
    return Data_Enum.toEnum(boundedEnumYear)((function (v) {
        return v - 1;
    })(Data_Enum.fromEnum(boundedEnumYear)($136)));
}, function ($137) {
    return Data_Enum.toEnum(boundedEnumYear)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumYear)($137)));
});
var boundedEnumWeekday = new Data_Enum.BoundedEnum(function () {
    return boundedWeekday;
}, function () {
    return enumWeekday;
}, 7, function (v) {
    if (v instanceof Monday) {
        return 1;
    };
    if (v instanceof Tuesday) {
        return 2;
    };
    if (v instanceof Wednesday) {
        return 3;
    };
    if (v instanceof Thursday) {
        return 4;
    };
    if (v instanceof Friday) {
        return 5;
    };
    if (v instanceof Saturday) {
        return 6;
    };
    if (v instanceof Sunday) {
        return 7;
    };
    throw new Error("Failed pattern match at Data.Date.Component line 184, column 14 - line 193, column 1: " + [ v.constructor.name ]);
}, function (v) {
    if (v === 1) {
        return new Data_Maybe.Just(Monday.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(Tuesday.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(Wednesday.value);
    };
    if (v === 4) {
        return new Data_Maybe.Just(Thursday.value);
    };
    if (v === 5) {
        return new Data_Maybe.Just(Friday.value);
    };
    if (v === 6) {
        return new Data_Maybe.Just(Saturday.value);
    };
    if (v === 7) {
        return new Data_Maybe.Just(Sunday.value);
    };
    return Data_Maybe.Nothing.value;
});
var enumWeekday = new Data_Enum.Enum(function () {
    return ordWeekday;
}, function ($138) {
    return Data_Enum.toEnum(boundedEnumWeekday)((function (v) {
        return v - 1;
    })(Data_Enum.fromEnum(boundedEnumWeekday)($138)));
}, function ($139) {
    return Data_Enum.toEnum(boundedEnumWeekday)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumWeekday)($139)));
});
var boundedEnumMonth = new Data_Enum.BoundedEnum(function () {
    return boundedMonth;
}, function () {
    return enumMonth;
}, 12, function (v) {
    if (v instanceof January) {
        return 1;
    };
    if (v instanceof February) {
        return 2;
    };
    if (v instanceof March) {
        return 3;
    };
    if (v instanceof April) {
        return 4;
    };
    if (v instanceof May) {
        return 5;
    };
    if (v instanceof June) {
        return 6;
    };
    if (v instanceof July) {
        return 7;
    };
    if (v instanceof August) {
        return 8;
    };
    if (v instanceof September) {
        return 9;
    };
    if (v instanceof October) {
        return 10;
    };
    if (v instanceof November) {
        return 11;
    };
    if (v instanceof December) {
        return 12;
    };
    throw new Error("Failed pattern match at Data.Date.Component line 94, column 14 - line 108, column 1: " + [ v.constructor.name ]);
}, function (v) {
    if (v === 1) {
        return new Data_Maybe.Just(January.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(February.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(March.value);
    };
    if (v === 4) {
        return new Data_Maybe.Just(April.value);
    };
    if (v === 5) {
        return new Data_Maybe.Just(May.value);
    };
    if (v === 6) {
        return new Data_Maybe.Just(June.value);
    };
    if (v === 7) {
        return new Data_Maybe.Just(July.value);
    };
    if (v === 8) {
        return new Data_Maybe.Just(August.value);
    };
    if (v === 9) {
        return new Data_Maybe.Just(September.value);
    };
    if (v === 10) {
        return new Data_Maybe.Just(October.value);
    };
    if (v === 11) {
        return new Data_Maybe.Just(November.value);
    };
    if (v === 12) {
        return new Data_Maybe.Just(December.value);
    };
    return Data_Maybe.Nothing.value;
});
var enumMonth = new Data_Enum.Enum(function () {
    return ordMonth;
}, function ($140) {
    return Data_Enum.toEnum(boundedEnumMonth)((function (v) {
        return v - 1;
    })(Data_Enum.fromEnum(boundedEnumMonth)($140)));
}, function ($141) {
    return Data_Enum.toEnum(boundedEnumMonth)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMonth)($141)));
});
var boundedDay = new Data_Bounded.Bounded(function () {
    return ordDay;
}, 1, 31);
var boundedEnumDay = new Data_Enum.BoundedEnum(function () {
    return boundedDay;
}, function () {
    return enumDay;
}, 31, function (v) {
    return v;
}, function (n) {
    if (n >= 1 && n <= 31) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Date.Component line 143, column 3 - line 145, column 26: " + [ n.constructor.name ]);
});
var enumDay = new Data_Enum.Enum(function () {
    return ordDay;
}, function ($142) {
    return Data_Enum.toEnum(boundedEnumDay)((function (v) {
        return v - 1;
    })(Data_Enum.fromEnum(boundedEnumDay)($142)));
}, function ($143) {
    return Data_Enum.toEnum(boundedEnumDay)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumDay)($143)));
});
module.exports = {
    January: January, 
    February: February, 
    March: March, 
    April: April, 
    May: May, 
    June: June, 
    July: July, 
    August: August, 
    September: September, 
    October: October, 
    November: November, 
    December: December, 
    Monday: Monday, 
    Tuesday: Tuesday, 
    Wednesday: Wednesday, 
    Thursday: Thursday, 
    Friday: Friday, 
    Saturday: Saturday, 
    Sunday: Sunday, 
    eqYear: eqYear, 
    ordYear: ordYear, 
    genericYear: genericYear, 
    boundedYear: boundedYear, 
    enumYear: enumYear, 
    boundedEnumYear: boundedEnumYear, 
    showYear: showYear, 
    eqMonth: eqMonth, 
    ordMonth: ordMonth, 
    genericMonth: genericMonth, 
    boundedMonth: boundedMonth, 
    enumMonth: enumMonth, 
    boundedEnumMonth: boundedEnumMonth, 
    showMonth: showMonth, 
    eqDay: eqDay, 
    ordDay: ordDay, 
    genericDay: genericDay, 
    boundedDay: boundedDay, 
    enumDay: enumDay, 
    boundedEnumDay: boundedEnumDay, 
    showDay: showDay, 
    eqWeekday: eqWeekday, 
    ordWeekday: ordWeekday, 
    genericWeekday: genericWeekday, 
    boundedWeekday: boundedWeekday, 
    enumWeekday: enumWeekday, 
    boundedEnumWeekday: boundedEnumWeekday, 
    showWeekday: showWeekday
};

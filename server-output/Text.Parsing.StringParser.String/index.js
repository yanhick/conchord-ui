// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Data_Array = require("../Data.Array");
var Data_Char = require("../Data.Char");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Maybe = require("../Data.Maybe");
var Data_String = require("../Data.String");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var Data_Eq = require("../Data.Eq");
var Data_Semiring = require("../Data.Semiring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Ord = require("../Data.Ord");
var Data_Unit = require("../Data.Unit");
var Control_Bind = require("../Control.Bind");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Control_Applicative = require("../Control.Applicative");
var Data_Function = require("../Data.Function");
var Data_Show = require("../Data.Show");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_List = require("../Data.List");
var Data_Monoid = require("../Data.Monoid");
var Data_Functor = require("../Data.Functor");
var string = function (nt) {
    return new Text_Parsing_StringParser.Parser(function (s) {
        if (Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqInt))(Data_String["indexOf'"](nt)(s.pos)(s.str))(new Data_Maybe.Just(s.pos))) {
            return new Data_Either.Right({
                result: nt, 
                suffix: {
                    str: s.str, 
                    pos: s.pos + Data_String.length(nt) | 0
                }
            });
        };
        return new Data_Either.Left({
            pos: s.pos, 
            error: new Text_Parsing_StringParser.ParseError("Expected '" + (nt + "'."))
        });
    });
};
var eof = new Text_Parsing_StringParser.Parser(function (s) {
    if (s.pos < Data_String.length(s.str)) {
        return new Data_Either.Left({
            pos: s.pos, 
            error: new Text_Parsing_StringParser.ParseError("Expected EOF")
        });
    };
    return new Data_Either.Right({
        result: Data_Unit.unit, 
        suffix: s
    });
});
var anyChar = new Text_Parsing_StringParser.Parser(function (v) {
    var $17 = Data_String.charAt(v.pos)(v.str);
    if ($17 instanceof Data_Maybe.Just) {
        return new Data_Either.Right({
            result: $17.value0, 
            suffix: {
                str: v.str, 
                pos: v.pos + 1 | 0
            }
        });
    };
    if ($17 instanceof Data_Maybe.Nothing) {
        return new Data_Either.Left({
            pos: v.pos, 
            error: new Text_Parsing_StringParser.ParseError("Unexpected EOF")
        });
    };
    throw new Error("Failed pattern match at Text.Parsing.StringParser.String line 42, column 3 - line 44, column 64: " + [ $17.constructor.name ]);
});
var anyDigit = Text_Parsing_StringParser["try"](Control_Bind.bind(Text_Parsing_StringParser.bindParser)(anyChar)(function (v) {
    var $22 = v >= "0" && v <= "9";
    if ($22) {
        return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(v);
    };
    if (!$22) {
        return Data_Function.apply(Text_Parsing_StringParser.fail)("Character " + (Data_Show.show(Data_Show.showChar)(v) + " is not a digit"));
    };
    throw new Error("Failed pattern match at Text.Parsing.StringParser.String line 50, column 3 - line 54, column 1: " + [ $22.constructor.name ]);
}));
var lowerCaseChar = Text_Parsing_StringParser["try"](Control_Bind.bind(Text_Parsing_StringParser.bindParser)(anyChar)(function (v) {
    var $24 = Data_Foldable.elem(Data_Foldable.foldableArray)(Data_Eq.eqInt)(Data_Char.toCharCode(v))(Data_Array.range(97)(122));
    if ($24) {
        return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(v);
    };
    if (!$24) {
        return Data_Function.apply(Text_Parsing_StringParser.fail)("Expected a lower case character but found " + Data_Show.show(Data_Show.showChar)(v));
    };
    throw new Error("Failed pattern match at Text.Parsing.StringParser.String line 95, column 3 - line 97, column 72: " + [ $24.constructor.name ]);
}));
var satisfy = function (f) {
    return Text_Parsing_StringParser["try"](Control_Bind.bind(Text_Parsing_StringParser.bindParser)(anyChar)(function (v) {
        var $26 = f(v);
        if ($26) {
            return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(v);
        };
        if (!$26) {
            return Data_Function.apply(Text_Parsing_StringParser.fail)("Character " + (Data_Show.show(Data_Show.showChar)(v) + " did not satisfy predicate"));
        };
        throw new Error("Failed pattern match at Text.Parsing.StringParser.String line 65, column 3 - line 69, column 1: " + [ $26.constructor.name ]);
    }));
};
var $$char = function (c) {
    return Text_Parsing_StringParser_Combinators.withError(satisfy(function (v) {
        return v === c;
    }))("Could not match character " + Data_Show.show(Data_Show.showChar)(c));
};
var noneOf = function (dictFoldable) {
    return function ($30) {
        return satisfy(Data_Function.flip(Data_Foldable.notElem(dictFoldable)(Data_Eq.eqChar))($30));
    };
};
var oneOf = function (dictFoldable) {
    return function ($31) {
        return satisfy(Data_Function.flip(Data_Foldable.elem(dictFoldable)(Data_Eq.eqChar))($31));
    };
};
var whiteSpace = Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_Combinators.many(satisfy(function (c) {
    return c === "\n" || (c === "\r" || (c === " " || c === "\t"));
})))(function (v) {
    return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(Data_Foldable.foldMap(Data_List.foldableList)(Data_Monoid.monoidString)(Data_String.singleton)(v));
});
var skipSpaces = Data_Functor["void"](Text_Parsing_StringParser.functorParser)(whiteSpace);
var upperCaseChar = Text_Parsing_StringParser["try"](Control_Bind.bind(Text_Parsing_StringParser.bindParser)(anyChar)(function (v) {
    var $29 = Data_Foldable.elem(Data_Foldable.foldableArray)(Data_Eq.eqInt)(Data_Char.toCharCode(v))(Data_Array.range(65)(90));
    if ($29) {
        return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(v);
    };
    if (!$29) {
        return Data_Function.apply(Text_Parsing_StringParser.fail)("Expected an upper case character but found " + Data_Show.show(Data_Show.showChar)(v));
    };
    throw new Error("Failed pattern match at Text.Parsing.StringParser.String line 103, column 3 - line 105, column 73: " + [ $29.constructor.name ]);
}));
var anyLetter = Text_Parsing_StringParser_Combinators.withError(Control_Alt.alt(Text_Parsing_StringParser.altParser)(lowerCaseChar)(upperCaseChar))("Expected a letter");
var alphaNum = Text_Parsing_StringParser_Combinators.withError(Control_Alt.alt(Text_Parsing_StringParser.altParser)(anyLetter)(anyDigit))("Expected a letter or a number");
module.exports = {
    alphaNum: alphaNum, 
    anyChar: anyChar, 
    anyDigit: anyDigit, 
    anyLetter: anyLetter, 
    "char": $$char, 
    eof: eof, 
    lowerCaseChar: lowerCaseChar, 
    noneOf: noneOf, 
    oneOf: oneOf, 
    satisfy: satisfy, 
    skipSpaces: skipSpaces, 
    string: string, 
    upperCaseChar: upperCaseChar, 
    whiteSpace: whiteSpace
};
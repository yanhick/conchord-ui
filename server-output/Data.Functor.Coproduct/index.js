// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Traversable = require("../Data.Traversable");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Coproduct = function (x) {
    return x;
};
var unCoproduct = function (v) {
    return v;
};
var right = function (ga) {
    return new Data_Either.Right(ga);
};
var left = function (fa) {
    return new Data_Either.Left(fa);
};
var functorCoproduct = function (dictFunctor) {
    return function (dictFunctor1) {
        return new Data_Functor.Functor(function (f) {
            return function (v) {
                return Data_Bifunctor.bimap(Data_Either.bifunctorEither)(Data_Functor.map(dictFunctor)(f))(Data_Functor.map(dictFunctor1)(f))(v);
            };
        });
    };
};
var coproduct = function (f) {
    return function (g) {
        return function (v) {
            return Data_Either.either(f)(g)(v);
        };
    };
};
var foldableCoproduct = function (dictFoldable) {
    return function (dictFoldable1) {
        return new Data_Foldable.Foldable(function (dictMonoid) {
            return function (f) {
                return coproduct(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(f))(Data_Foldable.foldMap(dictFoldable1)(dictMonoid)(f));
            };
        }, function (f) {
            return function (z) {
                return coproduct(Data_Foldable.foldl(dictFoldable)(f)(z))(Data_Foldable.foldl(dictFoldable1)(f)(z));
            };
        }, function (f) {
            return function (z) {
                return coproduct(Data_Foldable.foldr(dictFoldable)(f)(z))(Data_Foldable.foldr(dictFoldable1)(f)(z));
            };
        });
    };
};
var traversableCoproduct = function (dictTraversable) {
    return function (dictTraversable1) {
        return new Data_Traversable.Traversable(function () {
            return foldableCoproduct(dictTraversable["__superclass_Data.Foldable.Foldable_1"]())(dictTraversable1["__superclass_Data.Foldable.Foldable_1"]());
        }, function () {
            return functorCoproduct(dictTraversable["__superclass_Data.Functor.Functor_0"]())(dictTraversable1["__superclass_Data.Functor.Functor_0"]());
        }, function (dictApplicative) {
            return coproduct(function ($22) {
                return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(function ($23) {
                    return Coproduct(Data_Either.Left.create($23));
                })(Data_Traversable.sequence(dictTraversable)(dictApplicative)($22));
            })(function ($24) {
                return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(function ($25) {
                    return Coproduct(Data_Either.Right.create($25));
                })(Data_Traversable.sequence(dictTraversable1)(dictApplicative)($24));
            });
        }, function (dictApplicative) {
            return function (f) {
                return coproduct(function ($26) {
                    return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(function ($27) {
                        return Coproduct(Data_Either.Left.create($27));
                    })(Data_Traversable.traverse(dictTraversable)(dictApplicative)(f)($26));
                })(function ($28) {
                    return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(function ($29) {
                        return Coproduct(Data_Either.Right.create($29));
                    })(Data_Traversable.traverse(dictTraversable1)(dictApplicative)(f)($28));
                });
            };
        });
    };
};
var bihoistCoproduct = function (natF) {
    return function (natG) {
        return function (v) {
            return Data_Bifunctor.bimap(Data_Either.bifunctorEither)(natF)(natG)(v);
        };
    };
};
module.exports = {
    Coproduct: Coproduct, 
    bihoistCoproduct: bihoistCoproduct, 
    coproduct: coproduct, 
    left: left, 
    right: right, 
    unCoproduct: unCoproduct, 
    functorCoproduct: functorCoproduct, 
    foldableCoproduct: foldableCoproduct, 
    traversableCoproduct: traversableCoproduct
};
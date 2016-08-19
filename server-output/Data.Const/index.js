// Generated by psc version 0.9.3
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Field = require("../Data.Field");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Contravariant = require("../Data.Functor.Contravariant");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Const = function (x) {
    return x;
};
var showConst = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Const " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringConst = function (dictSemiring) {
    return new Data_Semiring.Semiring(function (v) {
        return function (v1) {
            return Data_Semiring.add(dictSemiring)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_Semiring.mul(dictSemiring)(v)(v1);
        };
    }, Data_Semiring.one(dictSemiring), Data_Semiring.zero(dictSemiring));
};
var semigroupoidConst = new Control_Semigroupoid.Semigroupoid(function (v) {
    return function (v1) {
        return v1;
    };
});
var semigroupConst = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semigroup.append(dictSemigroup)(v)(v1);
        };
    });
};
var ringConst = function (dictRing) {
    return new Data_Ring.Ring(function () {
        return semiringConst(dictRing["__superclass_Data.Semiring.Semiring_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ring.sub(dictRing)(v)(v1);
        };
    });
};
var monoidConst = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupConst(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]());
    }, Data_Monoid.mempty(dictMonoid));
};
var heytingAlgebraConst = function (dictHeytingAlgebra) {
    return new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra), function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.implies(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return Data_HeytingAlgebra.not(dictHeytingAlgebra)(v);
    }, Data_HeytingAlgebra.tt(dictHeytingAlgebra));
};
var getConst = function (v) {
    return v;
};
var functorConst = new Data_Functor.Functor(function (v) {
    return function (v1) {
        return v1;
    };
});
var invariantConst = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorConst));
var foldableConst = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (v) {
        return function (v1) {
            return Data_Monoid.mempty(dictMonoid);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            return z;
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            return z;
        };
    };
});
var traversableConst = new Data_Traversable.Traversable(function () {
    return foldableConst;
}, function () {
    return functorConst;
}, function (dictApplicative) {
    return function (v) {
        return Control_Applicative.pure(dictApplicative)(v);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            return Control_Applicative.pure(dictApplicative)(v1);
        };
    };
});
var eqConst = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(dictEq)(v)(v1);
        };
    });
};
var ordConst = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqConst(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
var contravariantConst = new Data_Functor_Contravariant.Contravariant(function (v) {
    return function (v1) {
        return v1;
    };
});
var commutativeRingConst = function (dictCommutativeRing) {
    return new Data_CommutativeRing.CommutativeRing(function () {
        return ringConst(dictCommutativeRing["__superclass_Data.Ring.Ring_0"]());
    });
};
var euclideanRingConst = function (dictEuclideanRing) {
    return new Data_EuclideanRing.EuclideanRing(function () {
        return commutativeRingConst(dictEuclideanRing["__superclass_Data.CommutativeRing.CommutativeRing_0"]());
    }, function (v) {
        return Data_EuclideanRing.degree(dictEuclideanRing)(v);
    }, function (v) {
        return function (v1) {
            return Data_EuclideanRing.div(dictEuclideanRing)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_EuclideanRing.mod(dictEuclideanRing)(v)(v1);
        };
    });
};
var fieldConst = function (dictField) {
    return new Data_Field.Field(function () {
        return euclideanRingConst(dictField["__superclass_Data.EuclideanRing.EuclideanRing_0"]());
    });
};
var boundedConst = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordConst(dictBounded["__superclass_Data.Ord.Ord_0"]());
    }, Data_Bounded.bottom(dictBounded), Data_Bounded.top(dictBounded));
};
var booleanAlgebraConst = function (dictBooleanAlgebra) {
    return new Data_BooleanAlgebra.BooleanAlgebra(function () {
        return heytingAlgebraConst(dictBooleanAlgebra["__superclass_Data.HeytingAlgebra.HeytingAlgebra_0"]());
    });
};
var applyConst = function (dictSemigroup) {
    return new Control_Apply.Apply(function () {
        return functorConst;
    }, function (v) {
        return function (v1) {
            return Data_Semigroup.append(dictSemigroup)(v)(v1);
        };
    });
};
var bindConst = function (dictSemigroup) {
    return new Control_Bind.Bind(function () {
        return applyConst(dictSemigroup);
    }, function (v) {
        return function (v1) {
            return v;
        };
    });
};
var applicativeConst = function (dictMonoid) {
    return new Control_Applicative.Applicative(function () {
        return applyConst(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]());
    }, function (v) {
        return Data_Monoid.mempty(dictMonoid);
    });
};
module.exports = {
    Const: Const, 
    getConst: getConst, 
    eqConst: eqConst, 
    ordConst: ordConst, 
    boundedConst: boundedConst, 
    showConst: showConst, 
    semigroupoidConst: semigroupoidConst, 
    semigroupConst: semigroupConst, 
    monoidConst: monoidConst, 
    semiringConst: semiringConst, 
    ringConst: ringConst, 
    euclideanRingConst: euclideanRingConst, 
    commutativeRingConst: commutativeRingConst, 
    fieldConst: fieldConst, 
    heytingAlgebraConst: heytingAlgebraConst, 
    booleanAlgebraConst: booleanAlgebraConst, 
    functorConst: functorConst, 
    invariantConst: invariantConst, 
    contravariantConst: contravariantConst, 
    applyConst: applyConst, 
    bindConst: bindConst, 
    applicativeConst: applicativeConst, 
    foldableConst: foldableConst, 
    traversableConst: traversableConst
};
// Generated by psc version 0.9.3
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Lazy = require("../Control.Lazy");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Either = require("../Data.Either");
var Data_Tuple = require("../Data.Tuple");
var Data_Functor = require("../Data.Functor");
var Control_Apply = require("../Control.Apply");
var Control_Monad = require("../Control.Monad");
var Control_Applicative = require("../Control.Applicative");
var Data_Function = require("../Data.Function");
var Control_Bind = require("../Control.Bind");
var Data_Unit = require("../Data.Unit");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var StateT = function (x) {
    return x;
};
var withStateT = function (f) {
    return function (v) {
        return function ($92) {
            return v(f($92));
        };
    };
};
var runStateT = function (v) {
    return v;
};
var monadTransStateT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function (m) {
        return function (s) {
            return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(m)(function (v) {
                return Data_Function.apply(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(v, s));
            });
        };
    };
});
var mapStateT = function (f) {
    return function (v) {
        return function ($93) {
            return f(v($93));
        };
    };
};
var lazyStateT = new Control_Lazy.Lazy(function (f) {
    return function (s) {
        var $47 = f(Data_Unit.unit);
        return $47(s);
    };
});
var functorStateT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return function (s) {
                return Data_Functor.map(dictFunctor)(function (v1) {
                    return new Data_Tuple.Tuple(f(v1.value0), v1.value1);
                })(v(s));
            };
        };
    });
};
var execStateT = function (dictFunctor) {
    return function (v) {
        return function (s) {
            return Data_Functor.map(dictFunctor)(Data_Tuple.snd)(v(s));
        };
    };
};
var evalStateT = function (dictFunctor) {
    return function (v) {
        return function (s) {
            return Data_Functor.map(dictFunctor)(Data_Tuple.fst)(v(s));
        };
    };
};
var monadStateT = function (dictMonad) {
    return new Control_Monad.Monad(function () {
        return applicativeStateT(dictMonad);
    }, function () {
        return bindStateT(dictMonad);
    });
};
var bindStateT = function (dictMonad) {
    return new Control_Bind.Bind(function () {
        return applyStateT(dictMonad);
    }, function (v) {
        return function (f) {
            return function (s) {
                return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(v(s))(function (v1) {
                    var $60 = f(v1.value0);
                    return $60(v1.value1);
                });
            };
        };
    });
};
var applyStateT = function (dictMonad) {
    return new Control_Apply.Apply(function () {
        return functorStateT(((dictMonad["__superclass_Control.Bind.Bind_1"]())["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]());
    }, Control_Monad.ap(monadStateT(dictMonad)));
};
var applicativeStateT = function (dictMonad) {
    return new Control_Applicative.Applicative(function () {
        return applyStateT(dictMonad);
    }, function (a) {
        return function (s) {
            return Data_Function.apply(Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(a, s));
        };
    });
};
var monadContStateT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadStateT(dictMonadCont["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return function (s) {
            return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                var $63 = f(function (a) {
                    return function (s$prime) {
                        return c(new Data_Tuple.Tuple(a, s$prime));
                    };
                });
                return $63(s);
            });
        };
    });
};
var monadEffState = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadStateT(dictMonadEff["__superclass_Control.Monad.Monad_0"]());
    }, function ($94) {
        return Control_Monad_Trans.lift(monadTransStateT)(dictMonadEff["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($94));
    });
};
var monadErrorStateT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadStateT(dictMonadError["__superclass_Control.Monad.Monad_0"]());
    }, function (v) {
        return function (h) {
            return function (s) {
                return Control_Monad_Error_Class.catchError(dictMonadError)(v(s))(function (e) {
                    var $66 = h(e);
                    return $66(s);
                });
            };
        };
    }, function (e) {
        return Control_Monad_Trans.lift(monadTransStateT)(dictMonadError["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
    });
};
var monadReaderStateT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadStateT(dictMonadReader["__superclass_Control.Monad.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransStateT)(dictMonadReader["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return mapStateT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadRecStateT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadStateT(dictMonadRec["__superclass_Control.Monad.Monad_0"]());
    }, function (f) {
        return function (a) {
            var f$prime = function (v) {
                return Control_Bind.bind((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())((function () {
                    var $68 = f(v.value0);
                    return $68;
                })()(v.value1))(function (v1) {
                    return Control_Applicative.pure((dictMonadRec["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]())((function () {
                        if (v1.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(new Data_Tuple.Tuple(v1.value0.value0, v1.value1));
                        };
                        if (v1.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(new Data_Tuple.Tuple(v1.value0.value0, v1.value1));
                        };
                        throw new Error("Failed pattern match at Control.Monad.State.Trans line 86, column 11 - line 88, column 42: " + [ v1.value0.constructor.name ]);
                    })());
                });
            };
            return function (s) {
                return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(f$prime)(new Data_Tuple.Tuple(a, s));
            };
        };
    });
};
var monadStateStateT = function (dictMonad) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadStateT(dictMonad);
    }, function (f) {
        return Data_Function.apply(StateT)(function ($95) {
            return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(f($95));
        });
    });
};
var monadWriterStateT = function (dictMonadWriter) {
    return new Control_Monad_Writer_Class.MonadWriter(function () {
        return monadStateT(dictMonadWriter["__superclass_Control.Monad.Monad_0"]());
    }, function (m) {
        return function (s) {
            return Control_Bind.bind((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m(s)))(function (v) {
                return Data_Function.apply(Control_Applicative.pure((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
            });
        };
    }, function (m) {
        return function (s) {
            return Control_Monad_Writer_Class.pass(dictMonadWriter)(Control_Bind.bind((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Bind.Bind_1"]())(m(s))(function (v) {
                return Data_Function.apply(Control_Applicative.pure((dictMonadWriter["__superclass_Control.Monad.Monad_0"]())["__superclass_Control.Applicative.Applicative_0"]()))(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
            }));
        };
    }, function (wd) {
        return Control_Monad_Trans.lift(monadTransStateT)(dictMonadWriter["__superclass_Control.Monad.Monad_0"]())(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
    });
};
var altStateT = function (dictMonad) {
    return function (dictAlt) {
        return new Control_Alt.Alt(function () {
            return functorStateT(dictAlt["__superclass_Data.Functor.Functor_0"]());
        }, function (v) {
            return function (v1) {
                return function (s) {
                    return Control_Alt.alt(dictAlt)(v(s))(v1(s));
                };
            };
        });
    };
};
var plusStateT = function (dictMonad) {
    return function (dictPlus) {
        return new Control_Plus.Plus(function () {
            return altStateT(dictMonad)(dictPlus["__superclass_Control.Alt.Alt_0"]());
        }, function (v) {
            return Control_Plus.empty(dictPlus);
        });
    };
};
var alternativeStateT = function (dictMonad) {
    return function (dictAlternative) {
        return new Control_Alternative.Alternative(function () {
            return applicativeStateT(dictMonad);
        }, function () {
            return plusStateT(dictMonad)(dictAlternative["__superclass_Control.Plus.Plus_1"]());
        });
    };
};
var monadZeroStateT = function (dictMonadZero) {
    return new Control_MonadZero.MonadZero(function () {
        return alternativeStateT(dictMonadZero["__superclass_Control.Monad.Monad_0"]())(dictMonadZero["__superclass_Control.Alternative.Alternative_1"]());
    }, function () {
        return monadStateT(dictMonadZero["__superclass_Control.Monad.Monad_0"]());
    });
};
var monadPlusStateT = function (dictMonadPlus) {
    return new Control_MonadPlus.MonadPlus(function () {
        return monadZeroStateT(dictMonadPlus["__superclass_Control.MonadZero.MonadZero_0"]());
    });
};
module.exports = {
    StateT: StateT, 
    evalStateT: evalStateT, 
    execStateT: execStateT, 
    mapStateT: mapStateT, 
    runStateT: runStateT, 
    withStateT: withStateT, 
    functorStateT: functorStateT, 
    applyStateT: applyStateT, 
    applicativeStateT: applicativeStateT, 
    altStateT: altStateT, 
    plusStateT: plusStateT, 
    alternativeStateT: alternativeStateT, 
    bindStateT: bindStateT, 
    monadStateT: monadStateT, 
    monadRecStateT: monadRecStateT, 
    monadZeroStateT: monadZeroStateT, 
    monadPlusStateT: monadPlusStateT, 
    monadTransStateT: monadTransStateT, 
    lazyStateT: lazyStateT, 
    monadEffState: monadEffState, 
    monadContStateT: monadContStateT, 
    monadErrorStateT: monadErrorStateT, 
    monadReaderStateT: monadReaderStateT, 
    monadStateStateT: monadStateStateT, 
    monadWriterStateT: monadWriterStateT
};

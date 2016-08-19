// Generated by psc version 0.9.3
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_Unsafe = require("../Control.Monad.Aff.Unsafe");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Data_Foldable = require("../Data.Foldable");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_List = require("../Data.List");
var Data_Maybe = require("../Data.Maybe");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude_1 = require("../Prelude");
var Prelude_1 = require("../Prelude");
var Pux_Html = require("../Pux.Html");
var React = require("../React");
var Signal = require("../Signal");
var Signal_Channel = require("../Signal.Channel");
var Control_Bind = require("../Control.Bind");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Applicative = require("../Control.Applicative");
var onlyEffects = function (state) {
    return function (effects) {
        return {
            state: state, 
            effects: effects
        };
    };
};
var noEffects = function (state) {
    return {
        state: state, 
        effects: [  ]
    };
};
var start = function (config) {
    return function __do() {
        var v = Signal_Channel.channel(Data_List.Nil.value)();
        var mapAffect = function (affect) {
            return Data_Function.apply(Control_Monad_Aff.launchAff)(Control_Monad_Aff_Unsafe.unsafeInterleaveAff(Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.later(affect))(function (v1) {
                return Data_Function.apply(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff))(Signal_Channel.send(v)(Data_List.singleton(v1)));
            })));
        };
        var foldState = function (effModel) {
            return function (action) {
                return config.update(action)(effModel.state);
            };
        };
        var foldActions = function (actions) {
            return function (effModel) {
                return Data_Foldable.foldl(Data_List.foldableList)(foldState)(noEffects(effModel.state))(actions);
            };
        };
        var actionSignal = Signal_Channel.subscribe(v);
        var input = Data_Function.apply(Partial_Unsafe.unsafePartial)(function (dictPartial) {
            return Data_Function.apply(Data_Maybe.fromJust(dictPartial))(Data_Function.apply(Signal.mergeMany(Data_List.functorList)(Data_List.foldableList))(Data_List.reverse(new Data_List.Cons(actionSignal, Data_Functor.map(Data_List.functorList)(Data_Functor.map(Signal.functorSignal)(Data_List.singleton))(Data_Function.apply(Data_List.fromFoldable(Data_Foldable.foldableArray))(config.inputs))))));
        });
        var effModelSignal = Signal.foldp(foldActions)(noEffects(config.initialState))(input);
        var effectsSignal = Signal.flippedMap(Signal.functorSignal)(effModelSignal)(function ($7) {
            return Data_Functor.map(Data_Functor.functorArray)(mapAffect)((function (v1) {
                return v1.effects;
            })($7));
        });
        var stateSignal = Signal.flippedMap(Signal.functorSignal)(effModelSignal)(function (v1) {
            return v1.state;
        });
        var htmlSignal = Signal.flippedMap(Signal.functorSignal)(stateSignal)(function (state) {
            return $foreign.render(function ($8) {
                return Signal_Channel.send(v)(Data_List.singleton($8));
            }, function (a) {
                return a;
            }, config.view(state));
        });
        Data_Function.apply(Signal.runSignal)(Signal.flippedMap(Signal.functorSignal)(effectsSignal)(Data_Foldable.sequence_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)))();
        return Data_Function.apply(Control_Applicative.pure(Control_Monad_Eff.applicativeEff))({
            html: htmlSignal, 
            state: stateSignal
        })();
    };
};
var mapState = function (a2b) {
    return function (effmodel) {
        return {
            state: a2b(effmodel.state), 
            effects: effmodel.effects
        };
    };
};
var mapEffects = function (action) {
    return function (effmodel) {
        return {
            state: effmodel.state, 
            effects: Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Control_Monad_Aff.functorAff)(action))(effmodel.effects)
        };
    };
};
var fromSimple = function (update) {
    return function (action) {
        return function (state) {
            return Data_Function.apply(noEffects)(update(action)(state));
        };
    };
};
module.exports = {
    fromSimple: fromSimple, 
    mapEffects: mapEffects, 
    mapState: mapState, 
    noEffects: noEffects, 
    onlyEffects: onlyEffects, 
    start: start, 
    renderToDOM: $foreign.renderToDOM, 
    renderToString: $foreign.renderToString, 
    toReact: $foreign.toReact
};
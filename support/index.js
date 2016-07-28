var Main = require('../client/Main.purs');
var initialState = require('../src/App.purs').init;
var debug = process.env.NODE_ENV === 'development'

Main[debug ? 'debug' : 'main'](initialState)();

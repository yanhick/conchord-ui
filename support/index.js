var Main = require('../client/Main.purs');
var initialState = require('../src/App.purs').init;

Main.main(window.puxLastState || initialState)();

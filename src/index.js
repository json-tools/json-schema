'use strict';

require('./index.html');
var Elm = require('./Main');

var storedState = localStorage.getItem('client-app-persisted-data');
var apiConfig = storedState ? JSON.parse(storedState) : null;
var clientApp = Elm.Main.fullscreen(apiConfig);

clientApp.ports.storeConfig.subscribe(function(state) {
    localStorage.setItem('client-app-persisted-data', JSON.stringify(state));
});

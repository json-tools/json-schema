'use strict';

require('./index.html');
var Elm = require('./Main');

var storedState = localStorage.getItem('client-app-config');
var apiConfig = storedState ? JSON.parse(storedState) : null;
var clientApp = Elm.Main.fullscreen(apiConfig);

clientApp.ports.storeConfig.subscribe(function(state) {
    localStorage.setItem('client-app-config', JSON.stringify(state));
});

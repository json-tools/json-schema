'use strict';

require('./index.html');
const Elm = require('./Main');

const storedState = localStorage.getItem('client-app-persisted-data-v3');
const appState = storedState ? JSON.parse(storedState) : {
    clientSettings: null
};

if (appState.clientSettings) {
    if (typeof appState.clientSettings.guide === 'undefined') {
        appState.clientSettings.guide = true;
    }
}

function init(state) {
    const clientApp = Elm.Main.fullscreen(appState);

    clientApp.ports.storeConfig.subscribe(function(state) {
        localStorage.setItem('client-app-persisted-data-v3', JSON.stringify(state));
    });
}

init(appState);

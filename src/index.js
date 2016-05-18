'use strict';

require('./index.html');
var Elm = require('./Main');

var elm = Elm.Main.fullscreen();

//interop
elm.ports.alert.subscribe(function(message) {
  alert(message);
  elm.ports.log.send('Alert called: ' + message);
});
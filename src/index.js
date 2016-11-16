'use strict';

require('./index.html');
const Elm = require('./Main');

const storedState = localStorage.getItem('client-app-persisted-data-v3');
const appState = storedState ? JSON.parse(storedState) : {
    clientSettings: null
};

const conf =
    { endpoints:
        [ [ 'vault/create-otp',
            { method: 'POST'
            , pathname: '/otp'
            , service: 'vault'
            , auth: true
            , request: null
            , response:
                { type: 'object'
                , properties: { id: { type: 'string', format: 'uuid' }}
                }
            , effects:
                [ [ 'pipe', { from: 'id', to: [ 'vault/create-pan', 'otp' ] } ]
                ]
            }]
        , [ 'vault/create-pan',
            { method: 'POST'
            , pathname: '/pan'
            , service: 'vault'
            , auth: false
            , request:
                { type: 'object'
                , properties:
                    { otp: { type: 'string', format: 'uuid' }
                    , pan: { type: 'string' }
                    }
                , required: [ 'pan', 'otp' ]
                }
            , response:
                { type: 'object'
                , properties:
                    { id: { type: 'string', format: 'uuid' }
                    , key: { type: 'string' }
                    }
                }
            , effects:
                [ [ 'pipe', { from: 'id', to: [ 'vault/create-fake-pan', 'panId' ] } ]
                , [ 'pipe', { from: 'key', to: [ 'service/create-job', 'payment/card/decryptionKey' ] } ]
                ]
            }]
        , [ 'vault/create-fake-pan',
            { method: 'POST'
            , pathname: '/pan/fake'
            , service: 'vault'
            , auth: true
            , request:
                { type: 'object'
                , properties: { panId: { type: 'string', format: 'uuid' }}
                }
            , response:
                { type: 'object'
                , properties: { pan: { type: 'string' }}
                }
            , effects:
                [ [ 'pipe', { from: 'pan', to: [ 'service/create-job', 'payment/card/token' ] } ]
                ]
            }]
        , [ 'service/list-services',
            { method: 'GET'
            , pathname: '/services'
            , service: 'service'
            , auth: true
            , request: null
            , response:
                { type: 'object'
                , properties:
                    { data:
                        { type: 'array'
                        , items:
                            { type: 'object'
                            , properties:
                                { id: { type: 'string', format: 'uuid' }
                                , schema: { type: 'object' }
                                }
                            }
                        }
                    }
                }
            }]
        , [ 'service/create-job',
            { method: 'POST'
            , pathname: '/jobs'
            , service: 'service'
            , auth: true
            , request:
                { properties:
                    { id: { type: 'string', format: 'uuid' }
                    , input: { type: 'object' }
                    }
                }
            , response:
                { type: 'object'
                }
            }]
        ]
    };

if (appState.clientSettings) {
    if (typeof appState.clientSettings.guide === 'undefined') {
        appState.clientSettings.guide = true;
    }
}

function init(state) {
    const clientApp = Elm.Main.fullscreen([appState, conf]);

    clientApp.ports.storeConfig.subscribe(function(state) {
        localStorage.setItem('client-app-persisted-data-v3', JSON.stringify(state));
    });
}

init(appState);

'use strict';

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
                , id: '#create-otp-response'
                , properties: { id: { type: 'string', format: 'uuid' }}
                }
            }]
        , [ 'vault/create-pan',
            { method: 'POST'
            , pathname: '/pan'
            , service: 'vault'
            , auth: false
            , request:
                { type: 'object'
                , id: '#create-pan-request'
                , properties:
                    { otp: { type: 'string', format: 'uuid' }
                    , pan: { type: 'string' }
                    }
                , required: [ 'pan', 'otp' ]
                }
            , response:
                { type: 'object'
                , id: '#create-pan-response'
                , properties:
                    { id: { type: 'string', format: 'uuid' }
                    , key: { type: 'string' }
                    }
                }
            }]
        , [ 'vault/create-fake-pan',
            { method: 'POST'
            , pathname: '/pan/fake'
            , service: 'vault'
            , auth: true
            , request:
                { type: 'object'
                , id: '#create-fake-pan-request'
                , properties: { panId: { type: 'string', format: 'uuid' }}
                }
            , response:
                { type: 'object'
                , id: '#create-fake-pan-response'
                , properties: { pan: { type: 'string' }}
                }
            }]
        , [ 'service/list-services',
            { method: 'GET'
            , pathname: '/services'
            , service: 'service'
            , auth: true
            , request: null
            , response:
                { type: 'object'
                , id: '#list-services-response'
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
                { type: 'object'
                , id: '#create-job-request'
                , properties:
                    { serviceId: { type: 'string', format: 'uuid' }
                    , input: { type: 'object' }
                    }
                }
            , response:
                { type: 'object'
                , id: '#show-job-response'
                }
            }]
        , [ 'service/show-job',
            { method: 'GET'
            , pathname: '/jobs/:id'
            , service: 'service'
            , auth: true
            , request:
                { type: 'object'
                , id: '#show-job-request'
                , properties:
                    { id: { type: 'string', format: 'uuid' }
                    }
                }
            , response:
                { type: 'object'
                , id: '#show-job-response'
                }
            }]
        ]
    , dependencies:
        [ [ '#create-otp-response/id', '#create-pan-request/otp' ]
        , [ '#create-pan-response/id', '#create-fake-pan-request/panId' ]
        , [ '#create-pan-response/key', '#payment-card/decryptionKey' ]
        , [ '#create-fake-pan-response/pan', '#payment-card/cardToken' ]
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

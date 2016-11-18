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
                , properties:
                    { id:
                        { type: 'string'
                        , format: 'uuid'
                        , description: 'One-time password which should be used to save PAN to secure vault. Format of OTP is UUID v4. This OTP can be used on frontend to allow user to save its PAN.'
                        }
                    }
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
                    { otp:
                        { type: 'string'
                        , format: 'uuid'
                        }
                    , pan:
                        { type: 'string'
                        }
                    }
                , required: [ 'pan', 'otp' ]
                }
            , response:
                { type: 'object'
                , id: '#create-pan-response'
                , properties:
                    { id:
                        { type: 'string'
                        , format: 'uuid'
                        , description: "ID of PAN. Save it in your database. This ID is the only representation of user's credit card number. Later it will be used to issue temporary card token. It can not be used to retrieve original card number."
                        }
                    , key:
                        { type: 'string'
                        , description: 'Decryption key of PAN. This pan should be given to automation cloud along with temporary card token and will be during job processing.'
                        }
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
                        , description: 'List of available automation services. Each item of type `Service`.'
                        , items:
                            { type: 'object'
                            , properties:
                                { id:
                                    { type: 'string'
                                    , format: 'uuid'
                                    , description: 'ID of service which will be used to create Job. This id never changes, so feel free to keep it in your database associated with particular automation preferences.'
                                    }
                                , schema:
                                    { type: 'object'
                                    , description: 'Schema formatted according to Json-Schema.org specifications. It describes input object required to create job.'
                                    }
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
                , properties:
                    { object:
                        { type: 'string'
                        , description: 'Object type. Value is "job".'
                        }
                    , id:
                        { type: 'string'
                        , format: 'uuid'
                        , description: 'Unique ID of Job which could be used to request Job information'
                        }
                    , state:
                        { type: 'string'
                        , description: 'Current job state, enum of `created`, `started`, `finished`, `failed`'
                        }
                    , input:
                        { type: 'object'
                        , description: 'Input data provided for job (according to schema of particular service)'
                        }
                    , output:
                        { type: 'array'
                        , description: 'Output produced by job, TBD'
                        }
                    , errors:
                        { type: 'array'
                        , description: 'Array with async errors produced by job'
                        }
                    , createdAt:
                        { type: 'number'
                        , description: 'Date of job creation as UNIX timestamp in milliseconds'
                        }
                    , updatedAt:
                        { type: 'number'
                        , description: 'Date of last job update as UNIX timestamp in milliseconds'
                        }
                    }
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

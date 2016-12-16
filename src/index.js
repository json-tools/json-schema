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
            , description: 'To save a payment card securely in our vault we issue one-time password (OTP). The resulting OTP can be used only once to save a single PAN.'
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
                        , description: 'One-time password used to authorise call'
                        }
                    , pan:
                        { type: 'string'
                        , minLength: 12
                        , maxLength: 19
                        , description: 'PAN (payment card number, usually 16 digits). This PAN will be encrypted and stored in secure vault permanently. It is not possible to decrypt PAN without key which is not stored in database.'
                        }
                    }
                , required: [ 'pan', 'otp' ]
                }
            , response:
                { type: 'object'
                , id: '#create-pan-response'
                , properties:
                    { panId:
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
            , description: 'Next you store your user’s PAN in the vault. This endpoint is the only one not authenticated with your client secret key, it requires OTP in order to authorise the request.\n\nThe result of this call must be stored in your database as the permanent id of the user\'s PAN. It can not be used to retrieve or decrypt the card, it can only be used to issue a replacement token.'
        }]
        , [ 'vault/create-fake-pan',
            { method: 'POST'
            , pathname: '/pan/temporary'
            , service: 'vault'
            , auth: true
            , request:
                { type: 'object'
                , id: '#create-fake-pan-request'
                , properties:
                    { panId:
                        { type: 'string'
                        , format: 'uuid'
                        , description: 'ID of PAN provided after pan creation'
                        }
                    , key:
                        { type: 'string'
                        , description: 'Decryption key of PAN provided after pan creation'
                        }
                    }
                }
            , response:
                { type: 'object'
                , id: '#create-fake-pan-response'
                , properties:
                    { id:
                        { type: 'string'
                        , description: 'Temporary card identifier used when you create an automation job'
                        }
                    , key:
                        { type: 'string'
                        , description: 'Decryption key which should accompany the PAN Token'
                        }
                    }
                }
            , description: 'This endpoint creates a token which will then be used to start a job which requires a PAN. The token expires after some time (currently 1 hour). A new token must be issued for each new job. The same token can\'t be used twice.'
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
            , description: 'The Automation cloud offers a number of automation services. Each of these services requires particular input data in JSON format. This endpoint provides list of the available services with schemas describing the format of the input data.'
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
                    { serviceId:
                        { type: 'string'
                        , format: 'uuid'
                        , description: 'ID of service. This service will be used to perform automation job.'
                        }
                    , input:
                        { type: 'object'
                        , description: 'Input for job. It should conform to service.schema requirements. Schema can be unique for each service. Attempt to supply an invalid data to `input` param will result in validation error. Even if API is accepting data validation error might happen asyncronously later. Poll status of created job to get updates about job state.'
                        }
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
            , description: 'This is the starting point of the automation process, and creates your automation job. This is a function call with an object as an argument, and it returns the object which will represent your job (including the output, errors and yields).'
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
            , description: ''
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

window.wdsOk = function() {
    const err = document.querySelector('body > #err');

    if (err) {
        document.body.removeChild(err);
    }
};

window.wdsError = function(errors) {
    const err = document.createElement('pre');
    err.id = 'err';
    err.innerHTML = errors.join('\n\n');
    err.style.background = 'white';
    err.style.color = 'black';
    err.style.padding = '10px';
    err.style.position = 'fixed';
    err.style.top = 0;
    err.style.bottom = 0;
    err.style.left = 0;
    err.style.right = 0;
    err.style.overflow = 'auto';
    document.body.appendChild(err);
}

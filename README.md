# JSON Schema decoder and validator for elm

[![Build Status](https://travis-ci.org/1602/json-schema.svg?branch=master)](https://travis-ci.org/1602/json-schema)

> JSON Schema is a vocabulary that allows you to annotate and validate JSON documents. (http://json-schema.org/)

This code is experimental, it doesn't cover json schema spec in full (yet), just allows to parse minimal subset of it in order to implement very basic proof of concept of "type as value" in elm.

The end goal of this project is to cover json schema draft 6 spec in full, if you're interested - feel free to pick up some of the open issues and submit PR.

## When to use this library?

### ✍ form generation

Sometimes it is not possible by design to come up with some static type definition, for example if you are building REST API test console, where each endpoint requires its own data. The simplest solution would be to allow user to enter data as json string, decode it into value to ensure that json is valid and send to a server as value, without looking inside this value to make sure it makes sense. But what if we want to enter data using form, perform some basic validation before sending it to a server? Then we have a valid use case for this library.

### ☝ documentation generation

JSON Schema allows you to specify some meta data like title, description, examples, definitions and also some validation keywords like type, format, enum, and all sub-schemas (e.g. properties, items) which is a useful source of information for content generation if you want to document data structures.

### ✌ validation

Instead of writing validation code as part of your frontend app you could describe it in a declarative way as JSON schema, so that you can focus on what your are validating rather than how. Combined with form generation this is a great time-saver while building interfaces.

## Current status of this project

- [x] decode draft 6 of json-schema
- [x] validate all the things
- [x] schema builder api
- [x] documentation
- [x] random value generator
- [x] demo: json editor
- [x] multiple errors
- [ ] i18n
- [ ] demo: docs generator
- [ ] demo: schema builder

# JSON-Schema for elm

[![Build Status](https://travis-ci.org/1602/elm-json-schema.svg?branch=master)](https://travis-ci.org/1602/elm-json-schema)

This code is experimental, it doesn't cover json schema spec (yet), just allows to parse minimal subset of it in order to implement very basic proof of concept of "type as value" in elm.

The end goal of this project is to cover json schema draft 6 spec in full, if you're interested - feel free to pick up some of the open issues and submit PR.

## Current status

- [x] decode draft 6 of json-schema (without Boolean schema, yet)
- [x] validate all the things
- [ ] schema builder api
- [ ] documentation
- [x] forms builder api (legacy, will be updated soon-ish)

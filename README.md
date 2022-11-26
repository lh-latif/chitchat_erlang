# ChitChat Erlang
---
An Chat Server and Webserver, written from scratch and inspired by Whatsapp and Telegram. Using symmetric encryption, which is Diffie-Hellman Key Exchange. So technically users message cannot be read or viewed by third party.
Build with Erlang programming language, whose has been built for fault-tolerance, High scalable, and Soft-realtime System.

## Status
This project been build for catch up with Go version of ChitChat Server. Which is technically still in early progress. But same with Go version, it can send message between two party (only peer to peer chat supported right now). It also can register user, signin, and manage contact.

## Vision
I want to build a chat server that vertically scalable and modular. So i need to start small before going big (or maybe microservice). I plan the server can be composed by many Stack and Tech, each of server will be used as client-websocket pool and session registry. For now i'm thinking about Documenting what i has been build from scratch, it can be API, Websocket Chat Protocol, App Architecture, Session Registering, or OTP Process Flow.
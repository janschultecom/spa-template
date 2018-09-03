[![License](http://img.shields.io/:license-Apache%202-blue.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)

# SPA template
This is a single-page-app template I use for my web project. It consists of a headless, decoupled frontend and a backend REST API. The frontend is written in purescript in the Elm architecture (TEA) using Spork. The backend is written in Haskell using Servant.

# Design goals & decisions
I wanted to have a minimalist SPA template that I can use as a starting point for developing SPAs. 

## Decoupled
I decided to completely separate the frontend from the backend (headless frontend). This is of course trade-off and hosting the frontend within the backend has some advantages, I think the advantages of separation outweigh the disadvantages.

* :x: no static checks possible that verify backend & frontend are consistent 
* :white_check_mark: separate development workflow, no need to spinup backend or have haskell installed
* :white_check_mark: easier to leverage the JavaScript eco-system (webpack, parcel, etc.)

## Lightweight
I want to have both, frontend and backend, as lightweight as possible from a development point of view. 

* Frontend uses spork, a purescript implementation of TEA, which https://github.com/i-am-the-slime recommended me. In comparison to other frameworks like react or halogen, spork respectively TEA caused me the least headache. 
* Backend uses servant as a lightweight http api on top of a http server. 

## Dockerised with hot-reload
It should be self-contained so one can just check it out and immediately run it using docker-compose. Furthermore, I wanted to have both frontend & backend having hot-reload. It makes development just so much more fun.

# Using

```
git clone git@github.com:janschultecom/spa-template.git

docker-compose up

open http://localhost:80
```

#!/bin/sh

pulp --watch build & parcel index.html --hmr-port 1235 & wait

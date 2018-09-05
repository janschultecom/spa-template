#!/bin/sh

# initial installation of packages and initial build
psc-package install && pulp build 

# hot reload  
pulp --watch build & parcel index.html --hmr-port 1235 & wait

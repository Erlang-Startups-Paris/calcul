#!/bin/bash

NODENAME=${1:-slave}

erl -pa ebin -sname $NODENAME -setcookie demo_app -eval "application:start(calcul_slave)"

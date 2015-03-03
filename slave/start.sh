#!/bin/bash

erl -pa ebin -sname $1 -setcookie demo_app -eval "application:start(calcul_slave)"

#!/bin/bash

erl -pa ebin -sname master@localhost -setcookie demo_app -eval "application:start(calcul_master)"

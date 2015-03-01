#!/bin/bash

erl -pa ebin -sname slave@localhost -setcookie demo_app -eval "application:start(calcul_slave)"

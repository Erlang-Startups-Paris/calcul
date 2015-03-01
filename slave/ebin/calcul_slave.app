{application, calcul_slave,
 [{vsn, "1.0.0"},
  {description, "Calcul slave"},
  {modules, [calcul_slave, calcul_slave_sup, calcul_slave_server, calcul_slave_lib]},
  {applications, [stdlib, kernel]},
  {registered, [calcul_slave_sup]},
  {mod, {calcul_slave, []}},
  {env, []}
 ]}.

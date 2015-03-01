{application, calcul_master,
 [{vsn, "1.0.0"},
  {description, "Calcul master"},
  {modules, [calcul_master, calcul_master_sup, calcul_master_server]},
  {applications, [stdlib, kernel]},
  {registered, [calcul_master_sup]},
  {mod, {calcul_master, []}},
  {env, []}
 ]}.

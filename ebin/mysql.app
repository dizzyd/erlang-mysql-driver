{application, mysql,
 [{description, "MySQL Library"},
  {vsn, "34"},
  {modules, [mysql,
             mysql_auth,
             mysql_conn,
             mysql_recv]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.


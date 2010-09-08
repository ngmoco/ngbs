{application, ngbs,
 [
  {description, "Ngmoco:) Bert-RPC Server"},
  {vsn, "1"},
  {registered, [ngbs_sup, ngbs_acl, ngbs_listener, ngbs_conn_sup]},
  {applications, [kernel,
                  stdlib
                 ]},
  {env, [{port, 8000}
         ,{allowed_calls, []}
         ,{listen_on_startup, false}
        ]},
  {modules, [ngbs_app
             ,ngbs_sup
             ,ngbs
             ,ngbs_bert
             ,ngbs_acl
             ,ngbs_listener
             ,ngbs_conn
             ,ngbs_conn_sup
             ,ngbs_client
             ]},
  {mod, {ngbs_app, []}}
 ]}.

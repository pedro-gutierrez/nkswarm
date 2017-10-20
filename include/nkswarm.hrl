-define(APP, nkswarm).
-define(SRV, nkswarm).
-define(NKSWARM_CLUSTER_SIZE, "erlang_nkswarm_cluster_size").
-define(LOG(Level, Txt, Args), lager:Level("NkSwarm: "++Txt, Args)).
-define(INFO(Txt, Args), ?LOG(info, Txt, Args)).
-define(DEBUG(Txt, Args), ?LOG(debug, Txt, Args)).
-define(ERROR(Txt, Args), ?LOG(error, Txt, Args)).
-define(WARN(Txt, Args), ?LOG(warn, Txt, Args)).

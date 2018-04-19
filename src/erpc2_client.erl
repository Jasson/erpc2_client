-module(erpc2_client).
-export([squery/3]).

%test  {now(),erpc2_client:squery(io,format, ["~p~n", [now()]])}.
squery(M, F, A) ->
    erpc2_client_server:squery(pool1,{M, F, A}).


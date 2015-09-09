%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%%
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(webhooks_conference).

-export([init/0
         ,bindings_and_responders/0
         ,handle_event/2
        ]).

-include("../webhooks.hrl").

-define(ID, wh_util:to_binary(?MODULE)).
-define(NAME, <<"conference">>).
-define(DESC, <<"Conference events">>).
-define(METADATA
        ,wh_json:from_list([{<<"_id">>, ?ID}
                            ,{<<"name">>, ?NAME}
                            ,{<<"description">>, ?DESC}
                           ])
       ).
-define(BINDINGS, [{'conference', ['federate']}]).
-define(RESPONDERS
        ,[{{'webhooks_conference', 'handle_event'}
           ,[{<<"conference">>, <<"*">>}
            ]
          }
         ]
       ).

-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

-spec bindings_and_responders() ->
                                     {gen_listener:bindings()
                                      ,gen_listener:responders()
                                     }.
bindings_and_responders() ->
    {?BINDINGS, ?RESPONDERS}.

-spec handle_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_event(JObj, _Props) ->
    io:format("MARKER:webhooks_conference.erl:49 ~p~n", [JObj]).
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    {?BINDINGS, ?RESPONDERS}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_event(JObj, _Props) ->
    % maybe_fire_conf_event(JObj).
    EventName = wh_api:event_name(JObj),
    format_event(EventName, JObj).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
% maybe_fire_conf_event(JObj) ->
%     wh_util:put_callid(JObj),
%     AccountId = find_account_id(JObj),
%     EventName = wh_api:event_name(JObj),
%     case webhooks_util:find_webhooks(?NAME, AccountId) of
%         [] ->
%             lager:debug(
%                 "no hooks to handle ~s for ~s"
%                 ,[EventName, AccountId]
%             );
%         Hooks ->
%             webhooks_util:fire_hooks(format_event(EventName, JObj), Hooks)
%     end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_event(binary(), wh_json:object()) -> 'ok'.
format_event(<<"search_req">>, _JObj) ->
    lager:warning("ignore search_req event");
format_event(<<"discovery_req">>, _JObj) ->
    lager:warning("ignore discovery_req event");
format_event(<<"participants_event">>, _JObj) ->
    lager:warning("ignore participants_event event");
format_event(EventName, _JObj) ->
    io:format("MARKER:webhooks_conference.erl:74 ~p~n", [EventName]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
% -spec find_account_id(wh_json:object()) -> ne_binary().
% find_account_id(_JObj) ->
%     <<"">>.
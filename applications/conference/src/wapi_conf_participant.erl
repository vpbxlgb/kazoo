%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_conf_participant).

-include("conference.hrl").

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([participant_create/1]).
-export([publish_participant_create/1, publish_participant_create/2]).

-define(PARTICIPANT_CREATE, <<"create">>).
-define(PARTICIPANT_CREATE_HEADERS, [<<"Conference-ID">>]).
-define(OPTIONAL_PARTICIPANT_CREATE_HEADERS, [<<"Account-ID">>, <<"Participant-ID">>]).
-define(PARTICIPANT_CREATE_VALUES, [{<<"Event-Category">>, <<"conference">>}
                              ,{<<"Event-Name">>, <<"participant_create">>}
                             ]).
-define(PARTICIPANT_CREATE_TYPES, []).


%%--------------------------------------------------------------------
%% @doc
%% Bind a queue to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props), Props).


bind_to_q(Q, 'undefined', _) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, <<"conference.participant.*.*">>);
bind_to_q(Q, ['participant_create'|T], Props) ->
    'ok' = amqp_util:bind_q_to_conference(Q,  <<"conference.participant.*.", ?PARTICIPANT_CREATE/binary>>),
    bind_to_q(Q, T, Props);
bind_to_q(_Q, [], _) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% Unbind a queue from the conference exhange
%% @end
%%--------------------------------------------------------------------
-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_from_q(Queue, props:get_value('restrict_to', Props), Props).

unbind_from_q(Q, 'undefined', _) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, <<"conference.participant.*">>);
unbind_from_q(Q, ['participant_create'|T], Props) ->
    'ok' = amqp_util:unbind_q_from_conference(Q, ?PARTICIPANT_CREATE),
    unbind_from_q(Q, T, Props);
unbind_from_q(_Q, [], _) -> 'ok'.


participant_create(Prop) when is_list(Prop) ->
    case participant_create_v(Prop) of
        true -> wh_api:build_message(Prop, ?PARTICIPANT_CREATE_HEADERS, ?OPTIONAL_PARTICIPANT_CREATE_HEADERS);
        false -> {error, "Proplist failed validation for dialplan req"}
    end;
participant_create(JObj) ->
    participant_create(wh_json:to_proplist(JObj)).

participant_create_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PARTICIPANT_CREATE_HEADERS, ?PARTICIPANT_CREATE_VALUES, ?PARTICIPANT_CREATE_TYPES);
participant_create_v(JObj) ->
    participant_create_v(wh_json:to_proplist(JObj)).




%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:conference_exchange().

%%--------------------------------------------------------------------
%% @doc
%% Publish to the participant
%% @end
%%--------------------------------------------------------------------
-spec publish_participant_create(api_terms()) -> 'ok'.
-spec publish_participant_create(api_terms(), ne_binary()) -> 'ok'.
publish_participant_create(JObj) ->
    publish_participant_create(JObj, ?DEFAULT_CONTENT_TYPE).

publish_participant_create(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?PARTICIPANT_CREATE_VALUES, fun ?MODULE:participant_create/1),
    amqp_util:conference_publish(Payload, 'participant', key(Req, ?PARTICIPANT_CREATE), [], ContentType).


key(JObj, Event) ->
    ConfId = wh_json:get_value(<<"Conference-ID">>, JObj),
    <<?KEY_CONFERENCE_PARTICIPANT/binary, ConfId/binary, ".", Event/binary>>.


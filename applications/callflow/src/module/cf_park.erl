%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_park).

-include("../callflow.hrl").

-export([handle/2]).
-export([update_presence/3]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".park">>).

-define(DB_DOC_NAME, whapps_config:get(?MOD_CONFIG_CAT, <<"db_doc_name">>, <<"parked_calls">>)).
-define(DEFAULT_RINGBACK_TM, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"default_ringback_time">>, 120000)).
-define(PARKED_PRESENCE_TYPE, whapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"parked_presence_type">>, <<"early">>)).
-define(PARKED_CALLS_KEY(Db), {?MODULE, 'parked_calls', Db}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec update_presence(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
update_presence(SlotNumber, _PresenceId, AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    ParkedCalls = get_parked_calls(AccountDb, AccountId),
    case find_slot_call_id(SlotNumber, ParkedCalls) of
        'undefined' -> 'ok';
        ParkedCallId ->
            update_parked_call_presence(SlotNumber, ParkedCalls, ParkedCallId)
    end.

-spec update_parked_call_presence(wh_json:key(), wh_json:object(), ne_binary()) -> 'ok'.
update_parked_call_presence(SlotNumber, ParkedCalls, ParkedCallId) ->
    Slot = get_slot(SlotNumber, ParkedCalls),
    case whapps_call_command:b_channel_status(ParkedCallId) of
        {'ok', _Status} -> update_presence(?PARKED_PRESENCE_TYPE, Slot);
        {'error', _} -> update_presence(<<"terminated">>, Slot)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    ReferredTo = whapps_call:custom_channel_var(<<"Referred-To">>, <<>>, Call),
    case re:run(ReferredTo, "Replaces=([^;]*)", [{'capture', [1], 'binary'}]) of
        'nomatch' when ReferredTo =:= <<>> ->
            handle_direct_dial(Data, Call);
        'nomatch' ->
            handle_blind_transfer(Call);
        {'match', [Replaces]} ->
            handle_attended_transfer(Replaces, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_direct_dial(wh_json:object(), whapps_call:call()) -> 'ok'.
handle_direct_dial(Data, Call) ->
    lager:debug("call was the result of a direct dial"),
    case wh_json:get_value(<<"action">>, Data, <<"park">>) of
        <<"park">> ->
            lager:info("action is to park the call"),
            direct_park_call(Call);
        <<"retrieve">> ->
            lager:info("action is to retrieve a parked call"),
            case maybe_retrieve_call(Call) of
                {'ok', _} -> 'ok';
                _Else ->
                    _ = whapps_call_command:b_answer(Call),
                    _ = whapps_call_command:b_prompt(<<"park-no_caller">>, Call),
                    cf_exe:continue(Call)
            end;
        <<"auto">> ->
            lager:info("action is to automatically determine if we should retrieve or park"),
            case maybe_retrieve_call(Call) of
                {'ok', _} -> cf_exe:continue(Call);
                {'error', _} -> direct_park_call(Call)
            end
    end.

-spec direct_park_call(whapps_call:call()) -> 'ok'.
direct_park_call(Call) ->
    SlotNumber = <<"100">>,
    case park_call(create_slot(SlotNumber, Call, cf_exe:callid(Call)), Call) of
        %% attended transfer but the provided slot number is occupied, we are still connected to the 'parker'
        %% not the 'parkee'
        {'error', 'occupied'} ->
%%            lager:info("selected slot is occupied"),
            %% Update screen with error that the slot is occupied
%%            _ = whapps_call_command:b_answer(Call),
            %% playback message that caller will have to try a different slot
%%            _ = whapps_call_command:b_prompt(<<"park-already_in_use">>, Call),
            cf_exe:continue(Call);
        %% attended transfer and allowed to update the provided slot number, we are still connected to the 'parker'
        %% not the 'parkee'
        {'ok', _} ->
%%            lager:info("playback slot number ~s to caller", [SlotNumber]),
            %% Update screen with new slot number
%%            _ = whapps_call_command:b_answer(Call),
            %% Caller parked in slot number...
%%            _ = whapps_call_command:b_prompt(<<"park-call_placed_in_spot">>, Call),
%%            _ = whapps_call_command:b_say(wh_util:to_binary(SlotNumber), Call),
            cf_exe:transfer(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_blind_transfer(whapps_call:call()) -> 'ok'.
handle_blind_transfer(Call) ->
    lager:info("call was the result of a blind transfer, assuming intention was to park"),
    SlotNumber = <<"100">>,
    Slot = create_slot(SlotNumber, Call),
    case park_call(Slot, Call) of
        {'ok', _} ->
            update_presence(?PARKED_PRESENCE_TYPE, Slot),
            wait_for_pickup(SlotNumber, Slot, Call);
        {'error', 'occupied'} ->
            lager:info("blind transfer to a occupied slot, call the parker back.."),
            case maybe_ringback_parker(SlotNumber, Slot, Call) of
                'answered' -> cf_exe:continue(Call);
                'channel_hungup' -> cf_exe:stop(Call);
                'failed' ->
                    whapps_call_command:hangup(Call),
                    cf_exe:stop(Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_attended_transfer(ne_binary(), whapps_call:call()) -> 'ok'.
handle_attended_transfer(Replaces, Call) ->
    lager:info("call was the result of an attended-transfer completion, updating call id"),
    {'ok', FoundInSlotNumber, Slot} = update_call_id(Replaces, Call),
    wait_for_pickup(FoundInSlotNumber, Slot, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to retrieve a parked call
%% @end
%%--------------------------------------------------------------------
-spec maybe_retrieve_call(whapps_call:call()) ->
                      {'ok', 'channel_hungup'} |
                      {'error', 'slot_empty' | 'timeout' | 'failed'}.
maybe_retrieve_call(Call) ->
    case get_slot(Call) of
        'undefined' ->
            lager:info("no parked call to attempt to retrieve", []),
            {'error', 'slot_empty'};
        Slot -> retrieve_call(Slot, Call)
    end.

-spec retrieve_call(wh_json:object(), whapps_call:call()) ->
                      {'ok', 'channel_hungup'} |
                      {'error', 'slot_empty' | 'timeout' | 'failed'}.
retrieve_call(Slot, Call) ->
    _  = cleanup_slot(Slot, Call),
    case pickup_parked_call(Slot, Call) of
        'ok' -> whapps_call_command:wait_for_hangup();
        {'error', _} -> {'error', 'failed'}
    end.

-spec pickup_parked_call(wh_json:object(), whapps_call:call()) ->
                                 'ok' |
                                 {'error', 'timeout' | 'failed'}.
pickup_parked_call(Slot, Call) ->
    SlotNumber = wh_json:get_value(<<"Slot-Number">>, Slot),
    Name = wh_json:get_value(<<"CID-Name">>, Slot, <<"Parking Slot ", SlotNumber/binary>>),
    Number = wh_json:get_value(<<"CID-Number">>, Slot, SlotNumber),
    Update = [{<<"Callee-ID-Name">>, Name}
              ,{<<"Callee-ID-Number">>, Number}
             ],
    _ = whapps_call_command:set(wh_json:from_list(Update), 'undefined', Call),
    _ = send_command(Slot, Call),
    wait_for_command(Call).

-spec send_command(wh_json:object(), whapps_call:call()) -> 'ok'.
send_command(Slot, Call) ->
    ParkedCallId = wh_json:get_ne_value(<<"Call-ID">>, Slot),
    lager:info("attempting to pickup parked call ~s"
               ,[ParkedCallId]
              ),
    Command = [{<<"Unbridged-Only">>, 'true'}
               ,{<<"Application-Name">>, <<"call_pickup">>}
               ,{<<"Continue-On-Fail">>, 'false'}
               ,{<<"Continue-On-Cancel">>, 'true'}
               ,{<<"Park-After-Pickup">>, 'false'}
               ,{<<"Target-Call-ID">>, ParkedCallId}
              ],
    whapps_call_command:send_command(Command, Call).

-spec wait_for_command(whapps_call:call()) ->
                             'ok' |
                             {'error', 'timeout' | 'failed'}.
wait_for_command(Call) ->
    case whapps_call_command:receive_event(10000) of
        {'ok', Event} ->
            command_event(Call, wh_util:get_event_type(Event), Event);
        {'error', 'timeout'}=E ->
            lager:debug("timed out attempting to pickup parked call"),
            E
    end.

-spec command_event(whapps_call:call(), {ne_binary(), ne_binary()}, wh_json:object()) ->
                          'ok' |
                          {'error', 'failed'}.
command_event(_Call, {<<"error">>, <<"dialplan">>}, _Event) ->
    lager:debug("failed to pickup parked call ~s"
                ,[wh_json:get_value([<<"Request">>, <<"Target-Call-ID">>], _Event)]
               ),
    {'error', 'failed'};
command_event(_Call, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}, _Event) ->
    lager:debug("succesfully bridged to parked call ~s"
                ,[wh_json:get_value(<<"Other-Leg-Call-ID">>, _Event)]
               );
command_event(Call, _Type, _Evt) ->
    wait_for_command(Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to park the current call scenario
%% @end
%%--------------------------------------------------------------------
-spec park_call(wh_json:object(), whapps_call:call()) -> 'ok'.
park_call(Slot, Call) ->
    ParkedCalls = get_parked_calls(Call),
    SlotNumber = wh_json:get_value(<<"Slot-Number">>, Slot),
    case is_slot_available(SlotNumber, ParkedCalls) of
        'true' ->
            %% It is available so create a new slot and
            %%  overwrite the current...
            maybe_save_slot(Slot, ParkedCalls, Call);
        'false' -> {'error', 'occupied'}
    end.

-spec maybe_save_slot(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                          {'ok', wh_json:object()} |
                          {'error', atom()}.
maybe_save_slot(Slot, ParkedCalls, Call) ->
    ParkedCallId = wh_json:get_value(<<"Call-ID">>, Slot),
    case whapps_call_command:b_channel_status(ParkedCallId) of
        {'ok', _} -> save_slot(Slot, ParkedCalls, Call);
        _Else ->
            lager:debug("ignoring attempt to park terminated call ~s", [ParkedCallId]),
            {'error', 'terminated'}
    end.

-spec save_slot(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                          {'ok', wh_json:object()} |
                          {'error', atom()}.
save_slot(Slot, ParkedCalls, Call) ->
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, ParkedCalls),
    SlotNumber = wh_json:get_value(<<"Slot-Number">>, Slot),
    UpdatedParkedCalls = wh_json:set_value([<<"slots">>, SlotNumber], Slot, ParkedCalls),
    lager:debug("attempting to update parked calls slot ~s with call ~s"
               ,[SlotNumber, wh_json:get_value(<<"Call-ID">>, Slot)]
               ),
    case couch_mgr:save_doc(AccountDb, UpdatedParkedCalls) of
        {'ok', _}=Ok ->
            lager:info("successfully saved parking slot ~s with parked call ~s"
                       ,[SlotNumber, wh_json:get_value(<<"Call-ID">>, Slot)]
                      ),
            Ok;
        {'error', 'conflict'} ->
            maybe_resolve_conflict(Slot, ParkedCalls, Call)
    end.

-spec maybe_resolve_conflict(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', atom()}.
maybe_resolve_conflict(Slot, ParkedCalls, Call) ->
    AccountDb = whapps_call:account_db(Call),
    SlotNumber = wh_json:get_value(<<"Slot-Number">>, Slot),
    ExpectedParkedCallId = find_slot_call_id(SlotNumber, ParkedCalls),
    {'ok', JObj} = couch_mgr:open_doc(AccountDb, ?DB_DOC_NAME),
    case find_slot_call_id(SlotNumber, JObj) of
        'undefined' ->
            lager:info("conflict during save, slot ~s was removed", [SlotNumber]),
            save_slot(Slot, JObj, Call);
        ExpectedParkedCallId ->
            %% If the doc in the db has a call id that matches
            %%  the one we just attempted to save then it must
            %%  have been another slot that bumped the rev
            lager:info("conflict during save, another parking slot was updated", []),
            save_slot(Slot, JObj, Call);
        CurrentParkedCallId ->
            case whapps_call_command:b_channel_status(CurrentParkedCallId) of
                {'ok', _} ->
                    lager:debug("conflict during save, slot ~s was updated with active call ~s"
                                ,[SlotNumber, CurrentParkedCallId]
                               ),
                    {'error', 'occupied'};
                {'error', _} ->
                    lager:debug("conflict during save, slot ~s was updated with terminated call ~s"
                                ,[SlotNumber, CurrentParkedCallId]
                               ),
                    park_call(Slot, Call)
            end
    end.

-spec is_slot_available(ne_binary(), wh_json:object()) -> boolean().
is_slot_available(SlotNumber, ParkedCalls) ->
    %% NOTE: the slot is available if:
    %%  1) There is no call id
    %%  2) The parker and parked call id are the same. This is
    %%     a result of a direct dial park just prior to
    %%     the completion of an attended transfer.  Therefore,
    %%     if the call in the slot currently was a direct dial
    %%     allow it to be replaced even if the parked call is active
    %%  3) The current call id is not active
    Slot = get_slot(SlotNumber, ParkedCalls),
    ParkedCallId = wh_json:get_value(<<"Call-ID">>, Slot),
    ParkerCallId = wh_json:get_value(<<"Parker-Call-ID">>, Slot),
    case wh_util:is_empty(ParkedCallId) orelse
        ParkedCallId =:= ParkerCallId orelse
        whapps_call_command:b_channel_status(ParkedCallId)
    of
        'true' -> 'true';
        {'error', _} ->
            lager:debug("slot ~s currently has nonexistent parked call ~s"
                        ,[SlotNumber, ParkedCallId]
                       ),
            'true';
        {'ok', _} ->
            lager:info("slot ~s is occupied by active call ~s"
                       ,[SlotNumber, ParkedCallId]
                      ),
            'false';
        'false' -> 'false'
    end.

-spec create_slot(ne_binary(), whapps_call:call()) -> wh_json:object() .
create_slot(SlotNumber, Call) ->
    create_slot(SlotNumber, Call, 'undefined').

-spec create_slot(ne_binary(), whapps_call:call(), api_binary()) -> wh_json:object().
create_slot(SlotNumber, Call, ParkerCallId) ->
    CallId = cf_exe:callid(Call),
    AccountDb = whapps_call:account_db(Call),
    AccountId = whapps_call:account_id(Call),
    RingbackId = maybe_get_ringback_id(Call),
    SlotCallId = wh_util:rand_hex_binary(16),
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Call-ID">>, CallId}
         ,{<<"Slot-Call-ID">>, SlotCallId}
         ,{<<"Switch-URI">>, whapps_call:switch_uri(Call)}
         ,{<<"From-Tag">>, whapps_call:from_tag(Call)}
         ,{<<"To-Tag">>, whapps_call:to_tag(Call)}
         ,{<<"Parker-Call-ID">>, ParkerCallId}
         ,{<<"Ringback-ID">>, RingbackId}
         ,{<<"Presence-ID">>, <<(whapps_call:request_user(Call))/binary
                                ,"@", (wh_util:get_account_realm(AccountDb, AccountId))/binary
                              >>
          }
         ,{<<"Slot-Number">>, SlotNumber}
         ,{<<"Node">>, whapps_call:switch_nodename(Call)}
         ,{<<"CID-Number">>, whapps_call:caller_id_number(Call)}
         ,{<<"CID-Name">>, whapps_call:caller_id_name(Call)}
         ,{<<"CID-URI">>, whapps_call:from(Call)}
         ,{<<"Hold-Media">>, cf_attributes:moh_attributes(RingbackId, <<"media_id">>, Call)}
        ])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% After an attended transfer we need to find the callid that we stored
%% because it was the "C-Leg" of a transfer and now we have the
%% actuall "A-Leg".  Find the old callid and update it with the new one.
%% @end
%%--------------------------------------------------------------------
update_call_id(Replaces, Call) ->
    ParkedCalls = get_parked_calls(Call),
    update_call_id(Replaces, ParkedCalls, Call).

-spec update_call_id(ne_binary(), wh_json:object(), whapps_call:call()) ->
                            {'ok', ne_binary(), wh_json:object()}.
update_call_id(Replaces, ParkedCalls, Call) ->
    update_call_id(Replaces, ParkedCalls, Call, 0).

update_call_id(_, _, _, Loops) when Loops > 5 ->
    lager:info("unable to update parked call id after ~p tries", [Loops]),
    {'error', 'update_failed'};
update_call_id(Replaces, ParkedCalls, Call, Loops) ->
    CallId = cf_exe:callid(Call),
    lager:info("update parked call id ~s with new call id ~s", [Replaces, CallId]),
    Slots = wh_json:get_value(<<"slots">>, ParkedCalls, wh_json:new()),
    case find_slot_by_callid(Slots, Replaces) of
        {'ok', SlotNumber, Slot} ->
            lager:info("found parked call id ~s in slot ~s", [Replaces, SlotNumber]),
            CallerNode = whapps_call:switch_nodename(Call),
            Updaters = [fun(J) -> wh_json:set_value(<<"Call-ID">>, CallId, J) end
                        ,fun(J) -> wh_json:set_value(<<"Node">>, CallerNode, J) end
                        ,fun(J) -> wh_json:set_value(<<"CID-Number">>, whapps_call:caller_id_number(Call), J) end
                        ,fun(J) -> wh_json:set_value(<<"CID-Name">>, whapps_call:caller_id_name(Call), J) end
                        ,fun(J) -> wh_json:set_value(<<"CID-URI">>, whapps_call:from(Call), J) end
                        ,fun(J) -> maybe_set_hold_media(J, Call) end
                        ,fun(J) -> maybe_set_ringback_id(J, Call) end
                      ],
            UpdatedSlot = lists:foldr(fun(F, J) -> F(J) end, Slot, Updaters),
            JObj = wh_json:set_value([<<"slots">>, SlotNumber], UpdatedSlot, ParkedCalls),
            case couch_mgr:save_doc(whapps_call:account_db(Call), JObj) of
                {'ok', _} ->
                    update_presence(?PARKED_PRESENCE_TYPE, UpdatedSlot),
                    {'ok', SlotNumber, UpdatedSlot};
                {'error', 'conflict'} ->
                    AccountDb = whapps_call:account_db(Call),
                    wh_cache:erase_local(?CALLFLOW_CACHE, ?PARKED_CALLS_KEY(AccountDb)),
                    update_call_id(Replaces, get_parked_calls(Call), Call);
                {'error', _R} ->
                    lager:info("failed to update parking slot with call id ~s: ~p", [Replaces, _R]),
                    timer:sleep(250),
                    update_call_id(Replaces, get_parked_calls(Call), Call, Loops + 1)
            end;
        {'error', _R} ->
            lager:info("failed to find parking slot with call id ~s: ~p", [Replaces, _R]),
            timer:sleep(250),
            update_call_id(Replaces, get_parked_calls(Call), Call, Loops + 1)
    end.

-spec maybe_set_ringback_id(wh_json:object(), whapps_call:call()) -> wh_json:object().
maybe_set_ringback_id(JObj, Call) ->
    case wh_json:get_value(<<"Ringback-ID">>, JObj) =:= 'undefined'
        andalso maybe_get_ringback_id(Call)
    of
        'undefined' -> JObj; %% no found ringback id
        'false' -> JObj; %% ringback on JObj
        RingbackId ->
            wh_json:set_value(<<"Ringback-ID">>, RingbackId, JObj)
    end.

-spec maybe_set_hold_media(wh_json:object(), whapps_call:call()) -> wh_json:object().
maybe_set_hold_media(JObj, Call) ->
    RingbackId = wh_json:get_value(<<"Ringback-ID">>, JObj),
    HoldMedia = wh_json:get_value(<<"Hold-Media">>, JObj),
    case RingbackId =/= 'undefined' andalso HoldMedia =:= 'undefined' of
        'false' -> JObj;
        'true' ->
            case cf_attributes:moh_attributes(RingbackId, <<"media_id">>, Call) of
                'undefined' -> JObj;
                RingbackHoldMedia ->
                    wh_json:set_value(<<"Hold-Media">>, RingbackHoldMedia, JObj)
            end
    end.

-spec maybe_get_ringback_id(whapps_call:call()) -> api_binary().
maybe_get_ringback_id(Call) ->
    Referred = whapps_call:custom_channel_var(<<"Referred-By">>, Call),
    ReOptions = [{'capture', [1], 'binary'}],
    case catch(re:run(Referred, <<".*sip:(.*)@.*">>, ReOptions)) of
        {'match', [Match]} -> get_endpoint_id(Match, Call);
        _ -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the provided slot number or the next available if none
%% was provided
%% @end
%%--------------------------------------------------------------------
-spec get_slot(whapps_call:call()) -> api_binary().
get_slot(Call) ->
    get_slot(Call, get_parked_calls(Call)).

-spec get_slot(ne_binary() | whapps_call:call(), wh_json:object()) -> api_object().
get_slot(SlotNumber, ParkedCalls) when is_binary(SlotNumber) ->
    case wh_json:get_ne_value([<<"slots">>, SlotNumber], ParkedCalls) of
        'undefined' -> 'undefined';
        Slot -> wh_json:set_value(<<"Slot-Number">>, SlotNumber, Slot)
    end;
get_slot(Call, ParkedCalls) ->
    SlotNumber = get_slot_number(ParkedCalls, whapps_call:kvs_fetch('cf_capture_group', Call)),
    get_slot(SlotNumber, ParkedCalls).

-spec get_slot_number(wh_json:object(), whapps_call:call()) -> ne_binary().
get_slot_number(_, CaptureGroup) when is_binary(CaptureGroup)
                                      andalso size(CaptureGroup) > 0 ->
    CaptureGroup;
get_slot_number(ParkedCalls, _) ->
    Slots = [wh_util:to_integer(Slot)
             || Slot <- wh_json:get_keys(<<"slots">>, ParkedCalls)
            ],
    Sorted = ordsets:to_list(ordsets:from_list([100|Slots])),
    wh_util:to_binary(find_slot_number(Sorted)).

-spec find_slot_number([integer(),...]) -> integer().
find_slot_number([A]) -> A + 1;
find_slot_number([A|[B|_]=Slots]) ->
    case B =:= A + 1 of
        'false' -> A + 1;
        'true' -> find_slot_number(Slots)
    end.

-spec find_slot_call_id(ne_binary(), wh_json:object()) -> api_binary().
find_slot_call_id(SlotNumber, ParkedCalls) ->
    wh_json:get_ne_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given the parked calls and a list of parked keys find the slot with
%% the provided call id.
%% @end
%%--------------------------------------------------------------------
-spec find_slot_by_callid(wh_json:object(), ne_binary()) ->
                                 {'ok', ne_binary(), wh_json:object()} |
                                 {'error', 'not_found'}.
find_slot_by_callid(Slots, CallId) ->
    find_slot_by_callid(wh_json:get_keys(Slots), Slots, CallId).

-spec find_slot_by_callid(ne_binaries(), wh_json:object(), ne_binary()) ->
                                 {'ok', ne_binary(), wh_json:object()} |
                                 {'error', 'not_found'}.
find_slot_by_callid([], _, _) ->
    {'error', 'not_found'};
find_slot_by_callid([SlotNumber|SlotNumbers], Slots, CallId) ->
    Slot = wh_json:get_value(SlotNumber, Slots),
    case wh_json:get_value(<<"Call-ID">>, Slot) of
        CallId -> {'ok', SlotNumber, Slot};
        _ ->
            case wh_json:get_value(<<"Slot-Call-ID">>, Slot) of
                CallId -> {'ok', SlotNumber, Slot};
                _ -> find_slot_by_callid(SlotNumbers, Slots, CallId)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to retrieve the parked calls list from the datastore, if
%% the list does not exist then it returns an new empty instance
%% @end
%%--------------------------------------------------------------------
-spec get_parked_calls(whapps_call:call()) -> wh_json:object().
get_parked_calls(Call) ->
    get_parked_calls(whapps_call:account_db(Call), whapps_call:account_id(Call)).

-spec get_parked_calls(ne_binary(), ne_binary()) -> wh_json:object().
get_parked_calls(AccountDb, AccountId) ->
    case couch_mgr:open_cache_doc(AccountDb, ?DB_DOC_NAME) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} -> create_parked_calls(AccountDb, AccountId);
        {'error', _R}=E ->
            lager:info("unable to get parked calls: ~p", [_R]),
            E
    end.

-spec create_parked_calls(ne_binary(), ne_binary()) -> wh_json:object().
create_parked_calls(AccountDb, AccountId) ->
    lager:debug("generating new parked call document for ~s", [AccountId]),
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Generators = [fun(J) -> wh_doc:set_id(J, <<"parked_calls">>) end
                  ,fun(J) -> wh_doc:set_type(J, <<"parked_calls">>) end
                  ,fun(J) -> wh_doc:set_account_id(J, AccountId) end
                  ,fun(J) -> wh_doc:set_account_db(J, AccountDb) end
                  ,fun(J) -> wh_doc:set_created(J, Timestamp) end
                  ,fun(J) -> wh_doc:set_modified(J, Timestamp) end
                  ,fun(J) -> wh_doc:set_vsn(J, <<"1">>) end
                  ,fun(J) -> wh_json:set_value(<<"slots">>, wh_json:new(), J) end
                 ],
    lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), Generators).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
cleanup_slot(Slot, Call) ->
    ParkedCalls = get_parked_calls(Call),
    SlotNumber = wh_json:get_value(<<"Slot-Number">>, Slot),
    ParkedCallId = wh_json:get_value(<<"Call-ID">>, Slot),
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, ParkedCalls),
    %% Compare the slot call id we are attempting to clean
    %%  to the current call id in the same slot
    case find_slot_call_id(SlotNumber, ParkedCalls) of
        ParkedCallId ->
            %%  if the two call ids match then remove the slot
            UpdatedParkedCalls = wh_json:delete_key([<<"slots">>, SlotNumber], ParkedCalls),
            case couch_mgr:save_doc(AccountDb, UpdatedParkedCalls) of
                {'error', 'conflict'} ->
                    lager:info("conflict attempting to removed slot ~s with parked call ~s", [SlotNumber, ParkedCallId]),
                    cleanup_slot(Slot, Call);
                {'ok', _} ->
                    lager:info("removed slot ~s with parked call ~s", [SlotNumber, ParkedCallId]),
                    update_presence(<<"terminated">>, Slot),
                    'ok'
            end;
        _CurrentParkedCallId ->
            %% if the call ids no longer match then ignore it
            lager:info("attempted to remove slot ~s with parked call ~s but slot occupied with unexpected parked call ~s"
                       ,[SlotNumber, ParkedCallId, _CurrentParkedCallId]
                      ),
            'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_for_pickup(ne_binary(), wh_json:object(), whapps_call:call()) -> 'ok'.
wait_for_pickup(SlotNumber, Slot, Call) ->
    Timeout = wait_for_pickup_timeout(Slot),
    lager:debug("waiting (~p) for parked call to be picked up or hangup", [Timeout]),
    _ = wait_hold_media(Slot, Call),
    case whapps_call_command:wait_for_unparked_call(Call, Timeout) of
        {'error', 'timeout'} ->
            parked_call_timeout(SlotNumber, Slot, Call);
        {'error', 'channel_disconnected'} ->
            parked_call_disconected(SlotNumber, Slot, Call);
        {'error', _} ->
            parked_call_error(SlotNumber, Slot, Call);
        {'ok', _} ->
            parked_call_pickedup(SlotNumber, Slot, Call)
    end.

wait_hold_media(Slot, Call) ->
    case wh_json:get_ne_value(<<"Hold-Media">>, Slot) of
        'undefined' -> 'ok';
        HoldMedia ->
            whapps_call_command:hold(HoldMedia, Call)
    end.

wait_for_pickup_timeout(Slot) ->
    case wh_json:get_ne_value(<<"Ringback-ID">>, Slot) of
        'undefined' -> 'infinity';
        _Else -> ?DEFAULT_RINGBACK_TM
    end.

parked_call_pickedup(SlotNumber, Slot, Call) ->
    lager:info("parked call in slot ~s has been picked up", [SlotNumber]),
    _ = cleanup_slot(Slot, Call),
    cf_exe:transfer(Call).

parked_call_error(SlotNumber, Slot, Call) ->
    lager:info("parked call in slot ~s has hungup", [SlotNumber]),
    _ = cleanup_slot(Slot, Call),
    cf_exe:transfer(Call).

parked_call_disconected(SlotNumber, Slot, Call) ->
    lager:info("parked call in slot ~s has disconnected, checking status", [SlotNumber]),
    case whapps_call_command:b_channel_status(cf_exe:callid(Call)) of
        {'ok', _} ->
            lager:info("call '~s' is still active", [cf_exe:callid(Call)]),
            wait_for_pickup(SlotNumber, Slot, Call);
        _Else ->
            lager:info("call '~s' is no longer active, ", [cf_exe:callid(Call)]),
            _ = cleanup_slot(Slot, Call),
            cf_exe:transfer(Call)
    end.

parked_call_timeout(SlotNumber, Slot, Call) ->
    ParkedCallId = wh_json:get_value(<<"Call-ID">>, Slot),
    case whapps_call_command:b_channel_status(ParkedCallId) of
        {'error', _} ->
            lager:info("parked call doesnt exist anymore, hangup"),
            _ = cleanup_slot(Slot, Call),
            cf_exe:stop(Call);
        {'ok', _} ->
            case maybe_ringback_parker(SlotNumber, Slot, Call) of
                'answered' ->
                    %% TODO: wait for hangup??
                    cf_exe:continue(Call);
                'failed' ->
                    lager:info("parked call ringback was not answered, continuing to hold"),
                    wait_for_pickup(SlotNumber, Slot, Call)
                end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ringback the device that parked the call
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint_id(api_binary(), whapps_call:call()) -> api_binary().
get_endpoint_id('undefined', _) -> 'undefined';
get_endpoint_id(Username, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case cf_util:endpoint_id_by_sip_username(AccountDb, Username) of
        {'ok', EndpointId} -> EndpointId;
        {'error', _} -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ringback the device that parked the call
%% @end
%%--------------------------------------------------------------------
maybe_ringback_parker(SlotNumber, Slot, Call) ->
    case wh_json:get_ne_value(<<"Ringback-ID">>, Slot) of
        'undefined' -> 'failed';
        EndpointId ->
            ringback_parker(EndpointId, SlotNumber, Slot, Call)
    end.

-spec ringback_parker(ne_binary(), ne_binary(), wh_json:object(), whapps_call:call()) ->
                             'answered' | 'failed' | 'channel_hungup'.
ringback_parker(EndpointId, SlotNumber, Slot, Call) ->
    case cf_endpoint:build(EndpointId, wh_json:from_list([{<<"can_call_self">>, 'true'}]), Call) of
        {'ok', Endpoints} ->
            lager:info("attempting to ringback endpoint ~s", [EndpointId]),
            RingbackCID = <<"Parking slot ", SlotNumber/binary, " ringback">>,
            OriginalCID = whapps_call:caller_id_name(Call),
            Call1 = whapps_call:set_caller_id_name(RingbackCID, Call),
            whapps_call_command:bridge(Endpoints, ?DEFAULT_TIMEOUT_S, Call1),
            wait_for_ringback(ringback_cleanup(OriginalCID, Slot, Call), Call1);
        _ -> 'failed'
    end.

ringback_cleanup(OriginalCID, Slot, Call) ->
    fun(_) ->
            lager:info("parking ringback was answered", []),
            _ = whapps_call:set_caller_id_name(OriginalCID, Call),
            cleanup_slot(Slot, Call)
    end.

-spec wait_for_ringback(function(), whapps_call:call()) ->
                             'answered' | 'failed' | 'channel_hungup'.
wait_for_ringback(Fun, Call) ->
     case whapps_call_command:wait_for_bridge(30000, Fun, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the ringback device"),
            'answered';
        {'fail', JObj} ->
            case wh_json:get_value(<<"Event-Name">>, JObj) of
                <<"CHANNEL_DESTROY">> ->
                    lager:info("channel hungup during ringback"),
                    'channel_hungup';
                _Else ->
                    lager:info("ringback failed, returning caller to parking slot: ~p", [_Else]),
                    'failed'
            end;
        _Else ->
            lager:info("ringback failed, returning caller to parking slot: ~p" , [_Else]),
            'failed'
    end.

-spec update_presence(ne_binary(), api_object()) -> 'ok'.
update_presence(_State, 'undefined') -> 'ok';
update_presence(State, Slot) ->
    PresenceId = wh_json:get_value(<<"Presence-ID">>, Slot),
    TargetURI = wh_json:get_value(<<"CID-URI">>, Slot),
    ToTag = wh_json:get_value(<<"To-Tag">>, Slot),
    FromTag = wh_json:get_value(<<"From-Tag">>, Slot),
    SwitchURI = wh_json:get_value(<<"Switch-URI">>, Slot),
    CallId = wh_json:get_value(<<"Call-ID">>, Slot),
    SlotCallId = wh_json:get_value(<<"Slot-Call-ID">>, Slot),
    Command = props:filter_undefined(
                [{<<"Presence-ID">>, PresenceId}
                 ,{<<"From">>, TargetURI}
                 ,{<<"From-Tag">>, FromTag}
                 ,{<<"To-Tag">>, ToTag}
                 ,{<<"State">>, State}
                 ,{<<"Call-ID">>, SlotCallId}
                 ,{<<"Target-Call-ID">>, CallId}
                 ,{<<"Switch-URI">>, SwitchURI}
                 | wh_api:default_headers(<<"park">>, ?APP_VERSION)
                ]),
    lager:debug("update presence-id '~s' with state: ~s", [PresenceId, State]),
    wh_amqp_worker:cast(Command, fun wapi_presence:publish_update/1).

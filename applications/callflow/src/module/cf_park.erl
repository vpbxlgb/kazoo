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
    case get_slot_call_id(SlotNumber, ParkedCalls) of
        'undefined' -> 'ok';
        ParkedCallId ->
            update_parked_call_presence(SlotNumber, ParkedCalls, ParkedCallId)
    end.

-spec update_parked_call_presence(wh_json:key(), wh_json:object(), ne_binary()) -> 'ok'.
update_parked_call_presence(SlotNumber, ParkedCalls, ParkedCallId) ->
    Slot = wh_json:get_value([<<"slots">>, SlotNumber], ParkedCalls),
    case whapps_call_command:b_channel_status(ParkedCallId) of
        {'ok', _Status} -> update_presence(?PARKED_PRESENCE_TYPE, Slot);
        {'error', _} -> update_presence(<<"terminated">>, Slot)
    end.

-spec get_slot_call_id(wh_json:key(), wh_json:object()) -> api_binary().
get_slot_call_id(SlotNumber, ParkedCalls) ->
    wh_json:get_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls).

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

-spec handle_direct_dial(wh_json:object(), whapps_call:call()) -> 'ok'.
handle_direct_dial(Data, Call) ->
    lager:info("call was the result of a direct dial"),
    ParkedCalls = get_parked_calls(Call),
    SlotNumber = get_slot_number(ParkedCalls, whapps_call:kvs_fetch('cf_capture_group', Call)),
    case wh_json:get_value(<<"action">>, Data, <<"park">>) of
        <<"park">> ->
            lager:info("action is to park the call"),
            Slot = create_slot(Call, cf_exe:callid(Call)),
            park_call(SlotNumber, Slot, ParkedCalls, 'undefined', Call);
        <<"retrieve">> ->
            lager:info("action is to retrieve a parked call"),
            case maybe_retrieve_call(SlotNumber, ParkedCalls, Call) of
                {'ok', _} -> 'ok';
                _Else ->
                    _ = whapps_call_command:b_answer(Call),
                    _ = whapps_call_command:b_prompt(<<"park-no_caller">>, Call),
                    cf_exe:continue(Call)
            end;
        <<"auto">> ->
            lager:info("action is to automatically determine if we should retrieve or park"),
            case maybe_retrieve_call(SlotNumber, ParkedCalls, Call) of
                {'ok', _} -> cf_exe:continue(Call);
                {'error', _} ->
                    Slot = create_slot(Call, cf_exe:callid(Call)),
                    park_call(SlotNumber, Slot, ParkedCalls, 'undefined', Call)
            end
    end.

handle_blind_transfer(Call) ->
    lager:info("call was the result of a blind transfer, assuming intention was to park"),
    ParkedCalls = get_parked_calls(Call),
    SlotNumber = get_slot_number(ParkedCalls, whapps_call:kvs_fetch('cf_capture_group', Call)),
    Slot = create_slot(Call),
    park_call(SlotNumber, Slot, ParkedCalls, ReferredTo, Call).

handle_attended_transfer(Replaces, Call) ->
    lager:info("call was the result of an attended-transfer completion, updating call id"),
    ParkedCalls = get_parked_calls(Call),
    {'ok', FoundInSlotNumber, Slot} = update_call_id(Replaces, ParkedCalls, Call),
    wait_for_pickup(FoundInSlotNumber, Slot, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to retrieve a parked call
%% @end
%%--------------------------------------------------------------------
-spec maybe_retrieve_call(ne_binary(), wh_json:object(), whapps_call:call()) ->
                      {'ok', 'channel_hungup'} |
                      {'error', 'slot_empty' | 'timeout' | 'failed'}.
maybe_retrieve_call(SlotNumber, ParkedCalls, Call) ->
    case wh_json:get_value([<<"slots">>, SlotNumber], ParkedCalls) of
        'undefined' ->
            lager:info("attempted to retrieve parked call from empty parking slot ~s", [SlotNumber]),
            {'error', 'slot_empty'};
        Slot ->
            retrieve_call(Slot, SlotNumber, ParkedCalls, Call)
    end.

-spec retrieve_call(wh_json:object(), ne_binary(), wh_json:object(), whapps_call:call()) ->
                      {'ok', 'channel_hungup'} |
                      {'error', 'slot_empty' | 'timeout' | 'failed'}.
retrieve_call(Slot, SlotNumber, ParkedCalls, Call) ->
    case pickup_parked_call(Slot, SlotNumber, Call) of
        'ok' ->
            lager:debug("successfully picked up parked caller"),
            cleanup_slot(SlotNumber, ParkedCall, whapps_call:account_db(Call)),
            whapps_call_command:wait_for_hangup();
        {'error', _E}=E ->
            update_presence(<<"terminated">>, Slot),
            lager:debug("failed to pickup parked call ~s: ~p"
                       ,[ParkedCall, _E]),
            E
    end.

-spec pickup_parked_call(wh_json:object(), ne_binary(), whapps_call:call()) ->
                                 'ok' |
                                 {'error', 'timeout' | 'failed'}.
pickup_parked_call(Slot, SlotNumber, Call) ->
    ParkedCall = wh_json:get_ne_value(<<"Call-ID">>, Slot),
    lager:info("attempting to pickup parked call ~s from slot ~s"
              ,[ParkedCall, SlotNumber]),
    Name = wh_json:get_value(<<"CID-Name">>, Slot, <<"Parking Slot ", SlotNumber/binary>>),
    Number = wh_json:get_value(<<"CID-Number">>, Slot, SlotNumber),
    Update = [{<<"Callee-ID-Name">>, Name}
             ,{<<"Callee-ID-Number">>, Number}
             ],
    _ = whapps_call_command:set(wh_json:from_list(Update), 'undefined', Call),
    _ = send_pickup(ParkedCall, Call),
    wait_for_pickup(Call).

-spec send_pickup(ne_binary(), whapps_call:call()) -> 'ok'.
send_pickup(ParkedCall, Call) ->
    Req = [{<<"Unbridged-Only">>, 'true'}
           ,{<<"Application-Name">>, <<"call_pickup">>}
           ,{<<"Target-Call-ID">>, ParkedCall}
           ,{<<"Continue-On-Fail">>, 'false'}
           ,{<<"Continue-On-Cancel">>, 'true'}
           ,{<<"Park-After-Pickup">>, 'false'}
          ],
    whapps_call_command:send_command(Req, Call).

-spec wait_for_pickup(whapps_call:call()) ->
                             'ok' |
                             {'error', 'timeout' | 'failed'}.
wait_for_pickup(Call) ->
    case whapps_call_command:receive_event(10000) of
        {'ok', Evt} ->
            pickup_event(Call, wh_util:get_event_type(Evt), Evt);
        {'error', 'timeout'}=E -> E
    end.

-spec pickup_event(whapps_call:call(), {ne_binary(), ne_binary()}, wh_json:object()) ->
                          'ok' |
                          {'error', 'failed'}.
pickup_event(_Call, {<<"error">>, <<"dialplan">>}, Evt) ->
    lager:debug("error in dialplan: ~s", [wh_json:get_value(<<"Error-Message">>, Evt)]),
    {'error', 'failed'};
pickup_event(_Call, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}, _Evt) ->
    lager:debug("channel bridged to ~s", [wh_json:get_value(<<"Other-Leg-Call-ID">>, _Evt)]);
pickup_event(Call, _Type, _Evt) ->
    wait_for_pickup(Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to park the current call scenario
%% @end
%%--------------------------------------------------------------------
-spec park_call(ne_binary(), wh_json:object(), wh_json:object(), api_binary(), whapps_call:call()) -> 'ok'.
park_call(SlotNumber, Slot, ParkedCalls, ReferredTo, Call) ->
    lager:info("attempting to park call in slot ~s", [SlotNumber]),
    case {ReferredTo, maybe_save_slot(SlotNumber, Slot, ParkedCalls, Call)} of
        %% attended transfer but the provided slot number is occupied, we are still connected to the 'parker'
        %% not the 'parkee'
        {'undefined', {'error', 'occupied'}} ->
            lager:info("selected slot is occupied"),
            %% Update screen with error that the slot is occupied
            _ = whapps_call_command:b_answer(Call),
            %% playback message that caller will have to try a different slot
            _ = whapps_call_command:b_prompt(<<"park-already_in_use">>, Call),
            cf_exe:continue(Call),
            'ok';
        %% attended transfer and allowed to update the provided slot number, we are still connected to the 'parker'
        %% not the 'parkee'
        {'undefined', _} ->
            lager:info("playback slot number ~s to caller", [SlotNumber]),
            %% Update screen with new slot number
            _ = whapps_call_command:b_answer(Call),
            %% Caller parked in slot number...
            _ = whapps_call_command:b_prompt(<<"park-call_placed_in_spot">>, Call),
            _ = whapps_call_command:b_say(wh_util:to_binary(SlotNumber), Call),
            cf_exe:transfer(Call);
        %% blind transfer and but the provided slot number is occupied
        {_, {'error', 'occupied'}} ->
            lager:info("blind transfer to a occupied slot, call the parker back.."),
            TmpCID = <<"Parking slot ", SlotNumber/binary, " occupied">>,
            case ringback_parker(wh_json:get_value(<<"Ringback-ID">>, Slot), SlotNumber, TmpCID, Call) of
                'answered' -> cf_exe:continue(Call);
                'channel_hungup' -> cf_exe:stop(Call);
                'failed' ->
                    whapps_call_command:hangup(Call),
                    cf_exe:stop(Call)
            end,
            'ok';
        %% blind transfer and allowed to update the provided slot number
        {_, {'ok', _}} ->
            ParkedCallId = wh_json:get_value(<<"Call-ID">>, Slot),
            lager:info("call ~s parked in slot ~s", [ParkedCallId, SlotNumber]),
            update_presence(?PARKED_PRESENCE_TYPE, Slot),
            wait_for_pickup(SlotNumber, Slot, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the json object representing the call in the parking slot
%% @end
%%--------------------------------------------------------------------
-spec create_slot(whapps_call:call()) -> wh_json:object().
create_slot(Call) ->
    create_slot(Call, 'undefined').

-spec create_slot(whapps_call:call(), api_binary()) -> wh_json:object().
create_slot(Call, ParkerCallId) ->
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
         ,{<<"Node">>, whapps_call:switch_nodename(Call)}
         ,{<<"CID-Number">>, whapps_call:caller_id_number(Call)}
         ,{<<"CID-Name">>, whapps_call:caller_id_name(Call)}
         ,{<<"CID-URI">>, whapps_call:from(Call)}
         ,{<<"Hold-Media">>, cf_attributes:moh_attributes(RingbackId, <<"media_id">>, Call)}
        ])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the provided slot number or the next available if none
%% was provided
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save the slot data in the parked calls object at the slot number.
%% If, on save, it conflicts then it gets the new instance
%% and tries again, determining the new slot.
%% @end
%%--------------------------------------------------------------------
-spec maybe_save_slot(ne_binary(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                       {'ok', wh_json:object()} |
                       {'error', atom()}.
maybe_save_slot(SlotNumber, Slot, ParkedCalls, Call) ->
    ParkedCallId = wh_json:get_ne_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls),
    ParkerCallId = wh_json:get_ne_value([<<"slots">>, SlotNumber, <<"Parker-Call-ID">>], ParkedCalls),
    case wh_util:is_empty(ParkedCallId) orelse
        %% NOTE: the parker and parked call id will be the same as a
        %%   result of a direct dial, this happens just prior to
        %%   the completion of an attended transfer.  Therefore,
        %%   if the call in the slot currently was a direct dial
        %%   allow it to be replaced even if the parked call is active
        ParkedCallId =:= ParkerCallId orelse
        whapps_call_command:b_channel_status(ParkedCallId)
    of
        'true' -> save_slot(SlotNumber, Slot, ParkedCalls, Call);
        {'ok', _} ->
            lager:info("slot ~s is occupied by active call ~s",
                       [SlotNumber, ParkedCallId]
                      ),
            {'error', 'occupied'};
        _Else ->
            lager:info("slot ~s is occupied by inactive call ~s",
                       [SlotNumber, ParkedCallId]
                      ),
            save_slot(SlotNumber, Slot, ParkedCalls, Call)
    end.

-spec save_slot(ne_binary(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                          {'ok', wh_json:object()} |
                          {'error', atom()}.
save_slot(SlotNumber, Slot, ParkedCalls, Call) ->
    AccountDb = whapps_call:account_db(Call),
    CallId = wh_json:get_value(<<"Call-ID">>, Slot),
    case whapps_call_command:b_channel_status(CallId) of
        {'ok', _} ->
            lager:debug("attempting to update parked calls document for slot ~s with call ~s", [SlotNumber, CallId]),
            case couch_mgr:save_doc(AccountDb, wh_json:set_value([<<"slots">>, SlotNumber], Slot, ParkedCalls)) of
                {'ok', JObj}=Ok ->
                    lager:info("successfully stored call parking data for slot ~s", [SlotNumber]),
                    cache_parked_calls(JObj, AccountDb),
                    Ok;
                {'error', 'conflict'} ->
                    maybe_resolve_conflict(SlotNumber, Slot, ParkedCalls, Call)
            end;
        _Else ->
            lager:debug("ignoring attempt to park terminated call ~s", [CallId]),
            {'error', 'terminated'}
    end.

-spec maybe_resolve_conflict(ne_binary(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', atom()}.
maybe_resolve_conflict(SlotNumber, Slot, ParkedCalls, Call) ->
    AccountDb = whapps_call:account_db(Call),
    ExpectedParkedCall = wh_json:get_ne_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls),
    {'ok', JObj} = couch_mgr:open_doc(AccountDb, ?DB_DOC_NAME),
    case wh_json:get_ne_value([<<"slots">>, SlotNumber, <<"Call-ID">>], JObj, ExpectedParkedCall) of
        ExpectedParkedCall ->
            lager:info("conflict when saving parking slot due to a different slot update", []),
            save_slot(SlotNumber, Slot, JObj, Call);
        CurrentParkedCall ->
            lager:info("conflict when saving parking slot due to slot update with call ~s", [CurrentParkedCall]),
            maybe_save_slot(SlotNumber, Slot, JObj, Call)
    end.

-spec cache_parked_calls(wh_json:object(), ne_binary()) -> 'ok'.
cache_parked_calls(JObj, AccountDb) ->
    %% NOTE: do NOT move to couch_mgr:open_cache_doc, using the
    %%   local cache ensures the messages in the gen_server mailbox
    %%   are in the correct order....
    CacheProps = [{'origin', {'db', AccountDb, ?DB_DOC_NAME}}],
    wh_cache:store_local(?CALLFLOW_CACHE, ?PARKED_CALLS_KEY(AccountDb), JObj, CacheProps).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% After an attended transfer we need to find the callid that we stored
%% because it was the "C-Leg" of a transfer and now we have the
%% actuall "A-Leg".  Find the old callid and update it with the new one.
%% @end
%%--------------------------------------------------------------------
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
    case wh_cache:peek_local(?CALLFLOW_CACHE, ?PARKED_CALLS_KEY(AccountDb)) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} ->
            fetch_parked_calls(AccountDb, AccountId)
    end.

-spec fetch_parked_calls(ne_binary(), ne_binary()) -> wh_json:object().
fetch_parked_calls(AccountDb, AccountId) ->
    case couch_mgr:open_doc(AccountDb, ?DB_DOC_NAME) of
        {'ok', JObj} ->
            cache_parked_calls(JObj, AccountDb),
            JObj;
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
    lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), Generators);

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
cleanup_slot(Slot, SlotNumber, Call) ->
    update_presence(<<"terminated">>, Slot),
    ParkedCalls = get_parked_calls(Call),
    ParkedCallId = wh_json:get_value(<<"Call-ID">>, Slot),
    case wh_json:get_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls) of
        ParkedCallId ->
            lager:info("delete parked call ~s in slot ~s", [ParkedCallId, SlotNumber]),
            UpdatedParkedCalls = wh_json:delete_key([<<"slots">>, SlotNumber], ParkedCalls),
            case couch_mgr:save_doc(AccountDb, UpdatedParkedCalls) of
                {'ok', JObj}=Ok ->
                    cache_parked_calls(JObj, AccountDb),
                    Ok;


                {'error', 'conflict'} -> cleanup_slot(SlotNumber, ParkedCallId, AccountDb);



                {'error', _R}=E ->
                    lager:info("failed to delete slot: ~p", [_R]),
                    E
            end;
        _Else ->
            lager:info("call ~s is parked in slot ~s and we expected ~s", [_Else, SlotNumber, ParkedCallId]),
            {'error', 'unexpected_callid'}
    end.

-spec cleanup_slot(ne_binary(), ne_binary(), ne_binary()) ->
                          {'ok', wh_json:object()} |
                          {'error', _}.
cleanup_slot(SlotNumber, ParkedCallId, AccountDb) ->
    case couch_mgr:open_doc(AccountDb, ?DB_DOC_NAME) of
        {'ok', JObj} ->
            case wh_json:get_value([<<"slots">>, SlotNumber, <<"Call-ID">>], JObj) of
                ParkedCallId ->
                    lager:info("delete parked call ~s in slot ~s", [ParkedCallId, SlotNumber]),
                    case couch_mgr:save_doc(AccountDb, wh_json:delete_key([<<"slots">>, SlotNumber], JObj)) of
                        {'ok', _}=Ok ->
                            Slot = wh_json:get_value([<<"slots">>, SlotNumber], JObj),
                            update_presence(<<"terminated">>, Slot),
                            Ok;
                        {'error', 'conflict'} -> cleanup_slot(SlotNumber, ParkedCallId, AccountDb);
                        {'error', _R}=E ->
                            lager:info("failed to delete slot: ~p", [_R]),
                            E
                    end;
                _Else ->
                    lager:info("call ~s is parked in slot ~s and we expected ~s", [_Else, SlotNumber, ParkedCallId]),
                    {'error', 'unexpected_callid'}
            end;
        {'error', _R}=E ->
            lager:info("failed to open the parked calls doc: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_for_pickup(ne_binary(), wh_json:object(), whapps_call:call()) -> 'ok'.
wait_for_pickup(SlotNumber, Slot, Call) ->
    RingbackId = wh_json:get_value(<<"Ringback-ID">>, Slot),
    HoldMedia = wh_json:get_value(<<"Hold-Media">>, Slot),
    Timeout = case wh_util:is_empty(RingbackId) of
                  'true' -> 'infinity';
                  'false' -> ?DEFAULT_RINGBACK_TM
              end,
    lager:info("waiting for parked caller to be picked up or hangup"),
    whapps_call_command:hold(HoldMedia, Call),
    case whapps_call_command:wait_for_unparked_call(Call, Timeout) of
        {'error', 'timeout'} ->
            TmpCID = <<"Parking slot ", SlotNumber/binary>>,
            ChannelUp = case whapps_call_command:b_channel_status(Call) of
                            {'ok', _} -> 'true';
                            {'error', _} -> 'false'
                        end,
            case ChannelUp andalso ringback_parker(RingbackId, SlotNumber, TmpCID, Call) of
                'answered' ->
                    lager:info("parked caller ringback was answered"),
                    cf_exe:continue(Call);
                'failed' ->
                    lager:info("ringback was not answered, continuing to hold parked call"),
                    wait_for_pickup(SlotNumber, Slot, Call);
                _Else ->
                    lager:info("parked call doesnt exist anymore, hangup"),
                    _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call)),
                    cf_exe:stop(Call)
            end;
        {'error', 'channel_disconnected'} ->
            lager:info("parked caller has disconnected, checking status"),
            case whapps_call_command:b_channel_status(cf_exe:callid(Call)) of
                {'ok', _} ->
                    lager:info("call '~s' is still active", [cf_exe:callid(Call)]),
                    wait_for_pickup(SlotNumber, Slot, Call);
                _Else ->
                    lager:info("call '~s' is no longer active, ", [cf_exe:callid(Call)]),
                    _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call)),
                    cf_exe:transfer(Call)
            end;
        {'error', _} ->
            lager:info("parked caller has hungup"),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call)),
            cf_exe:transfer(Call);
        {'ok', _} ->
            lager:info("parked caller has been picked up"),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call)),
            cf_exe:transfer(Call)
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
-spec ringback_parker(api_binary(), ne_binary(), ne_binary(), whapps_call:call()) ->
                             'answered' | 'failed' | 'channel_hungup'.
ringback_parker('undefined', _, _, _) -> 'failed';
ringback_parker(EndpointId, SlotNumber, TmpCID, Call) ->
    case cf_endpoint:build(EndpointId, wh_json:from_list([{<<"can_call_self">>, 'true'}]), Call) of
        {'ok', Endpoints} ->
            lager:info("attempting to ringback endpoint ~s", [EndpointId]),
            OriginalCID = whapps_call:caller_id_name(Call),
            CleanUpFun = fun(_) ->
                                 lager:info("parking ringback was answered", []),
                                 _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call)),
                                 whapps_call:set_caller_id_name(OriginalCID, Call)
                         end,
            Call1 = whapps_call:set_caller_id_name(TmpCID, Call),
            whapps_call_command:bridge(Endpoints, ?DEFAULT_TIMEOUT_S, Call1),
            wait_for_ringback(CleanUpFun, Call1);
        _ -> 'failed'
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
    lager:info("update presence-id '~s' with state: ~s", [PresenceId, State]),
    wh_amqp_worker:cast(Command, fun wapi_presence:publish_update/1).

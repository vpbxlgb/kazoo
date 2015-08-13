%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_account_trial_upgrade).

-export([init/0
         ,handle_req/2
        ]).

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"account_trial_upgrade">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(?ACCOUNT_MACROS)
       ).

-define(TEMPLATE_TEXT, <<"">>).
-define(TEMPLATE_HTML, <<"">>).
-define(TEMPLATE_SUBJECT, <<"">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"Account Trial Upgrade">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
                                           ,{'text', ?TEMPLATE_TEXT}
                                           ,{'html', ?TEMPLATE_HTML}
                                           ,{'subject', ?TEMPLATE_SUBJECT}
                                           ,{'category', ?TEMPLATE_CATEGORY}
                                           ,{'friendly_name', ?TEMPLATE_NAME}
                                           ,{'to', ?TEMPLATE_TO}
                                           ,{'from', ?TEMPLATE_FROM}
                                           ,{'cc', ?TEMPLATE_CC}
                                           ,{'bcc', ?TEMPLATE_BCC}
                                           ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:account_trial_upgrade_v(JObj),

    wh_util:put_callid(JObj),
    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(wh_json:object()) -> 'ok'.
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    %% Load templates
    process_req(DataJObj, teletype_templates:fetch(?TEMPLATE_ID, DataJObj)).

process_req(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
process_req(DataJObj, Templates) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"admin">>, admin_user_properties(DataJObj)}
             ],

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_meta(?TEMPLATE_ID
                                      ,teletype_util:find_account_id(DataJObj)
                                     ),

    Subject =
        teletype_util:render_subject(
          wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
          ,Macros
         ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

admin_user_properties(DataJObj) ->
    AccountJObj = wh_json:get_value(<<"account">>, DataJObj),
    account_admin_user_properties(AccountJObj).

account_admin_user_properties(AccountJObj) ->
    AccountDb = wh_doc:account_db(AccountJObj),
    case couch_mgr:get_all_results(AccountDb, <<"users/crossbar_listing">>) of
        {'error', _E} ->
            lager:debug("failed to get user listing from ~s: ~p", [AccountDb, _E]),
            [];
        {'ok', Users} ->
            find_admin(Users)
    end.

find_admin([]) ->
    lager:debug("account has no admin users"),
    [];
find_admin([User|Users]) ->
    case wh_json:get_value([<<"value">>, <<"priv_level">>], User) of
        <<"admin">> -> admin_properties(wh_json:get_value(<<"value">>, User));
        _ -> find_admin(Users)
    end.

admin_properties(User) ->
    Ks = [<<"first_name">>, <<"last_name">>, <<"email">>, <<"timezone">>],
    [{K, wh_json:get_value(K, User)} || K <- Ks].

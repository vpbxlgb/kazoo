#!/bin/bash

SCRIPT_NAME=`basename "$0"`
pushd `dirname $0` > /dev/null
cd ../
KAZOO_ROOT=`pwd -P`
popd > /dev/null

function refactor_code() {
    if [[ $(grep -Rl "$1" * | grep -Ev "\.beam|.*\.log|$SCRIPT_NAME|\.md|\.git") ]]; then
        echo "Updating code '$1' -> '$2'"
        grep -Rl "$1" * | grep -Ev "\.beam|.*\.log|$SCRIPT_NAME|\.md|\.git" | xargs sed -i "s|$1|$2|g"
    fi
}

function rename_file() {
    if [[ $(find applications core -type f -name "$1*" | grep -v "$SCRIPT_NAME") ]]; then
        echo "Update file named '$1' -> '$2'"
        find applications core -type f -name "$1*" | grep -v "$SCRIPT_NAME" | xargs -n1 rename $1 $2
    fi
    while read i; do
        echo "===[ NOTICE: FILE RENAME MIGHT HAVE CREATED DUPLICATE MODULE NAMES! ]==="
        find applications core -type f -name `echo -n $i | awk '{print \$2}'` | sed 's|^|    |g'
    done < <(find applications core -type f -name "$2*" -exec basename {} \; | grep -Ev "\.md|Makefile|\.json|\.placeholder" | sort | uniq -c | grep -v "^[ \t]*1")
}

function util_split() {
    local ToModule=$1; shift
    local FNames=$*
    echo "Refactoring code to support '$ToModule'"
    for f in $FNames; do
        refactor_code "kz_util:$f" "$ToModule:$f"
    done
}

function rename_fname() {
    local From=$1
    local To=$2
    grep -Rl $From | xargs sed -i 's/'$From'(/'$To'(/g'
    grep -Rl $From | xargs sed -i 's%'$From'/%'$To'/%g'
}

function rename_module() {
    rename_file $1 $2
    refactor_code $1 $2
    refactor_code ${1^^} ${2^^}
    if [[ $(grep -Ri "$1" * | grep -Ev "beam.*\.log|$SCRIPT_NAME|\.md|\.git") ]]; then
        echo "===[ NOTICE: REFERENCES STILL EXIST FOR $1! ]==="
        grep -Ri "$1" * | grep -Ev "beam.*\.log|$SCRIPT_NAME|\.md|\.git" | grep -oi "$1" | sort | uniq -c
    fi
    commit_changes "renamed module $1 to $2"
}

function refactor_kz_term() {
    local kz_term_exports='always_false always_true binary_md5 ceiling clean_binary floor from_hex_binary from_hex_string hexencode_binary identity is_boolean is_empty is_false is_not_empty is_proplist is_true join_binary lcfirst_binary pad_binary pad_binary_left rand_hex_binary remove_white_spaces shuffle_list strip_binary strip_left_binary strip_right_binary suffix_binary to_atom to_binary to_boolean to_float to_hex to_hex_binary to_integer to_list to_lower_binary to_lower_string to_number to_upper_binary to_upper_string truncate_binary truncate_left_binary truncate_right_binary ucfirst_binary'
    util_split 'kz_term' $kz_term_exports
}

function refactor_kz_time() {
    local kz_time_exports='current_tstamp current_unix_tstamp decr_timeout elapsed_ms elapsed_s elapsed_us format_date format_datetime format_time gregorian_seconds_to_unix_seconds iso8601 microseconds_to_seconds milliseconds_to_seconds now now_ms now_s now_us pad_month pretty_print_datetime pretty_print_elapsed_s rfc1036 to_date to_datetime unix_seconds_to_gregorian_seconds unix_timestamp_to_gregorian_seconds'
    util_split 'kz_time' $kz_time_exports
}

function refactor_kz_accounts() {
    local kz_accounts_exports='account_update disable_account enable_account format_account_db format_account_id format_account_mod_id format_account_modb get_account_realm is_account_enabled is_account_expired is_in_account_hierarchy is_in_account_hierarchy is_system_admin is_system_db maybe_disable_account normalize_account_name set_allow_number_additions set_superduper_admin'
    util_split 'kz_accounts' $kz_accounts_exports
}

function refactor_kz_http_util() {
    local kz_http_util_from_kz_util='resolve_uri safe_urlencode uri uri_decode uri_encode'
    util_split 'kz_http_util' $kz_http_util_from_kz_util
}

function commit_changes() {
    if [[ $(git diff) ]]; then
##        read -rsp $'In another screen check the changes with \'git diff\', then press any key to commit...\n' -n1 key
        git add . 1> /dev/null
        git commit -m "4.0 refactor: $1" 1> /dev/null
    fi
}

cd $KAZOO_ROOT

commit_changes 'initial changes'

### Remove references to whistle, replaced with kazoo
rename_module whistle kazoo
rename_module whapps kapps
rename_module wapi kapi
rename_module wh_ kz_
rename_module whs_account_conversion kz_services_rearrange
rename_module wht_util kz_transactions_util
refactor_code Whistle/Kazoo
commit_changes 'removed references to whistle, replaced with kazoo'

### Avoice conflict with kazoo_documents when kzd_ is dropped
rename_module kz_service_ kz_services_
refactor_code kz_services: kz_services_mgr:
rename_file kz_services.er kz_services_mgr.er
commit_changes 'renamed module kz_service.erl to kz_services_mgr.erl'

### Move kapps_call/_command to core/kazoo
mv core/kazoo_apps/src/kapps_call* core/kazoo/src
refactor_code 'kazoo_apps/src/kapps_call_command_types' 'kazoo/src/kz_call_command_types' ## -includes
rename_module kapps_call_command_types kz_call_command_types
refactor_code 'kazoo_apps/src/kapps_call_command' 'kazoo/src/kz_call_command' ## -includes
rename_module kapps_call_command kz_call_command
rename_module kapps_call kz_call

### Move kapps_conference/_command to core/kazoo
mv core/kazoo_apps/src/kapps_conference* core/kazoo/src
rename_module kapps_conference_command kz_conference_command
rename_module kapps_conference kz_conference

### move kapps_sms to core/kazoo
mv core/kazoo_apps/src/kapps_sms* core/kazoo/src
rename_module kapps_sms_command kz_sms_command

### Standardize core libraries to kz_{LIBRARY}
rename_module kapps_ kz_apps_
rename_module kzsip_ kz_sip_
rename_module kapi_ kz_api_
rename_module amqp_util kz_amqp_util
rename_module kz_ip_utils kz_ips_utils
rename_module kazoo_oauth_util kz_oauth_util
rename_module kazoo_oauth_client kz_oauth_client
rename_module kz_tracers kz_pref_tracers
rename_module kz_buckets kz_token_buckets
rename_module kz_vm_message kz_voicemail_message

## Quick correction
rename_module kz_apps_maintenance kazoo_apps_maintenance

## rename_module kzs_ kz_data_
## rename_module kz_dataconfig kz_data_config ## move to kazoo_configs?
## rename_module kz_dataconnection_sup kz_data_connection_sup
## rename_module kz_dataconnections kz_data_connections
## rename_module kz_dataconnection kz_data_connection
## rename_module kz_datamgr kz_data_mgr

### Don't be lazy, consistency FTW
rename_module kzt_ kz_translator_
rename_module kzt kz_translator
rename_module kzl kazoo_ledgers

### Consistency in the core, kz_{LIBRARY} but applied to the config modules
refactor_code kz_config: kz_config_ini:
rename_file kz_config.er kz_config_ini.er
commit_changes 'renamed module kz_config.erl to kz_config_ini.erl'
rename_module kz_apps_account_config kz_config_account
rename_module kz_apps_config kz_config_db

### Inconsistency... kazoo_documents is special, allow kz_ only....
rename_module kzd_ kz_

### Move kz_apps_speech to kazoo_media
mv core/kazoo_apps/src/kz_apps_speech* core/kazoo_media/src
sed -i 's|kazoo_apps.hrl|kazoo_media.hrl|g' core/kazoo_media/src/kz_apps_speech.erl
rename_module kz_apps_speech kz_media_speech

rename_module knm_config kz_number_config
rename_module knm_errors kz_number_errors
rename_module knm_gen_carrier kz_gen_carrier
rename_module knm_gen_provider kz_gen_provider
rename_module knm_locality kz_number_locality
rename_module knm_maintenance kazoo_number_maintenance
rename_module knm_number_crawler kz_number_crawler
rename_module knm_number kz_number
rename_module knm_number_options kz_number_options
rename_module knm_numbers kz_numbers
rename_module knm_number_states kz_number_states
rename_module knm_phone_number kz_phone_number
rename_module knm_port_request_crawler kz_port_request_crawler
rename_module knm_port_request kz_port_request
rename_module knm_services kz_number_services

### Rename the number manager modules 
cd $KAZOO_ROOT/core/kazoo_number_manager/src/carriers
for carrier in `ls -1 ./*`; do
    carrier_name=`basename ${carrier%.erl}`
    if [ "$carrier_name" != "knm_carriers" ]; then
        rename knm_ kz_carrier_ $carrier
    else
        rename knm_ kz_ $carrier
    fi
    commit_changes "renamed module $carrier_name kz_carrier_${carrier_name#knm_}"
done

cd $KAZOO_ROOT/core/kazoo_number_manager/src/providers
for provider in `ls -1 ./*`; do
    provider_name=`basename ${provider%.erl}`
    if [ "$provider_name" != "knm_providers" ]; then
        rename knm_ kz_provider_ $provider
    else
        rename knm_ kz_ $provider
    fi
    commit_changes "renamed module $provider_name kz_provider_${provider_name#knm_}"
done
cd $KAZOO_ROOT
rename_module knm_vitelity_util kz_vitelity_util
rename_module knm_vitelity kz_vitelity
rename_module knm_util kz_number_util
rename_module knm_sip kz_number_sip
rename_module knm kz_number
rename_module kazoo_number_manager kazoo_number
mv $KAZOO_ROOT/core/kazoo_number_manager $KAZOO_ROOT/core/kazoo_number

### Break up wh_util/kz_util for better intent and a stronger separation of concerns
#refactor_kz_term
#refactor_kz_time
#refactor_kz_accounts
#refactor_kz_http_util
#rename_fname kz_util:get_xml_value kz_xml:value

## Other fixes
sed -i '/kazoo_apps.hrl/d' core/kazoo_apps/src/kazoo_apps_sup.erl
sed -i 's|kz_call_command.hrl|kazoo_apps.hrl|g' core/kazoo_apps/src/kazoo_apps_sup.erl
sed -i 's|kz_config|kz_config_ini|g' core/kazoo_config/src/kz_config_ini.erl
sed -i 's|-module(kz_services)|-module(kz_services_mgr)|g' core/kazoo_services/src/kz_services_mgr.erl

## TODO:
## try kz_translator_translator:exec(Call, kz_util:to_list(RespBody), CT) of
## amqp_util:kapps_publish( -> amqp_util:kz_apps_publish(
##   and  amqp_util:kapps_exchange() -> amqp_util:kz_apps_exchange()
## Double check the configs

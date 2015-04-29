%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kzd_services).

-export([billing_id/1, billing_id/2
         ,is_reseller/1, is_reseller/2
         ,reseller_id/1, reseller_id/2
         ,is_dirty/1, is_dirty/2
         ,status/1, status/2
         ,tree/1, tree/2
         ,type/0, type/1
         ,plans/1, plans/2
         ,quantities/1, quantities/2
        ]).

-export([set_billing_id/2
         ,set_is_reseller/2
         ,set_resller_id/2
         ,set_is_dirty/2
         ,set_status/2
         ,set_tree/2
         ,set_type/1
         ,set_plans/2
         ,set_quantities/2
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().

-define(BILLING_ID, <<"billing_id">>).
-define(IS_RESELLER, <<"pvt_reseller">>).
-define(RESELLER_ID, <<"pvt_reseller_id">>).
-define(IS_DIRTY, <<"pvt_dirty">>).
-define(STATUS, <<"pvt_status">>).
-define(STATUS_GOOD, <<"good_standing">>).
-define(TREE, <<"pvt_tree">>).
-define(TYPE, <<"service">>).
-define(PLANS, <<"plans">>).
-define(QUANTITIES, <<"quantities">>).

-spec billing_id(doc()) -> api_binary().
-spec billing_id(doc(), Default) -> ne_binary() | Default.
billing_id(JObj) ->
    billing_id(JObj, 'undefined').
billing_id(JObj, Default) ->
    wh_json:get_value(?BILLING_ID, JObj, Default).

-spec is_reseller(doc()) -> boolean().
-spec is_reseller(doc(), Default) -> boolean() | Default.
is_reseller(JObj) ->
    is_reseller(JObj, 'false').
is_reseller(JObj, Default) ->
    wh_json:is_true(?IS_RESELLER, JObj, Default).

-spec reseller_id(doc()) -> api_binary().
-spec reseller_id(doc(), Default) -> ne_binary() | Default.
reseller_id(JObj) ->
    reseller_id(JObj, 'undefined').
reseller_id(JObj, Default) ->
    wh_json:get_value(?RESELLER_ID, JObj, Default).

-spec is_dirty(doc()) -> boolean().
-spec is_dirty(doc(), Default) -> boolean() | Default.
is_dirty(JObj) ->
    is_dirty(JObj, 'false').
is_dirty(JObj, Default) ->
    wh_json:is_true(?IS_DIRTY, JObj, Default).

-spec status(doc()) -> ne_binary().
-spec status(doc(), Default) -> ne_binary() | Default.
status(JObj) ->
    status(JObj, ?STATUS_GOOD).
status(JObj, Default) ->
    wh_json:get_value(?STATUS, JObj, Default).

-spec tree(doc()) -> ne_binaries().
-spec tree(doc(), Default) -> ne_binaries() | Default.
tree(JObj) ->
    tree(JObj, []).
tree(JObj, Default) ->
    wh_json:get_value(?TREE, JObj, Default).

type() -> ?TYPE.
type(JObj) ->
    wh_doc:pvt_type(JObj, ?TYPE).

-spec plans(doc()) -> wh_json:object().
-spec plans(doc(), Default) -> wh_json:object() | Default.
plans(JObj) ->
    plans(JObj, wh_json:new()).
plans(JObj, Default) ->
    wh_json:get_json_value(?PLANS, JObj, Default).

-spec quantities(doc()) -> wh_json:object().
-spec quantities(doc(), Default) -> wh_json:object() | Default.
quantities(JObj) ->
    quantities(JObj, wh_json:new()).
quantities(JObj, Default) ->
    wh_json:get_json_value(?QUANTITIES, JObj, Default).

-spec set_billing_id(doc(), api_binary()) -> doc().
set_billing_id(JObj, BillingId) ->
    wh_json:set_value(?BILLING_ID, BillingId, JObj).

-spec set_is_reseller(doc(), boolean()) -> doc().
set_is_reseller(JObj, IsResller) ->
    wh_json:set_value(?IS_RESELLER, IsResller, JObj).

-spec set_resller_id(doc(), api_binary()) -> doc().
set_resller_id(JObj, ResellerId) ->
    wh_json:set_value(?RESELLER_ID, ResellerId, JObj).

-spec set_is_dirty(doc(), boolean()) -> doc().
set_is_dirty(JObj, IsDirty) ->
    wh_json:set_value(?IS_DIRTY, IsDirty, JObj).

-spec set_status(doc(), api_binary()) -> doc().
set_status(JObj, Status) ->
    wh_json:set_value(?STATUS, Status, JObj).

-spec set_tree(doc(), ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    wh_json:set_value(?TREE, Tree, JObj).

-spec set_type(doc()) -> doc().
set_type(JObj) ->
    wh_doc:set_pvt_type(JObj, ?TYPE).

-spec set_plans(doc(), wh_json:object()) -> doc().
set_plans(JObj, Plans) ->
    wh_json:set_value(?PLANS, Plans, JObj).

-spec set_quantities(doc(), wh_json:object()) -> wh_json:object().
set_quantities(JObj, Quantities) ->
    wh_json:set_value(?QUANTITIES, Quantities, JObj).

%%%-------------------------------------------------------------------
%%% @author cdmaji1
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. ÁùÔÂ 2015 15:19
%%%-------------------------------------------------------------------
-module(kzt_xpass_util).
-author("cdmaji1").

%% API
-export([get_default_voice/0,
		get_lang/1,
		get_engine/1,
		loop_count/1,
		get_ask_subactions/1,
		action_url/1]).

-spec get_default_voice() -> atom().
get_default_voice() ->
	'undefined'.



-spec get_lang(list()) -> binary().
get_lang(Props) ->
	case props:get_binary_value('language', Props) of
		'undefined' -> <<"zh-cn">>;
		<<"en">> -> <<"en-US">>;
		<<"en-gb">> -> <<"en-GB">>;
		<<"es">> -> <<"es">>;
		<<"fr">> -> <<"fr">>;
		<<"de">> -> <<"de">>;
		<<"it">> -> <<"it">>
	end.


-spec get_engine(list()) -> binary().
get_engine(Props) ->
	case props:get_binary_value('engine', Props) of
		'undefined' -> <<"flite">>;
		Engine -> Engine
	end.

-spec loop_count(list()) -> integer().
loop_count(Props) -> props:get_integer_value('loop', Props, 1).


get_ask_subactions(Args)->
	SayContents = wh_json:get_value(<<"say">>, Args),
	SubActions = [wh_json:from_list([{<<"say">>, [El]}]) || El <- SayContents],
	SubActions.


-spec action_url(whapps_call:call()) -> binary().
action_url(Call) -> kzt_util:get_voice_uri(Call).
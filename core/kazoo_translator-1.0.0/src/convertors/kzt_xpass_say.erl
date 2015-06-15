%%%-------------------------------------------------------------------
%%% @author cdmaji1
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. ÁùÔÂ 2015 15:08
%%%-------------------------------------------------------------------
-module(kzt_xpass_say).
-author("cdmaji1").

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").


%% API
-export([exec/3]).


exec(Call, <<"say">>, Args) ->
	lager:debug("execute say, arg: ~p", [Args]),
	whapps_call_command:answer(Call),
	SayMe = wh_json:get_value(<<"value">>, Args),
	Voice = kzt_xpass_util:get_default_voice(),
	Lang = kzt_xpass_util:get_lang([]),
	Engine = kzt_xpass_util:get_engine([]),
	Terminators = wapi_dialplan:terminators(undefined),

	lager:info("SAY: '~s' using voice ~s, in lang ~s, and engine ~s", [SayMe, Voice, Lang, Engine]),
	%%default is 1 times
	case kzt_xpass_util:loop_count([]) of
		0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, 'infinity');
		N when N > 0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, N)
	end.
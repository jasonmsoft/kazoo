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

%% API
-export([exec/3]).


exec(Call, XmlText, Attrs) ->
	whapps_call_command:answer(Call),
	SayMe = kz_xml:texts_to_binary(XmlText, whapps_config:get_integer(<<"pivot">>, <<"tts_texts_size">>, ?TTS_SIZE_LIMIT)),

	Props = kz_xml:attributes_to_proplist(Attrs),

	Voice = kzt_twiml_util:get_voice(Props),
	Lang = kzt_twiml_util:get_lang(Props),
	Engine = kzt_twiml_util:get_engine(Props),

	Terminators = kzt_twiml_util:get_terminators(Props),

	lager:info("SAY: '~s' using voice ~s, in lang ~s, and engine ~s", [SayMe, Voice, Lang, Engine]),

	case kzt_twiml_util:loop_count(Props) of
		0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, 'infinity');
		N when N > 0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, N)
	end.
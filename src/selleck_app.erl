-module(selleck_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ok, Port} = application:get_env(selleck, port),
	Dispatch = cowboy_router:compile([
    {'_', [
      {"/:product/:page", [], template_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(my_http_listener, 100,
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]
  ),
	selleck_sup:start_link().

stop(_State) ->
	ok.

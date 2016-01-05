-module(template_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {PageBinding, Req2} = cowboy_req:binding(page, Req),
  {ProductBinding, Req3} = cowboy_req:binding(product, Req2),
	{ok, Body, Req4} = cowboy_req:body(Req3),
	
  Output = process_template(code:priv_dir(selleck), ProductBinding, PageBinding, Body),
  
	{ok, Req5} = output(Output, Req4),
	{ok, Req5, State}.

terminate(_Reason, _Req, _State) ->
	ok.
  
output(file_not_found, Req) ->
  cowboy_req:reply(404, [{<<"content-type">>, <<"text/html">>}], "File Not Found\n", Req);
output(no_product, Req) ->
  cowboy_req:reply(404, [{<<"content-type">>, <<"text/html">>}], "No Such Product\n", Req);
output(no_template, Req) ->
  cowboy_req:reply(404, [{<<"content-type">>, <<"text/html">>}], "No Such Template\n", Req);
output(Data, Req) ->
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Data, Req).
  
get_template_filename({error, bad_name}, Product, TemplateName) ->
  get_template_filename("priv/", Product, TemplateName);
get_template_filename(BaseDir, Product, TemplateName) ->
  FName = BaseDir ++ "/templates/" ++ binary_to_list(Product) ++ "/" ++ binary_to_list(TemplateName) ++ ".mustache",
  case file:read_file_info(FName) of
    {ok, _} -> FName;
    _ -> file_not_found
  end.
  
get_template(BaseDir, Product, TemplateName) ->
  FileName = get_template_filename(BaseDir, Product, TemplateName),
  case FileName of
    file_not_found -> file_not_found;
    _ -> bbmustache:parse_file(FileName)
  end.
  
output_template(file_not_found, _) ->
  file_not_found;
output_template(Template, Data) ->
  bbmustache:compile(Template, Data, [{key_type, binary}]).
  
process_template(_, undefined, _, _) ->
  no_product;
process_template(_, _, undefined, _) ->
  no_template;
process_template(BaseDir, Product, TemplateName, <<>>) ->
  T = get_template(BaseDir, Product, TemplateName),
  output_template(T, []);
process_template(BaseDir, Product, TemplateName, Data) ->
  Results = jiffy:decode(Data, [return_maps]),
  T = get_template(BaseDir, Product, TemplateName),
  output_template(T, Results).
  
%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
get_template_test_() ->
  {"Get Template Tests",
    [
      { "File Not Found", ?_assertMatch(file_not_found, get_template("priv/", <<"test">>,<<"bogus_template_name">>))},
      { "Test File Found", ?_assertMatch({bbmustache, _}, get_template("priv/", <<"test">>,<<"test">>)) }
    ]
  }.
  
output_template_test_() ->
  {"Output Template Tests",
    [{"File Not Found", ?_assertMatch(file_not_found, output_template(file_not_found, {}))}]
  }.  

process_template_test_() ->
  {"Process Template Tests",
    [
      {"No Product", ?_assertMatch(no_product, process_template("priv/", undefined, <<"test">>, <<>>))},
      {"No Product, No Template", ?_assertMatch(no_product, process_template("priv/", undefined, undefined, <<>>))},
      {"No Template", ?_assertMatch(no_template, process_template("priv/", <<"test">>, undefined, <<>>))},
      {"File Not Found", ?_assertMatch(file_not_found, process_template("priv/", <<"test">>, <<"bogus_template_name">>, <<>>))}
    ]
  }.

-endif.

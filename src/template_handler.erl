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
	
  Output = process_template(ProductBinding, PageBinding, Body),
  
	{ok, Req5} = output(Output, Req4),
	{ok, Req5, State}.

terminate(_Reason, _Req, _State) ->
	ok.
  
output(no_product, Req) ->
  cowboy_req:reply(404, [{<<"content-type">>, <<"text/html">>}], "Not Such Product\n", Req);
output(no_template, Req) ->
  cowboy_req:reply(404, [{<<"content-type">>, <<"text/html">>}], "Not Such Template\n", Req);
output(Data, Req) ->
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Data, Req).
  
get_template(Product, TemplateName) ->
  FileName = code:priv_dir(selleck) ++ "/templates/" ++ binary_to_list(Product) ++ "/" ++ binary_to_list(TemplateName) ++ ".mustache",
  bbmustache:parse_file(FileName).
  
process_template(undefined, _, _) ->
  no_product;
process_template(_, undefined, _) ->
  no_template;
process_template(Product, TemplateName, <<>>) ->
  OutputTemplate = get_template(Product, TemplateName),
  bbmustache:compile(OutputTemplate, []);
process_template(Product, TemplateName, Data) ->
  OutputTemplate = get_template(Product, TemplateName),
  Results = jiffy:decode(Data, [return_maps]),
  bbmustache:compile(OutputTemplate, Results, [{key_type, binary}]).

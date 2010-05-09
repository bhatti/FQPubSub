%%%-------------------------------------------------------------------
%%% File    : web_server.erl
%%% Author  : Shahzad Bhatti
%%% Purpose : Web Server using Yaws
%%% Created : May 8, 2010 
%%%-------------------------------------------------------------------

-module(web_server).
       
-author('bhatti@plexobject.com').

-include("quote_utils.hrl").
-include("deps/yaws/include/yaws.hrl").

-export([start/0, stop/0]).


start() ->
    code:add_path("yaws/ebin"),
    application:set_env(yaws, embedded, true),
    yaws:start_embedded("docroot", [{servername, ?TEST_XMPP_SERVER}, {listen, {0,0,0,0}}]).

stop() ->
    application:stop(yaws),
    ok.

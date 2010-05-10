%%%-------------------------------------------------------------------
%%% File    : quote_utils.erl
%%% Author  : Shahzad Bhatti 
%%% Purpose : Utility functions for connection and session management with Bosh/XMPP
%%% Created : May 8, 2010 
%%%-------------------------------------------------------------------
-module(quote_utils).
  
-author('bhatti@plexobject.com').

-include_lib("quote_utils.hrl").

-export([
    init_session/2, 
    connect/4, 
    disconnect/1]).

bosh_url(Host, Port) ->
    "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/http-bind".


connect(Host, _Port, User, Password) ->
    safe_start_apps(),
    MySession = exmpp_session:start({1,0}),
    exmpp_xml:start_parser(), %% Create XMPP ID (Session Key):
    MyJID = exmpp_jid:make(User, Host, random),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    %% Connect in standard TCP:
    %_StreamId = exmpp_session:connect_TCP(MySession, Host, Port),
    {ok, _StreamId, _Features} = exmpp_session:connect_BOSH(MySession, bosh_url(Host, 5280), Host, []),
    try quote_utils:init_session(MySession, Password)
    catch
        _:Error -> io:format("got error: ~p~n", [Error]), {error, Error}
    end,
    {ok, {MySession, MyJID}}.

init_session(MySession, Password) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession, "PLAIN")
    catch
        throw:{auth_error, 'not-authorized'} ->
        %% Try creating a new user:
        io:format("Register~n",[]),
        %% In a real life client, we should trap error case here
        %% and print the correct message.
        exmpp_session:register_account(MySession, Password),
        %% After registration, retry to login:
        exmpp_session:login(MySession)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession, exmpp_presence:set_status(exmpp_presence:available(), "Ready to publish!!!")),
    ok.

disconnect(MySession) ->
    exmpp_session:stop(MySession).

safe_start_apps() ->
    try start_apps()
    catch
        _:Error -> io:format("apps already started : ~p~n", [Error]), {error, Error}
    end.

start_apps() ->
    ok = application:start(exmpp),
    ok = application:start(crypto),
    ok = application:start(ssl),
    ok = application:start(lhttpc).

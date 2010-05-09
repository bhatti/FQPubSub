%%%-------------------------------------------------------------------
%%% File    : quote_client.erl
%%% Author  : Shahzad Bhatti
%%% Purpose : OTP server for subscribing to quotes
%%% Created : May 8, 2010
%%%-------------------------------------------------------------------
-module(quote_client).

-author('bhatti@plexobject.com').

-behaviour(gen_server).

-export([
    start/0, 
    start/4, 
    start_link/4, 
    stop/0]).

%% gen_server callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    terminate/2, 
    code_change/3]).
  
% API
-export([
    subscribe/1, 
    subscribe/2]).

-include_lib("quote_utils.hrl").

-record(state, {session, jid, on_message}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subscribe(Symbol) ->
    subscribe(?TEST_XMPP_PUBSUB, Symbol).

subscribe(Service, SymbolNode) ->
    gen_server:call(?MODULE, {subscribe, Service, SymbolNode}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lifecycle APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    start(?TEST_XMPP_SERVER, ?TEST_XMPP_PORT, ?CONSUMER_USERNAME, ?CONSUMER_PASSWORD).

start(Host, Port, User, Password) ->
  gen_server:start({local, ?MODULE}, ?MODULE, {Host, Port, User, Password}, []).

start_link(Host, Port, User, Password) -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, {Host, Port, User, Password}, []).
  
stop() ->
  gen_server:cast(?MODULE, stop).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({Host, Port, User, Password}) ->
    {ok, {MySession, MyJID}} = quote_utils:connect(Host, Port, User, Password),
    Fun = fun(_XML_Element, Child) -> 
      case exmpp_xml:get_element(Child, ?QUOTE_DATA) of
        undefined -> not_a_notification;
        Quote -> 
          io:format("#### Got quote : ~s~n", [exmpp_xml:get_cdata_as_list(Quote)])
      end
    end,
    {ok, #state{session=MySession, jid=MyJID, on_message=Fun}}.
  

handle_call({subscribe, Service, Node}, _From, #state{session=MySession, jid=MyJID}=State) ->
    IQ = exmpp_client_pubsub:subscribe(exmpp_jid:to_list(MyJID), Service, Node),
    PacketId = exmpp_session:send_packet(MySession, exmpp_stanza:set_sender(IQ, MyJID)),
    PacketId2 = erlang:binary_to_list(PacketId),
    Reply = receive#received_packet{id=PacketId2, raw_packet=Raw} ->
        case exmpp_iq:is_error(Raw) of
            true -> error;
            _ -> ok
          end
    end,
    io:format("###################Reply XML ~p~n", [Raw]),
    {reply, Reply, State};
  
handle_call(_Request, _From, State) -> 
    {reply, ok, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.


handle_info(#received_packet{packet_type='message'}=Packet, #state{on_message=Fun}=State) ->
    process_received_packet(Packet, Fun),
    {noreply, State};
handle_info(_Info, State) -> 
    {noreply, State}.


terminate(_Reason, #state{session=MySession}) -> 
    quote_utils:disconnect(MySession),
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

process_received_packet(#received_packet{raw_packet=Raw}, Fun) ->
    io:format("!!!!!RECEIVED RAW XML ~p~n", [exmpp_xml:node_to_list(Raw, "", "")]),
    Event = exmpp_xml:get_element(Raw, ?NS_PUBSUB_EVENT, 'event'),
    Items = exmpp_xml:get_element(Event, 'items'),
    exmpp_xml:foreach(Fun, Items),
    ok.


%%%-------------------------------------------------------------------
%%% File    : quote_publisher.erl
%%% Author  : Shahzad Bhatti
%%% Purpose : OTP server for publishing quotes
%%% Created : May 8, 2010 
%%%-------------------------------------------------------------------
-module(quote_publisher).

-author('bhatti@plexobject.com').

-export([
    start/1, 
    start/5, 
    stop/1]).

-export([init/5]).

-include_lib("quote_utils.hrl").

-record(state, {session, jid, service=?TEST_XMPP_PUBSUB, symbol}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Symbol) ->
    start(?TEST_XMPP_SERVER, ?TEST_XMPP_PORT, ?PRODUCER_USERNAME,
        ?PRODUCER_PASSWORD, Symbol).

start(Host, Port, User, Password, Symbol) ->
    spawn(?MODULE, init, [Host, Port, User, Password, Symbol]).

stop(Pid) ->
    Pid ! stop.
  
init(Host, Port, User, Password, Symbol) ->
    {ok, {MySession, MyJID}} = quote_utils:connect(Host, Port, User, Password),
    State = #state{session=MySession, jid=MyJID, symbol = Symbol},
    create_symbol_node(State),
    loop(State).

loop(#state{session=MySession, jid=_MyJID, service = _Service,
        symbol = _Symbol}=State) -> 
    receive
        stop ->
            quote_utils:disconnect(MySession);
        Record = #received_packet{packet_type=message, raw_packet=_Packet} ->
            io:format("publisher received packet : ~p~n", [Record]),
            loop(State);
        Record ->
            io:format("publisher received unknown packet ~p~n", [Record]),
            loop(State)
    after 2000 ->
        publish_quote(State),
        loop(State)
    end.

create_symbol_node(#state{session=MySession, jid=MyJID, service = Service,
        symbol = Symbol}) -> 
    IQ = exmpp_client_pubsub:create_node(Service, Symbol),
    PacketId = exmpp_session:send_packet(MySession, exmpp_stanza:set_sender(IQ, MyJID)),
    PacketId2 = erlang:binary_to_list(PacketId),
    receive #received_packet{id=PacketId2, raw_packet=Raw} ->
      case exmpp_iq:is_error(Raw) of
        true -> {error, Raw};
        _ -> ok
      end
    end.
  
publish_quote(#state{session=MySession, jid=MyJID, service = Service, symbol = Symbol}) ->
    Quote = yquote_client:quote(Symbol), 
    JsonQuote = ?record_to_json(quote, Quote),
    M = exmpp_xml:element(?QUOTE_DATA),
    IQ = exmpp_client_pubsub:publish(Service, Symbol, exmpp_xml:append_cdata(M,
            JsonQuote)),
    Xml = exmpp_stanza:set_id(exmpp_stanza:set_sender(IQ, MyJID), Symbol),
    io:format("Sending : ~p~n", [exmpp_xml:node_to_list(Xml, "http://jabber.org/protocol/pubsub", "")]),
    PacketId = exmpp_session:send_packet(MySession, exmpp_stanza:set_sender(IQ, MyJID)),
    PacketId2 = erlang:binary_to_list(PacketId),
    receive #received_packet{id=PacketId2, raw_packet=Raw} ->
      io:format("######Got Packet: ~s~n", [exmpp_xml:document_to_iolist(Raw)]),
      case exmpp_iq:is_error(Raw) of
        true -> error;
        _ -> ok
      end
    end.

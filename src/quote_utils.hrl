-include_lib("yquote_client.hrl").
-include_lib("jsonerl.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%%%%%%%%%
%%% TODO - Replace paraclete.local with your hostname
%%%%%%%%%
-define(TEST_XMPP_SERVER, "paraclete.local").
-define(TEST_XMPP_PUBSUB, "pubsub." ++ ?TEST_XMPP_SERVER).
%%%%%%%%%
-define(TEST_XMPP_PORT, 5222).
-define(PRODUCER_USERNAME, "producer").
-define(PRODUCER_PASSWORD, "producer").
-define(CONSUMER_USERNAME, "consumer").
-define(CONSUMER_PASSWORD, "consumer").
-define(QUOTE_DATA, "data").

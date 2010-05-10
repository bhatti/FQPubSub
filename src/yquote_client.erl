%%%-------------------------------------------------------------------
%%% File    : yquote_client.erl
%%% Author  : Shahzad Bhatti
%%% Purpose : Wrapper Library for Yahoo Stock Quotes
%%% Created : May 8, 2010 
%%%-------------------------------------------------------------------

-module(yquote_client).

-author('bhatti@plexobject.com').

-export([
         quote/1
        ]).

-include_lib("yquote_client.hrl").

quote(Symbol) ->
    inets:start(),
    {ok,{_Status, _Headers, Response}} = http:request(get, {url(Symbol), []},
        [{timeout, 5000}], [{sync, true}]),

    Values = re:split(Response, "[,\r\n]"),
    #quote{
        price = to_float(lists:nth(1, Values)), 
        change = to_float(lists:nth(2, Values)), 
        volume = to_integer(lists:nth(3, Values)), 
        avg_daily_volume = to_integer(lists:nth(4, Values)), 
        stock_exchange = to_string(lists:nth(5, Values)),
        market_cap = to_float(lists:nth(6, Values)),  % B
        book_value = to_float(lists:nth(7, Values)), 
        ebitda = to_float(lists:nth(8, Values)),  % B
        dividend_per_share = to_float(lists:nth(9, Values)), 
        dividend_yield = to_float(lists:nth(10, Values)), 
        earnings_per_share = to_float(lists:nth(11, Values)), 
        week_52_high = to_float(lists:nth(12, Values)), 
        week_52_low = to_float(lists:nth(13, Values)), 
        day_50_moving_avg = to_float(lists:nth(14, Values)),
        day_200_moving_avg = to_float(lists:nth(15, Values)), 
        price_earnings_ratio = to_float(lists:nth(16, Values)), 
        price_earnings_growth_ratio = to_float(lists:nth(17, Values)),
        price_sales_ratio = to_float(lists:nth(18, Values)), 
        price_book_ratio = to_float(lists:nth(19, Values)), 
        short_ratio = to_float(lists:nth(20, Values))}.

url(Symbol) ->
    "http://finance.yahoo.com/d/quotes.csv?s=" ++ Symbol ++ "&f=l1c1va2xj1b4j4dyekjm3m4rr5p5p6s7".

to_float(<<"N/A">>) ->
    -1;
to_float(Bin) ->
    {Multiplier, Bin1} = case bin_ends_with(Bin, <<$B>>) of
        true ->
            {1000000000, bin_replace(Bin, <<$B>>, <<>>)};
        false ->
            case bin_ends_with(Bin, <<$M>>) of
                true ->
                    {1000000, bin_replace(Bin, <<$M>>, <<>>)};
                false ->
                    {1,Bin}
            end
    end,
    L = binary_to_list(Bin1),
    list_to_float(L) * Multiplier.

to_integer(Bin) ->
    L = binary_to_list(Bin),
    list_to_integer(L).

to_string(Bin) ->
    Bin1 = bin_replace(Bin, <<$">>, <<>>),
    binary_to_list(Bin1).

bin_replace(What, From, To) ->
    bin_replace(What, erlang:byte_size(From), What, From, To, <<>>).
   
bin_replace(WhatOrig, FromLen, What, From, To, Acc) ->
    case What of
        <<From:FromLen/binary, Other/binary>> ->
            OtherRepl = bin_replace(Other, From, To),
            <<Acc/binary, To/binary, OtherRepl/binary>>;
       
        <<Char:8, Other/binary>> ->
            bin_replace(WhatOrig, FromLen, Other, From, To, <<Acc/binary,Char>>);
       
        <<>> ->
            WhatOrig
    end. 

bin_ends_with(Suffix, Suffix) when is_binary(Suffix) ->
    true;
bin_ends_with(<<>>, _Suffix) ->
    false;
bin_ends_with(What, Suffix) when is_binary(What), is_binary(Suffix), erlang:byte_size(What) < erlang:byte_size(Suffix) ->
    false;
bin_ends_with(<<_:8,Rest/binary>>, Suffix) when is_binary(Suffix) ->
    bin_ends_with(Rest, Suffix).

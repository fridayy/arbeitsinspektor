%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2023, leftshift.one software gmbh
%%% @doc
%%% 
%%% @end
%%% Created : 30. Jan 2023
%%%-------------------------------------------------------------------
-module(aicore_config).

-author("bnjm").

-export([get_env/1]).
-export([get_env/2]).
-export([get_env_ensure/2]).
-export([get_env_ensure/3]).

-type options() :: dry_run.

-spec get_env(Key, Default) -> term() | Default when
    Key :: options(),
    Default :: term().
get_env(Key, Default) ->
    case application:get_env(aicore, Key) of
        undefined -> Default;
        %% an empty string is considered as an absence
        {ok, []} -> Default;
        {ok, Result} -> Result
    end.

-spec get_env(Key) -> term() when
    Key :: options().
get_env(Key) ->
    case get_env(Key, undefined) of
        undefined -> error({not_set, Key});
        Else -> Else
    end.


%% @doc
%% Ensures the returned result is transformed into the given datatype.
%% @end
-spec get_env_ensure(Key, DataType) -> Result when
    Key :: atom(),
    DataType :: integer | string | atom | binary,
    Result :: integer() | string() | atom() | binary() | undefined.
get_env_ensure(Key, DataType) -> do_ensure(get_env(Key), DataType).
get_env_ensure(Key, Default, DataType) ->
    do_ensure(get_env(Key, Default), DataType).

do_ensure(Value, integer) when is_list(Value) -> erlang:list_to_integer(Value);
do_ensure(Value, integer) when is_integer(Value) -> Value;
do_ensure(Value, binary) when is_list(Value) -> erlang:list_to_binary(Value);
do_ensure(Value, string) when is_list(Value) -> Value;
do_ensure(Value, string) when is_binary(Value) -> erlang:binary_to_list(Value);
do_ensure(<<"true">>, boolean) -> true;
do_ensure(<<"false">>, boolean) -> false;
do_ensure("true", boolean) -> true;
do_ensure("false", boolean) -> false;
do_ensure(true, boolean) -> true;
do_ensure(false, boolean) -> false;
do_ensure(X, boolean) -> error({not_convertible, X, boolean}).

%% should be used with care / in tests only
put_env(Key, Value) ->
    application:set_env(aksnth, Key, Value).

all() -> application:get_all_env(aksnth).

-spec is_defined(Key) -> Result when
    Key :: atom(),
    Result :: boolean().
is_defined(Key) ->
    case get_env(Key, undefined) of
        undefined -> false;
        _ -> true
    end.

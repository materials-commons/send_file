%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc Setup allowed users and directories.
%%%
%%% @copyright Copyright (c) 2013, Regents of the University of Michigan.
%%% All rights reserved.
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%% ===================================================================
-module(sf_allowed).

%% API
-export([new/1, authenticate/3, access_allowed/3]).

-record(allowed,
        {
            users :: dict() | all
        }).

-record(access_control,
        {
            directories :: list(),
            pw_hash :: string()
        }).

%%%===================================================================
%%% API
%%%===================================================================

new(all) ->
    #allowed{users = all};
new(FilePath) ->
    {ok, [Terms]} = file:consult(FilePath),
    Dict = lists:foldl(
                fun ({Username, _Dirs, _PwHash} = Item, Dict) ->
                        dict:store(Username, new_access_control(Item), Dict);
                    ({Username, _Dirs} = Item, Dict) ->
                        dict:store(Username, new_access_control(Item), Dict)
                end, dict:new(), Terms),
    #allowed{users = Dict}.

authenticate(Username, Password, #allowed{} = Allowed) ->
    case get_user_pw_hash(Username, Allowed) of
        no_such_user -> false;
        Hash -> bcrypt:hashpw(Password, Hash) =:= {ok, Hash}
    end.

access_allowed(Username, Directory,
        #allowed{users = Users}) when Users =:= all ->
    Directory =:= handyuser:user_home(Username) orelse
        Directory =:= "/tmp";
access_allowed(Username, Directory, #allowed{users = Users}) ->
    case dict:find(Username, Users) of
        {ok, {Directories, _PwHash}} -> check_access(Username, Directory, Directories);
        error -> false
    end.

%%%===================================================================
%%% Local
%%%===================================================================

new_access_control({_Username, Directories, PwHash}) ->
    #access_control{directories = Directories, pw_hash = PwHash};
new_access_control({_Username, Directories}) ->
    #access_control{directories = Directories, pw_hash = []}.

get_user_pw_hash(Username, #allowed{users = Users}) when Users =:= all ->
    pw_hash_from_user_home(Username);
get_user_pw_hash(Username, #allowed{users = Users}) ->
    case dict:find(Username, Users) of
        {ok, {_Directories, []}} -> pw_hash_from_user_home(Username);
        {ok, {_Directories, PwHash}} -> PwHash;
        error -> no_such_user
    end.

pw_hash_from_user_home(Username) ->
    Home = handyuser:user_home(Username),
    SfHashFile = filename:join([Home, ".sf", "sf_hash"]),
    case file:read_file(SfHashFile) of
        {ok, Contents} -> binary_to_list(Contents);
        {error, _Reason} -> no_such_user
    end.

check_access(Username, Dir, Directories) ->
    lists:foldl(
        fun (DirEntry, false) ->
                case Dir of
                    home -> Dir =:= handyuser:user_home(Username);
                    tmp -> Dir =:= "/tmp";
                    _ -> DirEntry =:= Dir
                end;
            (_Dir, true) -> true
        end, false, Directories).


% Dan Sahlin <dan@cslab.ericsson>, Ericsson Telecom, 1997-08-07
% version 1.2
% @hidden
-module(assoc).
-export([empty/0,get/2, get_min/1, get_max/1, 
  set/3, put/3, put_getoldval/3, delete/2, delete_min/1, delete_max/1,
  to_list/1, list_to/1, keymerge/2, get_previous/2, get_next/2,
  is_key/2, map/2, fold_l/3, fold_r/3]).

% TODO: get reasonable defaults or exceptions for accessing empty trees etc.
% define assoc:map, fold..., (db_match, db_matchdelete for ets use)
% define 'assoc:add' for adding one more value to a *list* of values 
% better name for put_getoldval
% construct a test suite

%i empty() 
%i returns the empty associative array

empty() -> nil.

%i get(Key, Assoc) 
%i returns {value,Value} if Value is associated with Key in Assoc.
%i returns false otherwise.

get(_X, nil) -> false;
get(X, T) -> acc23(T, X).

% acc23({l, X, W}, X) -> 
%     {value,W};
% acc23({l, K, W}, _) ->
%     false;
% acc23({n2,T1,M,_}, X) when M > X ->
%     acc23(T1, X);
% acc23({n2,_,_,T2}, X) ->
%     acc23(T2, X);
% acc23({n3,T1,M2,_,_,_}, X) when M2 > X ->
%     acc23(T1, X);
% acc23({n3,_,_,T2,M3,_}, X) when M3 > X ->
%     acc23(T2, X);
% acc23({n3,_,_,_,_,T3}, X) ->
%     acc23(T3, X).

acc23({l, X, W}, Y) -> 
  if 
  X==Y -> {value,W};
  true -> false
  end;
acc23({n2,T1,M,T2}, X) ->
  if 
  M>X -> acc23(T1,X);
  true -> acc23(T2,X)
  end;
acc23({n3,T1,M2,T2,M3,T3}, X) ->
  if
  M2>X -> acc23(T1, X);
  M3>X -> acc23(T2, X);
  true -> acc23(T3, X)
  end.

%i get_min(Assoc)
%i returns {Key, Value} where Key is the smallest Key in Assoc
%i if Assoc is empty, 'none' is returned

get_min(nil) ->
  none;
get_min(Old) ->
  get_min23(Old).

get_min23({l, K, V}) -> {K, V};
get_min23({n2,T1, _, _}) -> get_min23(T1);
get_min23({n3, T1, _, _, _, _}) -> get_min23(T1).

%i get_max(Assoc)
%i returns {Key, Value} where Key is the largest Key in Assoc
%i if Assoc is empty, 'none' is returned

get_max(nil) ->
  none;
get_max(Old) ->
  get_max23(Old).

get_max23({l, K, V}) -> {K, V}; 
get_max23({n2,_, _, T2}) -> get_max23(T2);
get_max23({n3,_, _, _, _, T3}) ->get_max23(T3).

%i set(Key, Value, Assoc) 
%i returns {Oldvalue, NewAssoc} where Oldvalue is the value associated with 
%i Key in Assoc. NewAssoc is identical to Assoc but with Key associated with 
%i Value.
%i An exception is generated if the Key is not found in Assoc.

set(X, V, T0) ->
  {_W,_T}=set23(T0, X, V).

set23({l,X,U}, X, V) ->
  {U,{l,X,V}}; 
set23({n2,T1,M,T2}, X, V) when X < M ->
      {W,N1} = set23(T1, X, V),
  {W,{n2,N1,M,T2}};
set23({n2,T1,M,T2}, X, V) when M =< X ->
  {W,N2} = set23(T2, X, V),
  {W,{n2,T1,M,N2}};
set23({n3,T1,M1,T2,M2,T3}, X, V) when X < M1 ->
      {W,N1} = set23(T1, X, V),
  {W,{n3,N1,M1,T2,M2,T3}};
set23({n3,T1,M1,T2,M2,T3}, X, V) when X < M2 ->
     {W,N2} = set23(T2, X, V),
  {W,{n3,T1,M1,N2,M2,T3}};
set23({n3,T1,M1,T2,M2,T3}, X, V) when M2 =< X ->
      {W,N3} = set23(T3, X, V),
  {W,{n3,T1,M1,T2,M2,N3}}.

%i put(Key, Value, Assoc)
%i returns a new associative array where Assoc is updated to associate Value with
%i Key

put(X, V, Tree0) ->
  {Trees,_Old}=insert23(Tree0, X, V),
  make_23tree(Trees).

%i put_getoldval(Key, Value, Assoc)
%i returns {Oldvalue, Newassoc} where Newassoc is a new associative array 
%i where Assoc is updated to associate Value with Key.
%i Oldvalue is {value,Value} if an old value existed for Key, otherwise 
%i Oldvalue is false.

put_getoldval(X, V, Tree0) ->
  {Trees,Old}=insert23(Tree0, X, V),
  {Old,make_23tree(Trees)}.
  
make_23tree({t1,T}) -> T;
make_23tree({t2,M,T1,T2}) -> {n2,T1, M, T2}.

% Changed by Anders Andersson to support set_def/6 as well as put/4
insert23(nil, X, V) ->
      {{t1,{l,X,V}},false};
insert23({l,M,V2}, X, V) when X<M ->
      {{t2,M,{l,X,V},{l,M,V2}},false};
insert23({l,M,V2}, X, V) when M<X ->
      {{t2,X,{l,M,V2},{l,X,V}},false};
insert23({l,X,Old}, X, V) ->
      {{t1,{l,X,V}},{value,Old}};
insert23({n2,T1,M,T2}, X, V) when X<M ->
      {T1s,Oldval} = insert23(T1, X, V),
  {combine_left2(T1s, M, T2),Oldval};
insert23({n2,T1,M,T2}, X, V) when M=<X ->
      {T2s,Oldval} = insert23(T2, X, V),
  {combine_right2(T2s, M, T1),Oldval};
insert23({n3,T1,M1,T2,M2,T3}, X, V) when X<M1 ->
      {T1s,Oldval} = insert23(T1, X, V),
  {combine1(T1s, M1, T2, M2, T3), Oldval};
insert23({n3,T1,M1,T2,M2,T3}, X, V) when X<M2 ->
      {T2s,Oldval} = insert23(T2, X, V),
  {combine2(T2s, M1, T1, M2, T3), Oldval};
insert23({n3,T1,M1,T2,M2,T3}, X, V) when M2 =< X ->
      {T3s,Oldval} = insert23(T3, X, V),
  {combine3(T3s, M1, T1, M2, T2), Oldval}.

combine3({t1,T3}, M1, T1, M2, T2) ->
      {t1,{n3,T1,M1,T2,M2,T3}};
combine3({t2,M, Ta, Tb}, M1, T1, M2, T2) -> 
      {t2,M2, {n2,T1, M1, T2}, {n2,Ta, M, Tb}}.

combine2({t1,T2}, M1, T1, M2, T3) ->
      {t1,{n3,T1,M1,T2,M2,T3}};
combine2({t2,M, Ta, Tb}, M1, T1, M2, T3) ->
      {t2,M, {n2,T1, M1, Ta}, {n2,Tb, M2, T3}}.

combine1({t1,T1}, M1, T2, M2, T3) -> 
      {t1,{n3,T1,M1,T2,M2,T3}};
combine1({t2,M, Ta, Tb}, M1, T2, M2, T3) ->
      {t2,M1, {n2,Ta, M, Tb}, {n2,T2, M2, T3}}.

combine_left2({t1,T1}, M, T2) ->
      {t1,{n2,T1,M,T2}};
combine_left2({t2,M1,T1,T2}, M2, T3) ->
      {t1,{n3,T1,M1,T2,M2,T3}}.

combine_right2({t1,T2}, M, T1) ->
      {t1,{n2,T1,M,T2}};
combine_right2({t2,M2,T2,T3}, M1, T1) ->
      {t1,{n3,T1,M1,T2,M2,T3}}.


%i delete(Key,Assoc) returns
%i {Value, NewAssoc} where 
%i Value is the value of Key in Assoc if it exists and
%i NewAssoc is the associative array with the {Key,Value} deleted.
%i If the Key does not exist the function returns
%i 'false'.

delete(_Key, nil) -> false;
delete(Key, OldAssoc) ->  
    case delete23(OldAssoc, Key) of
  {false,_} -> false;
  {{value,Thevalue},NewAssoc} -> {Thevalue,fix_root(NewAssoc)}
    end.

fix_root({n1,Tree})  ->  Tree;
fix_root(Old)  -> Old.

delete23({l,Key, V}, Key) -> {{value,V},nil};
delete23({l,KeyOther, V}, _Key) -> {false,{l,KeyOther,V}};
delete23({n2,T1, M, T2}, Key) when M > Key ->
      {Value,NewT1} = delete23(T1, Key),
  {Value, merge_with_right_sibling(NewT1, T2, M)};
delete23({n2, T1, M, T2}, Key) ->
      {Value,NewT2} = delete23(T2, Key),
  {Value,merge_with_left_sibling(NewT2, T1, M)};
delete23({n3, T1, M1, T2, M2, T3}, Key) when M1 > Key ->
      {Value, NewT1} = delete23(T1, Key),
  NewT = merge_with_right_sibling(NewT1, T2, M1),
  {Value, extend_right(NewT, M2, T3)};
delete23({n3, T1, M1, T2, M2, T3}, Key) when M2 > Key ->
      {Value,NewT2} = delete23(T2, Key),
  NewT = merge_with_left_sibling(NewT2, T1, M1),
  {Value, extend_right(NewT, M2, T3)};
delete23({n3, T1, M1, T2, M2, T3}, Key) ->
      {Value, NewT3} = delete23(T3, Key),
  NewT = merge_with_left_sibling(NewT3, T2, M2),
  {Value, extend_left(NewT, M1, T1)}.

extend_right({n1,T1}, M, T2) -> {n2,T1, M, T2};
extend_right({n2, T1, M1, T2}, M2, T3) -> {n3, T1, M1, T2, M2, T3}.

extend_left({n1,T2}, M, T1) -> {n2, T1, M, T2};
extend_left({n2, T2, M2, T3}, M1, T1) -> {n3, T1, M1, T2, M2, T3}.

merge_with_right_sibling(nil, T, _) ->  {n1,T};
merge_with_right_sibling({n1,T1}, {n2,T2, M2, T3}, M1) ->
    {n1,{n3,T1,M1,T2,M2,T3}};
merge_with_right_sibling({n1,T1}, {n3, T2, M2, T3, M3, T4}, M1) ->
    {n2,{n2,T1,M1,T2}, M2, {n2, T3, M3, T4}};
merge_with_right_sibling(T1, T2, M) ->  {n2,T1, M, T2}.

merge_with_left_sibling(nil, T, _) ->  {n1,T};
merge_with_left_sibling({n1, T3}, {n2, T1, M1, T2}, M2) ->
    {n1,{n3,T1,M1,T2,M2,T3}};
merge_with_left_sibling({n1,T4}, {n3,T1, M1, T2, M2, T3}, M3) ->
  {n2,{n2,T1,M1,T2}, M2, {n2,T3, M3, T4}};
merge_with_left_sibling(T2, T1, M) ->
  {n2,T1, M, T2}.

%i delete_min(Assoc) returns
%i {{Key, Value}, NewAssoc} where
%i Key is the smallest key in Assoc and Value its corresponding value and
%i NewAssoc is the associative array with the {Key,Value} deleted
%i If the Key does not exist the function returns
%i 'false'

delete_min(nil) -> false;
delete_min(OldAssoc) ->  
  {KeyValue,Assoc} = delete_min23(OldAssoc),
  {KeyValue,fix_root(Assoc)}.

delete_min23({l, K, V}) -> {{K,V},nil};
delete_min23({n2,T1, M, T2}) ->
  {KeyValue,NewT1} = delete_min23(T1),
  {KeyValue,merge_with_right_sibling(NewT1, T2, M)};
delete_min23({n3, T1, M1, T2, M2, T3}) ->
  {KeyValue,NewT1} = delete_min23(T1),
  NewT = merge_with_right_sibling(NewT1, T2, M1),
  {KeyValue,extend_right(NewT, M2, T3)}.

%i delete_max(Assoc) 
%i as delete_min/1 but operates on the largest Key

delete_max(nil) -> false;
delete_max(OldAssoc) ->
      {KeyValue,Assoc} = delete_max23(OldAssoc),
  {KeyValue,fix_root(Assoc)}.

delete_max23({l, K, V}) -> {{K,V},nil};
delete_max23({n2, T1, M, T2}) ->
      {KeyValue,NewT2} = delete_max23(T2),
  {KeyValue,merge_with_left_sibling(NewT2, T1, M)};
delete_max23({n3,T1, M1, T2, M2, T3}) -> 
      {KeyValue,NewT3} = delete_max23(T3),
  NewT = merge_with_left_sibling(NewT3, T2, M2),
  {KeyValue,extend_left(NewT, M1, T1)}.

%i to_list(Assoc)
%i returns a list with items of the form {Key,Value} which corresponds to 
%i the association in Assoc. 
%i The list is ordered with respect to the keys.

to_list(nil) ->
  [];
to_list(Assoc) ->
  to_list(Assoc,[]).

to_list({l,Key,Val},List) ->
  [{Key,Val}|List];
to_list({n2,T1,_,T2},List) -> 
  to_list(T1,to_list(T2,List));
to_list({n3,T1,_,T2,_,T3},List) ->
  to_list(T1,to_list(T2,to_list(T3,List))).

%i list_to(List)
%i returns the associative array corresponding to the Listt with items of the 
%i form {Key,Value}
%i The argument list need not be sorted.

list_to(List) ->
  list_to(List,empty()).

list_to([],Assoc) -> Assoc;
list_to([{K,V}|List],Assoc) ->
  list_to(List,put(K,V,Assoc)).

%i keymerge(Assoc1, Assoc2)
%i The two associative arrays Assoc1 and Assoc2 are merged.
%i If a key already exists in Assoc1, its value gets replace by the value
%i of the corresponding key in Assoc2

keymerge(Assoc1,Assoc2) ->
  List2 = to_list(Assoc2),
  keymerge_list(List2,Assoc1).

keymerge_list([],Assoc) -> Assoc;
keymerge_list([{K,V}|List],Assoc) ->
  keymerge_list(List,put(K,V,Assoc)).

% used by dict:
is_key(Key,Assoc) -> 
    case get(Key,Assoc) of
  {value,_} -> true;
  false -> false
    end.


%i get_previous(SearchKey,Assoc) 
%i returns the tuple {Key, Value} such that the Key is the nearest smaller 
%i key than SearchKey in Assoc.
%i If no such key can be found, 'none' is returned.
%i Complexity: O(lg n)

get_previous(_X, nil) ->
  none;
get_previous(X, T) ->
  acc_previous(T, X, nil).

acc_previous({l,K1,W}, X, _) when X > K1 ->
      {K1,W};
acc_previous({l,_,_}, _, NearestLessTree) -> % we went on awild goose chase..
      get_max(NearestLessTree);     % ..correct the mistake
acc_previous({n2,T1,M,_}, X, NLT) when M>X ->
        acc_previous(T1, X, NLT);
acc_previous({n2,NLT,_,T2}, X, _) ->
        acc_previous(T2, X, NLT);
acc_previous({n3,T1,M2,_,_,_}, X, NLT) when M2>X->
        acc_previous(T1, X, NLT);
acc_previous({n3,NLT,_,T2,M3,_}, X, _) when M3>X-> 
      acc_previous(T2, X, NLT);
acc_previous({n3,_,_,NLT,_,T3}, X, _) ->
      acc_previous(T3, X, NLT).



%i get_next(SearchKey,Assoc) 
%i returns the tuple {Key, Value} such that the Key is the nearest smaller 
%i key than SearchKey in Assoc
%i If no such key can be found, 'none' is returned
%i Complexity: O(lg n)

get_next(_X, nil) ->
  none;
get_next(X, T) ->
  acc_next(T, X, nil).

acc_next({l,K1,W}, X, _) when X < K1 ->
      {K1,W};
acc_next({l,_,_}, _, NGT) -> % we went on awild goose chase..
      get_min(NGT);      % ..correct the mistake
acc_next({n2,T1,M,NGT}, X, _) when M>X ->
        acc_next(T1, X, NGT);
acc_next({n2,_,_,T2}, X, NGT) ->
        acc_next(T2, X, NGT);
acc_next({n3,T1,M2,NGT,_,_}, X, _) when M2>X->
        acc_next(T1, X, NGT);
acc_next({n3,_,_,T2,M3,NGT}, X, _) when M3>X-> 
      acc_next(T2, X, NGT);
acc_next({n3,_,_,_,_,T3}, X, NGT) ->
      acc_next(T3, X, NGT).

%i map(F,Assoc)
%i maps the function F to all values in Assoc

map(_F,nil) ->
  nil;
map(F,Assoc) ->
  map2(Assoc,F).

%%map2({l,Key,Val},F) ->
%%  {l,Key,F(Val)};
map2({n2,T1,M1,T2},F) -> 
  {n2,map2(T1,F),M1,map2(T2,F)};
map2({n3,T1,M1,T2,M2,T3},F) ->
  {n3,map2(T1,F),M1,map2(T2,F),M2,map2(T3,F)}.


%i fold_l(F,U,Assoc)
%i fold left, i.e. apply the function F and the value U on the associative 
%i array as follows
%i F(K1,V1,F(K2,V2,....F(Kn,Vn,U)...))

%%fold_l({l,Key,Val},F,U) ->
%%  F(Key,Val,U);
fold_l({n2,T1,_M1,T2},F,U) -> 
  fold_l(T1,F,fold_l(T2,F,U));
fold_l({n3,T1,_M1,T2,_M2,T3},F,U) ->
  fold_l(T1,F,fold_l(T2,F,fold_l(T3,F,U))).

%i fold_r(F,U,Assoc)
%i fold right, as above but with
%i F(Kn,Vn, ...F(K1,V2,F(K1,V1,U))...)

%%fold_r({l,Key,Val},F,U) ->
%%  F(Key,Val,U);
fold_r({n2,T1,_M1,T2},F,U) -> 
  fold_r(T2,F,fold_r(T1,F,U));
fold_r({n3,T1,_M1,T2,_M2,T3},F,U) ->
  fold_r(T3,F,fold_r(T2,F,fold_r(T1,F,U))).

% match(Assoc,Pattern)...


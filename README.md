# Armchair

Janus を参考にした並行制約プログラミング言語です。

# Examples

- バブルソート

```
armchair>長さを得る(I, !L)::
     ...>I=tail -> L=0,
     ...>e(E, Next):=I -> (長さを得る(Next, !LNext), L=LNext+1).

armchair>
     ...>      バブルソート(I, !O)::
     ...>        長さを得る(I, !L), バブルソートループ(I, L-1, !O).

armchair>
     ...>      バブルソートループ(I, L, !O)::
     ...>        L >= 1 -> (
     ...>          バブルソートループ(I, L-1, !OSwapped),
     ...>          後ろの群のスワップ(OSwapped, O)
     ...>        ),
     ...>        L = 0 -> O = I.

armchair>
     ...>      後ろの群のスワップ(Elm, !O)::
     ...>        e(E, tail) := Elm -> O = e(E, tail),
     ...>        e(E, e(Next, Rest)) := Elm -> (
     ...>          Next >= E -> (O=e(E, NextSwapped), ToSwap=e(Next, Rest)),
     ...>          Next <  E -> (O=e(Next, NextSwapped), ToSwap=e(E, Rest)),
     ...>          後ろの群のスワップ(ToSwap, !NextSwapped)
     ...>        ).

armchair>
     ...>      ?バブルソート(e(5, e(1, e(2, e(4, e(3, tail))))), !O).
O = e(1, e(2, e(3, e(4, e(5, 'tail')))));
```

[ソースコード](examples/examples.rs)

# Docs

[日本語 概要](armchair-doc/Overview-ja.md)

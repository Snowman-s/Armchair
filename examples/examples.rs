use armchair::{executors, Constraint};

fn main() {
    bubble_sort();
}

fn bubble_sort() {
    let code = "長さを得る(I, !L)::
        I=tail -> L=0,
        e(E, Next):=I -> (長さを得る(Next, !LNext), L=LNext+1).

      バブルソート(I, !O)::
        長さを得る(I, !L), バブルソートループ(I, L-1, !O).

      バブルソートループ(I, L, !O)::
        L >= 1 -> (
          バブルソートループ(I, L-1, !OSwapped),
          後ろの群のスワップ(OSwapped, O)
        ),
        L = 0 -> O = I.

      後ろの群のスワップ(Elm, !O)::
        e(E, tail) := Elm -> O = e(E, tail),
        e(E, e(Next, Rest)) := Elm -> (
          Next >= E -> (O=e(E, NextSwapped), ToSwap=e(Next, Rest)),
          Next <  E -> (O=e(Next, NextSwapped), ToSwap=e(E, Rest)),
          後ろの群のスワップ(ToSwap, !NextSwapped)
        ).
  
      ?バブルソート(e(5, e(1, e(2, e(4, e(3, tail))))), !O).";

    let result = &executors::executors::execute(code).unwrap()[0];
    let constraint_of_o = result.get("O").unwrap();
    let Constraint::EqualTo(atom_o) = constraint_of_o else {panic!()};

    assert_eq!(atom_o.to_string(), "e(1, e(2, e(3, e(4, e(5, 'tail')))))");
}

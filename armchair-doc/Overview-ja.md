_この Markdown は正確なドキュメントのプロトタイプです._

_Armchair 言語は、Kahn や Saraswat による言語である Janus 言語の拡張です._

# BNF

```
<PROGRAM> ::= (<WS> <clause> <WS>)*
<clause> ::= <behavior_name> <lbracket> [<clause_head_term> (<sep> <clause_head_term>)*] <rbracket> <double_colon> <agent>
<agent> ::= <tell_contraint> | <behavior_name> <lbracket> [<tell_term> (<sep> <tell_term>)*] <rbracket> | <ask_constraint> <arrow> <agent> | <agent> (<sep> <agent>)+

<teller> ::= <emphasize> <variable>
<asker> ::= <variable>
<clause_head_term> ::= <teller> | <asker>

<WS> ::= (" " | "\n" | "\t")*
<sep> ::= <WS> "," <WS>
<lbracket> ::= <WS> "(" <WS>
<rbracket> ::= <WS> ")" <WS>
<double_colon> ::= <WS> ":" ":" <WS>
<arrow> ::= <WS> "-" ">" <WS>
<emphasize> ::= <WS> "!" <WS>
```

# How Armchair work?

## 変数ストア

**変数ストア** とは、各々の*変数*ごとに、 **"制約"** を記憶するグ r－バルなオブジェクトです。
(*変数*は、通常の言語でよくみられるような値を格納する物では**ありません**！)

## エージェント

エージェントとは、自身が使う変数ストアの各変数に対する制約を引数として受け取り、他のエージェントを解決するか、新しい制約を _tell_ するものです。

エージェントには、以下の３つの種類と、その連立があります。

1. エージェントが新しい**制約**を*tell* したとき、**変数ストア**はその制約を、今までにあった制約に加えて記憶します。(\<tell_constraint> がこれに対応。)
1. エージェントは、ある制約が満たされる場合において(_ask_)、他のエージェントを用いることが出来ます。制約が満たされるような状況がいくつか存在する場合、任意の一つを勝手に選択します。(\<ask_constraint> \<arrow> \<agent> がこれに対応。)　このとき、制約が上手く満たされるよう\<ask_constraint> 以外の変数に制約がつく場合があります。
1. エージェントは、他のエージェントを用いて変数を制約することが出来ます。(\<behavior_name> \<lbracket> [\<tell_term> (\<sep> \<tell_term>)\*] \<rbracket> がこれに対応。) このとき、制約が上手く満たされるよう\<tell_term> 以外の変数に制約がつく場合があります。

エージェントが連立されたとき、それらのエージェントは並列に実行され、解決出来なかったものは即座に破棄されます。

## teller, asker

- teller と呼ばれる項を用いて、ある変数の、「制約を追加するための参照」を得ることが出来ます。
- asker と呼ばれる項を用いて、ある変数の、「ある制約が成立するかどうかを問い合わせるための参照」を得ることが出来ます。

結果的に、エージェントは behavior_name の先頭にある変数にしかアクセスできません。

同じエージェント内では、teller と asker は**合計 2 つまで**使用することが出来ます。それを超えては使用できません。ただし、「unless enclosing ask-conditions ensure that the variable will be ground at run-time」(未訳)

(\<tell_constraint> の左辺は!は付きません。それは節の頭の変数につきます)

## 制約の種類

Armchair には、**バッグ** と呼ばれるコレクション(無限もありうる)が存在します。これを踏まえて、以下の定義を説明します。

*ask*制約は、以下の 3 つの内のいずれかを選択します。

1. `[variable] = T`。 ただし、`T`はバッグではない。`variable`が`T`になりうるという制約を示す。
1. `[variable1] >= [variable2]`。この通りの順序関係を示す。
1. `[variable1] U [bag]`。???

*tell*制約は、以下の 2 つの内のいずれかを選択します。

1. `[variable] = {T}`。ただし、`T`は定数か変数。
1. `[bag] = [variable] U [variable2] U ...`。`bag`がこれらの変数を含むことを示す。

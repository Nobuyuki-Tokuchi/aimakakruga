# aimakakrúgá
架空言語の単語の語形を変化させるためのDSL  
読み方は/ɛːmakakruːɡaː/または/zimakakruːɡaː/．

# 注意
現在，構文が未確定のためバージョンが上がるごとに変更される可能性があります．

# 構文
先に記述した規則から適用される．

```
-- 一行コメント
-- 変数定義はZatlinの記法とほぼ同様に記述できる
V = "a" | "e" | "i" | "o" | "u" | "a" "i"
T = "p" | "t" | "k"
C = T | "f" | "s" | "h"
    | "l" | "y"

-- セミコロンを使用すると一行に複数のパターンを記述できる
^ "s" "k" V -> "s" @3; "e" "a" | "i" "a" -> "y" "a"

-- when句に細かい適用条件を記述できる
"i" V when @2 == "i" or @2 == "e" -> "i" "i"
V T T V when @2 == @3 -> @1 @2 @4

-- 正規表現のように"^"や"$"が使える
"l" "l" V $ -> "l" @3
C "l" V | C "l" "y" when @1 /= "l" -> @1 @3
"t" "s" V
when
    not (@0 like @1 @2 "a" $ or @0 like @1 @2 "u" $)
->
    "s" @3
```

右辺やwhen句では"@(1以上の整数)"で左辺の該当位置の文字列を使用することができる．

when句では@@および@nを使用することができる．
* @@: 元の単語
* @n: 1つ前までの規則を適用した状態の単語

## 改行が許可されている場所
次の場所では改行が許可されている．
* "|"の前
* "->","when","and","or"の前後
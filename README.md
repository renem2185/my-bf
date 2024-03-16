# my-bf
Learning Scheme language with imprementation Brainf*ck

## [Play here!](https://renem2185.github.io/bf/vectorbfi.html)
See also -> https://gist.github.com/renem2185/eb907c6b0f554f95de929420d7d20a77

## メモ書き (Japanese)
自分で遊ぶときは [Chicken Scheme](http://www.call-cc.org/) でコンパイルしていましたが、
少しいじれば他の環境でも動くと思います (たぶん)

テストに使っていた Quineのコードは[こちら](https://ja.wikipedia.org/wiki/%E3%82%AF%E3%83%AF%E3%82%A4%E3%83%B3_(%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0)#Brainfuck)

- `old/` に作業中の残骸が残っています
  - ループの中で Listを操作すると分かりやすく実行時間が伸びて面白かったです
  - 結局 `vectorbfi.scm`では全部 `u8vector` (可変長配列) にしました
  - あと遅延評価のことをよく知らなかったりだとか


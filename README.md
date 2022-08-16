# ironcc

# これはなに
プログラミング言語Rustで書かれたC言語のコンパイラ。

# 動かし方

## 1. Rust のインストール
linux系の場合、Rustツールチェインのインストールは次を実行することでできる。
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```
参考 : [https://www.rust-lang.org/ja/tools/install](https://www.rust-lang.org/ja/tools/install)

環境変数の設定のためにシェルを再起動したりするのを忘れないようにしましょう。

## 2. 任意の (すでに実装された機能のみの) Cファイルをコンパイルする
1. ソースを書き込んだCファイルを`src.c`とする。
2. そのソースへのパスを ironcc の引数にとって実行する。

    例:
    ```bash
    cargo run src.c
    ```

    こうすることで `src.s` が生成される。
3. `src.s` はアセンブリ言語が書かれたファイルである。中身をみたいときは
    ```
    cat src.s
    ```
    などをすると中身を見ることができる。
4. clang などを用いてアセンブルする。
    
    例:
    ```
    clang src.s
    ```

    また、他に ironcc で実装していない機能を部分的に使うときは、clang を用いて、コンパイル、リンクすると良い。

    例(`link.c` にリンクしたい関数などを切り出している場合):
    ```bash
    clang src.s link.c
    ```
5. 実行する。clang のアセンブル、リンク時になにも引数を渡していない場合、 `a.out` という名前で出力される。

    例:
    ```
    ./a.out
    ```

# テストを動かす
make を用いて自動化されているため、次を実行することで、すべてのテストが実行される。
```bash
make testall
```
また、このリポジトリの [Github Actions](https://github.com/lemolatoon/ironcc/actions) の欄からもテストの実行を見ることができる。

# cargo doc を見る
このコンパイラには自動生成されたドキュメントがあります。たまにコメントもついています。

[そのドキュメント](https://lemolatoon.github.io/ironcc/ironcc/)

# ChangeLog を見る
各 [pull requests](https://github.com/lemolatoon/ironcc/pulls) ごとに ChangeLog を書いています。

# コンパイルできるサンプルプログラム

## Life Game を動かす
```
make life
```
Thanks to @uint256_t

## ドーナツを動かす
```
make donut
```
Thanks to @a1k0n, @hsjoihs


# 謝辞
このコンパイラはセキュリティキャンプ2022のゼミで開発が始められ、その際には講師の uint256_t さん、 hsjoihs さん、チューターのみなさんによるレビュー、助言、そして同じCコンパイラ受講生のみんなとの切磋琢磨によって開発が進みました。この場を借りて感謝します。
# usocomp

- あそびかた
  - [Wasm版](https://boletales.github.io/usocomp/) で遊べます
  - 好きなコードをコンパイルして実行しよう！
- これはなんですか
  - 嘘の機械語(MachineLang)を吐く、嘘の中間言語(SimpleLang)のコンパイラ(app/SimpleLangC.hs)とそのツール群です
  - MachineLang
    - RISC風味の機械語
    - 即値はすべて64bit整数
    - レジスタは8個+プログラムカウンタ
  - SimpleLang
    - LLVM IRとか、wasmぐらいの抽象度の手続き型言語
    - 単純な型（1ワード整数・無名構造体・無名共用体・ポインタ・関数ポインタ）を持つ
    - 変数はスコープ付きローカル変数だけ
    - 末尾呼出対応
  - MachineLang.FromSimpleLang用のデバッガ(MacineLang.FromSimpleLang.Debugger)
    - コンパイル元のコードの対応する位置を表示
    - 生メモリのかわりにスタックフレームの構造を表示
    - 位置情報が変わるまでスキップできる
```
===============================
Tick: 1834

Instruction:
const r2 -1

Position:
main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2)

Registers:
  RegPC  : 178
  RegFPtr: 10
  RegSPtr: 14
  RegX   : 610
  RegY   : 3
  RegZ   : 1
  RegW   : 81

Stack Frame:
  === stack top ===
  local vars:
    14: 610
    13: 377
    12: 610
    11: 5

  frame:
    10 (old FPtr) : 3
    9 (old SPtr) : 4
    8 (return)   : 41

  args:
    7: 610
    6: 377
    5: 6
  === frame bottom ===

===============================
```
  - SimpleLangの抽象構文木を手で書きたくないため、SimpleLangを生成するライブラリ（SimpleLang.Tools.Manual）
    - SimpleLang.Tools.Manualを使ったコードの例（フィボナッチ数列の計算）：
```hs
tailRecTest :: SLProgram
tailRecTest =
  runSLMFuncsM $ do
    let fibonacci = slmVirtualFunc (SLUserFunc "main" "fibonacci") :: '[SLTInt, SLTInt, SLTInt] --> SLTInt

    _ :: '[] --> SLTInt <- slmFunc SLFuncMain (do
        x <- slmNewVar $ _app fibonacci (_const 20) (_const 0) (_const 1)
        slmReturn (_local x)
        pure ()
      )
    
    slmSetRealFunc fibonacci (\steps a b -> slmFundef $ do
        slmCase (V.fromList [
            ( steps `_eq` _const 0, do
                slmReturn b
                pure ()
              )
          ]) (do
            slmTailCall fibonacci (steps `_sub` _const 1) b (a `_add` b)
          )
        pure ()
      )

    pure ()
```
  - ……から生成されるASTを、人間に読みやすく書き下したもの:
    - 
```
function main() -> int
{
  int $0 = main.fibonacci(20, 0, 1)
  return $0
}

function main.fibonacci(int $A0, int $A1, int $A2) -> int
{
  when ($A0 == 0)
  {
    return $A2
  }
  else
  {
    tailcall main.fibonacci(($A0 - 1), $A2, ($A1 + $A2))
  }
}
```
  - ……を入力すると、同様の構文木を出力するパーサ

- アピールポイント
  - 課題の分割
    - MachineLang.FromSimpleLangで、制御構文や式の翻訳をほぼ「決まった命令を順番に出力するだけ」になる単位まで分割した
    - 結果、特に問題の起こりやすい関数呼び出し・リターン・末尾再帰などの操作について、効率的にデバッグすることができた
  - デバッグツール
    - SimpleLangをMachineLangにコンパイルする際に、MachineLangの命令がSimpleLangのどの位置に由来するのか逐一計算した
    - スタックフレームの構造を表示できるデバッガを実装した
    - MachineLangとSimpleLangについて、可読な文字列表現を実装した
    - 結果、コンパイル後のプログラムの挙動・コンパイル生成物のMachineLang・FromSimpleLangのコードのうち生成を行った部分の対応を理解しながらデバッグすることが容易になった
  - DSL
    - 関数型プログラミングを強く推奨する言語で開発を行っているため、関数型プログラミングを主にすべきことは明らかである
    - 一方で、コード生成や構文木生成は手続き的に書く方が見通しがよい
    - MonadStateやMonadErrorを活用することで都合よく関数型の書き方と手続き型の書き方を使い分け、直接コンストラクタを用いる場面を減らすことでコードの柔軟性を高めた
  - 型システムの濫用
    - ASTの直書きは苦行そのものであるため、SimpleLangのコードをより楽な形で書き下したい
    - SimpleLangの関数を書く際には、Haskellのラムダ式をそのまま利用できると楽である
    - 型レベル自然数と複数の型族を濫用し、任意のnについてHaskellのn変数関数からSimpleLangのn変数関数を生成できるようにした
  
### メモ：Wasm版をコンパイルする際の流れについて
- 以下の流れ：
  - https://www.haskell.org/ghcup/guide/#ghc-wasm-cross-bindists-experimental に従って、GHCのWASM backendをインストール
  - `web/build.sh`
    - https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#using-the-ghc-wasm-backend-to-compile-link-code の指示に従ってcabalに`--with-compiler=`, `--with-hc-pkg=`, `--with-hsc2hs=` オプションを渡してコンパイル
    - `$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i bin/slangcweb.wasm -o bin/ghc_wasm_jsffi_raw.js` でグルーコードを生成
    - [wizer](https://github.com/bytecodealliance/wizer) で初期化済みWASMを生成
    - `wasm-opt` と `wasm-strip` (要wabt)で最適化
    - `docs/` にコピー
  - `ghc_wasm_jsffi_raw.js` の `await import("node:timers")` がブラウザだと落ちるのでコメントアウト (`ghc_wasm_jsffi.js`)
- シンタックスハイライトは以下を利用：
  - `tmlanguage.json`: `usocomp-highlighting` からコピー
  - VSCodeのシンタックスハイライトは monaco に直接対応していないため、以下を使用：
    - `monaco-editor-textmate`
    - `monaco-vscode-textmate-theme-converter`
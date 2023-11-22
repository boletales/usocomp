# usocomp

- これはなに
  - 嘘の機械語(MachineLang)を吐く、嘘の中間言語(SimpleLang)のコンパイラ(MachineLang.FromSimpleLang)とそのツール群です
  - MachineLang
    - RISC風味の機械語
    - 即値はすべて64bit整数
    - レジスタは8個+プログラムカウンタ
  - SimpleLang
    - LLVM IRとか、wasmぐらいの抽象度の手続き型言語
    - 型はない（ない）
    - 変数はスコープ付きローカル変数だけ
    - 末尾呼出対応
    - 関数呼出で引数の数を間違えたときの安全策がない……
  - MachineLang.FromSimpleLang用のデバッガ(MacineLang.FromSimpleLang.Debugger)
    - コンパイル元のコードの対応する位置を表示
    - 生メモリのかわりにスタックフレームの構造を表示
    - 位置情報が変わるまでスキップできる
    - ```
          ===============================
          Tick: 133

          Instruction:
          const r4 -1

          Position:
          SLUserFunc "main" "fibonacci".SLLPMulti 0.SLLPCaseElseBody.SLLPMulti 0

          Registers:
            RegPC  : 193
            RegFPtr: 10
            RegSPtr: 16
            RegX   : 1
            RegY   : 11
            RegZ   : 3
            RegW   : 0

          Stack Frame:
            === stack top ===
            local vars:
              16: 3
              15: 4
              14: 40
              13: 1
              12: 1
              11: 19

            frame:
              10 (old FPtr) : 3
              9 (old SPtr) : 4
              8 (return)   : 40

            args:
              7: 1
              6: 0
              5: 20
            === frame bottom ===

          ===============================


          Enter: s (skip to next), q (quit), else (step)
      ```
  - SimpleLangの抽象構文木を手で書きたくないため、SimpleLangを生成するライブラリ（SimpleLang.Tools.Manual）
    - SimpleLang.Tools.Manualを使ったコードの例（フィボナッチ数列の計算）：
      - ```hs
            tailRecTest :: SLProgram
            tailRecTest =
              runSLMFuncsM $ do
                let fibonacci = slmVirtualFunc (SLUserFunc "main" "fibonacci") :: SLMFuncOf 3

                _ <- slmFunc SLFuncMain (do
                    x <- slmNewVar $ _app fibonacci (_const 20) (_const 0) (_const 1)
                    slmReturn (_local x)
                    pure ()
                  )
                
                slmSetRealFunc fibonacci (\steps a b -> slmFundef $ do
                    slmCase (V.fromList [
                        ( _arg steps `_eq` _const 0, do
                            slmReturn (_arg b)
                            pure ()
                          )
                      ]) (do
                        slmTailCall fibonacci (_arg steps `_sub` _const 1) (_arg b) (_arg a `_add` _arg b)
                      )
                    pure ()
                  )

                pure ()
          ```
      - ……から生成されるASTを、人間に読みやすく書き下したもの:
        - ```
            function #main ($A0)
            {
              var $L0 = #main.fibonacci(20, 0, 1)
              return $L0
            }

            function #main.fibonacci ($A0, $A1, $A2, $A3)
            {
              when ($A0 == 0)
              {
                return $A2
              }
              else
              {
                tailcall #main.fibonacci(($A0 - 1), $A2, ($A1 + $A2))
              }
            }
          ```

- なにをがんばった
  - 課題の分割
    - MachineLang.FromSimpleLangで、制御構文や式の翻訳をほぼ「決まった命令を順番に出力するだけ」になる単位まで分割した
    - 結果、特に問題の起こりやすい関数呼び出し・リターン・末尾再帰などの操作について、効率的にデバッグすることができた
  - デバッグツールの制作
    - SimpleLangをMachineLangにコンパイルする際に、MachineLangの命令がSimpleLangのどの位置に由来するのか逐一計算した
    - スタックフレームの構造を表示できるデバッガを実装した
    - MachineLangとSimpleLangについて、可読な文字列表現を実装した
    - 結果、コンパイル後のプログラムの挙動・コンパイル生成物のMachineLang・FromSimpleLangのコードのうち生成を行った部分の対応を理解しながらデバッグすることが容易になった
  - モナドの活用
    - 関数型プログラミングを強く推奨する言語で開発を行っているため、関数型プログラミングを主にすべきことは明らかである
    - 一方で、コード生成や構文木生成は手続き的に書く方が見通しがよい
    - MonadStateやMonadErrorを活用することで都合よく関数型の書き方と手続き型の書き方を使い分け、直接コンストラクタを用いる場面を減らすことでコードの柔軟性を高めた
  - 型システムの活用
    - ASTの直書きは苦行そのものであるため、SimpleLangのコードをより楽な形で書き下したい
    - SimpleLangの関数を書く際には、Haskellのラムダ式をそのまま利用できると楽である
    - 型レベル自然数と複数の型族を濫用し、任意のnについてHaskellのn変数関数からSimpleLangのn変数関数を生成できるようにした
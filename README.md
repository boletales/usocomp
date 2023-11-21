# usocomp

- これはなに
  - 嘘の機械語(MachineLang)を吐く、嘘の中間言語(SimpleLang)のコンパイラです
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
  - SimpleLangの抽象構文木を手で書きたくないため、SimpleLangを生成するライブラリ（SimpleLang.Tools.Manual）も書きました
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
  - 
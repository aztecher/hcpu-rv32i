# hcpu-rv32i

Haskell + Clash で RISC-V 準拠のCPUを作成してみたい。

## TODO

* [ ] 未実装の命令セットの実装
	* [ ] JALR
	* [ ] FENCE
	* [ ] FENCEI
	* [ ] CSRRC
	* [ ] CSRRS
	* [ ] CSRRW
	* [ ] CSRRCI
	* [ ] CSRRSI
	* [ ] CSRRWI
* [ ] 実装した命令セットの確認
	* [ ] R-type Format
		* [x] ADD
	* [ ] I-type Format
		* [ ] ADDI
	* [ ] S-type Format
	* [ ] B-type Format
	* [ ] U-type Format
	* [ ] J-type Format
* [ ] テストの記述
* [ ] コードの整理（モジュール化など）
* [ ] Verilog-HDLでの動作確認
* [ ] FPGAでの動作確認

## Usage

現時点ではちゃんとまとまってないので、`ghci`から`simpleProgramOutput`関数を呼び出すと`simpleProgram`で定義されたアセンブリ命令が実行される。
またこの命令も実際に実行すると停止が明確でないので、結果自体は出力されるものの、命令が終了していこうの結果出力も出力しようとして`Exception`が発生する。

```
# * simpleProgram
# 各レジスタの初期状態は0にしてあるので、下記のような動作になる。
# Line1 > 即値1 + X0の値0 = 1 をX1へ書き込み
# Line2 > 即値2 + X0の値0 = 2 をX2へ書き込み
# Line3 > X1 + X2 = 3 をX0へ書き込み
simpleProgram :: Vec 3 Instruction
simpleProgram = I (IArith ADDI (Word12 1) X0 X1) :>
                I (IArith ADDI (Word12 2) X0 X2) :>
                R (RArith ADD X1 X2 X0) :>
                Nil
```

```ghci
$ stack ghci
λ > simpleProgramOutput
λ > simpleProgramOutput
[(0,0,0),(0,0,0),(0,1,0),(0,1,0),(0,1,2),(0,1,2),(3,1,2),(3,1,2)*** Exception: /Users/mikiyaf/Documents/haskell/Clash/hcpu/riscv-rv32i/src/RV32I.hs:(463,39)-(475,2): Non-exhaustive patterns in case
```

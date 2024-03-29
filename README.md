# hcpu-rv32i

hcpu-rv32i is a RISC-V RV32I compliant software CPU implementation using [Haskell](https://www.haskell.org/) + [Clash](https://clash-lang.org/)


## Supported/Unsupported Instruction Sets

hcpu-rv32 now support some basic instruction sets of RV32I.  
However, some instruction sets, s.t. FENCE, FENCEI, ... are not implemented yet.  

### Supported instruction sets

* R-type Format
	* [x] ADD
	* [x] SUB
	* [x] AND
	* [x] OR
	* [x] XOR
	* [x] SLL
	* [x] SRA
	* [x] SRL
	* [x] SLT
	* [x] SLTU
* I-type Format
	* [x] ADDI
	* [x] SLTI
	* [x] SLTIU
	* [x] XORI
	* [x] ORI
	* [x] ANDI
	* [x] SLLI
	* [x] SRAI
	* [x] SRLI
	* [x] JALR
	* [x] LB
	* [x] LH
	* [x] LW
	* [x] LBU
	* [x] LHU
* S-type Format
	* [x] SB
	* [x] SH
	* [x] SW
* B-type Format
	* [x] BEQ
	* [x] BNE
	* [x] BLT
	* [x] BGE
	* [x] BLTU
	* [x] BGEU
* U-type Format
	* [x] LUI
	* [x] AUIPC
* J-type Format
	* [x] JAL

### Unsupported instruction sets (2021.10.03)

* I-type Format
	* [ ] FENCE
	* [ ] FENCEI
	* [ ] EBREAK
	* [ ] ECALL
	* [ ] CSRRC
	* [ ] CSRRS
	* [ ] CSRRW
	* [ ] CSRRCI
	* [ ] CSRRSI
	* [ ] CSRRWI

## TODO
* [x] Add Test
* [x] Refactor Codes
* [x] Consider relation between `Vec n Instruction` and address space
* [x] Create fibonacci function and check it's execution
* [x] Compile to Verilog-HDL
* [ ] Check working on compiled Verilog-HDL
* [ ] Check working on FPGA

## Usage

Now, you have to hardcode instruction that you want to execute.  
Please check some example in [src/RV32I/Programs/Example.hs](./src/RV32I/Programs/Example.hs) and test codes.

For example, If you want to run `addImm` program in [Example.hs](./src/RV32I/Programs/Example.hs), you can execute it by GHCi like bellow.  

```bash
λ > Prelude.take 10 $ sample (cpu initCPUState (programmedRAM addImm) :: Signal System Registers)
...
```

or, you can use `runCPU'` helper function.

```bash
λ > runCPU' addImm 0 10
```

[`sample`](https://hackage.haskell.org/package/clash-prelude-0.99.3/docs/Clash-Signal-Internal.html#v:sample) function generates infinite list of result of CPU execution, and `addImm` code will loop by jumping same addresss, so you take some length from this.

You can see the 'fibonacci program' in [src/RV32I/Programs/Fib.hs](./src/RV32I/Programs/Fib.hs) as a more complex example.  

## Clash

In this repository, we use Clash version `1.4.5` and can check its version as follows

```bash
stack run -- clash --version
Clash, version 1.4.5 (using clash-lib, version: 1.4.5)
```

## Known Issue

Take too much time to compile this program.
In my local computer, total compilation time is about 9h!

```bash
cabal run clash -- RV32I.Top --verilog
Up to date
Loaded package environment from /Users/mikiyaf/Documents/haskell/Clash/hcpu-rv32i/.ghc.environment.x86_64-darwin-8.10.7
GHC: Parsing and optimising modules took: 8.366s
GHC: Loading external modules from interface files took: 0.008s
GHC: Parsing annotations took: 0.010s
Clash: Parsing and compiling primitives took 0.176s
GHC+Clash: Loading modules cumulatively took 13.695s
Clash: Compiling RV32I.Top.topEntity
Clash: Normalization took 8h57m46.924s
Clash: Netlist generation took 2m0.349s
Clash: Total compilation took 9h0m3.538s
```

and generaed code is total 70000+ LoC verilog file.


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

### Unupported instruction sets (2021.09.24)

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
* [ ] Create fibonacci function and check it's execution
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

[`sample`](https://hackage.haskell.org/package/clash-prelude-0.99.3/docs/Clash-Signal-Internal.html#v:sample) function generates infinit list of result of CPU execution, and `addImm` code will loop by jumping same addresss, so you take some length from this.

# Icarus
Icarus is a compiled typed functional programming language designed as the final project of my programming languages course.

## Icarus Virtual Machine
The Icarus Virtual Machine (IVM) is a 64-bit virtual machine with two components: the register set and the memory.

### Registers
There are eight general purpose registers (`a` -`c`, encoded as `0b000` - `0b111`), the stack pointer `sp`, program counter `pc`, and link register `lr`.

### Memory

The memory is represented as an array of 64-bit values.

### Instruction Set

Each instruction is 64 bits wide. The first four bits `[64:61]` specify the ID of the instruction. The remaining bits are instruction specific.

| Mnemonic | Name | Parameters | Description | ID
| - | - | :-: | :-: | :-:|
| `HCF` | Halt | — | End Program Execution | `0000` |
| `LD` | Load Constant | REG | Load a Constant into a Register | `0001` |
| `STR` | Store to Memory | REG ADDR | Store value from a Register into Memory | `0010` |
| `B` | Branch | ADDR LINK | Transfer Program Execution and Link if Specified | `0100` |
| `RET` | Return | — | Return execution to `lr` | `0101` |
| `BC` | Conditional Branch | COND REG ADDR | Jumps to ADDR if REG satisfies COND | `0111` |
| `ADD` | Add | ACC TYPE ADD0 ADD1 | Signed Integer or Floating Point Addition | `1000`|
| `SUB` | Sub | ACC TYPE MIN SUB | Signed Integer or Floating Point Subtraction | `1001` |
| `MUL` | Mul | ACC TYPE MUL FAC | Signed Integer or Floating Point Multiplication | `1010` |
| `DIV` | Div | ACC TYPE DIVID DIVIS | Signed Integer or Floating Point Division | `1011` |
| `OR` | Or | ACC OR0 OR1 | Bitwise boolean or | `1100` |
| `AND` | And | ACC AND0 AND1 | Bitwise boolean and | `1101` |
| `NOT` | Not | ACC NOT | Bitwise boolean not | `1110` |
| `IO` | I/O Operation | Implementation Dependant | Interface the Implementation Specific IO | `1111` |

#### Load

Loads either a constant value or a value from memory into a register.

`[64:61]`: `0001`

`[60:58]`: The target register to load the value into.

`[57]`: `1` if loading from register `0` if constant value.

If `[57]` is set to `[1]`, bits `[56:54]` specify the register containing the address to load.

Otherwise, if `[57]` is set to 0, bit `[56]` specifies which half of the target register we want to load the constant to. We do this since we can't encode a 64-bit value in the remaining bits so, to load a constant value we must load the lower and upper halves separately. Bits `[55:24]` specify the 32-bit constant value.

#### Store

Stores a register value into a memory location specified by another register.

`[64:61]`: `0010`

`[60:58]`: The target register to store the value from.

`[57:55]`: The register containing the memory address to store the value to.

#### Branch

Switches execution to a new address.

`[64:61]`: `0100`

`[60:58]`: The register with the memory address to resume execution from.

#### Return

Returns execution to the link register.
`[64:61]`: `0101`

#### Conditional Branch
Branch to the address specified in a register if a test register meets the condition

`[64:61]`: `0111`

`[60:58]`: The register with the address to start execution from

`[57]`: Reserved
`[56:55]`: The condition to apply:
- `00` zero
- `01` positive
- `10` negative

#### Arithmetic Operations

`[64:61]`: Operation

`[60]`: Operation variant, `0` for integer, `1` for floating point

`[59:57]`: Target register

`[56]`: Input 1 type `0` for register, `1` for constant value.

`[55:29]`: Input 1. If `[56]` is `0`, `[55:53]` specify the register, otherwise, the whole range can be used for a constant value.

`[28]`: Input 2 type.
`[27:1]`: Input 1.

#### Binary Boolean Operations

`[64:61]`: Operation

`[60:58]`: Target register

`[57]`: Input 1 type, `0` for register, `1` for constant

`[56:29]`: Input 1. If `[56]` is `0`, `[55:53]` specify the register, otherwise, the whole range can be used for a constant value

`[28]`: Input 2 type

`[27:0]`: Input 2

#### Not
`[64:61]`: `1110`

`[60:58]`: Target register
`[57:55]`: Input register

#### IO
`[64:61]`: `1111`

This calls the implementation defined IO function.
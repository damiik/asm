type c6502_opcode = 
  | ADC  | AND  | ASL  | BCC  | BCS  | BEQ  | BIT  | BMI  | BNE  | BPL  | BRK  | BVC  | BVS  | CLC  | CLD  | CLI  | CLV
  | CMP  | CPX  | CPY  | DEC  | DEX  | DEY  | EOR  | INC  | INX  | INY  | JMP  | JSR  | LDA  | LDX  | LDY  | LSR  | NOP
  | ORA  | PHA  | PHP  | PLA  | PLP  | ROL  | ROR  | RTI  | RTS  | SBC  | SEC  | SED  | SEI  | STA  | STX  | STY  | TAX
  | TAY  | TSX  | TXA  | TXS  | TYA

type address_mode = 
  | Immediate         (* one byte *)
  | Relative          (* one byte *)
  | ZeroPage          (* one byte *)
  | ZeroPageXIndexed  (* one byte *)
  | ZeroPageYIndexed  (* one byte *)
  | XIndexedIndirect  (* one byte *)
  | IndirectYIndexed  (* one byte *)
  | Absolute          (* two bytes *)
  | AbsoluteXIndexed  (* two bytes *)
  | AbsoluteYIndexed  (* two bytes *)
  | Indirect          (* two bytes *)
  | Accumulator
  | Implicit


type address_mode_desc = {
  
  mode: address_mode; 
  desc: string;
  size: int
}

let mkAdrrMod m s d = {mode= m; size = s; desc = d}
let address_mode_descs: address_mode_desc list = [

  (mkAdrrMod Immediate        2 "#");
  (mkAdrrMod ZeroPage         2 "$44");
  (mkAdrrMod ZeroPageXIndexed 2 "$44,X");
  (mkAdrrMod ZeroPageYIndexed 2 "$44,X");
  (mkAdrrMod Absolute         3 "$4400");
  (mkAdrrMod AbsoluteXIndexed 3 "$4400,X");
  (mkAdrrMod AbsoluteYIndexed 3 "$4400,Y");
  (mkAdrrMod XIndexedIndirect 2 "($44, X)");
  (mkAdrrMod IndirectYIndexed 2 "($44),Y");
  (mkAdrrMod Indirect         2 "($5597)");
  (mkAdrrMod Relative         2 "label");
  (mkAdrrMod Accumulator      1 "A");
  (mkAdrrMod Implicit         1 "");
]

type instruction = {

  opcode   : c6502_opcode;
  desc     : string;
  addrmode : address_mode;
  code     : int;
}

let mkInstr o d a c = {opcode=o; desc=d; addrmode=a; code=c}
let instructions : instruction list = [
    (mkInstr BRK "BRK" Implicit         0);
    (mkInstr ORA "ORA" XIndexedIndirect 1);
    (mkInstr ORA "ORA" ZeroPage         5);
    (mkInstr ASL "ASL" ZeroPage         6);
    (mkInstr PHP "PHP" Implicit         8);
    (mkInstr ORA "ORA" Immediate        9);
    (mkInstr ASL "ASL" Accumulator      10);
    (mkInstr ORA "ORA" Absolute         13);
    (mkInstr ASL "ASL" Absolute         14);
    (mkInstr BPL "BPL" Relative         16);
    (mkInstr ORA "ORA" IndirectYIndexed 17);
    (mkInstr ORA "ORA" ZeroPageXIndexed 21);
    (mkInstr ASL "ASL" ZeroPageXIndexed 22);
    (mkInstr CLC "CLC" Implicit         24);
    (mkInstr ORA "ORA" AbsoluteYIndexed 25);
    (mkInstr ORA "ORA" AbsoluteXIndexed 29);
    (mkInstr ASL "ASL" AbsoluteXIndexed 30);
    (mkInstr JSR "JSR" Absolute         32);
    (mkInstr AND "AND" XIndexedIndirect 33);
    (mkInstr BIT "BIT" ZeroPage         36);
    (mkInstr AND "AND" ZeroPage         37);
    (mkInstr ROL "ROL" ZeroPage         38);
    (mkInstr PLP "PLP" Implicit         40);
    (mkInstr AND "AND" Immediate        41);
    (mkInstr ROL "ROL" Accumulator      42);
    (mkInstr BIT "BIT" Absolute         44);
    (mkInstr AND "AND" Absolute         45);
    (mkInstr ROL "ROL" Absolute         46);
    (mkInstr BMI "BMI" Relative         48);
    (mkInstr AND "AND" IndirectYIndexed 49);
    (mkInstr AND "AND" ZeroPageXIndexed 53);
    (mkInstr ROL "ROL" ZeroPageXIndexed 54);
    (mkInstr SEC "SEC" Implicit         56);
    (mkInstr AND "AND" AbsoluteYIndexed 57);
    (mkInstr AND "AND" AbsoluteXIndexed 61);
    (mkInstr ROL "ROL" AbsoluteXIndexed 62);
    (mkInstr RTI "RTI" Implicit         64);
    (mkInstr EOR "EOR" XIndexedIndirect 65);
    (mkInstr EOR "EOR" ZeroPage         69);
    (mkInstr LSR "LSR" ZeroPage         70);
    (mkInstr PHA "PHA" Implicit         72);
    (mkInstr EOR "EOR" Immediate        73);
    (mkInstr LSR "LSR" Accumulator      74);
    (mkInstr JMP "JMP" Absolute         76);
    (mkInstr EOR "EOR" Absolute         77);
    (mkInstr LSR "LSR" Absolute         78);
    (mkInstr BVC "BVC" Relative         80);
    (mkInstr EOR "EOR" IndirectYIndexed 81);
    (mkInstr EOR "EOR" ZeroPageXIndexed 85);
    (mkInstr LSR "LSR" ZeroPageXIndexed 86);
    (mkInstr CLI "CLI" Implicit         88);
    (mkInstr EOR "EOR" AbsoluteYIndexed 89);
    (mkInstr EOR "EOR" AbsoluteXIndexed 93);
    (mkInstr LSR "LSR" AbsoluteXIndexed 94);
    (mkInstr RTS "RTS" Implicit         96);
    (mkInstr ADC "ADC" XIndexedIndirect 97);
    (mkInstr ADC "ADC" ZeroPage         101);
    (mkInstr ROR "ROR" ZeroPage         102);
    (mkInstr PLA "PLA" Implicit         104);
    (mkInstr ADC "ADC" Immediate        105);
    (mkInstr ROR "ROR" Accumulator      106);
    (mkInstr JMP "JMP" Indirect         108);
    (mkInstr ADC "ADC" Absolute         109);
    (mkInstr ROR "ROR" Absolute         110);
    (mkInstr BVS "BVS" Relative         112);
    (mkInstr ADC "ADC" IndirectYIndexed 113);
    (mkInstr ADC "ADC" ZeroPageXIndexed 117);
    (mkInstr ROR "ROR" ZeroPageXIndexed 118);
    (mkInstr SEI "SEI" Implicit         120);
    (mkInstr ADC "ADC" AbsoluteYIndexed 121);
    (mkInstr ADC "ADC" AbsoluteXIndexed 125);
    (mkInstr ROR "ROR" AbsoluteXIndexed 126);
    (mkInstr STA "STA" XIndexedIndirect 129);
    (mkInstr STY "STY" ZeroPage         132);
    (mkInstr STA "STA" ZeroPage         133);
    (mkInstr STX "STX" ZeroPage         134);
    (mkInstr DEY "DEY" Implicit         136);
    (mkInstr TXA "TXA" Implicit         138);
    (mkInstr STY "STY" Absolute         140);
    (mkInstr STA "STA" Absolute         141);
    (mkInstr STX "STX" Absolute         142);
    (mkInstr BCC "BCC" Relative         144);
    (mkInstr STA "STA" IndirectYIndexed 145);
    (mkInstr STY "STY" ZeroPageXIndexed 148);
    (mkInstr STA "STA" ZeroPageXIndexed 149);
    (mkInstr STX "STX" ZeroPageYIndexed 150);
    (mkInstr TYA "TYA" Implicit         152);
    (mkInstr STA "STA" AbsoluteYIndexed 153);
    (mkInstr TXS "TXS" Implicit         154);
    (mkInstr STA "STA" AbsoluteXIndexed 157);
    (mkInstr LDY "LDY" Immediate        160);
    (mkInstr LDA "LDA" XIndexedIndirect 161);
    (mkInstr LDX "LDX" Immediate        162);
    (mkInstr LDY "LDY" ZeroPage         164);
    (mkInstr LDA "LDA" ZeroPage         165);
    (mkInstr LDX "LDX" ZeroPage         166);
    (mkInstr TAY "TAY" Implicit         168);
    (mkInstr LDA "LDA" Immediate        169);
    (mkInstr TAX "TAX" Implicit         170);
    (mkInstr LDY "LDY" Absolute         172);
    (mkInstr LDA "LDA" Absolute         173);
    (mkInstr LDX "LDX" Absolute         174);
    (mkInstr BCS "BCS" Relative         176);
    (mkInstr LDA "LDA" IndirectYIndexed 177);
    (mkInstr LDY "LDY" ZeroPageXIndexed 180);
    (mkInstr LDA "LDA" ZeroPageXIndexed 181);
    (mkInstr LDX "LDX" ZeroPageYIndexed 182);
    (mkInstr CLV "CLV" Implicit         184);
    (mkInstr LDA "LDA" AbsoluteYIndexed 185);
    (mkInstr TSX "TSX" Implicit         186);
    (mkInstr LDY "LDY" AbsoluteXIndexed 188);
    (mkInstr LDA "LDA" AbsoluteXIndexed 189);
    (mkInstr LDX "LDX" AbsoluteYIndexed 190);
    (mkInstr CPY "CPY" Immediate        192);
    (mkInstr CMP "CMP" XIndexedIndirect 193);
    (mkInstr CPY "CPY" ZeroPage         196);
    (mkInstr CMP "CMP" ZeroPage         197);
    (mkInstr DEC "DEC" ZeroPage         198);
    (mkInstr INY "INY" Implicit         200);
    (mkInstr CMP "CMP" Immediate        201);
    (mkInstr DEX "DEX" Implicit         202);
    (mkInstr CPY "CPY" Absolute         204);
    (mkInstr CMP "CMP" Absolute         205);
    (mkInstr DEC "DEC" Absolute         206);
    (mkInstr BNE "BNE" Relative         208);
    (mkInstr CMP "CMP" IndirectYIndexed 209);
    (mkInstr CMP "CMP" ZeroPageXIndexed 213);
    (mkInstr DEC "DEC" ZeroPageXIndexed 214);
    (mkInstr CLD "CLD" Implicit         216);
    (mkInstr CMP "CMP" AbsoluteYIndexed 217);
    (mkInstr CMP "CMP" AbsoluteXIndexed 221);
    (mkInstr DEC "DEC" AbsoluteXIndexed 222);
    (mkInstr CPX "CPX" Immediate        224);
    (mkInstr SBC "SBC" XIndexedIndirect 225);
    (mkInstr CPX "CPX" ZeroPage         228);
    (mkInstr SBC "SBC" ZeroPage         229);
    (mkInstr INC "INC" ZeroPage         230);
    (mkInstr INX "INX" Implicit         232);
    (mkInstr SBC "SBC" Immediate        233);
    (mkInstr NOP "NOP" Implicit         234);
    (mkInstr CPX "CPX" Absolute         236);
    (mkInstr SBC "SBC" Absolute         237);
    (mkInstr INC "INC" Absolute         238);
    (mkInstr BEQ "BEQ" Relative         240);
    (mkInstr SBC "SBC" IndirectYIndexed 241);
    (mkInstr SBC "SBC" ZeroPageXIndexed 245);
    (mkInstr INC "INC" ZeroPageXIndexed 246);
    (mkInstr SED "SED" Implicit         248);
    (mkInstr SBC "SBC" AbsoluteYIndexed 249);
    (mkInstr SBC "SBC" AbsoluteXIndexed 253);
    (mkInstr INC "INC" AbsoluteXIndexed 254);
]
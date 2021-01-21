type c6502_opcode = 
  | ADC
  | AND
  | ASL
  | BCC
  | BCS
  | BEQ
  | BIT
  | BMI
  | BNE
  | BPL
  | BRK
  | BVC
  | BVS
  | CLC
  | CLD
  | CLI
  | CLV
  | CMP
  | CPX
  | CPY
  | DEC
  | DEX
  | DEY
  | EOR
  | INC
  | INX
  | INY
  | JMP
  | JSR
  | LDA
  | LDX
  | LDY
  | LSR
  | NOP
  | ORA
  | PHA
  | PHP
  | PLA
  | PLP
  | ROL
  | ROR
  | RTI
  | RTS
  | SBC
  | SEC
  | SED
  | SEI
  | STA
  | STX
  | STY
  | TAX
  | TAY
  | TSX
  | TXA
  | TXS
  | TYA


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


type address_modes_desc = {
  
  mode: address_mode; 
  desc: string
}


type instruction = {

  opcode   : c6502_opcode;
  desc     : string;
  addrmode : address_mode;
  code     : int;
  size     : int
}

let address_modes_descs: address_modes_desc list = [

  {mode= Immediate; desc = "#"};
  {mode= ZeroPage; desc = "$44"};
  {mode= ZeroPageXIndexed; desc = "$44,X"};
  {mode= ZeroPageYIndexed; desc = "$44,X"};
  {mode= Absolute; desc = "$4400"};
  {mode= AbsoluteXIndexed; desc = "$4400,X"};
  {mode= AbsoluteYIndexed; desc = "$4400,Y"};
  {mode= XIndexedIndirect; desc = "($44, X)"};
  {mode= IndirectYIndexed; desc = "($44),Y"};
  {mode= Indirect; desc = "($5597)"};
  {mode= Relative; desc = "label"};
  {mode= Accumulator; desc = "A"};
  {mode= Implicit; desc = ""};
]

let instructions : instruction list = [
                {opcode=BRK; desc="BRK"; addrmode=Implicit; code=0; size = 1};
                {opcode=ORA; desc="ORA"; addrmode=XIndexedIndirect; code=1; size = 2};
                {opcode=ORA; desc="ORA"; addrmode=ZeroPage; code=5; size = 2};
                {opcode=ASL; desc="ASL"; addrmode=ZeroPage; code=6; size = 2};
                {opcode=PHP; desc="PHP"; addrmode=Implicit; code=8; size = 1};
                {opcode=ORA; desc="ORA"; addrmode=Immediate; code=9; size = 2};
                {opcode=ASL; desc="ASL"; addrmode=Accumulator; code=10; size = 1};
                {opcode=ORA; desc="ORA"; addrmode=Absolute; code=13; size = 3};
                {opcode=ASL; desc="ASL"; addrmode=Absolute; code=14; size = 3};
                {opcode=BPL; desc="BPL"; addrmode=Relative; code=16; size = 2};
                {opcode=ORA; desc="ORA"; addrmode=IndirectYIndexed; code=17; size =2};
                {opcode=ORA; desc="ORA"; addrmode=ZeroPageXIndexed; code=21; size = 2};
                {opcode=ASL; desc="ASL"; addrmode=ZeroPageXIndexed; code=22; size = 2};
                {opcode=CLC; desc="CLC"; addrmode=Implicit; code=24; size = 1};
                {opcode=ORA; desc="ORA"; addrmode=AbsoluteYIndexed; code=25; size = 3};
                {opcode=ORA; desc="ORA"; addrmode=AbsoluteXIndexed; code=29; size = 3};
                {opcode=ASL; desc="ASL"; addrmode=AbsoluteXIndexed; code=30; size = 3};
                {opcode=JSR; desc="JSR"; addrmode=Absolute; code=32; size = 3};
                {opcode=AND; desc="AND"; addrmode=XIndexedIndirect; code=33; size = 2};
                {opcode=BIT; desc="BIT"; addrmode=ZeroPage; code=36; size = 2};
                {opcode=AND; desc="AND"; addrmode=ZeroPage; code=37; size = 2};
                {opcode=ROL; desc="ROL"; addrmode=ZeroPage; code=38; size = 2};
                {opcode=PLP; desc="PLP"; addrmode=Implicit; code=40; size = 1};
                {opcode=AND; desc="AND"; addrmode=Immediate; code=41; size = 2};
                {opcode=ROL; desc="ROL"; addrmode=Accumulator; code=42; size = 1};
                {opcode=BIT; desc="BIT"; addrmode=Absolute; code=44; size = 3};
                {opcode=AND; desc="AND"; addrmode=Absolute; code=45; size = 3};
                {opcode=ROL; desc="ROL"; addrmode=Absolute; code=46; size = 3};
                {opcode=BMI; desc="BMI"; addrmode=Relative; code=48; size = 2};
                {opcode=AND; desc="AND"; addrmode=IndirectYIndexed; code=49; size = 2};
                {opcode=AND; desc="AND"; addrmode=ZeroPageXIndexed; code=53; size = 2};
                {opcode=ROL; desc="ROL"; addrmode=ZeroPageXIndexed; code=54; size = 2};
                {opcode=SEC; desc="SEC"; addrmode=Implicit; code=56; size = 1};
                {opcode=AND; desc="AND"; addrmode=AbsoluteYIndexed; code=57; size = 3};
                {opcode=AND; desc="AND"; addrmode=AbsoluteXIndexed; code=61; size = 3};
                {opcode=ROL; desc="ROL"; addrmode=AbsoluteXIndexed; code=62; size = 3};
                {opcode=RTI; desc="RTI"; addrmode=Implicit; code=64; size = 1};
                {opcode=EOR; desc="EOR"; addrmode=XIndexedIndirect; code=65; size = 2};
                {opcode=EOR; desc="EOR"; addrmode=ZeroPage; code=69; size = 2};
                {opcode=LSR; desc="LSR"; addrmode=ZeroPage; code=70; size = 2};
                {opcode=PHA; desc="PHA"; addrmode=Implicit; code=72; size = 1};
                {opcode=EOR; desc="EOR"; addrmode=Immediate; code=73; size = 2};
                {opcode=LSR; desc="LSR"; addrmode=Accumulator; code=74; size = 1};
                {opcode=JMP; desc="JMP"; addrmode=Absolute; code=76; size = 3};
                {opcode=EOR; desc="EOR"; addrmode=Absolute; code=77; size = 3};
                {opcode=LSR; desc="LSR"; addrmode=Absolute; code=78; size = 3};
                {opcode=BVC; desc="BVC"; addrmode=Relative; code=80; size = 2};
                {opcode=EOR; desc="EOR"; addrmode=IndirectYIndexed; code=81; size = 2};
                {opcode=EOR; desc="EOR"; addrmode=ZeroPageXIndexed; code=85; size = 2};
                {opcode=LSR; desc="LSR"; addrmode=ZeroPageXIndexed; code=86; size = 2};
                {opcode=CLI; desc="CLI"; addrmode=Implicit; code=88; size = 1};
                {opcode=EOR; desc="EOR"; addrmode=AbsoluteYIndexed; code=89; size = 3};
                {opcode=EOR; desc="EOR"; addrmode=AbsoluteXIndexed; code=93; size = 3};
                {opcode=LSR; desc="LSR"; addrmode=AbsoluteXIndexed; code=94; size = 3};
                {opcode=RTS; desc="RTS"; addrmode=Implicit; code=96; size = 1};
                {opcode=ADC; desc="ADC"; addrmode=XIndexedIndirect; code=97; size = 2};
                {opcode=ADC; desc="ADC"; addrmode=ZeroPage; code=101; size = 2};
                {opcode=ROR; desc="ROR"; addrmode=ZeroPage; code=102; size = 2};
                {opcode=PLA; desc="PLA"; addrmode=Implicit; code=104; size = 1};
                {opcode=ADC; desc="ADC"; addrmode=Immediate; code=105; size = 2};
                {opcode=ROR; desc="ROR"; addrmode=Accumulator; code=106; size = 1};
                {opcode=JMP; desc="JMP"; addrmode=Indirect; code=108; size = 3};
                {opcode=ADC; desc="ADC"; addrmode=Absolute; code=109; size = 3};
                {opcode=ROR; desc="ROR"; addrmode=Absolute; code=110; size = 3};
                {opcode=BVS; desc="BVS"; addrmode=Relative; code=112; size = 2};
                {opcode=ADC; desc="ADC"; addrmode=IndirectYIndexed; code=113; size = 2};
                {opcode=ADC; desc="ADC"; addrmode=ZeroPageXIndexed; code=117; size = 2};
                {opcode=ROR; desc="ROR"; addrmode=ZeroPageXIndexed; code=118; size = 2};
                {opcode=SEI; desc="SEI"; addrmode=Implicit; code=120; size = 1};
                {opcode=ADC; desc="ADC"; addrmode=AbsoluteYIndexed; code=121; size = 3};
                {opcode=ADC; desc="ADC"; addrmode=AbsoluteXIndexed; code=125; size = 3};
                {opcode=ROR; desc="ROR"; addrmode=AbsoluteXIndexed; code=126; size = 3};
                {opcode=STA; desc="STA"; addrmode=XIndexedIndirect; code=129; size = 2};
                {opcode=STY; desc="STY"; addrmode=ZeroPage; code=132; size = 2};
                {opcode=STA; desc="STA"; addrmode=ZeroPage; code=133; size = 2};
                {opcode=STX; desc="STX"; addrmode=ZeroPage; code=134; size = 2};
                {opcode=DEY; desc="DEY"; addrmode=Implicit; code=136; size = 1};
                {opcode=TXA; desc="TXA"; addrmode=Implicit; code=138; size = 1};
                {opcode=STY; desc="STY"; addrmode=Absolute; code=140; size = 3};
                {opcode=STA; desc="STA"; addrmode=Absolute; code=141; size = 3};
                {opcode=STX; desc="STX"; addrmode=Absolute; code=142; size = 3};
                {opcode=BCC; desc="BCC"; addrmode=Relative; code=144; size = 2};
                {opcode=STA; desc="STA"; addrmode=IndirectYIndexed; code=145; size = 2};
                {opcode=STY; desc="STY"; addrmode=ZeroPageXIndexed; code=148; size = 2};
                {opcode=STA; desc="STA"; addrmode=ZeroPageXIndexed; code=149; size = 2};
                {opcode=STX; desc="STX"; addrmode=ZeroPageYIndexed; code=150; size = 2};
                {opcode=TYA; desc="TYA"; addrmode=Implicit; code=152; size = 1};
                {opcode=STA; desc="STA"; addrmode=AbsoluteYIndexed; code=153; size = 3};
                {opcode=TXS; desc="TXS"; addrmode=Implicit; code=154; size = 1};
                {opcode=STA; desc="STA"; addrmode=AbsoluteXIndexed; code=157; size = 3};
                {opcode=LDY; desc="LDY"; addrmode=Immediate; code=160; size = 2};
                {opcode=LDA; desc="LDA"; addrmode=XIndexedIndirect; code=161; size = 2};
                {opcode=LDX; desc="LDX"; addrmode=Immediate; code=162; size = 2};
                {opcode=LDY; desc="LDY"; addrmode=ZeroPage; code=164; size = 2};
                {opcode=LDA; desc="LDA"; addrmode=ZeroPage; code=165; size = 2};
                {opcode=LDX; desc="LDX"; addrmode=ZeroPage; code=166; size = 2};
                {opcode=TAY; desc="TAY"; addrmode=Implicit; code=168; size = 1};
                {opcode=LDA; desc="LDA"; addrmode=Immediate; code=169; size = 2};
                {opcode=TAX; desc="TAX"; addrmode=Implicit; code=170; size = 1};
                {opcode=LDY; desc="LDY"; addrmode=Absolute; code=172; size = 3};
                {opcode=LDA; desc="LDA"; addrmode=Absolute; code=173; size = 3};
                {opcode=LDX; desc="LDX"; addrmode=Absolute; code=174; size = 3};
                {opcode=BCS; desc="BCS"; addrmode=Relative; code=176; size = 2};
                {opcode=LDA; desc="LDA"; addrmode=IndirectYIndexed; code=177; size = 2};
                {opcode=LDY; desc="LDY"; addrmode=ZeroPageXIndexed; code=180; size = 2};
                {opcode=LDA; desc="LDA"; addrmode=ZeroPageXIndexed; code=181; size = 2};
                {opcode=LDX; desc="LDX"; addrmode=ZeroPageYIndexed; code=182; size = 2};
                {opcode=CLV; desc="CLV"; addrmode=Implicit; code=184; size = 1};
                {opcode=LDA; desc="LDA"; addrmode=AbsoluteYIndexed; code=185; size = 3};
                {opcode=TSX; desc="TSX"; addrmode=Implicit; code=186; size = 1};
                {opcode=LDY; desc="LDY"; addrmode=AbsoluteXIndexed; code=188; size = 3};
                {opcode=LDA; desc="LDA"; addrmode=AbsoluteXIndexed; code=189; size = 3};
                {opcode=LDX; desc="LDX"; addrmode=AbsoluteYIndexed; code=190; size = 3};
                {opcode=CPY; desc="CPY"; addrmode=Immediate; code=192; size = 2};
                {opcode=CMP; desc="CMP"; addrmode=XIndexedIndirect; code=193; size = 2};
                {opcode=CPY; desc="CPY"; addrmode=ZeroPage; code=196; size = 2};
                {opcode=CMP; desc="CMP"; addrmode=ZeroPage; code=197; size = 2};
                {opcode=DEC; desc="DEC"; addrmode=ZeroPage; code=198; size = 2};
                {opcode=INY; desc="INY"; addrmode=Implicit; code=200; size = 1};
                {opcode=CMP; desc="CMP"; addrmode=Immediate; code=201; size = 2};
                {opcode=DEX; desc="DEX"; addrmode=Implicit; code=202; size = 1};
                {opcode=CPY; desc="CPY"; addrmode=Absolute; code=204; size = 3};
                {opcode=CMP; desc="CMP"; addrmode=Absolute; code=205; size = 3};
                {opcode=DEC; desc="DEC"; addrmode=Absolute; code=206; size = 3};
                {opcode=BNE; desc="BNE"; addrmode=Relative; code=208; size = 2};
                {opcode=CMP; desc="CMP"; addrmode=IndirectYIndexed; code=209; size = 2};
                {opcode=CMP; desc="CMP"; addrmode=ZeroPageXIndexed; code=213; size = 2};
                {opcode=DEC; desc="DEC"; addrmode=ZeroPageXIndexed; code=214; size = 2};
                {opcode=CLD; desc="CLD"; addrmode=Implicit; code=216; size = 1};
                {opcode=CMP; desc="CMP"; addrmode=AbsoluteYIndexed; code=217; size = 3};
                {opcode=CMP; desc="CMP"; addrmode=AbsoluteXIndexed; code=221; size = 3};
                {opcode=DEC; desc="DEC"; addrmode=AbsoluteXIndexed; code=222; size = 3};
                {opcode=CPX; desc="CPX"; addrmode=Immediate; code=224; size = 2};
                {opcode=SBC; desc="SBC"; addrmode=XIndexedIndirect; code=225; size = 2};
                {opcode=CPX; desc="CPX"; addrmode=ZeroPage; code=228; size = 2};
                {opcode=SBC; desc="SBC"; addrmode=ZeroPage; code=229; size = 2};
                {opcode=INC; desc="INC"; addrmode=ZeroPage; code=230; size = 2};
                {opcode=INX; desc="INX"; addrmode=Implicit; code=232; size = 1};
                {opcode=SBC; desc="SBC"; addrmode=Immediate; code=233; size = 2};
                {opcode=NOP; desc="NOP"; addrmode=Implicit; code=234; size = 1};
                {opcode=CPX; desc="CPX"; addrmode=Absolute; code=236; size = 3};
                {opcode=SBC; desc="SBC"; addrmode=Absolute; code=237; size = 3};
                {opcode=INC; desc="INC"; addrmode=Absolute; code=238; size = 3};
                {opcode=BEQ; desc="BEQ"; addrmode=Relative; code=240; size = 2};
                {opcode=SBC; desc="SBC"; addrmode=IndirectYIndexed; code=241; size = 2};
                {opcode=SBC; desc="SBC"; addrmode=ZeroPageXIndexed; code=245; size = 2};
                {opcode=INC; desc="INC"; addrmode=ZeroPageXIndexed; code=246; size = 2};
                {opcode=SED; desc="SED"; addrmode=Implicit; code=248; size = 1};
                {opcode=SBC; desc="SBC"; addrmode=AbsoluteYIndexed; code=249; size = 3};
                {opcode=SBC; desc="SBC"; addrmode=AbsoluteXIndexed; code=253; size = 3};
                {opcode=INC; desc="INC"; addrmode=AbsoluteXIndexed; code=254; size = 3};

]
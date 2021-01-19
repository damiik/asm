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


type addres_modes = 
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

type instruction = {

  opcode : c6502_opcode;
  addrmode: addres_modes;
  code: int;
  size: int
}

let instructions : instruction list = [
                {opcode=BRK; addrmode=Implicit; code=0; size = 1};
                {opcode=ORA; addrmode=XIndexedIndirect; code=1; size = 2};
                {opcode=ORA; addrmode=ZeroPage; code=5; size = 2};
                {opcode=ASL; addrmode=ZeroPage; code=6; size = 2};
                {opcode=PHP; addrmode=Implicit; code=8; size = 1};
                {opcode=ORA; addrmode=Immediate; code=9; size = 2};
                {opcode=ASL; addrmode=Accumulator; code=10; size = 1};
                {opcode=ORA; addrmode=Absolute; code=13; size = 3};
                {opcode=ASL; addrmode=Absolute; code=14; size = 3};
                {opcode=BPL; addrmode=Relative; code=16; size = 2};
                {opcode=ORA; addrmode=IndirectYIndexed; code=17; size =2};
                {opcode=ORA; addrmode=ZeroPageXIndexed; code=21; size = 2};
                {opcode=ASL; addrmode=ZeroPageXIndexed; code=22; size = 2};
                {opcode=CLC; addrmode=Implicit; code=24; size = 1};
                {opcode=ORA; addrmode=AbsoluteYIndexed; code=25; size = 3};
                {opcode=ORA; addrmode=AbsoluteXIndexed; code=29; size = 3};
                {opcode=ASL; addrmode=AbsoluteXIndexed; code=30; size = 3};
                {opcode=JSR; addrmode=Absolute; code=32; size = 3};
                {opcode=AND; addrmode=XIndexedIndirect; code=33; size = 2};
                {opcode=BIT; addrmode=ZeroPage; code=36; size = 2};
                {opcode=AND; addrmode=ZeroPage; code=37; size = 2};
                {opcode=ROL; addrmode=ZeroPage; code=38; size = 2};
                {opcode=PLP; addrmode=Implicit; code=40; size = 1};
                {opcode=AND; addrmode=Immediate; code=41; size = 2};
                {opcode=ROL; addrmode=Accumulator; code=42; size = 1};
                {opcode=BIT; addrmode=Absolute; code=44; size = 3};
                {opcode=AND; addrmode=Absolute; code=45; size = 3};
                {opcode=ROL; addrmode=Absolute; code=46; size = 3};
                {opcode=BMI; addrmode=Relative; code=48; size = 2};
                {opcode=AND; addrmode=IndirectYIndexed; code=49; size = 2};
                {opcode=AND; addrmode=ZeroPageXIndexed; code=53; size = 2};
                {opcode=ROL; addrmode=ZeroPageXIndexed; code=54; size = 2};
                {opcode=SEC; addrmode=Implicit; code=56; size = 1};
                {opcode=AND; addrmode=AbsoluteYIndexed; code=57; size = 3};
                {opcode=AND; addrmode=AbsoluteXIndexed; code=61; size = 3};
                {opcode=ROL; addrmode=AbsoluteXIndexed; code=62; size = 3};
                {opcode=RTI; addrmode=Implicit; code=64; size = 1};
                {opcode=EOR; addrmode=XIndexedIndirect; code=65; size = 2};
                {opcode=EOR; addrmode=ZeroPage; code=69; size = 2};
                {opcode=LSR; addrmode=ZeroPage; code=70; size = 2};
                {opcode=PHA; addrmode=Implicit; code=72; size = 1};
                {opcode=EOR; addrmode=Immediate; code=73; size = 2};
                {opcode=LSR; addrmode=Accumulator; code=74; size = 1};
                {opcode=JMP; addrmode=Absolute; code=76; size = 3};
                {opcode=EOR; addrmode=Absolute; code=77; size = 3};
                {opcode=LSR; addrmode=Absolute; code=78; size = 3};
                {opcode=BVC; addrmode=Relative; code=80; size = 2};
                {opcode=EOR; addrmode=IndirectYIndexed; code=81; size = 2};
                {opcode=EOR; addrmode=ZeroPageXIndexed; code=85; size = 2};
                {opcode=LSR; addrmode=ZeroPageXIndexed; code=86; size = 2};
                {opcode=CLI; addrmode=Implicit; code=88; size = 1};
                {opcode=EOR; addrmode=AbsoluteYIndexed; code=89; size = 3};
                {opcode=EOR; addrmode=AbsoluteXIndexed; code=93; size = 3};
                {opcode=LSR; addrmode=AbsoluteXIndexed; code=94; size = 3};
                {opcode=RTS; addrmode=Implicit; code=96; size = 1};
                {opcode=ADC; addrmode=XIndexedIndirect; code=97; size = 2};
                {opcode=ADC; addrmode=ZeroPage; code=101; size = 2};
                {opcode=ROR; addrmode=ZeroPage; code=102; size = 2};
                {opcode=PLA; addrmode=Implicit; code=104; size = 1};
                {opcode=ADC; addrmode=Immediate; code=105; size = 2};
                {opcode=ROR; addrmode=Accumulator; code=106; size = 1};
                {opcode=JMP; addrmode=Indirect; code=108; size = 3};
                {opcode=ADC; addrmode=Absolute; code=109; size = 3};
                {opcode=ROR; addrmode=Absolute; code=110; size = 3};
                {opcode=BVS; addrmode=Relative; code=112; size = 2};
                {opcode=ADC; addrmode=IndirectYIndexed; code=113; size = 2};
                {opcode=ADC; addrmode=ZeroPageXIndexed; code=117; size = 2};
                {opcode=ROR; addrmode=ZeroPageXIndexed; code=118; size = 2};
                {opcode=SEI; addrmode=Implicit; code=120; size = 1};
                {opcode=ADC; addrmode=AbsoluteYIndexed; code=121; size = 3};
                {opcode=ADC; addrmode=AbsoluteXIndexed; code=125; size = 3};
                {opcode=ROR; addrmode=AbsoluteXIndexed; code=126; size = 3};
                {opcode=STA; addrmode=XIndexedIndirect; code=129; size = 2};
                {opcode=STY; addrmode=ZeroPage; code=132; size = 2};
                {opcode=STA; addrmode=ZeroPage; code=133; size = 2};
                {opcode=STX; addrmode=ZeroPage; code=134; size = 2};
                {opcode=DEY; addrmode=Implicit; code=136; size = 1};
                {opcode=TXA; addrmode=Implicit; code=138; size = 1};
                {opcode=STY; addrmode=Absolute; code=140; size = 3};
                {opcode=STA; addrmode=Absolute; code=141; size = 3};
                {opcode=STX; addrmode=Absolute; code=142; size = 3};
                {opcode=BCC; addrmode=Relative; code=144; size = 2};
                {opcode=STA; addrmode=IndirectYIndexed; code=145; size = 2};
                {opcode=STY; addrmode=ZeroPageXIndexed; code=148; size = 2};
                {opcode=STA; addrmode=ZeroPageXIndexed; code=149; size = 2};
                {opcode=STX; addrmode=ZeroPageYIndexed; code=150; size = 2};
                {opcode=TYA; addrmode=Implicit; code=152; size = 1};
                {opcode=STA; addrmode=AbsoluteYIndexed; code=153; size = 3};
                {opcode=TXS; addrmode=Implicit; code=154; size = 1};
                {opcode=STA; addrmode=AbsoluteXIndexed; code=157; size = 3};
                {opcode=LDY; addrmode=Immediate; code=160; size = 2};
                {opcode=LDA; addrmode=XIndexedIndirect; code=161; size = 2};
                {opcode=LDX; addrmode=Immediate; code=162; size = 2};
                {opcode=LDY; addrmode=ZeroPage; code=164; size = 2};
                {opcode=LDA; addrmode=ZeroPage; code=165; size = 2};
                {opcode=LDX; addrmode=ZeroPage; code=166; size = 2};
                {opcode=TAY; addrmode=Implicit; code=168; size = 1};
                {opcode=LDA; addrmode=Immediate; code=169; size = 2};
                {opcode=TAX; addrmode=Implicit; code=170; size = 1};
                {opcode=LDY; addrmode=Absolute; code=172; size = 3};
                {opcode=LDA; addrmode=Absolute; code=173; size = 3};
                {opcode=LDX; addrmode=Absolute; code=174; size = 3};
                {opcode=BCS; addrmode=Relative; code=176; size = 2};
                {opcode=LDA; addrmode=IndirectYIndexed; code=177; size = 2};
                {opcode=LDY; addrmode=ZeroPageXIndexed; code=180; size = 2};
                {opcode=LDA; addrmode=ZeroPageXIndexed; code=181; size = 2};
                {opcode=LDX; addrmode=ZeroPageYIndexed; code=182; size = 2};
                {opcode=CLV; addrmode=Implicit; code=184; size = 1};
                {opcode=LDA; addrmode=AbsoluteYIndexed; code=185; size = 3};
                {opcode=TSX; addrmode=Implicit; code=186; size = 1};
                {opcode=LDY; addrmode=AbsoluteXIndexed; code=188; size = 3};
                {opcode=LDA; addrmode=AbsoluteXIndexed; code=189; size = 3};
                {opcode=LDX; addrmode=AbsoluteYIndexed; code=190; size = 3};
                {opcode=CPY; addrmode=Immediate; code=192; size = 2};
                {opcode=CMP; addrmode=XIndexedIndirect; code=193; size = 2};
                {opcode=CPY; addrmode=ZeroPage; code=196; size = 2};
                {opcode=CMP; addrmode=ZeroPage; code=197; size = 2};
                {opcode=DEC; addrmode=ZeroPage; code=198; size = 2};
                {opcode=INY; addrmode=Implicit; code=200; size = 1};
                {opcode=CMP; addrmode=Immediate; code=201; size = 2};
                {opcode=DEX; addrmode=Implicit; code=202; size = 1};
                {opcode=CPY; addrmode=Absolute; code=204; size = 3};
                {opcode=CMP; addrmode=Absolute; code=205; size = 3};
                {opcode=DEC; addrmode=Absolute; code=206; size = 3};
                {opcode=BNE; addrmode=Relative; code=208; size = 2};
                {opcode=CMP; addrmode=IndirectYIndexed; code=209; size = 2};
                {opcode=CMP; addrmode=ZeroPageXIndexed; code=213; size = 2};
                {opcode=DEC; addrmode=ZeroPageXIndexed; code=214; size = 2};
                {opcode=CLD; addrmode=Implicit; code=216; size = 1};
                {opcode=CMP; addrmode=AbsoluteYIndexed; code=217; size = 3};
                {opcode=CMP; addrmode=AbsoluteXIndexed; code=221; size = 3};
                {opcode=DEC; addrmode=AbsoluteXIndexed; code=222; size = 3};
                {opcode=CPX; addrmode=Immediate; code=224; size = 2};
                {opcode=SBC; addrmode=XIndexedIndirect; code=225; size = 2};
                {opcode=CPX; addrmode=ZeroPage; code=228; size = 2};
                {opcode=SBC; addrmode=ZeroPage; code=229; size = 2};
                {opcode=INC; addrmode=ZeroPage; code=230; size = 2};
                {opcode=INX; addrmode=Implicit; code=232; size = 1};
                {opcode=SBC; addrmode=Immediate; code=233; size = 2};
                {opcode=NOP; addrmode=Implicit; code=234; size = 1};
                {opcode=CPX; addrmode=Absolute; code=236; size = 3};
                {opcode=SBC; addrmode=Absolute; code=237; size = 3};
                {opcode=INC; addrmode=Absolute; code=238; size = 3};
                {opcode=BEQ; addrmode=Relative; code=240; size = 2};
                {opcode=SBC; addrmode=IndirectYIndexed; code=241; size = 2};
                {opcode=SBC; addrmode=ZeroPageXIndexed; code=245; size = 2};
                {opcode=INC; addrmode=ZeroPageXIndexed; code=246; size = 2};
                {opcode=SED; addrmode=Implicit; code=248; size = 1};
                {opcode=SBC; addrmode=AbsoluteYIndexed; code=249; size = 3};
                {opcode=SBC; addrmode=AbsoluteXIndexed; code=253; size = 3};
                {opcode=INC; addrmode=AbsoluteXIndexed; code=254; size = 3};

]
use crate::bus::Bus;

pub struct C6502 {
    acc: u8,
    x: u8,
    y: u8
}

impl C6502 {
    pub fn init() -> Self {
        C6502 {
            acc: 0,
            x: 0,
            y: 0,
        }
    }

    pub fn tick(self: &mut Self, bus: &mut Bus) {
        self.acc = 1;
        bus.write(15, self.acc)
    }
}

// Addressng Modes

fn acc() -> bool {
    unimplemented!()
}

fn imm() -> bool {
    unimplemented!()
}

fn abs() -> bool {
    unimplemented!()
}

fn zp0() -> bool {
    unimplemented!()
}

fn zpx() -> bool {
    unimplemented!()
}

fn zpy() -> bool {
    unimplemented!()
}

fn abx() -> bool {
    unimplemented!()
}

fn aby() -> bool {
    unimplemented!()
}

fn imp() -> bool {
    unimplemented!()
}

fn rel() -> bool {
    unimplemented!()
}

fn ind() -> bool {
    unimplemented!()
}

fn izx() -> bool {
    unimplemented!()
}

fn izy() -> bool {
    unimplemented!()
}

type AddrFn = fn() -> bool;
type InstrFn = fn() -> bool;
struct Instruction {
    name: String,
    instr_fn: InstrFn,
    addr_fn: AddrFn,
    cycles: u8,
}

fn init_instruction_table() -> [[Instruction; 16]; 16] {
    use super::instructions::*;
    fn op(name: &str, instr_fn: InstrFn, addr_fn: AddrFn, cycles: u8) -> Instruction {
        Instruction{name: name.into(), instr_fn: instr_fn, addr_fn: addr_fn, cycles: cycles }
    }
    [
        [op("BRK", brk, imp, 7), op("ORA", ora, izx, 6), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("???", xxx, imp, 3), op("ORA", ora, zp0, 3), op("ASL", asl, zp0, 5), op("???", xxx, imp, 5), op("PHP", php, imp, 3), op("ORA", ora, imm, 2), op("ASL", asl, imp, 2), op("???", xxx, imp, 2), op("???", xxx, abs, 4), op("ORA", ora, abs, 4), op("ASL", asl, abs, 6), op("???", xxx, imp, 6)],
        [op("BPL", bpl, rel, 2), op("ORA", ora, izy, 5), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("???", xxx, imp, 4), op("ORA", ora, zpx, 4), op("ASL", asl, zpx, 6), op("???", xxx, imp, 6), op("CLC", clc, imp, 2), op("ORA", ora, aby, 4), op("???", xxx, imp, 2), op("???", xxx, imp, 7), op("???", xxx, imp, 4), op("ORA", ora, abx, 4), op("ASL", asl, abx, 7), op("???", xxx, imp, 7)],
        [op("JSR", jsr, abs, 6), op("AND", and, izx, 6), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("BIT", bit, zp0, 3), op("AND", and, zp0, 3), op("ROL", rol, zp0, 5), op("???", xxx, imp, 5), op("PLP", plp, imp, 4), op("AND", and, imm, 2), op("ROL", rol, imp, 2), op("???", xxx, imp, 2), op("BIT", bit, abs, 4), op("AND", and, abs, 4), op("ROL", rol, abs, 6), op("???", xxx, imp, 6)],
        [op("BMI", bmi, rel, 2), op("AND", and, izy, 5), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("???", xxx, imp, 4), op("AND", and, zpx, 4), op("ROL", rol, zpx, 6), op("???", xxx, imp, 6), op("SEC", sec, imp, 2), op("AND", and, aby, 4), op("???", xxx, imp, 2), op("???", xxx, imp, 7), op("???", xxx, imp, 4), op("AND", and, abx, 4), op("ROL", rol, abx, 7), op("???", xxx, imp, 7)],
        [op("RTI", rti, imp, 6), op("EOR", eor, izx, 6), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("???", xxx, imp, 3), op("EOR", eor, zp0, 3), op("LSR", lsr, zp0, 5), op("???", xxx, imp, 5), op("PHA", pha, imp, 3), op("EOR", eor, imm, 2), op("LSR", lsr, imp, 2), op("???", xxx, imp, 2), op("JMP", jmp, abs, 3), op("EOR", eor, abs, 4), op("LSR", lsr, abs, 6), op("???", xxx, imp, 6)],
        [op("BVC", bvc, rel, 2), op("EOR", eor, izy, 5), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("???", xxx, imp, 4), op("EOR", eor, zpx, 4), op("LSR", lsr, zpx, 6), op("???", xxx, imp, 6), op("CLI", cli, imp, 2), op("EOR", eor, aby, 4), op("???", xxx, imp, 2), op("???", xxx, imp, 7), op("???", xxx, imp, 4), op("EOR", eor, abx, 4), op("LSR", lsr, abx, 7), op("???", xxx, imp, 7)],
        [op("RTS", rts, rel, 2), op("ADC", adc, izx, 6), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("???", xxx, imp, 3), op("ADC", adc, zp0, 3), op("ROR", ror, zp0, 5), op("???", xxx, imp, 5), op("PLA", pla, imp, 4), op("ADC", adc, imm, 2), op("ROR", ror, imp, 2), op("???", xxx, imp, 2), op("JMP", jmp, ind, 5), op("ADC", adc, abs, 4), op("ROR", ror, abs, 6), op("???", xxx, imp, 6)],
        [op("BVS", bvs, rel, 2), op("ADC", adc, izy, 5), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("???", xxx, imp, 4), op("ADC", adc, zpx, 4), op("ROR", ror, zpx, 6), op("???", xxx, imp, 6), op("SEI", sei, imp, 2), op("ADC", adc, aby, 4), op("???", xxx, imp, 2), op("???", xxx, imp, 7), op("???", xxx, imp, 4), op("ADC", adc, abx, 4), op("ROR", ror, abx, 7), op("???", xxx, imp, 7)],
        [op("???", xxx, imp, 2), op("STA", sta, izx, 6), op("???", xxx, imp, 2), op("???", xxx, imp, 6), op("STY", sty, zp0, 3), op("STA", sta, zp0, 3), op("STX", stx, zp0, 3), op("???", xxx, imp, 3), op("DEY", dey, imp, 2), op("???", xxx, imp, 2), op("TXA", txa, imp, 2), op("???", xxx, imp, 2), op("STY", sty, abs, 4), op("STA", sta, abs, 4), op("STX", stx, abs, 4), op("???", xxx, imp, 4)],
        [op("BCC", bcc, rel, 2), op("STA", sta, izy, 6), op("???", xxx, imp, 2), op("???", xxx, imp, 6), op("STY", sty, zpx, 4), op("STA", sta, zpx, 4), op("STX", stx, zpy, 4), op("???", xxx, imp, 4), op("TYA", tya, imp, 2), op("STA", sta, aby, 5), op("TXS", txs, imp, 2), op("???", xxx, imp, 5), op("???", xxx, imp, 5), op("STA", sta, abx, 5), op("???", xxx, imp, 5), op("???", xxx, imp, 5)],
        [op("LDY", ldy, imm, 2), op("LDA", lda, izx, 6), op("LDX", ldx, imm, 2), op("???", xxx, imp, 6), op("LDY", ldy, zp0, 3), op("LDA", lda, zp0, 3), op("LDX", ldx, zp0, 3), op("???", xxx, zp0, 3), op("TAY", tay, imp, 2), op("LDA", lda, imm, 2), op("TAX", tax, imp, 2), op("???", xxx, imp, 2), op("LDY", ldy, abs, 4), op("LDA", lda, abs, 4), op("LDX", ldx, abs, 4), op("???", xxx, imp, 4)],
        [op("BCS", bcs, rel, 2), op("LDA", lda, izy, 5), op("???", xxx, imp, 2), op("???", xxx, imp, 6), op("LDY", ldy, zpx, 4), op("LDA", lda, zpx, 4), op("LDX", ldx, zpy, 4), op("???", xxx, imp, 4), op("CLV", clv, imp, 2), op("LDA", lda, aby, 4), op("TSX", tsx, imp, 2), op("???", xxx, imp, 4), op("LDY", ldy, abx, 4), op("LDA", lda, abx, 4), op("LDX", ldx, aby, 4), op("???", xxx, imp, 4)],
        [op("CPY", cpy, imm, 2), op("CMP", cmp, izx, 6), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("CPY", cpy, zp0, 3), op("CMP", cmp, zp0, 3), op("DEC", dec, zp0, 5), op("???", xxx, imp, 5), op("INY", iny, imp, 2), op("CMP", cmp, imm, 2), op("DEX", dex, imp, 2), op("???", xxx, imp, 2), op("CPY", cpy, abs, 4), op("CMP", cmp, abs, 4), op("DEC", dec, abs, 6), op("???", xxx, imp, 6)],
        [op("BNE", bne, rel, 2), op("CMP", cmp, izy, 5), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("???", xxx, imp, 4), op("CMP", cmp, zpx, 4), op("DEC", dec, zpx, 6), op("???", xxx, imp, 6), op("CLD", cld, imp, 2), op("CMP", cmp, aby, 4), op("???", xxx, imp, 2), op("???", xxx, imp, 7), op("???", xxx, imp, 4), op("CMP", cmp, abx, 4), op("DEC", dec, abx, 7), op("???", xxx, imp, 7)],
        [op("CPX", cpx, imm, 2), op("SBC", sbc, izx, 6), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("CPX", cpx, zp0, 3), op("SBC", sbc, zp0, 3), op("INC", inc, zp0, 5), op("???", xxx, imp, 5), op("INX", inx, imp, 2), op("SBC", sbc, imm, 2), op("NOP", nop, imp, 2), op("???", xxx, imp, 2), op("CPX", cpx, abs, 4), op("SBC", sbc, abs, 4), op("INC", inc, abs, 6), op("???", xxx, imp, 6)],
        [op("BEQ", beq, rel, 2), op("SBC", sbc, izy, 5), op("???", xxx, imp, 2), op("???", xxx, imp, 8), op("???", xxx, imp, 4), op("SBC", sbc, zpx, 4), op("INC", inc, zpx, 6), op("???", xxx, imp, 6), op("SED", sed, imp, 2), op("SBC", sbc, aby, 4), op("???", xxx, imp, 2), op("???", xxx, imp, 7), op("???", xxx, imp, 4), op("SBC", sbc, abx, 4), op("INC", inc, abx, 7), op("???", xxx, imp, 7)],
    ] 
}
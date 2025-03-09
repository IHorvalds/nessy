use crate::cpu::AddressingMode;
use std::collections::HashMap;

#[derive(Debug)]
pub struct OpCode {
    pub code: u8,
    pub mnemonic: &'static str,
    pub instruction_len: u8,
    pub cycle_count: u8,
    pub addressing_mode: AddressingMode,
}

impl OpCode {
    pub fn new(
        code: u8,
        mnemonic: &'static str,
        instruction_len: u8,
        cycle_count: u8,
        addressing_mode: AddressingMode,
    ) -> Self {
        OpCode {
            code,
            mnemonic,
            instruction_len,
            cycle_count,
            addressing_mode,
        }
    }
}

lazy_static::lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
        OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing),

        OpCode::new(0xaa, "TAX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xaa, "TAY", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xBA, "TSX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x8A, "TXA", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x9A, "TXS", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x98, "TYA", 1, 2, AddressingMode::NoneAddressing),

        OpCode::new(0xe8, "INX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xe8, "INY", 1, 2, AddressingMode::NoneAddressing),

        OpCode::new(0xe6, "INC", 2, 5, AddressingMode::Absolute),
        OpCode::new(0xf6, "INC", 2, 6, AddressingMode::Absolute),
        OpCode::new(0xee, "INC", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xfe, "INC", 3, 7, AddressingMode::Absolute),

        OpCode::new(0xC6, "DEC", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xD6, "DEC", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0xCE, "DEC", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xDE, "DEC", 3, 7, AddressingMode::Absolute_X),

        OpCode::new(0xca, "DEX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x88, "DEY", 1, 2, AddressingMode::NoneAddressing),

        OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xbd, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(0xb9, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xb1, "LDA", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(0xa2,"LDX", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa6,"LDX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb6,"LDX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(0xae,"LDX", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xbe,"LDX", 3, 4/*+1 if page crossed */, AddressingMode::Absolute_Y),

        OpCode::new(0xa0, "LDY", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa4, "LDY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb4, "LDY", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xac, "LDY", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xbc, "LDY", 3, 4/*+1 if page crossed */, AddressingMode::Absolute_X),

        OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x9d, "STA", 3, 5, AddressingMode::Absolute_X),
        OpCode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_Y),
        OpCode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y),

        OpCode::new(0x86, "STX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x96, "STX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(0x8e, "STX", 3, 4, AddressingMode::Absolute),

        OpCode::new(0x84, "STY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x94, "STY", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x8c, "STY", 3, 4, AddressingMode::Absolute),

        OpCode::new(0x4c, "JMP", 3, 3, AddressingMode::Absolute),
        OpCode::new(0x6c, "JMP", 3, 5, AddressingMode::Indirect),

        OpCode::new(0x20, "JSR", 3, 6, AddressingMode::Absolute),

        OpCode::new(0x60, "RTS", 1, 6, AddressingMode::NoneAddressing),

        OpCode::new(0x4A, "LSR", 1, 2, AddressingMode::Accumulator),
        OpCode::new(0x46, "LSR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x56, "LSR", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x4E, "LSR", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x5E, "LSR", 4, 7, AddressingMode::Absolute_X),

        OpCode::new(0x0A, "ASL", 1, 2, AddressingMode::Accumulator),
        OpCode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x0E, "ASL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x1E, "ASL", 3, 7, AddressingMode::Absolute_X),

        OpCode::new(0x2A, "ROL", 1, 2, AddressingMode::Accumulator),
        OpCode::new(0x26, "ROL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x36, "ROL", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x2E, "ROL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x3E, "ROL", 4, 7, AddressingMode::Absolute_X),

        OpCode::new(0x6A, "ROR", 1, 2, AddressingMode::Accumulator),
        OpCode::new(0x66, "ROR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x76, "ROR", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x6E, "ROR", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x7E, "ROR", 3, 7, AddressingMode::Absolute_X),

        OpCode::new(0x49, "EOR", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x45, "EOR", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x55, "EOR", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x4D, "EOR", 2, 4, AddressingMode::Absolute),
        OpCode::new(0x5D, "EOR", 2, 4/*+1 if page crossed */, AddressingMode::Absolute_X),
        OpCode::new(0x59, "EOR", 2, 4/*+1 if page crossed */, AddressingMode::Absolute_Y),
        OpCode::new(0x41, "EOR", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x51, "EOR", 2, 5/*+1 if page crossed */, AddressingMode::Indirect_Y),

        OpCode::new(0x09, "ORA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x05, "ORA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x15, "ORA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x0D, "ORA", 2, 4, AddressingMode::Absolute),
        OpCode::new(0x1D, "ORA", 2, 4/*+1 if page crossed */, AddressingMode::Absolute_X),
        OpCode::new(0x19, "ORA", 2, 4/*+1 if page crossed */, AddressingMode::Absolute_Y),
        OpCode::new(0x01, "ORA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x11, "ORA", 2, 5/*+1 if page crossed */, AddressingMode::Indirect_Y),

        OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x2D, "AND", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x3D, "AND", 3, 4/*+1 if page crossed */, AddressingMode::Absolute_X),
        OpCode::new(0x39, "AND", 3, 4/*+1 if page crossed */, AddressingMode::Absolute_Y),
        OpCode::new(0x21, "AND", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x31, "AND", 2, 5/*+1 if page crossed */, AddressingMode::Indirect_Y),

        OpCode::new(0x48, "PHA", 1, 3, AddressingMode::NoneAddressing),
        OpCode::new(0x08, "PHP", 1, 3, AddressingMode::NoneAddressing),
        OpCode::new(0x68, "PLA", 1, 3, AddressingMode::NoneAddressing),
        OpCode::new(0x28, "PLP", 1, 3, AddressingMode::NoneAddressing),

        OpCode::new(0xC9, "CMP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xC5, "CMP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xD5, "CMP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xCD, "CMP", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xDD, "CMP", 2, 4/*+1 if page crossed */, AddressingMode::Absolute_X),
        OpCode::new(0xD9, "CMP", 2, 4/*+1 if page crossed */, AddressingMode::Absolute_Y),
        OpCode::new(0xC1, "CMP", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xD1, "CMP", 2, 5/*+1 if page crossed */, AddressingMode::Indirect_Y),

        OpCode::new(0xE0, "CPX", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xE4, "CPX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xEC, "CPX", 3, 4, AddressingMode::Absolute),

        OpCode::new(0xC0, "CPY", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xC4, "CPY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xCC, "CPY", 3, 4, AddressingMode::Absolute),

        OpCode::new(0xB8, "CLV", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x58, "CLI", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x18, "CLC", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xD8, "CLD", 1, 2, AddressingMode::NoneAddressing),

        OpCode::new(0x38, "SEC", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xF8, "SED", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x78, "SEI", 1, 2, AddressingMode::NoneAddressing),

        OpCode::new(0x65, "ADC", 2, 4, AddressingMode::Immediate),
        OpCode::new(0x69, "ADC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x6D, "ADC", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x7D, "ADC", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(0x79, "ADC", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(0x61, "ADC", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x71, "ADC", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(0xE9, "SBC", 2, 4, AddressingMode::Immediate),
        OpCode::new(0xE5, "SBC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xF5, "SBC", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xED, "SBC", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xFD, "SBC", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(0xF9, "SBC", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(0xE1, "SBC", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xF1, "SBC", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(0x40, "RTI", 1, 6, AddressingMode::NoneAddressing),

        OpCode::new(0x90, "BCC", 2, 2/*+1 if branch succeeds, +2 if to a new page*/, AddressingMode::Relative),
        OpCode::new(0xB0, "BCS", 2, 2/*+1 if branch succeeds, +2 if to a new page*/, AddressingMode::Relative),
        OpCode::new(0xF0, "BEQ", 2, 2/*+1 if branch succeeds, +2 if to a new page*/, AddressingMode::Relative),
        OpCode::new(0xD0, "BNE", 2, 2/*+1 if branch succeeds, +2 if to a new page*/, AddressingMode::Relative),
        OpCode::new(0x10, "BPL", 2, 2/*+1 if branch succeeds, +2 if to a new page*/, AddressingMode::Relative),
        OpCode::new(0x30, "BMI", 2, 2/*+1 if branch succeeds, +2 if to a new page*/, AddressingMode::Relative),
        OpCode::new(0x50, "BVC", 2, 2/*+1 if branch succeeds, +2 if to a new page*/, AddressingMode::Relative),
        OpCode::new(0x70, "BVS", 2, 2/*+1 if branch succeeds, +2 if to a new page*/, AddressingMode::Relative),
    ];

    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();

        for cpuop in &*CPU_OPS_CODES {
            map.insert(cpuop.code, cpuop);
        }

        map
    };
}

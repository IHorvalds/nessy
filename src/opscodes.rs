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

        OpCode::new(0x60, "RTS", 1, 6, AddressingMode::NoneAddressing)
    ];

    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();

        for cpuop in &*CPU_OPS_CODES {
            map.insert(cpuop.code, cpuop);
        }

        map
    };
}

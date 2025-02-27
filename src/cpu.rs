use std::collections::HashMap;

use crate::opscodes::{OPCODES_MAP, OpCode};

enum Register {
    A,
    X,
    Y,
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Relative,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

pub struct CPU {
    pub register_a: u8,
    pub status: u8,
    pub program_counter: u16,
    pub register_x: u8,
    pub register_y: u8,
    pub register_stack: u8,
    memory: [u8; 0xFFFF],
}

trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.mem_write(pos, (data & 0xFF) as u8);
        self.mem_write(pos + 1, (data >> 8) as u8);
    }
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            register_stack: 0,
            status: 0,
            program_counter: 0,
            memory: [0u8; 0xFFFF],
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = 0;
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn run(&mut self) {
        let ref opcodes: HashMap<u8, &'static OpCode> = *OPCODES_MAP;

        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = opcodes
                .get(&code)
                .expect(&format!("OpCode {:x} is not known or implemented", code));

            match code {
                // LDA
                0xA9 | 0xA5 | 0xAD | 0xB5 | 0xBD | 0xB9 | 0xA1 | 0xB1 => {
                    self.load_reg(&opcode.addressing_mode, &Register::A)
                }
                // LDX
                0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => {
                    self.load_reg(&opcode.addressing_mode, &Register::X)
                }
                // LDY
                0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => {
                    self.load_reg(&opcode.addressing_mode, &Register::Y)
                }
                // STA
                0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => {
                    self.store_reg(&opcode.addressing_mode, self.register_a)
                }
                // STX
                0x86 | 0x96 | 0x8E => self.store_reg(&opcode.addressing_mode, self.register_x),
                // STY
                0x84 | 0x94 | 0x8C => self.store_reg(&opcode.addressing_mode, self.register_y),
                // TAX
                0xAA => self.transfer_accumulator(&Register::X),
                // TAY
                0xA8 => self.transfer_accumulator(&Register::Y),

                // INX
                0xE8 => self.inx(),
                // INY
                0xC8 => self.iny(),
                // INC
                0xE6 | 0xF6 | 0xEE | 0xFE => self.inc(&opcode.addressing_mode),

                // JMP
                0x4C | 0x6C => self.jmp(&opcode.addressing_mode),
                // RTS
                0x60 => self.rts(),

                // BRK
                0x00 => return,
                _ => todo!(),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.instruction_len - 1) as u16;
            }
        }
    }

    fn load_reg(&mut self, mode: &AddressingMode, reg: &Register) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);

        *(match reg {
            Register::A => &mut self.register_a,
            Register::X => &mut self.register_x,
            Register::Y => &mut self.register_y,
        }) = val;

        self.update_zero_and_negative_flags(val);
    }

    fn store_reg(&mut self, mode: &AddressingMode, reg: u8) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, reg);
    }

    fn transfer_accumulator(&mut self, reg: &Register) {
        let r = match reg {
            Register::X => &mut self.register_x,
            Register::Y => &mut self.register_x,
            _ => panic!("Can only transfers registers X and Y"),
        };
        *r = self.register_a;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }

        if result & 0b1000_000 != 0 {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
        }
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr).wrapping_add(1);
        self.mem_write(addr, val);

        self.update_zero_and_negative_flags(val);
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.program_counter = addr
    }

    fn rts(&mut self) {
        self.program_counter = ...;
        self.program_counter.wrapping_sub(1);
    }

    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }
            AddressingMode::Indirect => {
                let base = self.mem_read_u16(self.program_counter);
                let lo = self.mem_read(base) as u16;
                let hi = self.mem_read(base.wrapping_add(1)) as u16;
                (hi << 8) | lo
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);
                let ptr = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16) as u16;
                let hi = self.mem_read(ptr.wrapping_add(1) as u16) as u16;
                (hi << 8) | lo
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);
                let lo = self.mem_read(base as u16) as u16;
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16) as u16;
                let deref_base = (hi << 8) | lo;
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
            AddressingMode::Relative => {
                let base = self.mem_read(self.program_counter) as i8;
                self.program_counter = self.program_counter.wrapping_add_signed(base.into());
                self.program_counter
            }
            AddressingMode::NoneAddressing => panic!("Mode {:?} is not supported", mode),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 5);
        assert!(cpu.status & 0b0000_0010 == 0);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xa9_lda_negative_flat() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0x00]);
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;
        cpu.load_and_run(vec![0xa9, 0xa, 0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_jmp_indirect() {
        let mut cpu = CPU::new();
        cpu.memory[0x120] = 0xFC;
        cpu.memory[0x121] = 0xBA;
        // jmp ($0120)
        // pc == (0xBAFC + 1)
        cpu.load_and_run(vec![0x6C, 0x20, 0x01]);
        assert_eq!(cpu.program_counter, 0xBAFD)
    }

    #[test]
    fn test_jmp_absolute() {
        let mut cpu = CPU::new();
        // jmp $BAFC
        // pc == (0xBAFC + 1)
        cpu.load_and_run(vec![0x4C, 0xFC, 0xBA]);
        assert_eq!(cpu.program_counter, 0xBAFD)
    }
}

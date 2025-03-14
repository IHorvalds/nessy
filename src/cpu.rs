use crate::opscodes::{OPCODES_MAP, OpCode};
use bitflags::bitflags;
use std::collections::HashMap;

bitflags! {
    pub struct CPUFlags : u8 {
        const CARRY = 0b0000_0001;
        const ZERO = 0b0000_0010;
        const INTRERRUPT_DISABLE =0b0000_0100;
        const DEC_MODE = 0b0000_1000;
        const BRK_CMD = 0b0001_0000;
        // unused 0b0010_0000
        const OVERFLOW = 0b0100_0000;
        const NEGATIVE = 0b1000_0000;
    }
}

#[derive(Debug, PartialEq)]
enum Register {
    A,
    X,
    Y,
    Stack,
}

#[derive(Debug, PartialEq)]
enum Shift {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Accumulator,
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
    pub status: CPUFlags,
    pub program_counter: u16,
    pub register_x: u8,
    pub register_y: u8,
    pub register_stack: u8,
    memory: [u8; 0xFFFF],
}

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos.wrapping_add(1)) as u16;
        (hi << 8) | (lo)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.mem_write(pos, (data & 0xFF) as u8);
        self.mem_write(pos.wrapping_add(1), (data >> 8) as u8);
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
            register_stack: 0xFF,
            status: CPUFlags::from_bits_truncate(0b100100),
            program_counter: 0,
            memory: [0u8; 0xFFFF],
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_stack = 0xFF;
        self.status = CPUFlags::from_bits_truncate(0b100100);
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        let start = 0x0600;
        self.memory[start..(start + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, start as u16);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        let ref opcodes: HashMap<u8, &'static OpCode> = *OPCODES_MAP;

        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = opcodes
                .get(&code)
                .expect(&format!("OpCode {:x} is not known or implemented", code));

            match code {
                // CLV
                0xB8 => self.status.remove(CPUFlags::OVERFLOW),
                // CLI
                0x58 => self.status.remove(CPUFlags::INTRERRUPT_DISABLE),
                // CLC
                0x18 => self.status.remove(CPUFlags::CARRY),
                // CLD
                0xD8 => self.status.remove(CPUFlags::DEC_MODE),
                // SEC
                0x38 => self.status.insert(CPUFlags::CARRY),
                // SED
                0xF8 => self.status.insert(CPUFlags::DEC_MODE),
                // SEI
                0x78 => self.status.insert(CPUFlags::INTRERRUPT_DISABLE),
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
                0xAA => self.transfer_accumulator(&Register::A, &Register::X),
                // TAY
                0xA8 => self.transfer_accumulator(&Register::A, &Register::Y),
                // TSX
                0xBA => self.transfer_accumulator(&Register::Stack, &Register::A),
                // TXA
                0x8A => self.transfer_accumulator(&Register::X, &Register::A),
                // TXS
                0x9A => {
                    // WHY IS THIS SPECIAL???? WHYY????
                    self.register_stack = self.register_x;
                }
                // TYA
                0x98 => self.transfer_accumulator(&Register::Y, &Register::A),

                // INX
                0xE8 => self.inx(),
                // INY
                0xC8 => self.iny(),
                // INC
                0xE6 | 0xF6 | 0xEE | 0xFE => self.inc(&opcode.addressing_mode),

                // DEX
                0xca => self.dex(),
                // DEY
                0x88 => self.dey(),
                // DEC
                0xC6 | 0xD6 | 0xCE | 0xDE => self.dec(&opcode.addressing_mode),

                // JMP
                0x4C | 0x6C => self.jmp(&opcode.addressing_mode),
                // RTS
                0x60 => self.rts(),
                // JSR
                0x20 => self.jsr(),

                // RTI
                0x40 => {
                    self.status = CPUFlags::from_bits_truncate(self.pop_stack());
                    self.status.remove(CPUFlags::BRK_CMD);

                    self.program_counter = self.pop_stack_16();
                }

                // LSR
                0x4A | 0x46 | 0x56 | 0x4E | 0x5E => self.lsr(&opcode.addressing_mode),

                // ASL
                0x0A | 0x06 | 0x16 | 0x0E | 0x1E => self.asl(&opcode.addressing_mode),

                // ROL
                0x2A | 0x26 | 0x36 | 0x2E | 0x3E => {
                    self.rolr(&opcode.addressing_mode, &Shift::Left)
                }
                // ROR
                0x6A | 0x66 | 0x76 | 0x6E | 0x7E => {
                    self.rolr(&opcode.addressing_mode, &Shift::Right)
                }
                // EOR
                0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => {
                    self.eor(&opcode.addressing_mode)
                }
                // ORA
                0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => {
                    self.ora(&opcode.addressing_mode)
                }
                // AND
                0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
                    self.and(&opcode.addressing_mode)
                }

                // PHP
                0x08 => {
                    let mut flags = CPUFlags::from_bits_truncate(self.status.bits());
                    flags.insert(CPUFlags::BRK_CMD);
                    self.push_stack(flags.bits());
                }
                // PHA
                0x48 => self.push_stack(self.register_a),
                // PLP
                0x28 => {
                    self.status = CPUFlags::from_bits_retain(self.pop_stack());
                    self.status.remove(CPUFlags::BRK_CMD);
                }
                // PLA
                0x68 => {
                    self.register_a = self.pop_stack();
                    self.update_zero_and_negative_flags(self.register_a);
                }

                // ADC
                0x65 | 0x69 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => {
                    self.adc(&opcode.addressing_mode);
                }
                // SBC
                0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => {
                    self.sbc(&opcode.addressing_mode)
                }

                // CMP
                0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => {
                    self.cmp(&opcode.addressing_mode, &Register::A)
                }
                // CPX
                0xE0 | 0xE4 | 0xEC => self.cmp(&opcode.addressing_mode, &Register::X),
                // CPY
                0xC0 | 0xC4 | 0xCC => self.cmp(&opcode.addressing_mode, &Register::Y),

                // BCC
                0x90 => self.branch(!self.status.contains(CPUFlags::CARRY)),
                // BCS
                0xB0 => self.branch(self.status.contains(CPUFlags::CARRY)),
                // BEQ
                0xF0 => self.branch(self.status.contains(CPUFlags::ZERO)),
                // BNE
                0xD0 => self.branch(!self.status.contains(CPUFlags::ZERO)),
                // BPL
                0x10 => self.branch(!self.status.contains(CPUFlags::NEGATIVE)),
                // BMI
                0x30 => self.branch(self.status.contains(CPUFlags::NEGATIVE)),
                // BVC
                0x50 => self.branch(!self.status.contains(CPUFlags::OVERFLOW)),
                // BVS
                0x70 => self.branch(self.status.contains(CPUFlags::OVERFLOW)),

                // BIT
                0x24 | 0x2c => self.bit(&opcode.addressing_mode),

                // NOP (0xea==234)
                0xEA => {}
                // BRK
                0x00 => return,
                _ => todo!("Unknown thing"),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.instruction_len - 1) as u16;
            }

            callback(self);
        }
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    fn load_reg(&mut self, mode: &AddressingMode, reg: &Register) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);

        *(match reg {
            Register::A => &mut self.register_a,
            Register::X => &mut self.register_x,
            Register::Y => &mut self.register_y,
            _ => panic!("Register {:?} cannot be loaded with LD*", reg),
        }) = val;

        self.update_zero_and_negative_flags(val);
    }

    fn store_reg(&mut self, mode: &AddressingMode, reg: u8) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, reg);
    }

    fn transfer_accumulator(&mut self, src: &Register, dest: &Register) {
        let val = match src {
            Register::A => self.register_a,
            Register::X => self.register_x,
            Register::Y => self.register_y,
            Register::Stack => self.register_stack,
        };

        let r = match dest {
            Register::A => &mut self.register_a,
            Register::X => &mut self.register_x,
            Register::Y => &mut self.register_y,
            Register::Stack => &mut self.register_stack,
        };

        *r = val;
        self.update_zero_and_negative_flags(val);
    }

    fn update_carry_flag(&mut self, val: u8) {
        match val & 0x1 {
            1 => self.status.insert(CPUFlags::CARRY),
            0 => self.status.remove(CPUFlags::CARRY),
            _ => panic!("?????????HOW?????????????"),
        };
    }

    fn set_register_a(&mut self, val: u8) {
        self.register_a = val;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status.insert(CPUFlags::ZERO);
        } else {
            self.status.remove(CPUFlags::ZERO);
        }

        if result & 0b1000_000 != 0 {
            self.status.insert(CPUFlags::NEGATIVE);
        } else {
            self.status.remove(CPUFlags::NEGATIVE);
        }
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.mem_read_u16(self.program_counter);

        // Known processor bug.
        // In indirect mode on a page boundary, reads the low half of the address from addr
        // and the hi half from (addr & 0xFF00)
        self.program_counter = if *mode == AddressingMode::Indirect && (addr & 0xFF) == 0xFF {
            let lo = self.mem_read(addr) as u16;
            let hi = self.mem_read(addr & 0xFF00) as u16;
            (hi << 8) | lo
        } else {
            self.get_operand_address(mode)
        }
    }

    fn branch(&mut self, cond: bool) {
        if cond {
            let addr = self.get_operand_address(&AddressingMode::Relative);
            self.program_counter = addr;
        }
    }

    fn rts(&mut self) {
        self.program_counter = self.pop_stack_16() + 1;
    }

    fn jsr(&mut self) {
        self.push_stack_16(self.program_counter.wrapping_add(1));
        self.program_counter = self.get_operand_address(&AddressingMode::Absolute);
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        let (val, result) = match mode {
            AddressingMode::Accumulator => {
                let current = self.register_a;
                self.register_a >>= 1;
                (current, self.register_a)
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let current = self.mem_read(addr);
                let res = current >> 1;
                self.mem_write(addr, res);
                (current, res)
            }
        };

        self.update_carry_flag(val);
        self.update_zero_and_negative_flags(result);
    }

    fn rotate(&mut self, val: u8, direction: &Shift) -> u8 {
        let c_val = match self.status.contains(CPUFlags::CARRY) {
            true => 1,
            false => 0,
        };
        match direction {
            Shift::Left => {
                self.update_carry_flag(val >> 7);
                val << 1 | c_val
            }
            Shift::Right => {
                self.update_carry_flag(val);
                (c_val << 7) | val >> 1
            }
        }
    }

    fn rolr(&mut self, mode: &AddressingMode, direction: &Shift) {
        let result = match mode {
            AddressingMode::Accumulator => {
                self.register_a = self.rotate(self.register_a, direction);
                self.register_a
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let res = self.rotate(self.mem_read(addr), direction);
                self.mem_write(addr, res);
                res
            }
        };

        self.update_zero_and_negative_flags(result);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        let (val, result) = match mode {
            AddressingMode::Accumulator => {
                let val = self.register_a;
                self.register_a = val << 1;
                (val, self.register_a)
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let val = self.mem_read(addr);
                let res = val << 1;
                self.mem_write(addr, res);
                (val, res)
            }
        };

        self.update_carry_flag(val >> 7);
        self.update_zero_and_negative_flags(result);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr).wrapping_add(1);
        self.mem_write(addr, val);

        self.update_zero_and_negative_flags(val);
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr).wrapping_sub(1);
        self.mem_write(addr, val);

        self.update_zero_and_negative_flags(val);
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);

        self.carry_add_to_reg_a(val);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);

        if self.register_a & val == 0 {
            self.status.insert(CPUFlags::ZERO);
        } else {
            self.status.remove(CPUFlags::ZERO);
        }

        if val >> 7 == 1 {
            self.status.insert(CPUFlags::NEGATIVE);
        } else {
            self.status.remove(CPUFlags::NEGATIVE);
        }

        if (val >> 6) & 0b1 == 1 {
            self.status.insert(CPUFlags::OVERFLOW);
        } else {
            self.status.remove(CPUFlags::OVERFLOW);
        }
    }

    fn carry_add_to_reg_a(&mut self, val: u8) {
        let sum = self.register_a as u16
            + val as u16
            + (if self.status.contains(CPUFlags::CARRY) {
                1
            } else {
                0
            }) as u16;

        if sum > 0xff {
            self.status.insert(CPUFlags::CARRY);
        } else {
            self.status.remove(CPUFlags::CARRY);
        }

        let result = sum as u8;
        if (val ^ result) & (result ^ self.register_a) & 0x80 != 0 {
            self.status.insert(CPUFlags::OVERFLOW);
        } else {
            self.status.remove(CPUFlags::OVERFLOW);
        }

        self.set_register_a(result);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);
        self.carry_add_to_reg_a((val as i8).wrapping_neg().wrapping_sub(1) as u8);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let result = self.register_a ^ self.mem_read(addr);
        self.set_register_a(result);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let result = self.register_a | self.mem_read(addr);
        self.set_register_a(result);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let result = self.register_a & self.mem_read(addr);
        self.set_register_a(result);
    }

    fn cmp(&mut self, mode: &AddressingMode, reg: &Register) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);

        let comparison_base = match reg {
            Register::A => self.register_a,
            Register::X => self.register_x,
            Register::Y => self.register_y,
            Register::Stack => panic!("No instructions should compare the stack pointer value."),
        };

        if comparison_base >= val {
            self.status.insert(CPUFlags::CARRY);
        } else {
            self.status.remove(CPUFlags::CARRY);
        }

        self.update_zero_and_negative_flags(comparison_base.wrapping_sub(val));
    }

    fn push_stack(&mut self, val: u8) {
        self.register_stack = self.register_stack.wrapping_sub(1);
        let sp = 0x100 | self.register_stack as u16;
        self.mem_write(sp, val);
    }

    fn pop_stack(&mut self) -> u8 {
        let sp = 0x100 | self.register_stack as u16;
        let val = self.mem_read(sp);

        self.register_stack = self.register_stack.wrapping_add(1);
        val
    }

    fn push_stack_16(&mut self, val: u16) {
        self.register_stack = self.register_stack.wrapping_sub(2);
        let sp = 0x100 | self.register_stack as u16;
        self.mem_write_u16(sp, val);
    }

    fn pop_stack_16(&mut self) -> u16 {
        let sp = 0x100 | self.register_stack as u16;
        let val = self.mem_read_u16(sp);

        self.register_stack = self.register_stack.wrapping_add(2);
        val
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
                self.mem_read_u16(base)
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);
                let ptr = base.wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16) as u16;
                let hi = self.mem_read(ptr.wrapping_add(1) as u16) as u16;
                (hi << 8) | lo
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);
                let lo = self.mem_read(base as u16) as u16;
                let hi = self.mem_read(base.wrapping_add(1) as u16) as u16;
                let deref_base = (hi << 8) | lo;
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
            AddressingMode::Relative => {
                let base = self.mem_read(self.program_counter) as i8;
                let jump = self
                    .program_counter
                    .wrapping_add(1)
                    .wrapping_add_signed(base.into());
                jump
            }
            AddressingMode::Accumulator => panic!(
                "Mode {:?} does not read an operand from memory, therefore no address can be specified",
                mode
            ),
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
        assert!(!cpu.status.contains(CPUFlags::ZERO));
        assert!(!cpu.status.contains(CPUFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status.contains(CPUFlags::ZERO));
    }

    #[test]
    fn test_0xa9_lda_negative_flat() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0x00]);
        assert!(cpu.status.contains(CPUFlags::NEGATIVE));
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
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
    fn test_jmp_indirect_bug() {
        let mut cpu = CPU::new();
        cpu.memory[0x200] = 0xDE;
        cpu.memory[0x2ff] = 0xFC;
        cpu.memory[0x300] = 0xBA;
        // jmp ($2ff)
        // pc == (0xDEFC + 1)
        cpu.load_and_run(vec![0x6C, 0xff, 0x02]);
        assert_eq!(cpu.program_counter, 0xDEFD)
    }

    #[test]
    fn test_jmp_absolute() {
        let mut cpu = CPU::new();
        // jmp $BAFC
        // pc == (0xBAFC + 1)
        cpu.load_and_run(vec![0x4C, 0xFC, 0xBA]);
        assert_eq!(cpu.program_counter, 0xBAFD)
    }

    #[test]
    fn test_jsr_rts() {
        let mut cpu = CPU::new();
        // lda #$12
        // jsr subr
        // lda $abcd ; sp=$ff, pc=$8005 (pre) pc=$8008 (post), A=$34
        // brk
        //
        // subr:
        // ldx #$34 ;sp=$fd
        // stx $abcd
        // rts
        cpu.load_and_run(vec![
            0xa9, 0x12, 0x20, 0x09, 0x06, 0xad, 0xcd, 0xab, 0x00, 0xa2, 0x34, 0x8e, 0xcd, 0xab,
            0x60,
        ]);

        assert_eq!(cpu.program_counter, 0x0609);
        assert_eq!(cpu.register_stack, 0xff);
        assert_eq!(cpu.register_a, 0x34);
        assert_eq!(cpu.register_x, 0x34);
    }
}

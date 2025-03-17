mod bus;
mod cartridge;
mod cpu;
mod opscodes;
mod snake;

use crate::cpu::{CPU, Mem};
use cartridge::Rom;
use rand::Rng;
use sdl2::EventPump;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;

fn handle_user_input(cpu: &mut CPU, evt_pump: &mut EventPump) {
    for event in evt_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => std::process::exit(0),
            Event::KeyDown {
                keycode: Some(Keycode::W),
                ..
            } => cpu.mem_write(0xff, 0x77),
            Event::KeyDown {
                keycode: Some(Keycode::S),
                ..
            } => cpu.mem_write(0xff, 0x73),
            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => cpu.mem_write(0xff, 0x61),
            Event::KeyDown {
                keycode: Some(Keycode::D),
                ..
            } => cpu.mem_write(0xff, 0x64),
            _ => { /* do nothing */ }
        }
    }
}

fn color(byte: u8) -> Color {
    match byte {
        0 => Color::BLACK,
        1 => Color::WHITE,
        2 | 9 => Color::GREY,
        3 | 10 => Color::RED,
        4 | 11 => Color::GREEN,
        5 | 12 => Color::BLUE,
        6 | 13 => Color::MAGENTA,
        7 | 14 => Color::YELLOW,
        _ => Color::CYAN,
    }
}

fn should_update_screen(cpu: &CPU, frame: &mut [u8; 32 * 32 * 3]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;
    // screen mapped bytes in NES memory
    for i in 0x200..0x600 {
        let color_idx = cpu.mem_read(i as u16);
        let (r, g, b) = color(color_idx).rgb();
        if (frame[frame_idx], frame[frame_idx + 1], frame[frame_idx + 2]) != (r, g, b) {
            (frame[frame_idx], frame[frame_idx + 1], frame[frame_idx + 2]) = (r, g, b);
            update = true;
        }
        frame_idx += 3;
    }
    update
}

fn main() {
    // initialize SDl2
    let sdl_ctx = sdl2::init().unwrap();
    let video_subsystem = sdl_ctx.video().unwrap();
    let window = video_subsystem
        .window("Snake lol", (32 * 10) as u32, (32 * 10) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut evt_pump = sdl_ctx.event_pump().unwrap();
    canvas.set_scale(10.0, 10.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32)
        .unwrap();

    let mut screen_state = [0u8; 32 * 32 * 3];
    let mut rng = rand::thread_rng();

    // load the game
    let bytes: Vec<u8> = std::fs::read("snake.nes").unwrap();
    let rom = Rom::new(&bytes).unwrap();

    // Create the CPU, load the game, run(update the screen).
    let mut cpu = CPU::new_with_rom(rom);
    cpu.reset();
    cpu.run_with_callback(move |cpu| {
        handle_user_input(cpu, &mut evt_pump);
        cpu.mem_write(0xfe, rng.gen_range(1, 16));

        if should_update_screen(cpu, &mut screen_state) {
            texture.update(None, &screen_state, 32 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        ::std::thread::sleep(std::time::Duration::new(0, 70_000));
    });
}

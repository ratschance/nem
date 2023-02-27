#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;

mod cartridge;
mod controller;
mod cpu;
mod mapper;
mod memory;
mod nes;

use sdl2::pixels::PixelFormatEnum;

use crate::cpu::c6502::State;
use crate::nes::Nes;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <rom>", &args[0]);
        std::process::exit(1);
    }

    tracing_subscriber::fmt::init();

    // Init video system. This introduces some annoying lifetime dependencies so I can't really organize this how I would like
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("NEM", (32.0 * 10.0) as u32, (32.0 * 10.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    canvas.set_scale(10.0, 10.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32)
        .unwrap();

    // texture.update(None, &[0; 32], 0).unwrap();

    match Nes::new(&args[1], sdl_context) {
        Ok(mut nes) => {
            nes.reset();
            loop {
                if nes.c6502.get_execution_state() == &State::InfiniteLoop {
                    panic!("CPU stuck in infinite loop");
                }
                nes.tick();
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            eprintln!("Usage: {} <rom>", &args[0]);
            std::process::exit(1)
        }
    }
}

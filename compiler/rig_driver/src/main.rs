#![feature(panic_info_message)]
#![feature(panic_backtrace_config)]

use clap::Parser as ClapParser;
use parking_lot::lock_api::RwLock;
use rig_errors::display_compiler_error;
use rig_intern::{intern, Interner, INTERNER};

use rig_parser::{parse_module};

use rig_session::Session;
use std::backtrace::Backtrace;

use std::panic::{set_backtrace_style, set_hook, BacktraceStyle};
use std::process::ExitCode;

#[derive(ClapParser)]
struct Args {
    /// Path to source file
    source: String,

    #[arg(long)]
    /// Enables debug outputs
    debug: bool,

    /// Prettify debug outputs
    #[arg(long)]
    debug_pretty: bool,
}

fn main() -> ExitCode {
    INTERNER.init_once(|| RwLock::new(Interner::new()));

    set_backtrace_style(BacktraceStyle::Full);
    set_hook(Box::new(|pi| {
        display_compiler_error("Fatal Internal compiler error!");

        if let Some(msg) = pi.message() {
            display_compiler_error(format!("Message: {msg}"));
        }

        if let Some(location) = pi.location() {
            display_compiler_error(format!("Location: {location}"));
        }

        if let Some(payload) = pi.payload().downcast_ref::<&str>() {
            display_compiler_error(format!("Payload: {payload}"));
        }

        display_compiler_error(format!("Backtrace:\n{}", Backtrace::force_capture()));
        display_compiler_error("Please submit a bug report at: https://github.com/mdgaziur/riglang")
    }));

    let args = Args::parse();

    let mut session = Session::new(args.debug, args.debug_pretty);
    if let Err(e) = session.create_module(&args.source) {
        display_compiler_error(e);
        ExitCode::FAILURE
    } else {
        if parse_module(&mut session, intern!(args.source)) {
            ExitCode::FAILURE
        } else {
            ExitCode::SUCCESS
        }
    }
}

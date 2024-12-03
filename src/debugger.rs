use crate::instruction::Instruction;
use crate::machine::{Frame as MachineFrame, Machine};
use anyhow::{anyhow, Ok, Result};
use crossterm::event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent};
use crossterm::style::Stylize;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use crossterm::ExecutableCommand;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span, Text};

use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::widgets::{Block, Borders, Paragraph};
use ratatui::{Frame, Terminal};
use std::collections::HashSet;
use std::io::{self, stdout};
use std::time::{Duration, Instant};

const HELP: &str = r#"
commands <required> [optional]
q|quit              - quit
h|help              - show this help message
s|step              - step
c|continue          - continue
b|breakpoint <addr> - add breakpoint
r|run [file]        - run
 |reset             - reset stacks and set ip to 0
j|jump <addr>       - jump to address
"#;

#[derive(Debug)]
pub enum Command {
    Help,
    Quit,
    Step,
    Continue,
    AddBreakPoint(usize),
    Run(Option<String>),
    Reset,
    Jump(usize),
}

#[derive(Debug, Default)]
enum State {
    #[default]
    Running,
    Paused,
}

#[derive(Debug)]
pub struct Debugger<'a> {
    input_buffer: String,
    last_instruction_pointer: Option<usize>,
    output: String,
    machine: &'a mut Machine,
    state: State,
    instructions: Vec<Instruction>,
    breakpoints: HashSet<usize>,
    is_running: bool,
}

impl<'a> Debugger<'a> {
    pub fn new(machine: &'a mut Machine) -> Result<Self> {
        let instructions = machine.decompile()?;
        Ok(Self {
            input_buffer: String::new(),
            last_instruction_pointer: None,
            output: String::new(),
            machine,
            state: State::default(),
            instructions,
            breakpoints: HashSet::new(),
            is_running: true,
        })
    }

    pub fn with_breakpoints(mut self, breakpoints: Vec<usize>) -> Self {
        self.breakpoints = HashSet::from_iter(breakpoints);
        self
    }

    pub fn run(&mut self) -> Result<()> {
        let mut terminal = Terminal::new(CrosstermBackend::new(io::stdout()))?;
        enable_raw_mode()?;
        stdout().execute(EnterAlternateScreen)?;
        stdout()
            .execute(EnableMouseCapture)
            .expect("Could not enable mouse capture");

        let tick_rate = Duration::from_millis(250);
        let mut last_tick = Instant::now();

        while self.is_running {
            match self.state {
                State::Running if self.breakpoints.contains(&self.machine.ip) => {
                    self.state = State::Paused;
                }
                State::Running => {
                    self.machine.run_once()?;
                }
                State::Paused => {}
            }

            terminal.draw(|f| self.draw(f))?;
            if event::poll(tick_rate - last_tick.elapsed())? {
                let event = event::read()?;
                self.event_handler(event)?;
            }

            last_tick = Instant::now();
        }
        Ok(())
    }

    fn event_handler(&mut self, event: Event) -> Result<()> {
        match &event {
            Event::FocusGained => eprintln!("focus gained"),
            Event::FocusLost => eprintln!("focus lost"),
            Event::Key(key_event) => self.key_handler(key_event)?,
            Event::Mouse(mouse_event) => self.mouse_handler(mouse_event),
            Event::Paste(string) => self.paste_handler(string),
            Event::Resize(w, h) => self.resize_handler(*w, *h),
        }
        Ok(())
    }

    fn draw(&mut self, f: &mut Frame) {
        let size = f.area();

        // Layout with two sections: input and VM state
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(0), Constraint::Length(3)].as_ref())
            .split(size);

        let data = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(75), Constraint::Percentage(25)].as_ref())
            .split(chunks[0]);

        let frame_data_block = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Percentage(50),
                    Constraint::Percentage(25),
                    Constraint::Percentage(25),
                ]
                .as_ref(),
            )
            .split(data[1]);

        // Render user input box
        let input_box = Paragraph::new(self.input_buffer.clone())
            .block(Block::default().borders(Borders::ALL).title("Input"));

        let x = chunks[1].x + 1 + self.input_buffer.len() as u16;
        let y = chunks[1].y + 1;
        f.set_cursor_position((x, y));

        // Render vm frame data
        let vm_frame_data = 'block: {
            let ip = self.machine.ip;
            let Result::Ok(frame) = self.machine.get_current_frame() else {
                break 'block "Empty VM frame".to_string();
            };
            let return_address = if let Some(address) = frame.return_address {
                format!("return address: {}", address)
            } else {
                String::from("return address: None")
            };

            let mut free = String::from("Free:\n");
            for (i, value) in self.machine.free.iter().enumerate() {
                free += &format!("[{}]: {}\n", i, value);
            }
            let mut vm_frame_data = String::from("STACK:\n");
            for (i, value) in frame.stack.iter().enumerate() {
                vm_frame_data += &format!("[{}]: {}\n", i, value);
            }
            let mut locals = String::from("LOCALS:\n");
            for (i, value) in frame.args.iter().enumerate() {
                locals += &format!("[{}]: {}\n", i, value);
            }
            format!("{return_address}\nip: {ip}\n{free}\n{locals}\n{vm_frame_data}")
        };

        let frame_data = Paragraph::new(vm_frame_data)
            .block(Block::default().borders(Borders::ALL).title("VM Frame"));

        let globals = self
            .machine
            .global
            .iter()
            .enumerate()
            .map(|(i, v)| format!("global[{}]: {}", i, v))
            .collect::<Vec<_>>()
            .join("\n");
        let global_data =
            Paragraph::new(globals).block(Block::default().borders(Borders::ALL).title("Globals"));

        // Render dummy VM state
        // 🤮
        if let Some(last_instruction_pointer) = self.last_instruction_pointer {
            if last_instruction_pointer != self.machine.ip {
                self.last_instruction_pointer = Some(self.machine.ip);
            }
        } else {
            self.last_instruction_pointer = Some(self.machine.ip);
        }
        let text = self.debug();

        let scroll_index = self.get_current_instruction_index().saturating_sub(20);
        let vm_box = Paragraph::new(text)
            .block(Block::default().borders(Borders::ALL).title("VM State"))
            .scroll((scroll_index as u16, 0));

        // TEMP
        let mut temp_output = String::new();
        for breakpoint in &self.breakpoints {
            temp_output += breakpoint.to_string().as_str();
            temp_output += "\n";
        }
        temp_output.push_str(&format!("{:?}\n{}", self.state, self.output));

        let temp = Paragraph::new(temp_output)
            .block(Block::default().borders(Borders::ALL).title("Breakpoints"));

        // Render both input and VM state
        f.render_widget(vm_box, data[0]);
        f.render_widget(frame_data, frame_data_block[0]);
        f.render_widget(global_data, frame_data_block[1]);
        f.render_widget(temp, frame_data_block[2]);
        f.render_widget(input_box, chunks[1]);
    }

    fn toggle_breakpoint(&mut self, breakpoint: usize) {
        if self.breakpoints.contains(&breakpoint) {
            self.breakpoints.remove(&breakpoint);
        } else {
            self.breakpoints.insert(breakpoint);
        }
    }

    fn key_handler(&mut self, key_event: &KeyEvent) -> Result<()> {
        let KeyEvent { code, .. } = key_event;
        self.key_code_handler(code)?;
        Ok(())
    }

    fn key_code_handler(&mut self, code: &KeyCode) -> Result<()> {
        match code {
            KeyCode::Char(c) => {
                self.input_buffer.push(*c);
            }
            KeyCode::Backspace => {
                self.input_buffer.pop();
            }
            KeyCode::Enter => {
                match parse_input(&self.input_buffer) {
                    Result::Ok(command) => match command {
                        Command::Help => self.output = HELP.to_string(),
                        Command::Quit => self.is_running = false,
                        Command::Step => match self.machine.run_once() {
                            Result::Ok(_) => {}
                            Result::Err(err) => self.output.push_str(&format!("error: {}\n", err)),
                        },
                        Command::Continue => self.state = State::Running,
                        Command::AddBreakPoint(address) => {
                            self.toggle_breakpoint(address);
                        }
                        Command::Run(Some(file)) => match std::fs::read_to_string(&file) {
                            Result::Ok(src) => {
                                match self.machine.load_from_string(&src) {
                                    Result::Ok(_) => {}
                                    Result::Err(errors) => {
                                        for error in errors {
                                            error.report(&file, &src)?;
                                        }
                                    }
                                };
                                let instructions = self.machine.decompile()?;
                                self.instructions = instructions;
                            }
                            Result::Err(err) => {
                                self.output.push_str(&format!("error: {}\n", err));
                            }
                        },
                        Command::Run(None) => {
                            self.machine.ip = 0x0000;
                        }
                        Command::Reset => {
                            self.machine.global.clear();
                            self.machine.free.clear();
                            self.machine.stack.clear();
                            self.machine.ip = 0x0000;
                            self.machine.stack.push(MachineFrame {
                                return_address: None,
                                args: Vec::with_capacity(256),
                                stack: Vec::with_capacity(1024),
                            });
                        }
                        Command::Jump(address) => {
                            self.machine.ip = address;
                        }
                    },
                    Result::Err(err) => self.output.push_str(&format!("error: {}\n", err)),
                }

                self.input_buffer.clear();
            }
            _ => self.output.push_str(&format!("{:?}\n", code)),
        }
        Ok(())
    }

    fn mouse_handler(&mut self, _: &event::MouseEvent) {

        // let event::MouseEvent {
        //     kind,
        //     ..
        //     // column,
        //     // row,
        //     // modifiers,
        // } = mouse_event;
        // let k = match kind {
        //     event::MouseEventKind::Down(mouse_button) => format!("{mouse_button:?}\n"),
        //     event::MouseEventKind::Up(mouse_button) => format!("{mouse_button:?}\n"),
        //     event::MouseEventKind::Drag(mouse_button) => format!("{mouse_button:?}\n"),
        //     event::MouseEventKind::Moved => String::from("Moved\n"),
        //     event::MouseEventKind::ScrollDown => String::from("Scroll Down\n"),
        //     event::MouseEventKind::ScrollUp => String::from("Scroll Up\n"),
        //     event::MouseEventKind::ScrollLeft => String::from("Scroll Left\n"),
        //     event::MouseEventKind::ScrollRight => String::from("Scroll Right\n"),
        // };
        // self.output.push_str(&k)
    }

    fn paste_handler(&mut self, string: &str) {
        self.input_buffer.push_str(string);
    }

    fn resize_handler(&self, w: u16, h: u16) {
        eprintln!("resizing to {w}x{h}");
    }

    fn get_current_instruction_index(&self) -> usize {
        let current_ip = self.machine.ip;
        let mut ip = 0;
        for (i, instruction) in self.instructions.iter().enumerate() {
            if ip == current_ip {
                return i;
            }
            ip += instruction.size();
        }
        0
    }

    fn debug(&self) -> Text {
        let mut offset = 0;

        let mut result = Vec::new();

        for int in self.instructions.iter() {
            let selected = if self.machine.ip == offset {
                Span::styled(
                    format!("0x{offset:02X} {offset:>3} "),
                    Style::default()
                        .fg(Color::Green)
                        .add_modifier(Modifier::UNDERLINED),
                )
            } else {
                Span::raw(format!("0x{offset:02X} {offset:>3} "))
            };

            let breakpoint = if self.breakpoints.contains(&offset) {
                Span::raw("🔴".to_string())
            } else {
                Span::raw("  ".to_string())
            };

            let bytecode = int
                .to_bytecode()
                .into_iter()
                .fold(String::new(), |acc, i| format!("{acc}{i:02X} "));

            let debug_int = format!("{int:?}");

            let line = Line::from(vec![
                breakpoint,
                selected,
                Span::raw(format!("{debug_int:<20} ")),
                Span::raw(bytecode.trim().to_string()),
            ]);

            result.push(line);
            offset += int.size();
        }

        Text::from(result)
    }
}

impl Drop for Debugger<'_> {
    fn drop(&mut self) {
        disable_raw_mode().expect("Could not disable raw mode");
        stdout()
            .execute(LeaveAlternateScreen)
            .expect("Could not leave alternate screen");
        stdout()
            .execute(DisableMouseCapture)
            .expect("Could not disable mouse capture");
    }
}

// TODO: move to FromString
fn parse_input(input: &str) -> Result<Command> {
    if input.is_empty() {
        return Err(anyhow!("Empty input"));
    }
    let command = input.split(' ').collect::<Vec<_>>();
    match command[0] {
        "help" | "h" => Ok(Command::Help),
        "quit" | "q" => Ok(Command::Quit),
        "step" | "s" => Ok(Command::Step),
        "continue" | "c" => Ok(Command::Continue),
        "breakpoint" | "b" if command.len() == 2 => Ok(Command::AddBreakPoint(command[1].parse()?)),
        "run" | "r" if command.len() == 2 => Ok(Command::Run(Some(command[1].to_string()))),
        "run" | "r" => Ok(Command::Run(None)),
        "reset" => Ok(Command::Reset),
        "jump" | "j" if command.len() == 2 => Ok(Command::Jump(command[1].parse()?)),
        _ => Err(anyhow!(format!("Unknown command `{}`", input.red()))),
    }
}

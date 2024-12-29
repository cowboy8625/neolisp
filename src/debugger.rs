use crate::instruction::Instruction;
use crate::machine::{Frame as MachineFrame, Machine};
use anyhow::{anyhow, Ok, Result};
use crossterm::event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent};
use crossterm::style::Stylize;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use crossterm::ExecutableCommand;
use ratatui::widgets::{Clear, Widget};

use crate::widgets::{
    BreakpointsWidget, FrameWidget, GlobalsWidget, InstructionsWidget, PopupWidget,
};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::{Frame, Terminal};
use std::collections::HashSet;
use std::io::{self, stdout};
use std::time::{Duration, Instant};

const HELP: &str = r#"If no command is given, the last command will be repeated.
Keys:
<- arrow -> to change display data on right side
Mode Keys Normal:
?  -  to show this message
n  -  to toggle highlighting scopes
:  -  to enter command mode

Default command is step.
<commands> [optional]
q|quit              - quit
h|help              - show this help message
s|step              - step
c|continue          - continue
b|breakpoint <addr> - add breakpoint
r|run [file]        - run
 |reset             - reset stacks and set ip to 0
j|jump <addr>       - jump to address
 |highlight_scope   - toggle highlighting scopes
"#;

#[derive(Default, Debug, Clone)]
pub enum Command {
    Help,
    Quit,
    #[default]
    Step,
    Continue,
    AddBreakPoint(usize),
    Run(Option<String>),
    Reset,
    Jump(usize),
    HighlightScope,
}

#[derive(Debug, Default, PartialEq, Eq)]
enum State {
    #[default]
    Running,
    Paused,
}

#[derive(Debug, Clone, Copy, Default)]
enum DisplayDataState {
    #[default]
    Frame,
    Global,
    Breakpoints,
}

impl DisplayDataState {
    fn next(&mut self) {
        *self = match self {
            Self::Frame => Self::Global,
            Self::Global => Self::Breakpoints,
            Self::Breakpoints => Self::Frame,
        };
    }

    fn previous(&mut self) {
        *self = match self {
            Self::Frame => Self::Breakpoints,
            Self::Global => Self::Frame,
            Self::Breakpoints => Self::Global,
        };
    }
}

#[derive(Debug, Default)]
enum Mode {
    #[default]
    Normal,
    Input,
}

#[derive(Debug)]
pub struct Debugger<'a> {
    input_buffer: String,
    last_command: Command,
    last_instruction_pointer: Option<usize>,
    next_instruction_pointer: usize,
    output: String,
    machine: &'a mut Machine,
    state: State,
    stack_data_state: DisplayDataState,
    error_message: Option<String>,
    instructions: Vec<Instruction>,
    breakpoints: HashSet<usize>,
    is_running: bool,
    show_help: bool,
    highlight_scope: bool,
    mode: Mode,
    scroll_offset: u16,
}

impl<'a> Debugger<'a> {
    pub fn new(machine: &'a mut Machine) -> Result<Self> {
        let instructions = machine.decompile()?;
        let mut this = Self {
            machine,
            instructions,
            is_running: true,
            // Defaults
            input_buffer: String::default(),
            last_command: Command::default(),
            last_instruction_pointer: Option::default(),
            next_instruction_pointer: usize::default(),
            output: String::default(),
            state: State::default(),
            stack_data_state: DisplayDataState::default(),
            error_message: Option::default(),
            breakpoints: HashSet::default(),
            show_help: bool::default(),
            highlight_scope: bool::default(),
            mode: Mode::default(),
            scroll_offset: u16::default(),
        };
        this.set_next_instruction_pointer();
        Ok(this)
    }

    pub fn with_breakpoints(mut self, breakpoints: Vec<usize>) -> Self {
        self.breakpoints = HashSet::from_iter(breakpoints);
        self
    }

    pub fn run(&mut self) -> Result<()> {
        let mut terminal = Terminal::new(CrosstermBackend::new(io::stdout()))?;
        enable_raw_mode()?;
        stdout().execute(EnterAlternateScreen)?;
        stdout().execute(EnableMouseCapture)?;

        let mut last_tick = Instant::now();

        while self.is_running {
            if self.error_message.is_some() {
                self.state = State::Paused;
            }
            if self.state == State::Running {
                for _ in 0..100 {
                    if self.breakpoints.contains(&self.machine.ip) {
                        terminal.clear()?;
                        self.state = State::Paused;
                        break;
                    }
                    if self.run_machine_once() {
                        terminal.clear()?;
                        self.state = State::Paused;
                        break;
                    }
                }
            }

            if self.state == State::Paused || last_tick.elapsed() >= Duration::from_millis(250) {
                terminal.draw(|f| self.draw(f))?;
                last_tick = Instant::now();
            }

            if event::poll(Duration::from_millis(1))? {
                let event = event::read()?;
                self.event_handler(event)?;
            }
        }

        Ok(())
    }

    fn event_handler(&mut self, event: Event) -> Result<()> {
        match &event {
            Event::FocusGained => {}
            Event::FocusLost => {}
            Event::Key(key_event) => self.key_handler(key_event)?,
            Event::Mouse(mouse_event) => self.mouse_handler(mouse_event),
            Event::Paste(string) => self.paste_handler(string),
            Event::Resize(w, h) => self.resize_handler(*w, *h),
        }
        Ok(())
    }

    fn draw_frame_widget(&self, f: &mut Frame, area: Rect) {
        FrameWidget::new(self.machine.ip, &self.machine.free, self.machine)
            .render(area, f.buffer_mut());
    }

    fn draw_global_widget(&self, f: &mut Frame, area: Rect) {
        GlobalsWidget::new(self.machine.ip, &self.machine.global).render(area, f.buffer_mut())
    }

    fn draw_breakpoints_widget(&self, f: &mut Frame, area: Rect) {
        BreakpointsWidget::new(self.machine.ip, &self.breakpoints).render(area, f.buffer_mut())
    }

    fn draw_instructions_widget(&self, f: &mut Frame, area: Rect) {
        let current_function_name = self
            .machine
            .get_current_function_name()
            .unwrap_or("main".to_string());
        let location = self
            .machine
            .symbol_table
            .get(&current_function_name)
            .and_then(|symbol| symbol.get_location())
            .unwrap_or(0..1);
        InstructionsWidget::new(
            self.machine.ip,
            self.next_instruction_pointer,
            location,
            &self.instructions,
            &self.breakpoints,
            self.highlight_scope,
        )
        .with_scroll_offset(self.scroll_offset)
        .render(area, f.buffer_mut());
    }

    fn draw_input_widget(&self, f: &mut Frame, area: Rect) {
        let popup = PopupWidget::new(&self.input_buffer)
            .width_closing_message(false)
            .with_title("Input")
            .with_percent_width(50)
            .with_height(3);
        let clear_area = popup.get_area(area);

        let x = clear_area.x + 1 + self.input_buffer.len() as u16;
        let y = clear_area.y + 1;

        f.set_cursor_position((x, y));
        f.render_widget(Clear, clear_area);
        popup.render(area, f.buffer_mut());
    }

    fn draw(&mut self, f: &mut Frame) {
        let size = f.area();

        let (instruction_area, data_area) = get_instruction_and_data_area(size);

        if self.last_instruction_pointer != Some(self.machine.ip) {
            self.last_instruction_pointer = Some(self.machine.ip);
        }

        self.draw_instructions_widget(f, instruction_area);

        match self.stack_data_state {
            DisplayDataState::Frame => self.draw_frame_widget(f, data_area),
            DisplayDataState::Global => self.draw_global_widget(f, data_area),
            DisplayDataState::Breakpoints => self.draw_breakpoints_widget(f, data_area),
        }

        if let Mode::Input = self.mode {
            self.draw_input_widget(f, size);
            return;
        }
        if let Some(error_message) = self.error_message.clone() {
            let popup = PopupWidget::new(&error_message);
            let area = popup.get_area(size);
            f.render_widget(Clear, area);
            popup.render(size, f.buffer_mut());
        }
        if self.show_help {
            let popup = PopupWidget::new(HELP);
            let area = popup.get_area(size);
            f.render_widget(Clear, area);
            popup.render(size, f.buffer_mut());
        }
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
        match self.mode {
            Mode::Input => match code {
                KeyCode::Char(c) => {
                    self.input_buffer.push(*c);
                }
                KeyCode::Backspace => {
                    self.input_buffer.pop();
                }
                KeyCode::Esc => {
                    self.mode = Mode::Normal;
                    self.input_buffer.clear();
                }
                KeyCode::Enter if !self.input_buffer.is_empty() => {
                    match parse_input(&self.input_buffer) {
                        Result::Ok(command) => self.execute_command(command)?,
                        Result::Err(err) => {
                            self.error_message = Some(format!("error: {}\n", err));
                        }
                    }

                    self.input_buffer.clear();
                    self.mode = Mode::Normal;
                }
                KeyCode::Enter => {
                    self.mode = Mode::Normal;
                }
                _ => {}
            },
            Mode::Normal => self.key_code_handler(code)?,
        }
        Ok(())
    }

    fn key_code_handler(&mut self, code: &KeyCode) -> Result<()> {
        if *code != KeyCode::Null && self.error_message.is_some() {
            self.error_message = None;
            return Ok(());
        } else if *code != KeyCode::Null && self.show_help {
            self.show_help = false;
            return Ok(());
        }
        if self.error_message.is_some() || self.show_help {
            return Ok(());
        }
        match code {
            KeyCode::Char('h') => {
                self.highlight_scope = !self.highlight_scope;
            }
            KeyCode::Char(':') => {
                self.mode = Mode::Input;
            }
            KeyCode::Char('?') => {
                self.show_help = true;
            }
            KeyCode::Char('z') => {
                self.scroll_offset = 0;
            }
            KeyCode::Esc => self.show_help = false,
            KeyCode::Enter if self.input_buffer.is_empty() => {
                self.execute_command(self.last_command.clone())?;
            }
            KeyCode::Right => self.stack_data_state.next(),
            KeyCode::Left => self.stack_data_state.previous(),
            _ => self.output.push_str(&format!("{:?}\n", code)),
        }
        Ok(())
    }

    fn execute_command(&mut self, command: Command) -> Result<()> {
        self.last_command = command.clone();
        match command {
            Command::Help => self.show_help = true,
            Command::Quit => self.is_running = false,
            Command::Step => {
                let _ = self.run_machine_once();
            }
            Command::Continue => {
                self.state = State::Running;
                if self.run_machine_once() {
                    self.state = State::Paused;
                }
            }
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
                let program = self.machine.program.clone();
                let symbol_table = self.machine.symbol_table.clone();
                *self.machine = Machine::new(program, symbol_table);
                self.set_next_instruction_pointer();
                self.state = State::Paused;
            }
            Command::Jump(address) => {
                self.machine.ip = address;
            }
            Command::HighlightScope => {
                self.highlight_scope = !self.highlight_scope;
            }
        }
        Ok(())
    }

    fn mouse_handler(&mut self, mouse_event: &event::MouseEvent) {
        let event::MouseEvent {
            kind,
            ..
            // column,
            // row,
            // modifiers,
        } = mouse_event;
        match kind {
            // event::MouseEventKind::Down(mouse_button) => format!("{mouse_button:?}\n"),
            // event::MouseEventKind::Up(mouse_button) => format!("{mouse_button:?}\n"),
            // event::MouseEventKind::Drag(mouse_button) => format!("{mouse_button:?}\n"),
            // event::MouseEventKind::Moved => String::from("Moved\n"),
            event::MouseEventKind::ScrollDown => self.scroll_offset += 1,
            event::MouseEventKind::ScrollUp => {
                self.scroll_offset = self.scroll_offset.saturating_sub(1)
            }
            // event::MouseEventKind::ScrollLeft => String::from("Scroll Left\n"),
            // event::MouseEventKind::ScrollRight => String::from("Scroll Right\n"),
            _ => {}
        };
    }

    fn paste_handler(&mut self, string: &str) {
        self.input_buffer.push_str(string);
    }

    fn resize_handler(&self, _: u16, _: u16) {}

    fn set_next_instruction_pointer(&mut self) {
        let mut vm = self.machine.clone();
        if vm.run_once().is_ok() {
            self.next_instruction_pointer = vm.ip;
        }
    }

    fn run_machine_once(&mut self) -> bool {
        if !self.machine.is_running {
            self.state = State::Paused;
            self.next_instruction_pointer = self.machine.ip;
            return false;
        }

        let result = match self.machine.run_once() {
            Result::Ok(_) => false,
            Result::Err(err) => {
                self.error_message = Some(err.to_string());
                return true;
            }
        };
        self.set_next_instruction_pointer();
        result
    }
}

fn get_instruction_and_data_area(main: Rect) -> (Rect, Rect) {
    let instruction_area_data_rect = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(55), Constraint::Percentage(45)].as_ref())
        .split(main);
    (instruction_area_data_rect[0], instruction_area_data_rect[1])
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
        "highlight-scope" => Ok(Command::HighlightScope),
        _ => Err(anyhow!(format!(
            "Unknown command `{}`\nPress `?` for help",
            input
        ))),
    }
}

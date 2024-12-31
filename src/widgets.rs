use std::collections::HashSet;

// TODO:
//  1. highlight next instruction to be executed if on screen
//  2. Do this for last instruction!  Just need to pipe it to the widget.
//  3. Also Wrap the next ip in a Result Enum to change the color if the next ip run is going to fail.
//
// TODO: make the call stack scollable.
// By using the up and down arrow keys you should be able to go
// up and down the call stack and this should update the local
// stack and arg stack display
//
// TODO: show source code of instruction if it contains a span

use ratatui::{
    buffer::Buffer,
    layout::{Constraint, Flex, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, Paragraph, Widget},
};

use crate::{instruction::Instruction, machine::Machine};

pub struct InstructionsWidget<'a> {
    pub ip: usize,
    pub next_ip: usize,
    pub location: std::ops::Range<usize>,
    pub instructions: &'a [Instruction],
    pub breakpoints: &'a HashSet<usize>,
    pub scroll_offset: u16,
    pub highlight_scope: bool,
}

impl<'a> InstructionsWidget<'a> {
    pub fn new(
        ip: usize,
        next_ip: usize,
        location: std::ops::Range<usize>,
        instructions: &'a [Instruction],
        breakpoints: &'a HashSet<usize>,
        highlight_scope: bool,
    ) -> Self {
        Self {
            ip,
            next_ip,
            location,
            instructions,
            breakpoints,
            scroll_offset: 0,
            highlight_scope,
        }
    }

    pub fn with_scroll_offset(mut self, scroll_offset: u16) -> Self {
        self.scroll_offset = scroll_offset;
        self
    }

    fn get_current_instruction_index(current_ip: usize, instructions: &'a [Instruction]) -> usize {
        let mut ip = 0;
        for (i, instruction) in instructions.iter().enumerate() {
            if ip == current_ip {
                return i;
            }
            ip += instruction.size();
        }
        0
    }

    fn format_instruction(
        ip: usize,
        next_ip: usize,
        location: std::ops::Range<usize>,
        instructions: &'a [Instruction],
        breakpoints: &'a HashSet<usize>,
        highlight_scope: bool,
    ) -> Text<'a> {
        let mut offset = 0;

        let mut result = Vec::new();

        for int in instructions.iter() {
            let line_style = if ip == offset {
                Style::default()
                    .fg(Color::Green)
                    .add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
            } else if next_ip == offset {
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
            } else if !highlight_scope || location.contains(&offset) {
                Style::default().fg(Color::White)
            } else {
                Style::default().fg(Color::DarkGray)
            };

            let selected = Span::styled(format!("0x{offset:06X} {offset:>5} "), Style::default());

            let breakpoint = if breakpoints.contains(&offset) {
                Span::raw("🔴".to_string())
            } else {
                Span::raw("  ".to_string())
            };

            let debug_int = int.debugger_display();

            let line = Line::from(vec![
                breakpoint,
                selected,
                Span::raw(format!("{debug_int:<20} ")),
            ])
            .style(line_style);

            result.push(line);
            offset += int.size();
        }

        Text::from(result)
    }
}

impl Widget for InstructionsWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let scroll_index = Self::get_current_instruction_index(self.ip, self.instructions)
            .saturating_sub(area.height as usize / 2) as u16;
        let text = Self::format_instruction(
            self.ip,
            self.next_ip,
            self.location,
            self.instructions,
            self.breakpoints,
            self.highlight_scope,
        );

        let scroll_x = (scroll_index + self.scroll_offset).min(self.instructions.len() as u16 - 1);
        let paragraph = Paragraph::new(text)
            .block(Block::default().borders(Borders::RIGHT).title("VM State"))
            .scroll((scroll_x, 0));
        paragraph.render(area, buf);
    }
}

pub(crate) struct FrameWidget<'a> {
    ip: usize,
    machine: &'a Machine,
    free: &'a [crate::instruction::Value],
}

impl<'a> FrameWidget<'a> {
    pub(crate) fn new(
        ip: usize,
        free: &'a [crate::instruction::Value],
        machine: &'a Machine,
    ) -> Self {
        Self { ip, machine, free }
    }
}

impl Widget for FrameWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let mut lines = Vec::new();

        let line = Line::from(vec![
            Span::raw("return address: "),
            Span::styled(
                self.machine
                    .get_current_frame()
                    .unwrap()
                    .return_address
                    .to_string(),
                Style::default().fg(Color::Green),
            ),
        ]);
        lines.push(line);

        let line = Line::from(vec![Span::raw("stack trace: ")]);
        lines.push(line);
        for (i, frame) in self.machine.stack.iter().enumerate() {
            let line = Line::from(vec![Span::styled(
                format!("[{i:02X}] {}", frame.scope_name),
                Style::default().fg(Color::Green),
            )]);
            lines.push(line);
        }

        let line = Line::from(vec![Span::raw("Free Stack")]);
        lines.push(line);
        for (i, item) in self.free.iter().enumerate() {
            let line = Line::from(vec![Span::raw(format!("[{}]: {}", i, item))]);
            lines.push(line);
        }

        let line = Line::from(vec![Span::raw("Args Stack")]);
        lines.push(line);
        let frame = self.machine.get_current_frame().unwrap();
        for (i, item) in frame.args.iter().enumerate() {
            let line = Line::from(vec![Span::raw(format!("[{}]: {}", i, item))]);
            lines.push(line);
        }

        let line = Line::from(vec![Span::raw("Local Stack")]);
        lines.push(line);
        for (i, item) in frame.stack.iter().enumerate() {
            let line = Line::from(vec![Span::raw(format!("[{}]: {}", i, item))]);
            lines.push(line);
        }

        let text = Text::from(lines);
        let title = format!("Frame: {ip:>4}", ip = self.ip);
        let paragraph =
            Paragraph::new(text).block(Block::default().borders(Borders::NONE).title(title));
        paragraph.render(area, buf);
    }
}

pub(crate) struct GlobalsWidget<'a> {
    ip: usize,
    globals: &'a [crate::instruction::Value],
}

impl<'a> GlobalsWidget<'a> {
    pub(crate) fn new(ip: usize, globals: &'a [crate::instruction::Value]) -> Self {
        Self { ip, globals }
    }
}

impl Widget for GlobalsWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let mut lines = Vec::new();

        for (i, item) in self.globals.iter().enumerate() {
            let line = Line::from(vec![Span::raw(format!("[{}]: {}", i, item))]);
            lines.push(line);
        }

        let text = Text::from(lines);
        let title = format!("Globals: {ip:>4}", ip = self.ip);
        let paragraph =
            Paragraph::new(text).block(Block::default().borders(Borders::NONE).title(title));
        paragraph.render(area, buf);
    }
}

pub(crate) struct BreakpointsWidget<'a> {
    ip: usize,
    breakpoints: &'a HashSet<usize>,
}

impl<'a> BreakpointsWidget<'a> {
    pub(crate) fn new(ip: usize, breakpoints: &'a HashSet<usize>) -> Self {
        Self { ip, breakpoints }
    }
}

impl Widget for BreakpointsWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let mut lines = Vec::new();

        let mut breakpoints = self.breakpoints.iter().collect::<Vec<_>>();
        breakpoints.sort();
        for (i, item) in breakpoints.iter().enumerate() {
            let line = Line::from(vec![Span::styled(
                format!("[{}]: {}", i, item),
                Style::default().fg(Color::Green),
            )]);
            lines.push(line);
        }

        let text = Text::from(lines);
        let title = format!("Breakpoints: {ip:>4}", ip = self.ip);
        let paragraph =
            Paragraph::new(text).block(Block::default().borders(Borders::NONE).title(title));
        paragraph.render(area, buf);
    }
}

pub(crate) struct PopupWidget<'a> {
    message: &'a str,
    width_percent: Option<u16>,
    height_percent: Option<u16>,
    width: Option<u16>,
    height: Option<u16>,
    title: Option<String>,
    dont_show_closing_message: bool,
}

impl<'a> PopupWidget<'a> {
    pub(crate) fn new(message: &'a str) -> Self {
        Self {
            message,
            width_percent: None,
            height_percent: None,
            width: None,
            height: None,
            title: None,
            dont_show_closing_message: true,
        }
    }

    pub fn width_closing_message(mut self, flag: bool) -> Self {
        self.dont_show_closing_message = flag;
        self
    }

    pub fn _with_width(mut self, width: u16) -> Self {
        self.width = Some(width);
        self
    }

    pub fn with_height(mut self, height: u16) -> Self {
        self.height = Some(height);
        self
    }

    pub fn with_percent_width(mut self, percent: u16) -> Self {
        self.width_percent = Some(percent);
        self
    }

    pub fn _with_percent_height(mut self, percent: u16) -> Self {
        self.height_percent = Some(percent);
        self
    }

    pub fn with_title(mut self, title: impl Into<String>) -> Self {
        self.title = Some(title.into());
        self
    }

    fn calculate_width(&self, area: Rect) -> u16 {
        self.width_percent.unwrap_or_else(|| {
            let max_line_length = self
                .message
                .lines()
                .map(|line| line.len())
                .max()
                .unwrap_or(1) as u16;
            ((max_line_length + 4) as f64 / area.width as f64 * 100.0).ceil() as u16
        })
    }

    fn calculate_height(&self, area: Rect) -> u16 {
        self.height_percent.unwrap_or_else(|| {
            let lines = self.message.lines().count() as u16;
            ((lines + 2) as f64 / area.height as f64 * 100.0).ceil() as u16
        })
    }

    pub fn get_area(&self, area: Rect) -> Rect {
        let vertical = if let Some(width) = self.width {
            Layout::horizontal([Constraint::Max(width)]).flex(Flex::Center)
        } else {
            let width_percent = self.calculate_width(area);
            Layout::horizontal([Constraint::Percentage(width_percent)]).flex(Flex::Center)
        };
        let horizontal = if let Some(height) = self.height {
            Layout::vertical([Constraint::Max(height)]).flex(Flex::Center)
        } else {
            let height_percent = self.calculate_height(area);
            Layout::vertical([Constraint::Percentage(height_percent)]).flex(Flex::Center)
        };

        let [area] = vertical.areas(area);
        let [area] = horizontal.areas(area);

        area
    }
}

impl Widget for PopupWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let area = self.get_area(area);
        let mut lines = self
            .message
            .lines()
            .map(|line| Line::from(vec![Span::raw(line)]))
            .collect::<Vec<_>>();
        if self.dont_show_closing_message {
            lines.push(Line::from(vec![Span::styled(
                "Press any key to close",
                Style::default().fg(Color::Green),
            )]));
        }
        let text = Text::from(lines);
        let paragraph = Paragraph::new(text).block(
            Block::default()
                .borders(Borders::ALL)
                .title(self.title.unwrap_or_default()),
        );
        paragraph.render(area, buf);
    }
}

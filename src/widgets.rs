use std::collections::HashSet;

use ratatui::{
    buffer::Buffer,
    layout::{Constraint, Flex, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, Paragraph, Widget},
};

use crate::{instruction::Instruction, machine::Frame};

pub struct InstructionsWidget<'a> {
    pub ip: usize,
    pub location: std::ops::Range<usize>,
    pub instructions: &'a [Instruction],
    pub breakpoints: &'a HashSet<usize>,
    pub scroll_offset: u16,
    pub highlight_scope: bool,
}

impl<'a> InstructionsWidget<'a> {
    pub fn new(
        ip: usize,
        location: std::ops::Range<usize>,
        instructions: &'a [Instruction],
        breakpoints: &'a HashSet<usize>,
        highlight_scope: bool,
    ) -> Self {
        Self {
            ip,
            location,
            instructions,
            breakpoints,
            scroll_offset: 0,
            highlight_scope,
        }
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
                    .add_modifier(Modifier::UNDERLINED)
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

            let debug_int = format!("{int}");

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
            .saturating_sub(area.height as usize / 2);
        let text = Self::format_instruction(
            self.ip,
            self.location,
            self.instructions,
            self.breakpoints,
            self.highlight_scope,
        );
        let paragraph = Paragraph::new(text)
            .block(Block::default().borders(Borders::ALL).title("VM State"))
            .scroll((scroll_index as u16, 0));
        paragraph.render(area, buf);
    }
}

pub(crate) struct FrameWidget<'a> {
    ip: usize,
    frame: &'a Frame,
    free: &'a [crate::instruction::Value],
}

impl<'a> FrameWidget<'a> {
    pub(crate) fn new(ip: usize, free: &'a [crate::instruction::Value], frame: &'a Frame) -> Self {
        Self { ip, frame, free }
    }
}

impl Widget for FrameWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let mut lines = Vec::new();

        let line = Line::from(vec![
            Span::raw("return address: "),
            Span::styled(
                self.frame
                    .return_address
                    .map(|a| a.to_string())
                    .unwrap_or("N/A".to_string()),
                Style::default().fg(Color::Green),
            ),
        ]);
        lines.push(line);

        let line = Line::from(vec![
            Span::raw("scope name: "),
            Span::styled(&self.frame.scope_name, Style::default().fg(Color::Green)),
        ]);
        lines.push(line);

        let line = Line::from(vec![Span::raw("Free Stack")]);
        lines.push(line);
        for (i, item) in self.free.iter().enumerate() {
            let line = Line::from(vec![Span::raw(format!("[{}]: {}", i, item))]);
            lines.push(line);
        }

        let line = Line::from(vec![Span::raw("Local Stack")]);
        lines.push(line);
        for (i, item) in self.frame.args.iter().enumerate() {
            let line = Line::from(vec![Span::raw(format!("[{}]: {}", i, item))]);
            lines.push(line);
        }

        let line = Line::from(vec![Span::raw("Frame Stack")]);
        lines.push(line);
        for (i, item) in self.frame.stack.iter().enumerate() {
            let line = Line::from(vec![Span::raw(format!("[{}]: {}", i, item))]);
            lines.push(line);
        }

        let text = Text::from(lines);
        let title = format!("Frame: {ip:>4}", ip = self.ip);
        let paragraph =
            Paragraph::new(text).block(Block::default().borders(Borders::ALL).title(title));
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
            Paragraph::new(text).block(Block::default().borders(Borders::ALL).title(title));
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
            Paragraph::new(text).block(Block::default().borders(Borders::ALL).title(title));
        paragraph.render(area, buf);
    }
}

pub(crate) struct PopupWidget<'a> {
    message: &'a str,
}

impl<'a> PopupWidget<'a> {
    pub(crate) fn new(message: &'a str) -> Self {
        Self { message }
    }

    fn calculate_popup_size(&self, area: Rect) -> (u16, u16) {
        let lines = self.message.lines().count() as u16;
        let max_line_length = self
            .message
            .lines()
            .map(|line| line.len() as u16)
            .max()
            .unwrap_or(1);

        let width_percent =
            ((max_line_length + 4) as f64 / area.width as f64 * 100.0).ceil() as u16;
        let height_percent = ((lines + 2) as f64 / area.height as f64 * 100.0).ceil() as u16;

        (width_percent.clamp(10, 90), height_percent.clamp(10, 90))
    }

    fn popup_area(&self, area: Rect) -> Rect {
        let (percent_x, percent_y) = self.calculate_popup_size(area);

        let vertical = Layout::vertical([Constraint::Percentage(percent_y)]).flex(Flex::Center);
        let horizontal = Layout::horizontal([Constraint::Percentage(percent_x)]).flex(Flex::Center);

        let [area] = vertical.areas(area);
        let [area] = horizontal.areas(area);

        area
    }

    pub fn get_area(&self, area: Rect) -> Rect {
        self.popup_area(area)
    }
}

impl Widget for PopupWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let area = self.get_area(area);
        let paragraph = Paragraph::new(self.message).block(Block::default().borders(Borders::ALL));
        paragraph.render(area, buf);
    }
}

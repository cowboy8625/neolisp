/// Header is 64 bytes
#[derive(Debug, Clone, PartialEq)]
pub struct Header {
    pub magic_number: [u8; 4], // NLVM
    pub version: u32,          // 0
    pub data_section_offset: u32,
    pub code_section_offset: u32,
    pub start: u32,
}

impl Default for Header {
    fn default() -> Self {
        Self::new()
    }
}

impl Header {
    pub const SIZE: u32 = 64;
    pub fn new() -> Self {
        Self {
            magic_number: *b"NLVM",
            version: 0,
            data_section_offset: Header::SIZE,
            code_section_offset: Header::SIZE,
            start: Header::SIZE,
        }
    }

    pub fn set_start(&mut self, start: u32) {
        self.start = start;
    }

    pub fn set_code_section_offset(&mut self, code_section_offset: u32) {
        self.code_section_offset = code_section_offset;
    }

    pub fn set_data_section_offset(&mut self, data_section_offset: u32) {
        self.data_section_offset = data_section_offset;
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(self.magic_number);
        bytes.extend(self.version.to_le_bytes());
        bytes.extend(self.data_section_offset.to_le_bytes());
        bytes.extend(self.code_section_offset.to_le_bytes());
        bytes.extend(self.start.to_le_bytes());
        let length = bytes.len() as u32;
        bytes.extend((0..(Header::SIZE - length)).map(|_| 0).collect::<Vec<u8>>());
        bytes
    }
}

impl From<&[u8]> for Header {
    fn from(bytes: &[u8]) -> Self {
        Self {
            magic_number: bytes[0..4].try_into().unwrap(),
            version: u32::from_le_bytes(bytes[4..8].try_into().unwrap()),
            data_section_offset: u32::from_le_bytes(bytes[8..12].try_into().unwrap()),
            code_section_offset: u32::from_le_bytes(bytes[12..16].try_into().unwrap()),
            start: u32::from_le_bytes(bytes[16..20].try_into().unwrap()),
        }
    }
}

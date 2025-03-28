pub mod bitbox;

pub fn u8_to_ascii_or_value(byte: u8) -> String {
    if byte.is_ascii_graphic() || byte == b' ' {
        (byte as char).to_string()
    } else {
        format!("\\x{:02X}", byte)
    }
}

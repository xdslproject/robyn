use std::mem::size_of;

use thiserror::Error;

/// Amount of bits from which data should be stored offline (inclusive), in bits.
const BITBOX_OFFLINE_THRESHOLD: usize = size_of::<usize>() * 8;

#[cfg_attr(feature = "gc", derive(dumpster::Collectable))]
#[derive(PartialEq, Eq, Debug)]
pub struct BitBox {
    len: usize,
    data: BitArrData,
}

/// Stores the data of a bit array, always as little endian in memory.
/// Materialized but inaccessible bits must be set to 0. Storage
/// must be of the smallest possible size, and must be inline if under
/// the inline threshold.
#[cfg_attr(feature = "gc", derive(dumpster::Collectable))]
#[derive(PartialEq, Eq, Debug)]
enum BitArrData {
    Inline(usize),
    Offline(Box<[usize]>),
}

use BitArrData::*;

#[derive(Error, Debug, PartialEq, Eq)]
#[error("cannot create bit box of {got} bytes as {max} is the maximum on this platform")]
pub struct TooManyBytesError {
    got: usize,
    max: usize,
}

impl BitBox {
    pub fn empty() -> Self {
        Self {
            len: 0,
            data: Inline(0),
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, TooManyBytesError> {
        if bytes.len() > usize::MAX / size_of::<usize>() {
            return Err(TooManyBytesError {
                got: bytes.len(),
                max: usize::MAX / size_of::<usize>(),
            });
        }

        /// Assumes bytes_usize is size_of(usize) bytes or fewer.
        #[inline(always)]
        fn bytes_to_usize(bytes_usize: &[u8]) -> usize {
            let mut data = 0;
            for &byte in bytes_usize.iter().rev() {
                data <<= 8;
                data |= byte as usize;
            }
            data.to_le()
        }

        Ok(Self {
            len: bytes.len() * 8,
            data: match bytes.len() <= BITBOX_OFFLINE_THRESHOLD / 8 {
                true => Inline(bytes_to_usize(bytes)),
                false => Offline(bytes.chunks(size_of::<usize>()).map(bytes_to_usize).collect()),
            },
        })
    }

    pub fn get(&self, id: usize) -> Option<bool> {
        #[inline(always)]
        fn get_bit(data: usize, id: usize) -> bool {
            eprintln!("data: {data:x}, id: {id}, res: {}", data.to_le() & (1 << id));
            data.to_le() & (1 << id) != 0
        }

        (id < self.len).then(|| match &self.data {
            Inline(data) => get_bit(*data, id),
            Offline(data) => get_bit(data[id / (size_of::<usize>() * 8)], id % (size_of::<usize>() * 8)),
        })
    }

    pub fn get_byte(&self, id: usize) -> Option<u8> {
        #[inline(always)]
        fn get_byte(data: usize, id: usize) -> u8 {
            (data >> (id * 8)) as u8
        }

        (id <= usize::MAX / 8 && id * 8 < self.len).then(|| match &self.data {
            Inline(data) => get_byte(*data, id),
            Offline(data) => get_byte(data[id / size_of::<usize>()], id % size_of::<usize>()),
        })
    }

    /// Amount of bits in the box.
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn as_bytes(&self) -> Option<&[u8]> {
        (self.len % 8 == 0).then(|| match &self.data {
            Inline(data) => &bytemuck::must_cast_ref::<usize, [u8; size_of::<usize>()]>(data)[..(self.len / 8)],
            Offline(data) => &bytemuck::must_cast_slice::<usize, u8>(data)[..(self.len / 8)],
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{BitArrData::*, BitBox};

    #[test]
    fn empty() {
        let bbox = BitBox::empty();
        assert_eq!(bbox.len(), 0);
        assert!(bbox.get(0).is_none());
        assert!(bbox.get(1).is_none());
        assert!(bbox.get_byte(0).is_none());
        assert!(bbox.get_byte(1).is_none());
        assert_eq!(bbox.as_bytes(), Some(&[] as &[u8]));
    }

    #[test]
    fn from_empty_bytes() {
        assert_eq!(BitBox::from_bytes(&[]), Ok(BitBox::empty()));
    }

    fn bytes(bytes: &[u8], bit_tests: &[(usize, Option<bool>)], byte_tests: &[(usize, Option<u8>)], is_inline: bool) {
        let bbox: BitBox = BitBox::from_bytes(bytes).unwrap();
        assert_eq!(bbox.len(), bytes.len() * 8);
        assert_eq!(bbox.as_bytes(), Some(bytes));

        for &(bit, expected) in bit_tests {
            assert_eq!(bbox.get(bit), expected, "testing bit {}", bit);
        }

        for &(byte, expected) in byte_tests {
            assert_eq!(bbox.get_byte(byte), expected, "testing byte {}", byte);
        }

        match bbox.data {
            Inline(_) => assert!(is_inline),
            Offline(_) => assert!(!is_inline),
        }
    }

    #[test]
    fn bytes_inline() {
        bytes(
            &[255, 7, 12],
            &[
                (0, Some(true)),
                (1, Some(true)),
                (5, Some(true)),
                (8, Some(true)),
                (12, Some(false)),
                (23, Some(false)),
                (24, None),
                (25, None),
            ],
            &[
                (0, Some(255)),
                (1, Some(7)),
                (2, Some(12)),
                (3, None),
                (4, None),
                (25, None),
            ],
            true,
        );
    }

    #[test]
    fn bytes_offline() {
        bytes(
            &[124, 7, 12, 8, 47, 32, 78, 2, 54, 87],
            &[
                (0, Some(false)),
                (1, Some(false)),
                (2, Some(true)),
                (12, Some(false)),
                (23, Some(false)),
                (72, Some(true)),
                (73, Some(true)),
                (75, Some(false)),
                (79, Some(false)),
                (80, None),
                (81, None),
                (1485, None),
            ],
            &[
                (0, Some(124)),
                (1, Some(7)),
                (2, Some(12)),
                (3, Some(8)),
                (4, Some(47)),
                (5, Some(32)),
                (6, Some(78)),
                (7, Some(2)),
                (8, Some(54)),
                (9, Some(87)),
                (10, None),
                (11, None),
                (140, None),
            ],
            false,
        );
    }
}

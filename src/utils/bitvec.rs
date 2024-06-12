use std::mem::size_of;

use bitvec::{field::BitField, vec::BitVec};

pub trait BitVecUtils {
    fn from_bytes(bytes: &[u8]) -> Self;
    fn as_bytes(&self) -> Option<&[u8]>;
}

impl BitVecUtils for BitVec {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut bitvec = BitVec::with_capacity(bytes.len() * 8);
        for (i, &byte) in bytes.iter().enumerate() {
            bitvec[i * 8..(i + 1) * 8].store::<u8>(byte);
        }
        bitvec
    }
    
    fn as_bytes(&self) -> Option<&[u8]> {
        (self.len() % 8 == 0).then(|| {
            
        })
    }
}

use std::slice;

use md5::{Digest, Md5};

#[repr(C)]
pub struct Md5Digest([u8; 16]);

#[unsafe(no_mangle)]
pub unsafe extern "C" fn md5(data: *const u8, len: usize) -> Md5Digest {
    let data = unsafe { slice::from_raw_parts(data, len) };
    Md5Digest(*Md5::new().chain_update(data).finalize().as_ref())
}

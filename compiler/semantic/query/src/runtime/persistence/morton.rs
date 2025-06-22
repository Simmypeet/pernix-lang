//! Morton code generation for 128-bit values.

/// Interleave bits of two 128-bit values into a 256-bit Morton code.
/// Returns (high, low) halves of the resultant 256-bit Morton value.
#[must_use]
pub const fn morton(x: u128, y: u128) -> (u128, u128) {
    const fn part_by_1(mut v: u128) -> u128 {
        // We need to spread out the bits so they occupy every other position
        // Since we're working with 128-bit values, we can only process 64 bits effectively
        // to fit the interleaved result in 128 bits
        v &= 0x0000_0000_0000_0000_FFFF_FFFF_FFFF_FFFFu128; // Keep only lower 64 bits
        v = (v | (v << 32)) & 0x0000_0000_FFFF_FFFF_0000_0000_FFFF_FFFFu128;
        v = (v | (v << 16)) & 0x0000_FFFF_0000_FFFF_0000_FFFF_0000_FFFFu128;
        v = (v | (v << 8)) & 0x00FF_00FF_00FF_00FF_00FF_00FF_00FF_00FFu128;
        v = (v | (v << 4)) & 0x0F0F_0F0F_0F0F_0F0F_0F0F_0F0F_0F0F_0F0Fu128;
        v = (v | (v << 2)) & 0x3333_3333_3333_3333_3333_3333_3333_3333u128;
        v = (v | (v << 1)) & 0x5555_5555_5555_5555_5555_5555_5555_5555u128;
        v
    }

    let sx = part_by_1(x);
    let sy = part_by_1(y);

    // Build 256-bit interleaved result:
    // even positions <- bits of x, odd positions <- bits of y.
    let low = sx | (sy << 1);
    
    // For 256-bit output, we need to handle overflow into high part
    // Process upper 64 bits separately
    let x_upper = part_by_1(x >> 64);
    let y_upper = part_by_1(y >> 64);
    let high = x_upper | (y_upper << 1);

    (high, low)
}

#[cfg(test)]
mod test;

pub const ret: u8 = 0x00;
pub const const_0: u8 = 0x01;
pub const const_1: u8 = 0x02;
pub const i_const_0: u8 = 0x03;
pub const i_const_1: u8 = 0x04;
pub const load_const: u8 = 0x05;
pub const load_fn: u8 = 0x06;
pub const load_0: u8 = 0x07;
pub const load_1: u8 = 0x08;
pub const load_2: u8 = 0x09;
pub const load_3: u8 = 0x0A;
pub const load_n: u8 = 0x0B;
pub const store_0: u8 = 0x0C;
pub const store_1: u8 = 0x0D;
pub const store_2: u8 = 0x0E;
pub const store_3: u8 = 0x0F;
pub const store_n: u8 = 0x10;
pub const discard: u8 = 0x11;
pub const dup: u8 = 0x12;
//
pub const add: u8 = 0x20;
pub const sub: u8 = 0x21;
pub const mul: u8 = 0x22;
pub const div: u8 = 0x23;
pub const n_to_i: u8 = 0x24;
//
pub const i_add: u8 = 0x30;
pub const i_sub: u8 = 0x31;
pub const i_mul: u8 = 0x32;
pub const i_div: u8 = 0x33;
pub const i_to_n: u8 = 0x34;
pub const i_shl: u8 = 0x35;
pub const i_shr: u8 = 0x36;
pub const i_ushr: u8 = 0x37;
pub const i_neg: u8 = 0x38;
pub const i_and: u8 = 0x39;
pub const i_or: u8 = 0x3A;
pub const i_xor: u8 = 0x3B;
pub const i_rem: u8 = 0x3C;
//
pub const call_fn: u8 = 0x40;
pub const call_self: u8 = 0x41;
pub const apply: u8 = 0x42;
pub const if_zero: u8 = 0x43;
pub const if_nil: u8 = 0x44;
pub const if_not_nil: u8 = 0x45;
pub const recur: u8 = 0x46;
//
pub const arr_new: u8 = 0x50;
pub const arr_get: u8 = 0x51;
pub const arr_set: u8 = 0x52;
pub const arr_len: u8 = 0x53;
pub const arr_init: u8 = 0x54;
pub const arr_slice: u8 = 0x55;
pub const arr_copy: u8 = 0x56;
//
pub const cmp_eq: u8 = 0x60;
pub const cmp_gt: u8 = 0x61;
pub const cmp_lt: u8 = 0x62;
pub const and_: u8 = 0x63;
pub const or_: u8 = 0x64;

pub const Opcode = enum(u8) {
    ret = 0x00,
    const_0 = 0x01,
    const_1 = 0x02,
    i_const_0 = 0x03,
    i_const_1 = 0x04,
    load_const = 0x05,
    load_fn = 0x06,
    load_0 = 0x07,
    load_1 = 0x08,
    load_2 = 0x09,
    load_3 = 0x0A,
    load_n = 0x0B,
    store_0 = 0x0C,
    store_1 = 0x0D,
    store_2 = 0x0E,
    store_3 = 0x0F,
    store_n = 0x10,
    discard = 0x11,
    dup = 0x12,
    //
    add = 0x20,
    sub = 0x21,
    mul = 0x22,
    div = 0x23,
    n_to_i = 0x24,
    //
    i_add = 0x30,
    i_sub = 0x31,
    i_mul = 0x32,
    i_div = 0x33,
    i_to_n = 0x34,
    i_shl = 0x35,
    i_shr = 0x36,
    i_ushr = 0x37,
    i_neg = 0x38,
    i_and = 0x39,
    i_or = 0x3A,
    i_xor = 0x3B,
    i_rem = 0x3C,
    //
    call_fn = 0x40,
    apply = 0x41,
    if_zero = 0x42,
    if_nil = 0x43,
    if_not_nil = 0x44,
    call_self = 0x45,
    //
    arr_new = 0x50,
    arr_get = 0x51,
    arr_set = 0x52,
    arr_len = 0x53,
    arr_init = 0x54,
    arr_slice = 0x55,
    arr_copy = 0x56,
    //
    cmp_eq = 0x60,
    cmp_gt = 0x61,
    cmp_lt = 0x62,
    @"and" = 0x63,
    @"or" = 0x64,
};

test "Opcode from int" {
    const std = @import("std");

    const opcode: Opcode = @enumFromInt(0x0B);
    try std.testing.expectEqual(opcode, Opcode.load_n);
}

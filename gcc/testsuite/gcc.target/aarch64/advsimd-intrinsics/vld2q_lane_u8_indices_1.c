#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-excess-errors "" { xfail arm*-*-* } } */
/* { dg-skip-if "" { arm*-*-* } } */

uint8x16x2_t
f_vld2q_lane_u8 (uint8_t * p, uint8x16x2_t v)
{
  uint8x16x2_t res;
  /* { dg-error "lane 16 out of range 0 - 15" "" { xfail arm*-*-* } 0 } */
  res = vld2q_lane_u8 (p, v, 16);
  /* { dg-error "lane -1 out of range 0 - 15" "" { xfail arm*-*-* } 0 } */
  res = vld2q_lane_u8 (p, v, -1);
  return res;
}

#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-excess-errors "" { xfail arm*-*-* } } */

int16x4x3_t
f_vld3_lane_s16 (int16_t * p, int16x4x3_t v)
{
  int16x4x3_t res;
  /* { dg-error "lane 4 out of range 0 - 3" "" { xfail arm*-*-* } 0 } */
  res = vld3_lane_s16 (p, v, 4);
  /* { dg-error "lane -1 out of range 0 - 3" "" { xfail arm*-*-* } 0 } */
  res = vld3_lane_s16 (p, v, -1);
  return res;
}

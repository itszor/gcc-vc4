#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-excess-errors "" { xfail arm*-*-* } } */

int32x4x2_t
f_vld2q_lane_s32 (int32_t * p, int32x4x2_t v)
{
  int32x4x2_t res;
  /* { dg-error "lane 4 out of range 0 - 3" "" { xfail arm*-*-* } 0 } */
  res = vld2q_lane_s32 (p, v, 4);
  /* { dg-error "lane -1 out of range 0 - 3" "" { xfail arm*-*-* } 0 } */
  res = vld2q_lane_s32 (p, v, -1);
  return res;
}

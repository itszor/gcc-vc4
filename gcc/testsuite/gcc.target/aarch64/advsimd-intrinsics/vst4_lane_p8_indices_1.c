#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-excess-errors "" { xfail arm*-*-* } } */

void
f_vst4_lane_p8 (poly8_t * p, poly8x8x4_t v)
{
  /* { dg-error "lane 8 out of range 0 - 7" "" { xfail arm*-*-* } 0 } */
  vst4_lane_p8 (p, v, 8);
  /* { dg-error "lane -1 out of range 0 - 7" "" { xfail arm*-*-* } 0 } */
  vst4_lane_p8 (p, v, -1);
  return;
}

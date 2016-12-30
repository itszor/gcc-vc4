/* vim: set ts=4 sw=4:
 *
 * Definitions of target machine for GNU compiler,
 * for Broadcom VideoCore IV processor.
 * Copyright (C) 1993-2013 Free Software Foundation, Inc.
 *
 * This file is part of GCC.
 *
 * GCC is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3, or (at your
 * option) any later version.
 *
 * GCC is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GCC; see the file COPYING3.  If not see
 * <http://www.gnu.org/licenses/>.
 */

#ifndef VC4_PROTOS_H
#define VC4_PROTOS_H 1

extern void vc4_init_expanders (void);
extern int vc4_initial_elimination_offset(int, int);

extern bool vc4_regno_ok_for_base_p (int regno, bool strict_p);
extern bool vc4_short_form_addr_p (enum machine_mode, rtx, bool);
extern bool vc4_conditional_form_addr_p (enum machine_mode, rtx, bool);
extern bool vc4_displacement_form_addr_p (enum machine_mode, rtx, bool);
extern bool vc4_long_form_addr_p (enum machine_mode, rtx, bool);
extern bool vc4_push_pop_operation_p (rtx op, bool is_push, bool returns);
extern const char *vc4_emit_multi_reg_push (rtx par);
extern const char *vc4_emit_multi_reg_pop (rtx par);
extern void vc4_expand_prologue (void);
extern void vc4_expand_epilogue (void);
extern void vc4_set_return_address (rtx, rtx);
extern bool vc4_shiftable_const (HOST_WIDE_INT);
#ifdef RTX_CODE
extern machine_mode vc4_select_cc_mode (RTX_CODE, rtx, rtx);
#endif
extern bool vc4_hard_regno_mode_ok (int regno, machine_mode mode);
extern bool vc4_valid_float_immediate (rtx x);
#endif

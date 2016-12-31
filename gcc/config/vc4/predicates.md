;; Predicate definitions for the Broadcom Videocore IV
;; Copyright (C) 2005-2013 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_special_predicate "vc4_push_multiple"
  (match_code "parallel")
{
  return vc4_push_pop_operation_p (op, true, false);
})

(define_special_predicate "vc4_pop_multiple"
  (match_code "parallel")
{
  return vc4_push_pop_operation_p (op, false, false);
})

(define_special_predicate "vc4_pop_multiple_return"
  (match_code "parallel")
{
  return vc4_push_pop_operation_p (op, false, true);
})

(define_special_predicate "cc_register"
  (and (match_code "reg")
       (match_test "REGNO (op) == CC_REGNUM")))

(define_predicate "arith_shift_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 1 && INTVAL (op) <= 8")))

; By default, register_operand permits (subreg:N (mem:M)) operands, which is
; never desirable for us (and appears to trigger an LRA bug).  Instead, only
; permit subregs of regs in operands (like ARM does).  This is probably a hack.

(define_predicate "s_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return (REG_P (op)
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
})

(define_predicate "alu_rhs_operand"
  (ior (match_operand 0 "immediate_operand")
       (match_operand 0 "s_register_operand")))

(define_predicate "extend_operand"
  (ior (match_operand 0 "s_register_operand")
       (match_operand 0 "memory_operand")))

(define_predicate "bit_number_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 0 && INTVAL (op) < 32")))

(define_predicate "btest_operand"
  (ior (match_operand 0 "bit_number_operand")
       (match_operand 0 "s_register_operand")))

(define_predicate "signext_operand"
  (ior (match_operand 0 "s_register_operand")
       (and (match_code "const_int")
	    (match_test "INTVAL (op) > 0 && INTVAL (op) <= 32"))))

(define_predicate "cmpbranch_immediate"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 0 && INTVAL (op) < 64")))

(define_predicate "cmpbranch_operand"
  (ior (match_operand 0 "s_register_operand")
       (match_operand 0 "cmpbranch_immediate")))

(define_predicate "s4_immediate_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= -8 && INTVAL (op) < 8")))

(define_predicate "low_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return REG_P (op) && REGNO (op) < 16;
})

(define_predicate "addcmpbranch_operand"
  (ior (match_operand 0 "low_register_operand")
       (match_operand 0 "s4_immediate_operand")))

(define_predicate "low_cmpbranch_operand"
  (ior (match_operand 0 "low_register_operand")
       (match_operand 0 "cmpbranch_immediate")))

(define_predicate "float_rhs_operand"
  (ior (match_operand 0 "s_register_operand")
       (and (match_code "const_double")
	    (match_test "vc4_valid_float_immediate (op)"))))

(define_predicate "float_div_lhs"
  (ior (match_operand 0 "s_register_operand")
       (and (match_code "const_double")
	    (match_test "real_equal (CONST_DOUBLE_REAL_VALUE (op),
				     &dconst1)"))))

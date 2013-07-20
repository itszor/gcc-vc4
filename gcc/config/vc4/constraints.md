;; Constraint definitions for the Motorola MCore
;; Copyright (C) 2011-2013 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Register constraints.
(define_register_constraint "f" "FAST_REGS"
  "@internal")

;; Integer constraints.
(define_constraint "I"
  "A 5-bit unsigned integer in the range 0 to 31, mostly used in ALU ops."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 31)")))

(define_constraint "J"
  "An 6-bit signed integer in the range -32 to 31, used by div ops."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32, 31)")))

(define_constraint "K"
  "A 6-bit unsigned integer in the rand 0 to 63, used by comparison ops."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 63)")))

(define_constraint "L"
  "A negative arithmetic operand in the range -32 to -1."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32, -1)")))

(define_constraint "M"
  "A constant loadable by bgeni."
  (and (match_code "const_int")
       (match_test "exact_log2 (ival) >= 0 && exact_log2 (ival) <= 30")))

(define_constraint "N"
  "A constant loadable by bmaskii, including -1."
  (and (match_code "const_int")
       (ior (match_test "ival == -1")
	    (and (match_test "exact_log2 (ival + 1) >= 0")
		 (match_test "exact_log2 (ival + 1) <= 30")))))

(define_constraint "O"
  "A constant allowed by cmov with two constants +/- 1 of each other."
  (and (match_code "const_int")
       (ior (match_test "insn_const_int_ok_for_constraint (ival, CONSTRAINT_I)")
	    (match_test "insn_const_int_ok_for_constraint (ival, CONSTRAINT_M)")
	    (match_test "insn_const_int_ok_for_constraint (ival, CONSTRAINT_N)")
	    (match_test "insn_const_int_ok_for_constraint (ival - 1, CONSTRAINT_M)")
	    (match_test "insn_const_int_ok_for_constraint (ival + 1, CONSTRAINT_N)"))))

(define_constraint "P"
  "A value that can be generated without an lrw instruction."
  (and (match_code "const_int")
       (match_test "mcore_const_ok_for_inline (ival)")))

;; Floating-point constraints.
(define_constraint "G"
  "@internal"
  (and (match_code "const_double")
       (match_test "insn_const_int_ok_for_constraint (hval, CONSTRAINT_I)")
       (match_test "insn_const_int_ok_for_constraint (ival, CONSTRAINT_I)")))

;; Other constraints.
(define_constraint "Q"
  "The integer constant one."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "R"
  "@internal"
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == LABEL_REF")))

(define_constraint "S"
  "An integer constant with 0, 1, or 2 bits clear."
  (and (match_code "const_int")
       (match_test "mcore_num_zeros (ival) <= 2")))

(define_constraint "T"
  "The integer constant two."
  (and (match_code "const_int")
       (match_test "ival == 2")))

(define_constraint "U"
  "The integer constant zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

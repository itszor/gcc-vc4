;; Constraint definitions for the Broadcom Videocore IV
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

(define_register_constraint "k" "SPECIAL_REGS"
  "@internal")

;; Integer constraints.
(define_constraint "I00"
  "Integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "I03"
  "Integer three."
  (and (match_code "const_int")
       (match_test "ival == 3")))

(define_constraint "Ish"
  "Shift amount for arithmetic+shift insn."
  (and (match_code "const_int")
       (match_test "ival >= 1 && ival <= 8")))

(define_constraint "Ip2"
  "A power of two."
  (and (match_code "const_int")
       (match_test "exact_log2 (ival & 0xffffffff) != -1")))

(define_constraint "Kp2"
  "A power of two, inverted."
  (and (match_code "const_int")
       (match_test "exact_log2 ((~ival) & 0xffffffff) != -1")))

(define_constraint "Ims"
  "A bitmask of N contiguous low-order bits (N <= 31)."
  (and (match_code "const_int")
       (match_test "ival != 0
		    && (ival & 0xffffffff) != 0xffffffff
		    && exact_log2 ((ival & 0xffffffff) + 1) != -1")))

(define_constraint "Iu5"
  "A 5-bit unsigned integer in the range 0 to 31, used in short ALU ops."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 31)")))

(define_constraint "Ju5"
  "A 5-bit integer in the range -31 to 0, used for immediate subtraction."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -31, 0)")))

(define_constraint "Ku5"
  "An inverted 5-bit unsigned integer."
  (and (match_code "const_int")
       (match_test "IN_RANGE (~ival, 0, 31)")))

(define_constraint "Is4"
  "An 4-bit signed integer in the range -8 to 7."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -8, 7)")))

(define_constraint "Is6"
  "An 6-bit signed integer in the range -32 to 31, used in conditional ALU ops."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32, 31)")))

(define_constraint "Js6"
  "An 6-bit signed integer in the range -31 to 32."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -31, 32)")))

(define_constraint "Ks6"
  "An 6-bit signed integer in the range -32 to 31, inverted."
  (and (match_code "const_int")
       (match_test "IN_RANGE (~ival, -32, 31)")))

(define_constraint "IU5"
  "A 5-bit integer in the range 0 to 31, or the same left-shifted 3 places."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 31)
		    || ((ival & 7) == 0 && IN_RANGE (ival >> 3, 0, 31))")))

(define_constraint "IS6"
  "A shiftable 6-bit signed immediate used for immediate addition."
  (and (match_code "const_int")
       (match_test "vc4_shiftable_const (ival)")))

(define_constraint "JS6"
  "A shiftable 6-bit signed immediate used for immediate subtraction."
  (and (match_code "const_int")
       (match_test "vc4_shiftable_const (-ival)")))

(define_constraint "Iu6"
  "A 6-bit unsigned integer in the range 0 to 63, used by comparison ops."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 63)")))

; 'L' for "eleven". This isn't actually used, maybe delete it.
;(define_constraint "IsL"
;  "A signed integer in the range -1024 to 1023, used by index memory ops."
;  (and (match_code "const_int")
;       (match_test "IN_RANGE (ival, -1024, 1023)")))

(define_constraint "IsX"
  "A signed integer in the range -32768 to 32767, used by 32-bit dyadic ALU ops."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 32767)")))

(define_constraint "JsX"
  "A signed integer in the range -32767 to 32768, used for immediate subtraction."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32767, 32768)")))

(define_constraint "KsX"
  "The inverse of an integer in the range -32768 to 32767."
  (and (match_code "const_int")
       (match_test "IN_RANGE (~ival, -32768, 32767)")))

(define_constraint "G01"
  "Floating-point 1.0"
  (and (match_code "const_double")
       (match_test "real_equal (CONST_DOUBLE_REAL_VALUE (op), &dconst1)")))

(define_memory_constraint "Us"
  "A memory operand suitable for short-form memory ops."
  (and (match_code "mem")
       (match_test "vc4_short_form_addr_p (mode, XEXP (op, 0), true)")))

(define_memory_constraint "Uc"
  "A memory operand suitable for conditional memory ops."
  (and (match_code "mem")
       (match_test "vc4_conditional_form_addr_p (mode, XEXP (op, 0), true)")))

(define_memory_constraint "Ud"
  "A memory operand suitable for 32-bit displacement-form memory ops."
  (and (match_code "mem")
       (match_test "vc4_displacement_form_addr_p (mode, XEXP (op, 0), true)")))

(define_memory_constraint "Ul"
  "A memory operand suitable for long-form memory ops."
  (and (match_code "mem")
       (match_test "vc4_long_form_addr_p (mode, XEXP (op, 0), true)")))

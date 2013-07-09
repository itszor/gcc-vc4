;; Predicate definitions for Motorola MCore.
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

;; Nonzero if OP is a fast register.

(define_predicate "fast_register"
  (match_code "reg,subreg")
  {
    if (!register_operand(op, mode))
      return 0;

    if (GET_CODE(op) != REG)
      return 0;

    return (REGNO_REG_CLASS(REGNO(op)) == FAST_REGS);
  }
)

;; Nonzero if OP is a small immediate integer of type I.

(define_predicate "alu_int"
  (match_code "const_int")
  {
    if (GET_CODE(op) != CONST_INT)
      return 0;

    return CONST_OK_FOR_I(INTVAL(op));
  }
)


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

(define_special_predicate "reg_or_pc_operand"
  (ior (match_operand 0 "register_operand")
       (match_code "pc")))

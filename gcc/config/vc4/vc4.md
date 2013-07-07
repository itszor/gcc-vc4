;;  Machine description the Motorola MCore
;;  Copyright (C) 1993-2013 Free Software Foundation, Inc.
;;  Contributed by Motorola.

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

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

(define_attr "length" "" (const_int 2))

(include "predicates.md")
(include "constraints.md")

;; --- Special --------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  "1"
  "nop"
  [(set_attr "length" "2")]
)

;; --- Moves ----------------------------------------------------------------

(define_insn "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=f,f,r")
	(match_operand:SI 1 "general_operand" "f,I,r"))]
  ""
  "@
  	mov %0, %1
  	mov %0, %C1
  	mov %0, %1"
  [(set_attr "length" "2,2,4")]
)

;; --- Arithmetic -----------------------------------------------------------

(define_insn "addsi3"
  [(set (match_operand:SI 0 "" "+f,f,r,r")
	(plus:SI (match_operand:SI 1 "" "%0,0,r,r")
		 (match_operand:SI 2 "" "f,I,r,I")))]
  ""
  "@
  	add %0, %2
  	add %0, %C2
  	add %0, %1, %2
  	add %0, %1, %C2"
  [(set_attr "length" "2, 2, 4, 4")]
)

(define_insn "subsi3"
  [(set (match_operand:SI 0 "" "+r")
	(minus:SI (match_operand:SI 1 "" "r")
		 (match_operand:SI 2 "" "r")))]
  ""
  "sub %0, %1, %2"
  [(set_attr "length" "4")]
)

;; --- Jumps ----------------------------------------------------------------

(define_insn "indirect_jump"
  [(set (pc)
  	(match_operand:SI 0 "nonimmediate_operand" "r"))]
  ""
  "b %0"
  [(set_attr "length" "2")]
)

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "b %l0"
  [(set_attr "length" "2")]
)


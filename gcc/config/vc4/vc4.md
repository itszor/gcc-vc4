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

;; --- SI moves -------------------------------------------------------------

(define_expand "movsi"
  [
    (set
      (match_operand:SI 0 "general_operand" "")
      (match_operand:SI 1 "general_operand" "")
    )
  ]
  ""
  ""
)

(define_insn "*vc4_si_literals"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,r,r")
      (match_operand:SI 1 "const_int_operand" "I,I,i")
    )
  ]
  ""
  "@
  	mov %0, %C1
  	mov %0, %C1
  	mov %0, %C1"
  [(set_attr "length" "2,4,6")]
)

(define_insn "*vc4_si_moves"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,r")
      (match_operand:SI 1 "register_operand" "f,r")
    )
  ]
  ""
  "@
  	mov %0, %1
  	mov %0, %1"
  [(set_attr "length" "2,4")]
)

(define_insn "*vc4_si_load_indexed_by_register"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r")
      (mem:SI
	(plus:SI
	  (mult:SI
	    (match_operand:SI 1 "register_operand" "%r")
	    (const_int 4)
	  )
	  (match_operand:SI 2 "register_operand" "r")
	)
      )
    )
  ]
  ""
  "ld %0, (%2, %1)"
  [(set_attr "length" "4")]
)

(define_insn "*vc4_si_load_indexed_by_constant"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,r")
      (mem:SI
	(plus:SI
	  (match_operand:SI 2 "register_operand" "f,r")
	  (match_operand:SI 1 "const_int_operand" "I,i")
	)
      )
    )
  ]
  ""
  "@
  	ld %0, %1 (%2)
  	ld %0, %1 (%2)"
  [(set_attr "length" "2,4")]
)

(define_insn "*vc4_si_loads"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,r")
      (mem:SI
	(match_operand:SI 1 "register_operand" "f,r")
      )
    )
  ]
  ""
  "@
  	ld %0, (%1)
  	ld %0, (%1)"
  [(set_attr "length" "2,4")]
)

(define_insn "*vc4_si_stores_indexed_by_constant"
  [
    (set
      (mem:SI
	(plus:SI
	  (match_operand:SI 2 "register_operand" "f,r")
	  (match_operand:SI 1 "const_int_operand" "I,i")
	)
      )
      (match_operand:SI 0 "register_operand" "f,r")
    )
  ]
  ""
  "@
  	st %0, %1 (%2)
  	st %0, %1 (%2)"
  [(set_attr "length" "2,4")]
)

(define_insn "*vc4_si_stores"
  [
    (set
      (mem:SI
	(match_operand:SI 1 "register_operand" "f,r")
      )
      (match_operand:SI 0 "register_operand" "f,r")
    )
  ]
  ""
  "@
  	st %1, (%0)
  	st %1, (%0)"
  [(set_attr "length" "2,4")]
)

;; --- QI moves -------------------------------------------------------------

(define_expand "movqi"
  [
    (set
      (match_operand:QI 0 "general_operand" "")
      (match_operand:QI 1 "general_operand" "")
    )
  ]
  ""
  ""
)

(define_insn "*vc4_qi_literals"
  [
    (set
      (match_operand:QI 0 "register_operand" "=f")
      (match_operand:QI 1 "const_int_operand" "I")
    )
  ]
  ""
  "@
  	mov %0, %C1"
  [(set_attr "length" "2")]
)

(define_insn "*vc4_qi_moves"
  [
    (set
      (match_operand:QI 0 "nonimmediate_operand" "=f,r")
      (match_operand:QI 1 "register_operand" "f,r")
    )
  ]
  ""
  "@
  	mov %0, %1
  	mov %0, %1"
  [(set_attr "length" "2,4")]
)

(define_insn "*vc4_qi_loads"
  [
    (set
      (match_operand:QI 0 "register_operand" "=f,r")
      (mem:QI
	(match_operand:SI 1 "register_operand" "f,r")
      )
    )
  ]
  ""
  "@
  	ldb %0, (%1)
  	ldb %0, (%1)"
  [(set_attr "length" "2,4")]
)

(define_insn "*vc4_qi_load_indexed"
  [
    (set
      (match_operand:QI 0 "register_operand" "=r")
      (mem:QI
	(plus:SI
	  (match_operand:SI 1 "register_operand" "r")
	  (match_operand:SI 2 "register_operand" "r")
	)
      )
    )
  ]
  ""
  "ldb %0, (%1, %2)"
  [(set_attr "length" "4")]
)

;; --- Arithmetic -----------------------------------------------------------

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=f,f,?r,?r")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,0,r,r")
		 (match_operand:SI 2 "nonmemory_operand" "f,I,r,I")))]
  ""
  "@
  	add %0, %2
  	add %0, %C2
  	add %0, %1, %2
  	add %0, %1, %C2"
  [(set_attr "length" "2,2,4,4")]
)

(define_insn "subsi3"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,f,?r,?r")
      (minus:SI
	(match_operand:SI 1 "register_operand" "0,0,r,r")
	(match_operand:SI 2 "nonmemory_operand" "f,I,r,I")
      )
    )
  ]
  ""
  "@
  	sub %0, %2
  	sub %0, %C2
  	sub %0, %1, %2
  	sub %0, %1, %2"
  [(set_attr "length" "2,2,4,4")]
)

(define_insn "ashlsi3"
  [
    (set
      (match_operand:SI 0 "" "=f,f,r,r")
      (ashift:SI
        (match_operand:SI 1 "" "%0,0,r,r")
	(match_operand:SI 2 "" "f,I,r,I")
      )
    )
  ]
  ""
  "@
  	lsl %0, %2
  	lsl %0, %C2
  	lsl %0, %1, %2
  	lsl %0, %1, %C2"
  [(set_attr "length" "2,2,4,4")]
)

(define_insn "lshrsi3"
  [
    (set
      (match_operand:SI 0 "" "=f,f,r,r")
      (lshiftrt:SI
        (match_operand:SI 1 "" "%0,0,r,r")
	(match_operand:SI 2 "" "f,I,r,I")
      )
    )
  ]
  ""
  "@
  	lsr %0, %2
  	lsr %0, %C2
  	lsr %0, %1, %2
  	lsr %0, %1, %C2"
  [(set_attr "length" "2,2,4,4")]
)

;; --- Sign extension -------------------------------------------------------

(define_insn "zero_extendqisi2"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r")
      (zero_extend:SI
        (match_operand:QI 1 "general_operand" "0")
      )
    )
  ]
  ""
  ""
  [(set_attr "length" "0")]
)

(define_insn "extendqisi2"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,r")
      (sign_extend:SI
        (match_operand:QI 1 "register_operand" "0,r")
      )
    )
  ]
  ""
  "@
  	exts %0, #8
	exts %0, %1, #8"
  [(set_attr "length" "2,4")]
)

;; --- Jumps ----------------------------------------------------------------

(define_insn "indirect_jump"
  [(set (pc)
  	(match_operand:SI 0 "register_operand" "r"))]
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


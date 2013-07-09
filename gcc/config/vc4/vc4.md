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
      (match_operand:SI 0 "register_operand" "=f,?r,?r")
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
      (match_operand:SI 0 "register_operand" "=f,?r")
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

;; Fast and slow ALU instructions.

(define_code_iterator alu_fast
  [
    plus
    mult
    minus
    xor
    lshiftrt
    ashift
    ashiftrt
  ]
)

(define_code_iterator alu_slow
  [
    plus
    mult
    xor
    minus
    and
    rotate
    ior
    lshiftrt
    ashift
    ashiftrt
  ]
)

;; Mappings from insn names to the RTL node that actually does it.

(define_code_attr insn
  [
    (plus "addsi3")
    (mult "mulsi3")
    (xor "xorsi3")
    (minus "subsi3")
    (and "andsi3")
    (rotate "rotrsi3")
    (ior "iorsi3")
    (lshiftrt "lshrsi3")
    (ashift "ashlsi3")
    (ashiftrt "ashrsi3")
  ]
)

;; Mappings from insn names to the VC4 opcode that implements it.

(define_code_attr opcode
  [
    (plus "add")
    (mult "mul")
    (xor "xor")
    (minus "sub")
    (and "and")
    (rotate "ror")
    (ior "xor")
    (lshiftrt "lsr")
    (ashift "lsl")
    (ashiftrt "asr")
  ]
)

;; Expand all ALU instructions.

(define_expand "<alu_slow:insn>"
  [
    (set
      (match_operand:SI 0 "general_operand" "")
      (alu_slow:SI
	(match_operand:SI 1 "general_operand" "")
	(match_operand:SI 2 "general_operand" "")
      )
    )
  ]
  ""
  ""
)

;; Actually generate the code for the ALU instructions.

(define_insn "*vc4_<alu_fast:insn>_reg_reg"
  [
    (set
      (match_operand:SI 0 "fast_register" "=f")
      (alu_fast:SI
	(match_operand:SI 1 "fast_register" "0")
	(match_operand:SI 2 "fast_register" "f")
      )
    )
  ]
  ""
  "<alu_fast:opcode> %0, %2 ; fast 2op"
  [(set_attr "length" "2")]
)

(define_insn "*vc4_<alu_fast:insn>_int"
  [
    (set
      (match_operand:SI 0 "fast_register" "=f")
      (alu_fast:SI
	(match_operand:SI 1 "fast_register" "0")
	(match_operand:SI 2 "alu_int" "i")
      )
    )
  ]
  ""
  "<alu_fast:opcode> %0, #%2 ; fast 2op"
  [(set_attr "length" "2")]
)

(define_insn "*vc4_<alu_slow:insn>_reg_reg"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r")
      (alu_slow:SI
	(match_operand:SI 1 "register_operand" "r")
	(match_operand:SI 2 "register_operand" "r")
      )
    )
  ]
  ""
  "<alu_slow:opcode> %0, %1, %2"
  [(set_attr "length" "4")]
)

(define_insn "*vc4_<alu_slow:insn>_reg_and_largeint"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r,r")
      (alu_slow:SI
	(match_operand:SI 1 "register_operand" "r,0")
	(match_operand:SI 2 "const_int_operand" "I,i")
      )
    )
  ]
  ""
  "@
  	<alu_slow:opcode> %0, %1, #%2
  	<alu_slow:opcode> %0, #%2 ; largeint 2op"
  [(set_attr "length" "6,4")]
)

;; --- Sign extension -------------------------------------------------------

(define_insn "zero_extendqisi2"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,r")
      (zero_extend:SI
        (match_operand:QI 1 "general_operand" "0,r")
      )
    )
  ]
  ""
  "@
  	extu %0, #8
  	extu %0, %1, #8"
  [(set_attr "length" "2,4")]
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


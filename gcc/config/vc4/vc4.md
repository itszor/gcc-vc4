;;  vim: set ts=8 sw=2:
;;  Machine description the Broadcom Videocore IV.
;;  Copyright (C) 1993-2013 Free Software Foundation, Inc.
;;  Contributed by David Given <dg@cowlark.com>.

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
  {
    /* Ensure that mem->mem moves are split via a temporary register. */
    if (GET_CODE(operands[0]) == MEM)
      operands[1] = force_reg(SImode, operands[1]);
  }
)

(define_insn "*vc4_si_simple_moves"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,f,r,r,r")
      (match_operand:SI 1 "nonmemory_operand" "I,f,I,i,r")
    )
  ]
  ""
  "@
  	mov %0, #%1 ; fast
  	mov %0, %1 ; fast
  	mov %0, #%1 ; slow smallint
  	mov %0, #%1 ; largeint
  	mov %0, %1 ; slow"
  [(set_attr "length" "2,2,4,6,4")]
)

(define_insn "*vc4_si_load_indexed_by_register"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r")
      (mem:SI
	(plus:SI
	  (mult:SI
	    (match_operand:SI 1 "register_operand" "r")
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

(define_insn "*vc4_si_load"
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

(define_insn "*vc4_si_store_indexed_by_constant"
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
  	st %0, (%1)
  	st %0, (%1)"
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
  {
    if (GET_CODE(operands[0]) == MEM)
      operands[1] = force_reg(SImode, operands[1]);
  }
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

(define_insn "*vc4_ai_load_indexed_by_constant"
  [
    (set
      (match_operand:QI 0 "register_operand" "=r,r")
      (mem:QI
	(plus:SI
	  (match_operand:SI 2 "register_operand" "r,r")
	  (match_operand:SI 1 "const_int_operand" "I,i")
	)
      )
    )
  ]
  ""
  "@
  	ldb %0, %1 (%2)
  	ldb %0, %1 (%2)"
  [(set_attr "length" "4,4")]
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

;; --- Generic arithmetic ---------------------------------------------------

;; A number of the VC4 ALU instructions follow a common form, which means
;; we can generate the patterns for them algorithmically.

;; Fast and slow ALU instructions. (They must not overlap.)

(define_code_iterator alu_fast
  [
    plus
    mult
    xor
    lshiftrt
    ashift
    ashiftrt
  ]
)

(define_code_iterator alu_slow
  [
    and
    rotate
    ior
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

(define_expand "<alu_fast:insn>"
  [
    (set
      (match_operand:SI 0 "general_operand" "")
      (alu_fast:SI
	(match_operand:SI 1 "general_operand" "")
	(match_operand:SI 2 "general_operand" "")
      )
    )
  ]
  ""
  {
    /* Ensure we don't get any mem operands. */
    operands[1] = force_reg(SImode, operands[1]);
    operands[2] = force_reg(SImode, operands[2]);
  }
)

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
  {
    /* Ensure we don't get any mem operands. */
    operands[1] = force_reg(SImode, operands[1]);
    operands[2] = force_reg(SImode, operands[2]);
  }
)

;; Actually generate the code for the ALU instructions.

(define_insn "*vc4_<alu_fast:insn>_fast"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,f,r,r,r,r")
      (alu_fast:SI
	(match_operand:SI 1 "register_operand" "0,0,r,0,0,r")
	(match_operand:SI 2 "nonmemory_operand" "I,f,I,i,r,r")
      )
    )
  ]
  ""
  "@
  	<alu_fast:opcode> %0, #%2 ; fast smallint
  	<alu_fast:opcode> %0, %2 ; fast reg
  	<alu_fast:opcode> %0, %1, #%2 ; slow smallint
  	<alu_fast:opcode> %0, #%2 ; largeint 2op
  	<alu_fast:opcode> %0, %2 ; slow
  	<alu_fast:opcode> %0, %1, %2"
  [(set_attr "length" "2,2,4,6,4,4")]
)

(define_insn "*vc4_<alu_slow:insn>_slow"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r,r,r,r")
      (alu_slow:SI
	(match_operand:SI 1 "register_operand" "r,0,0,r")
	(match_operand:SI 2 "nonmemory_operand" "I,i,r,r")
      )
    )
  ]
  ""
  "@
  	<alu_slow:opcode> %0, %1, #%2
  	<alu_slow:opcode> %0, #%2 ; largeint 2op
  	<alu_slow:opcode> %0, %2 ; slow
  	<alu_slow:opcode> %0, %1, %2"
  [(set_attr "length" "4,6,4,4")]
)

;; --- Special-cased arithmetic ---------------------------------------------

;; These ALU instructions are weird and so need special-cased patterns.

(define_insn "subsi3"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,f,f,f,r,r,r,r,r,r,r")
      (minus:SI
	(match_operand:SI 1 "register_operand" "0,I,0,f,r,I,0,i,0,r,r")
	(match_operand:SI 2 "nonmemory_operand" "I,0,f,0,I,0,i,0,r,0,r")
      )
    )
  ]
  ""
  "@
  	sub %0, #%2 ; fast smallint
	rsb %0, #%1 ; fast smallint
  	sub %0, %2 ; fast reg
	rsb %0, %1 ; fast smallint
  	sub %0, %1, #%2 ; slow smallint
  	rsb %0, %1, #%1 ; slow smallint
  	sub %0, #%2 ; largeint 2op
  	rsb %0, #%1 ; largeint 2op
  	sub %0, %2 ; slow
  	rsb %0, %1 ; slow
  	sub %0, %1, %2"
  [(set_attr "length" "2,2,2,2,4,4,6,6,4,4,4")]
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


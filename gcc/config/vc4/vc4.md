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

(define_constants
  [
    (GP_REGNO 25)
    (SP_REGNO 26)
    (LR_REGNO 27)
    (CC_REGNO 29)
  ]
)

(define_mode_iterator QHSI [QI HI SI])
(define_mode_iterator QHI [QI HI])
(define_mode_iterator SIF [SI SF])
(define_mode_attr suffix [(QI "b") (HI "h") (SI "") (SF "")])

(define_c_enum "unspec" [
  UNSPEC_PRLG_STK
])

;; --- Special --------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  "1"
  "nop"
  [(set_attr "length" "2")]
)

;; --- Move expanders -------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:QHSI 0 "nonimmediate_operand" "")
	(match_operand:QHSI 1 "general_operand" ""))]
  ""
{
  /* Ensure that mem->mem moves are split via a temporary register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (<MODE>mode, operands[1]);
})

;(define_expand "movdi"
;  [(set (match_operand:DI 0 "nonimmediate_operand" "")
;	(match_operand:DI 1 "general_operand" ""))]
;  ""
;{
;  if (MEM_P (operands[0]))
;    operands[1] = force_reg (DImode, operands[1]);
;})

;; --- Generic moves --------------------------------------------------------

(define_insn "*mov<mode>_insn"
  [(set (match_operand:QHSI 0 "nonimmediate_operand" "=f,f,r,r,r, f,r,Us,m")
	(match_operand:QHSI 1 "general_operand"	      "I,f,I,i,r,Us,m, f,r"))]
  ""
  "@
  mov %0, #%1\t; fast
  mov %0, %1\t; fast
  mov %0, #%1\t; slow smallint
  mov %0, #%1\t; largeint
  mov %0, %0, %1\t; slow
  ld<suffix> %0, %1\t;short form
  ld<suffix> %0, %1\t;long form
  st<suffix> %1, %0\t;short form
  st<suffix> %1, %0\t;long form"
  [(set_attr "length" "2,2,4,6,4,2,4,2,4")]
)

;(define_insn "*movdi_insn"
;  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,m")
;	(match_operand:DI 1 "general_operand"       "r,i,m,r"))]
;  ""
;  "@
;  movdi %0,%1
;  movdi %0,#%1
;  ldd %0,%1
;  std %1,%0"
;  [(set_attr "length" "4")]
;)

;; pushes/pops.

; This will look like:
; (parallel [(set (base-reg)
;		  (plus (base-reg) #-12))
;	     (set (mem:SI (plus (base-reg) #0)) (r6))
;	     (set (mem:SI (plus (base-reg) #4)) (r7))
;	     (set (mem:SI (plus (base-reg) #8)) (lr))])


(define_insn "vc4_push_multi"
  [(match_parallel 0 "vc4_push_multiple"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_dup 1)
		   (match_operand:SI 2 "const_int_operand" "i")))
     (set (mem:SI (plus:SI
		    (match_dup 1)
		    (match_operand:SI 4 "const_int_operand" "i")))
	  (match_operand:SI 3 "register_operand" "r"))])]
  "reload_completed"
{
  return vc4_emit_multi_reg_push (operands[0]);
}
  [(set_attr "length" "2")]
)

(define_insn "vc4_pop_multi_return"
  [(match_parallel 0 "vc4_pop_multiple_return"
    [(return)
     (set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_dup 1)
		   (match_operand:SI 2 "const_int_operand" "i")))
     (set (match_operand:SI 3 "register_operand" "=rk")
	  (mem:SI (match_dup 1)))])]
  "reload_completed"
{
  return vc4_emit_multi_reg_pop (operands[0]);
}
  [(set_attr "length" "2")]
)

(define_insn "vc4_pop_multi"
  [(match_parallel 0 "vc4_pop_multiple"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_dup 1)
		   (match_operand:SI 2 "const_int_operand" "i")))
     (set (match_operand:SI 3 "register_operand" "=r")
	  (mem:SI (match_dup 1)))])]
  "reload_completed"
{
  return vc4_emit_multi_reg_pop (operands[0]);
}
  [(set_attr "length" "2")]
)

(define_expand "prologue"
  [(pc)]
  ""
{
  vc4_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(pc)]
  ""
{
  vc4_expand_epilogue ();
  DONE;
})

(define_insn "vc4_return"
  [(return)]
  ""
  "rts"
  [(set_attr "length" "2")]
)

(define_insn "stack_tie"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:SI 0 "register_operand" "r")
		     (match_operand:SI 1 "register_operand" "r")]
		    UNSPEC_PRLG_STK))]
  ""
  ""
  [(set_attr "length" "0")]
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

(define_code_attr alu_insn
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

(define_code_attr alu_opcode
  [
    (plus "add")
    (mult "mul")
    (xor "eor")
    (minus "sub")
    (and "and")
    (rotate "ror")
    (ior "or")
    (lshiftrt "lsr")
    (ashift "lsl")
    (ashiftrt "asr")
  ]
)

;; Expand all ALU instructions.

(define_expand "<alu_fast:alu_insn>"
  [
    (set
      (match_operand:SI 0 "register_operand" "")
      (alu_fast:SI
	(match_operand:SI 1 "register_operand" "")
	(match_operand:SI 2 "nonmemory_operand" "")
      )
    )
  ]
  ""
  {
  }
)

(define_expand "<alu_slow:alu_insn>"
  [
    (set
      (match_operand:SI 0 "register_operand" "")
      (alu_slow:SI
	(match_operand:SI 1 "register_operand" "")
	(match_operand:SI 2 "nonmemory_operand" "")
      )
    )
  ]
  ""
  {
  }
)

;; Actually generate the code for the ALU instructions.

(define_insn "*vc4_<alu_fast:alu_insn>_fast"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,f,r,r,r")
      (alu_fast:SI
	(match_operand:SI 1 "register_operand" "0,0,r,0,r")
	(match_operand:SI 2 "nonmemory_operand" "I,f,I,i,r")
      )
    )
  ]
  ""
  "@
  	<alu_fast:alu_opcode> %0, #%2 ; fast smallint
  	<alu_fast:alu_opcode> %0, %2 ; fast reg
  	<alu_fast:alu_opcode> %0, %1, #%2 ; slow smallint
  	<alu_fast:alu_opcode> %0, #%2 ; largeint 2op
  	<alu_fast:alu_opcode> %0, %1, %2"
  [(set_attr "length" "2,2,4,6,4")]
)

(define_insn "*vc4_<alu_slow:alu_insn>_slow"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r,r,r")
      (alu_slow:SI
	(match_operand:SI 1 "register_operand" "r,0,r")
	(match_operand:SI 2 "nonmemory_operand" "I,i,r")
      )
    )
  ]
  ""
  "@
  	<alu_slow:alu_opcode> %0, %1, #%2
  	<alu_slow:alu_opcode> %0, #%2 ; largeint 2op
  	<alu_slow:alu_opcode> %0, %1, %2"
  [(set_attr "length" "4,6,4")]
)

;; --- Special-cased arithmetic ---------------------------------------------

;; These ALU instructions are weird and so need special-cased patterns.

(define_insn "subsi3"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,f,f,f,r,r,r,r,r")
      (minus:SI
        (match_operand:SI 1 "register_operand" "0,I,0,f,r,I,0,i,r")
        (match_operand:SI 2 "nonmemory_operand" "I,0,f,0,I,0,i,0,r")
      )
    )
  ]
  ""
  "@
  	sub %0, #%2 ; fast smallint
	rsub %0, #%1 ; fast smallint
  	sub %0, %2 ; fast reg
	rsub %0, %1 ; fast smallint
  	sub %0, %1, #%2 ; slow smallint
  	rsub %0, %1, #%1 ; slow smallint
  	sub %0, #%2 ; largeint 2op
  	rsub %0, #%1 ; largeint 2op
  	sub %0, %1, %2"
  [(set_attr "length" "2,2,2,2,4,4,6,6,4")]
)

(define_insn "divsi3"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r,r")
      (div:SI
        (match_operand:SI 1 "register_operand" "r,r")
        (match_operand:SI 2 "nonmemory_operand" "r,J")
      )
    )
  ]
  ""
  "@
  	divss %0, %1, %2
  	divss %0, %1, #%2"
  [(set_attr "length" "4,4")]
)

(define_insn "udivsi3"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r,r")
      (udiv:SI
        (match_operand:SI 1 "register_operand" "r,r")
        (match_operand:SI 2 "nonmemory_operand" "r,J")
      )
    )
  ]
  ""
  "@
  	divuu %0, %1, %2
  	divuu %0, %1, #%2"
  [(set_attr "length" "4,4")]
)

;; --- Float arithmetic -----------------------------------------------------

;; These are mostly simple 2op and 3op instructions and can be generated
;; algorithmically.

(define_code_iterator fpu_list_2op
  [
    abs
  ]
)

(define_code_iterator fpu_list_3op
  [
    plus
    minus
    mult
    div
    smin
    smax
  ]
)

;; Mappings from insn names to the RTL node that actually does it.

(define_code_attr fpu_insn
  [
    (plus "addsf3")
    (mult "mulsf3")
    (minus "subsf3")
    (div "divsf3")
    (smin "sminsf3")
    (smax "smaxsf3")
    (abs "abssf2")
  ]
)

;; Mappings from insn names to the VC4 opcode that implements it.

(define_code_attr fpu_opcode
  [
    (plus "fadd")
    (mult "fmul")
    (minus "fsub")
    (div "fdiv")
    (smin "fmin")
    (smax "fmax")
    (abs "fabs")
  ]
)

;; Expand all FPU instructions.

(define_insn "<fpu_list_3op:fpu_insn>"
  [
    (set
      (match_operand:SF 0 "register_operand" "=r")
      (fpu_list_3op:SF
	(match_operand:SF 1 "register_operand" "r")
	(match_operand:SF 2 "register_operand" "r")
      )
    )
  ]
  ""
  "<fpu_list_3op:fpu_opcode> %0, %1, %2"
  [(set_attr "length" "4")]
)

(define_insn "<fpu_list_2op:fpu_insn>"
  [
    (set
      (match_operand:SF 0 "register_operand" "=r")
      (fpu_list_2op:SF
	(match_operand:SF 1 "register_operand" "r")
      )
    )
  ]
  ""
  "<fpu_list_2op:fpu_opcode> %0, %1, %1"
  [(set_attr "length" "4")]
)

;; Extra float operations, such as conversions.

(define_insn "fix_truncsfsi2"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r")
      (fix:SI
	(match_operand:SF 1 "register_operand" "r")
      )
    )
  ]
  ""
  "ftrunc %0, %1 ; sasl #0"
  [(set_attr "length" "4")]
)

(define_insn "fixuns_truncsfsi2"
  [
    (set
      (match_operand:SI 0 "register_operand" "=r")
      (unsigned_fix:SI
	(match_operand:SF 1 "register_operand" "r")
      )
    )
  ]
  ""
  "ftrunc %0, %1; sasl #0"
  [(set_attr "length" "4")]
)

(define_insn "floatsisf2"
  [
    (set
      (match_operand:SF 0 "register_operand" "=r")
      (float:SF
	(match_operand:SI 1 "register_operand" "r")
      )
    )
  ]
  ""
  "flts %0, %1, sasr #0"
  [(set_attr "length" "4")]
)

(define_insn "floatunssisf2"
  [
    (set
      (match_operand:SF 0 "register_operand" "=r")
      (unsigned_float:SF
	(match_operand:SI 1 "register_operand" "r")
      )
    )
  ]
  ""
  "fltu %0, %1, sasr #0"
  [(set_attr "length" "4")]
)

;; --- Sign extension -------------------------------------------------------

(define_insn "zero_extendqisi2"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,r")
      (zero_extend:SI
        (match_operand:QI 1 "register_operand" "0,r")
      )
    )
  ]
  ""
  "@
  	bmask %0, #8
	bmask %0, %1, #8"
  [(set_attr "length" "2,4")]
)

(define_insn "zero_extendhisi2"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,r")
      (zero_extend:SI
        (match_operand:HI 1 "register_operand" "0,r")
      )
    )
  ]
  ""
  "@
  	bmask %0, #16
	bmask %0, %1, #16"
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
  	signext %0, #8
	signext %0, %1, #8"
  [(set_attr "length" "2,4")]
)

(define_insn "extendhisi2"
  [
    (set
      (match_operand:SI 0 "register_operand" "=f,r")
      (sign_extend:SI
        (match_operand:HI 1 "register_operand" "0,r")
      )
    )
  ]
  ""
  "@
  	signext %0, #16
	signext %0, %1, #16"
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

;; Call a function with no return value.

(define_expand "call"
  [
    (call
      (match_operand 0 "" "")
	  (match_operand 1 "" "")
	)
  ]
  ""
  ""
)

(define_insn "*vc4_simple_call"
  [
    (call
      (mem
	    (match_operand 0 "immediate_operand" "i")
      )
      (match_operand 1 "const_int_operand")
    )
  ]
  ""
  "bl %0"
)

(define_insn "*vc4_call_indirect"
  [(call (mem:HI (match_operand:SI 0 "register_operand" "r"))
	 (match_operand 1 "" ""))]
  ""
  "bl %0"
  [(set_attr "length" "2")])

;; Call a function *with* a return value.

(define_expand "call_value"
  [
	(set
	  (match_operand 0 "" "")
	  (call
	    (match_operand 1 "" "")
	    (match_operand 2 "" "")
	  )
	)
  ]
  ""
  ""
)

(define_insn "*vc4_value_call"
  [
	(set
	  (match_operand 0 "register_operand" "=r")
	  (call
        (mem
	      (match_operand 1 "immediate_operand" "i")
        )
	    (match_operand 2 "const_int_operand")
	  )
	)
  ]
  ""
  "bl %1"
)

(define_insn "*vc4_call_value_indirect"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:HI (match_operand:SI 1 "register_operand" "r"))
	      (match_operand 2 "" "")))]
  ""
  "bl %1"
  [(set_attr "length" "2")])


;; --- Conditionals ---------------------------------------------------------

(define_code_iterator condition
  [
    ne
    eq
    gt
    gtu
    lt
    ltu
    ge
    geu
    le
    leu
  ]
)

(define_code_attr condition_code
  [
    (ne "ne")
    (eq "eq")
    (gt "gt")
    (gtu "hi")
    (lt "lt")
    (ltu "lo")
    (ge "ge")
    (geu "hs")
    (le "le")
    (leu "ls")
  ]
)

;; Combined test-and-branch instructions.

(define_expand "cbranchsi4"
  [
    (set
      (reg:CC CC_REGNO)
      (compare:CC
	(match_operand 1)
	(match_operand 2)
      )
    )
    (set
      (pc)
      (if_then_else
        (match_operator 0 "comparison_operator"
          [
	    (reg:CC CC_REGNO)
	    (const_int 0)
          ]
        )
        (label_ref
          (match_operand 3 "" "")
        )
        (pc)
      )
    )
  ]
  ""
  {
    /* Ensure that comparisons against memory go via a temporary register. */
    if (GET_CODE(operands[1]) == MEM)
      operands[1] = force_reg(SImode, operands[1]);
    if (GET_CODE(operands[2]) == MEM)
      operands[2] = force_reg(SImode, operands[2]);
  }
)

(define_expand "cbranchsf4"
  [
    (set
      (reg:CC CC_REGNO)
      (compare:CC
	(match_operand:SF 1 "register_operand")
	(match_operand:SF 2 "register_operand")
      )
    )
    (set
      (pc)
      (if_then_else
        (match_operator 0 "comparison_operator"
          [
	    (reg:CC CC_REGNO)
	    (const_int 0)
          ]
        )
        (label_ref
          (match_operand 3 "" "")
        )
        (pc)
      )
    )
  ]
  ""
  {}
)

;; This is disabled for now because it doesn't understand limited offset range.

(define_insn "*vc4_test_and_branch_<condition:code>"
  [
    (set
      (pc)
      (if_then_else
        (condition
	  (match_operand:SI 0 "register_operand" "f,f")
	  (match_operand:SI 1 "nonmemory_operand" "f,K")
        )
        (label_ref
          (match_operand 2)
        )
        (pc)
      )
    )
  ]
  "0"
  "@
  	b<condition:condition_code> %0, %1, %2
  	b<condition:condition_code> %0, #%1, %2"
  [(set_attr "length" "4,4")]
)

;; Separated comparisons.

(define_insn "*vc4_test_si"
  [
    (set
      (reg:CC CC_REGNO)
      (compare:CC
        (match_operand:SI 0 "register_operand" "f,f,r,r,r")
        (match_operand:SI 1 "nonmemory_operand" "f,I,r,I,i")
      )
    )
  ]
  ""
  "@
  	cmp %0, %1 ; fast
	cmp %0, #%1 ; fast
	cmp %0, r0, %1
	cmp %0, r0, #%1 ; assembler workaround
	cmp %0, #%1 ; largeint"
  [(set_attr "length" "2,2,4,4,6")]
)

(define_insn "*vc4_test_sf"
  [
    (set
      (reg:CC CC_REGNO)
      (compare:CC
        (match_operand:SF 0 "register_operand" "r")
        (match_operand:SF 1 "register_operand" "r")
      )
    )
  ]
  ""
  "fcmp %0, %1, %1"
  [(set_attr "length" "4")]
)

(define_insn "*vc4_branch_<condition:code>"
  [
    (set
      (pc)
      (if_then_else
        (condition
	  (reg:CC CC_REGNO)
	  (const_int 0)
	)
	(label_ref
	  (match_operand 0)
	)
	(pc)
      )
    )
  ]
  ""
  "b<condition:condition_code> %0"
  [(set_attr "length" "4")]
)


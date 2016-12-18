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
(define_attr "enabled" "no,yes" (const_string "yes"))
(define_attr "predicable" "no,yes" (const_string "no"))

(include "predicates.md")
(include "constraints.md")

(define_constants [
  (CC_REGNO 34)
])

(define_mode_iterator QHSI [QI HI SI])
(define_mode_iterator QHI [QI HI])
(define_mode_iterator SIF [SI SF])
(define_mode_attr suffix [(QI "b") (HI "h") (SI "") (SF "")])

(define_c_enum "unspec" [
  UNSPEC_PRLG_STK
])

(define_c_enum "unspecv" [
  VUNSPEC_EH_RETURN
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

;; --- Generic moves --------------------------------------------------------

; NOTE: Conditional loads/stores throw undefined instruction exceptions on the
; hardware, so don't use those.

(define_insn "*mov<mode>_insn"
  [(set (match_operand:QHSI 0 "nonimmediate_operand"
	       "=f,  f,f,  r,  r,  r,  r,  r,  r,r,r, f, r, r,r, Us,Uc,Ud,Ul")
	(match_operand:QHSI 1 "general_operand"
	      "Iu5,Ku5,f,Is6,Js6,Ks6,IsX,JsX,KsX,i,r,Us,Uc,Ud,Ul, f, r, r, r"))]
  ""
  "@
  mov.s\t%0,#%1
  not.s\t%0,#%v1
  mov.s\t%0,%1
  mov%?.m\t%0,#%1
  neg%?.m\t%0,#%n1
  not%?.m\t%0,#%v1
  mov.m\t%0,#%1\t; non-predicable
  neg.m\t%0,#%n1\t; non-predicable
  not.m\t%0,#%v1\t; non-predicable
  mov.l\t%0,#%1
  mov%?.m\t%0,%1
  ld<suffix>.s\t%0,%1
  ld<suffix>.m\t%0,%1
  ld<suffix>.m\t%0,%1
  ld<suffix>.l\t%0,%1
  st<suffix>.s\t%1,%0
  st<suffix>.m\t%1,%0
  st<suffix>.m\t%1,%0
  st<suffix>.l\t%1,%0"
  [(set_attr "length" "2,2,2,4,4,4,4,4,4,6,4,2,4,4,6,2,4,4,6")
   (set_attr "predicable"
     "no,no,no,yes,yes,yes,no,no,no,no,yes,no,no,no,no,no,no,no,no")]
)

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

; Here:
;   LEA forms could be explicitly supported.

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"
					   "=f,  f,  f,r,  r,  r,  r,  r,r")
	(plus:SI (match_operand:SI 1 "register_operand"
					   "%0,  0,  0,r,  r,  r,  r,  0,r")
		 (match_operand:SI 2 "nonmemory_operand"
					    "f,IU5,Ju5,r,IS6,JS6,IsX,JsX,i")))]
  ""
  "@
  add.s\t%0,%2
  add.s\t%0,#%2
  sub.s\t%0,#%n2
  add%?.m\t%0,%1,%2
  add%?.m\t%0,%1,#%2
  sub%?.m\t%0,%1,#%n2
  add.m\t%0,%1,#%2
  sub.m\t%0,#%n2
  add.l\t%0,%1,#%2"
  [(set_attr "length" "2,2,2,4,4,4,4,4,6")
   (set_attr "predicable" "no,no,no,yes,yes,yes,no,no,no")]
)


(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand"            "=f,  f,r,  r")
	(ashift:SI (match_operand:SI 1 "register_operand"  "0,  0,r,  r")
		   (match_operand:SI 2 "nonmemory_operand" "f,Iu5,r,Iu5")))]
  ""
  "@
  shl.s\t%0,%2
  shl.s\t%0,#%2
  shl%?.m\t%0,%1,%2
  shl%?.m\t%0,%1,#%2"
  [(set_attr "length" "2,2,4,4")
   (set_attr "predicable" "no,no,yes,yes")]
)

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand"              "=f,  f,r,  r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand"  "0,  0,r,  r")
		     (match_operand:SI 2 "nonmemory_operand" "f,Iu5,r,Iu5")))]
  ""
  "@
  asr.s\t%0,%2
  asr.s\t%0,#%2
  asr%?.m\t%0,%1,%2
  asr%?.m\t%0,%1,#%2"
  [(set_attr "length" "2,2,4,4")
   (set_attr "predicable" "no,no,yes,yes")]
)

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand"              "=f,  f,r,  r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand"  "0,  0,r,  r")
		     (match_operand:SI 2 "nonmemory_operand" "f,Iu5,r,Iu5")))]
  ""
  "@
  lsr.s\t%0,%2
  lsr.s\t%0,#%2
  lsr%?.m\t%0,%1,%2
  lsr%?.m\t%0,%1,#%2"
  [(set_attr "length" "2,2,4,4")
   (set_attr "predicable" "no,no,yes,yes")]
)

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand"              "=f,r,  r")
	(rotatert:SI (match_operand:SI 1 "register_operand"  "0,r,  r")
		     (match_operand:SI 2 "nonmemory_operand" "f,r,Iu5")))]
  ""
  "@
  ror.s\t%0,%2
  ror%?.m\t%0,%1,%2
  ror%?.m\t%0,%1,#%2"
  [(set_attr "length" "2,4,4")
   (set_attr "predicable" "no,yes,yes")]
)

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand"          "=f,  f,r,  r,  r,r")
        (mult:SI (match_operand:SI 1 "register_operand" "%0,  0,r,  r,  0,0")
                 (match_operand:SI 2 "nonmemory_operand"
							 "f,Iu5,r,Is6,IsX,i")))]
  ""
  "@
  mul.s\t%0,%2
  mul.s\t%0,#%2
  mul%?.m\t%0,%1,%2
  mul%?.m\t%0,%1,#%2
  mul.m\t%0,#%2
  mul.l\t%0,#%2"
  [(set_attr "length" "2,2,4,4,4,6")
   (set_attr "predicable" "no,no,yes,yes,no,no")]
)

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand"      "=f,  f,r,  r,  r,  r,r")
        (xor:SI (match_operand:SI 1 "register_operand"
						    "%0,  0,r,  r,  r,  0,0")
                (match_operand:SI 2 "nonmemory_operand"
						     "f,Ip2,r,Ip2,Is6,IsX,i")))]
  ""
  "@
  eor.s\t%0,%2
  bitflip.s\t%0,#%p2
  eor%?.m\t%0,%1,%2
  bitflip%?.m\t%0,%1,#%p2
  eor%?.m\t%0,%1,#%2
  eor.m\t%0,#%2
  eor.l\t%0,#%2"
  [(set_attr "length" "2,2,4,4,4,4,6")
   (set_attr "predicable" "no,no,yes,yes,yes,no,no")]
)

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand"
				   "=f,  f,  f,r,  r,  r,  r,  r,  r,  r,r")
        (and:SI (match_operand:SI 1 "register_operand"
				   "%0,  0,  0,r,  r,  r,  r,  r,  0,  0,0")
                (match_operand:SI 2 "nonmemory_operand"
				    "f,Kp2,Ims,r,Is6,Ks6,Kp2,Ims,IsX,KsX,i")))]
  ""
  "@
  and.s\t%0,%2
  bitclear.s\t%0,#%P2
  bmask.s\t%0,#%k2
  and%?.m\t%0,%1,%2
  and%?.m\t%0,%1,#%2
  bic%?.m\t%0,%1,#%v2
  bitclear%?.m\t%0,%1,#%P2
  bmask%?.m\t%0,%1,#%k2
  and.m\t%0,#%2
  bic.m\t%0,#%v2
  and.l\t%0,#%2"
  [(set_attr "length" "2,2,2,4,4,4,4,4,4,4,6")
   (set_attr "predicable" "no,no,no,yes,yes,yes,yes,yes,no,no,no")]
)

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand"      "=f,  f,r,  r,  r,  r,r")
        (ior:SI (match_operand:SI 1 "register_operand"
						    "%0,  0,r,  r,  r,  0,0")
                (match_operand:SI 2 "nonmemory_operand"
						     "f,Ip2,r,Is6,Ip2,IsX,i")))]
  ""
  "@
  or.s\t%0,%2
  bitset.s\t%0,#%p2
  or%?.m\t%0,%1,%2
  or%?.m\t%0,%1,#%2
  bitset%?.m\t%0,%1,#%p2
  or.m\t%0,#%2
  or.l\t%0,#%2"
  [(set_attr "length" "2,2,4,4,4,4,6")
   (set_attr "predicable" "no,no,yes,yes,yes,no,no")]
)

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand"        "=f,r")
	(not:SI (match_operand:SI 1 "register_operand" "f,r")))]
  ""
  "@
  not.s\t%0,%1
  not%?.m\t%0,%1"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand"   "=f,  f,f,  r,r,r")
	(minus:SI
	  (match_operand:SI 1 "nonmemory_operand" "0,I00,f,Is6,i,r")
          (match_operand:SI 2 "register_operand"  "f,  f,0,  r,0,r")))]
  ""
  "@
  sub.s\t%0,%2
  neg.s\t%0,%2
  rsub.s\t%0,%1
  rsub%?.m\t%0,%2,#%1
  rsub.l\t%0,#%1
  sub%?.m\t%0,%1,%2"
  [(set_attr "length" "2,2,2,4,6,4")
   (set_attr "predicable" "no,no,no,yes,no,yes")]
)

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,  r")
	(div:SI (match_operand:SI 1 "register_operand"  "r,  r")
        	(match_operand:SI 2 "nonmemory_operand" "r,Is6")))]
  ""
  "@
  div%?.ss\t%0,%1,%2
  div%?.ss\t%0,%1,#%2"
  [(set_attr "length" "4,4")
   (set_attr "predicable" "yes")]
)

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r,  r")
	(udiv:SI (match_operand:SI 1 "register_operand"  "r,  r")
        	 (match_operand:SI 2 "nonmemory_operand" "r,Is6")))]
  ""
  "@
  div%?.uu\t%0,%1,%2
  div%?.uu\t%0,%1,#%2"
  [(set_attr "length" "4,4")
   (set_attr "predicable" "yes")]
)

(define_insn "vc4_add_asl"
  [(set (match_operand:SI 0 "register_operand"			      "=f,r")
	(plus:SI (ashift:SI (match_operand:SI 2 "register_operand"     "f,r")
			    (match_operand:SI 3 "arith_shift_operand"
								     "I03,Ish"))
		 (match_operand:SI 1 "register_operand"		       "0,r")))]
  ""
  "@
  addscale.s\t%0,%2<<3
  addscale%?.m\t%0,%1,%2<<%3"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "vc4_sub_asl"
  [(set (match_operand:SI 0 "register_operand"			"=r")
	(minus:SI
	  (match_operand:SI 1 "register_operand"		 "r")
	  (ashift:SI (match_operand:SI 2 "register_operand"	 "r")
		     (match_operand:SI 3 "arith_shift_operand" "Ish"))))]
  ""
  "subscale%?.m\t%0,%1,%2<<%3"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
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
  [(set (match_operand:SF 0 "register_operand" "=r")
	(fpu_list_3op:SF
	  (match_operand:SF 1 "register_operand" "r")
	  (match_operand:SF 2 "register_operand" "r")))]
  ""
  "<fpu_list_3op:fpu_opcode>%?\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_insn "<fpu_list_2op:fpu_insn>"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(fpu_list_2op:SF (match_operand:SF 1 "register_operand" "r")))]
  ""
  "<fpu_list_2op:fpu_opcode>%?\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

;; Extra float operations, such as conversions.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (fix:SI (match_operand:SF 1 "register_operand" "r")))]
  ""
  "ftrunc%?\t%0,%1,sasl #0"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(float:SF (match_operand:SI 1 "register_operand" "r")))]
  ""
  "flts%?\t%0,%1,sasr #0"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_insn "floatunssisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(unsigned_float:SF (match_operand:SI 1 "register_operand" "r")))]
  ""
  "fltu%?\t%0,%1,sasr #0"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

;; --- Sign extension -------------------------------------------------------

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=f,r")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "0,r")))]
  ""
  "@
  bmask.s\t%0,#8
  bmask%?.m\t%0,%1,#8"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=f,r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "0,r")))]
  ""
  "@
  bmask.s\t%0,#16
  bmask%?.m\t%0,%1,#16"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand"  "=f,r")
        (sign_extend:SI
          (match_operand:QI 1 "register_operand" "0,r")))]
  ""
  "@
  signext.s\t%0,#7
  signext%?.m\t%0,%1,#7"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand"  "=f,r")
        (sign_extend:SI
          (match_operand:HI 1 "register_operand" "0,r")))]
  ""
  "@
  signext.s\t%0,#15
  signext%?.m\t%0,%1,#15"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

;; --- Jumps ----------------------------------------------------------------

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "b\t%0"
  [(set_attr "length" "2")]
)

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "b\t%l0"
  [(set_attr "length" "2")]
)

;; Call a function with no return value.

(define_expand "call"
  [(call (match_operand 0 "" "") (match_operand 1 "" ""))]
  ""
  ""
)

(define_insn "*vc4_simple_call"
  [(call (mem (match_operand 0 "immediate_operand" "i"))
         (match_operand 1 "const_int_operand"))]
  ""
  "bl\t%0"
)

(define_insn "*vc4_call_indirect"
  [(call (mem:HI (match_operand:SI 0 "register_operand" "r"))
	 (match_operand 1 "" ""))]
  ""
  "bl\t%0"
  [(set_attr "length" "2")])

;; Call a function *with* a return value.

(define_expand "call_value"
  [(set (match_operand 0 "" "")
	(call (match_operand 1 "" "")
	      (match_operand 2 "" "")))]
  ""
  ""
)

(define_insn "*vc4_value_call"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem (match_operand 1 "immediate_operand" "i"))
	      (match_operand 2 "const_int_operand")))]
  ""
  "bl\t%1"
)

(define_insn "*vc4_call_value_indirect"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:HI (match_operand:SI 1 "register_operand" "r"))
	      (match_operand 2 "" "")))]
  ""
  "bl\t%1"
  [(set_attr "length" "2")])

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
                    (const_int 0))
              (match_operand 1 "" "")
              (match_operand 2 "" "")])]
  ""
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  emit_insn (gen_blockage ());
  DONE;
})

(define_insn_and_split "eh_return"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")]
		    VUNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vc4_set_return_address (operands[0], operands[1]);
  DONE;
})

;; --- Conditionals ---------------------------------------------------------

;; Combined test-and-branch instructions.

(define_expand "cbranchsi4"
  [(set (reg:CC CC_REGNO) (compare:CC
	                    (match_operand:SI 1 "register_operand" "")
	                    (match_operand:SI 2 "nonmemory_operand" "")))
   (set (pc) (if_then_else
               (match_operator 0 "comparison_operator"
                 [(reg:CC CC_REGNO) (const_int 0)])
               (label_ref (match_operand 3 "" ""))
               (pc)))]
  ""
{
})

(define_expand "cbranchsf4"
  [(set (reg:CCFP CC_REGNO) (compare:CCFP
	                      (match_operand:SF 1 "register_operand" "")
	                      (match_operand:SF 2 "register_operand" "")))
   (set (pc) (if_then_else
               (match_operator 0 "ordered_comparison_operator"
                 [(reg:CCFP CC_REGNO) (const_int 0)])
               (label_ref (match_operand 3 "" ""))
               (pc)))]
  ""
{
})

;; This is disabled for now because it doesn't understand limited offset range.

;(define_insn "*vc4_test_and_branch_<condition:code>"
;  [(set (pc) (if_then_else
;               (condition
;	         (match_operand:SI 0 "register_operand" "f,f")
;	         (match_operand:SI 1 "nonmemory_operand" "f,K"))
;               (label_ref (match_operand 2))
;               (pc)))]
;  "0"
;  "@
;  b<condition:condition_code> %0, %1, %2
;  b<condition:condition_code> %0, #%1, %2"
;  [(set_attr "length" "4,4")]
;)

;; Separated comparisons.

(define_insn "*vc4_test_si"
  [(set (reg:CC CC_REGNO)
        (compare:CC (match_operand:SI 0 "register_operand"
							"f,  f,r,  r,  r,r")
                    (match_operand:SI 1 "nonmemory_operand"
							"f,Iu5,r,Is6,IsX,i")))]
  ""
  "@
  cmp.s\t%0,%1
  cmp.s\t%0,#%1
  cmp%?.m\t%0,%1
  cmp%?.m\t%0,#%1
  cmp.m\t%0,#%1
  cmp.l\t%0,#%1"
  [(set_attr "length" "2,2,4,4,4,6")
   (set_attr "predicable" "no,no,yes,yes,no,no")]
)

(define_insn "*vc4_test_sf"
  [(set (reg:CCFP CC_REGNO)
        (compare:CCFP (match_operand:SF 0 "register_operand" "r")
                      (match_operand:SF 1 "register_operand" "r")))]
  ""
  "fcmp%?\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_insn "*vc4_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "ordered_comparison_operator"
            [(match_operand 2 "cc_register" "") (const_int 0)])
          (label_ref (match_operand 0 "" ""))
          (pc)))]
  ""
  "b%c1\t%0"
  [(set_attr "length" "4")]
)

; General conditional execution.
(define_cond_exec
  [(match_operator 0 "ordered_comparison_operator"
    [(match_operand 1 "cc_register" "") (const_int 0)])]
  ""
  ""
)

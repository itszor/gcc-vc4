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

(define_attr "length" "" (const_int 10))
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
  UNSPEC_MSB
  UNSPEC_SMULSI_HIGHPART
  UNSPEC_UMULSI_HIGHPART
  UNSPEC_USMULSI_HIGHPART
  UNSPEC_FLOOR
  UNSPEC_CEIL
  UNSPEC_LOG2
  UNSPEC_EXP2
  UNSPEC_RSQRT
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

; Length 4 here is a worst case (popping LR).

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
  [(set_attr "length" "4")]
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
  [(set (match_operand:SI 0 "s_register_operand"
					   "=f,  f,  f,r,  r,  r,  r,  r,r")
	(plus:SI (match_operand:SI 1 "s_register_operand"
					   "%0,  0,  0,r,  r,  r,  r,  0,r")
		 (match_operand:SI 2 "alu_rhs_operand"
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
  [(set (match_operand:SI 0 "s_register_operand"           "=f,  f,r,  r")
	(ashift:SI (match_operand:SI 1 "s_register_operand" "0,  0,r,  r")
		   (match_operand:SI 2 "alu_rhs_operand"    "f,Iu5,r,Iu5")))]
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
  [(set (match_operand:SI 0 "s_register_operand"             "=f,  f,r,  r")
	(ashiftrt:SI (match_operand:SI 1 "s_register_operand" "0,  0,r,  r")
		     (match_operand:SI 2 "alu_rhs_operand"    "f,Iu5,r,Iu5")))]
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
  [(set (match_operand:SI 0 "s_register_operand"             "=f,  f,r,  r")
	(lshiftrt:SI (match_operand:SI 1 "s_register_operand" "0,  0,r,  r")
		     (match_operand:SI 2 "alu_rhs_operand"    "f,Iu5,r,Iu5")))]
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
  [(set (match_operand:SI 0 "s_register_operand"             "=f,r,  r")
	(rotatert:SI (match_operand:SI 1 "s_register_operand" "0,r,  r")
		     (match_operand:SI 2 "alu_rhs_operand"    "f,r,Iu5")))]
  ""
  "@
  ror.s\t%0,%2
  ror%?.m\t%0,%1,%2
  ror%?.m\t%0,%1,#%2"
  [(set_attr "length" "2,4,4")
   (set_attr "predicable" "no,yes,yes")]
)

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "s_register_operand"   "=f,  f,r,  r,  r,r")
        (mult:SI
	  (match_operand:SI 1 "s_register_operand" "%0,  0,r,  r,  0,0")
          (match_operand:SI 2 "alu_rhs_operand"	    "f,Iu5,r,Is6,IsX,i")))]
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
  [(set (match_operand:SI 0 "s_register_operand"   "=f,  f,r,  r,  r,  r,r")
        (xor:SI
	  (match_operand:SI 1 "s_register_operand" "%0,  0,r,  r,  r,  0,0")
          (match_operand:SI 2 "alu_rhs_operand"     "f,Ip2,r,Ip2,Is6,IsX,i")))]
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
  [(set (match_operand:SI 0 "s_register_operand"
				   "=f,  f,  f,r,  r,  r,  r,  r,  r,  r,r")
        (and:SI (match_operand:SI 1 "s_register_operand"
				   "%0,  0,  0,r,  r,  r,  r,  r,  0,  0,0")
                (match_operand:SI 2 "alu_rhs_operand"
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
  [(set (match_operand:SI 0 "s_register_operand"    "=f,  f,r,  r,  r,  r,r")
        (ior:SI (match_operand:SI 1 "s_register_operand"
						    "%0,  0,r,  r,  r,  0,0")
                (match_operand:SI 2 "alu_rhs_operand"
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

(define_insn "vc4_bitflip"
  [(set (match_operand:SI 0 "s_register_operand"		  "=f,r")
	(xor:SI
	  (ashift:SI
	    (const_int 1) (match_operand:SI 2 "s_register_operand" "f,r"))
	  (match_operand:SI 1 "s_register_operand"		   "0,r")))]
  ""
  "@
  bitflip.s\t%0,%2
  bitflip%?.m\t%0,%1,%2"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "vc4_bic"
  [(set (match_operand:SI 0 "s_register_operand"		"=f,r")
	(and:SI (not:SI (match_operand:SI 2 "s_register_operand" "f,r"))
		(match_operand:SI 1 "s_register_operand"	 "0,r")))]
  ""
  "@
  bic.s\t%0,%2
  bic%?.m\t%0,%1,%2"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "vc4_bitclear"
  [(set (match_operand:SI 0 "s_register_operand"		    "=f,r")
	(and:SI
	  (rotate:SI
	    (const_int -2) (match_operand:SI 2 "s_register_operand" "f,r"))
	  (match_operand:SI 1 "s_register_operand"		     "0,r")))]
  ""
  "@
  bitclear.s\t%0,%2
  bitclear%?.m\t%0,%1,%2"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "vc4_bitset"
  [(set (match_operand:SI 0 "s_register_operand"		  "=f,r")
	(ior:SI
	  (ashift:SI
	    (const_int 1) (match_operand:SI 2 "s_register_operand" "f,r"))
	  (match_operand:SI 1 "s_register_operand"		   "0,r")))]
  ""
  "@
  bitset.s\t%0,%2
  bitset%?.m\t%0,%1,%2"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "s_register_operand"        "=f,r")
	(not:SI (match_operand:SI 1 "s_register_operand" "f,r")))]
  ""
  "@
  not.s\t%0,%1
  not%?.m\t%0,%1"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "subsi3"
  [(set (match_operand:SI 0 "s_register_operand"  "=f,  f,f,  r,r,r")
	(minus:SI
	  (match_operand:SI 1 "alu_rhs_operand"    "0,I00,f,Is6,i,r")
          (match_operand:SI 2 "s_register_operand" "f,  f,0,  r,0,r")))]
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
  [(set (match_operand:SI 0 "s_register_operand"        "=r,  r")
	(div:SI (match_operand:SI 1 "s_register_operand" "r,  r")
        	(match_operand:SI 2 "alu_rhs_operand"    "r,Is6")))]
  ""
  "@
  div%?.ss\t%0,%1,%2
  div%?.ss\t%0,%1,#%2"
  [(set_attr "length" "4,4")
   (set_attr "predicable" "yes")]
)

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "s_register_operand"         "=r,  r")
	(udiv:SI (match_operand:SI 1 "s_register_operand" "r,  r")
        	 (match_operand:SI 2 "alu_rhs_operand"    "r,Is6")))]
  ""
  "@
  div%?.uu\t%0,%1,%2
  div%?.uu\t%0,%1,#%2"
  [(set_attr "length" "4,4")
   (set_attr "predicable" "yes")]
)

(define_insn "vc4_add_asl"
  [(set (match_operand:SI 0 "s_register_operand"		      "=f,r")
	(plus:SI (ashift:SI (match_operand:SI 2 "s_register_operand"   "f,r")
			    (match_operand:SI 3 "arith_shift_operand"
								     "I03,Ish"))
		 (match_operand:SI 1 "s_register_operand"	       "0,r")))]
  ""
  "@
  addscale.s\t%0,%2<<3
  addscale%?.m\t%0,%1,%2<<%3"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "vc4_sub_asl"
  [(set (match_operand:SI 0 "s_register_operand"			"=r")
	(minus:SI
	  (match_operand:SI 1 "s_register_operand"		 "r")
	  (ashift:SI (match_operand:SI 2 "s_register_operand"	 "r")
		     (match_operand:SI 3 "arith_shift_operand" "Ish"))))]
  ""
  "subscale%?.m\t%0,%1,%2<<%3"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_insn "sminsi3"
  [(set (match_operand:SI 0 "s_register_operand"          "=f,r,  r,  r,r")
	(smin:SI (match_operand:SI 1 "s_register_operand" "%0,r,  r,  0,r")
		 (match_operand:SI 2 "alu_rhs_operand"     "f,r,Is6,IsX,i")))]
  ""
  "@
  min.s\t%0,%2
  min%?.m\t%0,%1,%2
  min%?.m\t%0,%1,#%2
  min.m\t%0,#%2
  min.l\t%0,#%2"
  [(set_attr "length" "2,4,4,4,6")
   (set_attr "predicable" "no,yes,yes,no,no")]
)

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "s_register_operand"          "=f,r,  r,  r,r")
	(smax:SI (match_operand:SI 1 "s_register_operand" "%0,r,  r,  0,r")
		(match_operand:SI 2 "alu_rhs_operand"      "f,r,Is6,IsX,i")))]
  ""
  "@
  max.s\t%0,%2
  max%?.m\t%0,%1,%2
  max%?.m\t%0,%1,#%2
  max.m\t%0,#%2
  max.l\t%0,#%2"
  [(set_attr "length" "2,4,4,4,6")
   (set_attr "predicable" "no,yes,yes,no,no")]
)

(define_insn "popcountsi2"
  [(set (match_operand:SI 0 "s_register_operand"              "=r,  r")
	(popcount:SI (match_operand:SI 1 "s_register_operand"  "r,Is6")))]
  ""
  "@
  count%?\t%0,%1
  count%?\t%0,#%1"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_expand "clzsi2"
  [(set (match_operand:SI 0 "s_register_operand")
	(clz:SI (match_operand:SI 1 "alu_rhs_operand")))]
  ""
{
  rtx tmp = gen_reg_rtx (SImode);
  emit_insn (gen_vc4_msb (tmp, operands[1]));
  emit_insn (gen_subsi3 (operands[0], gen_int_mode (31, SImode), tmp));
  DONE;
})

(define_insn "vc4_msb"
  [(set (match_operand:SI 0 "s_register_operand"	 "=f,r,  r,  r,r")
	(unspec:SI [(match_operand:SI 1 "alu_rhs_operand" "f,r,Is6,IsX,i")]
		   UNSPEC_MSB))]
  ""
  "@
  msb.s\t%0,%1
  msb%?.m\t%0,%1
  msb%?.m\t%0,#%1
  msb.m\t%0,#%1
  msb.l\t%0,#%1"
  [(set_attr "length" "2,4,4,4,6")
   (set_attr "predicable" "no,yes,yes,no,no")]
)

(define_insn "abssi2"
  [(set (match_operand:SI 0 "s_register_operand"        "=f,r")
	(abs:SI (match_operand:SI 1 "s_register_operand" "f,r")))]
  ""
  "@
  abs.s\t%0,%1
  abs%?.m\t%0,%1"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn_and_split "vc4_signext"
  [(set (match_operand:SI 0 "s_register_operand"  "=f,  f,r,  r")
	(sign_extract:SI
	  (match_operand:SI 1 "s_register_operand" "0,  0,r,  r")
	  (match_operand:SI 2 "signext_operand"	   "f,Iu5,r,Iu5")
	  (const_int 0)))
   (clobber (match_scratch:SI 3 "=&r,X,&r,X"))]
  ""
  "@
  #
  signext.s\t%0,#%d2
  #
  signext%?.m\t%0,%1,#%d2"
  "s_register_operand (operands[2], SImode)"
  [(set (match_dup 3) (minus:SI (match_dup 2) (const_int 1)))
   (set (match_dup 0)
	(sign_extract:SI
	  (match_dup 1)
	  (plus:SI (match_dup 3) (const_int 1))
	  (const_int 0)))]
  ""
  [(set_attr "length" "6,2,8,4")
   (set_attr "predicable" "no,no,no,yes")]
)

(define_insn "vc4_signext_reg"
  [(set (match_operand:SI 0 "s_register_operand"	   "=f,r")
	(sign_extract:SI
	  (match_operand:SI 1 "s_register_operand"	    "0,r")
	  (plus:SI (match_operand:SI 2 "s_register_operand" "f,r")
		   (const_int 1))
	  (const_int 0)))]
  ""
  "@
  signext.s\t%0,%2
  signext%?.m\t%0,%1,%2"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "vc4_zeroext"
  [(set (match_operand:SI 0 "s_register_operand"  "=f,  f,r,  r")
	(zero_extract:SI
	  (match_operand:SI 1 "s_register_operand" "0,  0,r,  r")
	  (match_operand:SI 2 "btest_operand"      "f,Iu5,r,Iu5")
	  (const_int 0)))]
  ""
  "@
  bmask.s\t%0,%2
  bmask.s\t%0,#%2
  bmask%?.m\t%0,%1,%2
  bmask%?.m\t%0,%1,#%2"
  [(set_attr "length" "2,2,4,4")
   (set_attr "predicable" "no,no,yes,yes")]
)

(define_insn "smulsi3_highpart"
  [(set (match_operand:SI 0 "s_register_operand"	    "=r,  r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "r,  r")
		    (match_operand:SI 2 "alu_rhs_operand"    "r,Is6")]
		   UNSPEC_SMULSI_HIGHPART))]
  ""
  "@
  mulhd%?.ss\t%0,%1,%2
  mulhd%?.ss\t%0,%1,#%2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_insn "umulsi3_highpart"
  [(set (match_operand:SI 0 "s_register_operand"	    "=r,  r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "r,  r")
		    (match_operand:SI 2 "alu_rhs_operand"    "r,Is6")]
		   UNSPEC_UMULSI_HIGHPART))]
  ""
  "@
  mulhd%?.uu\t%0,%1,%2
  mulhd%?.uu\t%0,%1,#%2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_expand "mulsidi3"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(mult:DI
	  (sign_extend:DI (match_operand:SI 1 "alu_rhs_operand" ""))
	  (sign_extend:DI (match_operand:SI 2 "alu_rhs_operand" ""))))]
  ""
{
  rtx tmp = gen_reg_rtx (SImode);
  emit_insn (gen_mulsi3 (tmp, operands[1], operands[2]));
  emit_insn (gen_smulsi3_highpart (gen_highpart_mode (SImode, DImode,
	     operands[0]), operands[1], operands[2]));
  emit_move_insn (gen_lowpart (SImode, operands[0]), tmp);
  DONE;
})

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(mult:DI
	  (sign_extend:DI (match_operand:SI 1 "alu_rhs_operand" ""))
	  (sign_extend:DI (match_operand:SI 2 "alu_rhs_operand" ""))))]
  ""
{
  rtx tmp = gen_reg_rtx (SImode);
  emit_insn (gen_mulsi3 (tmp, operands[1], operands[2]));
  emit_insn (gen_umulsi3_highpart (gen_highpart_mode (SImode, DImode,
	     operands[0]), operands[1], operands[2]));
  emit_move_insn (gen_lowpart (SImode, operands[0]), tmp);
  DONE;
})

(define_expand "usmulsidi3"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(mult:DI
	  (zero_extend:DI (match_operand:SI 1 "alu_rhs_operand" ""))
	  (sign_extend:DI (match_operand:SI 2 "alu_rhs_operand" ""))))]
  ""
{
  rtx tmp = gen_reg_rtx (SImode);
  emit_insn (gen_mulsi3 (tmp, operands[1], operands[2]));
  emit_insn (gen_vc4_usmulsi3_highpart (gen_highpart_mode (SImode, DImode,
	     operands[0]), operands[1], operands[2]));
  emit_move_insn (gen_lowpart (SImode, operands[0]), tmp);
  DONE;
})

(define_insn "vc4_usmulsi3_highpart"
  [(set (match_operand:SI 0 "s_register_operand"	 "=r,  r,  r")
	(unspec:SI [(match_operand:SI 1 "alu_rhs_operand" "r,  r,Is6")
		    (match_operand:SI 2 "alu_rhs_operand" "r,Is6,  r")]
		   UNSPEC_USMULSI_HIGHPART))]
  ""
  "@
  mulhd%?.us\t%0,%1,%2
  mulhd%?.us\t%0,%1,#%2
  mulhd%?.su\t%0,%2,#%1"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

; I don't think it's actually possible to generate these saturating patterns
; (from C) at present.  We could add builtins.

(define_insn "ssaddsi3"
  [(set (match_operand:SI 0 "s_register_operand"	     "=r,  r")
	(ss_plus:SI (match_operand:SI 1 "s_register_operand" "%r,  r")
		    (match_operand:SI 2 "alu_rhs_operand"     "r,Is6")))]
  ""
  "@
  adds%?.m\t%0,%1,%2
  adds%?.m\t%0,%1,#%2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_insn "sssubsi3"
  [(set (match_operand:SI 0 "s_register_operand"	      "=r,  r")
	(ss_minus:SI (match_operand:SI 1 "s_register_operand"  "r,  r")
		     (match_operand:SI 2 "alu_rhs_operand"     "r,Is6")))]
  ""
  "@
  subs%?.m\t%0,%1,%2
  subs%?.m\t%0,%1,#%2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

;; --- Float arithmetic -----------------------------------------------------

;; These are mostly simple 2op and 3op instructions and can be generated
;; algorithmically.

(define_code_iterator fpu_list_3op
  [
    plus
    mult
    smin
    smax
  ]
)

;; Mappings from insn names to the RTL node that actually does it.

(define_code_attr fpu_insn
  [
    (plus "addsf3")
    (mult "mulsf3")
    (smin "sminsf3")
    (smax "smaxsf3")
  ]
)

;; Mappings from insn names to the VC4 opcode that implements it.

(define_code_attr fpu_opcode
  [
    (plus "fadd")
    (mult "fmul")
    (smin "fmin")
    (smax "fmax")
  ]
)

;; Expand all FPU instructions.

; NOTE: Floating-point ops don't appear to support predication, although the
; assembler encoding suggests that they would.

(define_insn "<fpu_list_3op:fpu_insn>"
  [(set (match_operand:SF 0 "s_register_operand"  "=r,r")
	(fpu_list_3op:SF
	  (match_operand:SF 1 "s_register_operand" "r,r")
	  (match_operand:SF 2 "float_rhs_operand"  "r,F")))]
  ""
  "@
  <fpu_list_3op:fpu_opcode>\t%0,%1,%2
  <fpu_list_3op:fpu_opcode>\t%0,%1,#%f2"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

; NOTE: The "fabs" instruction doesn't seem to do the expected thing, so fake
; this with an integer instruction instead.

(define_insn "abssf2"
  [(set (match_operand:SF 0 "s_register_operand"	"=f,r")
	(abs:SF (match_operand:SF 1 "s_register_operand" "0,r")))]
  ""
  "@
   bitclear.s\t%0,#31\t; fabs
   bitclear%?.m\t%0,%1,#31\t; fabs"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

(define_insn "subsf3"
  [(set (match_operand:SF 0 "s_register_operand"	 "=r,r,r")
	(minus:SF (match_operand:SF 1 "float_rhs_operand" "r,r,F")
		  (match_operand:SF 2 "float_rhs_operand" "r,F,r")))]
  ""
  "@
   fsub\t%0,%1,%2
   fsub\t%0,%1,#%f2
   frsub\t%0,%2,#%f1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

(define_insn "negsf2"
  [(set (match_operand:SF 0 "s_register_operand"	"=f,r")
	(neg:SF (match_operand:SF 1 "s_register_operand" "0,r")))]
  ""
  "@
  bitflip.s\t%0,#31\t; fneg
  bitflip%?.m\t%0,%1,#31\t; fneg"
  [(set_attr "length" "2,4")
   (set_attr "predicable" "no,yes")]
)

; NOTE: We don't know how accurate frcp is.  (Experimentally, not very accurate ; at all.  Maybe not even enough for -ffast-math, when we get to that.  Ahh,
; it's been pointed out that this is probably intended to be the first estimate
; for a Newton-Raphson iteration sequence, so we could expand that out).

(define_insn "divsf3"
  [(set (match_operand:SF 0 "s_register_operand"       "=r,r,  r")
	(div:SF (match_operand:SF 1 "float_div_lhs"	"r,r,G01")
		(match_operand:SF 2 "float_rhs_operand" "r,F,  r")))]
  ""
  "@
   fdiv\t%0,%1,%2
   fdiv\t%0,%1,#%f2
   frcp\t%0,%2"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")
   (set_attr "enabled" "yes,yes,no")]
)

(define_insn "vc4_nmulsf3"
  [(set (match_operand:SF 0 "s_register_operand"		 "=r,r")
	(mult:SF (neg:SF (match_operand:SF 1 "s_register_operand" "r,r"))
		 (match_operand:SF 2 "s_register_operand"	  "r,F")))]
  ""
  "@
  fnmul\t%0,%1,%2
  fnmul\t%0,%1,#%f2"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

(define_insn "floorsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=r,r")
	(unspec:SF [(match_operand:SF 1 "float_rhs_operand" "r,F")]
		   UNSPEC_FLOOR))]
  ""
  "@
   ffloor\t%0,%1
   ffloor\t%0,#%f1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

(define_insn "ceilsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=r,r")
	(unspec:SF [(match_operand:SF 1 "float_rhs_operand" "r,F")]
		   UNSPEC_CEIL))]
  ""
  "@
   fceil\t%0,%1
   fceil\t%0,#%f1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

(define_insn "log2sf2"
  [(set (match_operand:SF 0 "s_register_operand" "=r,r")
	(unspec:SF [(match_operand:SF 1 "float_rhs_operand" "r,F")]
		   UNSPEC_LOG2))]
  ""
  "@
   flog2\t%0,%1
   flog2\t%0,#%f1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

(define_insn "exp2sf2"
  [(set (match_operand:SF 0 "s_register_operand" "=r,r")
	(unspec:SF [(match_operand:SF 1 "float_rhs_operand" "r,F")]
		   UNSPEC_EXP2))]
  ""
  "@
   fexp2\t%0,%1
   fexp2\t%0,#%f1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

; NOTE: We don't know how accurate frsqrt is.  Don't use for now.  (This might
; want to be an open-coded Newton-Raphson series expansion, together with
; frcp.)

(define_insn "rsqrtsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=r,r")
	(unspec:SF [(match_operand:SF 1 "float_rhs_operand" "r,F")]
		   UNSPEC_RSQRT))]
  "0"
  "@
   frsqrt\t%0,%1
   frsqrt\t%0,#%f1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

;; Extra float operations, such as conversions.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (fix:SI (match_operand:SF 1 "s_register_operand" "r")))]
  ""
  "ftrunc%?\t%0,%1,sasl #0"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "s_register_operand" "=r")
	(float:SF (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "flts%?\t%0,%1,sasr #0"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

(define_insn "floatunssisf2"
  [(set (match_operand:SF 0 "s_register_operand" "=r")
	(unsigned_float:SF (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "fltu%?\t%0,%1,sasr #0"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

;; --- Sign extension -------------------------------------------------------

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand"	 "=f,r, f, r, r, r")
	(zero_extend:SI (match_operand:QI 1 "extend_operand"
							  "0,r,Us,Uc,Ud,Ul")))]
  ""
  "@
  bmask.s\t%0,#8
  bmask%?.m\t%0,%1,#8
  ldb.s\t%0,%1
  ldb.m\t%0,%1
  ldb.m\t%0,%1
  ldb.l\t%0,%1"
  [(set_attr "length" "2,4,2,4,4,6")
   (set_attr "predicable" "no,yes,no,no,no,no")]
)

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "s_register_operand"	 "=f,r, f, r, r, r")
	(zero_extend:SI (match_operand:HI 1 "extend_operand"
							  "0,r,Us,Uc,Ud,Ul")))]
  ""
  "@
  bmask.s\t%0,#16
  bmask%?.m\t%0,%1,#16
  ldh.s\t%0,%1
  ldh.m\t%0,%1
  ldh.m\t%0,%1
  ldh.l\t%0,%1"
  [(set_attr "length" "2,4,2,4,4,6")
   (set_attr "predicable" "no,yes,no,no,no,no")]
)

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "=f,r, f, r, r, r")
        (sign_extend:SI
          (match_operand:QI 1 "extend_operand"    "0,r,Us,Uc,Ud,Ul")))]
  ""
  "@
  signext.s\t%0,#7
  signext%?.m\t%0,%1,#7
  ldsb.s\t%0,%1
  ldsb.m\t%0,%1
  ldsb.m\t%0,%1
  ldsb.l\t%0,%1"
  [(set_attr "length" "2,4,2,4,4,6")
   (set_attr "predicable" "no,yes,no,no,no,no")]
)

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "s_register_operand" "=f,r, f, r, r, r")
        (sign_extend:SI
          (match_operand:HI 1 "extend_operand"    "0,r,Us,Uc,Ud,Ul")))]
  ""
  "@
  signext.s\t%0,#15
  signext%?.m\t%0,%1,#15
  ldsh.s\t%0,%1
  ldsh.m\t%0,%1
  ldsh.m\t%0,%1
  ldsh.l\t%0,%1"
  [(set_attr "length" "2,4,2,4,4,6")
   (set_attr "predicable" "no,yes,no,no,no,no")]
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
{
  switch (get_attr_length (insn))
    {
    case 2: return "b.s\t%l0";
    case 4: return "b.m\t%l0";
    case 6: return "b.l\t%l0";
    default: gcc_unreachable ();
    }
}
  [(set (attr "length")
	(if_then_else
	  (and (ge (minus (match_dup 0) (pc)) (const_int -128))
	       (lt (minus (match_dup 0) (pc)) (const_int 126)))
	  (const_int 2)
	  (if_then_else
	    (and (ge (minus (match_dup 0) (pc)) (const_int -8388608))
		 (lt (minus (match_dup 0) (pc)) (const_int 8388604)))
	    (const_int 4)
	    (const_int 6))))
   (set_attr "predicable" "no")]
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
  [(set_attr "length" "4")]
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
  [(set_attr "length" "4")]
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
}
  [(set_attr "length" "4")]
)

(define_expand "casesi"
  [(match_operand:SI 0 "s_register_operand" "") ; index
   (match_operand:SI 1 "const_int_operand" "")	; lower bound
   (match_operand:SI 2 "const_int_operand" "")	; total range
   (match_operand:SI 3 "" "")			; label preceding table
   (match_operand:SI 4 "" "")]			; out of range label
  ""
{
  if (operands[1] != const0_rtx)
    {
      rtx reg = gen_reg_rtx (SImode);
      emit_insn (gen_addsi3 (reg, operands[0],
			     gen_int_mode (-INTVAL (operands[1]), SImode)));
      operands[0] = reg;
    }

  emit_jump_insn (gen_vc4_casesi (operands[0], operands[2], operands[3],
				  operands[4], gen_reg_rtx (SImode),
				  gen_reg_rtx (SImode)));
  DONE;
})

(define_insn "vc4_casesi"
  [(parallel [(set (pc)
	        (if_then_else
		  (leu (match_operand:SI 0 "s_register_operand" "r")
		       (match_operand:SI 1 "alu_rhs_operand" "i"))
		  (plus:SI (label_ref (match_operand 2 "" ""))
			   (mem:SI
			     (plus:SI (mult:SI (match_dup 0) (const_int 4))
			     (label_ref (match_dup 2)))))
		  (label_ref (match_operand 3 "" ""))))
	      (clobber (match_operand:SI 4 "s_register_operand" "=&r"))
	      (clobber (match_operand:SI 5 "s_register_operand" "=&r"))
	      (clobber (reg:CC CC_REGNO))
	      (use (label_ref (match_dup 2)))])]
  ""
{
  rtx diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[2])));

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  switch (GET_MODE (diff_vec))
    {
    case QImode:
    case HImode:
      asm_fprintf (asm_out_file, "\t; %s switch\n",
		   GET_MODE_NAME (GET_MODE (diff_vec)));
      return "cmp\t%0,#%1"		"\;"
	     "bhi\t%3"			"\;"
	     "switch\t%0";

    case SImode:
      /* FIXME: This can be slightly shorter for non-PIC.  */
      return "cmp\t%0,#%1"		"\;"
	     "bhi\t%3"			"\;"
	     "mov\t%4,#%2"		"\;"
	     "ld\t%5,(%4+%0<<2)"	"\;"
	     "add\t%5,%5,%4"		"\;"
	     "b\t%5";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "length" "26")
   (set_attr "predicable" "no")]
)

;; --- Conditionals ---------------------------------------------------------

;; Combined test-and-branch instructions.

;(define_expand "cbranchsi4"
;  [(set (reg:CC CC_REGNO) (compare:CC
;	                    (match_operand:SI 1 "register_operand" "")
;	                    (match_operand:SI 2 "nonmemory_operand" "")))
;   (set (pc) (if_then_else
;               (match_operator 0 "comparison_operator"
;                 [(reg:CC CC_REGNO) (const_int 0)])
;               (label_ref (match_operand 3 "" ""))
;               (pc)))]
;  ""
;{
;})

(define_expand "cbranch_cc"
  [(set (pc)
        (if_then_else
	  (match_operator 0 "" [(match_operand 1 "" "")
				(match_operand 2 "" "")])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))]
  ""
{
  machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[0]), operands[1],
				      operands[2]);
  rtx cc_reg = gen_rtx_REG (mode, CC_REGNO);

  emit_insn (gen_rtx_SET (cc_reg, gen_rtx_COMPARE (mode, operands[1],
						   operands[2])));

  operands[1] = cc_reg;
  operands[2] = const0_rtx;
})

(define_expand "cbranchsi4"
  [(set (pc) (if_then_else
	       (match_operator 0 "ordered_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "")
		  (match_operand:SI 2 "nonmemory_operand" "")])
	       (label_ref (match_operand 3 "" ""))
	       (pc)))]
  ""
{
  emit_jump_insn (gen_cbranch_cc (operands[0], operands[1], operands[2],
				  operands[3]));
  DONE;
})

;(define_expand "cbranchsf4"
;  [(set (reg:CCFP CC_REGNO) (compare:CCFP
;	                      (match_operand:SF 1 "register_operand" "")
;	                      (match_operand:SF 2 "register_operand" "")))
;   (set (pc) (if_then_else
;               (match_operator 0 "ordered_comparison_operator"
;                 [(reg:CCFP CC_REGNO) (const_int 0)])
;               (label_ref (match_operand 3 "" ""))
;               (pc)))]
;  ""
;{
;})

(define_expand "cbranchsf4"
  [(set (pc) (if_then_else
	       (match_operator 0 "ordered_comparison_operator"
		 [(match_operand:SF 1 "s_register_operand" "")
		  (match_operand:SF 2 "s_register_operand" "")])
	       (label_ref (match_operand 3 "" ""))
	       (pc)))]
  ""
{
  emit_jump_insn (gen_cbranch_cc (operands[0], operands[1], operands[2],
				  operands[3]));
  DONE;
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

(define_insn "*vc4_cmp"
  [(set (reg:CC CC_REGNO)
        (compare:CC (match_operand:SI 0 "s_register_operand"
							"f,  f,r,  r,  r,r")
                    (match_operand:SI 1 "alu_rhs_operand"
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

(define_insn "*vc4_cmn_c"
  [(set (reg:CC_C CC_REGNO)
	(compare:CC_C
	  (plus:SI
	    (match_operand:SI 0 "s_register_operand" "f,  f,r,  r,  r,r")
	    (match_operand:SI 1 "alu_rhs_operand"    "f,Iu5,r,Is6,IsX,i"))
	  (match_dup 0)))]
  ""
  "@
  cmn.s\t%0,%1
  cmn.s\t%0,#%1
  cmn%?.m\t%0,%1
  cmn%?.m\t%0,#%1
  cmn.m\t%0,#%1
  cmn.l\t%0,#%1"
  [(set_attr "length" "2,2,4,4,4,6")
   (set_attr "predicable" "no,no,yes,yes,no,no")]
)

(define_insn "*vc4_cmn_z"
  [(set (reg:CC_Z CC_REGNO)
	(compare:CC_Z
	  (neg:SI
	    (match_operand:SI 1 "alu_rhs_operand"  "f,Iu5,r,Is6,IsX,i"))
	  (match_operand:SI 0 "s_register_operand" "f,  f,r,  r,  r,r")))]
  ""
  "@
  cmn.s\t%0,%1
  cmn.s\t%0,#%1
  cmn%?.m\t%0,%1
  cmn%?.m\t%0,#%1
  cmn.m\t%0,#%1
  cmn.l\t%0,#%1"
  [(set_attr "length" "2,2,4,4,4,6")
   (set_attr "predicable" "no,no,yes,yes,no,no")]
)

(define_insn "*vc4_btest"
  [(set (reg:CC_Z CC_REGNO)
	(compare:CC_Z
	  (zero_extract:SI
	    (match_operand:SI 0 "s_register_operand"  "f,  f,  r,  r")
	    (const_int 1)
	    (match_operand:SI 1 "btest_operand"     "Iu5,  f,Iu5,  r"))
	  (const_int 0)))]
  ""
  "@
  btest.s\t%0,#%1
  btest.s\t%0,%1
  btest%?.m\t%0,#%1
  btest%?.m\t%0,%1"
  [(set_attr "length" "2,2,4,4")
   (set_attr "predicable" "no,no,yes,yes")]
)

(define_insn "*vc4_fcmp"
  [(set (reg:CCFP CC_REGNO)
        (compare:CCFP (match_operand:SF 0 "s_register_operand" "r,r")
                      (match_operand:SF 1 "float_rhs_operand"  "r,F")))]
  ""
  "@
  fcmp%?\t%0,%1
  fcmp%?\t%0,#%f1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")]
)

(define_insn "*vc4_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "ordered_comparison_operator"
            [(match_operand 2 "cc_register" "") (const_int 0)])
          (label_ref (match_operand 0 "" ""))
          (pc)))]
  ""
{
  if (get_attr_length (insn) == 2)
    return "b%c1.s\t%0";
  else
    return "b%c1.m\t%0";
}
  [(set (attr "length")
	(if_then_else
	  (and (ge (minus (match_dup 0) (pc)) (const_int -128))
	       (lt (minus (match_dup 0) (pc)) (const_int 126)))
	  (const_int 2)
	  (const_int 4)))
   (set_attr "predicable" "no")]
)

(define_insn "*vc4_cmpbranch"
  [(set (pc)
        (if_then_else
	  (match_operator 0 "ordered_comparison_operator"
	    [(match_operand:SI 1 "s_register_operand" "f,  f,r,  r")
	     (match_operand:SI 2 "cmpbranch_operand"  "f,Iu6,r,Iu6")])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))
   (clobber (reg:CC CC_REGNO))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      if (get_attr_length (insn) == 4)
        return "b%c0\t%1,%2,%3";
      else
	return "cmp.s\t%1,%2\;b%c0.m\t%3";

    case 1:
      if (get_attr_length (insn) == 4)
        return "b%c0\t%1,#%2,%3";
      else
	return "cmp\t%1,#%2\;b%c0.m\t%3";

    case 2:
      return "cmp.m\t%1,%2\;b%c0.m\t%3";

    case 3:
      return "cmp.m\t%1,#%2\;b%c0.m\t%3";

    default:
      gcc_unreachable ();
    }
}
  [(set (attr "length")
        (cond
	  [(match_test "which_alternative == 0")
	   (if_then_else
	     (and (ge (minus (match_dup 3) (pc)) (const_int -1024))
		  (lt (minus (match_dup 3) (pc)) (const_int 1020)))
	     (const_int 4)
	     (const_int 6))

	   (match_test "which_alternative == 1")
	   (if_then_else
	     (and (ge (minus (match_dup 3) (pc)) (const_int -256))
		  (lt (minus (match_dup 3) (pc)) (const_int 252)))
	     (const_int 4)
	     (const_int 8))]
	  (const_int 8)))
   (set_attr "predicable" "no")]
)

(define_peephole2
  [(set (match_operand:SI 0 "low_register_operand" "")
	(plus:SI (match_dup 0) (match_operand:SI 1 "addcmpbranch_operand" "")))
   (parallel
     [(set (pc)
	(if_then_else
	  (match_operator 2 "ordered_comparison_operator"
	    [(match_operand:SI 3 "low_register_operand")
	     (match_operand:SI 4 "low_cmpbranch_operand")])
	  (label_ref (match_operand 5 "" ""))
	  (pc)))
      (clobber (reg:CC CC_REGNO))])]
  "!rtx_equal_p (operands[3], operands[4])
   && (rtx_equal_p (operands[0], operands[3])
       || rtx_equal_p (operands[0], operands[4]))"
  [(parallel
     [(set (pc)
	   (if_then_else
	     (match_op_dup 2
	       [(match_dup 3) (match_dup 4)])
	     (label_ref (match_dup 5))
	     (pc)))
      (set (match_dup 0)
	   (plus:SI (match_dup 0) (match_dup 1)))
      (clobber (reg:CC CC_REGNO))])]
{
  if (rtx_equal_p (operands[0], operands[3]))
    operands[3] = gen_rtx_PLUS (SImode, operands[0], operands[1]);
  else if (rtx_equal_p (operands[0], operands[4]))
    {
      operands[4] = gen_rtx_PLUS (SImode, operands[0], operands[1]);
      operands[2] =
	gen_rtx_fmt_ee (swap_condition (GET_CODE (operands[2])), VOIDmode,
			operands[3], operands[4]);
      std::swap (operands[3], operands[4]);
    }
  else
    gcc_unreachable ();
})

(define_insn "*vc4_addcmpbranch"
  [(set (pc)
	(if_then_else
	  (match_operator 3 "ordered_comparison_operator"
	    [(plus:SI (match_dup 1) (match_dup 2))
	     (match_operand:SI 4 "low_cmpbranch_operand"    "f,Iu6,  f,Iu6")])
	  (label_ref (match_operand 5 "" ""))
	  (pc)))
   (set (match_operand:SI 0 "low_register_operand"	   "=f,  f,  f,  f")
	(plus:SI (match_operand:SI 1 "low_register_operand"
							   "%0,  0,  0,  0")
	         (match_operand:SI 2 "addcmpbranch_operand" "f,  f,Is4,Is4")))
   (clobber (reg:CC CC_REGNO))]
  "reload_completed"
{
  switch (which_alternative)
    {
    case 0:
      if (get_attr_length (insn) == 4)
	return "addcmpb%c3\t%0,%2,%4,%5";
      else
	return "add.s\t%0,%2\;cmp.s\t%0,%4\;b%c3.m\t%5";

    case 1:
      if (get_attr_length (insn) == 4)
	return "addcmpb%c3\t%0,%2,#%4,%5";
      else
	return "add.s\t%0,%2\;cmp\t%0,#%4\;b%c3.m\t%5";

    case 2:
      if (get_attr_length (insn) == 4)
	return "addcmpb%c3\t%0,#%2,%4,%5";
      else
	{
	  HOST_WIDE_INT diff = INTVAL (operands[2]);
	  if (diff >= 0)
	    return "add.s\t%0,#%2\;cmp.s\t%0,%4\;b%c3.m\t%5";
	  else
	    {
	      operands[2] = GEN_INT (-diff);
	      return "sub.s\t%0,#%2\;cmp.s\t%0,%4\;b%c3.m\t%5";
	    }
	}

    case 3:
      if (get_attr_length (insn) == 4)
	return "addcmpb%c3\t%0,#%2,#%4,%5";
      else
	{
	  HOST_WIDE_INT diff = INTVAL (operands[2]);
	  if (diff >= 0)
	    return "add.s\t%0,#%2\;cmp\t%0,#%4\;b%c3.m\t%5";
	  else
	    {
	      operands[2] = GEN_INT (-diff);
	      return "sub.s\t%0,#%2\;cmp\t%0,#%4\;b%c3.m\t%5";
	    }
	}

    default:
      gcc_unreachable ();
    }
}
  [(set (attr "length")
	(cond
	  [(match_test "which_alternative == 0 || which_alternative == 2")
	   (if_then_else
	     (and (ge (minus (match_dup 5) (pc)) (const_int -1024))
		  (lt (minus (match_dup 5) (pc)) (const_int 1020)))
	     (const_int 4)
	     (const_int 8))

	   (match_test "which_alternative == 1 || which_alternative == 3")
	   (if_then_else
	     (and (ge (minus (match_dup 5) (pc)) (const_int -256))
		  (lt (minus (match_dup 5) (pc)) (const_int 252)))
	     (const_int 4)
	     (const_int 10))]
	  (const_int 12)))
   (set_attr "predicable" "no")]
)

; General conditional execution.
(define_cond_exec
  [(match_operator 0 "ordered_comparison_operator"
    [(match_operand 1 "cc_register" "") (const_int 0)])]
  ""
  ""
)

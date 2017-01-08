/* vim: set ts=4 sw=4:
 *
 * Definitions of target machine for GNU compiler,
 * for Broadcom VideoCore IV processor.
 * Copyright (C) 1993-2013 Free Software Foundation, Inc.
 *
 * This file is part of GCC.
 *
 * GCC is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3, or (at your
 * option) any later version.
 *
 * GCC is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GCC; see the file COPYING3.  If not see
 * <http://www.gnu.org/licenses/>.
 */

#ifndef GCC_VC4_H
#define GCC_VC4_H

/* Run-time Target Specification.  */
#define TARGET_VC4

/* Get tree.c to declare a target-specific specialization of
   merge_decl_attributes.  */
#define TARGET_DLLIMPORT_DECL_ATTRIBUTES 1

#define TARGET_CPU_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_define ("__vc4__");			\
      builtin_define ("__VC4__");			\
      if (TARGET_SINGLE_FLOAT)				\
        builtin_define ("__VC4_SINGLE_FLOAT__");	\
    }							\
  while (0)

/* We don't have a -lg library, so don't put it in the list.  */
/*#undef	LIB_SPEC
#define LIB_SPEC "%{!shared: %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"*/

/* Target machine storage Layout.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  	\
  if (GET_MODE_CLASS (MODE) == MODE_INT         \
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    {						\
      (MODE) = SImode;				\
      /*(UNSIGNEDP) = 1;*/			\
    }

/* Endianness configuration. */
#define TARGET_LITTLE_END 1

#define BITS_BIG_ENDIAN  (! TARGET_LITTLE_END)
#define BYTES_BIG_ENDIAN (! TARGET_LITTLE_END)
#define WORDS_BIG_ENDIAN (! TARGET_LITTLE_END)

#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* The size of various important data types (in bits).  */
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE (TARGET_SINGLE_FLOAT ? 32 : 64)
#define LONG_DOUBLE_TYPE_SIZE DOUBLE_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE 64

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Every structures size must be a multiple of 8 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use 
   that to impose alignment on the enclosing structure.
   struct s {int a:8}; should have same alignment as "int", not "char".  */
#define	PCC_BITFIELD_TYPE_MATTERS 1

/* Largest integer machine mode for structures.  If undefined, the default
   is GET_MODE_SIZE(DImode).  */
#define MAX_FIXED_MODE_SIZE 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN) \
  ((TREE_CODE (EXP) == STRING_CST \
    && (ALIGN) < FASTEST_ALIGNMENT) \
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN) \
  (TREE_CODE (TYPE) == ARRAY_TYPE \
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode \
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Standard register usage.
 *
 * Our registers are:
 *
 * r0-r5     fast GPR, caller-saved
 * r6-r15    fast GPR
 * r16-r23   GPR
 * r24       gp
 * r25       sp
 * r26       lr
 * r27-r31   system
 * ?ap       virtual argument pointer
 * ?fp       virtual frame pointer
 * ?cc       virtual condition codes
 *
 * In emergencies, r6 is used as the frame pointer. r0-r5 are used for
 * passing parameters; r0 and r1 are used for returning parameters.
 *
 * In addition:
 * - r0, r6, r16 can be used in fast push/pop instructions.
 *
 */

enum {
    R0_REG, R1_REG, R2_REG, R3_REG, R4_REG, R5_REG, R6_REG, R7_REG,
    R8_REG, R9_REG, R10_REG, R11_REG, R12_REG, R13_REG, R14_REG, R15_REG,
    R16_REG, R17_REG, R18_REG, R19_REG, R20_REG, R21_REG, R22_REG, R23_REG,
    GP_REG, /* 24 */
    SP_REG, /* 25 */
    LR_REG, /* 26 */
    R27_REG, /* 27 */
    R28_REG, /* 28 */
    R29_REG, /* 29 */
    R30_REG, /* 30 */
    R31_REG, /* 31 */
    PC_REG = 31, /* The PC is not exposed, but this is useful in pop insns.  */
    AP_REG = 32, /* 32 */
    FP_REG, /* 33 */
    CC_REG /* 34 */
};

/* FIRST_PSEUDO_REGISTER needs to be defined as a preprocessor macro
   not part of an enum, as the preprocessor needs to make a decission
   based on this value in gcc/hard-reg-set.h */
#define FIRST_PSEUDO_REGISTER 35

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */


#define PC_REGNUM 31 /* This machine has no user-accessible program counter. */
#define STACK_POINTER_REGNUM 25 /* SP_REG */
#define FRAME_POINTER_REGNUM 33 /* FP_REG */ /* virtual frame pointer */
#define HARD_FRAME_POINTER_REGNUM 6 /* R6_REG */ /* real frame pointer */
#define ARG_POINTER_REGNUM 32 /* AP_REG */ /* virtual argument pointer */
#define CC_REGNUM 34

/* The assembler's names for the registers.  RFP need not always be used as
   the Real framepointer; it can also be used as a normal general register.
   Note that the name `fp' is horribly misleading since `fp' is in fact only
   the argument-and-return-context pointer.  */
#define REGISTER_NAMES \
{ \
  "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7", \
  "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15", \
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", \
  "gp",  "sp",  "lr",  "r27", "r28", "r29", "r30", "pc", \
  "?ap", "?fp", "?cc" \
}

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */
#define FIXED_REGISTERS \
{									\
/* r0  r1  r2  r3  r4  r5  r6  r7  r8  r9  r10 r11 r12 r13 r14 r15 */	\
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,	\
/* r16 r17 r18 r19 r20 r21 r22 r23 gp  sp  lr  r27 r28 r29 r30 r31 */	\
   0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  0,  1,  1,  1,  1,  1,	\
/* ?ap ?fp ?cc */							\
   1,  1,  1								\
}


/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

/* RBE: r15 {link register} not available across calls,
   But we don't mark it that way here....  */
#define CALL_USED_REGISTERS \
{									\
/* r0  r1  r2  r3  r4  r5  r6  r7  r8  r9  r10 r11 r12 r13 r14 r15 */	\
   1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,	\
/* r16 r17 r18 r19 r20 r21 r22 r23 gp  sp  lr  r27 r28 r29 r30 r31 */	\
   0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  1,  1,  1,	\
/* ?ap ?fp ?cc */							\
   1,  1,  1								\
}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the VC4 regs are UNITS_PER_WORD bits wide. */
#define HARD_REGNO_NREGS(REGNO, MODE)  \
   (((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE. */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  vc4_hard_regno_mode_ok (REGNO, MODE)

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '?')

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* Definitions for register eliminations.

   We have two registers that can be eliminated on the VC4.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.  */


/* Register in which the static-chain is passed to a function.
   FIXME: Chosen at random.  */
#define STATIC_CHAIN_REGNUM	R23_REG

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */
#define ELIMINABLE_REGS	\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM }, \
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM }, \
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM }, \
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM }, \
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM }}

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = vc4_initial_elimination_offset ((FROM), (TO))

#define HARD_FRAME_POINTER_IS_FRAME_POINTER 0
#define HARD_FRAME_POINTER_IS_ARG_POINTER 0

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

/* The VideoCore has several interesting register classes:
 *
 *  - fast registers, r0-r15; these get abbreviated instruction encodings.
 *  - stackable registers, r0, r6, r16, r24; these can be used with push/pop.
 *  - offsetable registers, r0, gp, sp; these have fast relative load/store encodings.
 */

enum reg_class {
    NO_REGS,
    FAST_REGS,
    GENERAL_REGS,
    SPECIAL_REGS,
    AFP_REG,
    SFP_REG,
    CC_REGS,
    ALL_REGS,

    LIM_REG_CLASSES,
    N_REG_CLASSES = LIM_REG_CLASSES
};


/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES		\
{				\
  "NO_REGS",			\
  "FAST_REGS",			\
  "GENERAL_REGS",		\
  "SPECIAL_REGS",		\
  "AFP_REG",			\
  "SFP_REG",			\
  "CC_REGS",			\
  "ALL_REGS",			\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS		\
{					\
  { 0x00000000, 0x0 },  /* NO_REGS */	\
  { 0x0000ffff, 0x0 },  /* FAST_REGS */ \
  { 0x07ffffff, 0x0 },  /* GENERAL_REGS */	\
  { 0xf8000000, 0x0 },  /* SPECIAL_REGS */	\
  { 0x08000000, 0x1 },  /* AFP_REG */	\
  { 0x10000000, 0x2 },  /* SFP_REG */	\
  { 0x20000000, 0x4 },  /* CC_REGS */	\
  { 0xffffffff, 0x4 }	/* ALL_REGS */  \
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern const enum reg_class vc4_regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) \
  ((REGNO) < FIRST_PSEUDO_REGISTER ? vc4_regno_reg_class[REGNO] : NO_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  GENERAL_REGS
#define BASE_REG_CLASS	 GENERAL_REGS

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS. 

   On the VC4 this is the size of MODE in words.  */
#define CLASS_MAX_NREGS(CLASS, MODE)  \
     (ROUND_ADVANCE (GET_MODE_SIZE (MODE)))

#define HARD_REGNO_RENAME_OK(SRC, DST) \
  df_regs_ever_live_p (DST)

/* Stack layout; function entry, exit and calling.  */

/* Define the number of register that can hold parameters.
   These two macros are used only in other macro definitions below.  */
#define NPARM_REGS 6
#define FIRST_PARM_REG 0
#define FIRST_RET_REG 0

#define INIT_EXPANDERS vc4_init_expanders ()

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 0

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* If defined, the maximum amount of space required for outgoing arguments
   will be computed and placed into the variable
   `crtl->outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL)  0

#define EH_RETURN_DATA_REGNO(N) (((N) < 2) ? (N) : INVALID_REGNUM)

#define VC4_EH_STACKADJ_REGNUM 2
#define EH_RETURN_STACKADJ_RTX gen_rtx_REG (SImode, VC4_EH_STACKADJ_REGNUM)

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE) \
	gen_rtx_REG (MODE, FIRST_RET_REG)

/* 1 if N is a possible register number for a function value.
   On the VC4, only r0 and r1 can return results.  */
#define FUNCTION_VALUE_REGNO_P(REGNO) \
	(((REGNO) == FIRST_RET_REG) || ((REGNO) == (FIRST_RET_REG+1)))

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(REGNO) \
  (IN_RANGE ((REGNO), FIRST_PARM_REG, (NPARM_REGS + FIRST_PARM_REG)))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the VC4, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus NARGREGS or more means all following args should go on the stack.  */
#define CUMULATIVE_ARGS  int

#define ROUND_ADVANCE(SIZE)	\
  ((SIZE + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* We have postincrement and predecrement, and want to use them. */

#define HAVE_POST_INCREMENT 1
#define HAVE_PRE_DECREMENT 1

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the VC4, the offset always starts at 0: the first parm reg is always
   the same reg.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  ((CUM) = 0)

/* Call the function profiler with a given profile label.  */
#define FUNCTION_PROFILER(STREAM,LABELNO)		\
{							\
  fprintf (STREAM, "	trap	1\n");			\
  fprintf (STREAM, "	.align	2\n");			\
  fprintf (STREAM, "	.long	LP%d\n", (LABELNO));	\
}

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 0

/* Length in units of the trampoline for entering a nested function (padded to
   L1 cache line size).  */
#define TRAMPOLINE_SIZE  32

/* Alignment required for a trampoline in bits (the size of an L1 icache
   line).  */
#define TRAMPOLINE_ALIGNMENT  (8 * 32)

/* Macros to check register numbers against specific register classes.  */

#ifdef REG_OK_STRICT

#define REGNO_OK_FOR_BASE_P(REGNO) \
  vc4_regno_ok_for_base_p ((REGNO), true)

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  vc4_regno_ok_for_base_p ((REGNO), true)

#else

#define REGNO_OK_FOR_BASE_P(REGNO) \
  vc4_regno_ok_for_base_p ((REGNO), false)

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  vc4_regno_ok_for_base_p ((REGNO), false)

#endif

/* Maximum number of registers that can appear in a valid memory 
   address.  */
#define MAX_REGS_PER_ADDRESS 2

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* 'char' is unsigned by default.  */
#define DEFAULT_SIGNED_CHAR  0

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  (((VALUE) = -1), 2)

/* This is suitable for bare metal, but might be better done as a SWI or
   something if running under some future hypothetical RTOS.  */

#define CLEAR_INSN_CACHE(BEG, END)                                         \
{                                                                          \
  int ints_enabled;                                                        \
  void *_begin = (BEG), *_end = (END) - 1;                                 \
  volatile int *const L1_IC0_CONTROL = (int*) 0x7ee02000;                  \
  volatile int *const L1_IC0_FLUSH_S = (int*) 0x7ee02008;                  \
  volatile int *const L1_IC0_FLUSH_E = (int*) 0x7ee0200c;                  \
  volatile int *const L1_IC1_CONTROL = (int*) 0x7ee02080;                  \
  volatile int *const L1_IC1_FLUSH_S = (int*) 0x7ee02088;                  \
  volatile int *const L1_IC1_FLUSH_E = (int*) 0x7ee0208c;                  \
  volatile int *const L1_D_CONTROL = (int*) 0x7ee02100;                    \
  volatile int *const L1_D_FLUSH_S = (int*) 0x7ee02104;                    \
  volatile int *const L1_D_FLUSH_E = (int*) 0x7ee02108;                    \
  __asm__ __volatile__ ("mov %0,sr\n\tdi" : "=r" (ints_enabled));          \
  *L1_D_FLUSH_S = (int) _begin;                                            \
  *L1_D_FLUSH_E = (int) _end;                                              \
  /* Flush dcache.  */                                                     \
  unsigned tmp = *L1_D_CONTROL;                                            \
  tmp |= 2; /* L1_D_CONTROL_DC0_FLUSH_SET.  */                             \
  tmp |= 4; /* L1_D_CONTROL_DC1_FLUSH_SET.  */                             \
  *L1_D_CONTROL = tmp;                                                     \
  while ((*L1_D_CONTROL) & 6)                                              \
    /* empty.  */;                                                         \
  *L1_IC0_FLUSH_S = (int) _begin;                                          \
  *L1_IC0_FLUSH_E = (int) _end;                                            \
  *L1_IC1_FLUSH_S = (int) _begin;                                          \
  *L1_IC1_FLUSH_E = (int) _end;                                            \
  /* Flush icache.  (Maybe we should figure out which core we are running  \
     on rather than just flushing both caches?).  */                       \
  unsigned tmp1, tmp2;                                                     \
  *L1_IC0_CONTROL |= 2; /* L1_IC0_CONTROL_START_FLUSH_SET.  */             \
  *L1_IC1_CONTROL |= 2; /* L1_IC1_CONTROL_START_FLUSH_SET.  */             \
  tmp1 = *L1_IC0_CONTROL;                                                  \
  tmp2 = *L1_IC1_CONTROL;                                                  \
  tmp1 += tmp2;                                                            \
  __asm__ __volatile__ ("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop"           \
                        : : "r" (tmp1));                                   \
  if (ints_enabled & (1 << 30))                                            \
    __asm__ __volatile__ ("ei");                                           \
}

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Byte accesses are not faster than word accesses.  */
#define SLOW_BYTE_ACCESS 1

/* Shift counts are truncated to 6-bits (0 to 63) instead of the expected
   5-bits, so we can not define SHIFT_COUNT_TRUNCATED to true for this
   target.  */
#define SHIFT_COUNT_TRUNCATED 1

/* All integers have the same format so truncation is easy.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC,INPREC)  1

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE 1

/* We don't know how expensive branches are.  */
#define BRANCH_COST(SPEED, PREDICTABLE) 2

#define MAX_CONDITIONAL_EXECUTE 3

/* The machine modes of pointers and functions.  */
#define Pmode          SImode
#define FUNCTION_MODE  HImode

#define JUMP_TABLES_IN_TEXT_SECTION 1

#define CASE_VECTOR_MODE SImode

#define CASE_VECTOR_PC_RELATIVE 1

#define CASE_VECTOR_SHORTEN_MODE(MIN, MAX, BODY)		\
  (((MIN) >= -256 && (MAX) < 256)				\
   ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 0, QImode)	\
   : ((MIN) >= -65536 && (MAX) < 65536)				\
   ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 0, HImode)	\
   : SImode)

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(STREAM, PREFIX, NUM, TABLE)    \
  do {							     \
    (*targetm.asm_out.internal_label) (STREAM, PREFIX, NUM); \
  } while (0)

#define ASM_OUTPUT_CASE_END(STREAM, NUM, TABLE)		\
  do {							\
    if (GET_MODE (PATTERN (TABLE)) != SImode)		\
      fprintf (STREAM, "\t.endswitch\n");		\
  } while (0)

/* Assembler output control.  */
#define ASM_COMMENT_START "\t;"

#define ASM_APP_ON	"; inline asm begin\n"
#define ASM_APP_OFF	"; inline asm end\n"

#define FILE_ASM_OP     "\t.file\n"

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  "\t.text"
#define DATA_SECTION_ASM_OP  "\t.data"

#define RETURN_ADDR_RTX(COUNT, FRAME) \
  ((COUNT) != 0 ? NULL_RTX : get_hard_reg_initial_val (Pmode, LR_REG))

#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, LR_REG)

#define SELECT_CC_MODE(OP, X, Y) vc4_select_cc_mode (OP, X, Y)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */
/*#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tsubi\t %s,%d\n\tstw\t %s,(%s)\n",	\
	   reg_names[STACK_POINTER_REGNUM],		\
	   (STACK_BOUNDARY / BITS_PER_UNIT),		\
	   reg_names[REGNO],				\
	   reg_names[STACK_POINTER_REGNUM])*/

/* Length in instructions of the code output by ASM_OUTPUT_REG_PUSH.  */
/*#define REG_PUSH_LENGTH 2*/

/* This is how to output an insn to pop a register from the stack.  */
/*#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tldw\t %s,(%s)\n\taddi\t %s,%d\n",	\
	   reg_names[REGNO],				\
	   reg_names[STACK_POINTER_REGNUM],		\
	   reg_names[STACK_POINTER_REGNUM],		\
	   (STACK_BOUNDARY / BITS_PER_UNIT))*/


/* Output a reference to a label.  */
#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME)  \
  fprintf (STREAM, "%s%s", USER_LABEL_PREFIX, \
	   (* targetm.strip_name_encoding) (NAME))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.p2align\t%d\n", LOG)

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

#define ASM_FPRINTF_EXTENSIONS(FILE, ARGS, P)			\
  case 'r':							\
    fprintf (FILE, "%s", reg_names [va_arg (ARGS, int)]);	\
    break;

#define SUPPORTS_ONE_ONLY 1
#define SUPPORTS_WEAK 1

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl\t"

/* The prefix to add to user-visible assembler symbols.  */
#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* Make an internal label into a string.  */
#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*.%s%ld", PREFIX, (long) NUM)

/* Output a relative address.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL) 	\
  do {								\
    switch (GET_MODE (BODY))					\
      {								\
      case QImode: case HImode:					\
	fprintf (STREAM, "\t.case .L%d-.L%d\n", VALUE, REL);	\
	break;							\
      case SImode:						\
	fprintf (STREAM, "\t.long .L%d-.L%d\n", VALUE, REL);	\
	break;							\
      default:							\
        gcc_unreachable ();					\
      }								\
  } while (0)

/* Output various types of constants.  */

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */
#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.fill %d, 1\n", (int)(SIZE))

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_absptr)

#endif                          /* ! GCC_VC4_H */

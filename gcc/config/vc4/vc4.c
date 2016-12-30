/* vim: set ts=4 sw=4:
 *
 * Output routines for Broadcom VideoCore IV processor
 * Copyright (C) 1993-2013 Free Software Foundation, Inc.
 * 
 * This file is part of GCC.
 * 
 * GCC is free software; you can redistribute it and/or modify it under the 
 * terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 3, or (at your option) any later
 * version.
 * 
 * GCC is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with GCC; see the file COPYING3.  If not see
 * <http://www.gnu.org/licenses/>.  
 */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "cfghooks.h"
#include "tree.h"
#include "rtl.h"
#include "df.h"
#include "alias.h"
#include "fold-const.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "regs.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "emit-rtl.h"
#include "stmt.h"
#include "expr.h"
#include "insn-codes.h"
#include "optabs.h"
#include "diagnostic-core.h"
#include "recog.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "cgraph.h"
#include "except.h"
#include "tm_p.h"
#include "target.h"
#include "sched-int.h"
#include "common/common-target.h"
#include "debug.h"
#include "langhooks.h"
#include "intl.h"
#include "libfuncs.h"
#include "params.h"
#include "opts.h"
#include "dumpfile.h"
#include "gimple-expr.h"
#include "target-globals.h"
#include "builtins.h"
#include "tm-constrs.h"
#include "rtl-iter.h"
#include "print-rtl.h"

/* This file should be included last.  */
#include "target-def.h"

/*
 * Global variables for machine-dependent things.  
 */

/*
 * Provides the class number of the smallest class containing reg number.
 * 
 */
const enum reg_class
vc4_regno_reg_class[FIRST_PSEUDO_REGISTER] =
{
  FAST_REGS, FAST_REGS, FAST_REGS, FAST_REGS, /* 00-03 */
  FAST_REGS, FAST_REGS, FAST_REGS, FAST_REGS, /* 04-07 */
  FAST_REGS, FAST_REGS, FAST_REGS, FAST_REGS, /* 08-11 */
  FAST_REGS, FAST_REGS, FAST_REGS, FAST_REGS, /* 12-15 */
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, /* 16-19 */
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, /* 20-23 */
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, SPECIAL_REGS, /* gp, sp, lr, 27 */
  SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS, /* 28-31 */
  AFP_REG, SFP_REG, CC_REGS			/* ?ap, ?fp, ?cc */
};

/* The stack frame layout we're going to use looks like follows.

  hi   ______________________
       |                    |
       |  incoming_params   |
       |____________________|  <-- old stack pointer
       |                    |
       |    pretend args    |
       |____________________|  <-- soft arg pointer
       |                    |
       |    callee_saves    |
       |____________________|
       |                    |
       |     local_vars     |
       | local_vars_padding |
       |____________________|  <-- hard+soft frame pointers
       |                    |
       |   (alloca space)   |
       |____________________|
       |                    |
       |  outgoing_params   |
  lo   |____________________|  <-- current stack pointer
 
 */
struct GTY (()) machine_function
{
  int callee_saves;
  int local_vars;
  int local_vars_padding;
  int pretend_size;
  int outgoing_args_size;

  /* Topmost register which needs to be saved (or 0 if none).  */
  int topreg;

  /* Does LR need to be saved?  */
  bool lrneedssaving;
  bool need_frame_pointer;
};

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
vc4_init_machine_status (void)
{
  struct machine_function *machine;
  machine = ggc_cleared_alloc<machine_function> ();
  return machine;
}

static struct machine_function *vc4_compute_frame(void);
static tree vc4_handle_naked_attribute(tree *, tree, tree, int, bool *);

/*
 * VC4 specific attributes.  
 */

static const struct attribute_spec vc4_attribute_table[] = {
    /*
     * { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
     * affects_type_identity } 
     */
    {"naked", 0, 0, true, false, false, vc4_handle_naked_attribute,
     false},
    {NULL, 0, 0, false, false, false, NULL, false}
};


/*
 * Compute cost of moving data between one register class and another.
 * Fast registers are cheap, everything else is expensive. 
 */
static int
vc4_target_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			       reg_class_t from, reg_class_t to)
{
  if ((from == FAST_REGS) && (to == from))
    return 2;

  return 4;
}

/*
 * Compute extra cost (over TARGET_REGISTER_MOVE_COST) to do memory
 * indirections. Slow registers are slow. 
 */
/*static int
vc4_target_memory_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			     reg_class_t rclass, bool in ATTRIBUTE_UNUSED)
{
  if (rclass == FAST_REGS)
    return 0;

  return 2;
}*/

/*
 * Compute extra cost (over TARGET_REGISTER_MOVE_COST) for an addressing
 * mode. On the VC4, all addressing modes cost the same. 
 */
/*static int
vc4_target_address_cost (rtx address ATTRIBUTE_UNUSED,
			 machine_mode mode ATTRIBUTE_UNUSED,
                         addr_space_t as ATTRIBUTE_UNUSED,
			 bool speed ATTRIBUTE_UNUSED)
{
  return 0;
}*/

static void
vc4_print_operand_address (FILE *stream, machine_mode /*mode*/, rtx x)
{
  switch (GET_CODE (x))
    {
    case REG:
      asm_fprintf (stream, "(%r)", REGNO (x));
      break;

    case PLUS:
      if (REG_P (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == CONST_INT)
	asm_fprintf (stream, "(%r+%d)", REGNO (XEXP (x, 0)),
                     (int) INTVAL (XEXP (x, 1)));
      else if (REG_P (XEXP (x, 0)) && REG_P (XEXP (x, 1)))
        asm_fprintf (stream, "(%r+%r)", REGNO (XEXP (x, 0)),
		     REGNO (XEXP (x, 1)));
      else if (GET_CODE (XEXP (x, 0)) == MULT
	       && REG_P (XEXP (XEXP (x, 0), 0))
	       && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	       && REG_P (XEXP (x, 1)))
	asm_fprintf (stream, "(%r+%r<<%d)", REGNO (XEXP (x, 1)),
		     REGNO (XEXP (XEXP (x, 0), 0)),
		     exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1))));
      else
        output_operand_lossage ("invalid PLUS operand");
      break;

    case POST_INC:
      if (REG_P (XEXP (x, 0)))
        asm_fprintf (stream, "(%r++)", REGNO (XEXP (x, 0)));
      else
        output_operand_lossage ("invalid POST_INC operand");
      break;

    case PRE_DEC:
      if (REG_P (XEXP (x, 0)))
        asm_fprintf (stream, "(--%r)", REGNO (XEXP (x, 0)));
      else
        output_operand_lossage ("invalid PRE_DEC operand");
      break;

    default:
      output_addr_const (stream, x);
    }
}

static void
vc4_print_cc_condition (FILE *stream, rtx_code code)
{
  switch (code)
    {
    case NE:
      fputs ("ne", stream);
      break;
    case EQ:
      fputs ("eq", stream);
      break;
    case GT:
      fputs ("gt", stream);
      break;
    case GE:
      fputs ("ge", stream);
      break;
    case LT:
      fputs ("lt", stream);
      break;
    case LE:
      fputs ("le", stream);
      break;
    case GTU:
      fputs ("hi", stream);
      break;
    case GEU:
      fputs ("hs", stream);
      break;
    case LTU:
      fputs ("lo", stream);
      break;
    case LEU:
      fputs ("ls", stream);
      break;
    default:
      gcc_unreachable ();
    }
}

static void
vc4_print_ccfp_condition (FILE *stream, rtx_code code)
{
  switch (code)
    {
    case NE:
      fputs ("ne", stream);
      break;
    case EQ:
      fputs ("eq", stream);
      break;
    case GT:
      fputs ("gt", stream);
      break;
    case GE:
      fputs ("ge", stream);
      break;
    case LT:
      fputs ("lt", stream);
      break;
    case LE:
      fputs ("le", stream);
      break;
    default:
      gcc_unreachable ();
    }
}

static void
vc4_print_ccz_condition (FILE *stream, rtx_code code)
{
  switch (code)
    {
    case NE:
      fputs ("ne", stream);
      break;
    case EQ:
      fputs ("eq", stream);
      break;
    default:
      gcc_unreachable ();
    }
}

static void
vc4_print_ccc_condition (FILE *stream, rtx_code code)
{
  switch (code)
    {
    case LTU:
      fputs ("cs", stream);
      break;
    case GEU:
      fputs ("cc", stream);
      break;
    default:
      gcc_unreachable ();
    }
}

static void
vc4_print_condition (FILE *stream, machine_mode mode, rtx_code code)
{
  switch (mode)
    {
    case CCmode:
    case SImode:
      vc4_print_cc_condition (stream, code);
      break;

    case CCFPmode:
      vc4_print_ccfp_condition (stream, code);
      break;

    case CC_Zmode:
      vc4_print_ccz_condition (stream, code);
      break;

    case CC_Cmode:
      vc4_print_ccc_condition (stream, code);
      break;

    default:
      gcc_unreachable ();
    }
}

/* Print operand x (an rtx) in assembler syntax to file stream according
   to modifier code.  */

static void
vc4_print_operand (FILE *stream, rtx x, int code)
{
  switch (code)
    {
      case 'C':
        fprintf (asm_out_file, "#" HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
        break;

      case 'c':
        vc4_print_condition (stream, GET_MODE (XEXP (x, 0)), GET_CODE (x));
        break;

      case 'v':
        gcc_assert (CONST_INT_P (x));
	asm_fprintf (stream, "%wd", ~INTVAL (x));
        break;

      case 'k':
        gcc_assert (CONST_INT_P (x));
	asm_fprintf (stream, "%d", exact_log2 ((INTVAL (x) & 0xffffffff) + 1));
	break;

      case 'p':
      case 'P':
        {
	  gcc_assert (CONST_INT_P (x));
	  HOST_WIDE_INT ival = INTVAL (x);

          if (code == 'P')
	    ival = ~ival;

	  asm_fprintf (stream, "%d", exact_log2 (ival & 0xffffffff));
        }
	break;

      case 'd':
	{
	  gcc_assert (CONST_INT_P (x));
	  asm_fprintf (stream, "%d", INTVAL (x) - 1);
	}
	break;

      case 'f':
	{
	  char fpstr[20];
	  real_to_decimal (fpstr, CONST_DOUBLE_REAL_VALUE (x), sizeof (fpstr),
			   0, 1);
	  fprintf (stream, "%s", fpstr);
	}
	break;

      case '?':
	if (current_insn_predicate)
	  {
	    fputc ('.', stream);
	    vc4_print_condition (stream,
				 GET_MODE (XEXP (current_insn_predicate, 0)),
				 GET_CODE (current_insn_predicate));
	  }
	break;

      default:
        switch (GET_CODE (x))
          {
          case REG:
            asm_fprintf (stream, "%r", REGNO (x));
            break;
          case MEM:
            vc4_print_operand_address (stream, GET_MODE (x), XEXP (x, 0));
            break;
          default:
            output_addr_const (stream, x);
            break;
          }
        break;
    }
}

static bool
vc4_target_rtx_costs (rtx x, machine_mode mode, int outer_code ATTRIBUTE_UNUSED,
		      int opno ATTRIBUTE_UNUSED, int *total,
		      bool speed ATTRIBUTE_UNUSED)
{
  bool value = false;

  switch (GET_CODE (x))
    {
      case MEM:
	{
          value = true;
          if (REG_P(XEXP(x, 0)))
            *total = COSTS_N_INSNS(2);
          else if (GET_CODE(XEXP(x, 0)) == PLUS)
            {
              rtx left = XEXP(XEXP(x, 0), 0);
              rtx right = XEXP(XEXP(x, 0), 1);
              /*
               * Test for ra[rb] 
               */
              if ((GET_CODE(left) == MULT)
                  && REG_P(XEXP(left, 0))
                  && CONST_INT_P(XEXP(left, 1))
                  && REG_P(right)
                  && (INTVAL(XEXP(left, 1)) == GET_MODE_SIZE(mode)))
        	*total = COSTS_N_INSNS(2);
              /* Test for ra[rb] where ra is a byte*.  */
              else if ((mode == QImode) && REG_P(left) && REG_P(right))
                *total = COSTS_N_INSNS(2);
              /* Test for ra[const].  */
              else if (REG_P(left) && CONST_INT_P(right))
                *total = COSTS_N_INSNS(2);
              else
                value = false;      /* don't know */
            }
          else
            value = false;
          break;
	}

      case CONST_INT:
      case CONST:
      case LABEL_REF:
      case SYMBOL_REF:
      case CONST_DOUBLE:
        *total = 0;
        value = true;
        break;

      case AND:
      case IOR:
        value = 1;
        break;

      case DIV:
      case UDIV:
      case MOD:
      case UMOD:
      case FLOAT:
      case FIX:
        *total = COSTS_N_INSNS(100);
        value = true;
        break;
      default:
        break;
    }

  return value;
}


/*
 * Code to generate prologue and epilogue sequences.  
 */
//static int number_of_regs_before_varargs;

/*
 * Set by TARGET_SETUP_INCOMING_VARARGS to indicate to prolog that this is
 * for a varargs function.  
 */
//static int current_function_anonymous_args;

/* Calculates the offset needed to convert accesses to the specified register
   to instead be an access to the stack pointer.  */

#if 0
static int
register_offset (struct machine_function *offsets, int reg)
{
  int offset = 0;

  switch (reg)
    {
    case STACK_POINTER_REGNUM:
      offset += offsets->outgoing_args_size;
      /* fall through */
    
    case HARD_FRAME_POINTER_REGNUM:
    case FRAME_POINTER_REGNUM:
      offset += offsets->callee_saves
		+ offsets->local_vars
		+ offsets->local_vars_padding;
      /* fall through */

    case ARG_POINTER_REGNUM:
      break;

    default:
      gcc_unreachable ();
    }

  return offset;
}
#endif

/* Implements the macro INITIAL_ELIMINATION_OFFSET. Returns the offset
   between the two specified registers.  */

int
vc4_initial_elimination_offset (int from, int to)
{
  struct machine_function *offsets = vc4_compute_frame ();
  /*int diff;

  diff = register_offset (offsets, to) - register_offset (offsets, from);*/

  /*fprintf (stderr, "eliminating %s to %s: diff = %d\n", reg_names[from],
	   reg_names[to], diff);*/

  if (from == ARG_POINTER_REGNUM)
    {
      switch (to)
	{
	case FRAME_POINTER_REGNUM:
	case HARD_FRAME_POINTER_REGNUM:
	  return offsets->callee_saves + offsets->local_vars
		 + offsets->local_vars_padding;

	case STACK_POINTER_REGNUM:
	  return offsets->callee_saves + offsets->local_vars
		 + offsets->local_vars_padding + offsets->outgoing_args_size;

	default:
	  gcc_unreachable ();
	}
    }
  else if (from == FRAME_POINTER_REGNUM)
    {
      switch (to)
	{
	case HARD_FRAME_POINTER_REGNUM:
	  return 0;

	case STACK_POINTER_REGNUM:
	  return offsets->outgoing_args_size;

	default:
	  gcc_unreachable ();
	}
    }
  else
    gcc_unreachable ();
}

bool
vc4_can_eliminate (const int from, const int to)
{
  if ((from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
      || (frame_pointer_needed && to == STACK_POINTER_REGNUM))
    return false;

  return true;
}

/* Compute the number of word sized registers needed to hold a function
   argument of mode MODE and type TYPE.  */

static int
num_arg_regs (machine_mode mode, const_tree type)
{
  int size;

  if (targetm.calls.must_pass_in_stack (mode, type))
    return 0;

  if (type && mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  return ROUND_ADVANCE (size);
}

/* Keep track of some information about varargs for the prolog.  */

static void
vc4_setup_incoming_varargs (cumulative_args_t args_so_far_v,
                            machine_mode, tree, int *ptr_pretend_size,
                            int second_time ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *args_so_far = get_cumulative_args (args_so_far_v);
  if (*args_so_far < 6)
    *ptr_pretend_size = (6 - *args_so_far) * UNITS_PER_WORD;
}

/*
 * Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue.  
 */

static struct machine_function *
vc4_compute_frame (void)
{
  /* For aligning the local variables.  */
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals;
  int regno;
  struct machine_function *offsets = cfun->machine;
  
  if (reload_completed)
    return offsets;

  offsets->need_frame_pointer = frame_pointer_needed;
  offsets->outgoing_args_size = crtl->outgoing_args_size;
  offsets->pretend_size = crtl->args.pretend_args_size;

  /* Padding needed for each element of the frame.  */
  offsets->local_vars = get_frame_size ();

  /* Align to the stack alignment.  */
  padding_locals = offsets->local_vars % stack_alignment;

  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  offsets->local_vars_padding = padding_locals;

  /* Save callee-saved registers.  */
  offsets->topreg = frame_pointer_needed ? HARD_FRAME_POINTER_REGNUM : 0;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (df_regs_ever_live_p (regno) && (!call_used_regs[regno]))
      offsets->topreg = regno;

  /* Check to see if lr needs saving. */
  offsets->lrneedssaving = !leaf_function_p ()
			   || df_regs_ever_live_p (LR_REG);

  offsets->callee_saves = (offsets->lrneedssaving ? 4 : 0);

  if (offsets->topreg > 0)
    offsets->callee_saves += (offsets->topreg - 5) * 4;
  
  return offsets;
}

static void
vc4_target_asm_function_prologue (FILE *file,
				  HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  struct machine_function *offsets = vc4_compute_frame ();

  fprintf (file, "\t; pretend size = %d bytes\n", offsets->pretend_size);
  fprintf (file, "\t; callee saves = %d bytes\n", offsets->callee_saves);
  fprintf (file, "\t; local vars = %d+%d bytes\n", offsets->local_vars,
	   offsets->local_vars_padding);
  fprintf (file, "\t; outgoing = %d bytes\n", offsets->outgoing_args_size);

  fprintf (file, "\t; needs frame pointer = %s\n",
	   offsets->need_frame_pointer ? "true" : "false");
  fprintf (file, "\t; topreg = %d\n", offsets->topreg);
  fprintf (file, "\t; lr needs saving = %s\n",
	   offsets->lrneedssaving ? "true" : "false");
}

static void
vc4_target_asm_function_epilogue (FILE *file ATTRIBUTE_UNUSED,
				  HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
}

bool
vc4_push_pop_operation_p (rtx op, bool is_push, bool returns)
{
  unsigned int par_len = XVECLEN (op, 0), i;
  rtx base_reg = NULL_RTX;
  bool have_return = false;

  for (i = 0; i < par_len; i++)
    {
      rtx set = XVECEXP (op, 0, i), reg, mem;
      
      if (!is_push && GET_CODE (set) == RETURN)
        {
          have_return = true;
	  continue;
	}

      if (GET_CODE (set) != SET)
        return false;

      if (is_push)
        {
	  reg = SET_SRC (set);
	  mem = SET_DEST (set);
	}
      else
        {
	  reg = SET_DEST (set);
	  mem = SET_SRC (set);
	}

      if (GET_CODE (SET_SRC (set)) == PLUS && REG_P (SET_DEST (set)))
        base_reg = SET_DEST (set);
      else if (MEM_P (mem) && REG_P (reg))
	;
      else
        return false;
    }

  if (base_reg == NULL_RTX)
    return false;

  if (have_return != returns)
    return false;

  return true;
}

const char *
vc4_emit_multi_reg_push (rtx par)
{
  unsigned int par_len = XVECLEN (par, 0), i;
  int base_reg = -1, lo_reg = -1, hi_reg = -1;
  bool lr_included = false;
  rtx operands[2];
  
  for (i = 0; i < par_len; i++)
    {
      rtx set = XVECEXP (par, 0, i);

      gcc_assert (GET_CODE (set) == SET);

      if (GET_CODE (SET_SRC (set)) == PLUS && REG_P (SET_DEST (set)))
	base_reg = REGNO (SET_DEST (set));
      else if (MEM_P (SET_DEST (set)) && REG_P (SET_SRC (set)))
	{
	  int pushed_reg = REGNO (SET_SRC (set));

	  if (pushed_reg == LR_REG)
	    lr_included = true;
	  else if (lo_reg == -1 || hi_reg == -1)
	    lo_reg = hi_reg = pushed_reg;
	  else
	    {
	      if (pushed_reg > hi_reg)
		hi_reg = pushed_reg;
	      if (pushed_reg < lo_reg)
	        lo_reg = pushed_reg;
	    }
	}
      else
	gcc_unreachable ();
    }

  gcc_assert (base_reg == SP_REG);
  gcc_assert (lo_reg == R0_REG || lo_reg == R6_REG || lo_reg == R16_REG
	      || lo_reg == GP_REG
	      || (lo_reg == -1 && hi_reg == -1 && lr_included));
  gcc_assert (hi_reg >= lo_reg
	      || (lo_reg == -1 && hi_reg == -1 && lr_included));

  if (lo_reg == hi_reg)
    {
      if (lo_reg == -1 && lr_included)
	return "stm\tlr,(--sp)";
      else
        {
	  operands[0] = gen_rtx_REG (SImode, lo_reg);
	  if (lr_included)
            output_asm_insn ("stm\t%0,lr,(--sp)", operands);
	  else
	    output_asm_insn ("stm\t%0,(--sp)", operands);
	  return "";
	}
    }
  else
    {
      operands[0] = gen_rtx_REG (SImode, lo_reg);
      operands[1] = gen_rtx_REG (SImode, hi_reg);
      if (lr_included)
        output_asm_insn ("stm\t%0-%1,lr,(--sp)", operands);
      else
	output_asm_insn ("stm\t%0-%1,(--sp)", operands);
      return "";
    }
}

const char *
vc4_emit_multi_reg_pop (rtx par)
{
  unsigned int par_len = XVECLEN (par, 0), i;
  int base_reg = -1, lo_reg = -1, hi_reg = -1;
  bool pc_included = false, have_return = false;
  rtx operands[2];
  
  for (i = 0; i < par_len; i++)
    {
      if (GET_CODE (XVECEXP (par, 0, i)) == RETURN)
        {
	  have_return = true;
          continue;
	}

      rtx set = XVECEXP (par, 0, i);

      gcc_assert (GET_CODE (set) == SET);

      if (GET_CODE (SET_SRC (set)) == PLUS && REG_P (SET_DEST (set)))
	base_reg = REGNO (SET_DEST (set));
      else if (REG_P (SET_DEST (set)) && MEM_P (SET_SRC (set)))
	{
	  int popped_reg = REGNO (SET_DEST (set));

	  if (popped_reg == PC_REG)
	    pc_included = true;
	  else if (lo_reg == -1 || hi_reg == -1)
	    lo_reg = hi_reg = popped_reg;
	  else
	    {
	      if (popped_reg > hi_reg)
		hi_reg = popped_reg;
	      if (popped_reg < lo_reg)
	        lo_reg = popped_reg;
	    }
	}
      else
	gcc_unreachable ();
    }

  gcc_assert (have_return == pc_included);
  gcc_assert (base_reg == SP_REG);
  /* Here, LR_REG is allowed as a pseudo-instruction to aid epilogue expansion
     for functions with pretend arguments.  It may be better to handle this
     elsewhere?  */
  gcc_assert (lo_reg == R0_REG || lo_reg == R6_REG || lo_reg == R16_REG
	      || lo_reg == GP_REG || lo_reg == LR_REG
	      || (lo_reg == -1 && hi_reg == -1 && pc_included));
  gcc_assert (hi_reg >= lo_reg
	      || (lo_reg == -1 && hi_reg == -1 && pc_included));

  if (lo_reg == hi_reg)
    {
      if (lo_reg == -1 && pc_included)
	return "ldm\tpc,(sp++)";
      else
        {
	  operands[0] = gen_rtx_REG (SImode, lo_reg);
	  if (pc_included)
            output_asm_insn ("ldm\t%0,pc,(sp++)", operands);
	  else if (lo_reg == LR_REG)
            output_asm_insn ("ld\t%0,(sp++)", operands);
          else
	    output_asm_insn ("ldm\t%0,(sp++)", operands);
	  return "";
	}
    }
  else
    {
      operands[0] = gen_rtx_REG (SImode, lo_reg);
      operands[1] = gen_rtx_REG (SImode, hi_reg);
      if (pc_included)
        output_asm_insn ("ldm\t%0-%1,pc,(sp++)", operands);
      else
	output_asm_insn ("ldm\t%0-%1,(sp++)", operands);
      return "";
    }
}

static rtx_insn *
vc4_emit_push (int lo_reg, int hi_reg, bool include_lr)
{
  int regs_to_push, i;
  rtx sp = gen_rtx_REG (SImode, SP_REG);
  
  if (lo_reg == -1 && hi_reg == -1)
    regs_to_push = 0;
  else
    regs_to_push = 1 + hi_reg - lo_reg;
  
  regs_to_push += (include_lr ? 1 : 0);

  gcc_assert (regs_to_push > 0);

  rtx par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (regs_to_push + 1));

  XVECEXP (par, 0, 0)
    = gen_rtx_SET (sp,
		   plus_constant (Pmode, sp, -regs_to_push * UNITS_PER_WORD));

  for (i = 0; i < regs_to_push; i++)
    {
      int regno = i + lo_reg;

      if (include_lr && i == regs_to_push - 1)
        regno = LR_REG;

      XVECEXP (par, 0, i + 1)
        = gen_rtx_SET (gen_rtx_MEM (SImode,
			 plus_constant (Pmode, sp,
					(i - regs_to_push) * UNITS_PER_WORD)),
		       gen_rtx_REG (SImode, regno));
    }

  return emit_insn (par);
}

static void
vc4_emit_pop (int lo_reg, int hi_reg, bool include_pc)
{
  int regs_to_pop, i;
  rtx sp = gen_rtx_REG (SImode, SP_REG);
  int vecidx = 0;
  
  if (lo_reg == -1 && hi_reg == -1)
    regs_to_pop = 0;
  else
    regs_to_pop = 1 + hi_reg - lo_reg;
  
  regs_to_pop += (include_pc ? 1 : 0);

  gcc_assert (regs_to_pop > 0);

  rtx par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (regs_to_pop + 1
						     + (include_pc ? 1 : 0)));

  if (include_pc)
    XVECEXP (par, 0, vecidx++) = gen_vc4_return ();

  XVECEXP (par, 0, vecidx++)
    = gen_rtx_SET (sp,
		   plus_constant (Pmode, sp, regs_to_pop * UNITS_PER_WORD));

  for (i = 0; i < regs_to_pop; i++)
    {
      rtx dst;

      if (include_pc && i == regs_to_pop - 1)
        dst = gen_rtx_REG (SImode, PC_REG);
      else
        dst = gen_rtx_REG (SImode, i + lo_reg);

      XVECEXP (par, 0, vecidx++)
        = gen_rtx_SET (dst,
		       gen_rtx_MEM (SImode,
			 plus_constant (Pmode, sp, i * UNITS_PER_WORD)));
    }

  if (include_pc)
    emit_jump_insn (par);
  else
    emit_insn (par);
}


void
vc4_expand_prologue (void)
{
  int sp_adjust;
  bool pushlr;
  rtx sp = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx fp = gen_rtx_REG (Pmode, HARD_FRAME_POINTER_REGNUM);
  rtx pat = NULL_RTX;
  rtx_insn *insn = 0;
  struct machine_function *offsets = vc4_compute_frame ();

  pushlr = offsets->lrneedssaving;

  if (flag_stack_usage_info)
    current_function_static_stack_size = offsets->pretend_size
      + offsets->local_vars + offsets->local_vars_padding
      + offsets->callee_saves + offsets->outgoing_args_size;

  if (offsets->pretend_size > 0)
    {
      int regno, offset = 0;
      insn = emit_insn (gen_addsi3 (sp, sp, GEN_INT (-offsets->pretend_size)));
      RTX_FRAME_RELATED_P (insn) = 1;
      for (offset = 0, regno = 6 - offsets->pretend_size / UNITS_PER_WORD;
	   regno < 6;
	   regno++, offset += UNITS_PER_WORD)
	{
	  insn = emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, sp,
							offset)),
				 gen_rtx_REG (SImode, regno));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  if (offsets->topreg > 0)
    insn = vc4_emit_push (R6_REG, offsets->topreg, pushlr);
  else if (pushlr)
    insn = vc4_emit_push (-1, -1, true);
  
  if (insn)
    {
#if 0
      rtx par = PATTERN (insn);
      int i;

      for (i = 0; i < XVECLEN (par, 0); i++)
        RTX_FRAME_RELATED_P (XVECEXP (par, 0, i)) = 1;
#endif

      RTX_FRAME_RELATED_P (insn) = 1;
    }
  
  sp_adjust = offsets->local_vars + offsets->local_vars_padding
	      + offsets->outgoing_args_size;

  if (sp_adjust > 0)
    {
      pat = gen_addsi3 (sp, sp, GEN_INT (-sp_adjust));
      insn = emit_insn (pat);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  
  if (offsets->need_frame_pointer)
    {
      /*emit_insn (gen_stack_tie (stack_pointer_rtx,
				hard_frame_pointer_rtx));*/
      pat = gen_addsi3 (fp, sp, GEN_INT (offsets->outgoing_args_size));
      insn = emit_insn (pat);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

void
vc4_expand_epilogue (void)
{
  int sp_adjust;
  bool pushlr;
  rtx sp = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx fp = gen_rtx_REG (Pmode, HARD_FRAME_POINTER_REGNUM);

  /*emit_insn (gen_blockage ());*/

  struct machine_function *offsets = vc4_compute_frame ();
  
  pushlr = offsets->lrneedssaving;
  
  sp_adjust = offsets->local_vars + offsets->local_vars_padding
	      + offsets->outgoing_args_size;

  if (offsets->need_frame_pointer)
    emit_insn (gen_addsi3 (sp, fp, GEN_INT (offsets->local_vars
					    + offsets->local_vars_padding)));
  else if (sp_adjust > 0)
    emit_insn (gen_addsi3 (sp, sp, GEN_INT (sp_adjust)));

  if (offsets->pretend_size == 0)
    {
      if (offsets->topreg > 0)
        vc4_emit_pop (R6_REG, offsets->topreg, pushlr);
      else if (pushlr)
        vc4_emit_pop (-1, -1, true);
    }
  else
    {
      if (offsets->topreg > 0)
        vc4_emit_pop (R6_REG, offsets->topreg, false);

      if (pushlr)
        vc4_emit_pop (LR_REG, LR_REG, false);

      emit_insn (gen_addsi3 (sp, sp, GEN_INT (offsets->pretend_size)));
    }

  if (!pushlr || offsets->pretend_size > 0)
    emit_jump_insn (gen_vc4_return ());
}

void
vc4_asm_trampoline_template (FILE *f)
{
  asm_fprintf (f, "\tmov\t%r,#0x12345678\n", STATIC_CHAIN_REGNUM);
  asm_fprintf (f, "\tj\t0x12345678\n");
  for (int i = 0; i < 10; i++)
    asm_fprintf (f, "\tnop\n");
}

void
vc4_trampoline_init (rtx m_tramp, tree fndecl, rtx static_chain)
{
  rtx m_tramp_l2only = adjust_address (m_tramp, HImode, 0x80000000);

  emit_block_move (m_tramp_l2only, assemble_trampoline_template (),
                   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  /* We have to move by halfwords because insns have 16-bit alignment so we
     can't use a single 32-bit move.  */
  rtx mem = adjust_address (m_tramp_l2only, HImode, 2);
  rtx tmp = force_reg (SImode, static_chain);
  emit_move_insn (mem, gen_lowpart (HImode, tmp));
  mem = adjust_address (m_tramp_l2only, HImode, 4);
  tmp = gen_rtx_LSHIFTRT (SImode, static_chain, GEN_INT (16));
  tmp = force_reg (SImode, tmp);
  emit_move_insn (mem, gen_lowpart (HImode, tmp));

  mem = adjust_address (m_tramp_l2only, HImode, 8);
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  tmp = force_reg (SImode, fnaddr);
  emit_move_insn (mem, gen_lowpart (HImode, tmp));
  mem = adjust_address (m_tramp_l2only, HImode, 10);
  tmp = gen_rtx_LSHIFTRT (SImode, fnaddr, GEN_INT (16));
  tmp = force_reg (SImode, tmp);
  emit_move_insn (mem, gen_lowpart (HImode, tmp));

  rtx a_tramp = XEXP (m_tramp, 0);
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__clear_cache"),
                     LCT_NORMAL, VOIDmode, 2, a_tramp, Pmode,
                     plus_constant (Pmode, a_tramp, TRAMPOLINE_SIZE), Pmode);
}

void
vc4_init_expanders (void)
{
  init_machine_status = vc4_init_machine_status;
}


/* TARGET_FUNCTION_VALUE: return RTX that represents where a function
 * return goes. */

rtx
vc4_function_value (const_tree valtype, const_tree func ATTRIBUTE_UNUSED,
		    bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;

  mode = TYPE_MODE (valtype);

  if (INTEGRAL_TYPE_P (valtype) && GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) < 4)
    mode = SImode;

  return gen_rtx_REG (mode, FIRST_RET_REG);
}

/*
 * Define where to put the arguments to a function. Value is zero to push
 * the argument on the stack, or a hard register in which to store the
 * argument.
 * 
 * MODE is the argument's machine mode. TYPE is the data type of the
 * argument (as a tree). This is null for libcalls where that information
 * may not be available. CUM is a variable of type CUMULATIVE_ARGS which
 * gives info about the preceding args and about the function being
 * called. NAMED is nonzero if this argument is a named parameter
 * (otherwise it is an extra parameter matching an ellipsis).
 * 
 * On the Videocore the first args are normally in registers and the rest
 * are pushed.  Any arg that starts within the first NPARM_REGS words is
 * at least partially passed in a register unless its data type forbids.  
 */

static rtx
vc4_function_arg (cumulative_args_t cum, machine_mode mode,
		  const_tree type, bool named)
{
  int arg_reg;

  if (!named || mode == VOIDmode)
    return 0;

  if (targetm.calls.must_pass_in_stack (mode, type))
    return 0;

  arg_reg = *get_cumulative_args (cum);

  if (arg_reg < NPARM_REGS)
    return gen_rtx_REG (mode, arg_reg + FIRST_PARM_REG);

  return 0;
}

static void
vc4_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			  const_tree type, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (!named)
    return;

  (*cum) += num_arg_regs (mode, type);
}

static unsigned int
vc4_function_arg_boundary (machine_mode /*mode*/, const_tree /*type*/)
{
  return PARM_BOUNDARY;
}

/* Returns the number of bytes of argument registers required to hold
   *part* of a parameter of machine mode MODE and type TYPE (which may be
   NULL if the type is not known).  If the argument fits entirely in the
   argument registers, or entirely on the stack, then 0 is returned.  CUM
   is the number of argument registers already used by earlier parameters
   to the function.  */

static int
vc4_arg_partial_bytes (cumulative_args_t cum, machine_mode mode,
                       tree type, bool named)
{
  int reg = *get_cumulative_args (cum);

  if (named == 0)
    return 0;

  if (targetm.calls.must_pass_in_stack (mode, type))
    return 0;

  if (reg >= NPARM_REGS)
    return 0;

  /* If the argument fits entirely in registers, return 0.  */
  if (reg + num_arg_regs (mode, type) <= NPARM_REGS)
    return 0;

  /* The argument overflows the number of available argument registers.
     Compute how many argument registers have not yet been assigned to
     hold an argument.  */
  reg = NPARM_REGS - reg;

  /* Return partially in registers and partially on the stack.  */
  return reg * UNITS_PER_WORD;
}

void
vc4_set_return_address (rtx source, rtx scratch ATTRIBUTE_UNUSED)
{
  struct machine_function *offsets = vc4_compute_frame ();
  if (!offsets->lrneedssaving)
    emit_move_insn (gen_rtx_REG (Pmode, LR_REG), source);
  else
    {
      rtx addr;
      if (frame_pointer_needed)
        addr = plus_constant (Pmode, hard_frame_pointer_rtx,
                              offsets->local_vars + offsets->local_vars_padding
                              + offsets->callee_saves - 4);
      else
        addr = plus_constant (Pmode, stack_pointer_rtx,
                              offsets->outgoing_args_size + offsets->local_vars
                              + offsets->local_vars_padding
                              + offsets->callee_saves - 4);
      rtx insn = emit_move_insn (gen_frame_mem (Pmode, addr), source);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_RESTORE, gen_rtx_REG (Pmode, LR_REG));
    }
}

void
vc4_asm_output_mi_thunk (FILE *file, tree, HOST_WIDE_INT delta,
			 HOST_WIDE_INT, tree function)
{
  int mi_delta = delta;
  const char *const mi_op = mi_delta < 0 ? "sub" : "add";
  int this_regno = (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)),
				       function) ? 1 : 0);

  if (mi_delta < 0)
    mi_delta = -mi_delta;

  final_start_function (emit_barrier (), file, 1);

  asm_fprintf (file, "\t%s\t%r,#%d\n", mi_op, this_regno, mi_delta);
  fputs ("\tb\t", file);
  assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
  fputc ('\n', file);

  final_end_function ();
}



/*
 * Handle a "naked" attribute; arguments as in struct
 * attribute_spec.handler.  
 */

static tree
vc4_handle_naked_attribute(tree * node, tree name,
                             tree args ATTRIBUTE_UNUSED,
                             int flags ATTRIBUTE_UNUSED,
                             bool * no_add_attrs)
{
    if (TREE_CODE(*node) != FUNCTION_DECL)
    {
        warning(OPT_Wattributes,
                "%qE attribute only applies to functions", name);
        *no_add_attrs = true;
    }

    return NULL_TREE;
}

/* TARGET_WARN_FUNC_RETURN: test whether this function must have its
 * return type checked. */
static bool vc4_warn_func_return(tree decl)
{
    /*
     * Naked functions are implemented entirely in assembly, including the
     * return sequence, so suppress warnings about this.  
     */
    return lookup_attribute("naked", DECL_ATTRIBUTES(decl)) == NULL_TREE;
}

bool
vc4_hard_regno_mode_ok (int regno, machine_mode mode)
{
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return regno == CC_REGNO;

  return (regno < AP_REG
	  || regno == ARG_POINTER_REGNUM
	  || regno == FRAME_POINTER_REGNUM);
}

/* TARGET_RETURN_IN_MEMORY: decides whether a value can be returned in
 * registers or must be written to memory.
 */

static bool
vc4_return_in_memory(const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
    const HOST_WIDE_INT size = int_size_in_bytes(type);
    return (size == -1 || size > 2 * UNITS_PER_WORD);
}

bool
vc4_regno_ok_for_base_p (int regno, bool strict_p)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict_p)
        return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }

  return regno < AP_REG
	 || regno == ARG_POINTER_REGNUM
	 || regno == FRAME_POINTER_REGNUM;
}

static bool
vc4_address_register_p (rtx x, bool strict_p)
{
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  if (!REG_P (x))
    return false;

  return vc4_regno_ok_for_base_p (REGNO (x), strict_p);
}

static bool
vc4_regno_ok_for_fast_base_p (int regno, bool strict_p)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict_p)
        return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }

  return regno < 16
	 || regno == ARG_POINTER_REGNUM
	 || regno == FRAME_POINTER_REGNUM;
}

static bool
vc4_sp_regno_p (int regno, bool strict_p)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict_p)
        return true;

      if (!reg_renumber)
        return false;

      regno = reg_renumber[regno];
    }

  return regno == SP_REG;
}

static bool
vc4_limited_base_regno_p (int regno, bool strict_p)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict_p)
        return true;

      if (!reg_renumber)
        return false;

      regno = reg_renumber[regno];
    }

  return regno == 0 || regno == GP_REG || regno == SP_REG || regno == PC_REG;
}

static bool
vc4_fast_address_register_p (rtx x, bool strict_p)
{
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  if (!REG_P (x))
    return false;
  
  return vc4_regno_ok_for_fast_base_p (REGNO (x), strict_p);
}

static bool
vc4_scaled_index_addr_p (machine_mode mode, rtx x, bool strict_p)
{
  switch (GET_MODE_SIZE (mode))
    {
    case 1:
      if (GET_CODE (x) == PLUS
	  && vc4_address_register_p (XEXP (x, 0), strict_p)
	  && vc4_address_register_p (XEXP (x, 1), strict_p))
	return true;
      break;

    case 2:
    case 4:
      if (GET_CODE (x) == PLUS
          && GET_CODE (XEXP (x, 0)) == MULT
	  && vc4_address_register_p (XEXP (XEXP (x, 0), 0), strict_p)
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && INTVAL (XEXP (XEXP (x, 0), 1)) == GET_MODE_SIZE (mode)
	  && vc4_address_register_p (XEXP (x, 1), strict_p))
	return true;
      break;

    default:
      ;
    }

  return false;

}

bool
vc4_short_form_addr_p (machine_mode mode, rtx x, bool strict_p)
{
  if (vc4_fast_address_register_p (x, strict_p))
    return true;

  if (GET_MODE_SIZE (mode) != 4)
    return false;

  if (GET_CODE (x) == PLUS
      && vc4_fast_address_register_p (XEXP (x, 0), strict_p)
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && INTVAL (XEXP (x, 1)) >= 0
      && INTVAL (XEXP (x, 1)) < 64
      && (INTVAL (XEXP (x, 1)) & 3) == 0)
    return true;

  if (REG_P (x) && vc4_sp_regno_p (REGNO (x), strict_p))
    return true;

  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && vc4_sp_regno_p (REGNO (XEXP (x, 0)), strict_p)
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && INTVAL (XEXP (x, 1)) >= 0
      && INTVAL (XEXP (x, 1)) < 128
      && (INTVAL (XEXP (x, 1)) & 3) == 0)
    return true;

  return false;
}

/* These are the address forms that can be used in the load/store insns with
   a bitfield for conditional execution, although those don't actually seem to
   work as predicable insns.  */

bool
vc4_conditional_form_addr_p (machine_mode mode, rtx x, bool strict_p)
{
  if (vc4_scaled_index_addr_p (mode, x, strict_p))
    return true;

  if (GET_CODE (x) == PRE_DEC || GET_CODE (x) == POST_INC)
    return vc4_address_register_p (XEXP (x, 0), strict_p);

  return false;
}

bool
vc4_displacement_form_addr_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x,
			      bool strict_p)
{
  if (vc4_address_register_p (x, strict_p))
    return true;

  /* 12-bit displacement forms.  */
  if (GET_CODE (x) == PLUS
      && vc4_address_register_p (XEXP (x, 0), strict_p)
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && INTVAL (XEXP (x, 1)) >= -2048
      && INTVAL (XEXP (x, 1)) < 2048)
    return true;

  /* 16-bit displacement forms.  */
  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && vc4_limited_base_regno_p (REGNO (XEXP (x, 0)), strict_p)
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && INTVAL (XEXP (x, 1)) >= -32768
      && INTVAL (XEXP (x, 1)) < 32768)
    return true;

  return false;
}

bool
vc4_long_form_addr_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x, bool strict_p)
{
  if (CONSTANT_P (x))
    return GET_CODE (x) == SYMBOL_REF
           || GET_CODE (x) == LABEL_REF
	   || (GET_CODE (x) == CONST
	       && GET_CODE (XEXP (x, 0)) == PLUS
	       && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
		   || GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF));

  if (CONSTANT_ADDRESS_P (x))
    return true;

  if (vc4_address_register_p (x, strict_p))
    return true;

  if (GET_CODE (x) == PLUS
      && vc4_address_register_p (XEXP (x, 0), strict_p)
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && INTVAL (XEXP (x, 1)) >= -0x4000000
      && INTVAL (XEXP (x, 1)) < 0x4000000)
    return true;

  return false;
}

static bool
vc4_legitimate_address_p_1 (machine_mode mode, rtx x, bool strict_p)
{
  return (vc4_long_form_addr_p (mode, x, strict_p)
	  || vc4_displacement_form_addr_p (mode, x, strict_p)
	  || vc4_conditional_form_addr_p (mode, x, strict_p)
	  || vc4_short_form_addr_p (mode, x, strict_p));
}

static bool
vc4_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  bool res;

#if 0
  fprintf (stderr, "check address: ");
  dump_value_slim (stderr, x, 0);
#endif

  res = vc4_legitimate_address_p_1 (mode, x, strict);

#if 0
  fprintf (stderr, "  %s\n", (res ? "OK" : "unrecognized"));
#endif

  return res;
}

/*
 * Implement TARGET_LEGITIMATE_CONSTANT_P
 * 
 * On the VC4, allow anything but a double.  
 */

/*static bool
vc4_legitimate_constant_p(machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
    return GET_CODE(x) != CONST_DOUBLE;
}*/

bool
vc4_shiftable_const (HOST_WIDE_INT x)
{
  for (int shift = 0; shift <= 8; shift++)
    {
      unsigned mask = (1 << shift) - 1;
      HOST_WIDE_INT shifted = x >> shift;
      if ((x & mask) == 0 && IN_RANGE (shifted, -32, 31))
        return true;
    }

  return false;
}

machine_mode
vc4_select_cc_mode (rtx_code op, rtx x, rtx y)
{
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    return CCFPmode;

  if ((op == EQ || op == NE)
      && (GET_CODE (x) == AND || GET_CODE (x) == ZERO_EXTRACT)
      && y == const0_rtx)
    return CC_Zmode;

  if ((op == EQ || op == NE)
      && GET_CODE (x) == NEG
      && (REG_P (y) || GET_CODE (y) == SUBREG))
    return CC_Zmode;

  if ((op == GEU || op == LTU)
      && GET_CODE (x) == PLUS
      && rtx_equal_p (y, XEXP (x, 0)))
    return CC_Cmode;

  return CCmode;
}

/* This is adapted from arm.c:vfp3_const_double_index, though the
   representable number range is somewhat different.  */

bool
vc4_valid_float_immediate (rtx x)
{
  REAL_VALUE_TYPE r, m;
  int exponent;
  unsigned HOST_WIDE_INT mantissa, mant_hi;
  unsigned HOST_WIDE_INT mask;
  int point_pos = 2 * HOST_BITS_PER_WIDE_INT - 1;
  bool fail;

  if (!CONST_DOUBLE_P (x))
    return false;

  r = *CONST_DOUBLE_REAL_VALUE (x);

  if (REAL_VALUE_ISINF (r) || REAL_VALUE_ISNAN (r) || REAL_VALUE_MINUS_ZERO (r))
    return false;

  r = real_value_abs (&r);
  exponent = REAL_EXP (&r);
  /* For the mantissa, we expand into two HOST_WIDE_INTS, apart from the
     highest (sign) bit, with a fixed binary point at bit point_pos.  */
  real_ldexp (&m, &r, point_pos - exponent);
  wide_int w = real_to_integer (&m, &fail, HOST_BITS_PER_WIDE_INT * 2);
  mantissa = w.elt (0);
  mant_hi = w.elt (1);

  /* If there are bits set in the low part of the mantissa, we can't
     represent this value.  */
  if (mantissa != 0)
    return false;

  /* Now make it so that mantissa contains the most-significant bits, and move
     the point_pos to indicate that the least-significant bits have been
     discarded.  */
  point_pos -= HOST_BITS_PER_WIDE_INT;
  mantissa = mant_hi;

  /* We permit 2 bits of mantissa only, plus a high bit which is always 1.  */
  mask = ((unsigned HOST_WIDE_INT) 1 << (point_pos - 3)) - 1;
  if ((mantissa & mask) != 0)
    return false;

  /* Now we know the mantissa is in range, chop off the unneeded bits.  */
  mantissa >>= point_pos - 3;

  /* The mantissa may be zero. Disallow that case.  */
  if (mantissa == 0)
    return false;

  /* Sanity check: two bits of mantissa, plus an always-on third bit.  */
  gcc_assert (mantissa >= 4 && mantissa <= 7);

  /* Adjust the exponent to match the required range (wrt. GCC's internal
     exponent bias and that of the VC4's compact float representation).  */
  exponent += 2;

  if (exponent < 0 || exponent > 7)
    return false;

  return true;
}

/*
 * Initialize the GCC target structure.  
 */
#ifdef OBJECT_FORMAT_ELF
#undef  TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.short\t"
#undef  TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.long\t"
#endif

#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS	vc4_print_operand_address

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND	        vc4_print_operand

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE          vc4_attribute_table
#undef  TARGET_ASM_FUNCTION_RODATA_SECTION
#define TARGET_ASM_FUNCTION_RODATA_SECTION default_no_function_rodata_section

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE	default_promote_function_mode_always_promote
#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES	hook_bool_const_tree_true

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY         vc4_return_in_memory
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK	must_pass_in_stack_var_size
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE  hook_pass_by_reference_must_pass_in_stack
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES	vc4_arg_partial_bytes
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG		vc4_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE	vc4_function_arg_advance
#undef  TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY	vc4_function_arg_boundary

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE           vc4_function_value

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE		vc4_can_eliminate

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS	vc4_setup_incoming_varargs

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE  vc4_asm_trampoline_template

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT          vc4_trampoline_init

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	vc4_legitimate_address_p

/*#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P    vc4_legitimate_constant_p*/

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN         vc4_warn_func_return

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_true

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE    vc4_target_asm_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE    vc4_target_asm_function_epilogue

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST       vc4_target_register_move_cost

/*#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST         vc4_target_memory_move_cost*/

/*#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST 		vc4_target_address_cost*/

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS                vc4_target_rtx_costs

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION default_elf_asm_named_section

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK vc4_asm_output_mi_thunk

#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-vc4.h"

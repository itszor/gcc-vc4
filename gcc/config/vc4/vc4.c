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
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "vc4.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "obstack.h"
#include "expr.h"
#include "reload.h"
#include "recog.h"
#include "function.h"
#include "ggc.h"
#include "diagnostic-core.h"
#include "target.h"
#include "target-def.h"
#include "df.h"

/*
 * Global variables for machine-dependent things.  
 */

/*
 * Provides the class number of the smallest class containing reg number.
 * 
 */
const enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER] =
{
    FAST_REGS, FAST_REGS, FAST_REGS, FAST_REGS, /* 00-03 */
    FAST_REGS, FAST_REGS, FAST_REGS, FAST_REGS, /* 04-07 */
    FAST_REGS, FAST_REGS, FAST_REGS, FAST_REGS, /* 08-11 */
    FAST_REGS, FAST_REGS, FAST_REGS, FAST_REGS, /* 12-15 */
    GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,     /* 16-19 */
    GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,     /* 20-23 */
    GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,   /* gp, sp, lr */
    ALL_REGS, ALL_REGS, ALL_REGS        /* ?ap, ?fp, ?cc */
};

/*
 * The stack frame layout we're going to use looks like follows.
 * hi   incoming_params
 *      ================== ?ap
 *      callee_saves
 *      ------------------
 *      local_vars
 *      local_vars_padding
 *      ------------------ ?fp, fp
 *      outgoing_params
 * lo   ------------------ sp
 * 
 */
struct GTY (()) machine_function {
    int callee_saves;
    int local_vars;
    int local_vars_padding;

    /* Topmost register which needs to be saved (or 0 if none). */
    int topreg;

	/* Does LR need to be saved? */
	bool lrneedssaving;
};

/*
 * Zero initialization is OK for all current fields.  
 */

static struct machine_function *vc4_init_machine_status(void)
{
    return ggc_alloc_cleared_machine_function();
}

static void vc4_compute_frame(void);
static void vc4_setup_incoming_varargs(cumulative_args_t,
                                       enum machine_mode, tree, int *,
                                       int);
static rtx handle_structs_in_regs(enum machine_mode, const_tree, int);
static tree vc4_handle_naked_attribute(tree *, tree, tree, int, bool *);
#ifdef OBJECT_FORMAT_ELF
static void vc4_asm_named_section(const char *, unsigned int, tree);
#endif
static void vc4_print_operand(FILE *, rtx, int);
static void vc4_print_operand_address(FILE *, rtx);
static bool vc4_print_operand_punct_valid_p(unsigned char code);
static void vc4_external_libcall(rtx);
static bool vc4_return_in_memory(const_tree, const_tree);
static int vc4_arg_partial_bytes(cumulative_args_t,
                                 enum machine_mode, tree, bool);
static rtx vc4_function_arg(cumulative_args_t,
                            enum machine_mode, const_tree, bool);
static void vc4_function_arg_advance(cumulative_args_t,
                                     enum machine_mode, const_tree, bool);
static unsigned int vc4_function_arg_boundary(enum machine_mode,
                                              const_tree);
static rtx vc4_function_value(const_tree valtype, const_tree func,
		bool outgoing);
static bool vc4_warn_func_return(tree);
static void vc4_option_override(void);
static bool vc4_legitimate_constant_p(enum machine_mode, rtx);

static void vc4_target_asm_function_prologue(FILE *file, HOST_WIDE_INT size);

static void vc4_target_asm_function_epilogue(FILE *file, HOST_WIDE_INT size);

static int vc4_target_register_move_cost(enum machine_mode mode,
                                         reg_class_t from, reg_class_t to);
static int vc4_target_memory_move_cost(enum machine_mode mode,
                                       reg_class_t rclass, bool in);
static int vc4_target_address_cost(rtx address,
                                   enum machine_mode mode,
                                   addr_space_t as, bool speed);
static bool vc4_target_rtx_costs(rtx, int, int, int, int *, bool);

static int num_arg_regs(enum machine_mode mode, const_tree type);

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
 * Initialize the GCC target structure.  
 */
#undef  TARGET_ASM_EXTERNAL_LIBCALL
#define TARGET_ASM_EXTERNAL_LIBCALL	vc4_external_libcall

#ifdef OBJECT_FORMAT_ELF
#undef  TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.short\t"
#undef  TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.long\t"
#endif

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

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS	vc4_setup_incoming_varargs

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE          vc4_option_override

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P    vc4_legitimate_constant_p

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN         vc4_warn_func_return

#if 0
#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_true
#endif

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE    vc4_target_asm_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE    vc4_target_asm_function_epilogue

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST       vc4_target_register_move_cost

#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST         vc4_target_memory_move_cost

#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST 		vc4_target_address_cost

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS                vc4_target_rtx_costs

struct gcc_target targetm = TARGET_INITIALIZER;

/*
 * Compute cost of moving data between one register class and another.
 * Fast registers are cheap, everything else is expensive. 
 */
static int
vc4_target_register_move_cost(enum machine_mode mode, reg_class_t from,
                              reg_class_t to)
{
    if ((from == FAST_REGS) && (to == from))
        return 2;
    return 4;
}

/*
 * Compute extra cost (over TARGET_REGISTER_MOVE_COST) to do memory
 * indirections. Slow registers are slow. 
 */
static int
vc4_target_memory_move_cost(enum machine_mode mode, reg_class_t rclass,
                            bool in)
{
    if (rclass == FAST_REGS)
        return 0;
    return 2;
}

/*
 * Compute extra cost (over TARGET_REGISTER_MOVE_COST) for an addressing
 * mode. On the VC4, all addressing modes cost the same. 
 */
static int
vc4_target_address_cost(rtx address, enum machine_mode mode,
                        addr_space_t as, bool speed)
{
    return 0;
}

/*
 * Print operand x (an rtx) in assembler syntax to file stream according
 * to modifier code.
 */

static void vc4_print_operand(FILE * stream, rtx x, int code)
{
    switch (code)
    {
        case 'C':
            fprintf(asm_out_file, "#" HOST_WIDE_INT_PRINT_DEC, INTVAL(x));
            break;

        default:
            switch (GET_CODE(x))
            {
                case REG:
                    fputs(reg_names[REGNO(x)], (stream));
                    break;
                case MEM:
                    output_address(XEXP(x, 0));
                    break;
                default:
                    output_addr_const(stream, x);
                    break;
            }
            break;
    }
}

static bool
vc4_target_rtx_costs(rtx x, int code, int outer_code,
                     int opno ATTRIBUTE_UNUSED, int *total,
                     bool speed ATTRIBUTE_UNUSED)
{
    enum machine_mode mode = GET_MODE(x);
    bool value = false;

    switch (code)
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
                /*
                 * Test for ra[rb] where ra is a byte* 
                 */
                else if ((mode == QImode) && REG_P(left) && REG_P(right))
                    *total = COSTS_N_INSNS(2);
                /*
                 * Test for ra[const] 
                 */
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
    }

    return value;
}


/*
 * Code to generate prologue and epilogue sequences.  
 */
static int number_of_regs_before_varargs;

/*
 * Set by TARGET_SETUP_INCOMING_VARARGS to indicate to prolog that this is
 * for a varargs function.  
 */
static int current_function_anonymous_args;

#define	STACK_BYTES (STACK_BOUNDARY/BITS_PER_UNIT)
#define	STORE_REACH (64)        /* Maximum displace of word store + 4.  */
#define	ADDI_REACH (32)         /* Maximum addi operand.  */

/*
 * Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  
 */

int vc4_initial_elimination_offset(int from, int to)
{
    int ret;

    /*
     * Compute this since we need to use cfun->machine->local_vars_size.  
     */
    vc4_compute_frame();

    if (from == to)
        ret = 0;
    else if ((from == FRAME_POINTER_REGNUM) && (to == STACK_POINTER_REGNUM))
    	ret = crtl->outgoing_args_size;
    else if ((from == ARG_POINTER_REGNUM) && (to == FRAME_POINTER_REGNUM))
        ret = cfun->machine->callee_saves +
              cfun->machine->local_vars +
              cfun->machine->local_vars_padding;
    else if ((from == ARG_POINTER_REGNUM) && (to == STACK_POINTER_REGNUM))
        ret = cfun->machine->callee_saves +
              cfun->machine->local_vars +
              cfun->machine->local_vars_padding +
              crtl->outgoing_args_size;
    else
        abort();

    return ret;
}

/*
 * Keep track of some information about varargs for the prolog.  
 */

static void
vc4_setup_incoming_varargs(cumulative_args_t args_so_far_v,
                           enum machine_mode mode, tree type,
                           int *ptr_pretend_size ATTRIBUTE_UNUSED,
                           int second_time ATTRIBUTE_UNUSED)
{
    CUMULATIVE_ARGS *args_so_far = get_cumulative_args(args_so_far_v);

    current_function_anonymous_args = 1;

    /*
     * We need to know how many argument registers are used before the
     * varargs start, so that we can push the remaining argument registers 
     * during the prologue.  
     */
    number_of_regs_before_varargs =
        *args_so_far + num_arg_regs(mode, type);

    /*
     * There is a bug somewhere in the arg handling code. Until I can find 
     * it this workaround always pushes the last named argument onto the
     * stack.  
     */
    number_of_regs_before_varargs = *args_so_far;

    /*
     * The last named argument may be split between argument registers and 
     * the stack.  Allow for this here.  
     */
    if (number_of_regs_before_varargs > NPARM_REGS)
        number_of_regs_before_varargs = NPARM_REGS;
}

/*
 * Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue.  
 */

static void vc4_compute_frame(void)
{
    /*
     * For aligning the local variables.  
     */
    int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
    int padding_locals;
    int regno;
    int savedregs;

    /*
     * Padding needed for each element of the frame.  
     */
    cfun->machine->local_vars = get_frame_size();

    /*
     * Align to the stack alignment.  
     */
    padding_locals = cfun->machine->local_vars % stack_alignment;
    if (padding_locals)
        padding_locals = stack_alignment - padding_locals;

    cfun->machine->local_vars_padding = padding_locals;

    /*
     * Save callee-saved registers.  
     */
    cfun->machine->topreg = 0;
    for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
        if (df_regs_ever_live_p(regno) && (!call_used_regs[regno]))
        {
            cfun->machine->topreg = regno;
        }

    cfun->machine->callee_saves = 4;
    if (cfun->machine->topreg > 0)
        cfun->machine->callee_saves += (cfun->machine->topreg - 5) * 4;

	/* Check to see if lr needs saving. */
	cfun->machine->lrneedssaving = !leaf_function_p();
}

static void vc4_target_asm_function_prologue(FILE *file, HOST_WIDE_INT size)
{
    rtx insn;
    int sp_adjust;
    int regno;
	bool pushlr;

    vc4_compute_frame();
    fprintf(file, "\t; callee saves = %d bytes\n",
		cfun->machine->callee_saves);
    fprintf(file, "\t; local vars = %d+%d bytes\n",
		cfun->machine->local_vars, cfun->machine->local_vars_padding);
    fprintf(file, "\t; outgoing = %d bytes\n",
		crtl->outgoing_args_size);
	fprintf(file, "\t; needs frame pointer = %d\n", frame_pointer_needed);
	fprintf(file, "\t; topreg = %d\n", cfun->machine->topreg);
	fprintf(file, "\t; lr needs saving = %d\n", cfun->machine->lrneedssaving);

	pushlr = cfun->machine->lrneedssaving;

    /*
     * Does not include callee_saves, as the push instruction adjusts sp
     * for us.
     */
    sp_adjust =
        cfun->machine->local_vars +
        cfun->machine->local_vars_padding + crtl->outgoing_args_size;

	
    /* Save callee-saved registers. */

    if (cfun->machine->topreg > 0)
    {
		rtx op = gen_rtx_REG(Pmode, cfun->machine->topreg);
    	if (cfun->machine->topreg == R6_REG)
    		output_asm_insn(pushlr ? "push %0, lr" : "push %0", &op);
    	else
    		output_asm_insn(pushlr ? "push r6-%0, lr" : "push r6-%0", &op);
    }
    else if (pushlr)
    	output_asm_insn("push lr", NULL);

	/* Allocate space for locals and outgoing. */

    if (sp_adjust > 0)
    {
    	rtx ops[2] =
    	{
    		stack_pointer_rtx,
    		GEN_INT(sp_adjust)
    	};
    	output_asm_insn("sub %0, #%1", ops);
    }
	/* If we need a frame pointer, set it up now. */

	if (frame_pointer_needed)
	{
		rtx ops[] =
		{
			gen_rtx_REG(Pmode, FRAME_POINTER_REGNUM),
			gen_rtx_REG(Pmode, STACK_POINTER_REGNUM)
		};

		output_asm_insn("mov %0, %1", ops);
	}

}

static void vc4_target_asm_function_epilogue(FILE *file, HOST_WIDE_INT size)
{
    rtx insn;
    int regno;
	bool pushlr;

    vc4_compute_frame();

	pushlr = cfun->machine->lrneedssaving;

	if (frame_pointer_needed)
	{
		/* If we had a frame pointer, reset the stack. */

		rtx ops[] =
		{
			gen_rtx_REG(Pmode, FRAME_POINTER_REGNUM),
			gen_rtx_REG(Pmode, STACK_POINTER_REGNUM)
		};

		output_asm_insn("mov %1, %0", ops);
	}
	else
	{
		/* Otherwise retract over the locals. */

		int sp_adjust = cfun->machine->local_vars +
			cfun->machine->local_vars_padding + crtl->outgoing_args_size;

		if (sp_adjust > 0)
		{
			rtx ops[2] =
			{
				stack_pointer_rtx,
				GEN_INT(sp_adjust)
			};
			output_asm_insn("sub %0, #%1", ops);
		}
    }

    /*
     * Reload callee-saved registers and return.
     */
    if (cfun->machine->topreg > 0)
    {
		rtx op = gen_rtx_REG(Pmode, cfun->machine->topreg);
    	if (cfun->machine->topreg == 6)
    		output_asm_insn(pushlr ? "pop %0, pc" : "pop %0", &op);
    	else
    		output_asm_insn(pushlr ? "pop r6-%0, pc" : "pop r6-%0", &op);
    }
    else if (pushlr)
    	output_asm_insn("pop pc", NULL);

	if (!pushlr)
		output_asm_insn("rts", NULL);
}

static void
vc4_option_override(void)
{
    /*
     * Set the per-function-data initializer.  
     */
    init_machine_status = vc4_init_machine_status;
}

/*
 * Compute the number of word sized registers needed to hold a function
 * argument of mode MODE and type TYPE.  
 */

static int
num_arg_regs(enum machine_mode mode, const_tree type)
{
    int size;

    if (targetm.calls.must_pass_in_stack(mode, type))
        return 0;

    if (type && mode == BLKmode)
        size = int_size_in_bytes(type);
    else
        size = GET_MODE_SIZE(mode);

    return ROUND_ADVANCE(size);
}

static rtx
handle_structs_in_regs(enum machine_mode mode, const_tree type, int reg)
{
    int size;

    /*
     * We define that a structure whose size is not a whole
     * multiple of bytes is passed packed into registers (or spilled onto
     * the stack if not enough registers are available) with the last few
     * bytes of the structure being packed, left-justified, into the last
     * register/stack slot. GCC handles this correctly if the last word is 
     * in a stack slot, but we have to generate a special, PARALLEL RTX if 
     * the last word is in an argument register.  
     */
    if (type
        && TYPE_MODE(type) == BLKmode
        && TREE_CODE(TYPE_SIZE(type)) == INTEGER_CST
        && (size = int_size_in_bytes(type)) > UNITS_PER_WORD
        && (size % UNITS_PER_WORD != 0)
        && (reg + num_arg_regs(mode, type) <=
            (FIRST_PARM_REG + NPARM_REGS)))
    {
        rtx arg_regs[NPARM_REGS];
        int nregs;
        rtx result;
        rtvec rtvec;

        for (nregs = 0; size > 0; size -= UNITS_PER_WORD)
        {
            arg_regs[nregs] =
                gen_rtx_EXPR_LIST(SImode,
                                  gen_rtx_REG(SImode, reg++),
                                  GEN_INT(nregs * UNITS_PER_WORD));
            nregs++;
        }

        /*
         * We assume here that NPARM_REGS == 6.  The assert checks this.  
         */
        gcc_assert(ARRAY_SIZE(arg_regs) == 6);
        rtvec = gen_rtvec(nregs, arg_regs[0], arg_regs[1], arg_regs[2],
                          arg_regs[3], arg_regs[4], arg_regs[5]);

        result = gen_rtx_PARALLEL(mode, rtvec);
        return result;
    }

    return gen_rtx_REG(mode, reg);
}

/* TARGET_FUNCTION_VALUE: return RTX that represents where a function
 * return goes. */

rtx vc4_function_value(const_tree valtype, const_tree func, bool outgoing)
{
    enum machine_mode mode;
    int unsigned_p;

    mode = TYPE_MODE(valtype);

    /*
     * Since we promote return types, we must promote the mode here too.  
     */
    mode = promote_function_mode(valtype, mode, &unsigned_p, func, 1);

    return handle_structs_in_regs(mode, valtype, FIRST_RET_REG);
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
vc4_function_arg(cumulative_args_t cum, enum machine_mode mode,
                 const_tree type, bool named)
{
    int arg_reg;

    if (!named || mode == VOIDmode)
        return 0;

    if (targetm.calls.must_pass_in_stack(mode, type))
        return 0;

    arg_reg = ROUND_REG(*get_cumulative_args(cum), mode);

    if (arg_reg < NPARM_REGS)
        return handle_structs_in_regs(mode, type,
                                      FIRST_PARM_REG + arg_reg);

    return 0;
}

static void
vc4_function_arg_advance(cumulative_args_t cum_v, enum machine_mode mode,
                         const_tree type, bool named ATTRIBUTE_UNUSED)
{
    CUMULATIVE_ARGS *cum = get_cumulative_args(cum_v);

    *cum = (ROUND_REG(*cum, mode)
            + (int) named * num_arg_regs(mode, type));
}

static unsigned int
vc4_function_arg_boundary(enum machine_mode mode,
                          const_tree type ATTRIBUTE_UNUSED)
{
    /*
     * Doubles must be aligned to an 8 byte boundary.  
     */
    return (mode != BLKmode && GET_MODE_SIZE(mode) == 8
            ? BIGGEST_ALIGNMENT : PARM_BOUNDARY);
}

/*
 * Returns the number of bytes of argument registers required to hold
 * *part* of a parameter of machine mode MODE and type TYPE (which may be
 * NULL if the type is not known).  If the argument fits entirely in the
 * argument registers, or entirely on the stack, then 0 is returned.  CUM
 * is the number of argument registers already used by earlier parameters
 * to the function.  
 */

static int
vc4_arg_partial_bytes(cumulative_args_t cum, enum machine_mode mode,
                      tree type, bool named)
{
    int reg = ROUND_REG(*get_cumulative_args(cum), mode);

    if (named == 0)
        return 0;

    if (targetm.calls.must_pass_in_stack(mode, type))
        return 0;

    if (reg >= NPARM_REGS)
        return 0;

    /*
     * If the argument fits entirely in registers, return 0.  
     */
    if (reg + num_arg_regs(mode, type) <= NPARM_REGS)
        return 0;

    /*
     * The argument overflows the number of available argument registers.
     * Compute how many argument registers have not yet been assigned to
     * hold an argument.  
     */
    reg = NPARM_REGS - reg;

    /*
     * Return partially in registers and partially on the stack.  
     */
    return reg * UNITS_PER_WORD;
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

#ifdef OBJECT_FORMAT_ELF
static void
vc4_asm_named_section(const char *name,
                        unsigned int flags ATTRIBUTE_UNUSED,
                        tree decl ATTRIBUTE_UNUSED)
{
    fprintf(asm_out_file, "\t.section %s\n", name);
}
#endif /* OBJECT_FORMAT_ELF */

/* TARGET_ASM_EXTERNAL_LIBCALL: import an extern symbol reference.
 */

static void vc4_external_libcall(rtx fun)
{
    fprintf(asm_out_file, "\t.import\t");
    assemble_name(asm_out_file, XSTR(fun, 0));
    fprintf(asm_out_file, "\n");
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

/*
 * Implement TARGET_LEGITIMATE_CONSTANT_P
 * 
 * On the VC4, allow anything but a double.  
 */

static bool
vc4_legitimate_constant_p(enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
    return GET_CODE(x) != CONST_DOUBLE;
}

#include "gt-vc4.h"

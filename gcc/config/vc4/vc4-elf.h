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

#ifndef __VC4_ELF_H__
#define __VC4_ELF_H__

#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DWARF2_DEBUGGING_INFO 1
#define DWARF2_UNWIND_INFO 0
#define OBJECT_FORMAT_ELF

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */
#undef  ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)         \
  do                                                                     \
    {                                                                    \
      const char * name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);           \
      HOST_WIDE_INT size;						 \
      if (!flag_inhibit_size_directive && DECL_SIZE (DECL)               \
          && ! AT_END && TOP_LEVEL                                       \
          && DECL_INITIAL (DECL) == error_mark_node                      \
          && !size_directive_output)                                     \
        {                                                                \
	  size_directive_output = 1;					 \
	  size = int_size_in_bytes (TREE_TYPE (DECL));			 \
	  ASM_OUTPUT_SIZE_DIRECTIVE (FILE, name, size);			 \
        }                                                                \
    }                                                                    \
  while (0)


#undef LIB_SPEC
//#define LIB_SPEC "--start-group -lc -lgloss --end-group"
#define LIB_SPEC "-lc"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crti.o%s crtbegin.o%s"

/* Include the OS stub library, so that the code can be simulated.
   This is not the right way to do this.  Ideally this kind of thing
   should be done in the linker script - but I have not worked out how
   to specify the location of a linker script in a gcc command line yet.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC  "crtend.o%s crtn.o%s"

/* The subroutine calls in the .init and .fini sections create literal
   pools which must be jumped around....  */
/*#define FORCE_CODE_SECTION_ALIGN	asm ("br 1f ; .literals ; 1:");*/

#undef  CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors,\"aw\""
#undef  DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors,\"aw\""

#endif

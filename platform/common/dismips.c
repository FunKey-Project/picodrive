/*
 * very basic mips disassembler for MIPS32/MIPS64 Release 1, only for picodrive
 * Copyright (C) 2019 kub
 *
 * This work is licensed under the terms of MAME license.
 * See COPYING file in the top-level directory.
 */

// XXX unimplemented: SYSCALL, BREAK, SYNC, SDBBP, T*, CACHE, PREF,
// MOVF/MOVT, LWC*/LDC*, SWC*/SDC*, COP*.
// however, it's certainly good enough for anything picodrive DRC throws at it.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "dismips.h"


static char *const register_names[32] = {
	"$zero",
	"$at",
	"$v0",
	"$v1",
	"$a0",
	"$a1",
	"$a2",
	"$a3",
	"$t0",
	"$t1",
	"$t2",
	"$t3",
	"$t4",
	"$t5",
	"$t6",
	"$t7",
	"$s0",
	"$s1",
	"$s2",
	"$s3",
	"$s4",
	"$s5",
	"$s6",
	"$s7",
	"$t8",
	"$t9",
	"$k0",
	"$k1",
	"$gp",
	"$sp",
	"$fp",
	"$ra"
};


enum insn_type {
	REG_DTS, REG_TS,	// 3, 2, or 1 regs
	REG_DS, REG_D, REG_S,
	S_IMM_DT,		// 2 regs with shift amount
	B_IMM_S, B_IMM_TS,	// pc-relative branches with 1 or 2 regs
	J_IMM,			// region-relative jump
	A_IMM_TS,		// arithmetic immediate with 1 or 2 regs
	L_IMM_T, L_IMM_TS,	// logical immediate with 2 regs
	M_IMM_TS,		// memory indexed with 2 regs
};

struct insn {
	unsigned char op;
	enum insn_type type;
	char *name;
};

// ATTN: these array MUST be sorted by op (decode relies on it)

// instructions with opcode SPECIAL (R-type)
#define OP_SPECIAL	0x00
static const struct insn special_insns[] = {
	{0x00, S_IMM_DT, "sll"},
	{0x02, S_IMM_DT, "srl"},
	{0x03, S_IMM_DT, "sra"},
	{0x04, REG_DTS, "sllv"},
	{0x06, REG_DTS, "srlv"},
	{0x07, REG_DTS, "srav"},
	{0x08, REG_S,   "jr"},
	{0x09, REG_DS,  "jalr"},
	{0x0a, REG_DTS, "movz"},
	{0x0b, REG_DTS, "movn"},
//	{0x0c, ,	"syscall"},
//	{0x0d, ,	"break"},
//	{0x0f, ,	"sync"},
	{0x10, REG_D,   "mfhi"},
	{0x11, REG_S,   "mthi"},
	{0x12, REG_D,   "mflo"},
	{0x13, REG_S,   "mtlo"},
	{0x14, REG_DTS, "dsllv"},
	{0x16, REG_DTS, "dslrv"},
	{0x17, REG_DTS, "dsrav"},
	{0x18, REG_TS,  "mult"},
	{0x19, REG_TS,  "multu"},
	{0x1A, REG_TS,  "div"},
	{0x1B, REG_TS,  "divu"},
	{0x1C, REG_TS,  "dmult"},
	{0x1D, REG_TS,  "dmultu"},
	{0x1E, REG_TS,  "ddiv"},
	{0x1F, REG_TS,  "ddivu"},
	{0x20, REG_DTS, "add"},
	{0x21, REG_DTS, "addu"},
	{0x22, REG_DTS, "sub"},
	{0x23, REG_DTS, "subu"},
	{0x24, REG_DTS, "and"},
	{0x25, REG_DTS, "or"},
	{0x26, REG_DTS, "xor"},
	{0x27, REG_DTS, "nor"},
	{0x2A, REG_DTS, "slt"},
	{0x2B, REG_DTS, "sltu"},
	{0x2C, REG_DTS, "dadd"},
	{0x2D, REG_DTS, "daddu"},
	{0x2E, REG_DTS, "dsub"},
	{0x2F, REG_DTS, "dsubu"},
//	{0x30, REG_TS,  "tge" },
//	{0x31, REG_TS,  "tgeu" },
//	{0x32, REG_TS,  "tlt" },
//	{0x33, REG_TS,  "tltu" },
//	{0x34, REG_TS,  "teq" },
//	{0x36, REG_TS,  "tne" },
	{0x38, S_IMM_DT, "dsll"},
	{0x3A, S_IMM_DT, "dsrl"},
	{0x3B, S_IMM_DT, "dsra"},
	{0x3C, S_IMM_DT, "dsll32"},
	{0x3E, S_IMM_DT, "dsrl32"},
	{0x3F, S_IMM_DT, "dsra32"},
};

// instructions with opcode SPECIAL2 (R-type)
#define OP_SPECIAL2	0x1C
static const struct insn special2_insns[] = {
	{0x00, REG_TS,  "madd" },
	{0x01, REG_TS,  "maddu" },
	{0x02, REG_TS,  "mul" },
	{0x04, REG_TS,  "msub" },
	{0x05, REG_TS,  "msubu" },
	{0x20, REG_DS,  "clz" },
	{0x21, REG_DS,  "clo" },
	{0x24, REG_DS,  "dclz" },
	{0x25, REG_DS,  "dclo" },
};

// instructions with opcode REGIMM (I-type)
#define OP_REGIMM	0x01
static const struct insn regimm_insns[] = {
	{0x00, B_IMM_S, "bltz"},
	{0x01, B_IMM_S, "bgez"},
	{0x02, B_IMM_S, "bltzl"},
	{0x03, B_IMM_S, "bgezl"},
//	{0x08, ,	"tgei"},
//	{0x09, ,	"tgeiu"},
//	{0x0a, ,	"tlti"},
//	{0x0b, ,	"tltiu"},
//	{0x0c, ,	"teqi"},
//	{0x0e, ,	"tnei"},
	{0x10, B_IMM_S, "bltzal"},
	{0x11, B_IMM_S, "bgezal"},
	{0x12, B_IMM_S, "bltzall"},
	{0x13, B_IMM_S, "bgezall"},
	{0x13, B_IMM_S, "bgezall"},
};

// instructions with other opcodes (I-type)
static const struct insn immediate_insns[] = {
	{0x02, J_IMM,	 "j"},
	{0x03, J_IMM,	 "jal"},
	{0x04, B_IMM_TS, "beq"},
	{0x05, B_IMM_TS, "bne"},
	{0x06, B_IMM_S,  "blez"},
	{0x07, B_IMM_S,  "bgtz"},
	{0x08, A_IMM_TS, "addi"},
	{0x09, A_IMM_TS, "addiu"},
	{0x0A, A_IMM_TS, "slti"},
	{0x0B, A_IMM_TS, "sltiu"},
	{0x0C, L_IMM_TS, "andi"},
	{0x0D, L_IMM_TS, "ori"},
	{0x0E, L_IMM_TS, "xori"},
	{0x0F, L_IMM_T,  "lui"},
	{0x14, B_IMM_TS, "beql"},
	{0x15, B_IMM_TS, "bnel"},
	{0x16, B_IMM_S,  "blezl"},
	{0x17, B_IMM_S,  "bgtzl"},
	{0x18, A_IMM_TS, "daddi"},
	{0x19, A_IMM_TS, "daddiu"},
	{0x1A, M_IMM_TS, "ldl"},
	{0x1B, M_IMM_TS, "ldr"},
	{0x20, M_IMM_TS, "lb"},
	{0x21, M_IMM_TS, "lh"},
	{0x22, M_IMM_TS, "lwl"},
	{0x23, M_IMM_TS, "lw"},
	{0x24, M_IMM_TS, "lbu"},
	{0x25, M_IMM_TS, "lhu"},
	{0x26, M_IMM_TS, "lwr"},
	{0x27, M_IMM_TS, "lwu"},
	{0x28, M_IMM_TS, "sb"},
	{0x29, M_IMM_TS, "sh"},
	{0x2A, M_IMM_TS, "swl"},
	{0x2B, M_IMM_TS, "sw"},
	{0x2C, M_IMM_TS, "sdl"},
	{0x2D, M_IMM_TS, "sdr"},
	{0x2E, M_IMM_TS, "swr"},
//	{0x2F, ,	 "cache"},
	{0x30, M_IMM_TS, "ll"},
//	{0x31, ,	 "lwc1"},
//	{0x32, ,	 "lwc2"},
//	{0x33, ,	 "pref"},
	{0x34, M_IMM_TS, "lld"},
//	{0x35, ,	 "ldc1"},
//	{0x36, ,	 "ldc2"},
	{0x37, M_IMM_TS, "ld"},
	{0x38, M_IMM_TS, "sc"},
//	{0x39, ,	 "swc1"},
//	{0x3A, ,	 "swc2"},
	{0x3C, M_IMM_TS, "scd"},
//	{0x3D, ,	 "sdc1"},
//	{0x3E, ,	 "sdc2"},
	{0x3F, M_IMM_TS, "sd"},
};

#define ARRAY_SIZE(a)	(sizeof(a)/sizeof(*a))

// find instruction description for insn
static const struct insn *decode_insn(uint32_t insn)
{
	uint32_t op = insn >> 26;
	const struct insn *pi;
	int l = 0, r = 0;

	if (op == OP_SPECIAL) {
		op = insn & 0x3f;
		pi = special_insns;
		r = ARRAY_SIZE(special_insns)-1;
	} else if (op == OP_SPECIAL2) {
		op = insn & 0x3f;
		pi = special2_insns;
		r = ARRAY_SIZE(special2_insns)-1;
	} else if (op == OP_REGIMM) {
		op = (insn>>16) & 0x1f;
		pi = regimm_insns;
		r = ARRAY_SIZE(regimm_insns)-1;
	} else {
		pi = immediate_insns;
		r = ARRAY_SIZE(immediate_insns)-1;
	}

	while (l <= r) {
		int m = (l+r) / 2;
		if (pi[m].op == op)
			return pi+m;
		else if (pi[m].op < op)
			l = m+1;
		else
			r = m-1;
	}
	return NULL;
}

// calculate target for pc-relative branches
static unsigned long b_target(unsigned long pc, uint32_t insn)
{
	return pc + 4 + (int16_t)insn * 4;
}

// calculate target for region-relative branches
static unsigned long j_target(unsigned long pc, uint32_t insn)
{
	return (pc & ~0x0fffffffL) | ((insn & 0x03ffffff) << 2);
}

// main disassembler function
int dismips(uintptr_t pc, uint32_t insn, char *buf, size_t buflen, uintptr_t *sym)
{
	const struct insn *pi = decode_insn(insn);
	char *rs = register_names[(insn >> 21) & 0x1f];
	char *rt = register_names[(insn >> 16) & 0x1f];
	char *rd = register_names[(insn >> 11) & 0x1f];
	int sa = (insn >> 6) & 0x1f;
	int imm = (int16_t) insn;

	*sym = 0;
	if (pi == NULL) {
		snprintf(buf, buflen, "0x%x", insn);
		return 0;
	}

	switch (pi->type) {
	case REG_DTS:
		if ((insn & 0x3f) == 0x25 /*OR*/ && (insn & 0x1f0000) == 0 /*zero*/)
			snprintf(buf, buflen, "move %s, %s", rd, rs);
		else
			snprintf(buf, buflen, "%s %s, %s, %s", pi->name, rd, rs, rt);
		break;
	case REG_TS:
		snprintf(buf, buflen, "%s %s, %s", pi->name, rs, rt);
		break;
	case REG_DS:
		snprintf(buf, buflen, "%s %s, %s", pi->name, rd, rs);
		break;
	case REG_D:
		snprintf(buf, buflen, "%s %s", pi->name, rd);
		break;
	case REG_S:
		snprintf(buf, buflen, "%s %s", pi->name, rs);
		break;
	case S_IMM_DT:
		if (insn == 0x00000000)
			snprintf(buf, buflen, "nop");
		else
			snprintf(buf, buflen, "%s %s, %s, %d", pi->name, rd, rt, sa);
		break;
	case B_IMM_S:
		*sym = b_target(pc, insn);
		snprintf(buf, buflen, "%s %s, 0x%lx", pi->name, rs, *sym);
		break;
	case B_IMM_TS:
		*sym = b_target(pc, insn);
		snprintf(buf, buflen, "%s %s, %s, 0x%lx", pi->name, rs, rt, *sym);
		break;
	case J_IMM:
		*sym = j_target(pc, insn);
		snprintf(buf, buflen, "%s 0x%lx", pi->name, *sym);
		break;
	case A_IMM_TS:
		if (abs(imm) < 1000)
			snprintf(buf, buflen, "%s %s, %s, %d", pi->name, rt, rs, imm);
		else
			snprintf(buf, buflen, "%s %s, %s, 0x%x", pi->name, rt, rs, imm);
		break;
	case L_IMM_T:
		snprintf(buf, buflen, "%s %s, 0x%x", pi->name, rt, (uint16_t)imm);
		break;
	case L_IMM_TS:
		if ((insn >> 26) == 0x34 /*ORI*/ && (insn & 0x03e00000) == 0 /*zero*/)
			snprintf(buf, buflen, "li %s, 0x%x", rt, (uint16_t)imm);
		else
			snprintf(buf, buflen, "%s %s, %s, 0x%x", pi->name, rt, rs, (uint16_t)imm);
		break;
	case M_IMM_TS:
		snprintf(buf, buflen, "%s %s, %d(%s)", pi->name, rt, imm, rs);
		break;
	}
	return 1;
}

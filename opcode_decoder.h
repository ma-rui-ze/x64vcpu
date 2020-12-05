enum x64cpu_register_set {
  X64CPU_REG_NONE = 0,
  X64CPU_REG_GP,
  X64CPU_REG_GP_H,
  X64CPU_REG_F,
  X64CPU_REG_FPU,
  X64CPU_REG_MM,
  X64CPU_REG_XMM,
  X64CPU_REG_S,
  X64CPU_REG_EEE1,
  X64CPU_REG_EEE2
};

enum x64cpu_registers {
  X64CPU_REGISTER_RAX = 0x00,
  X64CPU_REGISTER_RCX = 0x01,
  X64CPU_REGISTER_RDX = 0x02,
  X64CPU_REGISTER_RBX = 0x03,
  X64CPU_REGISTER_RSP = 0x04,
  X64CPU_REGISTER_RBP = 0x05,
  X64CPU_REGISTER_RSI = 0x06,
  X64CPU_REGISTER_RDI = 0x07,

  X64CPU_REGISTER_AH = 0x04,
  X64CPU_REGISTER_CH = 0x05,
  X64CPU_REGISTER_DH = 0x06,
  X64CPU_REGISTER_BH = 0x07,

  X64CPU_REGISTER_R8 = 0x00,
  X64CPU_REGISTER_R9 = 0x01,
  X64CPU_REGISTER_R10 = 0x02,
  X64CPU_REGISTER_R11 = 0x03,
  X64CPU_REGISTER_R12 = 0x04,
  X64CPU_REGISTER_R13 = 0x05,
  X64CPU_REGISTER_R14 = 0x06,
  X64CPU_REGISTER_R15 = 0x07,
};

enum x64cpu_operation {
  X64CPU_OP_NONE = 0,
  X64CPU_OP_INVALID = 0,
  X64CPU_OP_PREFIX = 0,
  X64CPU_OP_TODO = 0,

  X64CPU_OP_GROUP,
  X64CPU_OP_EGROUP,

  X64CPU_OP_NOOP,

  X64CPU_OP_ADD,
  X64CPU_OP_ADC,
  X64CPU_OP_AND,
  X64CPU_OP_XOR,
  X64CPU_OP_OR,
  X64CPU_OP_SBB,
  X64CPU_OP_SUB,
  X64CPU_OP_CMP,
  X64CPU_OP_TEST,
  X64CPU_OP_XCHG,
  X64CPU_OP_NOT,
  X64CPU_OP_NEG,

  X64CPU_OP_MOV,
  X64CPU_OP_LEA,

  X64CPU_OP_MOVZX,
  X64CPU_OP_MOVSX,
  X64CPU_OP_MOVSXD,

  X64CPU_OP_INC,
  X64CPU_OP_DEC,

  X64CPU_OP_PUSH,
  X64CPU_OP_POP,

  X64CPU_OP_IMUL,
  X64CPU_OP_IMUL1,
  X64CPU_OP_IMUL3,
  X64CPU_OP_MUL,
  X64CPU_OP_MUL1,
  X64CPU_OP_MUL3,
  X64CPU_OP_DIV,
  X64CPU_OP_IDIV,

  X64CPU_OP_MOVS,
  X64CPU_OP_CMPS,
  X64CPU_OP_STOS,
  X64CPU_OP_LODS,
  X64CPU_OP_SCAS,

  X64CPU_OP_JMP,
  X64CPU_OP_JMP_I,
  X64CPU_OP_JMPF,
  X64CPU_OP_JMPF_I,
  X64CPU_OP_CJMP,

  X64CPU_OP_CALL,
  X64CPU_OP_CALL_I,
  X64CPU_OP_CALLF_I,
  X64CPU_OP_IRET,
  X64CPU_OP_RETN,
  X64CPU_OP_RETF,

  X64CPU_OP_ENTER,
  X64CPU_OP_LEAVE,

  X64CPU_OP_INT,

  X64CPU_OP_SYSCALL,

  X64CPU_OP_LOOP,
  X64CPU_OP_LOOPE,
  X64CPU_OP_LOOPNE,
  X64CPU_OP_JRCX,

  X64CPU_OP_IN,
  X64CPU_OP_OUT,
  X64CPU_OP_INS,
  X64CPU_OP_OUTS,

  X64CPU_OP_CONV,
  X64CPU_OP_CONV2,

  X64CPU_OP_HLT,

  X64CPU_OP_CMC,

  X64CPU_OP_CLC,
  X64CPU_OP_STC,
  X64CPU_OP_CLI,
  X64CPU_OP_STI,
  X64CPU_OP_CLD,
  X64CPU_OP_STD,

  X64CPU_OP_SAHF,
  X64CPU_OP_LAHF,

  X64CPU_OP_XLAT,

  X64CPU_OP_ROL,
  X64CPU_OP_ROR,
  X64CPU_OP_RCL,
  X64CPU_OP_RCR,
  X64CPU_OP_SHL,
  X64CPU_OP_SHR,
  X64CPU_OP_SAR,

  X64CPU_OP_CMOV,
  X64CPU_OP_CSET,

  X64CPU_OP_CMPXCHG,
  X64CPU_OP_XADD,

  X64CPU_OP_BT,
  X64CPU_OP_BTS,
  X64CPU_OP_BTR,
  X64CPU_OP_BTC,
  X64CPU_OP_BSWAP,

  X64CPU_OP_BSF,
  X64CPU_OP_BSR,

  X64CPU_OP_CPUID,
  X64CPU_OP_RDTSC,

  /* FPU Instructions Groups */
  X64CPU_OP_FPU,

  /* FPU Instructions */
  X64CPU_OP_FPU_FADD,
  X64CPU_OP_FPU_FIADD,
  X64CPU_OP_FPU_FADDP,
  X64CPU_OP_FPU_FMUL,
  X64CPU_OP_FPU_FMULP,
  X64CPU_OP_FPU_FIMUL,
  X64CPU_OP_FPU_FCOM,
  X64CPU_OP_FPU_FICOM,
  X64CPU_OP_FPU_FCOMP,
  X64CPU_OP_FPU_FCOMPP,
  X64CPU_OP_FPU_FICOMP,
  X64CPU_OP_FPU_FSUB,
  X64CPU_OP_FPU_FSUBP,
  X64CPU_OP_FPU_FSUBR,
  X64CPU_OP_FPU_FSUBRP,
  X64CPU_OP_FPU_FISUB,
  X64CPU_OP_FPU_FISUBR,
  X64CPU_OP_FPU_FDIV,
  X64CPU_OP_FPU_FDIVP,
  X64CPU_OP_FPU_FDIVR,
  X64CPU_OP_FPU_FDIVRP,
  X64CPU_OP_FPU_FIDIV,
  X64CPU_OP_FPU_FIDIVP,
  X64CPU_OP_FPU_FIDIVR,
  X64CPU_OP_FPU_FIDIVRP,
  X64CPU_OP_FPU_FLD,
  X64CPU_OP_FPU_FILD,
  X64CPU_OP_FPU_FXCH,
  X64CPU_OP_FPU_FST,
  X64CPU_OP_FPU_FSTP,

  X64CPU_OP_FPU_FCHS,
  X64CPU_OP_FPU_FABS,
  X64CPU_OP_FPU_FTST,
  X64CPU_OP_FPU_FXAM,

  X64CPU_OP_FPU_FLD1,
  X64CPU_OP_FPU_FLDL2T,
  X64CPU_OP_FPU_FLDL2E,
  X64CPU_OP_FPU_FLDPI,
  X64CPU_OP_FPU_FLDLG2,
  X64CPU_OP_FPU_FLDLN2,
  X64CPU_OP_FPU_FLDZ,

  X64CPU_OP_FPU_F2XM1,
  X64CPU_OP_FPU_FYL2X,
  X64CPU_OP_FPU_FPTAN,
  X64CPU_OP_FPU_FPATAN,
  X64CPU_OP_FPU_FXTRACT,
  X64CPU_OP_FPU_FPREM1,
  X64CPU_OP_FPU_FDECSTP,
  X64CPU_OP_FPU_FINCSTP,

  X64CPU_OP_FPU_FPREM,
  X64CPU_OP_FPU_FYL2XP1,
  X64CPU_OP_FPU_FSQRT,
  X64CPU_OP_FPU_FSINCOS,
  X64CPU_OP_FPU_FRNDINT,
  X64CPU_OP_FPU_FSCALE,
  X64CPU_OP_FPU_FSIN,
  X64CPU_OP_FPU_FCOS,

  X64CPU_OP_FPU_FENI,
  X64CPU_OP_FPU_FDISI,
  X64CPU_OP_FPU_FCLEX,
  X64CPU_OP_FPU_FSETPM,

  X64CPU_OP_FPU_FLDENV,
  X64CPU_OP_FPU_FSTENV,

  X64CPU_OP_FPU_FLDCW,
  X64CPU_OP_FPU_FSTCW,

  X64CPU_OP_FPU_FFREE,
  X64CPU_OP_FPU_FFREEP,

  X64CPU_OP_FPU_FIST,
  X64CPU_OP_FPU_FISTP,
  X64CPU_OP_FPU_FISTTP,

  X64CPU_OP_FPU_FCMOVB,
  X64CPU_OP_FPU_FCMOVE,
  X64CPU_OP_FPU_FCMOVBE,
  X64CPU_OP_FPU_FCMOVU,
  X64CPU_OP_FPU_FCMOVNB,
  X64CPU_OP_FPU_FCMOVNE,
  X64CPU_OP_FPU_FCMOVNBE,
  X64CPU_OP_FPU_FCMOVNU,

  X64CPU_OP_FPU_FUCOM,
  X64CPU_OP_FPU_FUCOMI,
  X64CPU_OP_FPU_FUCOMIP,
  X64CPU_OP_FPU_FCOMI,
  X64CPU_OP_FPU_FCOMIP,
  X64CPU_OP_FPU_FUCOMP,
  X64CPU_OP_FPU_FUCOMPP,

  X64CPU_OP_FPU_FINIT,
  X64CPU_OP_FPU_FNINIT,

  X64CPU_OP_FPU_FRSTOR,
  X64CPU_OP_FPU_FSAVE,
  X64CPU_OP_FPU_FSTSW,

  X64CPU_OP_FPU_FBLD,
  X64CPU_OP_FPU_FBSTP,

  /* SSE Instructions */
  X64CPU_OP_SSE, /* Redirect to mmx and sse instructions definition */

  /* SSE Instructions */
  X64CPU_OP_SSE_MOV,
  X64CPU_OP_SSE_MOVSS,
  X64CPU_OP_SSE_MOVSD,
  X64CPU_OP_SSE_MOVUP,
  X64CPU_OP_SSE_MOVAPS,
  X64CPU_OP_SSE_MOVAPD,
  X64CPU_OP_SSE_MOVLPD,
  X64CPU_OP_SSE_MOVHPD,
  X64CPU_OP_SSE_MOVD,

  X64CPU_OP_SSE_MOVDQA,
  X64CPU_OP_SSE_MOVDQU,

  X64CPU_OP_SSE_PUNPCKLBW,
  X64CPU_OP_SSE_PUNPCKLWD,
  X64CPU_OP_SSE_PSHUFD,
  X64CPU_OP_SSE_POR,
  X64CPU_OP_SSE_PXOR,
  X64CPU_OP_SSE_PSUBB,
  X64CPU_OP_SSE_PSLLDQ,
  X64CPU_OP_SSE_PMOVMSKB,
  X64CPU_OP_SSE_PCMPEQB,
  X64CPU_OP_SSE_PMINUB,

  X64CPU_OP_SSE_GROUP1,

  /* 32-bit only instructions */
  X64CPU_OP_DAA,
  X64CPU_OP_DAS,
  X64CPU_OP_AAA,
  X64CPU_OP_AAM,
  X64CPU_OP_AAD,
  X64CPU_OP_PUSHA,
  X64CPU_OP_POPA,
  X64CPU_OP_BOUND,
  X64CPU_OP_ARPL,
  X64CPU_OP_SETALC,
  X64CPU_OP_LDS,
  X64CPU_OP_LES,
  X64CPU_OP_INTO,
  X64CPU_OP_CALL_FAR,
  X64CPU_OP_JMP_FAR,
  X64CPU_OP_CALL_I_FAR,
  X64CPU_OP_JMP_I_FAR,

};

enum x64cpu_parameter_type {
  X64CPU_PT_NONE = 0,

  X64CPU_PT_1,
  X64CPU_PT_3,

  X64CPU_PT_E,
  X64CPU_PT_G,
  X64CPU_PT_S,
  X64CPU_PT_F,
  X64CPU_PT_M,

  X64CPU_PT_I,
  X64CPU_PT_J,

  X64CPU_PT_O,
  X64CPU_PT_X,
  X64CPU_PT_Y,

  X64CPU_PT_RAX = 0xA00,
  X64CPU_PT_RCX = 0xA01,
  X64CPU_PT_RDX = 0xA02,
  X64CPU_PT_RBX = 0xA03,
  X64CPU_PT_RSP = 0xA04,
  X64CPU_PT_RBP = 0xA05,
  X64CPU_PT_RSI = 0xA06,
  X64CPU_PT_RDI = 0xA07,

  X64CPU_PT_RAX_R8 = 0xB00,
  X64CPU_PT_RCX_R9 = 0xB01,
  X64CPU_PT_RDX_R10 = 0xB02,
  X64CPU_PT_RBX_R11 = 0xB03,
  X64CPU_PT_RSP_R12 = 0xB04,
  X64CPU_PT_RBP_R13 = 0xB05,
  X64CPU_PT_RSI_R14 = 0xB06,
  X64CPU_PT_RDI_R15 = 0xB07,

  X64CPU_PT_RAH_R12 = 0xC04,
  X64CPU_PT_RCH_R13 = 0xC05,
  X64CPU_PT_RDH_R14 = 0xC06,
  X64CPU_PT_RBH_R15 = 0xC07,

  X64CPU_PT_RAH = 0xD04,
  X64CPU_PT_RCH = 0xD05,
  X64CPU_PT_RDH = 0xD06,
  X64CPU_PT_RBH = 0xD07,

  X64CPU_PT_rES = 0xF00,
  X64CPU_PT_rCS = 0xF01,
  X64CPU_PT_rSS = 0xF02,
  X64CPU_PT_rDS = 0xF03,
  X64CPU_PT_rFS = 0xF04,
  X64CPU_PT_rGS = 0xF05,

  X64CPU_PT_ES,  /* FPU */
  X64CPU_PT_EST, /* FPU */
  X64CPU_PT_ST,  /* FPU */

  X64CPU_PT_U, /* MMX or XMM ; depends on prefix */
  X64CPU_PT_V, /* MMX or XMM ; depends on prefix */
  X64CPU_PT_W, /* MMX or XMM ; depends on prefix */

  X64CPU_PT_A,
};

enum x64cpu_parameter_size {
  X64CPU_PS_NONE = 0,

  X64CPU_PS_b,
  X64CPU_PS_bs,
  X64CPU_PS_bss,

  X64CPU_PS_w,
  X64CPU_PS_d,
  X64CPU_PS_q,

  X64CPU_PS_dqp,

  X64CPU_PS_v,
  X64CPU_PS_vds,
  X64CPU_PS_vs,
  X64CPU_PS_vq,
  X64CPU_PS_vqp,
  X64CPU_PS_vqpMw,
  X64CPU_PS_vMw,

  X64CPU_PS_e,   /* FPU - x87 FPU env (14/28 bit) */
  X64CPU_PS_sr,  /* FPU - single real (32bit) */
  X64CPU_PS_er,  /* FPU - extended real (80bit) */
  X64CPU_PS_dr,  /* FPU - double real (64bit) */
  X64CPU_PS_wi,  /* FPU - word int (16bit) */
  X64CPU_PS_di,  /* FPU - double int (32bit) */
  X64CPU_PS_qi,  /* FPU - quad int (64bit) */
  X64CPU_PS_bcd, /* FPU - bcd format (80bit?) */

  X64CPU_PS_dq, /* Double-quad word / 128bit */

  X64CPU_PS_ps, /* SSE - Packed single-precision */
  X64CPU_PS_pd, /* SSE - Packed double-precision */
  X64CPU_PS_ss, /* SSE - Scalar single-precision */
  X64CPU_PS_sd, /* SSE - Scalar double-precision */

  X64CPU_PS_a,
  X64CPU_PS_p,
};

enum x64cpu_prefix_flags {
  X64CPU_PREFIX_NONE = 0,
  X64CPU_PREFIX_REX_W = 0x01,
  X64CPU_PREFIX_REX_R = 0x02,
  X64CPU_PREFIX_REX_X = 0x04,
  X64CPU_PREFIX_REX_B = 0x08,
  X64CPU_PREFIX_REX = 0x10,

  X64CPU_PREFIX_OP_SIZE = 0x20,
  X64CPU_PREFIX_ADDR_SIZE = 0x40,

  X64CPU_PREFIX_FS = 0x100,
  X64CPU_PREFIX_GS = 0x200,

  X64CPU_PREFIX_REPEAT_REPZ = 0x1000,
  X64CPU_PREFIX_REPEAT_REPNZ = 0x2000,

  X64CPU_PREFIX_LOCK = 0x10000,
  X64CPU_PREFIX_FWAIT = 0x20000,

  X64CPU_PREFIX_NULL = 0x8000000
};

enum x64cpu_instruction_set {
  X64CPU_INSTR_SET_GENERAL = 0,
  X64CPU_INSTR_SET_FPU,
  X64CPU_INSTR_SET_SSE,

  X64CPU_INSTR_SET_SSE2 /* TODO: remove this */
};

struct x64cpu_opcode_definition {
  int operation;

  int need_modrmbyte;

  struct {
    int type;
    int size;
    int hide;
  } parameters[4];

  /* If the operation is given by the register bits from the modrmbyte */
  x64cpu_opcode_definition *group;
};

extern const struct x64cpu_opcode_definition x64cpu_opcode_def_1byte[256];

extern const struct x64cpu_opcode_definition x64cpu_opcode_def_0F_2byte[256];

/* 32-bit instructions */
extern const struct x64cpu_opcode_definition x64cpu_opcode_32_def_1byte[256];

/* 1-byte opcodes */
const struct x64cpu_opcode_definition x64cpu_opcode_def_1byte[] = {
    /* 0x0_ */
    /* 0x00 */ {X64CPU_OP_ADD,
                1,
                {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x01 */
    {X64CPU_OP_ADD,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x02 */
    {X64CPU_OP_ADD,
     1,
     {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x03 */
    {X64CPU_OP_ADD,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x04 */
    {X64CPU_OP_ADD,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0x05 */
    {X64CPU_OP_ADD,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0x06 */ {X64CPU_OP_INVALID},
    /* 0x07 */ {X64CPU_OP_INVALID},
    /* 0x0_ */
    /* 0x08 */
    {X64CPU_OP_OR, 1, {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x09 */
    {X64CPU_OP_OR,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x0A */
    {X64CPU_OP_OR, 1, {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x0B */
    {X64CPU_OP_OR,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x0C */
    {X64CPU_OP_OR,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0x0D */
    {X64CPU_OP_OR,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0x0E */ {X64CPU_OP_INVALID},
    /* 0x0F */ {X64CPU_OP_INVALID},

    /* 0x1_ */
    /* 0x10 */
    {X64CPU_OP_ADC,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x11 */
    {X64CPU_OP_ADC,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x12 */
    {X64CPU_OP_ADC,
     1,
     {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x13 */
    {X64CPU_OP_ADC,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x14 */
    {X64CPU_OP_ADC,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0x15 */
    {X64CPU_OP_ADC,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0x16 */ {X64CPU_OP_INVALID},
    /* 0x17 */ {X64CPU_OP_INVALID},
    /* 0x1_ */
    /* 0x18 */
    {X64CPU_OP_SBB,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x19 */
    {X64CPU_OP_SBB,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x1A */
    {X64CPU_OP_SBB,
     1,
     {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x1B */
    {X64CPU_OP_SBB,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x1C */
    {X64CPU_OP_SBB,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0x1D */
    {X64CPU_OP_SBB,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0x1E */ {X64CPU_OP_INVALID},
    /* 0x1F */ {X64CPU_OP_INVALID},

    /* 0x2_ */
    /* 0x20 */
    {X64CPU_OP_AND,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x21 */
    {X64CPU_OP_AND,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x22 */
    {X64CPU_OP_AND,
     1,
     {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x23 */
    {X64CPU_OP_AND,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x24 */
    {X64CPU_OP_AND,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0x25 */
    {X64CPU_OP_AND,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0x26 */ {X64CPU_OP_INVALID},
    /* 0x27 */ {X64CPU_OP_INVALID},
    /* 0x2_ */
    /* 0x28 */
    {X64CPU_OP_SUB,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x29 */
    {X64CPU_OP_SUB,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x2A */
    {X64CPU_OP_SUB,
     1,
     {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x2B */
    {X64CPU_OP_SUB,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x2C */
    {X64CPU_OP_SUB,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0x2D */
    {X64CPU_OP_SUB,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0x2E */ {X64CPU_OP_INVALID},
    /* 0x2F */ {X64CPU_OP_INVALID},

    /* 0x3_ */
    /* 0x30 */
    {X64CPU_OP_XOR,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x31 */
    {X64CPU_OP_XOR,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x32 */
    {X64CPU_OP_XOR,
     1,
     {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x33 */
    {X64CPU_OP_XOR,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x34 */
    {X64CPU_OP_XOR,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0x35 */
    {X64CPU_OP_XOR,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0x36 */ {X64CPU_OP_INVALID},
    /* 0x37 */ {X64CPU_OP_INVALID},
    /* 0x3_ */
    /* 0x38 */
    {X64CPU_OP_CMP,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x39 */
    {X64CPU_OP_CMP,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x3A */
    {X64CPU_OP_CMP,
     1,
     {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x3B */
    {X64CPU_OP_CMP,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x3C */
    {X64CPU_OP_CMP,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0x3D */
    {X64CPU_OP_CMP,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0x3E */ {X64CPU_OP_INVALID},
    /* 0x3F */ {X64CPU_OP_INVALID},

    /* 0x4_ */ /* 0x4_ - REX Flags */
    /* 0x40 */ {X64CPU_OP_PREFIX},
    /* 0x41 */ {X64CPU_OP_PREFIX},
    /* 0x42 */ {X64CPU_OP_PREFIX},
    /* 0x43 */ {X64CPU_OP_PREFIX},
    /* 0x44 */ {X64CPU_OP_PREFIX},
    /* 0x45 */ {X64CPU_OP_PREFIX},
    /* 0x46 */ {X64CPU_OP_PREFIX},
    /* 0x47 */ {X64CPU_OP_PREFIX},
    /* 0x4_ */
    /* 0x48 */ {X64CPU_OP_PREFIX},
    /* 0x49 */ {X64CPU_OP_PREFIX},
    /* 0x4A */ {X64CPU_OP_PREFIX},
    /* 0x4B */ {X64CPU_OP_PREFIX},
    /* 0x4C */ {X64CPU_OP_PREFIX},
    /* 0x4D */ {X64CPU_OP_PREFIX},
    /* 0x4E */ {X64CPU_OP_PREFIX},
    /* 0x4F */ {X64CPU_OP_PREFIX},

    /* 0x5_ */
    /* 0x50 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_RAX_R8, X64CPU_PS_vq}}},
    /* 0x51 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_RCX_R9, X64CPU_PS_vq}}},
    /* 0x52 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_RDX_R10, X64CPU_PS_vq}}},
    /* 0x53 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_RBX_R11, X64CPU_PS_vq}}},
    /* 0x54 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_RSP_R12, X64CPU_PS_vq}}},
    /* 0x55 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_RBP_R13, X64CPU_PS_vq}}},
    /* 0x56 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_RSI_R14, X64CPU_PS_vq}}},
    /* 0x57 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_RDI_R15, X64CPU_PS_vq}}},
    /* 0x5_ */
    /* 0x58 */ {X64CPU_OP_POP, 0, {{X64CPU_PT_RAX_R8, X64CPU_PS_vq}}},
    /* 0x59 */ {X64CPU_OP_POP, 0, {{X64CPU_PT_RCX_R9, X64CPU_PS_vq}}},
    /* 0x5A */ {X64CPU_OP_POP, 0, {{X64CPU_PT_RDX_R10, X64CPU_PS_vq}}},
    /* 0x5B */ {X64CPU_OP_POP, 0, {{X64CPU_PT_RBX_R11, X64CPU_PS_vq}}},
    /* 0x5C */ {X64CPU_OP_POP, 0, {{X64CPU_PT_RSP_R12, X64CPU_PS_vq}}},
    /* 0x5D */ {X64CPU_OP_POP, 0, {{X64CPU_PT_RBP_R13, X64CPU_PS_vq}}},
    /* 0x5E */ {X64CPU_OP_POP, 0, {{X64CPU_PT_RSI_R14, X64CPU_PS_vq}}},
    /* 0x5F */ {X64CPU_OP_POP, 0, {{X64CPU_PT_RDI_R15, X64CPU_PS_vq}}},

    /* 0x6_ */
    /* 0x60 */ {X64CPU_OP_INVALID},
    /* 0x61 */ {X64CPU_OP_INVALID},
    /* 0x62 */ {X64CPU_OP_INVALID},
    /* 0x63 */
    {X64CPU_OP_MOVSXD,
     1,
     {{X64CPU_PT_G, X64CPU_PS_dqp}, {X64CPU_PT_E, X64CPU_PS_d}}},
    /* 0x64 */ {X64CPU_OP_PREFIX}, /* FS prefix */
    /* 0x65 */ {X64CPU_OP_PREFIX}, /* GS prefix */
    /* 0x66 */ {X64CPU_OP_PREFIX}, /* Operand-size prefix */
    /* 0x67 */ {X64CPU_OP_PREFIX}, /* Address-size prefix */
                                   /* 0x6_ */
    /* 0x68 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_I, X64CPU_PS_vs}}},
    /* 0x69 */
    {X64CPU_OP_IMUL3,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp},
      {X64CPU_PT_E, X64CPU_PS_vqp},
      {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0x6A */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_I, X64CPU_PS_bss}}},
    /* 0x6B */
    {X64CPU_OP_IMUL3,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp},
      {X64CPU_PT_E, X64CPU_PS_vqp},
      {X64CPU_PT_I, X64CPU_PS_bs}}},
    /* 0x6C */
    {X64CPU_OP_INS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_b}, {X64CPU_PT_RDX, X64CPU_PS_w}}},
    /* 0x6D */
    {X64CPU_OP_INS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_v}, {X64CPU_PT_RDX, X64CPU_PS_w}}},
    /* 0x6E */
    {X64CPU_OP_OUTS,
     0,
     {{X64CPU_PT_RDX, X64CPU_PS_w}, {X64CPU_PT_X, X64CPU_PS_b}}},
    /* 0x6F */
    {X64CPU_OP_OUTS,
     0,
     {{X64CPU_PT_RDX, X64CPU_PS_w}, {X64CPU_PT_X, X64CPU_PS_v}}},

    /* 0x7_ */
    /* 0x70 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x71 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x72 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x73 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x74 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x75 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x76 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x77 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x7_ */
    /* 0x78 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x79 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x7A */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x7B */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x7C */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x7D */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x7E */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},
    /* 0x7F */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_b}}},

    /* 0x8_ */
    /* 0x80 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_ADD,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_OR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_ADC,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SBB,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_AND,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SUB,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_XOR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_CMP,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
     }},
    /* 0x81 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_ADD,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
         {X64CPU_OP_OR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
         {X64CPU_OP_ADC,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
         {X64CPU_OP_SBB,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
         {X64CPU_OP_AND,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
         {X64CPU_OP_SUB,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
         {X64CPU_OP_XOR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
         {X64CPU_OP_CMP,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
     }},
    /* 0x82 */ {X64CPU_OP_INVALID},
    /* 0x83 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_ADD,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_bs}}},
         {X64CPU_OP_OR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_bs}}},
         {X64CPU_OP_ADC,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_bs}}},
         {X64CPU_OP_SBB,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_bs}}},
         {X64CPU_OP_AND,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_bs}}},
         {X64CPU_OP_SUB,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_bs}}},
         {X64CPU_OP_XOR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_bs}}},
         {X64CPU_OP_CMP,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_bs}}},
     }},
    /* 0x84 */
    {X64CPU_OP_TEST,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x85 */
    {X64CPU_OP_TEST,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x86 */
    {X64CPU_OP_XCHG,
     1,
     {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x87 */
    {X64CPU_OP_XCHG,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x8_ */
    /* 0x88 */
    {X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0x89 */
    {X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0x8A */
    {X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_b}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x8B */
    {X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x8C */
    {X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqpMw}, {X64CPU_PT_S, X64CPU_PS_w}}},
    /* 0x8D */
    {X64CPU_OP_LEA,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_M, X64CPU_PS_vqp}}},
    /* 0x8E */
    {X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_S, X64CPU_PS_w}, {X64CPU_PT_E, X64CPU_PS_w}}},
    /* 0x8F */ {X64CPU_OP_POP, 1, {{X64CPU_PT_E, X64CPU_PS_vq}}},

    /* 0x9_ */
    /* 0x90 */ {X64CPU_OP_NOOP},
    /* 0x91 */
    {X64CPU_OP_XCHG,
     0,
     {{X64CPU_PT_RCX_R9, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},
    /* 0x92 */
    {X64CPU_OP_XCHG,
     0,
     {{X64CPU_PT_RDX_R10, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},
    /* 0x93 */
    {X64CPU_OP_XCHG,
     0,
     {{X64CPU_PT_RBX_R11, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},
    /* 0x94 */
    {X64CPU_OP_XCHG,
     0,
     {{X64CPU_PT_RSP_R12, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},
    /* 0x95 */
    {X64CPU_OP_XCHG,
     0,
     {{X64CPU_PT_RBP_R13, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},
    /* 0x96 */
    {X64CPU_OP_XCHG,
     0,
     {{X64CPU_PT_RSI_R14, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},
    /* 0x97 */
    {X64CPU_OP_XCHG,
     0,
     {{X64CPU_PT_RDI_R15, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},
    /* 0x9_ */
    /* 0x98 */ {X64CPU_OP_CONV},
    /* 0x99 */ {X64CPU_OP_CONV2},
    /* 0x9A */ {X64CPU_OP_INVALID},
    /* 0x9B */ {X64CPU_OP_PREFIX}, /* FWAIT prefix */
    /* 0x9C */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_F, X64CPU_PS_vqp}}},
    /* 0x9D */ {X64CPU_OP_POP, 0, {{X64CPU_PT_F, X64CPU_PS_vqp}}},
    /* 0x9E */ {X64CPU_OP_SAHF},
    /* 0x9F */ {X64CPU_OP_LAHF},

    /* 0xA_ */
    /* 0xA0 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_O, X64CPU_PS_b}}},
    /* 0xA1 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_O, X64CPU_PS_vqp}}},
    /* 0xA2 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_O, X64CPU_PS_b}, {X64CPU_PT_RAX, X64CPU_PS_b}}},
    /* 0xA3 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_O, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},
    /* 0xA4 */
    {X64CPU_OP_MOVS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_b}, {X64CPU_PT_X, X64CPU_PS_b}}},
    /* 0xA5 */
    {X64CPU_OP_MOVS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_vqp}, {X64CPU_PT_X, X64CPU_PS_vqp}}},
    /* 0xA6 */
    {X64CPU_OP_CMPS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_b}, {X64CPU_PT_X, X64CPU_PS_b}}},
    /* 0xA7 */
    {X64CPU_OP_CMPS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_vqp}, {X64CPU_PT_X, X64CPU_PS_vqp}}},
    /* 0xA_ */
    /* 0xA8 */
    {X64CPU_OP_TEST,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xA9 */
    {X64CPU_OP_TEST,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0xAA */
    {X64CPU_OP_STOS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_b}, {X64CPU_PT_RAX, X64CPU_PS_b}}},
    /* 0xAB */
    {X64CPU_OP_STOS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},
    /* 0xAC */
    {X64CPU_OP_LODS,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_X, X64CPU_PS_b}}},
    /* 0xAD */
    {X64CPU_OP_LODS,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_vqp}, {X64CPU_PT_X, X64CPU_PS_vqp}}},
    /* 0xAE */
    {X64CPU_OP_SCAS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_b}, {X64CPU_PT_RAX, X64CPU_PS_b}}},
    /* 0xAF */
    {X64CPU_OP_SCAS,
     0,
     {{X64CPU_PT_Y, X64CPU_PS_vqp}, {X64CPU_PT_RAX, X64CPU_PS_vqp}}},

    /* 0xB_ */
    /* 0xB0 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RAX_R8, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xB1 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RCX_R9, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xB2 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RDX_R10, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xB3 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RBX_R11, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xB4 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RAH_R12, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xB5 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RCH_R13, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xB6 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RDH_R14, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xB7 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RBH_R15, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xB_ */
    /* 0xB8 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RAX_R8, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vqp}}},
    /* 0xB9 */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RCX_R9, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vqp}}},
    /* 0xBA */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RDX_R10, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vqp}}},
    /* 0xBB */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RBX_R11, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vqp}}},
    /* 0xBC */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RSP_R12, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vqp}}},
    /* 0xBD */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RBP_R13, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vqp}}},
    /* 0xBE */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RSI_R14, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vqp}}},
    /* 0xBF */
    {X64CPU_OP_MOV,
     0,
     {{X64CPU_PT_RDI_R15, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vqp}}},

    /* 0xC_ */
    /* 0xC0 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_ROL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_ROR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_RCL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_RCR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SHR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SAR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
     }},
    /* 0xC1 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_ROL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_ROR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_RCL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_RCR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SHR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_SAR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
     }},
    /* 0xC2 */ {X64CPU_OP_RETN, 0, {{X64CPU_PT_I, X64CPU_PS_w}}},
    /* 0xC3 */
    {
        X64CPU_OP_RETN,
        0,
    },
    /* 0xC4 */ {X64CPU_OP_INVALID}, /* TODO: VEX3 */
    /* 0xC5 */ {X64CPU_OP_INVALID}, /* TODO: VEX2 */
                                    /* 0xC6 */
    {X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xC7 */
    {X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
    /* 0xC_ */
    /* 0xC8 */
    {X64CPU_OP_ENTER,
     0,
     {{X64CPU_PT_I, X64CPU_PS_w}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xC9 */ {X64CPU_OP_LEAVE, 0},
    /* 0xCA */ {X64CPU_OP_RETF, 0, {{X64CPU_PT_I, X64CPU_PS_w}}},
    /* 0xCB */ {X64CPU_OP_RETF, 0},
    /* 0xCC */ {X64CPU_OP_INT, 0, {{X64CPU_PT_3, X64CPU_PS_b}}},
    /* 0xCD */ {X64CPU_OP_INT, 0, {{X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xCE */ {X64CPU_OP_INVALID}, /* TODO: INTO instruction on 64 bit ? */
    /* 0xCF */ {X64CPU_OP_IRET, 0},

    /* 0xD_ */
    /* 0xD0 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_ROL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_ROR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_RCL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_RCR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_SHR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_SAR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_1, X64CPU_PS_b}}},
     }},
    /* 0xD1 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_ROL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_ROR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_RCL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_RCR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_SHR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_1, X64CPU_PS_b}}},
         {X64CPU_OP_SAR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_1, X64CPU_PS_b}}},
     }},
    /* 0xD2 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_ROL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_ROR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_RCL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_RCR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_SHR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_SAR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
     }},
    /* 0xD3 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_ROL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_ROR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_RCL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_RCR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_SHR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_SHL,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
         {X64CPU_OP_SAR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_RCX, X64CPU_PS_b}}},
     }},
    /* 0xD4 */ {X64CPU_OP_INVALID},
    /* 0xD5 */ {X64CPU_OP_INVALID},
    /* 0xD6 */ {X64CPU_OP_INVALID},
    /* 0xD7 */ {X64CPU_OP_XLAT, 0},
    /* 0xD_ */
    /* 0xD8 */ {X64CPU_OP_FPU, 1},
    /* 0xD9 */ {X64CPU_OP_FPU, 1},
    /* 0xDA */ {X64CPU_OP_FPU, 1},
    /* 0xDB */ {X64CPU_OP_FPU, 1},
    /* 0xDC */ {X64CPU_OP_FPU, 1},
    /* 0xDD */ {X64CPU_OP_FPU, 1},
    /* 0xDE */ {X64CPU_OP_FPU, 1},
    /* 0xDF */ {X64CPU_OP_FPU, 1},

    /* 0xE_ */
    /* 0xE0 */ {X64CPU_OP_LOOPNE, 0, {{X64CPU_PT_J, X64CPU_PS_bs}}},
    /* 0xE1 */ {X64CPU_OP_LOOPE, 0, {{X64CPU_PT_J, X64CPU_PS_bs}}},
    /* 0xE2 */ {X64CPU_OP_LOOP, 0, {{X64CPU_PT_J, X64CPU_PS_bs}}},
    /* 0xE3 */
    {X64CPU_OP_JRCX,
     0,
     {{X64CPU_PT_J, X64CPU_PS_bs}, {X64CPU_PT_RCX, X64CPU_PS_vqp}}},
    /* 0xE4 */
    {X64CPU_OP_IN,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xE5 */
    {X64CPU_OP_IN,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_d}, {X64CPU_PT_I, X64CPU_PS_b}}},
    /* 0xE6 */
    {X64CPU_OP_OUT,
     0,
     {{X64CPU_PT_I, X64CPU_PS_b}, {X64CPU_PT_RAX, X64CPU_PS_b}}},
    /* 0xE7 */
    {X64CPU_OP_OUT,
     0,
     {{X64CPU_PT_I, X64CPU_PS_b}, {X64CPU_PT_RAX, X64CPU_PS_d}}},
    /* 0xE_ */
    /* 0xE8 */ {X64CPU_OP_CALL, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0xE9 */ {X64CPU_OP_JMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0xEA */ {X64CPU_OP_INVALID},
    /* 0xEB */ {X64CPU_OP_JMP, 0, {{X64CPU_PT_J, X64CPU_PS_bs}}},
    /* 0xEC */
    {X64CPU_OP_IN,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_b}, {X64CPU_PT_RDX, X64CPU_PS_w}}},
    /* 0xED */
    {X64CPU_OP_IN,
     0,
     {{X64CPU_PT_RAX, X64CPU_PS_d}, {X64CPU_PT_RDX, X64CPU_PS_w}}},
    /* 0xEE */
    {X64CPU_OP_OUT,
     0,
     {{X64CPU_PT_RDX, X64CPU_PS_w}, {X64CPU_PT_RAX, X64CPU_PS_b}}},
    /* 0xEF */
    {X64CPU_OP_OUT,
     0,
     {{X64CPU_PT_RDX, X64CPU_PS_w}, {X64CPU_PT_RAX, X64CPU_PS_d}}},

    /* 0xF_ */
    /* 0xF0 */ {X64CPU_OP_PREFIX}, /* LOCK: */
    /* 0xF1 */ {X64CPU_OP_INT, 0, {{X64CPU_PT_1, X64CPU_PS_b}}},
    /* 0xF2 */ {X64CPU_OP_PREFIX}, /* REPNE: */
    /* 0xF3 */ {X64CPU_OP_PREFIX}, /* REPE: */
    /* 0xF4 */ {X64CPU_OP_HLT},
    /* 0xF5 */ {X64CPU_OP_CMC},
    /* 0xF6 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_TEST,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_TEST,
          1,
          {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_NOT, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
         {X64CPU_OP_NEG, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
         {X64CPU_OP_MUL,
          1,
          {{X64CPU_PT_RAX, X64CPU_PS_w, .hide = 1},
           {X64CPU_PT_RAX, X64CPU_PS_b, .hide = 1},
           {X64CPU_PT_E, X64CPU_PS_b}}},
         {X64CPU_OP_IMUL,
          1,
          {{X64CPU_PT_RAX, X64CPU_PS_w, .hide = 1},
           {X64CPU_PT_RAX, X64CPU_PS_b, .hide = 1},
           {X64CPU_PT_E, X64CPU_PS_b}}},
         {X64CPU_OP_DIV,
          1,
          {{X64CPU_PT_RAX, X64CPU_PS_b, .hide = 1},
           {X64CPU_PT_RAH, X64CPU_PS_b, .hide = 1},
           {X64CPU_PT_RAX, X64CPU_PS_w, .hide = 1},
           {X64CPU_PT_E, X64CPU_PS_b}}},
         {X64CPU_OP_IDIV,
          1,
          {{X64CPU_PT_RAX, X64CPU_PS_b, .hide = 1},
           {X64CPU_PT_RAH, X64CPU_PS_b, .hide = 1},
           {X64CPU_PT_RAX, X64CPU_PS_w, .hide = 1},
           {X64CPU_PT_E, X64CPU_PS_b}}},
     }},
    /* 0xF7 */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_TEST,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
         {X64CPU_OP_TEST,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_vds}}},
         {X64CPU_OP_NOT, 1, {{X64CPU_PT_E, X64CPU_PS_vqp}}},
         {X64CPU_OP_NEG, 1, {{X64CPU_PT_E, X64CPU_PS_vqp}}},
         {X64CPU_OP_MUL1,
          1,
          {{X64CPU_PT_RAX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_RDX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_E, X64CPU_PS_vqp}}},
         {X64CPU_OP_IMUL1,
          1,
          {{X64CPU_PT_RAX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_RDX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_E, X64CPU_PS_vqp}}},
         {X64CPU_OP_DIV,
          1,
          {{X64CPU_PT_RAX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_RDX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_RAX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_E, X64CPU_PS_vqp}}},
         {X64CPU_OP_IDIV,
          1,
          {{X64CPU_PT_RAX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_RDX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_RAX, X64CPU_PS_vqp, .hide = 1},
           {X64CPU_PT_E, X64CPU_PS_vqp}}},
     }},
    /* 0xF_ */
    /* 0xF8 */ {X64CPU_OP_CLC},
    /* 0xF9 */ {X64CPU_OP_STC},
    /* 0xFA */ {X64CPU_OP_CLI},
    /* 0xFB */ {X64CPU_OP_STI},
    /* 0xFC */ {X64CPU_OP_CLD},
    /* 0xFD */ {X64CPU_OP_STD},
    /* 0xFE */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_INC, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
         {X64CPU_OP_DEC, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
     }},
    /* 0xFF */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {X64CPU_OP_INC, 1, {{X64CPU_PT_E, X64CPU_PS_vqp}}},
         {X64CPU_OP_DEC, 1, {{X64CPU_PT_E, X64CPU_PS_vqp}}},
         {X64CPU_OP_CALL_I, 1, {{X64CPU_PT_E, X64CPU_PS_q}}},
         {X64CPU_OP_CALLF_I, 1, {{X64CPU_PT_M, X64CPU_PS_vq}}},
         {X64CPU_OP_JMP_I, 1, {{X64CPU_PT_E, X64CPU_PS_q}}},
         {X64CPU_OP_JMPF_I, 1, {{X64CPU_PT_M, X64CPU_PS_vq}}},
         {X64CPU_OP_PUSH, 1, {{X64CPU_PT_E, X64CPU_PS_vq}}},
         {X64CPU_OP_INVALID},
     }},
};

/* 0x0F prefix - 2 byte opcodes */
const struct x64cpu_opcode_definition x64cpu_opcode_def_0F_2byte[] = {
    /* 0x0_ */
    /* 0x00 */ {X64CPU_OP_TODO},    /* Group #6 */
    /* 0x01 */ {X64CPU_OP_TODO},    /* Group #7 */
    /* 0x02 */ {X64CPU_OP_TODO},    /* LAR Gv, Ew */
    /* 0x03 */ {X64CPU_OP_TODO},    /* LSL Gv, Ew */
    /* 0x04 */ {X64CPU_OP_TODO},    /* Nothing ??? */
    /* 0x05 */ {X64CPU_OP_SYSCALL}, /* SYSCALL RCX, R11, SS, ... */
    /* 0x06 */ {X64CPU_OP_TODO}, /* CTLS CR0 - clear task-switched flag in CR0
                                  */
    /* 0x07 */ {X64CPU_OP_TODO}, /* SYSRET SS, Fd, R11, ... */
                                 /* 0x0_ */
    /* 0x08 */ {X64CPU_OP_TODO}, /* INVD - invalidate internal caches */
    /* 0x09 */ {X64CPU_OP_TODO}, /* WBIND - write back and invalidate caches */
    /* 0x0A */ {X64CPU_OP_TODO}, /* CL1INVMB ??? */
    /* 0x0B */ {X64CPU_OP_TODO}, /* UD1 / UD2 ??? */
    /* 0x0C */ {X64CPU_OP_TODO}, /* UD1 / UD2 ??? */
    /* 0x0D */ {X64CPU_OP_NOOP, 1, {{X64CPU_PT_E, X64CPU_PS_v}}}, /* NOOP Ev ???
                                                                   */
    /* 0x0E */ {X64CPU_OP_TODO}, /* 3DNow! ??? */
    /* 0x0F */ {X64CPU_OP_TODO}, /* 3DNow! ??? */

    /* 0x1_ */
    /* 0x10 */ {X64CPU_OP_TODO},    /* sse group, removed */
    /* 0x11 */ {X64CPU_OP_TODO},    /* sse group, removed */
    /* 0x12 */ {X64CPU_OP_TODO},    /* sse group, removed */
    /* 0x13 */ {X64CPU_OP_TODO},    /* sse group, removed */
    /* 0x14 */ {X64CPU_OP_TODO},    /* sse group, removed */
    /* 0x15 */ {X64CPU_OP_TODO},    /* sse group, removed */
    /* 0x16 */ {X64CPU_OP_TODO},    /* sse group, removed */
    /* 0x17 */ {X64CPU_OP_TODO},    /* sse group, removed */
                                    /* 0x1_ */
    /* 0x18 */ {X64CPU_OP_TODO},    /* Group 16 */
    /* 0x19 */ {X64CPU_OP_NOOP},    /* Hintable noop; Group 16 */
    /* 0x1A */ {X64CPU_OP_NOOP},    /* Hintable noop; Group 16 */
    /* 0x1B */ {X64CPU_OP_NOOP},    /* Hintable noop; Group 16 */
    /* 0x1C */ {X64CPU_OP_NOOP},    /* Hintable noop; Group 16 */
    /* 0x1D */ {X64CPU_OP_NOOP},    /* Hintable noop; Group 16 */
    /* 0x1E */ {X64CPU_OP_NOOP},    /* Hintable noop; Group 16 */
    /* 0x1F */ {X64CPU_OP_NOOP, 1}, /* Hintable noop; Group 16 */

    /* 0x2_ */
    /* 0x20 */ {X64CPU_OP_TODO}, /* MOV Rq, Cq - MOV Control Registers */
    /* 0x21 */ {X64CPU_OP_TODO}, /* MOV Rq, Cq - MOV Control Registers */
    /* 0x22 */ {X64CPU_OP_TODO}, /* MOV Rq, Cq - MOV Control Registers */
    /* 0x23 */ {X64CPU_OP_TODO}, /* MOV Rq, Cq - MOV Control Registers */
    /* 0x24 */ {X64CPU_OP_TODO}, /* MOV Rq, Cq - MOV Control Registers */
    /* 0x25 */ {X64CPU_OP_INVALID},
    /* 0x26 */ {X64CPU_OP_TODO}, /* MOV Rq, Cq - MOV Control Registers */
    /* 0x27 */ {X64CPU_OP_INVALID},
    /* 0x2_ */
    /* 0x28 */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0x29 */ {X64CPU_OP_TODO}, /* MOVA, etc - SSE2 */
    /* 0x2A */ {X64CPU_OP_TODO}, /* MOVA, etc - SSE2 */
    /* 0x2B */ {X64CPU_OP_TODO}, /* MOVA, etc - SSE2 */
    /* 0x2C */ {X64CPU_OP_TODO}, /* MOVA, etc - SSE2 */
    /* 0x2D */ {X64CPU_OP_TODO}, /* MOVA, etc - SSE2 */
    /* 0x2E */ {X64CPU_OP_TODO}, /* MOVA, etc - SSE2 */
    /* 0x2F */ {X64CPU_OP_TODO}, /* MOVA, etc - SSE2 */

    /* 0x3_ */
    /* 0x30 */ {X64CPU_OP_TODO},  /* WRMSR MSR, rCX, rAX, rDX */
    /* 0x31 */ {X64CPU_OP_RDTSC}, /* RDTSC */
    /* 0x32 */ {X64CPU_OP_TODO},  /* RDMSR MSR, rCX, rAX, rDX */
    /* 0x33 */ {X64CPU_OP_TODO},  /* RDPMC */
    /* 0x34 */ {X64CPU_OP_TODO},  /* SYSENTER SS, ESP, ... */
    /* 0x35 */ {X64CPU_OP_TODO},  /* SYSEXIT SS, ESP, ... */
    /* 0x36 */ {X64CPU_OP_INVALID},
    /* 0x37 */ {X64CPU_OP_TODO}, /* GETSEC - smx */
                                 /* 0x3_ */
    /* 0x38 */ {X64CPU_OP_TODO}, /* ssse3 group */
    /* 0x39 */ {X64CPU_OP_INVALID},
    /* 0x3A */ {X64CPU_OP_TODO}, /* ssse41 group */
    /* 0x3B */ {X64CPU_OP_INVALID},
    /* 0x3C */ {X64CPU_OP_INVALID},
    /* 0x3D */ {X64CPU_OP_INVALID},
    /* 0x3E */ {X64CPU_OP_INVALID},
    /* 0x3F */ {X64CPU_OP_INVALID},

    /* 0x4_ */
    /* 0x40 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x41 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x42 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x43 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x44 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x45 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x46 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x47 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x4_ */
    /* 0x48 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x49 */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x4A */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x4B */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x4C */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x4D */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x4E */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0x4F */
    {X64CPU_OP_CMOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},

    /* 0x5_ */
    /* 0x50 */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x51 */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x52 */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x53 */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x54 */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x55 */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x56 */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x57 */ {X64CPU_OP_TODO}, /* sse1 group */
                                 /* 0x5_ */
    /* 0x58 */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x59 */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x5A */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x5B */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x5C */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x5D */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x5E */ {X64CPU_OP_TODO}, /* sse1 group */
    /* 0x5F */ {X64CPU_OP_TODO}, /* sse1 group */

    /* 0x6_ */
    /* 0x60 */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0x61 */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0x62 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x63 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x64 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x65 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x66 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x67 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
                                 /* 0x6_ */
    /* 0x68 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x69 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x6A */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x6B */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x6C */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x6D */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x6E */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0x6F */ {X64CPU_OP_TODO}, /* sse group, removed */

    /* 0x7_ */
    /* 0x70 */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0x71 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x72 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x73 */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0x74 */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0x75 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x76 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x77 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
                                 /* 0x7_ */
    /* 0x78 */ {X64CPU_OP_TODO}, /* VMREAD / VMWRITE */
    /* 0x79 */ {X64CPU_OP_TODO}, /* VMREAD / VMWRITE */
    /* 0x7A */ {X64CPU_OP_INVALID},
    /* 0x7B */ {X64CPU_OP_INVALID},
    /* 0x7C */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x7D */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0x7E */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0x7F */ {X64CPU_OP_TODO}, /* sse group, removed */

    /* 0x8_ */
    /* 0x80 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x81 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x82 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x83 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x84 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x85 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x86 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x87 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x8_ */
    /* 0x88 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x89 */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x8A */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x8B */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x8C */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x8D */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x8E */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},
    /* 0x8F */ {X64CPU_OP_CJMP, 0, {{X64CPU_PT_J, X64CPU_PS_vds}}},

    /* 0x9_ */
    /* 0x90 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x91 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x92 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x93 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x94 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x95 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x96 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x97 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x9_ */
    /* 0x98 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x99 */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x9A */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x9B */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x9C */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x9D */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x9E */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0x9F */ {X64CPU_OP_CSET, 1, {{X64CPU_PT_E, X64CPU_PS_b}}},

    /* 0xA_ */
    /* 0xA0 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_rFS, X64CPU_PS_w}}},
    /* 0xA1 */ {X64CPU_OP_POP, 0, {{X64CPU_PT_rFS, X64CPU_PS_w}}},
    /* 0xA2 */ {X64CPU_OP_CPUID, 0},
    /* 0xA3 */
    {X64CPU_OP_BT,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0xA4 */ {X64CPU_OP_TODO}, /* SHLD Evqp, Gvqp, Ib */
    /* 0xA5 */ {X64CPU_OP_TODO}, /* SHLD Evqp, Gvqp, CL */
    /* 0xA6 */ {X64CPU_OP_INVALID},
    /* 0xA7 */ {X64CPU_OP_INVALID},
    /* 0xA_ */
    /* 0xA8 */ {X64CPU_OP_PUSH, 0, {{X64CPU_PT_rGS, X64CPU_PS_w}}},
    /* 0xA9 */ {X64CPU_OP_POP, 0, {{X64CPU_PT_rGS, X64CPU_PS_w}}},
    /* 0xAA */ {X64CPU_OP_TODO}, /* RSM */
                                 /* 0xAB */
    {X64CPU_OP_BTS,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0xAC */ {X64CPU_OP_TODO}, /* SHRD Evqp, Gvqp, Ib */
    /* 0xAD */ {X64CPU_OP_TODO}, /* SHRD Evqp, Gvqp, CL */
    /* 0xAE */ {X64CPU_OP_TODO}, /* save restore state, MXCSR, etc group */
                                 /* 0xAF */
    {X64CPU_OP_IMUL,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp},
      {X64CPU_PT_G, X64CPU_PS_vqp},
      {X64CPU_PT_E, X64CPU_PS_vqp}}},

    /* 0xB_ */
    /* 0xB0 */
    {X64CPU_OP_CMPXCHG,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b},
      {X64CPU_PT_RAX, X64CPU_PS_b},
      {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0xB1 */
    {X64CPU_OP_CMPXCHG,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp},
      {X64CPU_PT_RAX, X64CPU_PS_vqp},
      {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0xB2 */ {X64CPU_OP_TODO}, /* LSS SS, Gvqp, Mptp */
                                 /* 0xB3 */
    {X64CPU_OP_BTR,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0xB4 */ {X64CPU_OP_TODO}, /* LFS FS, Gvqp, Mptp */
    /* 0xB5 */ {X64CPU_OP_TODO}, /* LGS GS, Gvqp, Mptp */
                                 /* 0xB6 */
    {/* X64CPU_OP_MOVZX */ X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0xB7 */
    {/* X64CPU_OP_MOVZX */ X64CPU_OP_MOV,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_w}}},
    /* 0xB_ */
    /* 0xB8 */ {X64CPU_OP_TODO}, /* JMPE - jump to ia-64 instruction set */
    /* 0xB9 */ {X64CPU_OP_TODO}, /* UD ? */
                                 /* 0xBA */
    {X64CPU_OP_GROUP,
     1,
     {},
     (struct x64cpu_opcode_definition[]){
         {},
         {},
         {},
         {},
         {X64CPU_OP_BT,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_BTS,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_BTR,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
         {X64CPU_OP_BTC,
          1,
          {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_I, X64CPU_PS_b}}},
     }},
    /* 0xBB */
    {X64CPU_OP_BTC,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0xBC */
    {X64CPU_OP_BSF,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0xBD */
    {X64CPU_OP_BSR,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_vqp}}},
    /* 0xBE */
    {X64CPU_OP_MOVSX,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_b}}},
    /* 0xBF */
    {X64CPU_OP_MOVSX,
     1,
     {{X64CPU_PT_G, X64CPU_PS_vqp}, {X64CPU_PT_E, X64CPU_PS_w}}},

    /* 0xC_ */
    /* 0xC0 */
    {X64CPU_OP_XADD,
     1,
     {{X64CPU_PT_E, X64CPU_PS_b}, {X64CPU_PT_G, X64CPU_PS_b}}},
    /* 0xC1 */
    {X64CPU_OP_XADD,
     1,
     {{X64CPU_PT_E, X64CPU_PS_vqp}, {X64CPU_PT_G, X64CPU_PS_vqp}}},
    /* 0xC2 */ {X64CPU_OP_TODO}, /* SSE */
    /* 0xC3 */ {X64CPU_OP_TODO}, /* SSE */
    /* 0xC4 */ {X64CPU_OP_TODO}, /* SSE */
    /* 0xC5 */ {X64CPU_OP_TODO}, /* SSE */
    /* 0xC6 */ {X64CPU_OP_TODO}, /* SSE */
    /* 0xC7 */ {X64CPU_OP_TODO}, /* TODO: GROUP x - CMPXCHG8B ... */
                                 /* 0xC_ */
    /* 0xC8 */ {X64CPU_OP_BSWAP, 0, {{X64CPU_PT_RAX_R8, X64CPU_PS_b}}},
    /* 0xC9 */ {X64CPU_OP_BSWAP, 0, {{X64CPU_PT_RCX_R9, X64CPU_PS_b}}},
    /* 0xCA */ {X64CPU_OP_BSWAP, 0, {{X64CPU_PT_RDX_R10, X64CPU_PS_b}}},
    /* 0xCB */ {X64CPU_OP_BSWAP, 0, {{X64CPU_PT_RBX_R11, X64CPU_PS_b}}},
    /* 0xCC */ {X64CPU_OP_BSWAP, 0, {{X64CPU_PT_RSP_R12, X64CPU_PS_b}}},
    /* 0xCD */ {X64CPU_OP_BSWAP, 0, {{X64CPU_PT_RBP_R13, X64CPU_PS_b}}},
    /* 0xCE */ {X64CPU_OP_BSWAP, 0, {{X64CPU_PT_RSI_R14, X64CPU_PS_b}}},
    /* 0xCF */ {X64CPU_OP_BSWAP, 0, {{X64CPU_PT_RDI_R15, X64CPU_PS_b}}},

    /* 0xD_ */
    /* 0xD0 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xD1 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xD2 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xD3 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xD4 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xD5 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xD6 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xD7 */ {X64CPU_OP_TODO}, /* sse group, removed */
                                 /* 0xD_ */
    /* 0xD8 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xD9 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xDA */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0xDB */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xDC */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xDD */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xDE */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xDF */ {X64CPU_OP_TODO}, /* sse1/mmx group */

    /* 0xE_ */
    /* 0xE0 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xE1 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xE2 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xE3 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xE4 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xE5 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xE6 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xE7 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
                                 /* 0xE_ */
    /* 0xE8 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xE9 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xEA */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xEB */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0xEC */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xED */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xEE */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xEF */ {X64CPU_OP_TODO}, /* sse group, removed */

    /* 0xF_ */
    /* 0xF0 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xF1 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xF2 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xF3 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xF4 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xF5 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xF6 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xF7 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
                                 /* 0xF_ */
    /* 0xF8 */ {X64CPU_OP_TODO}, /* sse group, removed */
    /* 0xF9 */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xFA */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xFB */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xFC */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xFD */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xFE */ {X64CPU_OP_TODO}, /* sse1/mmx group */
    /* 0xFF */ {X64CPU_OP_INVALID},
};

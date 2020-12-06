#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "opcode_decoder.h"

#define uint128_t __uint128_t
#define int128_t __int128_t

/* Execution error / status */
enum x64cpu_execution_status {
  X64CPU_RES_SUCCESS = 0,
  X64CPU_RES_SYSCALL, /*!< syscall instruction was executed */
  X64CPU_RES_SOFTINT, /*!< soft interrupt */
  X64CPU_RES_EXCEPTION,
};

/* FLAGS */
enum x64cpu_flags {
  X64FLAG_CF = (1 << 0),
  X64FLAG_PF = (1 << 2),
  X64FLAG_AF = (1 << 4),
  X64FLAG_ZF = (1 << 6),
  X64FLAG_SF = (1 << 7),
  X64FLAG_OF = (1 << 11),

  X64FLAG_TF = (1 << 8),
  X64FLAG_IF = (1 << 9),

  X64FLAG_DF = (1 << 10),

  X64FLAG_ID = (1 << 21),
};

/* CPU Exceptions */
enum x64cpu_exception {
  X64CPU_EXCEPTION_NONE = -1,

  X64CPU_EXCEPTION_DE = 0,  /*!< Divide error */
  X64CPU_EXCEPTION_DB = 1,  /*!< Debug */
  X64CPU_EXCEPTION_BP = 3,  /*!< INT3 instruction */
  X64CPU_EXCEPTION_UD = 6,  /*!< Invalid opcode */
  X64CPU_EXCEPTION_GP = 13, /*!< General Protection */
  X64CPU_EXCEPTION_PF = 14, /*!< Page-Fault */

  X64CPU_EXCEPTION_MF = 16, /*!< Floating-Point Error */

  X64CPU_EXCEPTION_LAST
};

enum x64cpu_mem_access_flags {
  X64CPU_MEM_ACCESS_NONE = 0,
  X64CPU_MEM_ACCESS_READ,
  X64CPU_MEM_ACCESS_WRITE,
  X64CPU_MEM_ACCESS_EXECUTE
};

enum x64cpu_mem_access_error {
  X64CPU_MEM_ACCESS_SUCCESS = 0,
  X64CPU_MEM_ACCESS_GP, /*!< Protection Fault */
  X64CPU_MEM_ACCESS_PF, /*!< Page Fault - page not in memory */
};

struct x64cpu_regs {
  /* Instruction pointer */
  uint64_t rip;

  /* Flags */
  uint64_t rflags;

  /* General purpose registers */
  uint64_t rax, rcx, rdx, rbx;
  uint64_t rsp, rbp, rsi, rdi;

  /* New registers */
  uint64_t r8, r9, r10, r11, r12, r13, r14, r15;

  /* Segment registers */
  uint16_t cs, ds, es, fs, gs, ss;

  /* FS / GS Segment Table pointers */
  uint64_t fs_ptr;
  uint64_t gs_ptr;
};

enum x64cpu_operand_type {
  X64CPU_OPT_NONE = 0,
  X64CPU_OPT_IMMEDIATE,
  X64CPU_OPT_REGISTER,
  X64CPU_OPT_REGISTER_POINTER,
  X64CPU_OPT_MEMORY_ACCESS
};

struct x64cpu;
typedef int (*x64cpu_mem_cb)(struct x64cpu *cpu, void *user_data,
                             uint64_t address, uint8_t *data, uint8_t size,
                             int access_flags, uint64_t *fault_addr);

struct x64cpu_operand {
  union {
    uint64_t immediate;
    uint8_t *reg;
    uint64_t address;
  };
  uint8_t type; /* see enum x64cpu_operand_type */
  uint8_t size;
  uint8_t ptr_size;
  uint8_t sign_extend;

  uint8_t is_sib;
  uint8_t *base_reg;
  uint8_t *scaled_reg;
  uint8_t scale;
  int64_t displacement;
  uint64_t segment_offset;
};

struct x64cpu {
  struct x64cpu_regs regs;

  uint64_t tsc;

  int is_halted;

  /* Memory I/O */
  void *user_data;
  x64cpu_mem_cb mem_read;
  x64cpu_mem_cb mem_write;

  int execution_result;
  int interrupt_number; /*!< The number of the software interrupt */
  struct {
    int code;            /*!< -1 = none */
    uint32_t error_code; /*!< Error code pushed by exception */
    uint64_t rip;        /*!< Instruction Pointer */
    uint64_t
        address; /*!< Set if memory access caused exception (CR2 register?) */
    uint8_t r_w;
  } cpu_exception;

  uint64_t instruction_counter;

  /* Temporary emulator variables */
  uint64_t old_rip;

  /* Stores the decoded instruction to be executed */
  int prefix_flags;      /* see enum x64cpu_prefix_flags in opcode_decoder.h */
  int current_operation; /* see enum x64cpu_operation in opcode_decoder.h */
  int operation_instruction_set; /* 0 - general purpose, x - SSE2, etc. */
  uint8_t current_opcode;

  const struct x64cpu_opcode_definition *current_op_def;

  struct {
    uint8_t full;
    uint8_t mod;
    uint8_t rm;
    uint8_t reg;
  } modrmbyte;

  struct {
    uint8_t full;
    uint8_t ss;
    uint8_t index;
    uint8_t reg;
  } sibbyte;

  int32_t displacement; /* temporary */

  x64cpu_operand *op;

  int repeat_prefix;
  int repeat_use_zf;
  uint64_t repeat_rip;

  uint8_t instr_length;
};

#define INTERNAL_ERROR()                                                       \
  { (*((char *)0)) = 1; }

#define ASSERT(c)                                                              \
  {                                                                            \
    if (!(c)) {                                                                \
      fprintf(stderr, "Assertion failed at %s:%d in %s. Condition: " #c "\n",  \
              __FILE__, __LINE__, __PRETTY_FUNCTION__);                        \
      INTERNAL_ERROR();                                                        \
    }                                                                          \
  }

const uint64_t X64CPU_ALU_DEFAULT_FLAGS =
    X64FLAG_CF | X64FLAG_PF | X64FLAG_AF | X64FLAG_ZF | X64FLAG_SF | X64FLAG_OF;

inline void update_flags(x64cpu *cpu, uint64_t values, uint64_t bit_mask) {
  cpu->regs.rflags = (cpu->regs.rflags & (~bit_mask)) | (values & bit_mask);
}

enum x64cpu_alu_ops {
  X64CPU_ALU_NOOP = 0,
  X64CPU_ALU_ADD,
  X64CPU_ALU_OR,
  X64CPU_ALU_ADC,
  X64CPU_ALU_SBB,
  X64CPU_ALU_AND,
  X64CPU_ALU_SUB,
  X64CPU_ALU_XOR,
  X64CPU_ALU_CMP,
  X64CPU_ALU_TEST,
  X64CPU_ALU_XCHG,
  X64CPU_ALU_MOV,

  X64CPU_ALU_ROL,
  X64CPU_ALU_ROR,
  X64CPU_ALU_RCL,
  X64CPU_ALU_RCR,
  X64CPU_ALU_SHL,
  X64CPU_ALU_SHR,
  X64CPU_ALU_SAR,
  X64CPU_ALU_SAL,

  X64CPU_ALU_NOT,

  X64CPU_ALU_MUL1,
  X64CPU_ALU_MUL,
  X64CPU_ALU_IMUL1,
  X64CPU_ALU_IMUL,

  X64CPU_ALU_DIV,
  X64CPU_ALU_IDIV,

  X64CPU_ALU_BT,
  X64CPU_ALU_BTS,
  X64CPU_ALU_BSF,
  X64CPU_ALU_BSR,
};

const char *x64cpu_exception_name_list[] = {"#DE - Divide error",
                                            "#DB - Debug",
                                            "#NMI",
                                            "#BP - Breakpoint (INT 3)",
                                            "#OF",
                                            "#BR",
                                            "#UD - Undefined opcode",
                                            "#NM",
                                            "#DF",
                                            "#(Reserved)",
                                            "#TS",
                                            "#NP",
                                            "#SS",
                                            "#GP - General Protection",
                                            "#PF - Page Fault"};

const char *x64cpu_exception_name(int exception) {
  return x64cpu_exception_name_list[exception];
}

/* Generate a CPU exception */
void x64cpu_exception(x64cpu *cpu, int exception_code) {
  cpu->cpu_exception.code = exception_code;
  cpu->cpu_exception.rip = cpu->old_rip;
  cpu->execution_result = X64CPU_RES_EXCEPTION;
  cpu->is_halted = 1;
}

void x64cpu_memory_read(x64cpu *cpu, uint64_t address, uint8_t *val,
                        uint8_t size, int access_flags) {
  int result;
  uint64_t fault_addr = 0;

  if (cpu->mem_read != NULL) {
    result = cpu->mem_read(cpu, cpu->user_data, address, val, size,
                           access_flags, &fault_addr);
  } else {
    result = X64CPU_MEM_ACCESS_PF;
  }

  if (result == X64CPU_MEM_ACCESS_GP) {
    x64cpu_exception(cpu, X64CPU_EXCEPTION_GP);
    cpu->cpu_exception.address = fault_addr;
    cpu->cpu_exception.r_w = 0;
  } else if (result == X64CPU_MEM_ACCESS_PF) {
    x64cpu_exception(cpu, X64CPU_EXCEPTION_PF);
    cpu->cpu_exception.address = fault_addr;
    cpu->cpu_exception.r_w = 0;
  }
}

void x64cpu_memory_write(x64cpu *cpu, uint64_t address, uint8_t *val,
                         uint8_t size, int access_flags) {
  int result;
  uint64_t fault_addr = 0;

  if (cpu->mem_write != NULL) {
    result = cpu->mem_write(cpu, cpu->user_data, address, val, size,
                            access_flags, &fault_addr);
  } else {
    result = X64CPU_MEM_ACCESS_PF;
  }

  if (result == X64CPU_MEM_ACCESS_GP) {
    x64cpu_exception(cpu, X64CPU_EXCEPTION_GP);
    cpu->cpu_exception.address = fault_addr;
    cpu->cpu_exception.r_w = 1;
  } else if (result == X64CPU_MEM_ACCESS_PF) {
    x64cpu_exception(cpu, X64CPU_EXCEPTION_PF);
    cpu->cpu_exception.address = fault_addr;
    cpu->cpu_exception.r_w = 1;
  }
}

uint8_t x64cpu_fetch8(x64cpu *cpu) {
  uint8_t ret;
  x64cpu_memory_read(cpu, cpu->regs.rip, &ret, 1, X64CPU_MEM_ACCESS_EXECUTE);
  cpu->regs.rip += 1;
  return ret;
}

uint16_t x64cpu_fetch16(x64cpu *cpu) {
  uint16_t ret;
  x64cpu_memory_read(cpu, cpu->regs.rip, (uint8_t *)&ret, 2,
                     X64CPU_MEM_ACCESS_EXECUTE);
  cpu->regs.rip += 2;
  return ret;
}

uint32_t x64cpu_fetch32(x64cpu *cpu) {
  uint32_t ret;
  x64cpu_memory_read(cpu, cpu->regs.rip, (uint8_t *)&ret, 4,
                     X64CPU_MEM_ACCESS_EXECUTE);
  cpu->regs.rip += 4;
  return ret;
}

uint64_t x64cpu_fetch64(x64cpu *cpu) {
  uint64_t ret;
  x64cpu_memory_read(cpu, cpu->regs.rip, (uint8_t *)&ret, 8,
                     X64CPU_MEM_ACCESS_EXECUTE);
  cpu->regs.rip += 8;
  return ret;
}

/* CPU Operands util methods */

void x64cpu_operand_set_imm(x64cpu *cpu, int ope_index, uint8_t *val,
                            uint8_t size) {
  cpu->op[ope_index].type = X64CPU_OPT_IMMEDIATE;
  cpu->op[ope_index].immediate = 0;
  cpu->op[ope_index].size = size;
  cpu->op[ope_index].sign_extend = 0;
  memcpy(&cpu->op[ope_index].immediate, val, size);
  cpu->op[ope_index].is_sib = 0;
}

void x64cpu_operand_set_reg(x64cpu *cpu, int ope_index, uint8_t *reg,
                            uint8_t size) {
  cpu->op[ope_index].type = X64CPU_OPT_REGISTER;
  cpu->op[ope_index].reg = reg;
  cpu->op[ope_index].size = size;
  cpu->op[ope_index].sign_extend = 0;
  cpu->op[ope_index].is_sib = 0;
}

/* For RDI, RSI ; incrementable register ; behaves like address */
void x64cpu_operand_set_reg_ptr(x64cpu *cpu, int ope_index, uint8_t *reg,
                                uint8_t reg_size, uint8_t size) {
  cpu->op[ope_index].type = X64CPU_OPT_REGISTER_POINTER;
  cpu->op[ope_index].reg = reg;
  cpu->op[ope_index].size = size;
  cpu->op[ope_index].ptr_size = reg_size;
  cpu->op[ope_index].sign_extend = 0;
  cpu->op[ope_index].is_sib = 0;
}

void x64cpu_operand_set_address_abs(x64cpu *cpu, int ope_index,
                                    uint64_t address, uint8_t size) {
  cpu->op[ope_index].type = X64CPU_OPT_MEMORY_ACCESS;
  cpu->op[ope_index].address = address;
  cpu->op[ope_index].size = size;
  cpu->op[ope_index].sign_extend = 0;
  cpu->op[ope_index].is_sib = 0;
}

void x64cpu_operand_set_address_sib(x64cpu *cpu, int ope_index, uint8_t *base,
                                    uint8_t *scaled, uint8_t multiplier,
                                    int64_t displacement, uint8_t size) {
  cpu->op[ope_index].type = X64CPU_OPT_MEMORY_ACCESS;
  cpu->op[ope_index].address = 0;
  cpu->op[ope_index].size = size;
  cpu->op[ope_index].sign_extend = 0;
  cpu->op[ope_index].is_sib = 1;
  cpu->op[ope_index].base_reg = base;
  cpu->op[ope_index].scaled_reg = scaled;
  cpu->op[ope_index].scale = multiplier;
  cpu->op[ope_index].displacement = displacement;
}

uint64_t x64cpu_operand_ptr_address(x64cpu *cpu, int index) {
  uint64_t ret = 0;
  x64cpu_operand *op = &cpu->op[index];

  if (op->type != X64CPU_OPT_REGISTER_POINTER) {
    INTERNAL_ERROR();
  }

  switch (op->ptr_size) {
  case 4:
    ret = *((uint32_t *)op->reg);
    break;

  case 8:
    ret = *((uint64_t *)op->reg);
    break;

  default:
    INTERNAL_ERROR();
  }

  return ret;
}

void x64cpu_operand_ptr_increment(x64cpu *cpu, int ope_index, int value) {
  x64cpu_operand *op = &cpu->op[ope_index];

  if (op->type != X64CPU_OPT_REGISTER_POINTER) {
    return;
  }

  switch (op->ptr_size) {
  case 1:
    *((uint8_t *)op->reg) += value;
    break;
  case 2:
    *((uint16_t *)op->reg) += value;
    break;
  case 4:
    *((uint32_t *)op->reg) += value;
    break;
  case 8:
    *((uint64_t *)op->reg) += value;
    break;
  default:
    INTERNAL_ERROR();
  }
}

void x64cpu_operand_address_sib_to_abs(x64cpu *cpu, int index) {
  x64cpu_operand *op = &cpu->op[index];
  uint64_t base = 0, scaled = 0;

  if (op->type != X64CPU_OPT_MEMORY_ACCESS || op->is_sib == 0) {
    return;
  }

  if (op->base_reg) {
    memcpy(&base, op->base_reg, 8);
  }
  if (op->scaled_reg) {
    memcpy(&scaled, op->scaled_reg, 8);
  }

  op->address =
      base + (scaled * op->scale) + op->displacement + op->segment_offset;
  op->is_sib = 0;
}

uint64_t x64cpu_operand_get_address(x64cpu *cpu, int index) {
  x64cpu_operand_address_sib_to_abs(cpu, index);
  return cpu->op[index].address;
}

void x64cpu_operand_read(x64cpu *cpu, int index, uint8_t *out, uint8_t size) {
  x64cpu_operand *op = &cpu->op[index];
  uint64_t address;
  uint64_t val = 0x00;

  switch (op->type) {
  case X64CPU_OPT_IMMEDIATE:
    memcpy((uint8_t *)&val, &op->immediate, op->size);
    break;

  case X64CPU_OPT_REGISTER:
    memcpy((uint8_t *)&val, op->reg, op->size);
    break;

  case X64CPU_OPT_REGISTER_POINTER:
    address = x64cpu_operand_ptr_address(cpu, index);
    /* Memory access ; TODO: mode 64/32 */
    x64cpu_memory_read(cpu, address, (uint8_t *)&val, size,
                       X64CPU_MEM_ACCESS_READ);
    break;

  case X64CPU_OPT_MEMORY_ACCESS:
    /* Memory access ; TODO: mode 64/32 */
    if (op->is_sib != 0) {
      x64cpu_operand_address_sib_to_abs(cpu, index);
    }
    address = op->address;
    x64cpu_memory_read(cpu, address, (uint8_t *)&val, size,
                       X64CPU_MEM_ACCESS_READ);
    break;

  default:
    /* Internal error */
    INTERNAL_ERROR();
    break;
  }

  memcpy(out, (uint8_t *)&val, size);
}

void x64cpu_operand_read_u64(x64cpu *cpu, int index, uint64_t &out,
                             uint8_t size) {
  x64cpu_operand *op = &cpu->op[index];
  uint64_t address;

  switch (op->type) {
  case X64CPU_OPT_IMMEDIATE:
    out = op->immediate;
    break;

  case X64CPU_OPT_REGISTER:
    if (size == 8)
      out = *(uint64_t *)op->reg;
    else if (size == 4)
      out = *(uint32_t *)op->reg;
    else if (size == 2)
      out = *(uint16_t *)op->reg;
    else
      out = *op->reg;
    break;

  case X64CPU_OPT_REGISTER_POINTER:
    address = x64cpu_operand_ptr_address(cpu, index);
    /* Memory access ; TODO: mode 64/32 */
    x64cpu_memory_read(cpu, address, (uint8_t *)&out, size,
                       X64CPU_MEM_ACCESS_READ);
    break;

  case X64CPU_OPT_MEMORY_ACCESS:
    /* Memory access ; TODO: mode 64/32 */
    if (op->is_sib != 0) {
      x64cpu_operand_address_sib_to_abs(cpu, index);
    }
    address = op->address;
    x64cpu_memory_read(cpu, address, (uint8_t *)&out, size,
                       X64CPU_MEM_ACCESS_READ);
    break;

  default:
    /* Internal error */
    INTERNAL_ERROR();
    break;
  }
}

void x64cpu_operand_write(x64cpu *cpu, int index, uint8_t *in, uint8_t size) {
  x64cpu_operand *op = &cpu->op[index];
  uint64_t address;

  switch (op->type) {
  case X64CPU_OPT_IMMEDIATE:
    memcpy((uint8_t *)&op->immediate, in, size);
    op->size = size;
    break;

  case X64CPU_OPT_REGISTER:
    /* All 32-bit registers are zero-extended to 64 ??? */
    if (op->size == 4) {
      uint64_t val = 0;
      memcpy((uint8_t *)&val, in, size);
      memcpy(op->reg, (uint8_t *)&val, 8);
    } else {
      memcpy(op->reg, in, op->size);
    }
    break;

  case X64CPU_OPT_REGISTER_POINTER:
    address = x64cpu_operand_ptr_address(cpu, index);
    /* Memory access ; TODO: mode 64/32 */
    x64cpu_memory_write(cpu, address, in, size, X64CPU_MEM_ACCESS_WRITE);
    break;

  case X64CPU_OPT_MEMORY_ACCESS:
    /* Memory access ; TODO: mode 64/32 */
    if (op->is_sib != 0) {
      x64cpu_operand_address_sib_to_abs(cpu, index);
    }
    address = op->address;
    x64cpu_memory_write(cpu, address, in, size, X64CPU_MEM_ACCESS_WRITE);
    break;

  default:
    /* Internal error */
    INTERNAL_ERROR();
    break;
  }
}

void x64cpu_operand_write_u64(x64cpu *cpu, int index, uint64_t in,
                              uint8_t size) {
  x64cpu_operand *op = &cpu->op[index];
  uint64_t address;

  switch (op->type) {
  case X64CPU_OPT_IMMEDIATE:
    op->immediate = in;
    op->size = size;
    break;

  case X64CPU_OPT_REGISTER:
    /* All 32-bit registers are zero-extended to 64 ??? */
    if (size == 8)
      *(uint64_t *)op->reg = in;
    else if (size == 4)
      *(uint64_t *)op->reg = (uint32_t)in;
    else if (size == 2)
      *(uint16_t *)op->reg = in;
    else if (size == 1)
      *op->reg = in;
    break;

  case X64CPU_OPT_REGISTER_POINTER:
    address = x64cpu_operand_ptr_address(cpu, index);
    /* Memory access ; TODO: mode 64/32 */
    x64cpu_memory_write(cpu, address, (uint8_t *)&in, size,
                        X64CPU_MEM_ACCESS_WRITE);
    break;

  case X64CPU_OPT_MEMORY_ACCESS:
    /* Memory access ; TODO: mode 64/32 */
    if (op->is_sib != 0) {
      x64cpu_operand_address_sib_to_abs(cpu, index);
    }
    address = op->address;
    x64cpu_memory_write(cpu, address, (uint8_t *)&in, size,
                        X64CPU_MEM_ACCESS_WRITE);
    break;

  default:
    /* Internal error */
    INTERNAL_ERROR();
    break;
  }
}

/* Extend the size of an operand ; transforms it into an immediate value */
void x64cpu_operand_extend(x64cpu *cpu, int ope_index, uint8_t new_size,
                           int sign_extend) {
  x64cpu_operand *op = &cpu->op[ope_index];
  uint64_t tmp = 0;

  if (new_size <= op->size) {
    INTERNAL_ERROR();
    return;
  }

  /* Only register, immediate and memory supported; but memory turns to
   * immediate */
  if (op->type == X64CPU_OPT_REGISTER_POINTER) {
    INTERNAL_ERROR();
    return;
  }

  if (sign_extend == 0) {
    uint64_t tmp = 0x00;
    x64cpu_operand_read_u64(cpu, ope_index, tmp, op->size);
    x64cpu_operand_set_imm(cpu, ope_index, (uint8_t *)&tmp, new_size);
    return;
  }

  /* Sign extend */

  switch (op->size) {
  case 1: {
    int8_t v1;
    x64cpu_operand_read(cpu, ope_index, (uint8_t *)&v1, 1);
    tmp = (int8_t)v1;
  } break;
  case 2: {
    int16_t v1;
    x64cpu_operand_read(cpu, ope_index, (uint8_t *)&v1, 2);
    tmp = (int16_t)v1;
  } break;

  case 4: {
    int32_t v1;
    x64cpu_operand_read(cpu, ope_index, (uint8_t *)&v1, 4);
    tmp = (int32_t)v1;
  } break;
  }

  x64cpu_operand_set_imm(cpu, ope_index, (uint8_t *)&tmp, new_size);
}

void x64cpu_register_grab(x64cpu *cpu, int register_set, int rex_flag,
                          uint8_t reg, uint8_t size, uint8_t **ret,
                          uint8_t *ret_size) {
  uint8_t *op = NULL;
  uint8_t op_size = 8;
  int can_use_rh = 1;
  int use_rex = 0;

  ASSERT(reg >= 0 && reg <= 7);
  ASSERT(size == 1 || size == 2 || size == 4 || size == 8 || size == 10 ||
         size == 16);

  if (((cpu->prefix_flags & X64CPU_PREFIX_REX) != 0) || (size > 1)) {
    can_use_rh = 0;
  }

  if ((cpu->prefix_flags & rex_flag) != 0) {
    use_rex = 1;
  }

  switch (register_set) {
  case X64CPU_REG_GP:
  case X64CPU_REG_GP_H:
    if (register_set == X64CPU_REG_GP_H && can_use_rh == 1) {
      if (reg < 4) {
        op = (uint8_t *)&cpu->regs.rax + 8 * reg;
      } else {
        op = (uint8_t *)&cpu->regs.rax + 8 * reg - 32 + 1;
      }

      /* Only byte size */
      op_size = 1;
    } else if (use_rex != 0) {
      op = (uint8_t *)&cpu->regs.r8 + 8 * reg;
    } else {
      op = (uint8_t *)&cpu->regs.rax + 8 * reg;
    }
    break;

  case X64CPU_REG_F:
    /* Flags register; ignore reg byte */
    op = (uint8_t *)&cpu->regs.rflags;
    break;

  case X64CPU_REG_S:
    /* Only word access */
    op_size = 2;

    switch (reg) {
    case 0x00:
      op = (uint8_t *)&cpu->regs.es;
      break;
    case 0x01:
      op = (uint8_t *)&cpu->regs.cs;
      break;
    case 0x02:
      op = (uint8_t *)&cpu->regs.ss;
      break;
    case 0x03:
      op = (uint8_t *)&cpu->regs.ds;
      break;
    case 0x04:
      op = (uint8_t *)&cpu->regs.fs;
      break;
    case 0x05:
      op = (uint8_t *)&cpu->regs.gs;
      break;
    default:
      /* 0x06, 0x07 .res ??? */
      // TODO: Not implemented
      // INTERNAL_ERROR();
      x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
      break;
    }
    break;

  default:
    // TODO: not implemented
    INTERNAL_ERROR();
    break;
  }

  ASSERT(size <= op_size);

  op_size = size;

  (*ret) = op;
  (*ret_size) = op_size;
}

void x64cpu_select_operand_reg(x64cpu *cpu, int index, int register_set,
                               int rex_flag, uint8_t reg, uint8_t size) {
  uint8_t *op = NULL, opsize = 0;
  x64cpu_register_grab(cpu, register_set, rex_flag, reg, size, &op, &opsize);
  x64cpu_operand_set_reg(cpu, index, op, opsize);
}

/* Determine operand-size based on definition and modrmbyte */
void x64cpu_decode_operand_size(x64cpu *cpu, int type, int size,
                                uint8_t *ret_size, uint8_t *ret_sign_extended) {
  int ret = 0;
  int sign_extended = 0;
  int prefix = cpu->prefix_flags;

  switch (type) {
  case X64CPU_PT_ST:
  case X64CPU_PT_EST:
    ret = 10; /* 80-bits */
    break;

  case X64CPU_PT_1:
  case X64CPU_PT_3:
    ret = 1;
    break;

  case X64CPU_PT_S:
    ret = 2;
    break;

  default:
    switch (size) {
    case X64CPU_PS_b:
      ret = 1;
      break;
    case X64CPU_PS_bs:
      ret = 1;
      sign_extended = 1;
      break;

    /* Extended to 64 in decode_operand_type */
    case X64CPU_PS_bss:
      ret = 1;
      sign_extended = 1;
      break;

    case X64CPU_PS_w:
      ret = 2;
      break;
    case X64CPU_PS_d:
      ret = 4;
      break;
    case X64CPU_PS_q:
      ret = 8;
      break;

    case X64CPU_PS_dqp:
      if ((prefix & X64CPU_PREFIX_REX_W) != 0) {
        ret = 8;
      } else {
        ret = 4;
      }
      break;

    case X64CPU_PS_v:
      if ((prefix & X64CPU_PREFIX_OP_SIZE) != 0) {
        ret = 2;
      } else {
        ret = 4;
      }
      break;

    case X64CPU_PS_vs:
    case X64CPU_PS_vds:
      if ((prefix & X64CPU_PREFIX_OP_SIZE) != 0) {
        ret = 2;
      } else {
        ret = 4;
      }
      sign_extended = 1;
      break;

    case X64CPU_PS_vq:
      if ((prefix & X64CPU_PREFIX_OP_SIZE) != 0) {
        ret = 2;
      } else {
        ret = 8;
      }
      break;

    case X64CPU_PS_vqpMw: // TODO: somehow else ?
    case X64CPU_PS_vqp:
      if ((prefix & X64CPU_PREFIX_REX_W) != 0) {
        ret = 8;
      } else if ((prefix & X64CPU_PREFIX_OP_SIZE) != 0) {
        ret = 2;
      } else {
        ret = 4;
      }
      break;

    case X64CPU_PS_wi:
      ret = 2;
      break;

    case X64CPU_PS_di:
      ret = 4;
      break;

    case X64CPU_PS_qi:
      ret = 8;
      break;

    case X64CPU_PS_dq:
      ret = 16;
      break;

    case X64CPU_PS_bcd:
      ret = 10; /* 80bit ? */
      break;

    case X64CPU_PS_sr:
      ret = 4;
      break;

    case X64CPU_PS_dr:
      ret = 8;
      break;

    case X64CPU_PS_er:
      ret = 10;
      break;

    case X64CPU_PS_sd:
      ret = 16;
      break;

    case X64CPU_PS_pd:
      ret = 16;
      break;

    case X64CPU_PS_ss:
      ret = 16;
      break;

    case X64CPU_PS_ps:
      ret = 16;
      break;

    default:
      /* Not implemented */
      INTERNAL_ERROR();
      break;
    }
    break;
  }

  (*ret_size) = ret;
  (*ret_sign_extended) = sign_extended;
}

uint8_t x64cpu_decode_address_size(x64cpu *cpu) {
  uint8_t ret = 8; /* Default on 64-bit */

  if ((cpu->prefix_flags & X64CPU_PREFIX_ADDR_SIZE) == 1) {
    ret = 4;
  }

  return ret;
}

void x64cpu_decode_sib(x64cpu *cpu, uint8_t **p_base, uint8_t **p_scaled,
                       uint8_t *p_multiplier) {
  uint8_t multiplier = 1;
  uint8_t *scaled = NULL, *base = NULL;

  switch (cpu->sibbyte.ss) {
  case 0x01:
    multiplier = 2;
    break;
  case 0x02:
    multiplier = 4;
    break;
  case 0x03:
    multiplier = 8;
    break;
  }

  if ((cpu->prefix_flags & X64CPU_PREFIX_REX_X) == 0) {
    if (cpu->sibbyte.index != 0x04)
      scaled = (uint8_t *)&cpu->regs.rax + 8 * cpu->sibbyte.index;
    else
      scaled = (uint8_t *)NULL;
  } else {
    scaled = (uint8_t *)&cpu->regs.r8 + 8 * cpu->sibbyte.index;
  }

  if ((cpu->prefix_flags & X64CPU_PREFIX_REX_B) == 0) {
    if (cpu->sibbyte.reg == 0x05)
      switch (cpu->modrmbyte.mod) {
      case 0x00:
        break;
      case 0x01:
        base = (uint8_t *)&cpu->regs.rbp;
        break;
      case 0x02:
        base = (uint8_t *)&cpu->regs.rbp;
        break;
      }
    else
      base = (uint8_t *)&cpu->regs.rax + 8 * cpu->sibbyte.reg;
  } else {
    if (cpu->sibbyte.reg == 0x05)
      switch (cpu->modrmbyte.mod) {
      case 0x00:
        break;
      case 0x01:
        base = (uint8_t *)&cpu->regs.r13;
        break;
      case 0x10:
        base = (uint8_t *)&cpu->regs.r13;
        break;
      }
    else
      base = (uint8_t *)&cpu->regs.r8 + 8 * cpu->sibbyte.reg;
  }

  (*p_base) = base;
  (*p_scaled) = scaled;
  (*p_multiplier) = multiplier;
}

void x64cpu_decode_operand_EffA(x64cpu *cpu, int index, int register_set,
                                uint8_t size) {
  uint8_t *op = NULL;
  uint8_t op_size = 0;
  int is_mem_access = 0;
  uint8_t *base = NULL, *scaled = NULL;
  uint8_t multiplier = 1;

  switch (cpu->modrmbyte.mod) {
  /* Effective address */
  case 0x00:
  case 0x01:
  case 0x02:
    is_mem_access = 1;

    op_size = size;

    if ((cpu->prefix_flags & X64CPU_PREFIX_REX_B) == 0) {
      switch (cpu->modrmbyte.rm) {
      case 0x04:
        x64cpu_decode_sib(cpu, &base, &scaled, &multiplier);
        break;
      case 0x05:
        if (cpu->modrmbyte.mod == 0x00) {
          base = (uint8_t *)&cpu->regs.rip;
        } else {
          base = (uint8_t *)&cpu->regs.rbp;
        }
        break;
      default:
        base = (uint8_t *)&cpu->regs.rax + 8 * cpu->modrmbyte.rm;
      }
    } else {
      switch (cpu->modrmbyte.rm) {
      case 0x04:
        x64cpu_decode_sib(cpu, &base, &scaled, &multiplier);
        break;
      case 0x05:
        if (cpu->modrmbyte.mod == 0x00) {
          base = (uint8_t *)&cpu->regs.rip;
        } else {
          base = (uint8_t *)&cpu->regs.r13;
        }
        break;
      default:
        base = (uint8_t *)&cpu->regs.r8 + 8 * cpu->modrmbyte.rm;
      }
    }
    break;

  case 0x03:
    x64cpu_register_grab(cpu, register_set, X64CPU_PREFIX_REX_B,
                         cpu->modrmbyte.rm, size, &op, &op_size);
    break;
  }

  if (op_size == 0) {
    x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
  } else if (is_mem_access) {
    cpu->op[index].segment_offset = 0;
    x64cpu_operand_set_address_sib(cpu, index, base, scaled, multiplier,
                                   cpu->displacement, op_size);
    if ((cpu->prefix_flags & X64CPU_PREFIX_FS)) {
      cpu->op[index].segment_offset = cpu->regs.fs_ptr;
    } else if ((cpu->prefix_flags & X64CPU_PREFIX_GS)) {
      cpu->op[index].segment_offset = cpu->regs.gs_ptr;
    } else {
      cpu->op[index].segment_offset = 0;
    }
  } else {
    x64cpu_operand_set_reg(cpu, index, op, op_size);
  }
}

void x64cpu_decode_operand_E(x64cpu *cpu, int index, uint8_t size) {
  x64cpu_decode_operand_EffA(cpu, index, X64CPU_REG_GP_H, size);
}

void x64cpu_decode_operand_G(x64cpu *cpu, int index, uint8_t size) {
  uint8_t *op = NULL, opsize = 0;
  x64cpu_register_grab(cpu, X64CPU_REG_GP_H, X64CPU_PREFIX_REX_R,
                       cpu->modrmbyte.reg, size, &op, &opsize);
  x64cpu_operand_set_reg(cpu, index, op, opsize);
}

void x64cpu_decode_operand_S(x64cpu *cpu, int index, uint8_t size) {
  uint8_t *op = NULL, opsize = 0;
  x64cpu_register_grab(cpu, X64CPU_REG_S, 0, cpu->modrmbyte.reg, size, &op,
                       &opsize);
  x64cpu_operand_set_reg(cpu, index, op, opsize);
}

void x64cpu_decode_operand_F(x64cpu *cpu, int index, uint8_t size) {
  x64cpu_operand_set_reg(cpu, index, (uint8_t *)&cpu->regs.rflags, size);
}

void x64cpu_decode_operand_I(x64cpu *cpu, int index, uint8_t size) {
  uint64_t tmp = 0;

  switch (size) {
  case 1:
    tmp = x64cpu_fetch8(cpu);
    break;
  case 2:
    tmp = x64cpu_fetch16(cpu);
    break;
  case 4:
    tmp = x64cpu_fetch32(cpu);
    break;
  case 8:
    tmp = x64cpu_fetch64(cpu);
    break;
  default:
    /* Internal error */
    INTERNAL_ERROR();
    break;
  }

  x64cpu_operand_set_imm(cpu, index, (uint8_t *)&tmp, size);
}

void x64cpu_decode_operand_O(x64cpu *cpu, int index, uint8_t size) {
  uint64_t tmp = 0;

  switch (x64cpu_decode_address_size(cpu)) {
  case 4:
    tmp = x64cpu_fetch32(cpu);
    break;
  case 8:
    tmp = x64cpu_fetch64(cpu);
    break;
  default:
    /* Internal error */
    INTERNAL_ERROR();
    break;
  }

  x64cpu_operand_set_address_abs(cpu, index, tmp, size);
}

void x64cpu_decode_operand_XY(x64cpu *cpu, int index, uint8_t is_y,
                              uint8_t size) {
  uint8_t *ptr_reg = NULL;
  uint8_t ptr_reg_size = 0;
  uint8_t address_size;

  address_size = x64cpu_decode_address_size(cpu);

  if (is_y) {
    x64cpu_register_grab(cpu, X64CPU_REG_GP, 0, X64CPU_REGISTER_RDI,
                         address_size, &ptr_reg, &ptr_reg_size);
  } else {
    x64cpu_register_grab(cpu, X64CPU_REG_GP, 0, X64CPU_REGISTER_RSI,
                         address_size, &ptr_reg, &ptr_reg_size);
  }

  x64cpu_operand_set_reg_ptr(cpu, index, ptr_reg, ptr_reg_size, size);
}

void x64cpu_decode_operand_U(x64cpu *cpu, int index, uint8_t size) {
  uint8_t *op = NULL, opsize = 0;

  ASSERT(size == 16);

  /* SSE2 XMM Register */
  x64cpu_register_grab(cpu, X64CPU_REG_XMM, X64CPU_PREFIX_REX_B,
                       cpu->modrmbyte.rm, size, &op, &opsize);
  x64cpu_operand_set_reg(cpu, index, op, opsize);
}

void x64cpu_decode_operand_V(x64cpu *cpu, int index, uint8_t size) {
  uint8_t *op = NULL, opsize = 0;

  ASSERT(size == 16);

  /* SSE2 XMM Register */
  x64cpu_register_grab(cpu, X64CPU_REG_XMM, X64CPU_PREFIX_REX_R,
                       cpu->modrmbyte.reg, size, &op, &opsize);
  x64cpu_operand_set_reg(cpu, index, op, opsize);
}

void x64cpu_decode_operand_W(x64cpu *cpu, int index, uint8_t size) {
  ASSERT(size == 16);

  /* SSE2 XMM Register */
  x64cpu_decode_operand_EffA(cpu, index, X64CPU_REG_XMM, size);
}

void x64cpu_decode_operand_ES(x64cpu *cpu, int index, uint8_t size) {
  x64cpu_decode_operand_EffA(cpu, index, X64CPU_REG_FPU, size);
}

void x64cpu_decode_operand_type(x64cpu *cpu, int index, int type,
                                int def_size) {
  uint64_t tmp = 0;
  uint8_t size = 0;
  uint8_t sign_extended = 0;

  x64cpu_decode_operand_size(cpu, type, def_size, &size, &sign_extended);

  switch (type) {
  case X64CPU_PT_1:
    tmp = 1;
    x64cpu_operand_set_imm(cpu, index, (uint8_t *)&tmp, 1);
    break;
  case X64CPU_PT_3:
    tmp = 3;
    x64cpu_operand_set_imm(cpu, index, (uint8_t *)&tmp, 1);
    break;

  case X64CPU_PT_E:
    x64cpu_decode_operand_E(cpu, index, size);
    break;
  case X64CPU_PT_G:
    x64cpu_decode_operand_G(cpu, index, size);
    break;
  case X64CPU_PT_S:
    x64cpu_decode_operand_S(cpu, index, size);
    break;
  case X64CPU_PT_F:
    x64cpu_decode_operand_F(cpu, index, size);
    break;

  case X64CPU_PT_M:
    x64cpu_decode_operand_E(cpu, index, size);
    if (cpu->op[index].type != X64CPU_OPT_MEMORY_ACCESS) {
      cpu->op[index].type = X64CPU_OPT_NONE;
      x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
    }
    break;

  case X64CPU_PT_I:
    x64cpu_decode_operand_I(cpu, index, size);
    break;

  case X64CPU_PT_J:
    x64cpu_decode_operand_I(cpu, index, size);
    /* Always sign extend to address size (64bit) */
    x64cpu_operand_extend(cpu, index, 8, 1);
    break;

  case X64CPU_PT_O:
    x64cpu_decode_operand_O(cpu, index, size);
    break;

  case X64CPU_PT_X:
    x64cpu_decode_operand_XY(cpu, index, 0, size);
    break;
  case X64CPU_PT_Y:
    x64cpu_decode_operand_XY(cpu, index, 1, size);
    break;

  case X64CPU_PT_RAX:
  case X64CPU_PT_RCX:
  case X64CPU_PT_RDX:
  case X64CPU_PT_RBX:
  case X64CPU_PT_RSP:
  case X64CPU_PT_RBP:
  case X64CPU_PT_RSI:
  case X64CPU_PT_RDI:
    x64cpu_select_operand_reg(cpu, index, X64CPU_REG_GP, 0, (type & 0x07),
                              size);
    break;

  case X64CPU_PT_RAX_R8:
  case X64CPU_PT_RCX_R9:
  case X64CPU_PT_RDX_R10:
  case X64CPU_PT_RBX_R11:
  case X64CPU_PT_RSP_R12:
  case X64CPU_PT_RBP_R13:
  case X64CPU_PT_RSI_R14:
  case X64CPU_PT_RDI_R15:
    x64cpu_select_operand_reg(cpu, index, X64CPU_REG_GP, X64CPU_PREFIX_REX_B,
                              (type & 0x07), size);
    break;

  case X64CPU_PT_RAH_R12:
  case X64CPU_PT_RCH_R13:
  case X64CPU_PT_RDH_R14:
  case X64CPU_PT_RBH_R15:
    x64cpu_select_operand_reg(cpu, index, X64CPU_REG_GP_H, X64CPU_PREFIX_REX_B,
                              (type & 0x07), size);
    break;

  case X64CPU_PT_RAH:
    x64cpu_select_operand_reg(cpu, index, X64CPU_REG_GP_H, 0, (type & 0x07),
                              size);
    break;

  case X64CPU_PT_ST:
    x64cpu_select_operand_reg(cpu, index, X64CPU_REG_FPU, 0, (0 /* ST(0) */),
                              10 /* always full */);
    break;

  case X64CPU_PT_ES:
    x64cpu_decode_operand_ES(cpu, index, size);
    break;

  case X64CPU_PT_EST:
    x64cpu_decode_operand_ES(cpu, index, size);
    if (cpu->op[index].type == X64CPU_OPT_MEMORY_ACCESS) {
      cpu->op[index].type = X64CPU_OPT_NONE;
      x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
    }
    break;

  case X64CPU_PT_U:
    x64cpu_decode_operand_U(cpu, index, size);
    break;

  case X64CPU_PT_V:
    x64cpu_decode_operand_V(cpu, index, size);
    break;

  case X64CPU_PT_W:
    x64cpu_decode_operand_W(cpu, index, size);
    break;

  default:
    /* Not implemented */
    INTERNAL_ERROR();
    break;
  }

  if (sign_extended) {
    cpu->op[index].sign_extend = sign_extended;

    switch ((int)def_size) {
    case X64CPU_PS_bss:
    case X64CPU_PS_vs:
      /* Sign-extended to stack size (64bit) */
      x64cpu_operand_extend(cpu, index, 8, 1);
      cpu->op[index].sign_extend = 0;
      break;
    }
  }
}

/**
 * Decodes prefix instructions
 *
 * @return 0 - if opcode is not a prefix; non-0 - otherwise
 */
int x64cpu_decode_prefix(x64cpu *cpu, uint8_t opcode) {
  int flags = 0x00;

  /* REX Flags */
  if (((opcode & 0xF0) == 0x40) && !(cpu->prefix_flags & X64CPU_PREFIX_REX)) {
    flags |= X64CPU_PREFIX_REX;

    if (opcode & 0x01) {
      flags |= X64CPU_PREFIX_REX_B;
    }
    if (opcode & 0x02) {
      flags |= X64CPU_PREFIX_REX_X;
    }
    if (opcode & 0x04) {
      flags |= X64CPU_PREFIX_REX_R;
    }
    if (opcode & 0x08) {
      flags |= X64CPU_PREFIX_REX_W;
    }
  }
  /* Operand size prefix */
  else if (opcode ==
           0x66) { // && !(cpu->prefix_flags & X64CPU_PREFIX_OP_SIZE)) {
    flags |= X64CPU_PREFIX_OP_SIZE;
  }
  /* Address size prefix */
  else if (opcode == 0x67 && !(cpu->prefix_flags & X64CPU_PREFIX_ADDR_SIZE)) {
    flags |= X64CPU_PREFIX_ADDR_SIZE;
  }
  /* Repeat prefix */
  else if (((opcode & 0xFE) == 0xF2) && (cpu->repeat_prefix == 0)) {
    cpu->repeat_prefix = (opcode & 0x01) ? X64CPU_PREFIX_REPEAT_REPZ
                                         : X64CPU_PREFIX_REPEAT_REPNZ;
    cpu->repeat_rip = cpu->regs.rip;
    flags |= cpu->repeat_prefix;
  }
  /* FS Segment */
  else if (opcode == 0x64 && ((flags & X64CPU_PREFIX_FS) == 0 ||
                              ((flags & X64CPU_PREFIX_GS) == 0))) {
    flags |= X64CPU_PREFIX_FS;
  }
  /* GS Segment */
  else if (opcode == 0x65 && ((flags & X64CPU_PREFIX_FS) == 0 ||
                              ((flags & X64CPU_PREFIX_GS) == 0))) {
    flags |= X64CPU_PREFIX_GS;
  }
  /* LOCK prefix */
  else if (opcode == 0xF0 && ((flags & X64CPU_PREFIX_LOCK) == 0)) {
    flags |= X64CPU_PREFIX_LOCK;
  }
  /* FWAIT prefix */
  else if (opcode == 0x9B && ((flags & X64CPU_PREFIX_FWAIT) == 0)) {
    flags |= X64CPU_PREFIX_FWAIT;
  }
  /* Null-prefixes on 64bit */
  else {
    switch (opcode) {
    case 0x26: /* ES: */
    case 0x2E: /* CS: */
    case 0x36: /* SS: */
    case 0x3E: /* DS: */
      flags |= X64CPU_PREFIX_NULL;
      break;
    }
  }

  cpu->prefix_flags |= flags;

  return flags;
}

/**
 * Decode modrm and sib bytes
 */
void x64cpu_decode_modrm_sib(x64cpu *cpu, uint8_t opcode) {
  uint8_t modrmbyte;

  /* Read modrmbyte */
  modrmbyte = x64cpu_fetch8(cpu);

  cpu->modrmbyte.full = modrmbyte;
  cpu->modrmbyte.mod = (modrmbyte & 0xC0) >> 6;
  cpu->modrmbyte.rm = (modrmbyte & 0x07);
  cpu->modrmbyte.reg = (modrmbyte & 0x38) >> 3;

  /* Do we need SIB byte ? */
  if (cpu->modrmbyte.mod != 0x03 && cpu->modrmbyte.rm == 0x04) {
    uint8_t sib;

    /* Read sib byte */
    sib = x64cpu_fetch8(cpu);

    cpu->sibbyte.full = sib;
    cpu->sibbyte.ss = (sib & 0xC0) >> 6;
    cpu->sibbyte.index = (sib & 0x38) >> 3;
    cpu->sibbyte.reg = (sib & 0x07);
  }

  /* Do we need a displacement offset ? */
  cpu->displacement = 0x00;

  if (cpu->modrmbyte.mod == 0x02) {
    cpu->displacement = (int32_t)x64cpu_fetch32(cpu);
  } else if (cpu->modrmbyte.mod == 0x01) {
    cpu->displacement = (int8_t)x64cpu_fetch8(cpu);
  } else if (cpu->modrmbyte.mod == 0x00) {
    if (cpu->modrmbyte.rm == 0x05 ||
        (cpu->modrmbyte.rm == 0x04 && cpu->sibbyte.reg == 0x05)) {
      cpu->displacement = (int32_t)x64cpu_fetch32(cpu);
    }
  }
}

/**
 * Decode instruction; return the operation to be executed, if any
 */
int x64cpu_decode_opcode(x64cpu *cpu, uint8_t opcode,
                         const x64cpu_opcode_definition *opdef) {
  int i;

  /* Need modrm/sib/displacement bytes ? */
  if (opdef->need_modrmbyte) {
    x64cpu_decode_modrm_sib(cpu, opcode);
  }

  /* Operation instruction set */
  cpu->operation_instruction_set = X64CPU_INSTR_SET_GENERAL;

  while (1) {
    /* Check for groups - operation given by the modrmbyte */
    if (opdef->operation == X64CPU_OP_GROUP) {
      opdef = &(opdef->group[cpu->modrmbyte.reg]);
    } else {
      break;
    }
  }

  /* Decode parameters */
  for (i = 0; i < 4; i++) {
    if (opdef->parameters[i].type == X64CPU_PT_NONE) {
      break;
    }

    int optype = opdef->parameters[i].type;
    int opsize = opdef->parameters[i].size;

    x64cpu_decode_operand_type(cpu, i, optype, opsize);

    /* Was there a decode error ? */
    if (cpu->cpu_exception.code != X64CPU_EXCEPTION_NONE) {
      return X64CPU_OP_INVALID;
    }
  }

#if 0
    for (i = 0; i < 4; i++) {
        x64cpu_operand_address_sib_to_abs(cpu, i);
    }
#endif

  cpu->current_op_def = opdef;

  /* Return operation code */
  return opdef->operation;
}

int x64cpu_decode_opcode_1byte(x64cpu *cpu, uint8_t opcode) {
  const x64cpu_opcode_definition opdef = x64cpu_opcode_def_1byte[opcode];
  return x64cpu_decode_opcode(cpu, opcode, &opdef);
}

int x64cpu_decode_opcode_0F_2byte(x64cpu *cpu, uint8_t opcode) {
  const x64cpu_opcode_definition opdef = x64cpu_opcode_def_0F_2byte[opcode];
  return x64cpu_decode_opcode(cpu, opcode, &opdef);
}

/*---------------------------------------------------------------------------*/
/* Instruction decoding finished. Execution begins                           */
/*---------------------------------------------------------------------------*/

void set_carry(uint64_t &flags, bool val) {
  if (val)
    flags |= X64FLAG_CF;
  else
    flags &= ~X64FLAG_CF;
}
void set_parity(uint64_t &flags, bool val) {
  if (val)
    flags |= X64FLAG_PF;
  else
    flags &= ~X64FLAG_PF;
}
void set_zero(uint64_t &flags, bool val) {
  if (val)
    flags |= X64FLAG_ZF;
  else
    flags &= ~X64FLAG_ZF;
}
void set_sign(uint64_t &flags, bool val) {
  if (val)
    flags |= X64FLAG_SF;
  else
    flags &= ~X64FLAG_SF;
}
void set_overflow(uint64_t &flags, bool val) {
  if (val)
    flags |= X64FLAG_OF;
  else
    flags &= ~X64FLAG_OF;
}
bool is_carry(uint64_t flags) { return flags & X64FLAG_CF; }

void update_flags_add(uint64_t &flags, uint64_t v1, uint64_t v2,
                      uint64_t &result, uint8_t size) {
  uint64_t digits = size * 8 - 1, mask;
  bool sign1 = v1 >> digits;
  bool sign2 = v2 >> digits;
  bool signr = (result >> digits) & 1;

  if (size < 8) {
    set_carry(flags, result >> digits + 1);
    mask = (1ull << digits + 1) - 1;
  } else {
    set_carry(flags, (sign1 == sign2 && sign1) || (sign1 != sign2 && !signr));
    mask = ~0ull;
  }
  set_parity(flags, 1 ^ __builtin_parityll(result & 255));
  // adjust flag ignored
  set_zero(flags, (result & mask) == 0);
  set_sign(flags, signr);
  set_overflow(flags, sign1 == sign2 && sign1 != signr);
  result &= mask;
}

void update_flags_sub(uint64_t &flags, uint64_t v1, uint64_t v2,
                      uint64_t &result, uint8_t size) {
  uint64_t digits = size * 8 - 1, mask;
  bool sign1 = v1 >> digits;
  bool sign2 = v2 >> digits;
  bool signr = (result >> digits) & 1;

  if (size < 8) {
    set_carry(flags, result >> digits + 1);
    mask = (1ull << digits + 1) - 1;
  } else {
    set_carry(flags, (sign1 == sign2 && signr) || (sign1 != sign2 && sign2));
    mask = ~0ull;
  }
  set_parity(flags, 1 ^ __builtin_parityll(result & 255));
  // adjust flag ignored
  set_zero(flags, (result & mask) == 0);
  set_sign(flags, signr);
  set_overflow(flags, sign1 != sign2 && sign1 != signr);
  result &= mask;
}

void update_flags_result(uint64_t &flags, uint64_t result, uint8_t size) {
  uint64_t digits = size * 8 - 1;
  bool signr = result >> digits;
  set_parity(flags, __builtin_parity(result & 255));
  set_zero(flags, result == 0);
  set_sign(flags, signr);
}

void x64cpu_alu(x64cpu *cpu, int op) {
  uint64_t flags = cpu->regs.rflags;
  uint8_t size = 0;

  /* Fix operands size when mismatch */
  if (cpu->op[1].type != X64CPU_OPT_NONE) {
    if (cpu->op[0].size != cpu->op[1].size) {
      /* TODO: extend operand 2 to size of operand 1, right ? */
      x64cpu_operand_extend(cpu, 1, cpu->op[0].size, cpu->op[1].sign_extend);

      /* Now it should work... */
      if (cpu->op[0].size != cpu->op[1].size) {
        INTERNAL_ERROR();
      }
    }
  }

  size = cpu->op[0].size;

  switch (op) {
  case X64CPU_ALU_ADD: {
    uint64_t v1 = 0, v2 = 0, vr;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    vr = v1 + v2;
    update_flags_add(flags, v1, v2, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_OR: {
    uint64_t v1 = 0, v2 = 0, vr;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    vr = v1 | v2;
    set_overflow(flags, 0);
    set_carry(flags, 0);
    update_flags_result(flags, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_ADC: {
    uint64_t v1 = 0, v2 = 0, vr;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    vr = v1 + v2 + is_carry(flags);
    update_flags_add(flags, v1, v2, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_SBB: {
    uint64_t v1 = 0, v2 = 0, vr;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    vr = v1 - v2 - is_carry(flags);
    update_flags_sub(flags, v1, v2, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_AND: {
    uint64_t v1 = 0, v2 = 0, vr;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    vr = v1 & v2;
    set_overflow(flags, 0);
    set_carry(flags, 0);
    update_flags_result(flags, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_SUB: {
    uint64_t v1 = 0, v2 = 0, vr;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    vr = v1 - v2;
    update_flags_sub(flags, v1, v2, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_XOR: {
    uint64_t v1 = 0, v2 = 0, vr;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    vr = v1 ^ v2;
    set_overflow(flags, 0);
    set_carry(flags, 0);
    update_flags_result(flags, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_CMP: {
    uint64_t v1 = 0, v2 = 0, vr;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    vr = v1 - v2;
    update_flags_sub(flags, v1, v2, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
  } break;

  case X64CPU_ALU_TEST: {
    uint64_t v1 = 0, v2 = 0, vr;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    vr = v1 & v2;
    set_overflow(flags, 0);
    set_carry(flags, 0);
    update_flags_result(flags, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
  } break;

  case X64CPU_ALU_XCHG: {
    uint64_t v1 = 0, v2 = 0;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);

    x64cpu_operand_write_u64(cpu, 1, v1, size);
    x64cpu_operand_write_u64(cpu, 0, v2, size);
  } break;

  case X64CPU_ALU_MOV: {
    uint64_t val = 0;
    x64cpu_operand_read_u64(cpu, 1, val, size);
    x64cpu_operand_write_u64(cpu, 0, val, size);
  } break;

  case X64CPU_ALU_SHL:
  case X64CPU_ALU_SAL: {
    uint64_t v1 = 0, vr;
    uint8_t v2;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read(cpu, 1, (uint8_t *)&v2, 1);
    vr = v1 << v2;
    set_carry(flags, v1 << v2 - 1 >> size * 8 - 1);
    if (v2 == 1)
      set_overflow(flags, (vr >> size * 8 - 1) ^ is_carry(flags));
    update_flags_result(flags, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_SHR: {
    uint64_t v1 = 0, vr;
    uint8_t v2;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read(cpu, 1, (uint8_t *)&v2, 1);
    vr = v1 >> v2;
    set_carry(flags, v1 >> v2 - 1 & 1);
    if (v2 == 1)
      set_overflow(flags, 0);
    update_flags_result(flags, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_SAR: {
    uint64_t v1 = 0, vr;
    uint8_t v2;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read(cpu, 1, (uint8_t *)&v2, 1);
    if (v1 >> size * 8 - 1)
      vr = (v1 >> v2) | ((1ull << v2) - 1) << size * 8 - v2;
    else
      vr = v1 >> v2;
    set_carry(flags, v1 >> v2 - 1 & 1);
    if (v2 == 1)
      set_overflow(flags, 0);
    update_flags_result(flags, vr, size);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, vr, size);
  } break;

  case X64CPU_ALU_NOT: {
    uint64_t value;
    x64cpu_operand_read_u64(cpu, 0, value, size);
    value = ~value;
    x64cpu_operand_write_u64(cpu, 0, value, size);
  } break;

  case X64CPU_ALU_MUL1:
  case X64CPU_ALU_IMUL1:
  case X64CPU_ALU_MUL:
  case X64CPU_ALU_IMUL: {
    uint64_t rax = 0, rdx = 0, imm = 0;
    int flags_set = 0;

    x64cpu_operand_read_u64(cpu, 0, rax, size);
    x64cpu_operand_read_u64(cpu, 2, imm, size);

    if (op == X64CPU_ALU_MUL1 || op == X64CPU_ALU_MUL) {
      uint128_t res = (uint128_t)rax * (uint128_t)imm;
      rax = ((uint64_t *)&res)[0];
      rdx = ((uint64_t *)&res)[0];
    } else {
      int128_t res = (int128_t)(int64_t)rax * (int128_t)(int64_t)imm;
      rax = ((uint64_t *)&res)[0];
      rdx = ((uint64_t *)&res)[0];
    }

    switch (size) {
    case 1:
      if ((rax & 0xff00) != 0) {
        flags_set = 1;
      }
      break;
    case 2:
      rdx = (rax & 0xffff0000) >> 16;
      rax = (rax & 0x0000ffff);
      if (rdx) {
        flags_set = 1;
      }
      break;
    case 4:
      rdx = (rax & 0xffffffff00000000) >> 32;
      rax = (rax & 0x00000000ffffffff);
      if (rdx) {
        flags_set = 1;
      }
      break;
    case 8:
      if (rdx) {
        flags_set = 1;
      }
      break;
    }

    if (flags_set) {
      cpu->regs.rflags |= (X64FLAG_CF | X64FLAG_OF);
    } else {
      cpu->regs.rflags &= ~(X64FLAG_CF | X64FLAG_OF);
    }

    if (size == 1) {
      x64cpu_operand_write_u64(cpu, 0, rax, 2);
    } else {
      x64cpu_operand_write_u64(cpu, 0, rax, size);
      if (op == X64CPU_ALU_MUL1 || op == X64CPU_ALU_IMUL1) {
        x64cpu_operand_write_u64(cpu, 1, rdx, size);
      }
    }
  } break;

  case X64CPU_ALU_DIV:
  case X64CPU_ALU_IDIV: {
    uint64_t rax = 0, rdx = 0, imm = 0;

    if (size > 1) {
      x64cpu_operand_read_u64(cpu, 1, rdx, size);
    }
    x64cpu_operand_read_u64(cpu, 2, rax, size);
    x64cpu_operand_read_u64(cpu, 3, imm, size);

    if (imm == 0) {
      x64cpu_exception(cpu, X64CPU_EXCEPTION_DE);
      break;
    }

    if (rdx) {
      if (op == X64CPU_ALU_DIV) {
        uint128_t val = (uint128_t)rdx << 64 | (uint128_t)rax;
        uint128_t a = val / imm, b = val % imm;
        rax = ((uint64_t *)&a)[0];
        rdx = b;
      } else {
        int128_t val = (uint128_t)rdx << 64 | (uint128_t)rax;
        int128_t a = val / (int64_t)imm, b = val % (int64_t)imm;
        rax = ((uint64_t *)&a)[0];
        rdx = b;
      }
    } else {
      if (op == X64CPU_ALU_DIV) {
        uint64_t a = rax / imm, b = rax % imm;
        rax = a;
        rdx = b;
      } else {
        uint64_t a = (int64_t)rax / (int64_t)imm,
                 b = (int64_t)rax % (int64_t)imm;
        rax = a;
        rdx = b;
      }
    }

    x64cpu_operand_write_u64(cpu, 0, rax, size);
    x64cpu_operand_write_u64(cpu, 1, rdx, size);
  } break;

  case X64CPU_ALU_BT: {
    uint64_t v1 = 0, v2 = 0;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    set_carry(flags, v1 >> (v2 & size * 8 - 1) & 1);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
  } break;

  case X64CPU_ALU_BTS: {
    uint64_t v1 = 0, v2 = 0;
    x64cpu_operand_read_u64(cpu, 0, v1, size);
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    v2 &= size * 8 - 1;
    set_carry(flags, v1 >> v2 & 1);
    v1 |= 1ull << v2;
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, v1, size);
  } break;

  case X64CPU_ALU_BSF: {
    uint64_t v1 = 0, v2 = 0;
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    v1 = __builtin_ctzll(v2);
    set_zero(flags, v1 == 0);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, v1, size);
  } break;

  case X64CPU_ALU_BSR: {
    uint64_t v1 = 0, v2 = 0;
    x64cpu_operand_read_u64(cpu, 1, v2, size);
    v1 = size * 8 - 1 - __builtin_clzll(v2);
    set_zero(flags, v1 == 0);
    update_flags(cpu, flags, X64CPU_ALU_DEFAULT_FLAGS);
    x64cpu_operand_write_u64(cpu, 0, v1, size);
  } break;

  default:
    /* TODO: ALU operation not implemented */
    x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
    break;
  }
}

/** Push to stack, decreasing RSP */
void x64cpu_push(x64cpu *cpu, int ope_index) {
  uint64_t value = 0x00;

  /* On 64bit stack always operates on 64bit mode */
  x64cpu_operand_read(cpu, ope_index, (uint8_t *)&value,
                      cpu->op[ope_index].size);
  cpu->regs.rsp -= 8;
  x64cpu_memory_write(cpu, cpu->regs.rsp, (uint8_t *)&value, 8,
                      X64CPU_MEM_ACCESS_WRITE);
}

/** Pop from stack, increasing RSP */
void x64cpu_pop(x64cpu *cpu, int ope_index) {
  uint64_t value = 0x00;

  /* On 64bit stack always operates on 64bit mode */
  x64cpu_memory_read(cpu, cpu->regs.rsp, (uint8_t *)&value, 8,
                     X64CPU_MEM_ACCESS_READ);
  cpu->regs.rsp += 8;
  x64cpu_operand_write(cpu, ope_index, (uint8_t *)&value,
                       cpu->op[ope_index].size);
}

#define FLAG_ISSET(flags, flag) ((flags & flag) ? 1 : 0)

/** Check if the condition specified by the condition jump is true or false */
int x64cpu_condition(x64cpu *cpu, uint8_t opcode) {
  int cond = 0;

  switch (opcode & 0x0F) {
  /* JO */
  case 0x00:
    cond = ((cpu->regs.rflags & X64FLAG_OF) != 0);
    break;
  /* JNO */
  case 0x01:
    cond = ((cpu->regs.rflags & X64FLAG_OF) == 0);
    break;
  /* JB / JNAE / JC */
  case 0x02:
    cond = ((cpu->regs.rflags & X64FLAG_CF) != 0);
    break;
  /* JNB / JAE / JNC */
  case 0x03:
    cond = ((cpu->regs.rflags & X64FLAG_CF) == 0);
    break;
  /* JZ / JE */
  case 0x04:
    cond = ((cpu->regs.rflags & X64FLAG_ZF) != 0);
    break;
  /* JNZ / JNE */
  case 0x05:
    cond = ((cpu->regs.rflags & X64FLAG_ZF) == 0);
    break;
  /* JBE / JNA */
  case 0x06:
    cond = ((cpu->regs.rflags & X64FLAG_CF) != 0 ||
            (cpu->regs.rflags & X64FLAG_ZF) != 0);
    break;
  /* JNBE / JA */
  case 0x07:
    cond = ((cpu->regs.rflags & X64FLAG_CF) == 0 &&
            (cpu->regs.rflags & X64FLAG_ZF) == 0);
    break;
  /* JS */
  case 0x08:
    cond = ((cpu->regs.rflags & X64FLAG_SF) != 0);
    break;
  /* JNS */
  case 0x09:
    cond = ((cpu->regs.rflags & X64FLAG_SF) == 0);
    break;
  /* JP / JPE */
  case 0x0A:
    cond = ((cpu->regs.rflags & X64FLAG_PF) != 0);
    break;
  /* JNP / JPO */
  case 0x0B:
    cond = ((cpu->regs.rflags & X64FLAG_PF) == 0);
    break;
  /* JL / JNGE */
  case 0x0C:
    cond = (FLAG_ISSET(cpu->regs.rflags, X64FLAG_SF) !=
            FLAG_ISSET(cpu->regs.rflags, X64FLAG_OF));
    break;
  /* JNL / JGE */
  case 0x0D:
    cond = (FLAG_ISSET(cpu->regs.rflags, X64FLAG_SF) ==
            FLAG_ISSET(cpu->regs.rflags, X64FLAG_OF));
    break;
  /* JLE / JNG */
  case 0x0E:
    cond = (FLAG_ISSET(cpu->regs.rflags, X64FLAG_ZF) == 1) ||
           (FLAG_ISSET(cpu->regs.rflags, X64FLAG_SF) !=
            FLAG_ISSET(cpu->regs.rflags, X64FLAG_OF));
    break;
  /* JNLE / JG */
  case 0x0F:
    cond = (FLAG_ISSET(cpu->regs.rflags, X64FLAG_ZF) == 0) &&
           (FLAG_ISSET(cpu->regs.rflags, X64FLAG_SF) ==
            FLAG_ISSET(cpu->regs.rflags, X64FLAG_OF));
    break;
  }

  return cond;
}

/** Helper function for LOOPs and REPs ;
 *  @return 1 - if condition is good, 0 - if false
 */
int x64cpu_loop(x64cpu *cpu, int test_zf, int neg_test_zf) {
  int ret = 1;

  /* Decrement CX */
  cpu->regs.rcx -= 1;

  /* Check if CX != 0 */
  if (cpu->regs.rcx == 0) {
    ret = 0;
  }

  /* Check flags */
  if (test_zf) {
    if (neg_test_zf) {
      if ((cpu->regs.rflags & X64FLAG_ZF) != 0) {
        ret = 0;
      }
    } else {
      if ((cpu->regs.rflags & X64FLAG_ZF) == 0) {
        ret = 0;
      }
    }
  }

  return ret;
}

/* Add the offset from the specified operand to the current RIP position;
 * the operand is sign extended to 64bit
 */
void x64cpu_jump_offset(x64cpu *cpu, int ope_index) {
  int64_t offset = 0;

  x64cpu_operand_read(cpu, ope_index, (uint8_t *)&offset,
                      cpu->op[ope_index].size);

  switch (cpu->op[ope_index].size) {
  case 1: {
    int8_t tmp;
    x64cpu_operand_read(cpu, ope_index, (uint8_t *)&tmp, 1);
    offset = (int64_t)tmp;
  } break;

  case 2: {
    int16_t tmp;
    x64cpu_operand_read(cpu, ope_index, (uint8_t *)&tmp, 2);
    offset = (int64_t)tmp;
  } break;

  case 4: {
    int32_t tmp;
    x64cpu_operand_read(cpu, ope_index, (uint8_t *)&tmp, 4);
    offset = (int64_t)tmp;
  } break;

  case 8: {
    int64_t tmp;
    x64cpu_operand_read(cpu, ope_index, (uint8_t *)&tmp, 8);
    offset = (int64_t)tmp;
  } break;

  default:
    INTERNAL_ERROR();
    break;
  }

  cpu->regs.rip += (int64_t)offset;
}

void x64cpu_operation_execute_string(x64cpu *cpu, int operation,
                                     uint8_t opcode) {
  int ptr_increment = 1;

  switch (cpu->op[0].size) {
  case 1:
    ptr_increment = 1;
    break;
  case 2:
    ptr_increment = 2;
    break;
  case 4:
    ptr_increment = 4;
    break;
  case 8:
    ptr_increment = 8;
    break;
  }

  if (cpu->regs.rflags & X64FLAG_DF) {
    ptr_increment = -ptr_increment;
  }

  switch (operation) {
  case X64CPU_OP_MOVS:
    x64cpu_alu(cpu, X64CPU_ALU_MOV);
    break;
  case X64CPU_OP_CMPS:
    x64cpu_alu(cpu, X64CPU_ALU_CMP);
    break;
  case X64CPU_OP_SCAS:
    x64cpu_alu(cpu, X64CPU_ALU_CMP);
    break;
  case X64CPU_OP_STOS:
    x64cpu_alu(cpu, X64CPU_ALU_MOV);
    break;
  case X64CPU_OP_LODS:
    x64cpu_alu(cpu, X64CPU_ALU_MOV);
    break;
  default:
    break;
  }

  /* Increment pointers (RSI, RDI) ; only if pointer; doesn't increment AX for
   * STOS */
  x64cpu_operand_ptr_increment(cpu, 0, ptr_increment);
  x64cpu_operand_ptr_increment(cpu, 1, ptr_increment);
}

/**
 * Execute the instruction.
 * Must allow recursive call without consequences (see GRP3 calling IMUL, IDIV,
 * ...)
 */
void x64cpu_operation_execute(x64cpu *cpu, int operation, uint8_t opcode) {
  /* Don't allow repeat flags with non-string instructions ; allow with ret
   * because AMD is weird */
  if (cpu->repeat_prefix != 0) {
    switch (operation) {
    case X64CPU_OP_MOVS:
    case X64CPU_OP_STOS:
    case X64CPU_OP_LODS:
      break;

    case X64CPU_OP_CMPS:
    case X64CPU_OP_SCAS:
      /* The repeat prefix for this instruction will also use ZF */
      cpu->repeat_use_zf = 1;
      break;

    case X64CPU_OP_IRET:
    case X64CPU_OP_RETN:
    case X64CPU_OP_RETF:
    case X64CPU_OP_CJMP:
      /* HACK: AMD64 uses repz ret ... should ignore repz here */
      /* HACK: seen for jumps also */
      cpu->repeat_prefix = 0;
      break;

    default:
      x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
      return;
      break;
    }
  }

  switch (operation) {
  case X64CPU_OP_NOOP:
    /* Noop */
    break;

  case X64CPU_OP_ADD:
    x64cpu_alu(cpu, X64CPU_ALU_ADD);
    break;
  case X64CPU_OP_ADC:
    x64cpu_alu(cpu, X64CPU_ALU_ADC);
    break;
  case X64CPU_OP_AND:
    x64cpu_alu(cpu, X64CPU_ALU_AND);
    break;
  case X64CPU_OP_XOR:
    x64cpu_alu(cpu, X64CPU_ALU_XOR);
    break;
  case X64CPU_OP_OR:
    x64cpu_alu(cpu, X64CPU_ALU_OR);
    break;
  case X64CPU_OP_SBB:
    x64cpu_alu(cpu, X64CPU_ALU_SBB);
    break;
  case X64CPU_OP_SUB:
    x64cpu_alu(cpu, X64CPU_ALU_SUB);
    break;
  case X64CPU_OP_CMP:
    x64cpu_alu(cpu, X64CPU_ALU_CMP);
    break;
  case X64CPU_OP_TEST:
    x64cpu_alu(cpu, X64CPU_ALU_TEST);
    break;
  case X64CPU_OP_XCHG:
    x64cpu_alu(cpu, X64CPU_ALU_XCHG);
    break;

  case X64CPU_OP_NOT:
    x64cpu_alu(cpu, X64CPU_ALU_NOT);
    break;
  case X64CPU_OP_NEG: {
    uint64_t tmp = 0;
    x64cpu_operand_read(cpu, 0, (uint8_t *)&tmp, cpu->op[0].size);
    tmp = (0 - tmp);
    x64cpu_operand_write(cpu, 0, (uint8_t *)&tmp, cpu->op[0].size);
  } break;

  case X64CPU_OP_MOV:
    x64cpu_alu(cpu, X64CPU_ALU_MOV);
    break;

  case X64CPU_OP_LEA: {
    uint64_t addr = x64cpu_operand_get_address(cpu, 1);
    x64cpu_operand_write(cpu, 0, (uint8_t *)&addr, cpu->op[0].size);
  } break;

  case X64CPU_OP_MOVSX:
  case X64CPU_OP_MOVSXD:
    x64cpu_operand_extend(cpu, 1, cpu->op[0].size, 1);
    x64cpu_alu(cpu, X64CPU_ALU_MOV);
    break;

  case X64CPU_OP_MOVS:
  case X64CPU_OP_CMPS:
  case X64CPU_OP_SCAS:
  case X64CPU_OP_STOS:
  case X64CPU_OP_LODS:
    x64cpu_operation_execute_string(cpu, operation, opcode);
    break;

  case X64CPU_OP_CMOV:
    if (x64cpu_condition(cpu, opcode)) {
      x64cpu_alu(cpu, X64CPU_ALU_MOV);
    }
    break;

  case X64CPU_OP_PUSH:
    x64cpu_push(cpu, 0);
    break;
  case X64CPU_OP_POP:
    x64cpu_pop(cpu, 0);
    break;

  case X64CPU_OP_JMP:
    x64cpu_jump_offset(cpu, 0);
    break;

  case X64CPU_OP_CJMP:
    if (x64cpu_condition(cpu, opcode)) {
      x64cpu_jump_offset(cpu, 0);
    }
    break;

  case X64CPU_OP_LOOP:
  case X64CPU_OP_LOOPE:
  case X64CPU_OP_LOOPNE: {
    int test_zf = 0, neg_test_zf = 0;

    /* Check flags */
    switch ((int)operation) {
    case X64CPU_OP_LOOPE:
      test_zf = 1;
      break;
    case X64CPU_OP_LOOPNE:
      test_zf = 1;
      neg_test_zf = 1;
      break;
    }

    /* Jump to offset */
    if (x64cpu_loop(cpu, test_zf, neg_test_zf) == 1) {
      x64cpu_jump_offset(cpu, 0);
    }
  } break;

  case X64CPU_OP_JRCX: {
    uint64_t dummy = 0;

    /* Jump if RCX is 0 ; use an operand because it depends on operand size */
    x64cpu_operand_read(cpu, 1, (uint8_t *)&dummy, 8);

    if (dummy == 0) {
      x64cpu_jump_offset(cpu, 0);
    }
  } break;

  case X64CPU_OP_MUL:
    x64cpu_alu(cpu, X64CPU_ALU_MUL);
    break;
  case X64CPU_OP_MUL1:
    x64cpu_alu(cpu, X64CPU_ALU_MUL1);
    break;

  case X64CPU_OP_IMUL:
    x64cpu_alu(cpu, X64CPU_ALU_IMUL);
    break;
  case X64CPU_OP_IMUL1:
    x64cpu_alu(cpu, X64CPU_ALU_IMUL1);
    break;

  case X64CPU_OP_MUL3: {
    uint64_t val = 0;
    x64cpu_operand_read(cpu, 1, (uint8_t *)&val, cpu->op[1].size);
    x64cpu_operand_write(cpu, 0, (uint8_t *)&val, cpu->op[0].size);
    x64cpu_alu(cpu, X64CPU_ALU_MUL);
  } break;

  case X64CPU_OP_IMUL3: {
    uint64_t val = 0;
    x64cpu_operand_read(cpu, 1, (uint8_t *)&val, cpu->op[1].size);
    x64cpu_operand_write(cpu, 0, (uint8_t *)&val, cpu->op[0].size);
    x64cpu_alu(cpu, X64CPU_ALU_IMUL);
  } break;

  case X64CPU_OP_DIV:
    x64cpu_alu(cpu, X64CPU_ALU_DIV);
    break;
  case X64CPU_OP_IDIV:
    x64cpu_alu(cpu, X64CPU_ALU_IDIV);
    break;

  case X64CPU_OP_INC:
  case X64CPU_OP_DEC: {
    uint64_t val = 1;
    x64cpu_operand_set_imm(cpu, 1, (uint8_t *)&val, 1);
    if (operation == X64CPU_OP_INC) {
      x64cpu_alu(cpu, X64CPU_ALU_ADD);
    } else {
      x64cpu_alu(cpu, X64CPU_ALU_SUB);
    }
  } break;

  /* Call */
  case X64CPU_OP_CALL: {
    /* Push RIP */
    x64cpu_operand_set_imm(cpu, 3, (uint8_t *)&cpu->regs.rip, 8);
    x64cpu_push(cpu, 3);

    /* Jump to address */
    x64cpu_jump_offset(cpu, 0);
  } break;

  /* Call indirect */
  case X64CPU_OP_CALL_I: {
    uint64_t address = 0;

    /* Get address */
    x64cpu_operand_read(cpu, 0, (uint8_t *)&address, cpu->op[0].size);

    /* Push RIP */
    x64cpu_operand_set_imm(cpu, 3, (uint8_t *)&cpu->regs.rip, 8);
    x64cpu_push(cpu, 3);

    /* Jump to address */
    cpu->regs.rip = address;
  } break;

  /* Jump indirect */
  case X64CPU_OP_JMP_I: {
    uint64_t address = 0;

    /* Get address */
    x64cpu_operand_read(cpu, 0, (uint8_t *)&address, cpu->op[0].size);

    /* Jump to address */
    cpu->regs.rip = address;
  } break;

  /* Return */
  case X64CPU_OP_RETN:
  case X64CPU_OP_RETF: {
    uint64_t return_address = 0;

    /* Pop return address ; 64 bits on 64 */
    x64cpu_operand_set_imm(cpu, 3, (uint8_t *)&return_address, 8);
    x64cpu_pop(cpu, 3);
    x64cpu_operand_read(cpu, 3, (uint8_t *)&return_address, 8);

    // TODO: Return FAR on 64 bit .... POP CS ??? */

    /* Do we have a parameter ? */
    if (cpu->op[0].type > 0) {
      /* Remove bytes from stack */
      uint16_t bytes = 0;
      x64cpu_operand_read(cpu, 0, (uint8_t *)&bytes, 2);

      cpu->regs.rsp += bytes;
    }

    /* Return */
    cpu->regs.rip = return_address;
  } break;

  /* Enter */
  case X64CPU_OP_ENTER: {
    /* Push RBP */
    x64cpu_select_operand_reg(cpu, 3, X64CPU_REG_GP, 0, X64CPU_REGISTER_RBP, 8);
    x64cpu_push(cpu, 3);

    /* Mov RSP, RBP */
    cpu->regs.rbp = cpu->regs.rsp;

    /* Allocate num variables */
    uint16_t num_vars = 0;
    x64cpu_operand_read(cpu, 0, (uint8_t *)&num_vars, 2);

    cpu->regs.rsp -= num_vars;

    // TODO: nested level ??
  } break;

  /* Leave */
  case X64CPU_OP_LEAVE: {
    /* Mov RBP, RSP */
    cpu->regs.rsp = cpu->regs.rbp;

    /* Pop RBP */
    x64cpu_select_operand_reg(cpu, 3, X64CPU_REG_GP, 0, X64CPU_REGISTER_RBP, 8);
    x64cpu_pop(cpu, 3);
  } break;

  /* INT - Call to interrupt procedure */
  case X64CPU_OP_INT: {
    uint8_t interrupt_number = 0;
    x64cpu_operand_read(cpu, 0, (uint8_t *)&interrupt_number, 1);

#if 0
                /* Push flags */
                x64cpu_select_operand_reg(cpu, 3, X64CPU_REG_F, 0, 0, 8);
                x64cpu_push(cpu, 3);

                /* Clear IF and TF */
                cpu->regs.rflags &= ~(X64FLAG_TF | X64FLAG_IF);
#endif

    /* Interrupts must be intercepted by the client code */
    cpu->interrupt_number = interrupt_number;
    cpu->execution_result = X64CPU_RES_SOFTINT;
    cpu->is_halted = 1; /* Client must resume */
  } break;

  /* IRET - Return from interrupt procedure */
#if 0
        case X64CPU_OP_IRET: {
                uint64_t return_address = 0;

                /* Pop flags */
                x64cpu_select_operand_reg(cpu, 3, X64CPU_REG_F, 0, 0, 8);
                x64cpu_pop(cpu, 3);

                /* Pop return address ; 64 bits on 64 */
                x64cpu_operand_set_imm(cpu, 3, (uint8_t*)&return_address, 8);
                x64cpu_pop(cpu, 3);
                x64cpu_operand_read(cpu, 3, (uint8_t*)&return_address, 8);

                // TODO: Return FAR on 64 bit .... POP CS ??? */

                /* Return */
                cpu->regs.rip = return_address;
            }
            break;
#endif

  /* SYSCALL */
  case X64CPU_OP_SYSCALL: {
    /* syscalls must be intercepted by the client code */
    cpu->execution_result = X64CPU_RES_SYSCALL;
  } break;

  /* XLAT AL, (DS:rBX + AL) */
  case X64CPU_OP_XLAT: {
    uint64_t address = 0;
    uint8_t al = 0;
    uint8_t addr_size = x64cpu_decode_address_size(cpu);

    x64cpu_select_operand_reg(cpu, 3, X64CPU_REG_GP, 0, X64CPU_REGISTER_RBX,
                              addr_size);
    x64cpu_select_operand_reg(cpu, 4, X64CPU_REG_GP, 0, X64CPU_REGISTER_RAX, 1);

    x64cpu_operand_read(cpu, 4, (uint8_t *)&al, 1);
    x64cpu_operand_read(cpu, 3, (uint8_t *)&address, addr_size);
    // TODO: DS segment on 64 ?? */
    address += al;

    x64cpu_memory_read(cpu, address, (uint8_t *)&al, 1, X64CPU_MEM_ACCESS_READ);
    x64cpu_operand_write(cpu, 4, (uint8_t *)&al, 1);
  } break;

  case X64CPU_OP_ROL:
    x64cpu_alu(cpu, X64CPU_ALU_ROL);
    break;
  case X64CPU_OP_ROR:
    x64cpu_alu(cpu, X64CPU_ALU_ROR);
    break;
  case X64CPU_OP_RCL:
    x64cpu_alu(cpu, X64CPU_ALU_RCL);
    break;
  case X64CPU_OP_RCR:
    x64cpu_alu(cpu, X64CPU_ALU_RCR);
    break;
  case X64CPU_OP_SHL:
    x64cpu_alu(cpu, X64CPU_ALU_SHL);
    break;
  case X64CPU_OP_SHR:
    x64cpu_alu(cpu, X64CPU_ALU_SHR);
    break;
  case X64CPU_OP_SAR:
    x64cpu_alu(cpu, X64CPU_ALU_SAR);
    break;

  /* CMC - complement carry flag */
  case X64CPU_OP_CMC:
    cpu->regs.rflags ^= X64FLAG_CF;
    break;

  case X64CPU_OP_CLC:
  case X64CPU_OP_STC:
    if (operation == X64CPU_OP_CLC) {
      cpu->regs.rflags &= ~X64FLAG_CF;
    } else {
      cpu->regs.rflags |= X64FLAG_CF;
    }
    break;

  case X64CPU_OP_CLI:
  case X64CPU_OP_STI:
    if (operation == X64CPU_OP_CLI) {
      cpu->regs.rflags &= ~X64FLAG_IF;
    } else {
      cpu->regs.rflags |= X64FLAG_IF;
    }
    break;

  case X64CPU_OP_CLD:
  case X64CPU_OP_STD:
    if (operation == X64CPU_OP_CLD) {
      cpu->regs.rflags &= ~X64FLAG_DF;
    } else {
      cpu->regs.rflags |= X64FLAG_DF;
    }
    break;

  /* cwtl / cltq / cbtw */
  case X64CPU_OP_CONV:
  case X64CPU_OP_CONV2: {
    int reg_size = 0, new_size = 0;

    /* REX.W ; convert dw to qw */
    if (cpu->prefix_flags & X64CPU_PREFIX_REX_W) {
      reg_size = 4;
      new_size = 8;
    }
    /* Operand-size prefix; convert b to w */
    else if (cpu->prefix_flags & X64CPU_PREFIX_OP_SIZE) {
      reg_size = 1;
      new_size = 2;
    } else {
      reg_size = 2;
      new_size = 4;
    }

    /* Convert rAX to rAX */
    if (operation == X64CPU_OP_CONV) {
      x64cpu_select_operand_reg(cpu, 0, X64CPU_REG_GP, 0, X64CPU_REGISTER_RAX,
                                reg_size);
      x64cpu_operand_extend(cpu, 0, new_size, 1);
    }
    /* Convert to composite RAX.RDX ; aka move sign to RDX */
    else {
      uint64_t v1 = 0, v2 = 0;

      x64cpu_select_operand_reg(cpu, 0, X64CPU_REG_GP, 0, X64CPU_REGISTER_RAX,
                                reg_size);
      x64cpu_select_operand_reg(cpu, 1, X64CPU_REG_GP, 0, X64CPU_REGISTER_RDX,
                                reg_size);

      x64cpu_operand_read(cpu, 0, (uint8_t *)&v1, new_size);

      if (new_size == 2) {
        v2 = (v1 & 0x8000) ? -1 : 0;
      } else if (new_size == 4) {
        v2 = (v1 & 0x80000000) ? -1 : 0;
      } else if (new_size == 8) {
        v2 = (v1 & 0x8000000000000000) ? -1 : 0;
      }

      x64cpu_operand_write(cpu, 1, (uint8_t *)&v2, new_size);
    }
  } break;

  case X64CPU_OP_CSET: {
    uint8_t val = x64cpu_condition(cpu, opcode);
    x64cpu_operand_write(cpu, 0, (uint8_t *)&val, 1);
  } break;

  case X64CPU_OP_BT:
    x64cpu_alu(cpu, X64CPU_ALU_BT);
    break;
  case X64CPU_OP_BTS:
    x64cpu_alu(cpu, X64CPU_ALU_BTS);
    break;
  case X64CPU_OP_BSF:
    x64cpu_alu(cpu, X64CPU_ALU_BSF);
    break;
  case X64CPU_OP_BSR:
    x64cpu_alu(cpu, X64CPU_ALU_BSR);
    break;

  case X64CPU_OP_CMPXCHG: {
    uint64_t src = 0, dst = 0;

    x64cpu_operand_read(cpu, 0, (uint8_t *)&dst, cpu->op[0].size);
    x64cpu_operand_read(cpu, 2, (uint8_t *)&src, cpu->op[2].size);

    x64cpu_alu(cpu, X64CPU_ALU_CMP);

    if ((cpu->regs.rflags & X64FLAG_ZF) != 0) {
      x64cpu_operand_write(cpu, 0, (uint8_t *)&src, cpu->op[0].size);
    } else {
      x64cpu_operand_write(cpu, 1, (uint8_t *)&dst, cpu->op[1].size);
    }
  } break;

  case X64CPU_OP_XADD: {
    uint64_t old_val = 0;

    x64cpu_operand_read(cpu, 0, (uint8_t *)&old_val, cpu->op[0].size);

    x64cpu_alu(cpu, X64CPU_ALU_ADD);

    x64cpu_operand_write(cpu, 1, (uint8_t *)&old_val, cpu->op[1].size);
  } break;

  case X64CPU_OP_HLT:
    cpu->is_halted = 1;
    break;

  case X64CPU_OP_RDTSC:
    cpu->regs.rax = (cpu->tsc & 0xffffffff);
    cpu->regs.rdx = (cpu->tsc >> 32);
    break;

  case X64CPU_OP_INVALID:
    x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
    break;

  default:
    fprintf(stderr, "[-] CPU: opcode %02x not implemented\n", opcode);
    x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
    break;
  }
}

/* Reset instruction decoding state before decoding a new instruction */
void x64cpu_execution_state_reset(x64cpu *cpu) {
  cpu->old_rip = cpu->regs.rip;

  cpu->cpu_exception.code = X64CPU_EXCEPTION_NONE;
  cpu->interrupt_number = -1;

  cpu->prefix_flags = 0;
  memset(&cpu->modrmbyte, 0, sizeof(cpu->modrmbyte));
  memset(&cpu->sibbyte, 0, sizeof(cpu->sibbyte));
  cpu->displacement = 0;
  cpu->op[0].type = X64CPU_OPT_NONE;
  cpu->op[1].type = X64CPU_OPT_NONE;
  cpu->op[2].type = X64CPU_OPT_NONE;
  cpu->op[3].type = X64CPU_OPT_NONE;
  cpu->instr_length = 0;

  /* If instruction is comparative (rep(n)e: CMPS / SCAS) it will set this */
  cpu->repeat_use_zf = 0;
}

/* Decode current instruction */
int x64cpu_decode(x64cpu *cpu) {
  int ret = -1;
  uint8_t opcode;
  int operation = 0x00;

  /* Fetch opcode */
  opcode = x64cpu_fetch8(cpu);
  if (cpu->cpu_exception.code != X64CPU_EXCEPTION_NONE) {
    goto _end;
  }

  /* Check prefixes */ // TODO: prefixes must have an order, blah blah
  while (x64cpu_decode_prefix(cpu, opcode)) {
    opcode = x64cpu_fetch8(cpu);
    if (cpu->cpu_exception.code != X64CPU_EXCEPTION_NONE) {
      goto _end;
    }
  }

  /* Check for extended instructions prefixes */
  if (opcode == 0x0F) {
    /* Read another opcode */
    opcode = x64cpu_fetch8(cpu);
    if (cpu->cpu_exception.code != X64CPU_EXCEPTION_NONE) {
      goto _end;
    }

    operation = x64cpu_decode_opcode_0F_2byte(cpu, opcode);
  } else {
    /* 1-byte opcodes */
    operation = x64cpu_decode_opcode_1byte(cpu, opcode);
  }

  cpu->current_operation = operation;
  cpu->current_opcode = opcode;

  /* Success */
  ret = 0;

_end:
  return ret;
}

int x64cpu_execute(x64cpu *cpu) {
  if (cpu->is_halted) {
    return 0;
  }

  cpu->execution_result = X64CPU_RES_SUCCESS;

  x64cpu_execution_state_reset(cpu);

  x64cpu_decode(cpu);

  if (cpu->cpu_exception.code != X64CPU_EXCEPTION_NONE) {
    goto _end;
  }

  /* Determine instruction set */
  if (cpu->operation_instruction_set == X64CPU_INSTR_SET_GENERAL) {
    /* Execute operation */
    x64cpu_operation_execute(cpu, cpu->current_operation, cpu->current_opcode);
  } else {
    x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
  }

_end:
  cpu->instruction_counter += 1;

  /* Halt on error */
  if (cpu->execution_result != X64CPU_RES_SUCCESS) {
    cpu->is_halted = 1;
    cpu->repeat_prefix = 0; /* Interrupts stop repeating prefix loops */
  }

  /* Check for repeat prefix */
  if (cpu->repeat_prefix != 0x00) {
    int neg_test_zf =
        (cpu->repeat_prefix == X64CPU_PREFIX_REPEAT_REPNZ) ? 1 : 0;

    if (x64cpu_loop(cpu, cpu->repeat_use_zf, neg_test_zf) != 0) {
      /* Repeat instruction */
      cpu->regs.rip = cpu->repeat_rip;
    } else {
      /* When done; clear prefix */
      cpu->repeat_prefix = 0;
    }
  }

  /* Increment TSC by constant value */
  cpu->tsc += 26;

  return cpu->execution_result;
}

void x64cpu_reset(x64cpu *cpu) {
  /* Reset RIP */
  cpu->regs.rip = 0x00;

  /* Reset flags; exceptions, interrupts */
  cpu->regs.rflags = 0x00;
  cpu->cpu_exception.code = X64CPU_EXCEPTION_NONE;
  cpu->interrupt_number = -1;

  /* Reset internal control flags */
  cpu->repeat_prefix = 0;

  cpu->is_halted = 0;

  /* Reset TSC */
  cpu->tsc = 0;

  /* Reset registers ? */
}

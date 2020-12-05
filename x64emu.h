#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>

//#include <cassert>

namespace x64emu {
#include "cpu.h"

namespace no_memory_change_eval {
const uint64_t stack_size = 8192;
uint8_t stack[stack_size];
int memory_read(x64cpu *cpu, void *user_data, uint64_t address, uint8_t *data,
                uint8_t size, int access_flags, uint64_t *fault_addr) {
  if (address == 40) { // stack protection
    memset(data, 0, size);
    return 0;
  }
  std::map<uint64_t, uint8_t> &map = *(std::map<uint64_t, uint8_t> *)user_data;
  for (uint8_t i = 0; i < size; i++) {
    if (map.count(address + i)) {
      data[i] = map[address + i];
    } else {
      data[i] = reinterpret_cast<uint8_t *>(address)[i];
    }
  }
  return 0;
}

int memory_write(x64cpu *cpu, void *user_data, uint64_t address, uint8_t *data,
                 uint8_t size, int access_flags, uint64_t *fault_addr) {
  std::map<uint64_t, uint8_t> &map = *(std::map<uint64_t, uint8_t> *)user_data;
  for (uint8_t i = 0; i < size; i++) {
    map[address + i] = data[i];
  }
  return 0;
}

const int N = 10000;

x64cpu_operand dop[4], *ops;
int op_cnt = 0;

struct execution_state {
  uint64_t rip;
  int prefix_flags;
  int32_t displacement;
  int repeat_use_zf;
  int current_operation;
  int current_opcode;
  uint8_t instr_length;
  __typeof(x64cpu::modrmbyte) modrmbyte;
  __typeof(x64cpu::sibbyte) sibbyte;
  x64cpu_operand *op;
} * cached_states;
int cache_cnt = 0;

execution_state **execution_state_buf;
uint64_t base_rip;

void reset_cache(uint64_t new_rip) {
  base_rip = new_rip;
  memset(execution_state_buf, 0, N * 16);
  op_cnt = 0;
  cache_cnt = 0;
}

int x64cpu_fast_execute(x64cpu *cpu) {
  if (cpu->is_halted) {
    return 0;
  }

  cpu->execution_result = X64CPU_RES_SUCCESS;

  int64_t diff = cpu->regs.rip - base_rip;
  if (diff < -N || diff >= N) {
    cpu->op = dop;
    x64cpu_execution_state_reset(cpu);
    x64cpu_decode(cpu);
  } else {
    execution_state *es = execution_state_buf[diff + N];
    if (es) {
      cpu->old_rip = cpu->regs.rip;
      cpu->cpu_exception.code = X64CPU_EXCEPTION_NONE;
      cpu->interrupt_number = -1;

      cpu->regs.rip = es->rip;
      cpu->modrmbyte = es->modrmbyte;
      cpu->sibbyte = es->sibbyte;
      cpu->prefix_flags = es->prefix_flags;
      cpu->displacement = es->displacement;
      cpu->repeat_use_zf = es->repeat_use_zf;
      cpu->instr_length = es->instr_length;
      cpu->current_operation = es->current_operation;
      cpu->current_opcode = es->current_opcode;
      cpu->op = dop;
      memcpy(dop, es->op, sizeof(x64cpu_operand) * 4);
    } else {
      cpu->op = ops + op_cnt;
      op_cnt += 4;
      x64cpu_execution_state_reset(cpu);
      x64cpu_decode(cpu);
      es = &cached_states[cache_cnt++];

      es->rip = cpu->regs.rip;
      es->modrmbyte = cpu->modrmbyte;
      es->sibbyte = cpu->sibbyte;
      es->prefix_flags = cpu->prefix_flags;
      es->displacement = cpu->displacement;
      es->repeat_use_zf = cpu->repeat_use_zf;
      es->instr_length = cpu->instr_length;
      es->current_operation = cpu->current_operation;
      es->current_opcode = cpu->current_opcode;
      es->op = cpu->op;
      cpu->op = dop;
      memcpy(dop, es->op, sizeof(x64cpu_operand) * 4);
      execution_state_buf[diff + N] = es;
    }
  }

  /* Determine instruction set */
  if (cpu->operation_instruction_set == X64CPU_INSTR_SET_GENERAL) {
    /* Execute operation */
    x64cpu_operation_execute(cpu, cpu->current_operation, cpu->current_opcode);
  } else {
    x64cpu_exception(cpu, X64CPU_EXCEPTION_UD);
  }

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

x64cpu _cpu;

struct __init {
  __init() {
    ops = new x64cpu_operand[N];
    cached_states = new execution_state[N];
    execution_state_buf = new execution_state *[N * 2];
    x64cpu *cpu = &_cpu;
    cpu->mem_read = memory_read;
    cpu->mem_write = memory_write;
    cpu->user_data = new std::map<uint64_t, uint8_t>;
  }
} ___init;

uint64_t eval_function(void *func) {
  x64cpu *cpu = &_cpu;
  x64cpu_reset(cpu);
  std::map<uint64_t, uint8_t> &map =
      *(std::map<uint64_t, uint8_t> *)cpu->user_data;
  map.clear();
  cpu->regs.rip = reinterpret_cast<uint64_t>(func);
  if (cpu->regs.rip != base_rip)
    reset_cache(cpu->regs.rip);
  cpu->regs.rsp = reinterpret_cast<uint64_t>(stack + stack_size - 8);
  // memset(stack, 0, sizeof stack);
  *(uint64_t *)(stack + stack_size - 8) = 0;
  while (cpu->regs.rip) {
    int rc = x64cpu_fast_execute(cpu);
    if (rc == X64CPU_RES_EXCEPTION) {
      printf("[*] CPU Exception (%d): [%d] %s at RIP 0x%016lx.\n",
             cpu->execution_result, cpu->cpu_exception.code,
             x64cpu_exception_name(cpu->cpu_exception.code),
             cpu->cpu_exception.rip);
      exit(1);
    }
  }
  return cpu->regs.rax;
}
} // namespace no_memory_change_eval
} // namespace x64emu
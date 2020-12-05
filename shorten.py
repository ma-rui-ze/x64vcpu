import os, string

includes = '''#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>'''
includes_map = includes + '\n#include <map>'

open('work/x64emu.h', 'w').write(open('x64emu.h').read().replace(includes_map, ''))
open('work/cpu.h', 'w').write(open('cpu.h').read().replace(includes, ''))
open('work/opcode_decoder.h', 'w').write(open('opcode_decoder.h').read())

os.chdir('work/')
os.system('g++ x64emu.h -E -o out.h')
os.chdir('..')

keywords = {'alignas', 'alignof', 'and', 'and_eq', 'asm', 'atomic_cancel', 'atomic_commit', 'atomic_noexcept', 'auto', 'bitand', 'bitor', 'bool', 'break', 'case', 'catch', 'char', 'char8_t', 'char16_t', 'char32_t', 'class', 'compl', 'concept', 'const', 'consteval', 'constexpr', 'constinit', 'const_cast', 'continue', 'co_await', 'co_return', 'co_yield', 'decltype', 'default', 'delete', 'do', 'double', 'dynamic_cast', 'else', 'enum', 'explicit', 'export', 'extern', 'false', 'float', 'for', 'friend', 'goto', 'if', 'inline', 'int', 'long', 'mutable', 'namespace', 'new', 'noexcept', 'not', 'not_eq', 'nullptr', 'operator', 'or', 'or_eq', 'private', 'protected', 'public', 'reflexpr', 'register', 'reinterpret_cast', 'requires', 'return', 'short', 'signed', 'sizeof', 'static', 'static_assert', 'static_cast', 'struct', 'switch', 'synchronized', 'template', 'this', 'thread_local', 'throw', 'true', 'try', 'typedef', 'typeid', 'typename', 'union', 'unsigned', 'using', 'virtual', 'void', 'volatile', 'wchar_t', 'while', 'xor', 'xor_eq'}

keywords.add('reinterpret_cast')
keywords.add('map')
keywords.add('count')
keywords.add('std')

keywords.add('printf')
keywords.add('fprintf')
keywords.add('exit')
keywords.add('assert')
keywords.add('memset')
keywords.add('memcpy')
keywords.add('puts')
keywords.add('alloca')
keywords.add('malloc')
keywords.add('free')
keywords.add('stdout')
keywords.add('stderr')
keywords.add('clear')

keywords.add('x64emu')
keywords.add('normal_eval')
keywords.add('no_memory_change_eval')
keywords.add('eval_function')
keywords.add('init_instructions')

keywords.add('NULL')

s = open('work/out.h').readlines()
s2 = ''
for i in s:
	if i[0] != '#':
		s2 += i.strip().replace('NULL', '0') + ' '
src = ' '.join(s2.split()) + ' '

chars = string.ascii_letters + string.digits + '_'
ochars = chars

tokens = []

lst = ''
i_ = 0
while i_ < len(src):
	i = src[i_]
	if i == '"':
		if lst != '':
			tokens.append(lst)
		lst = ''
		t = src.find('"', i_ + 1) + 1
		tokens.append(src[i_:t])
		i_ = t
		continue
	if i == "'":
		if lst != '':
			tokens.append(lst)
		lst = ''
		t = src.find("'", i_ + 1) + 1
		tokens.append(src[i_:t])
		i_ = t
		continue
	if i == ' ':
		if lst != '':
			tokens.append(lst)
		lst = ''
		i_ += 1
		continue
	if lst == '':
		lst += i
		i_ += 1
		continue
	if (i in chars) != (lst[0] in chars):
		tokens.append(lst)
		lst = i
	else:
		lst += i
	i_ += 1

# print(tokens)

vars = {}
for i in tokens:
	if i[0] in string.ascii_letters and i not in keywords:
		if i not in vars:
			vars[i] = 0
		vars[i] += 1
varl = [(-vars[x], x) for x in vars]
varl.sort()

# print(varl)

chars = string.ascii_letters + '_'
# print(chars)
rmap = {}

skipped = 0

for i in range(len(varl)):
	while True:
		u = i + skipped
		st = ''
		while True:
			st += chars[u % len(chars)]
			u //= len(chars)
			if u == 0:
				break
		if st not in keywords:
			break
		skipped += 1
	rmap[varl[i][1]] = st
# print(rmap)

if 0:
	for i in rmap:
		if rmap[i] == 'Hg':
			print(i)
			exit()

types = []
for i in [8, 16, 32, 64]:
	types.append('uint%d_t' % i)
	types.append('int%d_t' % i)
out = ''
for i in types:
	if i in rmap:
		out += 'typedef %s %s;' % (i, rmap[i])
out = 'namespace x64emu{' + out + '}'
for i in tokens:
	if out[-1] in ochars and i[0] in ochars:
		out += ' '
	if i in rmap:
		out += rmap[i]
	else:
		out += i
out = out.replace('}namespace x64emu{', '')
open('x64emu_min.h', 'w').write(out)

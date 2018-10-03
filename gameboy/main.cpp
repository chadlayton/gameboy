#pragma comment(lib, "SDL2.lib")

#include <vector>
#include <fstream>

#include <stdio.h>
#include <stdarg.h>
#include <assert.h>

#define SDL_MAIN_HANDLED
#include <SDL.h>

namespace debug
{
	void print(const char* format, ...)
	{
		va_list argptr;
		va_start(argptr, format);
		vfprintf(stdout, format, argptr);
		va_end(argptr);
	}
}

namespace memory
{
	// Memory Map:
	// 0000 - 3FFF	16KB ROM Bank 00 (in cartridge, fixed at bank 00)
	// 4000 - 7FFF	16KB ROM Bank 01..NN (in cartridge, switchable bank number)
	// 8000 - 9FFF	8KB Video RAM(VRAM) (switchable bank 0 - 1 in CGB Mode)
	// A000 - BFFF	8KB External RAM (in cartridge, switchable bank, if any)
	// C000 - CFFF	4KB Work RAM Bank 0 (WRAM)
	// D000 - DFFF	4KB Work RAM Bank 1 (WRAM)(switchable bank 1 - 7 in CGB Mode)
	// E000 - FDFF	Same as C000 - DDFF (ECHO) (typically not used)
	// FE00 - FE9F	Sprite Attribute Table (OAM)
	// FEA0 - FEFF	Not Usable
	// FF00 - FF7F	I / O Ports
	// FF80 - FFFE	High RAM (HRAM)
	// FFFF			Interrupt Enable Register

	namespace registers
	{
		// Serial transfer data
		static const uint16_t SB = 0xFF01;

		// Serial Transfer Control
		//		7 : Transfer Start Flag (0 = No Transfer, 1 = Start)
		//		1 : Clock Speed (0 = Normal, 1 = Fast) ** CGB Mode Only **
		//		0 : Shift Clock (0 = External Clock, 1 = Internal Clock)
		static const uint16_t SC = 0xFF02;

		// Divider register. This register is incremented at rate of 16384Hz 
		// (~16779Hz on SGB). In CGB Double Speed Mode it is incremented twice 
		// as fast, ie. at 32768Hz. Writing any value to this register resets 
		// it to 00h.
		static const uint16_t DIV = 0xFF04;

		// Timer counter. This timer is incremented by a clock frequency specified 
		// by the TAC register. When the value overflows (bigger than 0xFF) then 
		// it will be reset to the value specified in TMA, and an interrupt will 
		// be requested.
		static const uint16_t TIMA = 0xFF05;

		// Timer modulo. When TIMA overflows, this data is loaded in its place.
		static const uint16_t TMA = 0xFF06;

		// Timer control. Timer Stop is bit 2:
		//		0 : Stop
		//		1 : Start
		// Input Clock Select is bits 1 and 0:
		//		00 : 4096 Hz (~4194 Hz SGB)
		//		01 : 262144 Hz (~268400 Hz SGB)
		//		10 : 65536 Hz (~67110 Hz SGB)
		//		11 : 16384 Hz (~16780 Hz SGB)
		static const uint16_t TAC = 0xFF07;

		// Interrupt Flags
		static const uint16_t IF = 0xFF0F;

		// Interrupt Enable
		static const uint16_t IE = 0xFFFF;

		// LCD Control
		//	7 : LCD Display Enable (0 = Off, 1 = On)
		//	6 : Window Tile Map Display Select (0 = 9800 - 9BFF, 1 = 9C00 - 9FFF)
		//	5 : Window Display Enable (0 = Off, 1 = On)
		//	4 : BG & Window Tile Data Select (0 = 8800 - 97FF, 1 = 8000 - 8FFF)
		//	3 : BG Tile Map Display Select (0 = 9800 - 9BFF, 1 = 9C00 - 9FFF)
		//	2 : OBJ (Sprite) Size (0 = 8x8, 1 = 8x16)
		//	1 : OBJ (Sprite) Display Enable (0 = Off, 1 = On)
		//	0 : BG Display (Depends on GameBoy type) (0 = Off, 1 = On)
		static const uint16_t LCDC = 0xFF40;

		// Specifies the position in the 256x256 pixels BG map (32x32 tiles) which 
		// is to be displayed at the upper/left LCD display position. Values in range 
		// from 0 - 255 may be used for X / Y each, the video controller automatically 
		// wraps if drawing exceeds the BG map area.
		static const uint16_t SCY = 0xFF42;
		static const uint16_t SCX = 0xFF43;

		// LCDC Y-Coordinate. The LY indicates the vertical line to which the present 
		// data is transferred to the LCD Driver. The LY can take on any value between
		// 0 through 153. The values between 144 and 153 indicate the V-Blank period. 
		// Writing will reset the counter.
		static const uint16_t LY = 0xFF44;
	}

	uint8_t read_byte(const std::vector<uint8_t>& memory, uint16_t addr)
	{
		// HACK: At 0x0235 Tetris reads LY (0xFF44) into A and loops until it 
		// equals  0x94. At 0x282A it loops until it equals 0x91. I don't know
		// what is responsible for setting this yet.
		if (addr == registers::LY)
		{
			static uint8_t LY = 0x91;
			++LY;
			if (LY > 0x94)
			{
				LY = 0x91;
			}
			return LY;
		}

		// HACK: No input yet
		if (addr == 0xFF00)
		{
			return 0xEF;
		}

		return memory[addr];
	}

	uint16_t make_word(uint8_t low, uint8_t high)
	{
		return (high << 8) | low;
	}

	uint8_t low_byte(uint16_t n)
	{
		return n & 0xFF;
	}

	uint8_t high_byte(uint16_t n)
	{
		return (n >> 8) & 0xFF;
	}

	void write_byte(std::vector<uint8_t>& memory, uint16_t addr, uint8_t byte)
	{
		if (addr == registers::SC && byte == 0x81)
		{
			debug::print("%u", memory[registers::SB]);
		}

		if (addr == registers::LCDC)
		{
			static int i = 0;
			++i;
		}

		memory[addr] = byte;
	}
}


namespace cpu
{
	struct cpu
	{
		struct registers
		{
			// There are eight 8-bit registers: A, B, C, D, E, F. Some instructions
			// allow you to pair two 8-bit registers into one 16-bit register: AF, BC, DE, HL
			union
			{
				struct
				{
					// The flag register can be queried for information about the results of
					// the last instruction.
					uint8_t F;
					uint8_t A;
				};
				uint16_t AF;
			};

			union
			{
				struct
				{
					uint8_t C;
					uint8_t B;
				};
				uint16_t BC;
			};

			union
			{
				struct
				{
					uint8_t E;
					uint8_t D;
				};
				uint16_t DE;
			};

			union
			{
				struct
				{
					uint8_t L;
					uint8_t H;
				};
				uint16_t HL;
			};

			// There are also two 16-bit registers. The stack pointer (SP) and the 
			// program counter (PC).
			uint16_t SP;
			uint16_t PC;
		};

		bool interrupt_master = true;
		uint32_t cycles = 0;
		registers registers;
	};

	namespace flags
	{
		static const uint8_t ZERO = 0x80;
		static const uint8_t NEGATIVE = 0x40;	// SUBTRACT
		static const uint8_t HALF_CARRY = 0x20;
		static const uint8_t CARRY = 0x10;
	}

	void set_flag(cpu& cpu, uint8_t flag)
	{
		cpu.registers.F |= flag;
	}

	void clear_flag(cpu& cpu, uint8_t flag)
	{
		cpu.registers.F &= ~flag;
	}

	bool get_flag(cpu& cpu, uint8_t flag)
	{
		return !!(cpu.registers.F & flag);
	}

	namespace instructions
	{
		void nop(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.cycles += 4;
		}

		namespace detail
		{
			// Decrement 8-bit value
			void dec(cpu& cpu, uint8_t& n)
			{
				n--;

				if ((n & 0x0F) == 0x0F)
				{
					set_flag(cpu, flags::HALF_CARRY);
				}
				else
				{
					clear_flag(cpu, flags::HALF_CARRY);
				}

				if (n == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}

				set_flag(cpu, flags::NEGATIVE);
			}
		}

		void dec_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::dec(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		void dec_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::dec(cpu, cpu.registers.B);
			cpu.cycles += 4;
		}

		void dec_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::dec(cpu, cpu.registers.C);
			cpu.cycles += 4;
		}

		void dec_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::dec(cpu, cpu.registers.D);
			cpu.cycles += 4;
		}

		void dec_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::dec(cpu, cpu.registers.E);
			cpu.cycles += 4;
		}

		void dec_f(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::dec(cpu, cpu.registers.F);
			cpu.cycles += 4;
		}

		void dec_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::dec(cpu, cpu.registers.H);
			cpu.cycles += 4;
		}

		void dec_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::dec(cpu, cpu.registers.L);
			cpu.cycles += 4;
		}

		void dec_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::dec(cpu, memory[cpu.registers.HL]);
			cpu.cycles += 12;
		}

		void dec_bc(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			--cpu.registers.BC;
			cpu.cycles += 8;
		}

		void dec_de(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			--cpu.registers.DE;
			cpu.cycles += 8;
		}

		void dec_hl(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			--cpu.registers.HL;
			cpu.cycles += 8;
		}

		void dec_sp(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			--cpu.registers.SP;
			cpu.cycles += 8;
		}

		namespace detail
		{
			// Increment 8-bit value
			void inc(cpu& cpu, uint8_t& n)
			{
				if ((n & 0x0F) == 0x0F)
				{
					set_flag(cpu, flags::HALF_CARRY);
				}
				else
				{
					clear_flag(cpu, flags::HALF_CARRY);
				}

				n++;

				if (n == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}

				clear_flag(cpu, flags::NEGATIVE);
			}
		}

		void inc_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::inc(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		void inc_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::inc(cpu, cpu.registers.B);
			cpu.cycles += 4;
		}

		void inc_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::inc(cpu, cpu.registers.C);
			cpu.cycles += 4;
		}

		void inc_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::inc(cpu, cpu.registers.D);
			cpu.cycles += 4;
		}

		void inc_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::inc(cpu, cpu.registers.E);
			cpu.cycles += 4;
		}

		void inc_f(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::inc(cpu, cpu.registers.F);
			cpu.cycles += 4;
		}

		void inc_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::inc(cpu, cpu.registers.H);
			cpu.cycles += 4;
		}

		void inc_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::inc(cpu, cpu.registers.L);
			cpu.cycles += 4;
		}

		void inc_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::inc(cpu, memory[cpu.registers.HL]);
			cpu.cycles += 12;
		}

		void inc_bc(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			++cpu.registers.BC;
			cpu.cycles += 8;
		}

		void inc_de(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			++cpu.registers.DE;
			cpu.cycles += 8;
		}

		void inc_hl(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			++cpu.registers.HL;
			cpu.cycles += 8;
		}

		void inc_sp(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			++cpu.registers.SP;
			cpu.cycles += 8;
		}

		namespace detail
		{
			// Add n to A
			void add_a(cpu& cpu, uint8_t& n)
			{
				const uint16_t half_result = (cpu.registers.A & 0x0F) + (n & 0x0F);

				if (half_result > 0x0F)
				{
					set_flag(cpu, flags::HALF_CARRY);
				}
				else
				{
					clear_flag(cpu, flags::HALF_CARRY);
				}

				const uint16_t result = cpu.registers.A + n;

				if (result > 0xFF)
				{
					set_flag(cpu, flags::CARRY);
				}
				else
				{
					clear_flag(cpu, flags::CARRY);
				}

				cpu.registers.A = (uint8_t)result;

				if (cpu.registers.A == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}

				clear_flag(cpu, flags::NEGATIVE);
			}

			// Add n to HL
			void add_hl(cpu& cpu, uint16_t& n)
			{
				const uint32_t half_result = (cpu.registers.HL & 0x0FFF) + (n & 0x0FFF);

				if (half_result > 0x1000)
				{
					set_flag(cpu, flags::HALF_CARRY);
				}
				else
				{
					clear_flag(cpu, flags::HALF_CARRY);
				}

				const uint32_t result = cpu.registers.HL + n;

				if (result > 0xFFFF)
				{
					set_flag(cpu, flags::CARRY);
				}
				else
				{
					clear_flag(cpu, flags::CARRY);
				}

				cpu.registers.HL = (uint16_t)result;

				if (cpu.registers.A == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}

				clear_flag(cpu, flags::NEGATIVE);
			}
		}

		void add_a_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_a(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		void add_a_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_a(cpu, cpu.registers.B);
			cpu.cycles += 4;
		}

		void add_a_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_a(cpu, cpu.registers.C);
			cpu.cycles += 4;
		}

		void add_a_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_a(cpu, cpu.registers.D);
			cpu.cycles += 4;
		}

		void add_a_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_a(cpu, cpu.registers.E);
			cpu.cycles += 4;
		}

		void add_a_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_a(cpu, cpu.registers.H);
			cpu.cycles += 4;
		}

		void add_a_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_a(cpu, cpu.registers.L);
			cpu.cycles += 4;
		}

		void add_a_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_a(cpu, memory[cpu.registers.HL]);
			cpu.cycles += 8;
		}

		void add_a_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_a(cpu, operands[0]);
			cpu.cycles += 8;
		}

		void add_hl_bc(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_hl(cpu, cpu.registers.BC);
			cpu.cycles += 8;
		}

		void add_hl_de(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_hl(cpu, cpu.registers.DE);
			cpu.cycles += 8;
		}

		void add_hl_hl(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_hl(cpu, cpu.registers.HL);
			cpu.cycles += 8;
		}

		void add_hl_sp(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::add_hl(cpu, cpu.registers.SP);
			cpu.cycles += 8;
		}

		namespace detail
		{
			void adc_a_n(cpu& cpu, uint8_t& n) 
			{
				if (get_flag(cpu, flags::CARRY))
				{
					++n;
				}

				add_a(cpu, n);
			}
		}

		void adc_a_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::adc_a_n(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		void adc_a_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::adc_a_n(cpu, cpu.registers.B);
			cpu.cycles += 4;
		}

		void adc_a_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::adc_a_n(cpu, cpu.registers.C);
			cpu.cycles += 4;
		}

		void adc_a_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::adc_a_n(cpu, cpu.registers.D);
			cpu.cycles += 4;
		}

		void adc_a_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::adc_a_n(cpu, cpu.registers.E);
			cpu.cycles += 4;
		}

		void adc_a_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::adc_a_n(cpu, cpu.registers.H);
			cpu.cycles += 4;
		}

		void adc_a_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::adc_a_n(cpu, cpu.registers.L);
			cpu.cycles += 4;
		}

		void adc_a_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::adc_a_n(cpu, memory[cpu.registers.HL]);
			cpu.cycles += 8;
		}

		void adc_a_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::adc_a_n(cpu, operands[0]);
			cpu.cycles += 8;
		}

		namespace detail
		{
			// Subtract n from A
			void sub_a(cpu& cpu, uint8_t& n)
			{
				if ((cpu.registers.A & 0x0F) < (n & 0x0F))
				{
					set_flag(cpu, flags::HALF_CARRY);
				}
				else
				{
					clear_flag(cpu, flags::HALF_CARRY);
				}

				if (cpu.registers.A < n)
				{
					set_flag(cpu, flags::CARRY);
				}
				else
				{
					clear_flag(cpu, flags::CARRY);
				}

				cpu.registers.A -= n;

				if (cpu.registers.A == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}

				set_flag(cpu, flags::NEGATIVE);
			}
		}

		void sub_a_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::sub_a(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		void sub_a_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::sub_a(cpu, cpu.registers.B);
			cpu.cycles += 4;
		}

		void sub_a_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::sub_a(cpu, cpu.registers.C);
			cpu.cycles += 4;
		}

		void sub_a_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::sub_a(cpu, cpu.registers.D);
			cpu.cycles += 4;
		}

		void sub_a_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::sub_a(cpu, cpu.registers.E);
			cpu.cycles += 4;
		}

		void sub_a_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::sub_a(cpu, cpu.registers.H);
			cpu.cycles += 4;
		}

		void sub_a_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::sub_a(cpu, cpu.registers.L);
			cpu.cycles += 4;
		}

		void sub_a_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::sub_a(cpu, memory[cpu.registers.HL]);
			cpu.cycles += 8;
		}

		void sub_a_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::sub_a(cpu, operands[0]);
			cpu.cycles += 8;
		}

		namespace detail
		{
			// Logical exclusive OR n with register A, result in A
			void xor(cpu& cpu, uint8_t& n)
			{
				cpu.registers.A ^= n;

				if (cpu.registers.A == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}
				clear_flag(cpu, flags::NEGATIVE);
				clear_flag(cpu, flags::HALF_CARRY);
				clear_flag(cpu, flags::CARRY);
			}
		}

		void xor_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::xor(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		void xor_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::xor(cpu, cpu.registers.B);
			cpu.cycles += 4;
		}

		void xor_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::xor(cpu, cpu.registers.C);
			cpu.cycles += 4;
		}

		void xor_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::xor(cpu, cpu.registers.D);
			cpu.cycles += 4;
		}

		void xor_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::xor(cpu, cpu.registers.E);
			cpu.cycles += 4;
		}

		void xor_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::xor(cpu, cpu.registers.H);
			cpu.cycles += 4;
		}

		void xor_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::xor(cpu, cpu.registers.L);
			cpu.cycles += 4;
		}

		void xor_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::xor(cpu, memory[cpu.registers.HL]);
			cpu.cycles += 8;
		}

		void xor_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::xor(cpu, operands[0]);
			cpu.cycles += 8;
		}

		namespace detail
		{
			// Logical OR n with register A, result in A.
			void or(cpu& cpu, uint8_t& n)
			{
				cpu.registers.A |= n;

				if (cpu.registers.A == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}
				clear_flag(cpu, flags::NEGATIVE);
				clear_flag(cpu, flags::HALF_CARRY);
				clear_flag(cpu, flags::CARRY);
			}
		}

		void or_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::or(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		void or_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::or(cpu, cpu.registers.B);
			cpu.cycles += 4;
		}

		void or_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::or(cpu, cpu.registers.C);
			cpu.cycles += 4;
		}

		void or_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::or(cpu, cpu.registers.D);
			cpu.cycles += 4;
		}

		void or_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::or(cpu, cpu.registers.E);
			cpu.cycles += 4;
		}

		void or_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::or(cpu, cpu.registers.H);
			cpu.cycles += 4;
		}

		void or_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::or(cpu, cpu.registers.L);
			cpu.cycles += 4;
		}

		namespace detail
		{
			// Logically AND n with A, result in A.
			void and(cpu& cpu, uint8_t& n)
			{
				cpu.registers.A &= n;

				if (cpu.registers.A == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}
				clear_flag(cpu, flags::NEGATIVE);
				set_flag(cpu, flags::HALF_CARRY);
				clear_flag(cpu, flags::CARRY);
			}
		}

		void and_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::and(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		void and_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::and(cpu, cpu.registers.B);
			cpu.cycles += 4;
		}

		void and_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::and(cpu, cpu.registers.C);
			cpu.cycles += 4;
		}

		void and_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::and(cpu, cpu.registers.D);
			cpu.cycles += 4;
		}

		void and_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::and(cpu, cpu.registers.E);
			cpu.cycles += 4;
		}

		void and_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::and(cpu, cpu.registers.H);
			cpu.cycles += 4;
		}

		void and_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::and(cpu, cpu.registers.L);
			cpu.cycles += 4;
		}

		void and_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::and(cpu, memory[cpu.registers.HL]);
			cpu.cycles += 8;
		}

		void and_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::and(cpu, operands[0]);
			cpu.cycles += 8;
		}

		void ld_bc_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.BC = memory::make_word(operands[0], operands[1]);
			cpu.cycles += 12;
		}

		void ld_de_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.DE = memory::make_word(operands[0], operands[1]);
			cpu.cycles += 12;
		}

		void ld_hl_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.HL = memory::make_word(operands[0], operands[1]);
			cpu.cycles += 12;
		}

		void ld_sp_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.SP = memory::make_word(operands[0], operands[1]);
			cpu.cycles += 12;
		}

		void ld_a_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = operands[0];
			cpu.cycles += 8;
		}

		void ld_b_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.B = operands[0];
			cpu.cycles += 8;
		}

		void ld_c_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.C = operands[0];
			cpu.cycles += 8;
		}

		void ld_d_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.D = operands[0];
			cpu.cycles += 8;
		}

		void ld_e_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.E = operands[0];
			cpu.cycles += 8;
		}

		void ld_h_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.H = operands[0];
			cpu.cycles += 8;
		}

		void ld_l_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.L = operands[0];
			cpu.cycles += 8;
		}

		void ld_a_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.cycles += 4;
		}

		void ld_a_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = cpu.registers.B;
			cpu.cycles += 4;
		}

		void ld_a_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = cpu.registers.C;
			cpu.cycles += 4;
		}

		void ld_a_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = cpu.registers.D;
			cpu.cycles += 4;
		}

		void ld_a_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = cpu.registers.E;
			cpu.cycles += 4;
		}

		void ld_a_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = cpu.registers.H;
			cpu.cycles += 4;
		}

		void ld_a_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = cpu.registers.L;
			cpu.cycles += 4;
		}

		void ld_a_BC(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = memory::read_byte(memory, cpu.registers.BC);
			cpu.cycles += 8;
		}

		void ld_a_DE(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = memory::read_byte(memory, cpu.registers.DE);
			cpu.cycles += 8;
		}

		void ld_a_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = memory::read_byte(memory, cpu.registers.HL);
			cpu.cycles += 8;
		}

		void ld_a_NN(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = memory::read_byte(memory, memory::make_word(operands[0], operands[1]));
			cpu.cycles += 16;
		}

		void ld_b_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.B = cpu.registers.A;
			cpu.cycles += 4;
		}

		void ld_c_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.C = cpu.registers.A;
			cpu.cycles += 4;
		}

		void ld_d_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.D = cpu.registers.A;
			cpu.cycles += 4;
		}

		void ld_e_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.E = cpu.registers.A;
			cpu.cycles += 4;
		}

		void ld_h_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.H = cpu.registers.A;
			cpu.cycles += 4;
		}

		void ld_l_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.L = cpu.registers.A;
			cpu.cycles += 4;
		}

		void ld_BC_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.BC, cpu.registers.A);
			cpu.cycles += 8;
		}

		void ld_DE_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.DE, cpu.registers.A);
			cpu.cycles += 8;
		}

		void ld_HL_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, cpu.registers.A);
			cpu.cycles += 8;
		}

		void ld_b_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.B = memory::read_byte(memory, cpu.registers.HL);
			cpu.cycles += 8;
		}

		void ld_c_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.C = memory::read_byte(memory, cpu.registers.HL);
			cpu.cycles += 8;
		}

		void ld_d_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.D = memory::read_byte(memory, cpu.registers.HL);
			cpu.cycles += 8;
		}

		void ld_e_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.E = memory::read_byte(memory, cpu.registers.HL);
			cpu.cycles += 8;
		}

		void ld_h_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.H = cpu.registers.B;
			cpu.cycles += 4;
		}

		void ld_h_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.H = cpu.registers.C;
			cpu.cycles += 4;
		}

		void ld_h_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.H = cpu.registers.D;
			cpu.cycles += 4;
		}

		void ld_h_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.H = cpu.registers.E;
			cpu.cycles += 4;
		}

		void ld_h_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.H = cpu.registers.H;
			cpu.cycles += 4;
		}

		void ld_h_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.H = cpu.registers.L;
			cpu.cycles += 4;
		}

		void ld_h_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.H = memory::read_byte(memory, cpu.registers.HL);
			cpu.cycles += 8;
		}

		void ld_l_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.L = cpu.registers.B;
			cpu.cycles += 4;
		}

		void ld_l_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.L = cpu.registers.C;
			cpu.cycles += 4;
		}

		void ld_l_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.L = cpu.registers.D;
			cpu.cycles += 4;
		}

		void ld_l_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.L = cpu.registers.E;
			cpu.cycles += 4;
		}

		void ld_l_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.L = cpu.registers.H;
			cpu.cycles += 4;
		}

		void ld_l_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.L = cpu.registers.L;
			cpu.cycles += 4;
		}

		void ld_l_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.L = memory::read_byte(memory, cpu.registers.HL);
			cpu.cycles += 8;
		}

		// TODO: Dozens of other trivial variants of 8-bit loads of form: LD r1, r2

		void ld_HL_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, cpu.registers.B);
			cpu.cycles += 8;
		}

		void ld_HL_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, cpu.registers.C);
			cpu.cycles += 8;
		}

		void ld_HL_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, cpu.registers.D);
			cpu.cycles += 8;
		}

		void ld_HL_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, cpu.registers.E);
			cpu.cycles += 8;
		}

		void ld_HL_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, cpu.registers.H);
			cpu.cycles += 8;
		}

		void ld_HL_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, cpu.registers.L);
			cpu.cycles += 8;
		}

		// Put n into memory address HL.
		void ld_HL_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, operands[0]);
			cpu.cycles += 12;
		}

		// Put A into memory address nn.
		void ld_NN_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, memory::make_word(operands[0], operands[1]), cpu.registers.A);
			cpu.cycles += 16;
		}

		// Put value at address $FF00 + register C into A.
		void ld_a_FFC(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = memory::read_byte(memory, 0xFF00 + cpu.registers.C);
			cpu.cycles += 8;
		}

		// Put A into memory address $FF00 + C.
		void ld_FFC_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, 0xFF00 + cpu.registers.C, cpu.registers.A);
			cpu.cycles += 8;
		}

		// Put A into memory address $FF00 + n.
		void ld_FFN_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, 0xFF00 + operands[0], cpu.registers.A);
			cpu.cycles += 12;
		}

		// Put memory address $FF00 + n into A.
		void ld_a_FFN(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = memory::read_byte(memory, 0xFF00 + operands[0]);
			cpu.cycles += 12;
		}

		// Put A into memory address HL. Decrement HL.
		void ldd_HL_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, cpu.registers.A);
			--cpu.registers.HL;
			cpu.cycles += 8;
		}

		// Put A into memory address HL. Increment HL.
		void ldi_HL_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			memory::write_byte(memory, cpu.registers.HL, cpu.registers.A);
			++cpu.registers.HL;
			cpu.cycles += 8;
		}

		// Put value at address HL into A. Increment HL.
		void ldi_a_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = memory::read_byte(memory, cpu.registers.HL);
			++cpu.registers.HL;
			cpu.cycles += 8;
		}

		// Disable interrupts
		void di(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.interrupt_master = false;
			cpu.cycles += 4;
		}

		// Enable interrupts
		void ei(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.interrupt_master = true;
			cpu.cycles += 4;
		}

		// Complement A register.
		void cpl(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.A = ~cpu.registers.A;

			set_flag(cpu, flags::NEGATIVE);
			set_flag(cpu, flags::HALF_CARRY);

			cpu.cycles += 4;
		}

		namespace detail
		{
			void cp_n(cpu& cpu, uint8_t n)
			{
				if (cpu.registers.A < n)
				{
					set_flag(cpu, flags::CARRY);
				}
				else
				{
					clear_flag(cpu, flags::CARRY);
				}

				if ((cpu.registers.A & 0x0F) < (n & 0x0F))
				{
					set_flag(cpu, flags::HALF_CARRY);
				}
				else
				{
					clear_flag(cpu, flags::HALF_CARRY);
				}

				if (cpu.registers.A == n)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}

				set_flag(cpu, flags::NEGATIVE);
			}
		}

		void cp_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::cp_n(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		void cp_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::cp_n(cpu, cpu.registers.B);
			cpu.cycles += 4;
		}

		void cp_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::cp_n(cpu, cpu.registers.C);
			cpu.cycles += 4;
		}

		void cp_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::cp_n(cpu, cpu.registers.D);
			cpu.cycles += 4;
		}
		void cp_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::cp_n(cpu, cpu.registers.E);
			cpu.cycles += 4;
		}

		void cp_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::cp_n(cpu, cpu.registers.H);
			cpu.cycles += 4;
		}

		void cp_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::cp_n(cpu, cpu.registers.L);
			cpu.cycles += 4;
		}

		void cp_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::cp_n(cpu, operands[0]);
			cpu.cycles += 8;
		}

		namespace detail
		{
			void push(cpu& cpu, std::vector<uint8_t>& memory, uint16_t n)
			{
				memory[--cpu.registers.SP] = memory::high_byte(n);
				memory[--cpu.registers.SP] = memory::low_byte(n);
			}

			uint16_t pop(cpu& cpu, const std::vector<uint8_t>& memory)
			{
				const uint8_t low = memory[cpu.registers.SP++];
				const uint8_t high = memory[cpu.registers.SP++];
				return memory::make_word(low, high);
			}
		}

		// Push register pair nn onto stack.
		void push_af(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.AF);
			cpu.cycles += 16;
		}

		void push_bc(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.BC);
			cpu.cycles += 16;
		}

		void push_de(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.DE);
			cpu.cycles += 16;
		}

		void push_hl(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.HL);
			cpu.cycles += 16;
		}

		// Pop two bytes off stack into register pair nn.
		void pop_af(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.AF = detail::pop(cpu, memory);
			cpu.cycles += 12;
		}

		void pop_bc(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.BC = detail::pop(cpu, memory);
			cpu.cycles += 12;
		}

		void pop_de(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.DE = detail::pop(cpu, memory);
			cpu.cycles += 12;
		}

		void pop_hl(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.HL = detail::pop(cpu, memory);
			cpu.cycles += 12;
		}

		// Push address of next instruction onto stack and then jump to address nn.
		void call_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.PC);
			cpu.registers.PC = memory::make_word(operands[0], operands[1]);
			cpu.cycles += 12;
		}

		void call_nz_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::ZERO))
			{
				cpu.cycles += 12;
			}
			else
			{
				detail::push(cpu, memory, cpu.registers.PC);
				cpu.registers.PC = memory::make_word(operands[0], operands[1]);
				cpu.cycles += 24;
			}

		}

		void call_z_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::ZERO))
			{
				detail::push(cpu, memory, cpu.registers.PC);
				cpu.registers.PC = memory::make_word(operands[0], operands[1]);
				cpu.cycles += 24;
			}
			else
			{
				cpu.cycles += 12;
			}
		}

		void call_nc_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::CARRY))
			{
				cpu.cycles += 12;
			}
			else
			{
				detail::push(cpu, memory, cpu.registers.PC);
				cpu.registers.PC = memory::make_word(operands[0], operands[1]);
				cpu.cycles += 24;
			}
		}

		void call_c_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::CARRY))
			{
				detail::push(cpu, memory, cpu.registers.PC);
				cpu.registers.PC = memory::make_word(operands[0], operands[1]);
				cpu.cycles += 24;
			}
			else
			{
				cpu.cycles += 12;
			}
		}

		// Pop two bytes from stack & jump to that address.
		void ret(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.PC = detail::pop(cpu, memory);
			cpu.cycles += 8;
		}

		void ret_nz(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::ZERO))
			{
				cpu.cycles += 4;
			}
			else
			{
				cpu.registers.PC = detail::pop(cpu, memory);
				cpu.cycles += 10;
			}

		}

		void ret_z(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::ZERO))
			{
				cpu.registers.PC = detail::pop(cpu, memory);
				cpu.cycles += 10;
			}
			else
			{
				cpu.cycles += 4;
			}
		}

		void ret_nc(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::CARRY))
			{
				cpu.cycles += 4;
			}
			else
			{
				cpu.registers.PC = detail::pop(cpu, memory);
				cpu.cycles += 10;
			}

		}

		void ret_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::CARRY))
			{
				cpu.registers.PC = detail::pop(cpu, memory);
				cpu.cycles += 10;
			}
			else
			{
				cpu.cycles += 4;
			}
		}

		void reti(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.interrupt_master = true;
			cpu.registers.PC = detail::pop(cpu, memory);
			cpu.cycles += 8;
		}

		void rst_00(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.PC);
			cpu.registers.PC = 0x0000;
			cpu.cycles += 32;
		}

		void rst_08(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.PC);
			cpu.registers.PC = 0x0008;
			cpu.cycles += 32;
		}

		void rst_10(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.PC);
			cpu.registers.PC = 0x0010;
			cpu.cycles += 32;
		}

		void rst_18(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.PC);
			cpu.registers.PC = 0x0018;
			cpu.cycles += 32;
		}

		void rst_20(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.PC);
			cpu.registers.PC = 0x0020;
			cpu.cycles += 32;
		}

		void rst_28(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.PC);
			cpu.registers.PC = 0x0028;
			cpu.cycles += 32;
		}

		void rst_30(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.PC);
			cpu.registers.PC = 0x0030;
			cpu.cycles += 32;
		}

		void rst_38(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::push(cpu, memory, cpu.registers.PC);
			cpu.registers.PC = 0x0038;
			cpu.cycles += 32;
		}

		void jr_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.PC += (int8_t)operands[0];
			cpu.cycles += 8;
		}

		void jr_nz_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::ZERO))
			{
				cpu.cycles += 8;
			}
			else
			{
				cpu.registers.PC += (int8_t)operands[0];
				cpu.cycles += 12;
			}
		}

		void jr_z_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::ZERO))
			{
				cpu.registers.PC += (int8_t)operands[0];
				cpu.cycles += 12;
			}
			else
			{
				cpu.cycles += 8;
			}
		}

		void jr_nc_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::CARRY))
			{
				cpu.cycles += 8;
			}
			else
			{
				cpu.registers.PC += (int8_t)operands[0];
				cpu.cycles += 12;
			}
		}

		void jr_c_n(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::CARRY))
			{
				cpu.registers.PC += (int8_t)operands[0];
				cpu.cycles += 12;
			}
			else
			{
				cpu.cycles += 8;
			}
		}

		void jp_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.PC = memory::make_word(operands[0], operands[1]);
			cpu.cycles += 12;
		}

		void jp_nz_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::ZERO))
			{
				cpu.cycles += 12;
			}
			else
			{
				cpu.registers.PC = memory::make_word(operands[0], operands[1]);
				cpu.cycles += 16;
			}
		}

		void jp_z_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::ZERO))
			{
				cpu.registers.PC = memory::make_word(operands[0], operands[1]);
				cpu.cycles += 16;
			}
			else
			{
				cpu.cycles += 12;
			}
		}
		void jp_nc_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::CARRY))
			{
				cpu.cycles += 12;
			}
			else
			{
				cpu.registers.PC = memory::make_word(operands[0], operands[1]);
				cpu.cycles += 16;
			}
		}

		void jp_c_nn(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			if (get_flag(cpu, flags::CARRY))
			{
				cpu.registers.PC = memory::make_word(operands[0], operands[1]);
				cpu.cycles += 16;
			}
			else
			{
				cpu.cycles += 12;
			}
		}

		void jp_hl(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			cpu.registers.PC = cpu.registers.HL;
			cpu.cycles += 4;
		}

		namespace detail
		{
			void rr_n(cpu& cpu, uint8_t& n)
			{
				n >>= 1;

				if (get_flag(cpu, flags::CARRY))
				{
					n |= 0x80;
				}

				if (n & 0x01)
				{
					set_flag(cpu, flags::HALF_CARRY);
				}
				else
				{
					clear_flag(cpu, flags::HALF_CARRY);
				}

				if (n == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}

				clear_flag(cpu, flags::NEGATIVE);
				clear_flag(cpu, flags::HALF_CARRY);
			}
		}

		void rra(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::rr_n(cpu, cpu.registers.A);
			cpu.cycles += 4;
		}

		// Halt CPU & LCD display until button pressed.
		void stop(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			// TODO:
			uint8_t unused = operands[0];
			cpu.cycles += 4;
		}

		// Power down CPU until an interrupt occurs.
		void halt(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			// TODO:
			cpu.cycles += 4;
		}

		typedef void(*func_ptr)(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory);

		struct instruction
		{
			const char* format;
			func_ptr exec;
			uint8_t operand_count;
		};

		static const instruction instructions[0x100] =
		{
			{ "NOP",				nop,		0 },	// 0x00
			{ "LD BC, nn",			ld_bc_nn,	2 },	// 0x01
			{ "LD (BC), A",			ld_BC_a,	0 },	// 0x02
			{ "INC BC",				inc_bc,		0 },	// 0x03
			{ "INC B",				inc_b,		0 },	// 0x04
			{ "DEC B",				dec_b,		0 },	// 0x05
			{ "LD B, n",			ld_b_n,		1 },	// 0x06
			{ "",					nullptr,	0 },	// 0x07
			{ "",					nullptr,	0 },	// 0x08
			{ "ADD HL, BC",			add_hl_bc,	0 },	// 0x09
			{ "LD A, (BC)",			ld_a_BC,	0 },	// 0x0A
			{ "DEC BC",				dec_bc,		0 },	// 0x0B
			{ "INC C",				inc_c,		0 },	// 0x0C
			{ "DEC C",				dec_c,		0 },	// 0x0D
			{ "LD C, n",			ld_c_n,		1 },	// 0x0E
			{ "",					nullptr,	0 },	// 0x0F
			{ "STOP",				stop,		1 },	// 0x10
			{ "LD DE, nn",			ld_de_nn,	2 },	// 0x11
			{ "LD (DE), A",			ld_DE_a,	0 },	// 0x12
			{ "INC DE",				inc_de,		0 },	// 0x13
			{ "INC D",				inc_d,		0 },	// 0x14
			{ "DEC D",				dec_d,		0 },	// 0x15
			{ "LD D, n",			ld_d_n,		1 },	// 0x16
			{ "",					nullptr,	0 },	// 0x17
			{ "JR N",				jr_n,		1 },	// 0x18
			{ "ADD HL, DE",			add_hl_de,	0 },	// 0x19
			{ "LD A, (DE)",			ld_a_DE,	0 },	// 0x1A
			{ "DEC DE",				dec_de,		0 },	// 0x1B
			{ "INC E",				inc_e,		0 },	// 0x1C
			{ "DEC E",				dec_e,		0 },	// 0x1D
			{ "LD E, n",			ld_e_n,		1 },	// 0x1E
			{ "RRA",				rra,		0 },	// 0x1F
			{ "JR NZ, n",			jr_nz_n,	1 },	// 0x20
			{ "LD HL, nn",			ld_hl_nn,	2 },	// 0x21
			{ "LDI (HL), A",		ldi_HL_a,	0 },	// 0x22
			{ "INC HL",				inc_hl,		0 },	// 0x23
			{ "INC H",				inc_h,		0 },	// 0x24
			{ "DEC H",				dec_h,		0 },	// 0x25
			{ "LD H, n",			ld_h_n,		1 },	// 0x26
			{ "",					nullptr,	0 },	// 0x27
			{ "JR Z, n",			jr_z_n,		1 },	// 0x28
			{ "ADD HL, HL",			add_hl_hl,	0 },	// 0x29
			{ "LDI A, (HL)",		ldi_a_HL,	0 },	// 0x2A
			{ "DEC HL",				dec_hl,		0 },	// 0x2B
			{ "INC L",				inc_l,		0 },	// 0x2C
			{ "DEC L",				dec_l,		0 },	// 0x2D
			{ "LD L, n",			ld_l_n,		1 },	// 0x2E
			{ "CPL",				cpl,		0 },	// 0x2F
			{ "JR NC, n",			jr_nc_n,	0 },	// 0x30
			{ "LD SP, nn",			ld_sp_nn,	2 },	// 0x31
			{ "LDD (HL), a",		ldd_HL_a,	0 },	// 0x32
			{ "INC SP",				inc_sp,		0 },	// 0x33
			{ "INC (HL)",			inc_HL,		0 },	// 0x34
			{ "DEC (HL)",			dec_HL,		0 },	// 0x35
			{ "LD (HL), n",			ld_HL_n,	1 },	// 0x36
			{ "",					nullptr,	0 },	// 0x37
			{ "JR C, n",			jr_c_n,		1 },	// 0x38
			{ "ADD HL, SP",			add_hl_sp,	0 },	// 0x39
			{ "",					nullptr,	0 },	// 0x3A
			{ "DEC SP",				dec_sp,		0 },	// 0x3B
			{ "INC A",				inc_a,		0 },	// 0x3C
			{ "DEC A",				dec_a,		0 },	// 0x3D
			{ "LD A, n",			ld_a_n,		1 },	// 0x3E
			{ "",					nullptr,	0 },	// 0x3F
			{ "",					nullptr,	0 },	// 0x40
			{ "",					nullptr,	0 },	// 0x41
			{ "",					nullptr,	0 },	// 0x42
			{ "",					nullptr,	0 },	// 0x43
			{ "",					nullptr,	0 },	// 0x44
			{ "",					nullptr,	0 },	// 0x45
			{ "LD B, (HL)",			ld_b_HL,	0 },	// 0x46
			{ "LD B, A",			ld_b_a,		0 },	// 0x47
			{ "",					nullptr,	0 },	// 0x48
			{ "",					nullptr,	0 },	// 0x49
			{ "",					nullptr,	0 },	// 0x4A
			{ "",					nullptr,	0 },	// 0x4B
			{ "",					nullptr,	0 },	// 0x4C
			{ "",					nullptr,	0 },	// 0x4D
			{ "LD C, (HL)",			ld_c_HL,	0 },	// 0x4E
			{ "LD C, A",			ld_c_a,		0 },	// 0x4F
			{ "",					nullptr,	0 },	// 0x50
			{ "",					nullptr,	0 },	// 0x51
			{ "",					nullptr,	0 },	// 0x52
			{ "",					nullptr,	0 },	// 0x53
			{ "",					nullptr,	0 },	// 0x54
			{ "",					nullptr,	0 },	// 0x55
			{ "LD D, (HL)",			ld_d_HL,	0 },	// 0x56
			{ "LD D, A",			ld_d_a,		0 },	// 0x57
			{ "",					nullptr,	0 },	// 0x58
			{ "",					nullptr,	0 },	// 0x59
			{ "",					nullptr,	0 },	// 0x5A
			{ "",					nullptr,	0 },	// 0x5B
			{ "",					nullptr,	0 },	// 0x5C
			{ "",					nullptr,	0 },	// 0x5D
			{ "LD E, (HL)",			ld_e_HL,	0 },	// 0x5E
			{ "LD E, A",			ld_e_a,		0 },	// 0x5F
			{ "LD H, B",			ld_h_b,		0 },	// 0x60
			{ "LD H, C",			ld_h_c,		0 },	// 0x61
			{ "LD H, D",			ld_h_d,		0 },	// 0x62
			{ "LD H, E",			ld_h_e,		0 },	// 0x63
			{ "LD H, H",			ld_h_h,		0 },	// 0x64
			{ "LD H, L",			ld_h_l,		0 },	// 0x65
			{ "LD H, (HL)",			ld_h_HL,	0 },	// 0x66
			{ "LD H, A",			ld_h_a,		0 },	// 0x67
			{ "LD L, B",			ld_l_b,		0 },	// 0x68
			{ "LD L, C",			ld_l_c,		0 },	// 0x69
			{ "LD L, D",			ld_l_d,		0 },	// 0x6A
			{ "LD L, E",			ld_l_e,		0 },	// 0x6B
			{ "LD L, H",			ld_l_h,		0 },	// 0x6C
			{ "LD L, L",			ld_l_l,		0 },	// 0x6D
			{ "LD L, (HL)",			ld_l_HL,	0 },	// 0x6E
			{ "LD L, A",			ld_l_a,		0 },	// 0x6F
			{ "LD (HL), B",			ld_HL_b,	0 },	// 0x70
			{ "LD (HL), C",			ld_HL_c,	0 },	// 0x71
			{ "LD (HL), D",			ld_HL_d,	0 },	// 0x72
			{ "LD (HL), E",			ld_HL_e,	0 },	// 0x73
			{ "LD (HL), H",			ld_HL_h,	0 },	// 0x74
			{ "LD (HL), L",			ld_HL_l,	0 },	// 0x75
			{ "HALT",				halt,		0 },	// 0x76
			{ "LD (HL), A",			ld_HL_a,	0 },	// 0x77
			{ "LD A, B",			ld_a_b,		0 },	// 0x78
			{ "LD A, C",			ld_a_c,		0 },	// 0x79
			{ "LD A, D",			ld_a_d,		0 },	// 0x7A
			{ "LD A, E",			ld_a_e,		0 },	// 0x7B
			{ "LD A, H",			ld_a_h,		0 },	// 0x7C
			{ "LD A, L",			ld_a_l,		0 },	// 0x7D
			{ "LD A, (HL)",			ld_a_HL,	0 },	// 0x7E
			{ "LD A, A",			ld_a_a,		0 },	// 0x7F
			{ "ADD A, B",			add_a_b,	0 },	// 0x80
			{ "ADD A, C",			add_a_c,	0 },	// 0x81
			{ "ADD A, D",			add_a_d,	0 },	// 0x82
			{ "ADD A, E",			add_a_e,	0 },	// 0x83
			{ "ADD A, H",			add_a_h,	0 },	// 0x84
			{ "ADD A, L",			add_a_l,	0 },	// 0x85
			{ "ADD A, (HL)",		add_a_HL,	0 },	// 0x86
			{ "ADD A, A",			add_a_a,	0 },	// 0x87
			{ "ADC A, B",			adc_a_b,	0 },	// 0x88
			{ "ADC A, C",			adc_a_c,	0 },	// 0x89
			{ "ADC A, D",			adc_a_d,	0 },	// 0x8A
			{ "ADC A, E",			adc_a_e,	0 },	// 0x8B
			{ "ADC A, H",			adc_a_h,	0 },	// 0x8C
			{ "ADC A, L",			adc_a_l,	0 },	// 0x8D
			{ "ADC A, (HL)",		adc_a_HL,	0 },	// 0x8E
			{ "ADC A, A",			adc_a_a,	0 },	// 0x8F
			{ "SUB B",				sub_a_b,	0 },	// 0x90
			{ "SUB C",				sub_a_c,	0 },	// 0x91
			{ "SUB D",				sub_a_d,	0 },	// 0x92
			{ "SUB E",				sub_a_e,	0 },	// 0x93
			{ "SUB H",				sub_a_h,	0 },	// 0x94
			{ "SUB L",				sub_a_l,	0 },	// 0x95
			{ "SUB (HL)",			sub_a_HL,	0 },	// 0x96
			{ "SUB A",				sub_a_a,	0 },	// 0x97
			{ "",					nullptr,	0 },	// 0x98
			{ "",					nullptr,	0 },	// 0x99
			{ "",					nullptr,	0 },	// 0x9A
			{ "",					nullptr,	0 },	// 0x9B
			{ "",					nullptr,	0 },	// 0x9C
			{ "",					nullptr,	0 },	// 0x9D
			{ "",					nullptr,	0 },	// 0x9E
			{ "",					nullptr,	0 },	// 0x9F
			{ "AND B",				and_b,		0 },	// 0xA0
			{ "AND C",				and_c,		0 },	// 0xA1
			{ "AND D",				and_d,		0 },	// 0xA2
			{ "AND E",				and_e,		0 },	// 0xA3
			{ "AND H",				and_h,		0 },	// 0xA4
			{ "AND L",				and_l,		0 },	// 0xA5
			{ "AND (HL)",			and_HL,		0 },	// 0xA6
			{ "AND A",				and_a,		0 },	// 0xA7
			{ "XOR B",				xor_b,		0 },	// 0xA8
			{ "XOR C",				xor_c,		0 },	// 0xA9
			{ "XOR D",				xor_d,		0 },	// 0xAA
			{ "XOR E",				xor_e,		0 },	// 0xAB
			{ "XOR H",				xor_h,		0 },	// 0xAC
			{ "XOR L",				xor_l,		0 },	// 0xAD
			{ "COR (HL)",			xor_HL,		0 },	// 0xAE
			{ "XOR A",				xor_a,		0 },	// 0xAF
			{ "OR B",				or_b,		0 },	// 0xB0
			{ "OR C",				or_c,		0 },	// 0xB1
			{ "OR D",				or_d,		0 },	// 0xB2
			{ "OR E",				or_e,		0 },	// 0xB3
			{ "OR H",				or_h,		0 },	// 0xB4
			{ "OR L",				or_l,		0 },	// 0xB5
			{ "",					nullptr,	0 },	// 0xB6
			{ "OR A",				or_a,		0 },	// 0xB7
			{ "CP B",				cp_b,		0 },	// 0xB8
			{ "CP C",				cp_c,		0 },	// 0xB9
			{ "CP D",				cp_d,		0 },	// 0xBA
			{ "CP E",				cp_e,		0 },	// 0xBB
			{ "CP H",				cp_h,		0 },	// 0xBC
			{ "CP L",				cp_l,		0 },	// 0xBD
			{ "",					nullptr,	0 },	// 0xBE
			{ "CP A",				cp_a,		0 },	// 0xBF
			{ "RET NZ",				ret_nz,		0 },	// 0xC0
			{ "POP BC",				pop_bc,		0 },	// 0xC1
			{ "JP NZ, nn",			jp_nz_nn,	2 },	// 0xC2
			{ "JP nn",				jp_nn,		2 },	// 0xC3
			{ "CALL NZ, nn",		call_nz_nn,	2 },	// 0xC4
			{ "PUSH BC",			push_bc,	0 },	// 0xC5
			{ "ADD A, n",			add_a_n,	1 },	// 0xC6
			{ "RST 00",				rst_00,		0 },	// 0xC7
			{ "RET Z",				ret_z,		0 },	// 0xC8
			{ "RET",				ret,		0 },	// 0xC9
			{ "JP Z, nn",			jp_z_nn,	2 },	// 0xCA
			{ nullptr,				nullptr,	1 },	// 0xCB
			{ "CALL Z, nn",			call_z_nn,	2 },	// 0xCC
			{ "CALL nn",			call_nn,	2 },	// 0xCD
			{ "ADC A, n",			adc_a_n,	1 },	// 0xCE
			{ "RST 08",				rst_08,		0 },	// 0xCF
			{ "RET NC",				ret_nc,		0 },	// 0xD0
			{ "POP DE",				pop_de,		0 },	// 0xD1
			{ "JP NC, nn",			jp_nc_nn,	2 },	// 0xD2
			{ "",					nullptr,	0 },	// 0xD3
			{ "CALL NC, nn",		call_nc_nn,	2 },	// 0xD4
			{ "PUSH DE",			push_de,	0 },	// 0xD5
			{ "SUB N",				sub_a_n,	1 },	// 0xD6
			{ "RST 10",				rst_10,		0 },	// 0xD7
			{ "RET C",				ret_c,		0 },	// 0xD8
			{ "RETI",				reti,		0 },	// 0xD9
			{ "JP C, nn",			jp_c_nn,	2 },	// 0xDA
			{ "",					nullptr,	0 },	// 0xDB
			{ "CALL C, nn",			call_c_nn,	2 },	// 0xDC
			{ "",					nullptr,	0 },	// 0xDD
			{ "",					nullptr,	0 },	// 0xDE
			{ "RST 18",				rst_18,		0 },	// 0xDF
			{ "LD (FF00 + n), A",	ld_FFN_a,	1 },	// 0xE0
			{ "POP HL",				pop_hl,		0 },	// 0xE1
			{ "LD (FF00 + C), A",	ld_FFC_a,	0 },	// 0xE2
			{ "",					nullptr,	0 },	// 0xE3
			{ "",					nullptr,	0 },	// 0xE4
			{ "PUSH HL",			push_hl,	0 },	// 0xE5
			{ "AND N",				and_n,		1 },	// 0xE6
			{ "RST 20",				rst_20,		0 },	// 0xE7
			{ "",					nullptr,	0 },	// 0xE8
			{ "JP HL",				jp_hl,		0 },	// 0xE9
			{ "LD (nn), A",			ld_NN_a,	2 },	// 0xEA
			{ "",					nullptr,	0 },	// 0xEB
			{ "",					nullptr,	0 },	// 0xEC
			{ "",					nullptr,	0 },	// 0xED
			{ "XOR N",				xor_n,		1 },	// 0xEE
			{ "RST 28",				rst_28,		0 },	// 0xEF
			{ "LD A, (FF00 + n)",	ld_a_FFN,	1 },	// 0xF0
			{ "POP AF",				pop_af,		0 },	// 0xF1
			{ "LD A, (FF00 + C)",	ld_a_FFC,	0 },	// 0xF2
			{ "DI",					di,			0 },	// 0xF3
			{ "",					nullptr,	0 },	// 0xF4
			{ "PUSH AF",			push_af,	0 },	// 0xF5
			{ "",					nullptr,	0 },	// 0xF6
			{ "RST 30",				rst_30,		0 },	// 0xF7
			{ "",					nullptr,	0 },	// 0xF8
			{ "",					nullptr,	0 },	// 0xF9
			{ "LD A, (nn)",			ld_a_NN,	2 },	// 0xFA
			{ "EI",					ei,			0 },	// 0xFB
			{ "",					nullptr,	0 },	// 0xFC
			{ "",					nullptr,	0 },	// 0xFD
			{ "CP N",				cp_n,		1 },	// 0xFE
			{ "RST 38",				rst_38,		0 },	// 0xFF
		};

		namespace detail
		{
			void swap(cpu& cpu, uint8_t& n)
			{
				n = ((n & 0xf) << 4) | ((n & 0xf0) >> 4);

				if (n == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}
				clear_flag(cpu, flags::NEGATIVE);
				clear_flag(cpu, flags::HALF_CARRY);
				clear_flag(cpu, flags::CARRY);
			}
		}

		void swap_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::swap(cpu, cpu.registers.A);
			cpu.cycles += 8;
		}

		namespace detail
		{
			void srl_n(cpu& cpu, uint8_t& n)
			{
				if (n & 0x01)
				{
					set_flag(cpu, flags::CARRY);
				}
				else
				{
					clear_flag(cpu, flags::CARRY);
				}

				n = n >> 1;

				if (n == 0)
				{
					set_flag(cpu, flags::ZERO);
				}
				else
				{
					clear_flag(cpu, flags::ZERO);
				}

				clear_flag(cpu, flags::NEGATIVE);
				clear_flag(cpu, flags::HALF_CARRY);
			}
		}

		void srl_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::srl_n(cpu, cpu.registers.A);
			cpu.cycles += 8;
		}

		void srl_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::srl_n(cpu, cpu.registers.B);
			cpu.cycles += 8;
		}

		void srl_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::srl_n(cpu, cpu.registers.C);
			cpu.cycles += 8;
		}

		void srl_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::srl_n(cpu, cpu.registers.D);
			cpu.cycles += 8;
		}

		void srl_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::srl_n(cpu, cpu.registers.E);
			cpu.cycles += 8;
		}

		void srl_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::srl_n(cpu, cpu.registers.H);
			cpu.cycles += 8;
		}

		void srl_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::srl_n(cpu, cpu.registers.L);
			cpu.cycles += 8;
		}

		void srl_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::srl_n(cpu, memory[cpu.registers.HL]);
			cpu.cycles += 16;
		}

		void rr_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::rr_n(cpu, cpu.registers.A);
			cpu.cycles += 8;
		}

		void rr_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::rr_n(cpu, cpu.registers.B);
			cpu.cycles += 8;
		}

		void rr_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::rr_n(cpu, cpu.registers.C);
			cpu.cycles += 8;
		}

		void rr_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::rr_n(cpu, cpu.registers.D);
			cpu.cycles += 8;
		}

		void rr_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::rr_n(cpu, cpu.registers.E);
			cpu.cycles += 8;
		}

		void rr_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::rr_n(cpu, cpu.registers.H);
			cpu.cycles += 8;
		}

		void rr_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::rr_n(cpu, cpu.registers.L);
			cpu.cycles += 8;
		}

		void rr_HL(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::rr_n(cpu, memory[cpu.registers.HL]);
			cpu.cycles += 8;
		}

		namespace detail
		{
			void res_b_n(cpu& cpu, uint8_t b, uint8_t& n)
			{
				n &= ~(1 << b);
			}
		}

		void res_0_a(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::res_b_n(cpu, 0, cpu.registers.A);
			cpu.cycles += 8;
		}

		void res_0_b(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::res_b_n(cpu, 0, cpu.registers.B);
			cpu.cycles += 8;
		}

		void res_0_c(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::res_b_n(cpu, 0, cpu.registers.C);
			cpu.cycles += 8;
		}

		void res_0_d(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::res_b_n(cpu, 0, cpu.registers.D);
			cpu.cycles += 8;
		}

		void res_0_e(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::res_b_n(cpu, 0, cpu.registers.E);
			cpu.cycles += 8;
		}

		void res_0_h(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::res_b_n(cpu, 0, cpu.registers.H);
			cpu.cycles += 8;
		}

		void res_0_l(uint8_t operands[2], cpu& cpu, std::vector<uint8_t>& memory)
		{
			detail::res_b_n(cpu, 0, cpu.registers.L);
			cpu.cycles += 8;
		}

		static const instruction extended_instructions[0x100] =
		{
			{ "",					nullptr,	0 },	// 0xCB 0x00
			{ "",					nullptr,	0 },	// 0xCB 0x01
			{ "",					nullptr,	0 },	// 0xCB 0x02
			{ "",					nullptr,	0 },	// 0xCB 0x03
			{ "",					nullptr,	0 },	// 0xCB 0x04
			{ "",					nullptr,	0 },	// 0xCB 0x05
			{ "",					nullptr,	0 },	// 0xCB 0x06
			{ "",					nullptr,	0 },	// 0xCB 0x07
			{ "",					nullptr,	0 },	// 0xCB 0x08
			{ "",					nullptr,	0 },	// 0xCB 0x09
			{ "",					nullptr,	0 },	// 0xCB 0x0A
			{ "",					nullptr,	0 },	// 0xCB 0x0B
			{ "",					nullptr,	0 },	// 0xCB 0x0C
			{ "",					nullptr,	0 },	// 0xCB 0x0D
			{ "",					nullptr,	0 },	// 0xCB 0x0E
			{ "",					nullptr,	0 },	// 0xCB 0x0F
			{ "",					nullptr,	0 },	// 0xCB 0x10
			{ "",					nullptr,	0 },	// 0xCB 0x11
			{ "",					nullptr,	0 },	// 0xCB 0x12
			{ "",					nullptr,	0 },	// 0xCB 0x13
			{ "",					nullptr,	0 },	// 0xCB 0x14
			{ "",					nullptr,	0 },	// 0xCB 0x15
			{ "",					nullptr,	0 },	// 0xCB 0x16
			{ "",					nullptr,	0 },	// 0xCB 0x17
			{ "RR B",				rr_b,		0 },	// 0xCB 0x18
			{ "RR C",				rr_c,		0 },	// 0xCB 0x19
			{ "RR D",				rr_d,		0 },	// 0xCB 0x1A
			{ "RR E",				rr_e,		0 },	// 0xCB 0x1B
			{ "RR H",				rr_h,		0 },	// 0xCB 0x1C
			{ "RR L",				rr_l,		0 },	// 0xCB 0x1D
			{ "RR (HL)",			rr_HL,		0 },	// 0xCB 0x1E
			{ "RR A",				rr_a,		0 },	// 0xCB 0x1F
			{ "",					nullptr,	0 },	// 0xCB 0x20
			{ "",					nullptr,	0 },	// 0xCB 0x21
			{ "",					nullptr,	0 },	// 0xCB 0x22
			{ "",					nullptr,	0 },	// 0xCB 0x23
			{ "",					nullptr,	0 },	// 0xCB 0x24
			{ "",					nullptr,	0 },	// 0xCB 0x25
			{ "",					nullptr,	0 },	// 0xCB 0x26
			{ "",					nullptr,	0 },	// 0xCB 0x27
			{ "",					nullptr,	0 },	// 0xCB 0x28
			{ "",					nullptr,	0 },	// 0xCB 0x29
			{ "",					nullptr,	0 },	// 0xCB 0x2A
			{ "",					nullptr,	0 },	// 0xCB 0x2B
			{ "",					nullptr,	0 },	// 0xCB 0x2C
			{ "",					nullptr,	0 },	// 0xCB 0x2D
			{ "",					nullptr,	0 },	// 0xCB 0x2E
			{ "",					nullptr,	0 },	// 0xCB 0x2F
			{ "",					nullptr,	0 },	// 0xCB 0x30
			{ "",					nullptr,	0 },	// 0xCB 0x31
			{ "",					nullptr,	0 },	// 0xCB 0x32
			{ "",					nullptr,	0 },	// 0xCB 0x33
			{ "",					nullptr,	0 },	// 0xCB 0x34
			{ "",					nullptr,	0 },	// 0xCB 0x35
			{ "",					nullptr,	0 },	// 0xCB 0x36
			{ "SWAP A",				swap_a,		0 },	// 0xCB 0x37
			{ "SRL B",				srl_b,		0 },	// 0xCB 0x38
			{ "SRL C",				srl_c,		0 },	// 0xCB 0x39
			{ "SRL D",				srl_d,		0 },	// 0xCB 0x3A
			{ "SRL E",				srl_e,		0 },	// 0xCB 0x3B
			{ "SRL H",				srl_h,		0 },	// 0xCB 0x3C
			{ "SRL L",				srl_l,		0 },	// 0xCB 0x3D
			{ "SRL (HL)",			srl_HL,		0 },	// 0xCB 0x3E
			{ "SRL A",				srl_a,		0 },	// 0xCB 0x3F
			{ "",					nullptr,	0 },	// 0xCB 0x40
			{ "",					nullptr,	0 },	// 0xCB 0x41
			{ "",					nullptr,	0 },	// 0xCB 0x42
			{ "",					nullptr,	0 },	// 0xCB 0x43
			{ "",					nullptr,	0 },	// 0xCB 0x44
			{ "",					nullptr,	0 },	// 0xCB 0x45
			{ "",					nullptr,	0 },	// 0xCB 0x46
			{ "",					nullptr,	0 },	// 0xCB 0x47
			{ "",					nullptr,	0 },	// 0xCB 0x48
			{ "",					nullptr,	0 },	// 0xCB 0x49
			{ "",					nullptr,	0 },	// 0xCB 0x4A
			{ "",					nullptr,	0 },	// 0xCB 0x4B
			{ "",					nullptr,	0 },	// 0xCB 0x4C
			{ "",					nullptr,	0 },	// 0xCB 0x4D
			{ "",					nullptr,	0 },	// 0xCB 0x4E
			{ "",					nullptr,	0 },	// 0xCB 0x4F
			{ "",					nullptr,	0 },	// 0xCB 0x50
			{ "",					nullptr,	0 },	// 0xCB 0x51
			{ "",					nullptr,	0 },	// 0xCB 0x52
			{ "",					nullptr,	0 },	// 0xCB 0x53
			{ "",					nullptr,	0 },	// 0xCB 0x54
			{ "",					nullptr,	0 },	// 0xCB 0x55
			{ "",					nullptr,	0 },	// 0xCB 0x56
			{ "",					nullptr,	0 },	// 0xCB 0x57
			{ "",					nullptr,	0 },	// 0xCB 0x58
			{ "",					nullptr,	0 },	// 0xCB 0x59
			{ "",					nullptr,	0 },	// 0xCB 0x5A
			{ "",					nullptr,	0 },	// 0xCB 0x5B
			{ "",					nullptr,	0 },	// 0xCB 0x5C
			{ "",					nullptr,	0 },	// 0xCB 0x5D
			{ "",					nullptr,	0 },	// 0xCB 0x5E
			{ "",					nullptr,	0 },	// 0xCB 0x5F
			{ "",					nullptr,	0 },	// 0xCB 0x60
			{ "",					nullptr,	0 },	// 0xCB 0x61
			{ "",					nullptr,	0 },	// 0xCB 0x62
			{ "",					nullptr,	0 },	// 0xCB 0x63
			{ "",					nullptr,	0 },	// 0xCB 0x64
			{ "",					nullptr,	0 },	// 0xCB 0x65
			{ "",					nullptr,	0 },	// 0xCB 0x66
			{ "",					nullptr,	0 },	// 0xCB 0x67
			{ "",					nullptr,	0 },	// 0xCB 0x68
			{ "",					nullptr,	0 },	// 0xCB 0x69
			{ "",					nullptr,	0 },	// 0xCB 0x6A
			{ "",					nullptr,	0 },	// 0xCB 0x6B
			{ "",					nullptr,	0 },	// 0xCB 0x6C
			{ "",					nullptr,	0 },	// 0xCB 0x6D
			{ "",					nullptr,	0 },	// 0xCB 0x6E
			{ "",					nullptr,	0 },	// 0xCB 0x6F
			{ "",					nullptr,	0 },	// 0xCB 0x70
			{ "",					nullptr,	0 },	// 0xCB 0x71
			{ "",					nullptr,	0 },	// 0xCB 0x72
			{ "",					nullptr,	0 },	// 0xCB 0x73
			{ "",					nullptr,	0 },	// 0xCB 0x74
			{ "",					nullptr,	0 },	// 0xCB 0x75
			{ "",					nullptr,	0 },	// 0xCB 0x76
			{ "",					nullptr,	0 },	// 0xCB 0x77
			{ "",					nullptr,	0 },	// 0xCB 0x78
			{ "",					nullptr,	0 },	// 0xCB 0x79
			{ "",					nullptr,	0 },	// 0xCB 0x7A
			{ "",					nullptr,	0 },	// 0xCB 0x7B
			{ "",					nullptr,	0 },	// 0xCB 0x7C
			{ "",					nullptr,	0 },	// 0xCB 0x7D
			{ "",					nullptr,	0 },	// 0xCB 0x7E
			{ "",					nullptr,	0 },	// 0xCB 0x7F
			{ "RES 0, B",			res_0_b,	0 },	// 0xCB 0x80
			{ "RES 0, C",			res_0_c,	0 },	// 0xCB 0x81
			{ "RES 0, D",			res_0_d,	0 },	// 0xCB 0x82
			{ "RES 0, E",			res_0_e,	0 },	// 0xCB 0x83
			{ "RES 0, H",			res_0_h,	0 },	// 0xCB 0x84
			{ "RES 0, L",			res_0_l,	0 },	// 0xCB 0x85
			{ "",					nullptr,	0 },	// 0xCB 0x86
			{ "RES 0, A",			res_0_a,	0 },	// 0xCB 0x87
			{ "",					nullptr,	0 },	// 0xCB 0x88
			{ "",					nullptr,	0 },	// 0xCB 0x89
			{ "",					nullptr,	0 },	// 0xCB 0x8A
			{ "",					nullptr,	0 },	// 0xCB 0x8B
			{ "",					nullptr,	0 },	// 0xCB 0x8C
			{ "",					nullptr,	0 },	// 0xCB 0x8D
			{ "",					nullptr,	0 },	// 0xCB 0x8E
			{ "",					nullptr,	0 },	// 0xCB 0x8F
			{ "",					nullptr,	0 },	// 0xCB 0x90
			{ "",					nullptr,	0 },	// 0xCB 0x91
			{ "",					nullptr,	0 },	// 0xCB 0x92
			{ "",					nullptr,	0 },	// 0xCB 0x93
			{ "",					nullptr,	0 },	// 0xCB 0x94
			{ "",					nullptr,	0 },	// 0xCB 0x95
			{ "",					nullptr,	0 },	// 0xCB 0x96
			{ "",					nullptr,	0 },	// 0xCB 0x97
			{ "",					nullptr,	0 },	// 0xCB 0x98
			{ "",					nullptr,	0 },	// 0xCB 0x99
			{ "",					nullptr,	0 },	// 0xCB 0x9A
			{ "",					nullptr,	0 },	// 0xCB 0x9B
			{ "",					nullptr,	0 },	// 0xCB 0x9C
			{ "",					nullptr,	0 },	// 0xCB 0x9D
			{ "",					nullptr,	0 },	// 0xCB 0x9E
			{ "",					nullptr,	0 },	// 0xCB 0x9F
			{ "",					nullptr,	0 },	// 0xCB 0xA0
			{ "",					nullptr,	0 },	// 0xCB 0xA1
			{ "",					nullptr,	0 },	// 0xCB 0xA2
			{ "",					nullptr,	0 },	// 0xCB 0xA3
			{ "",					nullptr,	0 },	// 0xCB 0xA4
			{ "",					nullptr,	0 },	// 0xCB 0xA5
			{ "",					nullptr,	0 },	// 0xCB 0xA6
			{ "",					nullptr,	0 },	// 0xCB 0xA7
			{ "",					nullptr,	0 },	// 0xCB 0xA8
			{ "",					nullptr,	0 },	// 0xCB 0xA9
			{ "",					nullptr,	0 },	// 0xCB 0xAA
			{ "",					nullptr,	0 },	// 0xCB 0xAB
			{ "",					nullptr,	0 },	// 0xCB 0xAC
			{ "",					nullptr,	0 },	// 0xCB 0xAD
			{ "",					nullptr,	0 },	// 0xCB 0xAE
			{ "",					nullptr,	0 },	// 0xCB 0xAF
			{ "",					nullptr,	0 },	// 0xCB 0xB0
			{ "",					nullptr,	0 },	// 0xCB 0xB1
			{ "",					nullptr,	0 },	// 0xCB 0xB2
			{ "",					nullptr,	0 },	// 0xCB 0xB3
			{ "",					nullptr,	0 },	// 0xCB 0xB4
			{ "",					nullptr,	0 },	// 0xCB 0xB5
			{ "",					nullptr,	0 },	// 0xCB 0xB6
			{ "",					nullptr,	0 },	// 0xCB 0xB7
			{ "",					nullptr,	0 },	// 0xCB 0xB8
			{ "",					nullptr,	0 },	// 0xCB 0xB9
			{ "",					nullptr,	0 },	// 0xCB 0xBA
			{ "",					nullptr,	0 },	// 0xCB 0xBB
			{ "",					nullptr,	0 },	// 0xCB 0xBC
			{ "",					nullptr,	0 },	// 0xCB 0xBD
			{ "",					nullptr,	0 },	// 0xCB 0xBE
			{ "",					nullptr,	0 },	// 0xCB 0xBF
			{ "",					nullptr,	0 },	// 0xCB 0xC0
			{ "",					nullptr,	0 },	// 0xCB 0xC1
			{ "",					nullptr,	0 },	// 0xCB 0xC2
			{ "",					nullptr,	0 },	// 0xCB 0xC3
			{ "",					nullptr,	0 },	// 0xCB 0xC4
			{ "",					nullptr,	0 },	// 0xCB 0xC5
			{ "",					nullptr,	0 },	// 0xCB 0xC6
			{ "",					nullptr,	0 },	// 0xCB 0xC7
			{ "",					nullptr,	0 },	// 0xCB 0xC8
			{ "",					nullptr,	0 },	// 0xCB 0xC9
			{ "",					nullptr,	0 },	// 0xCB 0xCA
			{ "",					nullptr,	0 },	// 0xCB 0xCB
			{ "",					nullptr,	0 },	// 0xCB 0xCC
			{ "",					nullptr,	0 },	// 0xCB 0xCD
			{ "",					nullptr,	0 },	// 0xCB 0xCE
			{ "",					nullptr,	0 },	// 0xCB 0xCF
			{ "",					nullptr,	0 },	// 0xCB 0xD0
			{ "",					nullptr,	0 },	// 0xCB 0xD1
			{ "",					nullptr,	0 },	// 0xCB 0xD2
			{ "",					nullptr,	0 },	// 0xCB 0xD3
			{ "",					nullptr,	0 },	// 0xCB 0xD4
			{ "",					nullptr,	0 },	// 0xCB 0xD5
			{ "",					nullptr,	0 },	// 0xCB 0xD6
			{ "",					nullptr,	0 },	// 0xCB 0xD7
			{ "",					nullptr,	0 },	// 0xCB 0xD8
			{ "",					nullptr,	0 },	// 0xCB 0xD9
			{ "",					nullptr,	0 },	// 0xCB 0xDA
			{ "",					nullptr,	0 },	// 0xCB 0xDB
			{ "",					nullptr,	0 },	// 0xCB 0xDC
			{ "",					nullptr,	0 },	// 0xCB 0xDD
			{ "",					nullptr,	0 },	// 0xCB 0xDE
			{ "",					nullptr,	0 },	// 0xCB 0xDF
			{ "",					nullptr,	0 },	// 0xCB 0xE0
			{ "",					nullptr,	0 },	// 0xCB 0xE1
			{ "",					nullptr,	0 },	// 0xCB 0xE2
			{ "",					nullptr,	0 },	// 0xCB 0xE3
			{ "",					nullptr,	0 },	// 0xCB 0xE4
			{ "",					nullptr,	0 },	// 0xCB 0xE5
			{ "",					nullptr,	0 },	// 0xCB 0xE6
			{ "",					nullptr,	0 },	// 0xCB 0xE7
			{ "",					nullptr,	0 },	// 0xCB 0xE8
			{ "",					nullptr,	0 },	// 0xCB 0xE9
			{ "",					nullptr,	0 },	// 0xCB 0xEA
			{ "",					nullptr,	0 },	// 0xCB 0xEB
			{ "",					nullptr,	0 },	// 0xCB 0xEC
			{ "",					nullptr,	0 },	// 0xCB 0xED
			{ "",					nullptr,	0 },	// 0xCB 0xEE
			{ "",					nullptr,	0 },	// 0xCB 0xEF
			{ "",					nullptr,	0 },	// 0xCB 0xF0
			{ "",					nullptr,	0 },	// 0xCB 0xF1
			{ "",					nullptr,	0 },	// 0xCB 0xF2
			{ "",					nullptr,	0 },	// 0xCB 0xF3
			{ "",					nullptr,	0 },	// 0xCB 0xF4
			{ "",					nullptr,	0 },	// 0xCB 0xF5
			{ "",					nullptr,	0 },	// 0xCB 0xF6
			{ "",					nullptr,	0 },	// 0xCB 0xF7
			{ "",					nullptr,	0 },	// 0xCB 0xF8
			{ "",					nullptr,	0 },	// 0xCB 0xF9
			{ "",					nullptr,	0 },	// 0xCB 0xFA
			{ "",					nullptr,	0 },	// 0xCB 0xFB
			{ "",					nullptr,	0 },	// 0xCB 0xFC
			{ "",					nullptr,	0 },	// 0xCB 0xFD
			{ "",					nullptr,	0 },	// 0xCB 0xFE
			{ "",					nullptr,	0 },	// 0xCB 0xFF
		};
	};

	int tick(cpu& cpu, std::vector<uint8_t>& memory)
	{
		static bool debug_instruction = false;
		static bool debug_registers = false;
		static bool debug_step = false;
		static bool debug_serial = false;

		// This is the ei instruction right before the first vblank
		if (cpu.registers.PC == 0x02BA)
		{
			static int i = 0;
			++i;
		}

		// This is the reti instructino at the end of the first vblank
		if (cpu.registers.PC == 0x020B)
		{
			static int i = 0;
			++i;
		}

		// This is the first call instruction right before the second vblank
		if (cpu.registers.PC == 0x02C4)
		{
			static int i = 0;
			++i;

			//debug_instruction = true;
			//debug_registers = true;
			//debug_step = true;
		}

		// This is the second call instruction right before the second vblank
		if (cpu.registers.PC == 0x02C7)
		{
			static int i = 0;
			++i;

			//debug_instruction = true;
			//debug_registers = true;
			//debug_step = true;
		}

		// This is where a tileset is first loaded into memory on Tetris
		if (cpu.registers.PC == 0x036C)
		{
			static int i = 0;
			++i;
		}


		if (debug_registers)
		{
			debug::print("\n");
			debug::print("AF = 0x%04X\n", cpu.registers.AF);
			debug::print("BC = 0x%04X\n", cpu.registers.BC);
			debug::print("DE = 0x%04X\n", cpu.registers.DE);
			debug::print("HL = 0x%04X\n", cpu.registers.HL);
			debug::print("SP = 0x%04X\n", cpu.registers.SP);
			debug::print("Z = %s\n", get_flag(cpu, flags::ZERO) ? "true" : "false");
			debug::print("N = %s\n", get_flag(cpu, flags::NEGATIVE) ? "true" : "false");
			debug::print("H = %s\n", get_flag(cpu, flags::HALF_CARRY) ? "true" : "false");
			debug::print("C = %s\n", get_flag(cpu, flags::CARRY) ? "true" : "false");
			debug::print("IME = %u\n", cpu.interrupt_master);
			debug::print("IE = 0x%02X\n", memory[memory::registers::IE]);
			debug::print("IF = 0x%02X\n", memory[memory::registers::IF]);
			debug::print("\n");
		}

		if (debug_instruction)
		{
			debug::print("%04X: ", cpu.registers.PC);
		}

		instructions::instruction instruction = instructions::instructions[0];

		const uint8_t opcode = memory::read_byte(memory, cpu.registers.PC++);

		if (debug_instruction)
		{
			debug::print("%02X ", opcode);
		}

		if (opcode == 0xCB)
		{
			const uint8_t extended_opcode = memory::read_byte(memory, cpu.registers.PC++);

			if (debug_instruction)
			{
				debug::print("%02X ", extended_opcode);
			}

			instruction = instructions::extended_instructions[extended_opcode];

			assert(instruction.exec);
		}
		else
		{
			instruction = instructions::instructions[opcode];

			assert(instruction.exec);
		}

		uint8_t operands[2];
		for (uint8_t i = 0; i < instruction.operand_count; ++i)
		{
			operands[i] = memory::read_byte(memory, cpu.registers.PC++);
		}

		if (debug_instruction)
		{
			uint8_t i = 0;
			for (; i < instruction.operand_count; ++i)
			{
				debug::print("%02X ", operands[i]);
			}
			for (; i < 2; ++i)
			{
				debug::print("   ");
			}
			debug::print("  %s\n", instruction.format);
		}

		if (debug_step)
		{
			getchar();
		}

		// If the current instruction enables interrupts we to delay acting on
		// any already requested interrupts until the end of the next instruction. 
		// The docs are somewhat ambiguous but this matches the behavior of BGB
		const bool interrupt_master = cpu.interrupt_master;

		instruction.exec(operands, cpu, memory);

		if (debug_serial)
		{
			debug::print("%u", memory[memory::registers::SB]);
		}

		// Handle interrupts
		if (interrupt_master)
		{
			static const uint8_t V_BLANK = 0x1;		// INT 40
			static const uint8_t LCD_STAT = 0x2;	// INT 48
			static const uint8_t TIMER = 0x4;		// INT 50
			static const uint8_t SERIAL = 0x8;		// INT 58
			static const uint8_t JOYPAD = 0x10;		// INT 60

			const uint8_t enabled_interrupts = memory[memory::registers::IE];
			const uint8_t requested_interrupts = memory[memory::registers::IF];
			const uint8_t triggered_interrupts = enabled_interrupts & requested_interrupts;

			if (triggered_interrupts)
			{
				if (triggered_interrupts & V_BLANK)
				{
					operands[0] = 0x40;
					operands[1] = 0x00;
					instructions::call_nn(operands, cpu, memory);
				}

				if (triggered_interrupts & LCD_STAT)
				{
					operands[0] = 0x48;
					operands[1] = 0x00;
					instructions::call_nn(operands, cpu, memory);
				}

				if (triggered_interrupts & TIMER)
				{
					operands[0] = 0x50;
					operands[1] = 0x00;
					instructions::call_nn(operands, cpu, memory);
				}

				if (triggered_interrupts & SERIAL)
				{
					operands[0] = 0x58;
					operands[1] = 0x00;
					instructions::call_nn(operands, cpu, memory);
				}

				if (triggered_interrupts & JOYPAD)
				{
					operands[0] = 0x60;
					operands[1] = 0x00;
					instructions::call_nn(operands, cpu, memory);
				}

				cpu.interrupt_master = 0;

				memory[memory::registers::IF] = requested_interrupts & ~triggered_interrupts;
			}
		}

		return 0;
	}
};

namespace display
{
	static const uint8_t width = 160;
	static const uint8_t height = 144;

	namespace detail
	{
		uint8_t read_tile_num(const std::vector<uint8_t>& memory, uint8_t bg_map_x, uint8_t bg_map_y)
		{
			// The gameboy contains two 32x32 tile background maps in VRAM at addresses 
			// 0x9800-0x9BFF and 0x9C00-0x9FFF. 
			return memory::read_byte(memory, 0x9800 + (((bg_map_y) / 8) * 32) + (bg_map_x / 8));
		}

		uint8_t read_tile_row(const std::vector<uint8_t>& memory, uint8_t tile_num, uint8_t row_idx, uint8_t pixel_start_idx, uint8_t pixel_end_idx, uint8_t* data)
		{
			assert(row_idx < 8);
			assert(pixel_start_idx <= 8);
			assert(pixel_end_idx <= 8);
			assert(pixel_start_idx <= pixel_end_idx);

			uint8_t pixels_read = 0;

			// Each tile is 8x8 4-color pixels (2 bpp). That makes each tile 16 bytes;
			const uint16_t row_addr = 0x8000 + (tile_num * 16) + row_idx * 2;
			const uint8_t row_msbs = memory::read_byte(memory, row_addr);
			const uint8_t row_lsbs = memory::read_byte(memory, row_addr + 1);
			for (uint8_t x = pixel_start_idx; x < pixel_end_idx; ++x)
			{
				data[pixels_read++] = (((row_msbs >> (7 - x)) & 0x1) << 1) | ((row_lsbs >> (7 - x)) & 0x1);
			}

			return pixels_read;
		}
	}

	void tick(const std::vector<uint8_t>& memory, uint8_t scanline_idx, uint8_t scanline[width])
	{
		// TODO: Tile bank switching
		// TODO: Wrapping

		assert(scanline_idx < height);

		const uint8_t SCX = memory::read_byte(memory, memory::registers::SCX);
		const uint8_t SCY = memory::read_byte(memory, memory::registers::SCY);

		uint8_t pixels_read = 0;

		{
			const uint8_t tile_num = detail::read_tile_num(memory, SCX, scanline_idx + SCY);
			pixels_read += detail::read_tile_row(memory, tile_num, (scanline_idx + SCY) % 8, SCX % 8, 8, &scanline[pixels_read]);
		}

		while (width - pixels_read >= 8)
		{
			const uint8_t tile_num = detail::read_tile_num(memory, SCX + pixels_read, scanline_idx + SCY);
			pixels_read += detail::read_tile_row(memory, tile_num, (scanline_idx + SCY) % 8, 0, 8, &scanline[pixels_read]);
		}

		const uint8_t remaining_pixels = width - pixels_read;
		if (remaining_pixels)
		{
			const uint8_t tile_num = detail::read_tile_num(memory, SCX + pixels_read, scanline_idx + SCY);
			pixels_read += detail::read_tile_row(memory, tile_num, (scanline_idx + SCY) % 8, 0, remaining_pixels, &scanline[pixels_read]);
		}

		assert(pixels_read == width);
	}
}

namespace cartridge
{
	bool load(const char* filename, std::vector<uint8_t>& rom)
	{
		std::ifstream stream(filename, std::ios::binary | std::ios::ate);
		if (!stream.is_open())
		{
			return false;
		}

		const std::ifstream::pos_type len = stream.tellg();

		rom.resize(static_cast<size_t>(len));

		stream.seekg(0, std::ios::beg);
		stream.read(reinterpret_cast<char*>(&rom[0]), len);

		return true;
	}
}

int main(int argc, char *argv[])
{
	SDL_Init(SDL_INIT_VIDEO);
	SDL_Window* window = SDL_CreateWindow(
		"gameboy", 
		SDL_WINDOWPOS_UNDEFINED, 
		SDL_WINDOWPOS_UNDEFINED,
		display::width,
		display::height, 
		SDL_WINDOW_SHOWN);

	SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, 0);

	SDL_Texture* texture = SDL_CreateTexture(
		renderer,
		SDL_PIXELFORMAT_RGBA8888,
		SDL_TEXTUREACCESS_STREAMING,
		display::width,
		display::height);

	std::vector<uint8_t> memory(0x10000);

	std::vector<uint8_t> rom(0x8000);
	if (!cartridge::load("Tetris-USA.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/cpu_instrs.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/01-special.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/02-interrupts.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/03-op sp,hl.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/04-op r,imm.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/06-ld r,r.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/08-misc instrs.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/09-op r,r.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/10-bit ops.gb", rom))
	//if (!cartridge::load("Tests/cpu_instrs/individual/11-op a,(hl).gb", rom))
	{
		printf("failed to load cartridge.\n");
		return -1;
	}

	char title[16];
	std::memcpy(title, &rom[0x0134], 16);
	printf("title = %s\n", title);

	// The color gameboy is 0x80. Otherwise classic.
	const uint8_t gameboy_type = rom[0x0143];
	printf("gameboy type = 0x%02X\n", gameboy_type);

	// The super gameboy is 0x03. Otherwise 0x00 for classic.
	const uint8_t super_gameboy = rom[0x146];
	printf("super gameboy = 0x%02X\n", super_gameboy);

	// Only support type 0x00 (ROM-ONLY) right now.
	const uint8_t cartridge_type = rom[0x147];
	printf("cartridge type = 0x%02X\n", cartridge_type);

	printf("\n");

	std::memcpy(&memory[0], &rom[0], rom.size());

	cpu::cpu cpu;

	// This would normally be done by the bootloader
	cpu.registers.AF = 0x01B0;
	cpu.registers.BC = 0x0013;
	cpu.registers.DE = 0x00D8;
	cpu.registers.HL = 0x014D;
	cpu.registers.SP = 0xFFFE;
	cpu.registers.PC = 0x0100;

	uint32_t pixels[display::width * display::height];
	memset(pixels, 0, display::width * display::height * sizeof(uint32_t));

	uint32_t scanline_index = 0;

	bool done = false;
	while (!done)
	{
		cpu::tick(cpu, memory);
		uint8_t scanline[display::width];
		display::tick(memory, scanline_index, scanline);

		static const uint32_t palette[4] =
		{
			0xFFFFFFFF,
			0xC0C0C0FF,
			0x606060FF,
			0x000000FF,
		};

		for (uint8_t x = 0; x < display::width; ++x)
		{
			pixels[scanline_index * display::width + x] = palette[scanline[x]];
		}

		if (++scanline_index >= display::height)
		{
			scanline_index = 0;
		}

		SDL_UpdateTexture(texture, nullptr, pixels, display::width * sizeof(uint32_t));

		SDL_Event event;
		while (SDL_PollEvent(&event))
		{
			switch (event.type)
			{
			case SDL_QUIT:
				done = true;
				break;
			}
		}

		SDL_RenderClear(renderer);
		SDL_RenderCopy(renderer, texture, nullptr, nullptr);
		SDL_RenderPresent(renderer);
	}

	SDL_DestroyTexture(texture);
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();

	return 0;
}

// Reference
// http://www.z80.info/decoding.htm
// https://realboyemulator.files.wordpress.com/2013/01/gbcpuman.pdf
// http://bgb.bircd.org/pandocs.htm
// http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-Graphics
// https://mgba.io/2015/06/27/cycle-counting-prefetch/
// https://github.com/geky/gb/blob/master/REPORT.md
// https://github.com/geky/gb/blob/master/ops/ops.txt
// https://github.com/jgilchrist/emulator/blob/7889489d46912a7563845220b2c03300528be7e0/src/cpu/opcode_cycles.h
// http://stackoverflow.com/questions/6953763/gameboy-emulator-testing-strategies

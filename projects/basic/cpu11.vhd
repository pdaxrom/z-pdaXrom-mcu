--===========================================================================--
--
--  S Y N T H E Z I A B L E    cpu11 - HC11 compatible CPU core
--
--  www.OpenCores.Org - September 2003
--  This core adheres to the GNU public license  
--
-- File name      : cpu11.vhd
--
-- Entity name    : cpu11
--
-- Purpose        : HC11 instruction set compatible CPU core
--
-- Dependencies   : ieee.std_logic_1164
--                  ieee.std_logic_unsigned
--
-- Uses           : Nothing
--
-- Author         : John Kent - dilbert57@opencores.org
--
-------------------------------------------------------------------------------
-- Revision list
--
-- Version 0.1 - 13 November 2002 - John Kent
-- revamped 6801 CPU into 68HC11 CPU.
-- Added Y index register
-- Added Y indexing prebyte
-- Added CMPD with prebyte
-- Added bit operators
-- Updated stack operations
--
-- Version 0.3 - 15 December 2002 - John Kent
-- implemented FDIV
-- implemented IDIV
--
-- Version 1.0 - 7 September 2003 - John Kent
-- Released to Open Cores
-- Basic 6800 instructions working
-- but not Divide and bit operations.
--
-- Version 1.1 - 4 April 2004
-- Removed Test_alu and Test_cc signals
-- Moved Dual operand execution into fetch state
-- Fixed Indexed bit operators
--
-- Added by sashz (11 Jul 2017):
-- 13 Jan 2004 1.1                John Kent  
-- As Reported by Michael Hasenfratz CLR did not clear the carry bit.
-- this is because the state sequencer enumerated the ALU with "alu_ld8"
-- rather than "alu_clr". I've also moved the "alu_clr" to the "alu_clc"
-- decode which clears the carry. It should not be necessary, but is a
-- more obvious way of doing things.
--
-- Added by sashz (15 Jul 2017):
-- Fixed mistyped prefix for page4 indexed state
--
-- Added by sashz (22 Jun 2018):
-- Fixed IDIV and MUL
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY cpu11 IS
	PORT (
		clk : IN std_logic;
		rst : IN std_logic;
		rw : OUT std_logic;
		vma : OUT std_logic;
		address : OUT std_logic_vector(15 DOWNTO 0);
		data_in : IN std_logic_vector(7 DOWNTO 0);
		data_out : OUT std_logic_vector(7 DOWNTO 0);
		irq : IN std_logic;
		xirq : IN std_logic;
		irq_ext3 : IN std_logic;
		irq_ext2 : IN std_logic;
		irq_ext1 : IN std_logic;
		irq_ext0 : IN std_logic
	);
END;

ARCHITECTURE CPU_ARCH OF cpu11 IS

	CONSTANT SBIT : INTEGER := 7;
	CONSTANT XBIT : INTEGER := 6;
	CONSTANT HBIT : INTEGER := 5;
	CONSTANT IBIT : INTEGER := 4;
	CONSTANT NBIT : INTEGER := 3;
	CONSTANT ZBIT : INTEGER := 2;
	CONSTANT VBIT : INTEGER := 1;
	CONSTANT CBIT : INTEGER := 0;

	TYPE state_type IS (reset_state, fetch_state, decode_state,
	extended_state, indexed_state, read8_state, read16_state, immediate16_state,
	write8_state, write16_state,
	execute_state, halt_state, spin_state,
	exchange_state,
	mul_state, mulea_state, muld_state, mul0_state,
	idiv_state,
	div1_state, div2_state, div3_state, div4_state, div5_state,
	jmp_state, jsr_state, jsr1_state,
	branch_state, bsr_state, bsr1_state,
	bitmask_state, brset_state, brclr_state,
	rts_hi_state, rts_lo_state,
	int_pcl_state, int_pch_state,
	int_ixl_state, int_ixh_state,
	int_iyl_state, int_iyh_state,
	int_cc_state, int_acca_state, int_accb_state,
	int_wai_state, int_maski_state, int_maskx_state,
	rti_state, rti_cc_state, rti_acca_state, rti_accb_state,
	rti_ixl_state, rti_ixh_state,
	rti_iyl_state, rti_iyh_state,
	rti_pcl_state, rti_pch_state,
	pula_state, psha_state, pulb_state, pshb_state,
	pulxy_lo_state, pulxy_hi_state, pshxy_lo_state, pshxy_hi_state,
	vect_lo_state, vect_hi_state);
	TYPE addr_type IS (idle_ad, fetch_ad, read_ad, write_ad, push_ad, pull_ad, int_hi_ad, int_lo_ad);
	TYPE dout_type IS (acca_dout, accb_dout, cc_dout,
	ix_lo_dout, ix_hi_dout, iy_lo_dout, iy_hi_dout,
	md_lo_dout, md_hi_dout, pc_lo_dout, pc_hi_dout);
	TYPE op_type IS (reset_op, fetch_op, latch_op);
	TYPE pre_type IS (reset_pre, fetch_pre, latch_pre);
	TYPE acca_type IS (reset_acca, load_acca, load_hi_acca, pull_acca, latch_acca);
	TYPE accb_type IS (reset_accb, load_accb, pull_accb, latch_accb);
	TYPE cc_type IS (reset_cc, load_cc, pull_cc, latch_cc);
	TYPE ix_type IS (reset_ix, load_ix, pull_lo_ix, pull_hi_ix, latch_ix, load_ix_idiv);
	TYPE iy_type IS (reset_iy, load_iy, pull_lo_iy, pull_hi_iy, latch_iy);
	TYPE sp_type IS (reset_sp, latch_sp, load_sp);
	TYPE pc_type IS (reset_pc, latch_pc, load_pc, pull_lo_pc, pull_hi_pc, incr_pc);
	TYPE md_type IS (reset_md, latch_md, load_md, fetch_first_md, fetch_next_md, shiftl_md);
	TYPE ea_type IS (reset_ea, latch_ea, load_ea, fetch_first_ea, fetch_next_ea, add_ix_ea, add_iy_ea);
	TYPE iv_type IS (reset_iv, latch_iv, swi_iv, xirq_iv, irq_iv, ext3_iv, ext2_iv, ext1_iv, ext0_iv);
	TYPE count_type IS (reset_count, latch_count, inc_count);
	TYPE left_type IS (acca_left, accb_left, accd_left, md_left, ix_left, iy_left, pc_left, sp_left, ea_left);
	TYPE right_type IS (md_right, zero_right, one_right, accb_right, pre_right, ea_right, sexea_right);
	TYPE alu_type IS (alu_add8, alu_sub8, alu_add16, alu_sub16, alu_adc, alu_sbc,
	alu_and, alu_ora, alu_eor,
	alu_tst, alu_inc, alu_dec, alu_clr, alu_neg, alu_com,
	alu_inc16, alu_dec16,
	alu_lsr16, alu_lsl16,
	alu_ror8, alu_rol8, alu_rol16,
	alu_asr8, alu_asl8, alu_lsr8,
	alu_sei, alu_cli, alu_sec, alu_clc, alu_sev, alu_clv,
	alu_sex, alu_clx, alu_tpa, alu_tap,
	alu_ld8, alu_st8, alu_ld16, alu_st16, alu_nop, alu_daa,
	alu_bset, alu_bclr, alu_mindiv, alu_div, alu_mul);

	SIGNAL op_code : std_logic_vector(7 DOWNTO 0);
	SIGNAL pre_byte : std_logic_vector(7 DOWNTO 0);
	SIGNAL acca : std_logic_vector(7 DOWNTO 0);
	SIGNAL accb : std_logic_vector(7 DOWNTO 0);
	SIGNAL cc : std_logic_vector(7 DOWNTO 0);
	SIGNAL cc_out : std_logic_vector(7 DOWNTO 0);
	SIGNAL xreg : std_logic_vector(15 DOWNTO 0);
	SIGNAL yreg : std_logic_vector(15 DOWNTO 0);
	SIGNAL sp : std_logic_vector(15 DOWNTO 0);
	SIGNAL ea : std_logic_vector(15 DOWNTO 0);
	SIGNAL pc : std_logic_vector(15 DOWNTO 0);
	SIGNAL md : std_logic_vector(15 DOWNTO 0);
	SIGNAL left : std_logic_vector(15 DOWNTO 0);
	SIGNAL right : std_logic_vector(15 DOWNTO 0);
	SIGNAL out_alu : std_logic_vector(15 DOWNTO 0);
	SIGNAL iv : std_logic_vector(2 DOWNTO 0);
	SIGNAL ea_bit : std_logic;
	SIGNAL count : std_logic_vector(4 DOWNTO 0);

	SIGNAL state : state_type;
	SIGNAL next_state : state_type;
	SIGNAL pc_ctrl : pc_type;
	SIGNAL ea_ctrl : ea_type;
	SIGNAL op_ctrl : op_type;
	SIGNAL pre_ctrl : pre_type;
	SIGNAL md_ctrl : md_type;
	SIGNAL acca_ctrl : acca_type;
	SIGNAL accb_ctrl : accb_type;
	SIGNAL ix_ctrl : ix_type;
	SIGNAL iy_ctrl : iy_type;
	SIGNAL cc_ctrl : cc_type;
	SIGNAL sp_ctrl : sp_type;
	SIGNAL iv_ctrl : iv_type;
	SIGNAL left_ctrl : left_type;
	SIGNAL right_ctrl : right_type;
	SIGNAL alu_ctrl : alu_type;
	SIGNAL addr_ctrl : addr_type;
	SIGNAL dout_ctrl : dout_type;
	SIGNAL count_ctrl : count_type;
BEGIN

	--------------------------------
	--
	-- Accumulator A
	--
	--------------------------------
	acca_reg : PROCESS (clk, acca_ctrl, out_alu, acca, data_in)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE acca_ctrl IS
				WHEN reset_acca =>
					acca <= "00000000";
				WHEN load_acca =>
					acca <= out_alu(7 DOWNTO 0);
				WHEN load_hi_acca =>
					acca <= out_alu(15 DOWNTO 8);
				WHEN pull_acca =>
					acca <= data_in;
				WHEN OTHERS =>
					--	 when latch_acca =>
					acca <= acca;
			END CASE;
		END IF;
	END PROCESS;

	--------------------------------
	--
	-- Accumulator B
	--
	--------------------------------
	accb_reg : PROCESS (clk, accb_ctrl, out_alu, accb, data_in)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE accb_ctrl IS
				WHEN reset_accb =>
					accb <= "00000000";
				WHEN load_accb =>
					accb <= out_alu(7 DOWNTO 0);
				WHEN pull_accb =>
					accb <= data_in;
				WHEN OTHERS =>
					--	 when latch_accb =>
					accb <= accb;
			END CASE;
		END IF;
	END PROCESS;

	----------------------------------
	--
	-- Condition Codes
	--
	----------------------------------

	cc_reg : PROCESS (clk, cc_ctrl, cc_out, cc, data_in)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE cc_ctrl IS
				WHEN reset_cc =>
					cc <= "11000000";
				WHEN load_cc =>
					cc <= cc_out;
				WHEN pull_cc =>
					cc <= data_in;
				WHEN OTHERS =>
					--  when latch_cc =>
					cc <= cc;
			END CASE;
		END IF;
	END PROCESS;

	--------------------------------
	--
	-- X Index register
	--
	--------------------------------
	ix_reg : PROCESS (clk, ix_ctrl, out_alu, xreg, data_in)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE ix_ctrl IS
				WHEN reset_ix =>
					xreg <= "0000000000000000";
				WHEN load_ix =>
					xreg <= out_alu(15 DOWNTO 0);
				WHEN pull_hi_ix =>
					xreg(15 DOWNTO 8) <= data_in;
				WHEN pull_lo_ix =>
					xreg(7 DOWNTO 0) <= data_in;
				WHEN load_ix_idiv =>
					xreg <= NOT xreg;
				WHEN OTHERS =>
					--	 when latch_ix =>
					xreg <= xreg;
			END CASE;
		END IF;
	END PROCESS;

	--------------------------------
	--
	-- Y Index register
	--
	--------------------------------
	iy_reg : PROCESS (clk, iy_ctrl, out_alu, yreg, data_in)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE iy_ctrl IS
				WHEN reset_iy =>
					yreg <= "0000000000000000";
				WHEN load_iy =>
					yreg <= out_alu(15 DOWNTO 0);
				WHEN pull_hi_iy =>
					yreg(15 DOWNTO 8) <= data_in;
				WHEN pull_lo_iy =>
					yreg(7 DOWNTO 0) <= data_in;
				WHEN OTHERS =>
					--	 when latch_iy =>
					yreg <= yreg;
			END CASE;
		END IF;
	END PROCESS;

	--------------------------------
	--
	-- stack pointer
	--
	--------------------------------
	sp_reg : PROCESS (clk, sp_ctrl, out_alu)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE sp_ctrl IS
				WHEN reset_sp =>
					sp <= "0000000000000000";
				WHEN load_sp =>
					sp <= out_alu(15 DOWNTO 0);
				WHEN OTHERS =>
					--	 when latch_sp =>
					sp <= sp;
			END CASE;
		END IF;
	END PROCESS;

	----------------------------------
	--
	-- Program Counter Control
	--
	----------------------------------

	pc_reg : PROCESS (clk, pc_ctrl, pc, out_alu, data_in)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE pc_ctrl IS
				WHEN reset_pc =>
					pc <= "0000000000000000";
				WHEN incr_pc =>
					pc <= pc + "0000000000000001";
				WHEN load_pc =>
					pc <= out_alu(15 DOWNTO 0);
				WHEN pull_lo_pc =>
					pc(7 DOWNTO 0) <= data_in;
				WHEN pull_hi_pc =>
					pc(15 DOWNTO 8) <= data_in;
				WHEN OTHERS =>
					--	 when latch_pc =>
					pc <= pc;
			END CASE;
		END IF;
	END PROCESS;

	----------------------------------
	--
	-- Effective Address  Control
	--
	----------------------------------

	ea_reg : PROCESS (clk, ea_ctrl, ea, out_alu, data_in, xreg, yreg)
	BEGIN

		IF clk'event AND clk = '0' THEN
			CASE ea_ctrl IS
				WHEN reset_ea =>
					ea <= "0000000000000000";
				WHEN fetch_first_ea =>
					ea(7 DOWNTO 0) <= data_in;
					ea(15 DOWNTO 8) <= "00000000";
				WHEN fetch_next_ea =>
					ea(15 DOWNTO 8) <= ea(7 DOWNTO 0);
					ea(7 DOWNTO 0) <= data_in;
				WHEN add_ix_ea =>
					ea <= ea + xreg;
				WHEN add_iy_ea =>
					ea <= ea + yreg;
				WHEN load_ea =>
					ea <= out_alu(15 DOWNTO 0);
				WHEN OTHERS =>
					--  	 when latch_ea =>
					ea <= ea;
			END CASE;
		END IF;
	END PROCESS;

	--------------------------------
	--
	-- Memory Data
	--
	--------------------------------
	md_reg : PROCESS (clk, md_ctrl, out_alu, data_in, md)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE md_ctrl IS
				WHEN reset_md =>
					md <= "0000000000000000";
				WHEN load_md =>
					md <= out_alu(15 DOWNTO 0);
				WHEN fetch_first_md =>
					md(15 DOWNTO 8) <= "00000000";
					md(7 DOWNTO 0) <= data_in;
				WHEN fetch_next_md =>
					md(15 DOWNTO 8) <= md(7 DOWNTO 0);
					md(7 DOWNTO 0) <= data_in;
				WHEN shiftl_md =>
					md(15 DOWNTO 1) <= md(14 DOWNTO 0);
					md(0) <= '0';
				WHEN OTHERS =>
					--	 when latch_md =>
					md <= md;
			END CASE;
		END IF;
	END PROCESS;

	----------------------------------
	--
	-- interrupt vector
	--
	----------------------------------

	iv_reg : PROCESS (clk, iv_ctrl)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE iv_ctrl IS
				WHEN reset_iv =>
					iv <= "111";
				WHEN xirq_iv =>
					iv <= "110";
				WHEN swi_iv =>
					iv <= "101";
				WHEN irq_iv =>
					iv <= "100";
				WHEN ext3_iv =>
					iv <= "011";
				WHEN ext2_iv =>
					iv <= "010";
				WHEN ext1_iv =>
					iv <= "001";
				WHEN ext0_iv =>
					iv <= "000";
				WHEN OTHERS =>
					iv <= iv;
			END CASE;
		END IF;
	END PROCESS;

	----------------------------------
	--
	-- op code register
	--
	----------------------------------

	op_reg : PROCESS (clk, data_in, op_ctrl, op_code)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE op_ctrl IS
				WHEN reset_op =>
					op_code <= "00000001"; -- nop
				WHEN fetch_op =>
					op_code <= data_in;
				WHEN OTHERS =>
					--	 when latch_op =>
					op_code <= op_code;
			END CASE;
		END IF;
	END PROCESS;

	----------------------------------
	--
	-- pre byte register
	--
	----------------------------------

	pre_reg : PROCESS (clk, pre_ctrl, data_in, pre_byte)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE pre_ctrl IS
				WHEN reset_pre =>
					pre_byte <= "00000000";
				WHEN fetch_pre =>
					pre_byte <= data_in;
				WHEN OTHERS =>
					--	 when latch_op =>
					pre_byte <= pre_byte;
			END CASE;
		END IF;
	END PROCESS;

	----------------------------------
	--
	-- counter
	--
	----------------------------------

	count_reg : PROCESS (clk, count_ctrl, count)
	BEGIN
		IF clk'event AND clk = '0' THEN
			CASE count_ctrl IS
				WHEN reset_count =>
					count <= "00000";
				WHEN inc_count =>
					count <= count + "00001";
				WHEN OTHERS =>
					--	 when latch_count =>
					count <= count;
			END CASE;
		END IF;
	END PROCESS;

	----------------------------------
	--
	-- Address output multiplexer
	--
	----------------------------------

	addr_mux : PROCESS (clk, addr_ctrl, pc, ea, sp, iv)
	BEGIN
		CASE addr_ctrl IS
			WHEN idle_ad =>
				address <= "1111111111111111";
				vma <= '0';
				rw <= '1';
			WHEN fetch_ad =>
				address <= pc;
				vma <= '1';
				rw <= '1';
			WHEN read_ad =>
				address <= ea;
				vma <= '1';
				rw <= '1';
			WHEN write_ad =>
				address <= ea;
				vma <= '1';
				rw <= '0';
			WHEN push_ad =>
				address <= sp;
				vma <= '1';
				rw <= '0';
			WHEN pull_ad =>
				address <= sp;
				vma <= '1';
				rw <= '1';
			WHEN int_hi_ad =>
				address <= "111111111111" & iv & "0";
				vma <= '1';
				rw <= '1';
			WHEN int_lo_ad =>
				address <= "111111111111" & iv & "1";
				vma <= '1';
				rw <= '1';
			WHEN OTHERS =>
				address <= "1111111111111111";
				vma <= '0';
				rw <= '1';
		END CASE;
	END PROCESS;

	--------------------------------
	--
	-- Data Bus output
	--
	--------------------------------
	dout_mux : PROCESS (clk, dout_ctrl, md, acca, accb, xreg, yreg, pc, cc)
	BEGIN
		CASE dout_ctrl IS
			WHEN acca_dout => -- accumulator a
				data_out <= acca;
			WHEN accb_dout => -- accumulator b
				data_out <= accb;
			WHEN cc_dout => -- condition codes
				data_out <= cc;
			WHEN ix_lo_dout => -- X index reg
				data_out <= xreg(7 DOWNTO 0);
			WHEN ix_hi_dout => -- X index reg
				data_out <= xreg(15 DOWNTO 8);
			WHEN iy_lo_dout => -- Y index reg
				data_out <= yreg(7 DOWNTO 0);
			WHEN iy_hi_dout => -- Y index reg
				data_out <= yreg(15 DOWNTO 8);
			WHEN md_lo_dout => -- memory data (ALU)
				data_out <= md(7 DOWNTO 0);
			WHEN md_hi_dout => -- memory data (ALU)
				data_out <= md(15 DOWNTO 8);
			WHEN pc_lo_dout => -- low order pc
				data_out <= pc(7 DOWNTO 0);
			WHEN pc_hi_dout => -- high order pc
				data_out <= pc(15 DOWNTO 8);
			WHEN OTHERS =>
				data_out <= "00000000";
		END CASE;
	END PROCESS;

	----------------------------------
	--
	-- ea bit mutiplexer (used by multiply)
	--
	----------------------------------

	ea_bit_mux : PROCESS (count, ea)
	BEGIN
		CASE count(3 DOWNTO 0) IS
			WHEN "0000" =>
				ea_bit <= ea(0);
			WHEN "0001" =>
				ea_bit <= ea(1);
			WHEN "0010" =>
				ea_bit <= ea(2);
			WHEN "0011" =>
				ea_bit <= ea(3);
			WHEN "0100" =>
				ea_bit <= ea(4);
			WHEN "0101" =>
				ea_bit <= ea(5);
			WHEN "0110" =>
				ea_bit <= ea(6);
			WHEN "0111" =>
				ea_bit <= ea(7);
			WHEN "1000" =>
				ea_bit <= ea(8);
			WHEN "1001" =>
				ea_bit <= ea(9);
			WHEN "1010" =>
				ea_bit <= ea(10);
			WHEN "1011" =>
				ea_bit <= ea(11);
			WHEN "1100" =>
				ea_bit <= ea(12);
			WHEN "1101" =>
				ea_bit <= ea(13);
			WHEN "1110" =>
				ea_bit <= ea(14);
			WHEN "1111" =>
				ea_bit <= ea(15);
			WHEN OTHERS =>
				NULL;
		END CASE;
	END PROCESS;

	----------------------------------
	--
	-- Left Mux
	--
	----------------------------------

	left_mux : PROCESS (left_ctrl, acca, accb, xreg, yreg, sp, pc, ea, md)
	BEGIN
		CASE left_ctrl IS
			WHEN acca_left =>
				left(15 DOWNTO 8) <= "00000000";
				left(7 DOWNTO 0) <= acca;
			WHEN accb_left =>
				left(15 DOWNTO 8) <= "00000000";
				left(7 DOWNTO 0) <= accb;
			WHEN accd_left =>
				left(15 DOWNTO 8) <= acca;
				left(7 DOWNTO 0) <= accb;
			WHEN md_left =>
				left <= md;
			WHEN ix_left =>
				left <= xreg;
			WHEN iy_left =>
				left <= yreg;
			WHEN sp_left =>
				left <= sp;
			WHEN pc_left =>
				left <= pc;
			WHEN OTHERS =>
				--	 when ea_left =>
				left <= ea;
		END CASE;
	END PROCESS;

	----------------------------------
	--
	-- Right Mux
	--
	----------------------------------

	right_mux : PROCESS (right_ctrl, data_in, md, accb, pre_byte, ea)
	BEGIN
		CASE right_ctrl IS
			WHEN zero_right =>
				right <= "0000000000000000";
			WHEN one_right =>
				right <= "0000000000000001";
			WHEN accb_right =>
				right <= "00000000" & accb; -- for abx / aby instructions
			WHEN pre_right =>
				right <= "00000000" & pre_byte; -- prebyte register doubles as bit mask
			WHEN ea_right =>
				right <= ea;
			WHEN sexea_right =>
				IF ea(7) = '0' THEN
					right <= "00000000" & ea(7 DOWNTO 0);
				ELSE
					right <= "11111111" & ea(7 DOWNTO 0);
				END IF;
			WHEN OTHERS =>
				--	 when md_right =>
				right <= md;
		END CASE;
	END PROCESS;

	----------------------------------
	--
	-- Arithmetic Logic Unit
	--
	----------------------------------

	alu_logic : PROCESS (alu_ctrl, cc, left, right, out_alu, cc_out)
		VARIABLE valid_lo, valid_hi : BOOLEAN;
		VARIABLE carry_in : std_logic;
		VARIABLE daa_reg : std_logic_vector(7 DOWNTO 0);
	BEGIN

		CASE alu_ctrl IS
			WHEN alu_adc | alu_sbc |
				alu_rol8 | alu_ror8 | alu_rol16 =>
				carry_in := cc(CBIT);
			WHEN OTHERS =>
				carry_in := '0';
		END CASE;

		valid_lo := left(3 DOWNTO 0) <= 9;
		valid_hi := left(7 DOWNTO 4) <= 9;

		IF (cc(CBIT) = '0') THEN
			IF (cc(HBIT) = '1') THEN
				IF valid_hi THEN
					daa_reg := "00000110";
				ELSE
					daa_reg := "01100110";
				END IF;
			ELSE
				IF valid_lo THEN
					IF valid_hi THEN
						daa_reg := "00000000";
					ELSE
						daa_reg := "01100000";
					END IF;
				ELSE
					IF (left(7 DOWNTO 4) <= 8) THEN
						daa_reg := "00000110";
					ELSE
						daa_reg := "01100110";
					END IF;
				END IF;
			END IF;
		ELSE
			IF (cc(HBIT) = '1') THEN
				daa_reg := "01100110";
			ELSE
				IF valid_lo THEN
					daa_reg := "01100000";
				ELSE
					daa_reg := "01100110";
				END IF;
			END IF;
		END IF;

		CASE alu_ctrl IS
			WHEN alu_add8 | alu_adc | alu_inc |
				alu_add16 | alu_inc16 | alu_mul =>
				out_alu <= left + right + ("000000000000000" & carry_in);
			WHEN alu_sub8 | alu_sbc | alu_dec |
				alu_sub16 | alu_dec16 | alu_div =>
				out_alu <= left - right - ("000000000000000" & carry_in);
			WHEN alu_and =>
				out_alu <= left AND right; -- and/bit
			WHEN alu_bclr =>
				out_alu <= left AND (NOT right); -- bclr
			WHEN alu_ora | alu_bset =>
				out_alu <= left OR right; -- or
			WHEN alu_eor =>
				out_alu <= left XOR right; -- eor/xor
			WHEN alu_lsl16 | alu_asl8 | alu_rol8 | alu_rol16 =>
				out_alu <= left(14 DOWNTO 0) & carry_in; -- rol8/rol16/asl8/lsl16
			WHEN alu_lsr16 | alu_lsr8 =>
				out_alu <= carry_in & left(15 DOWNTO 1); -- lsr
			WHEN alu_ror8 =>
				out_alu <= "00000000" & carry_in & left(7 DOWNTO 1); -- ror
			WHEN alu_asr8 =>
				out_alu <= "00000000" & left(7) & left(7 DOWNTO 1); -- asr
			WHEN alu_neg =>
				out_alu <= right - left; -- neg (right=0)
			WHEN alu_com =>
				out_alu <= NOT left;
			WHEN alu_clr | alu_ld8 | alu_ld16 =>
				out_alu <= right; -- clr, ld
			WHEN alu_st8 | alu_st16 =>
				out_alu <= left;
			WHEN alu_daa =>
				out_alu <= left + ("00000000" & daa_reg);
			WHEN alu_tpa =>
				out_alu <= "00000000" & cc;
			WHEN alu_mindiv =>
				out_alu <= left;
			WHEN OTHERS =>
				out_alu <= left; -- nop
		END CASE;

		--
		-- carry bit
		--
		CASE alu_ctrl IS
			WHEN alu_add8 | alu_adc =>
				cc_out(CBIT) <= (left(7) AND right(7)) OR
				(left(7) AND NOT out_alu(7)) OR
				(right(7) AND NOT out_alu(7));
			WHEN alu_sub8 | alu_sbc =>
				cc_out(CBIT) <= ((NOT left(7)) AND right(7)) OR
				((NOT left(7)) AND out_alu(7)) OR
				(right(7) AND out_alu(7));
			WHEN alu_add16 =>
				cc_out(CBIT) <= (left(15) AND right(15)) OR
				(left(15) AND NOT out_alu(15)) OR
				(right(15) AND NOT out_alu(15));
			WHEN alu_sub16 | alu_div =>
				cc_out(CBIT) <= ((NOT left(15)) AND right(15)) OR
				((NOT left(15)) AND out_alu(15)) OR
				(right(15) AND out_alu(15));
			WHEN alu_ror8 | alu_lsr16 | alu_lsr8 | alu_asr8 =>
				cc_out(CBIT) <= left(0);
			WHEN alu_rol8 | alu_asl8 =>
				cc_out(CBIT) <= left(7);
			WHEN alu_lsl16 | alu_rol16 =>
				cc_out(CBIT) <= left(15);
			WHEN alu_com =>
				cc_out(CBIT) <= '1';
			WHEN alu_neg | alu_clr =>
				cc_out(CBIT) <= out_alu(7) OR out_alu(6) OR out_alu(5) OR out_alu(4) OR
				out_alu(3) OR out_alu(2) OR out_alu(1) OR out_alu(0);
			WHEN alu_mul =>
				cc_out(CBIT) <= out_alu(7);
			WHEN alu_daa =>
				IF (daa_reg(7 DOWNTO 4) = "0110") THEN
					cc_out(CBIT) <= '1';
				ELSE
					cc_out(CBIT) <= '0';
				END IF;
			WHEN alu_sec =>
				cc_out(CBIT) <= '1';
			WHEN alu_clc =>
				cc_out(CBIT) <= '0';
			WHEN alu_tap =>
				cc_out(CBIT) <= left(CBIT);
			WHEN alu_mindiv =>
				cc_out(CBIT) <= NOT (ea(15) OR ea(14) OR ea (13) OR ea(12) OR
									 ea(11) OR ea(10) OR ea(9)   OR ea(8)  OR
									 ea(7)  OR ea(6)  OR ea(5)   OR ea(4)  OR
									 ea(3)  OR ea(2)  OR ea(1)   OR ea(0));

			WHEN OTHERS => -- carry is not affected by cpx
				cc_out(CBIT) <= cc(CBIT);
		END CASE;
		--
		-- Zero flag
		--
		CASE alu_ctrl IS
			WHEN alu_add8 | alu_sub8 |
				alu_adc | alu_sbc |
				alu_and | alu_ora | alu_eor |
				alu_inc | alu_dec |
				alu_neg | alu_com | alu_clr |
				alu_rol8 | alu_ror8 | alu_asr8 | alu_asl8 | alu_lsr8 |
				alu_ld8 | alu_st8 |
				alu_bset | alu_bclr =>
				cc_out(ZBIT) <= NOT(out_alu(7) OR out_alu(6) OR out_alu(5) OR out_alu(4) OR
				out_alu(3) OR out_alu(2) OR out_alu(1) OR out_alu(0));
			WHEN alu_add16 | alu_sub16 |
				alu_lsl16 | alu_lsr16 |
				alu_inc16 | alu_dec16 |
				alu_ld16 | alu_st16  |
				alu_div =>
				cc_out(ZBIT) <= NOT(out_alu(15) OR out_alu(14) OR out_alu(13) OR out_alu(12) OR
				out_alu(11) OR out_alu(10) OR out_alu(9) OR out_alu(8) OR
				out_alu(7) OR out_alu(6) OR out_alu(5) OR out_alu(4) OR
				out_alu(3) OR out_alu(2) OR out_alu(1) OR out_alu(0));
			WHEN alu_tap =>
				cc_out(ZBIT) <= left(ZBIT);
			WHEN alu_mindiv =>
				cc_out(ZBIT) <= xreg(15) OR xreg(14) OR xreg(13) OR xreg(12) OR
					            xreg(11) OR xreg(10) OR xreg(9)  OR xreg(8)  OR
					            xreg(7)  OR xreg(6)  OR xreg(5)  OR xreg(4)  OR
					            xreg(3)  OR xreg(2)  OR xreg(1)  OR xreg(0);

			WHEN OTHERS =>
				cc_out(ZBIT) <= cc(ZBIT);
		END CASE;

		--
		-- negative flag
		--
		CASE alu_ctrl IS
			WHEN alu_add8 | alu_sub8 |
				alu_adc | alu_sbc |
				alu_and | alu_ora | alu_eor |
				alu_rol8 | alu_ror8 | alu_asr8 | alu_asl8 | alu_lsr8 |
				alu_inc | alu_dec | alu_neg | alu_com | alu_clr |
				alu_ld8 | alu_st8 |
				alu_bset | alu_bclr =>
				cc_out(NBIT) <= out_alu(7);
			WHEN alu_add16 | alu_sub16 |
				alu_lsl16 | alu_lsr16 |
				alu_ld16 | alu_st16 =>
				cc_out(NBIT) <= out_alu(15);
			WHEN alu_tap =>
				cc_out(NBIT) <= left(NBIT);
			WHEN OTHERS =>
				cc_out(NBIT) <= cc(NBIT);
		END CASE;

		--
		-- Interrupt mask flag
		--
		CASE alu_ctrl IS
			WHEN alu_sei =>
				cc_out(IBIT) <= '1'; -- set interrupt mask
			WHEN alu_cli =>
				cc_out(IBIT) <= '0'; -- clear interrupt mask
			WHEN alu_tap =>
				cc_out(IBIT) <= left(IBIT);
			WHEN OTHERS =>
				cc_out(IBIT) <= cc(IBIT); -- interrupt mask
		END CASE;

		--
		-- Half Carry flag
		--
		CASE alu_ctrl IS
			WHEN alu_add8 | alu_adc =>
				cc_out(HBIT) <= (left(3) AND right(3)) OR
				(right(3) AND NOT out_alu(3)) OR
				(left(3) AND NOT out_alu(3));
			WHEN alu_tap =>
				cc_out(HBIT) <= left(HBIT);
			WHEN OTHERS =>
				cc_out(HBIT) <= cc(HBIT);
		END CASE;

		--
		-- Overflow flag
		--
		CASE alu_ctrl IS
			WHEN alu_add8 | alu_adc =>
				cc_out(VBIT) <= (left(7) AND right(7) AND (NOT out_alu(7))) OR
				((NOT left(7)) AND (NOT right(7)) AND out_alu(7));
			WHEN alu_sub8 | alu_sbc =>
				cc_out(VBIT) <= (left(7) AND (NOT right(7)) AND (NOT out_alu(7))) OR
				((NOT left(7)) AND right(7) AND out_alu(7));
			WHEN alu_add16 =>
				cc_out(VBIT) <= (left(15) AND right(15) AND (NOT out_alu(15))) OR
				((NOT left(15)) AND (NOT right(15)) AND out_alu(15));
			WHEN alu_sub16 =>
				cc_out(VBIT) <= (left(15) AND (NOT right(15)) AND (NOT out_alu(15))) OR
				((NOT left(15)) AND right(15) AND out_alu(15));
			WHEN alu_inc =>
				cc_out(VBIT) <= ((NOT left(7)) AND left(6) AND left(5) AND left(4) AND
				left(3) AND left(2) AND left(1) AND left(0));
			WHEN alu_dec | alu_neg =>
				cc_out(VBIT) <= (left(7) AND (NOT left(6)) AND (NOT left(5)) AND (NOT left(4)) AND
				(NOT left(3)) AND (NOT left(2)) AND (NOT left(1)) AND (NOT left(0)));
			WHEN alu_asr8 =>
				cc_out(VBIT) <= left(0) XOR left(7);
			WHEN alu_lsr8 | alu_lsr16 =>
				cc_out(VBIT) <= left(0);
			WHEN alu_ror8 =>
				cc_out(VBIT) <= left(0) XOR cc(CBIT);
			WHEN alu_lsl16 =>
				cc_out(VBIT) <= left(15) XOR left(14);
			WHEN alu_rol8 | alu_asl8 =>
				cc_out(VBIT) <= left(7) XOR left(6);
			WHEN alu_tap =>
				cc_out(VBIT) <= left(VBIT);
			WHEN alu_and | alu_ora | alu_eor | alu_com |
				alu_st8 | alu_st16 | alu_ld8 | alu_ld16 |
				alu_bset | alu_bclr |
				alu_clv =>
				cc_out(VBIT) <= '0';
			WHEN alu_sev =>
				cc_out(VBIT) <= '1';
			WHEN alu_mindiv =>
				cc_out(VBIT) <= '0';
			WHEN OTHERS =>
				cc_out(VBIT) <= cc(VBIT);
		END CASE;

		CASE alu_ctrl IS
			WHEN alu_sex =>
				cc_out(XBIT) <= '1'; -- set interrupt mask
			WHEN alu_clx =>
				cc_out(XBIT) <= '0'; -- clear interrupt mask
			WHEN alu_tap =>
				cc_out(XBIT) <= cc(XBIT) AND left(XBIT);
			WHEN OTHERS =>
				cc_out(XBIT) <= cc(XBIT) AND left(XBIT);
		END CASE;

		CASE alu_ctrl IS
			WHEN alu_tap =>
				cc_out(SBIT) <= left(SBIT);
			WHEN OTHERS =>
				cc_out(SBIT) <= cc(SBIT);
		END CASE;
	END PROCESS;
	------------------------------------
	--
	-- state sequencer
	--
	------------------------------------
	state_logic : PROCESS (state, op_code, pre_byte, cc, ea, md, irq, xirq,
		irq_ext3, irq_ext2, irq_ext1, irq_ext0, ea_bit, count)
	BEGIN
		CASE state IS
			WHEN reset_state => --  released from reset
				-- reset the registers
				op_ctrl <= reset_op;
				pre_ctrl <= reset_pre;
				acca_ctrl <= reset_acca;
				accb_ctrl <= reset_accb;
				ix_ctrl <= reset_ix;
				iy_ctrl <= reset_iy;
				sp_ctrl <= reset_sp;
				pc_ctrl <= reset_pc;
				ea_ctrl <= reset_ea;
				md_ctrl <= reset_md;
				iv_ctrl <= reset_iv;
				sp_ctrl <= reset_sp;
				count_ctrl <= reset_count;
				-- idle the ALU
				left_ctrl <= pc_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= reset_cc;
				-- idle the bus
				dout_ctrl <= md_lo_dout;
				addr_ctrl <= idle_ad;
				next_state <= vect_hi_state;

				--
				-- Jump via interrupt vector
				-- iv holds interrupt type
				-- fetch PC hi from vector location
				--
			WHEN vect_hi_state =>
				-- default the registers
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				md_ctrl <= latch_md;
				ea_ctrl <= latch_ea;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				-- idle the ALU
				left_ctrl <= pc_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				-- fetch pc low interrupt vector
				pc_ctrl <= pull_hi_pc;
				addr_ctrl <= int_hi_ad;
				dout_ctrl <= pc_hi_dout;
				next_state <= vect_lo_state;
				--
				-- jump via interrupt vector
				-- iv holds vector type
				-- fetch PC lo from vector location
				--
			WHEN vect_lo_state =>
				-- default the registers
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				md_ctrl <= latch_md;
				ea_ctrl <= latch_ea;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				-- idle the ALU
				left_ctrl <= pc_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				-- fetch the vector low byte
				pc_ctrl <= pull_lo_pc;
				addr_ctrl <= int_lo_ad;
				dout_ctrl <= pc_lo_dout;
				next_state <= fetch_state;

				--
				-- Here to fetch an instruction
				-- PC points to opcode
				-- Should service interrupt requests at this point
				-- either from the timer
				-- or from the external input.
				--
			WHEN fetch_state =>
				CASE op_code(7 DOWNTO 4) IS
					WHEN "0000" | -- inherent operators
						"0001" | -- bit operators come here				        
						"0010" | -- branch conditional
						"0011" | -- stack operators
						"0100" | -- acca single operand
						"0101" | -- accb single operand
						"0110" | -- indexed single op
						"0111" => -- extended single op
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- idle ALU
						left_ctrl <= acca_left;
						right_ctrl <= zero_right;
						alu_ctrl <= alu_nop;
						cc_ctrl <= latch_cc;
					WHEN "1000" | -- acca immediate
						"1001" | -- acca direct
						"1010" | -- acca indexed
						"1011" => -- acca extended
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0000" => -- suba
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_sub8;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0001" => -- cmpa
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_sub8;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0010" => -- sbca
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_sbc;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0011" => -- subd / cmpd
								left_ctrl <= accd_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_sub16;
								cc_ctrl <= load_cc;
								IF (pre_byte = "00011010") OR (pre_byte = "11001101") THEN
									-- CPD
									acca_ctrl <= latch_acca;
									accb_ctrl <= latch_accb;
								ELSE
									-- SUBD
									acca_ctrl <= load_hi_acca;
									accb_ctrl <= load_accb;
								END IF;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0100" => -- anda
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_and;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0101" => -- bita
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_and;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0110" => -- ldaa
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_ld8;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0111" => -- staa
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1000" => -- eora
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_eor;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1001" => -- adca
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_adc;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1010" => -- oraa
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_ora;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1011" => -- adda
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_add8;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1100" => -- cpx / cpy
								IF (pre_byte = "00011000") OR (pre_byte = "00011010") THEN
									-- cpy
									left_ctrl <= iy_left;
								ELSE
									-- cpx
									left_ctrl <= ix_left;
								END IF;
								right_ctrl <= md_right;
								alu_ctrl <= alu_sub16;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1101" => -- bsr / jsr
								left_ctrl <= pc_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1110" => -- lds
								left_ctrl <= sp_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_ld16;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= load_sp;
							WHEN "1111" => -- sts / xgdx / xgdy
								IF op_code(7 DOWNTO 4) = "1000" THEN
									--
									-- exchange registers
									-- at this point md holds accd
									-- accd holds either X or Y
									-- now transfer md to X or Y
									--
									left_ctrl <= md_left;
									right_ctrl <= zero_right;
									alu_ctrl <= alu_st16;
									cc_ctrl <= latch_cc;
									acca_ctrl <= latch_acca;
									accb_ctrl <= latch_accb;
									sp_ctrl <= latch_sp;
									IF pre_byte = "00011000" THEN
										ix_ctrl <= latch_ix;
										iy_ctrl <= load_iy;
									ELSE
										ix_ctrl <= load_ix;
										iy_ctrl <= latch_iy;
									END IF;
								ELSE
									-- sts
									left_ctrl <= sp_left;
									right_ctrl <= md_right;
									alu_ctrl <= alu_st16;
									cc_ctrl <= load_cc;
									acca_ctrl <= latch_acca;
									accb_ctrl <= latch_accb;
									ix_ctrl <= latch_ix;
									iy_ctrl <= latch_iy;
									sp_ctrl <= latch_sp;
								END IF;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
						END CASE;
					WHEN "1100" | -- accb immediate
						"1101" | -- accb direct
						"1110" | -- accb indexed
						"1111" => -- accb extended
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0000" => -- subb
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_sub8;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0001" => -- cmpb
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_sub8;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0010" => -- sbcb
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_sbc;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0011" => -- addd
								left_ctrl <= accd_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_hi_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0100" => -- andb
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_and;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0101" => -- bitb
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_and;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0110" => -- ldab
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_ld8;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "0111" => -- stab
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1000" => -- eorb
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_eor;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1001" => -- adcb
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_adc;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1010" => -- orab
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_ora;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1011" => -- addb
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_add8;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1100" => -- ldd
								left_ctrl <= accd_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_ld16;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_hi_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1101" => -- std
								left_ctrl <= accd_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN "1110" => -- ldx / ldy
								IF ((pre_byte = "00011000") OR (pre_byte = "00011010")) THEN
									-- LDY
									left_ctrl <= iy_left;
									ix_ctrl <= latch_ix;
									iy_ctrl <= load_iy;
								ELSE
									-- LDX
									left_ctrl <= ix_left;
									ix_ctrl <= load_ix;
									iy_ctrl <= latch_iy;
								END IF;
								right_ctrl <= md_right;
								alu_ctrl <= alu_ld16;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								sp_ctrl <= latch_sp;
							WHEN "1111" => -- stx / sty
								IF ((pre_byte = "00011000") OR (pre_byte = "00011010")) THEN
									-- STY
									left_ctrl <= iy_left;
								ELSE
									-- STX
									left_ctrl <= ix_left;
								END IF;
								right_ctrl <= md_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
							WHEN OTHERS =>
								left_ctrl <= accb_left;
								right_ctrl <= md_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
						END CASE;
					WHEN OTHERS =>
						left_ctrl <= accd_left;
						right_ctrl <= md_right;
						alu_ctrl <= alu_nop;
						cc_ctrl <= latch_cc;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
				END CASE;
				ea_ctrl <= reset_ea;
				md_ctrl <= latch_md;
				count_ctrl <= reset_count;
				-- fetch the op code
				op_ctrl <= fetch_op;
				pre_ctrl <= fetch_pre;
				addr_ctrl <= fetch_ad;
				dout_ctrl <= md_lo_dout;
				iv_ctrl <= latch_iv;
				-- service non maskable interrupts
				IF (xirq = '1') AND (cc(XBIT) = '0') THEN
					pc_ctrl <= latch_pc;
					next_state <= int_pcl_state;
					-- service maskable interrupts
				ELSE
					--
					-- IRQ is level sensitive
					--
					IF (irq = '1') AND (cc(IBIT) = '0') THEN
						pc_ctrl <= latch_pc;
						next_state <= int_pcl_state;
					ELSE
						-- Advance the PC to fetch next instruction byte
						pc_ctrl <= incr_pc;
						next_state <= decode_state;
					END IF;
				END IF;
				--
				-- Here to decode instruction
				-- and fetch next byte of intruction
				-- whether it be necessary or not
				--
			WHEN decode_state =>
				-- fetch first byte of address or immediate data
				addr_ctrl <= fetch_ad;
				dout_ctrl <= md_lo_dout;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				pre_ctrl <= latch_pre;
				CASE op_code(7 DOWNTO 4) IS
					WHEN "0000" =>
						md_ctrl <= reset_md;
						sp_ctrl <= latch_sp;
						pc_ctrl <= latch_pc;
						op_ctrl <= latch_op;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0000" => -- test -- spin PC
								left_ctrl <= accd_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ea_ctrl <= reset_ea;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								next_state <= spin_state;
							WHEN "0001" => -- nop
								left_ctrl <= accd_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ea_ctrl <= reset_ea;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								next_state <= fetch_state;
							WHEN "0010" => -- idiv
								-- transfer IX to ea
								left_ctrl <= ix_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								ea_ctrl <= load_ea;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								next_state <= idiv_state;
							WHEN "0011" => -- fdiv
								left_ctrl <= ix_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								ea_ctrl <= load_ea;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= reset_ix;
								iy_ctrl <= latch_iy;
								next_state <= div1_state;
							WHEN "0100" => -- lsrd
								left_ctrl <= accd_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_lsr16;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_hi_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN "0101" => -- lsld
								left_ctrl <= accd_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_lsl16;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_hi_acca;
								accb_ctrl <= load_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN "0110" => -- tap
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_tap;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN "0111" => -- tpa
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_tpa;
								cc_ctrl <= latch_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN "1000" => -- inx / iny
								IF pre_byte = "00011000" THEN
									-- iny
									left_ctrl <= iy_left;
									ix_ctrl <= latch_ix;
									iy_ctrl <= load_iy;
								ELSE
									-- inx
									left_ctrl <= ix_left;
									ix_ctrl <= load_ix;
									iy_ctrl <= latch_iy;
								END IF;
								ea_ctrl <= reset_ea;
								right_ctrl <= one_right;
								alu_ctrl <= alu_inc16;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								next_state <= fetch_state;
							WHEN "1001" => -- dex / dey
								IF pre_byte = "00011000" THEN
									-- dey
									left_ctrl <= iy_left;
									ix_ctrl <= latch_ix;
									iy_ctrl <= load_iy;
								ELSE
									-- dex
									left_ctrl <= ix_left;
									ix_ctrl <= load_ix;
									iy_ctrl <= latch_iy;
								END IF;
								ea_ctrl <= reset_ea;
								right_ctrl <= one_right;
								alu_ctrl <= alu_dec16;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								next_state <= fetch_state;
							WHEN "1010" => -- clv
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_clv;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN "1011" => -- sev
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_sev;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN "1100" => -- clc
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_clc;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN "1101" => -- sec
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_sec;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN "1110" => -- cli
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_cli;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN "1111" => -- sei
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_sei;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								ea_ctrl <= reset_ea;
								next_state <= fetch_state;
						END CASE;
						-- acca / accb inherent instructions
					WHEN "0001" =>
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0000" => -- sba
								op_ctrl <= latch_op;
								left_ctrl <= acca_left;
								right_ctrl <= accb_right;
								alu_ctrl <= alu_sub8;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= latch_pc;
								next_state <= fetch_state;
							WHEN "0001" => -- cba
								op_ctrl <= latch_op;
								left_ctrl <= acca_left;
								right_ctrl <= accb_right;
								alu_ctrl <= alu_sub8;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= latch_pc;
								next_state <= fetch_state;
							WHEN "0010" => -- brset direct
								op_ctrl <= latch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= read8_state;
							WHEN "0011" => -- brclr direct
								op_ctrl <= latch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= read8_state;
							WHEN "0100" => -- bset direct
								op_ctrl <= latch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= read8_state;
							WHEN "0101" => -- bclr direct
								op_ctrl <= latch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= read8_state;
							WHEN "0110" => -- tab
								op_ctrl <= latch_op;
								left_ctrl <= acca_left;
								right_ctrl <= accb_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= load_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= load_accb;
								pc_ctrl <= latch_pc;
								next_state <= fetch_state;
							WHEN "0111" => -- tba
								op_ctrl <= latch_op;
								left_ctrl <= acca_left;
								right_ctrl <= accb_right;
								alu_ctrl <= alu_ld8;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= latch_pc;
								next_state <= fetch_state;
							WHEN "1000" => -- indexed y prebyte
								op_ctrl <= fetch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= decode_state;
							WHEN "1001" => -- daa
								op_ctrl <= latch_op;
								left_ctrl <= acca_left;
								right_ctrl <= accb_right;
								alu_ctrl <= alu_daa;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= latch_pc;
								next_state <= fetch_state;
							WHEN "1010" => -- prebyte - CPD / CPY / LDY / STY ff,X
								op_ctrl <= fetch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= decode_state;
							WHEN "1011" => -- aba
								op_ctrl <= latch_op;
								left_ctrl <= acca_left;
								right_ctrl <= accb_right;
								alu_ctrl <= alu_add8;
								cc_ctrl <= load_cc;
								acca_ctrl <= load_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= latch_pc;
								next_state <= fetch_state;
							WHEN "1100" => -- bset indexed
								op_ctrl <= latch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= indexed_state;
							WHEN "1101" => -- bclr indexed
								op_ctrl <= latch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= indexed_state;
							WHEN "1110" => -- brset indexed
								op_ctrl <= latch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= indexed_state;
							WHEN "1111" => -- brclr indexed
								op_ctrl <= latch_op;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= load_pc;
								next_state <= indexed_state;
							WHEN OTHERS =>
								op_ctrl <= latch_op;
								left_ctrl <= acca_left;
								right_ctrl <= accb_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								acca_ctrl <= latch_acca;
								accb_ctrl <= latch_accb;
								pc_ctrl <= latch_pc;
								next_state <= fetch_state;
						END CASE;
					WHEN "0010" => -- branch conditional
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- increment the pc
						left_ctrl <= pc_left;
						right_ctrl <= one_right;
						alu_ctrl <= alu_add16;
						cc_ctrl <= latch_cc;
						pc_ctrl <= load_pc;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0000" => -- bra
								next_state <= branch_state;
							WHEN "0001" => -- brn
								next_state <= fetch_state;
							WHEN "0010" => -- bhi
								IF (cc(CBIT) OR cc(ZBIT)) = '0' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "0011" => -- bls
								IF (cc(CBIT) OR cc(ZBIT)) = '1' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "0100" => -- bcc/bhs
								IF cc(CBIT) = '0' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "0101" => -- bcs/blo
								IF cc(CBIT) = '1' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "0110" => -- bne
								IF cc(ZBIT) = '0' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "0111" => -- beq
								IF cc(ZBIT) = '1' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "1000" => -- bvc
								IF cc(VBIT) = '0' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "1001" => -- bvs
								IF cc(VBIT) = '1' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "1010" => -- bpl
								IF cc(NBIT) = '0' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "1011" => -- bmi
								IF cc(NBIT) = '1' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "1100" => -- bge
								IF (cc(NBIT) XOR cc(VBIT)) = '0' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "1101" => -- blt
								IF (cc(NBIT) XOR cc(VBIT)) = '1' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "1110" => -- bgt
								IF (cc(ZBIT) OR (cc(NBIT) XOR cc(VBIT))) = '0' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN "1111" => -- ble
								IF (cc(ZBIT) OR (cc(NBIT) XOR cc(VBIT))) = '1' THEN
									next_state <= branch_state;
								ELSE
									next_state <= fetch_state;
								END IF;
							WHEN OTHERS =>
								next_state <= fetch_state;
						END CASE;
						--
						-- Single byte stack operators
						-- Do not advance PC
						--
					WHEN "0011" =>
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						pc_ctrl <= latch_pc;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0000" => -- tsx / tsy
								left_ctrl <= sp_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								sp_ctrl <= latch_sp;
								IF pre_byte = "00011000" THEN
									-- tsy
									ix_ctrl <= latch_ix;
									iy_ctrl <= load_iy;
								ELSE
									-- tsx
									ix_ctrl <= load_ix;
									iy_ctrl <= latch_iy;
								END IF;
								next_state <= fetch_state;
							WHEN "0001" => -- ins
								left_ctrl <= sp_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= load_sp;
								next_state <= fetch_state;
							WHEN "0010" => -- pula
								left_ctrl <= sp_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= load_sp;
								next_state <= pula_state;
							WHEN "0011" => -- pulb
								left_ctrl <= sp_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= load_sp;
								next_state <= pulb_state;
							WHEN "0100" => -- des
								-- decrement sp
								left_ctrl <= sp_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_sub16;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= load_sp;
								next_state <= fetch_state;
							WHEN "0101" => -- txs / tys
								IF pre_byte = "00011000" THEN
									-- tys
									left_ctrl <= iy_left;
								ELSE
									-- txs
									left_ctrl <= ix_left;
								END IF;
								right_ctrl <= one_right;
								alu_ctrl <= alu_sub16;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= load_sp;
								next_state <= fetch_state;
							WHEN "0110" => -- psha
								left_ctrl <= sp_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
								next_state <= psha_state;
							WHEN "0111" => -- pshb
								left_ctrl <= sp_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
								next_state <= pshb_state;
							WHEN "1000" => -- pulxy
								left_ctrl <= sp_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= load_sp;
								next_state <= pulxy_hi_state;
							WHEN "1001" => -- rts
								left_ctrl <= sp_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= load_sp;
								next_state <= rts_hi_state;
							WHEN "1010" => -- abx / aby
								IF pre_byte = "00011000" THEN
									left_ctrl <= iy_left;
									ix_ctrl <= latch_ix;
									iy_ctrl <= load_iy;
								ELSE
									left_ctrl <= ix_left;
									ix_ctrl <= load_ix;
									iy_ctrl <= latch_iy;
								END IF;
								right_ctrl <= accb_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								sp_ctrl <= latch_sp;
								next_state <= fetch_state;
							WHEN "1011" => -- rti
								left_ctrl <= sp_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= load_sp;
								next_state <= rti_cc_state;
							WHEN "1100" => -- pshxy
								left_ctrl <= sp_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
								next_state <= pshxy_lo_state;
							WHEN "1101" => -- mul
								left_ctrl <= acca_left;
								right_ctrl <= accb_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
								next_state <= mul_state;
							WHEN "1110" => -- wai
								left_ctrl <= sp_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
								next_state <= int_pcl_state;
							WHEN "1111" => -- swi
								left_ctrl <= sp_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
								next_state <= int_pcl_state;
							WHEN OTHERS =>
								left_ctrl <= sp_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ix_ctrl <= latch_ix;
								iy_ctrl <= latch_iy;
								sp_ctrl <= latch_sp;
								next_state <= fetch_state;
						END CASE;
						--
						-- Accumulator A Single operand
						-- source = Acc A dest = Acc A
						-- Do not advance PC
						--
					WHEN "0100" => -- acca single op
						ea_ctrl <= latch_ea;
						md_ctrl <= latch_md;
						op_ctrl <= latch_op;
						accb_ctrl <= latch_accb;
						pc_ctrl <= latch_pc;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						left_ctrl <= acca_left;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0000" => -- neg
								right_ctrl <= zero_right;
								alu_ctrl <= alu_neg;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN "0011" => -- com
								right_ctrl <= zero_right;
								alu_ctrl <= alu_com;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN "0100" => -- lsr
								right_ctrl <= zero_right;
								alu_ctrl <= alu_lsr8;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN "0110" => -- ror
								right_ctrl <= zero_right;
								alu_ctrl <= alu_ror8;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN "0111" => -- asr
								right_ctrl <= zero_right;
								alu_ctrl <= alu_asr8;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN "1000" => -- asl
								right_ctrl <= zero_right;
								alu_ctrl <= alu_asl8;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN "1001" => -- rol
								right_ctrl <= zero_right;
								alu_ctrl <= alu_rol8;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN "1010" => -- dec
								right_ctrl <= one_right;
								alu_ctrl <= alu_dec;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN "1011" => -- undefined
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								acca_ctrl <= latch_acca;
								cc_ctrl <= latch_cc;
							WHEN "1100" => -- inc
								right_ctrl <= one_right;
								alu_ctrl <= alu_inc;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN "1101" => -- tst
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st8;
								acca_ctrl <= latch_acca;
								cc_ctrl <= load_cc;
							WHEN "1110" => -- jmp
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								acca_ctrl <= latch_acca;
								cc_ctrl <= latch_cc;
							WHEN "1111" => -- clr
								right_ctrl <= zero_right;
								alu_ctrl <= alu_clr;
								acca_ctrl <= load_acca;
								cc_ctrl <= load_cc;
							WHEN OTHERS =>
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								acca_ctrl <= latch_acca;
								cc_ctrl <= latch_cc;
						END CASE;
						next_state <= fetch_state;
						--
						-- single operand acc b
						-- Do not advance PC
						--
					WHEN "0101" =>
						ea_ctrl <= latch_ea;
						md_ctrl <= latch_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						pc_ctrl <= latch_pc;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						left_ctrl <= accb_left;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0000" => -- neg
								right_ctrl <= zero_right;
								alu_ctrl <= alu_neg;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN "0011" => -- com
								right_ctrl <= zero_right;
								alu_ctrl <= alu_com;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN "0100" => -- lsr
								right_ctrl <= zero_right;
								alu_ctrl <= alu_lsr8;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN "0110" => -- ror
								right_ctrl <= zero_right;
								alu_ctrl <= alu_ror8;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN "0111" => -- asr
								right_ctrl <= zero_right;
								alu_ctrl <= alu_asr8;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN "1000" => -- asl
								right_ctrl <= zero_right;
								alu_ctrl <= alu_asl8;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN "1001" => -- rol
								right_ctrl <= zero_right;
								alu_ctrl <= alu_rol8;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN "1010" => -- dec
								right_ctrl <= one_right;
								alu_ctrl <= alu_dec;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN "1011" => -- undefined
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								accb_ctrl <= latch_accb;
								cc_ctrl <= latch_cc;
							WHEN "1100" => -- inc
								right_ctrl <= one_right;
								alu_ctrl <= alu_inc;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN "1101" => -- tst
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st8;
								accb_ctrl <= latch_accb;
								cc_ctrl <= load_cc;
							WHEN "1110" => -- jmp
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								accb_ctrl <= latch_accb;
								cc_ctrl <= latch_cc;
							WHEN "1111" => -- clr
								right_ctrl <= zero_right;
								alu_ctrl <= alu_clr;
								accb_ctrl <= load_accb;
								cc_ctrl <= load_cc;
							WHEN OTHERS =>
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								accb_ctrl <= latch_accb;
								cc_ctrl <= latch_cc;
						END CASE;
						next_state <= fetch_state;
						--
						-- Single operand indexed
						-- Two byte instruction so advance PC
						-- EA should hold index offset
						--
					WHEN "0110" => -- indexed single op
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- increment the pc 
						left_ctrl <= pc_left;
						right_ctrl <= one_right;
						alu_ctrl <= alu_add16;
						cc_ctrl <= latch_cc;
						pc_ctrl <= load_pc;
						next_state <= indexed_state;
						--
						-- Single operand extended addressing
						-- three byte instruction so advance the PC
						-- Low order EA holds high order address
						--
					WHEN "0111" => -- extended single op
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- increment the pc
						left_ctrl <= pc_left;
						right_ctrl <= one_right;
						alu_ctrl <= alu_add16;
						cc_ctrl <= latch_cc;
						pc_ctrl <= load_pc;
						next_state <= extended_state;

					WHEN "1000" => -- acca immediate
						ea_ctrl <= fetch_first_ea; -- for BSR
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						cc_ctrl <= latch_cc;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0011" | -- subd #
								"1100" | -- cpx / cpy #
								"1110" => -- lds #
								-- increment the pc
								md_ctrl <= fetch_first_md;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								pc_ctrl <= load_pc;
								next_state <= immediate16_state;
							WHEN "1101" => -- bsr
								-- increment the pc
								md_ctrl <= fetch_first_md;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								pc_ctrl <= load_pc;
								next_state <= bsr_state;
							WHEN "1111" => -- egdx /egdy
								-- idle pc
								left_ctrl <= accd_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								pc_ctrl <= latch_pc;
								md_ctrl <= load_md;
								next_state <= exchange_state;
							WHEN OTHERS =>
								md_ctrl <= fetch_first_md;
								left_ctrl <= pc_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								pc_ctrl <= load_pc;
								next_state <= fetch_state;
						END CASE;

					WHEN "1001" => -- acca direct
						ea_ctrl <= fetch_first_ea;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- increment the pc
						pc_ctrl <= incr_pc;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0111" => -- staa direct
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1111" => -- sts direct
								left_ctrl <= sp_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write16_state;
							WHEN "1101" => -- jsr direct
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= fetch_first_md;
								next_state <= jsr_state;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= fetch_first_md;
								next_state <= read8_state;
						END CASE;

					WHEN "1010" => -- acca indexed
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- increment the pc
						left_ctrl <= pc_left;
						right_ctrl <= one_right;
						alu_ctrl <= alu_add16;
						cc_ctrl <= latch_cc;
						pc_ctrl <= load_pc;
						next_state <= indexed_state;

					WHEN "1011" => -- acca extended
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- increment the pc
						left_ctrl <= pc_left;
						right_ctrl <= one_right;
						alu_ctrl <= alu_add16;
						cc_ctrl <= latch_cc;
						pc_ctrl <= load_pc;
						next_state <= extended_state;

					WHEN "1100" => -- accb immediate
						ea_ctrl <= latch_ea;
						md_ctrl <= fetch_first_md;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- increment the pc
						left_ctrl <= pc_left;
						right_ctrl <= one_right;
						alu_ctrl <= alu_add16;
						cc_ctrl <= latch_cc;
						pc_ctrl <= load_pc;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0011" | -- addd #
								"1100" | -- ldd #
								"1110" => -- ldx # / ldy #
								op_ctrl <= latch_op;
								next_state <= immediate16_state;
							WHEN "1101" => -- indexed Y pre-byte $CD
								op_ctrl <= fetch_op;
								next_state <= decode_state;
							WHEN OTHERS =>
								op_ctrl <= latch_op;
								next_state <= fetch_state;
						END CASE;

					WHEN "1101" => -- accb direct
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						pc_ctrl <= incr_pc;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0111" => -- stab direct
								left_ctrl <= accb_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1101" => -- std direct
								left_ctrl <= accd_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write16_state;
							WHEN "1111" => -- stx / sty direct
								IF (pre_byte = "00011000") OR (pre_byte = "00011010") THEN
									left_ctrl <= iy_left;
								ELSE
									left_ctrl <= ix_left;
								END IF;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write16_state;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= fetch_first_md;
								next_state <= read8_state;
						END CASE;

					WHEN "1110" => -- accb indexed
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- increment the pc
						left_ctrl <= pc_left;
						right_ctrl <= one_right;
						alu_ctrl <= alu_add16;
						cc_ctrl <= latch_cc;
						pc_ctrl <= load_pc;
						next_state <= indexed_state;

					WHEN "1111" => -- accb extended
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- increment the pc
						left_ctrl <= pc_left;
						right_ctrl <= one_right;
						alu_ctrl <= alu_add16;
						cc_ctrl <= latch_cc;
						pc_ctrl <= load_pc;
						next_state <= extended_state;

					WHEN OTHERS =>
						ea_ctrl <= fetch_first_ea;
						md_ctrl <= fetch_first_md;
						op_ctrl <= latch_op;
						acca_ctrl <= latch_acca;
						accb_ctrl <= latch_accb;
						ix_ctrl <= latch_ix;
						iy_ctrl <= latch_iy;
						sp_ctrl <= latch_sp;
						-- idle the pc
						left_ctrl <= pc_left;
						right_ctrl <= zero_right;
						alu_ctrl <= alu_nop;
						cc_ctrl <= latch_cc;
						pc_ctrl <= latch_pc;
						next_state <= fetch_state;
				END CASE;

			WHEN immediate16_state =>
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				pre_ctrl <= latch_pre;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				ea_ctrl <= latch_ea;
				-- increment pc
				left_ctrl <= pc_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				pc_ctrl <= load_pc;
				-- fetch next immediate byte
				md_ctrl <= fetch_next_md;
				addr_ctrl <= fetch_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= fetch_state;
				--
				-- ea holds 8 bit index offet
				-- calculate the effective memory address
				-- using the alu
				--
			WHEN indexed_state =>
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				-- idle bus.
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				-- add 8 bit ea to ix or iy
				IF ((pre_byte = "00011000") OR (pre_byte = "11001101")) THEN
					ea_ctrl <= add_iy_ea;
				ELSE
					ea_ctrl <= add_ix_ea;
				END IF;
				CASE op_code(7 DOWNTO 4) IS
					WHEN "0001" => -- BSET, BCLR, BRSET, BRCLR
						left_ctrl <= acca_left;
						right_ctrl <= zero_right;
						alu_ctrl <= alu_nop;
						cc_ctrl <= latch_cc;
						md_ctrl <= latch_md;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "1100" | -- BSET
								"1101" | -- BCLR
								"1110" | -- BRSET
								"1111" => -- BRCLR
								next_state <= read8_state;
							WHEN OTHERS =>
								next_state <= fetch_state;
						END CASE;
					WHEN "0110" => -- single op indexed
						CASE op_code(3 DOWNTO 0) IS
							WHEN "1011" => -- undefined
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= fetch_state;
							WHEN "1110" => -- jmp
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= jmp_state;
							WHEN "1111" => -- clr
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								--				     alu_ctrl   <= alu_st8;
								alu_ctrl <= alu_clr; -- 13 Jan 2004 /sashz
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= read8_state;
						END CASE;
					WHEN "1010" => -- acca indexed
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0111" => -- staa
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1101" => -- jsr
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= jsr_state;
							WHEN "1111" => -- sts
								left_ctrl <= sp_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write16_state;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= read8_state;
						END CASE;
					WHEN "1110" => -- accb indexed
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0111" => -- stab direct
								left_ctrl <= accb_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1101" => -- std direct
								left_ctrl <= accd_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write16_state;
							WHEN "1111" => -- stx / sty direct
								IF (pre_byte = "00011000") OR (pre_byte = "00011010") THEN
									left_ctrl <= iy_left;
								ELSE
									left_ctrl <= ix_left;
								END IF;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write16_state;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= read8_state;
						END CASE;
					WHEN OTHERS =>
						left_ctrl <= acca_left;
						right_ctrl <= zero_right;
						alu_ctrl <= alu_nop;
						cc_ctrl <= latch_cc;
						md_ctrl <= latch_md;
						next_state <= fetch_state;
				END CASE;
				--
				-- ea holds 8 bit index offet
				-- calculate the effective memory address
				-- using the alu
				--
				--
				-- ea holds the low byte of the absolute address
				-- Move ea low byte into ea high byte
				-- load new ea low byte to for absolute 16 bit address
				-- advance the program counter
				--
			WHEN extended_state => -- fetch ea low byte
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				-- increment pc
				pc_ctrl <= incr_pc;
				-- fetch next effective address bytes
				ea_ctrl <= fetch_next_ea;
				addr_ctrl <= fetch_ad;
				dout_ctrl <= md_lo_dout;
				-- work out the next state
				CASE op_code(7 DOWNTO 4) IS
					WHEN "0111" => -- single op extended
						CASE op_code(3 DOWNTO 0) IS
							WHEN "1011" => -- undefined
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= fetch_state;
							WHEN "1110" => -- jmp
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= jmp_state;
							WHEN "1111" => -- clr
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								--				       alu_ctrl   <= alu_ld8;
								alu_ctrl <= alu_clr; -- 13 Jan 2004 /sashz
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= read8_state;
						END CASE;
					WHEN "1011" => -- acca extended
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0111" => -- staa
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1101" => -- jsr
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= jsr_state;
							WHEN "1111" => -- sts
								left_ctrl <= sp_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write16_state;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= read8_state;
						END CASE;
					WHEN "1111" => -- accb extended
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0111" => -- stab
								left_ctrl <= accb_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1101" => -- std
								left_ctrl <= accd_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write16_state;
							WHEN "1111" => -- stx / sty
								IF ((pre_byte = "00011000") OR (pre_byte = "00011010")) THEN
									left_ctrl <= iy_left;
								ELSE
									left_ctrl <= ix_left;
								END IF;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st16;
								cc_ctrl <= latch_cc;
								md_ctrl <= load_md;
								next_state <= write16_state;
							WHEN OTHERS =>
								left_ctrl <= acca_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= read8_state;
						END CASE;
					WHEN OTHERS =>
						md_ctrl <= latch_md;
						left_ctrl <= acca_left;
						right_ctrl <= zero_right;
						alu_ctrl <= alu_nop;
						cc_ctrl <= latch_cc;
						next_state <= fetch_state;
				END CASE;
				--
				-- here if ea holds low byte (direct page)
				-- can enter here from extended addressing
				-- read memory location
				-- note that reads may be 8 or 16 bits
				--
			WHEN read8_state => -- read data
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				-- read first data byte from ea
				md_ctrl <= fetch_first_md;
				addr_ctrl <= read_ad;
				dout_ctrl <= md_lo_dout;
				CASE op_code(7 DOWNTO 4) IS
					WHEN "0001" => -- bset / bclr / brset / brclr
						left_ctrl <= pc_left;
						right_ctrl <= one_right;
						alu_ctrl <= alu_add16;
						cc_ctrl <= latch_cc;
						ea_ctrl <= latch_ea;
						pc_ctrl <= load_pc;
						next_state <= bitmask_state;
					WHEN "0110" | "0111" => -- single operand
						left_ctrl <= ea_left;
						right_ctrl <= zero_right;
						alu_ctrl <= alu_nop;
						cc_ctrl <= latch_cc;
						ea_ctrl <= latch_ea;
						pc_ctrl <= latch_pc;
						next_state <= execute_state;
					WHEN "1001" | "1010" | "1011" => -- acca
						pc_ctrl <= latch_pc;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0011" | -- subd / cpd
								"1110" | -- lds
								"1100" => -- cpx / cpy
								-- increment the effective address in case of 16 bit load
								left_ctrl <= ea_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								ea_ctrl <= load_ea;
								next_state <= read16_state;
							WHEN OTHERS =>
								left_ctrl <= ea_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ea_ctrl <= latch_ea;
								next_state <= fetch_state;
						END CASE;
					WHEN "1101" | "1110" | "1111" => -- accb
						pc_ctrl <= latch_pc;
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0011" | -- addd
								"1100" | -- ldd
								"1110" => -- ldx / ldy
								-- increment the effective address in case of 16 bit load
								left_ctrl <= ea_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_add16;
								cc_ctrl <= latch_cc;
								ea_ctrl <= load_ea;
								next_state <= read16_state;
							WHEN OTHERS =>
								left_ctrl <= ea_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								ea_ctrl <= latch_ea;
								next_state <= fetch_state;
						END CASE;
					WHEN OTHERS =>
						left_ctrl <= ea_left;
						right_ctrl <= zero_right;
						alu_ctrl <= alu_nop;
						cc_ctrl <= latch_cc;
						ea_ctrl <= latch_ea;
						pc_ctrl <= latch_pc;
						next_state <= fetch_state;
				END CASE;

			WHEN read16_state => -- read second data byte from ea
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				-- idle the effective address
				left_ctrl <= ea_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				ea_ctrl <= latch_ea;
				-- read the low byte of the 16 bit data
				md_ctrl <= fetch_next_md;
				addr_ctrl <= read_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= fetch_state;

				--
				-- exchange registers
				-- at this point md holds accd
				-- transfer X or Y to accd
				--
			WHEN exchange_state => -- md holds accd
				-- default
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				md_ctrl <= latch_md;
				-- transfer x or y to accd
				IF pre_byte = "00011000" THEN
					left_ctrl <= iy_left;
				ELSE
					left_ctrl <= ix_left;
				END IF;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_st16;
				cc_ctrl <= latch_cc;
				acca_ctrl <= load_hi_acca;
				accb_ctrl <= load_accb;
				-- idle the address bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= fetch_state;

			WHEN bitmask_state => -- fetch bit mask from next op
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				ea_ctrl <= latch_ea;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				-- addvance the pc
				left_ctrl <= pc_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				pc_ctrl <= load_pc;
				-- read the bit mask into the pre byte register
				pre_ctrl <= fetch_pre;
				addr_ctrl <= fetch_ad;
				dout_ctrl <= md_lo_dout;
				CASE op_code IS
					WHEN "00010010" | "00011110" => -- brset
						next_state <= brset_state;
					WHEN "00010011" | "00011111" => -- brclr
						next_state <= brclr_state;
					WHEN "00010100" | "00011100" => -- bset
						next_state <= execute_state;
					WHEN "00010101" | "00011101" => -- bclr
						next_state <= execute_state;
					WHEN OTHERS =>
						next_state <= fetch_state;
				END CASE;

			WHEN brclr_state => -- fetch the branch offset
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				-- advance the pc
				left_ctrl <= pc_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				pc_ctrl <= load_pc;
				-- fetch the branch offset
				addr_ctrl <= fetch_ad;
				ea_ctrl <= fetch_first_ea;
				dout_ctrl <= md_lo_dout;
				IF (pre_byte AND md(7 DOWNTO 0)) = "00000000" THEN
					next_state <= branch_state;
				ELSE
					next_state <= fetch_state;
				END IF;

			WHEN brset_state => -- fetch the branch offset
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				-- advance the pc
				left_ctrl <= pc_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				pc_ctrl <= load_pc;
				-- fetch the branch offset
				addr_ctrl <= fetch_ad;
				ea_ctrl <= fetch_first_ea;
				dout_ctrl <= md_lo_dout;
				IF (pre_byte AND md(7 DOWNTO 0)) = "00000000" THEN
					next_state <= fetch_state;
				ELSE
					next_state <= branch_state;
				END IF;
			WHEN jmp_state =>
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- load PC with effective address
				left_ctrl <= pc_left;
				right_ctrl <= ea_right;
				alu_ctrl <= alu_ld16;
				cc_ctrl <= latch_cc;
				pc_ctrl <= load_pc;
				-- idle the bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= fetch_state;

			WHEN jsr_state => -- JSR
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write pc low
				addr_ctrl <= push_ad;
				dout_ctrl <= pc_lo_dout;
				next_state <= jsr1_state;

			WHEN jsr1_state => -- JSR
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write pc hi
				addr_ctrl <= push_ad;
				dout_ctrl <= pc_hi_dout;
				next_state <= jmp_state;

			WHEN branch_state => -- Bcc
				-- default registers
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- calculate signed branch
				left_ctrl <= pc_left;
				right_ctrl <= sexea_right; -- right must be sign extended effective address
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				pc_ctrl <= load_pc;
				-- idle the bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= fetch_state;

			WHEN bsr_state => -- BSR
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write pc low
				addr_ctrl <= push_ad;
				dout_ctrl <= pc_lo_dout;
				next_state <= bsr1_state;

			WHEN bsr1_state => -- BSR
				-- default registers
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write pc hi
				addr_ctrl <= push_ad;
				dout_ctrl <= pc_hi_dout;
				next_state <= branch_state;

			WHEN rts_hi_state => -- RTS
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment the sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- read pc hi
				pc_ctrl <= pull_hi_pc;
				addr_ctrl <= pull_ad;
				dout_ctrl <= pc_hi_dout;
				next_state <= rts_lo_state;

			WHEN rts_lo_state => -- RTS1
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- idle the ALU
				left_ctrl <= acca_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				-- read pc low
				pc_ctrl <= pull_lo_pc;
				addr_ctrl <= pull_ad;
				dout_ctrl <= pc_lo_dout;
				next_state <= fetch_state;

			WHEN mul_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- move acca to md
				left_ctrl <= acca_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_st16;
				cc_ctrl <= latch_cc;
				md_ctrl <= load_md;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= mulea_state;

			WHEN mulea_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				md_ctrl <= latch_md;
				-- move accb to ea
				left_ctrl <= accb_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_st16;
				cc_ctrl <= latch_cc;
				ea_ctrl <= load_ea;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= muld_state;

			WHEN muld_state =>
				-- default
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				md_ctrl <= latch_md;
				-- clear accd
				left_ctrl <= acca_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_ld8;
				cc_ctrl <= latch_cc;
				acca_ctrl <= load_hi_acca;
				accb_ctrl <= load_accb;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= mul0_state;

			WHEN mul0_state =>
				-- default
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= inc_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- if ea bit(count) set, add accd to md
				left_ctrl <= accd_left;
				right_ctrl <= md_right;
				alu_ctrl <= alu_mul;
				IF ea_bit = '1' THEN
					cc_ctrl <= load_cc;
					acca_ctrl <= load_hi_acca;
					accb_ctrl <= load_accb;
				ELSE
					cc_ctrl <= latch_cc;
					acca_ctrl <= latch_acca;
					accb_ctrl <= latch_accb;
				END IF;
				md_ctrl <= shiftl_md;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				IF count = "0111" THEN
					next_state <= fetch_state;
				ELSE
					next_state <= mul0_state;
				END IF;

				--
				-- Integer division
				-- ACCD = numerator
				-- EA = denominator
				-- IX = quotient
				-- 
				-- For integer divide, re-arrange registers
				-- IX = ACCD = dividend low word
				-- ACCD = 0 = dividend 
				--
			WHEN idiv_state =>
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- transfer ACCD to IX
				left_ctrl <= accd_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_st16;
				cc_ctrl <= load_cc;
				ix_ctrl <= load_ix; --- quotient / dividend
				acca_ctrl <= reset_acca;
				accb_ctrl <= reset_accb;
				md_ctrl <= reset_md;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= div2_state;
				--
				-- Common integer divide
				-- ACCD = Dividend high word
				-- IX = Dividend low word / Quotient
				-- EA = Divisor
				-- MD = Temp for subtraction
				--
				-- Test for divide
				-- MD = ACCD - EA
				--
			WHEN div1_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= inc_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- subtract denominator from numerator
				left_ctrl <= accd_left;
				right_ctrl <= ea_right;
				alu_ctrl <= alu_div;
				cc_ctrl <= load_cc;
				md_ctrl <= load_md; -- md = temporary result
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= div2_state;

				--
				-- shift carry into quotient
				-- IX = IX << 1 + Carry
				-- next state dependant on carry from previous state
				--
			WHEN div2_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= latch_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				md_ctrl <= latch_md;
				-- rotate carry into quotient
				left_ctrl <= ix_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_rol16;
				cc_ctrl <= load_cc;
				ix_ctrl <= load_ix;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				IF count = "10000" THEN
					next_state <= div5_state;
				ELSIF cc(CBIT) = '1' THEN
					next_state <= div3_state;
				ELSE
					next_state <= div4_state;
				END IF;

				--
				-- hear if Carry Set from subtract
				-- ACCD = ACCD << 1 + Carry
				--
			WHEN div3_state =>
				-- default
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= latch_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				md_ctrl <= latch_md;
				-- shift numerator left
				left_ctrl <= accd_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_rol16;
				cc_ctrl <= load_cc;
				acca_ctrl <= load_hi_acca;
				accb_ctrl <= load_accb;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= div1_state;
				--
				-- hear if no Carry from subtract
				-- ACCD = MD << 1 + Carry
				--
			WHEN div4_state =>
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= latch_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				md_ctrl <= latch_md;
				-- numerator = Subtraction rotated left
				left_ctrl <= md_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_rol16;
				cc_ctrl <= load_cc;
				acca_ctrl <= load_hi_acca;
				accb_ctrl <= load_accb;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= div1_state;

				--
				-- Get remainder
				-- ACCD = MIN(ACCD, MD)
				--
			WHEN div5_state =>
				-- default
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				count_ctrl <= latch_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				md_ctrl <= latch_md;
				-- complement quotient
				IF (acca & accb) < md THEN
					left_ctrl <= accd_left;
				ELSE
					left_ctrl <= md_left;
				END IF;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_mindiv;
				cc_ctrl <= load_cc;
				acca_ctrl <= load_hi_acca;
				accb_ctrl <= load_accb;
				ix_ctrl <= load_ix_idiv;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= fetch_state;

				--
				-- Spin the Program counter
				--
			WHEN spin_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				iv_ctrl <= latch_iv;
				count_ctrl <= latch_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				md_ctrl <= latch_md;
				-- complement quotient
				left_ctrl <= pc_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				pc_ctrl <= load_pc;
				cc_ctrl <= latch_cc;
				-- idle bus
				addr_ctrl <= fetch_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= spin_state;

				--
				-- Execute cycle is performed by
				-- single operand indexed and extended instructions
				-- and bit operators.
				--
			WHEN execute_state => -- execute
				-- default
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				count_ctrl <= reset_count;
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				iv_ctrl <= latch_iv;
				ea_ctrl <= latch_ea;
				-- idle the bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				CASE op_code(7 DOWNTO 4) IS
					WHEN "0001" => -- bit operators come here
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0100" | "1100" => -- bset
								-- OR bit
								left_ctrl <= md_left;
								right_ctrl <= pre_right;
								alu_ctrl <= alu_bset;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "0101" | "1101" => -- bclr
								-- AND bit
								left_ctrl <= md_left;
								right_ctrl <= pre_right;
								alu_ctrl <= alu_bclr;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN OTHERS =>
								-- idle ALU
								left_ctrl <= md_left;
								right_ctrl <= pre_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= fetch_state;
						END CASE;

					WHEN "0110" | -- indexed single op
						"0111" => -- extended single op
						CASE op_code(3 DOWNTO 0) IS
							WHEN "0000" => -- neg
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_neg;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "0011" => -- com
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_com;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "0100" => -- lsr
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_lsr8;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "0110" => -- ror
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_ror8;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "0111" => -- asr
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_asr8;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1000" => -- asl
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_asl8;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1001" => -- rol
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_rol8;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1010" => -- dec
								left_ctrl <= md_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_dec;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1011" => -- undefined
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= fetch_state;
							WHEN "1100" => -- inc
								left_ctrl <= md_left;
								right_ctrl <= one_right;
								alu_ctrl <= alu_inc;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN "1101" => -- tst
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_st8;
								cc_ctrl <= load_cc;
								md_ctrl <= latch_md;
								next_state <= fetch_state;
							WHEN "1110" => -- jmp
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= fetch_state;
							WHEN "1111" => -- clr
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_clr;
								cc_ctrl <= load_cc;
								md_ctrl <= load_md;
								next_state <= write8_state;
							WHEN OTHERS =>
								left_ctrl <= md_left;
								right_ctrl <= zero_right;
								alu_ctrl <= alu_nop;
								cc_ctrl <= latch_cc;
								md_ctrl <= latch_md;
								next_state <= fetch_state;
						END CASE;

					WHEN OTHERS =>
						left_ctrl <= accd_left;
						right_ctrl <= md_right;
						alu_ctrl <= alu_nop;
						cc_ctrl <= latch_cc;
						md_ctrl <= latch_md;
						next_state <= fetch_state;
				END CASE;
				--
				-- 16 bit Write state
				-- write high byte of ALU output.
				-- EA hold address of memory to write to
				-- Advance the effective address in ALU
				--
			WHEN write16_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				-- increment the effective address
				left_ctrl <= ea_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				ea_ctrl <= load_ea;
				-- write the ALU hi byte to ea
				addr_ctrl <= write_ad;
				dout_ctrl <= md_hi_dout;
				next_state <= write8_state;
				--
				-- 8 bit write
				-- Write low 8 bits of ALU output
				--
			WHEN write8_state =>
				-- default registers
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- idle the ALU
				left_ctrl <= acca_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				-- write ALU low byte output
				addr_ctrl <= write_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= fetch_state;

			WHEN psha_state =>
				-- default registers
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write acca
				addr_ctrl <= push_ad;
				dout_ctrl <= acca_dout;
				next_state <= fetch_state;

			WHEN pula_state =>
				-- default registers
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- idle sp
				left_ctrl <= sp_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				sp_ctrl <= latch_sp;
				-- read acca
				acca_ctrl <= pull_acca;
				addr_ctrl <= pull_ad;
				dout_ctrl <= acca_dout;
				next_state <= fetch_state;

			WHEN pshb_state =>
				-- default registers
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write accb
				addr_ctrl <= push_ad;
				dout_ctrl <= accb_dout;
				next_state <= fetch_state;

			WHEN pulb_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- idle sp
				left_ctrl <= sp_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				sp_ctrl <= latch_sp;
				-- read accb
				accb_ctrl <= pull_accb;
				addr_ctrl <= pull_ad;
				dout_ctrl <= accb_dout;
				next_state <= fetch_state;

			WHEN pshxy_lo_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write ix low
				addr_ctrl <= push_ad;
				IF pre_byte = "00011000" THEN
					dout_ctrl <= iy_lo_dout;
				ELSE
					dout_ctrl <= ix_lo_dout;
				END IF;
				next_state <= pshxy_hi_state;

			WHEN pshxy_hi_state =>
				-- default registers
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write ix / iy hi
				addr_ctrl <= push_ad;
				IF pre_byte = "00011000" THEN
					dout_ctrl <= iy_hi_dout;
				ELSE
					dout_ctrl <= ix_hi_dout;
				END IF;
				next_state <= fetch_state;

			WHEN pulxy_hi_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- pull ix hi
				IF pre_byte = "00011000" THEN
					ix_ctrl <= latch_ix;
					iy_ctrl <= pull_hi_iy;
					dout_ctrl <= iy_hi_dout;
				ELSE
					ix_ctrl <= pull_hi_ix;
					iy_ctrl <= latch_iy;
					dout_ctrl <= ix_hi_dout;
				END IF;
				addr_ctrl <= pull_ad;
				next_state <= pulxy_lo_state;

			WHEN pulxy_lo_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- idle sp
				left_ctrl <= sp_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				sp_ctrl <= latch_sp;
				-- read ix low
				IF pre_byte = "00011000" THEN
					ix_ctrl <= latch_ix;
					iy_ctrl <= pull_lo_iy;
					dout_ctrl <= iy_lo_dout;
				ELSE
					ix_ctrl <= pull_lo_ix;
					iy_ctrl <= latch_iy;
					dout_ctrl <= ix_lo_dout;
				END IF;
				addr_ctrl <= pull_ad;
				next_state <= fetch_state;

				--
				-- return from interrupt
				-- enter here from bogus interrupts
				--
			WHEN rti_state =>
				-- default registers
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				sp_ctrl <= load_sp;
				-- idle address bus
				cc_ctrl <= latch_cc;
				addr_ctrl <= idle_ad;
				dout_ctrl <= cc_dout;
				next_state <= rti_cc_state;

			WHEN rti_cc_state =>
				-- default registers
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				sp_ctrl <= load_sp;
				-- read cc
				cc_ctrl <= pull_cc;
				addr_ctrl <= pull_ad;
				dout_ctrl <= cc_dout;
				next_state <= rti_accb_state;

			WHEN rti_accb_state =>
				-- default registers
				acca_ctrl <= latch_acca;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- read accb
				accb_ctrl <= pull_accb;
				addr_ctrl <= pull_ad;
				dout_ctrl <= accb_dout;
				next_state <= rti_acca_state;

			WHEN rti_acca_state =>
				-- default registers
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- read acca
				acca_ctrl <= pull_acca;
				addr_ctrl <= pull_ad;
				dout_ctrl <= acca_dout;
				next_state <= rti_ixh_state;

			WHEN rti_ixh_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- read ix hi
				ix_ctrl <= pull_hi_ix;
				addr_ctrl <= pull_ad;
				dout_ctrl <= ix_hi_dout;
				next_state <= rti_ixl_state;

			WHEN rti_ixl_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- read ix low
				ix_ctrl <= pull_lo_ix;
				addr_ctrl <= pull_ad;
				dout_ctrl <= ix_lo_dout;
				next_state <= rti_iyh_state;

			WHEN rti_iyh_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- read iy hi
				iy_ctrl <= pull_hi_iy;
				addr_ctrl <= pull_ad;
				dout_ctrl <= iy_hi_dout;
				next_state <= rti_iyl_state;

			WHEN rti_iyl_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- read iy low
				iy_ctrl <= pull_lo_iy;
				addr_ctrl <= pull_ad;
				dout_ctrl <= iy_lo_dout;
				next_state <= rti_pch_state;

			WHEN rti_pch_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- increment sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_add16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- pull pc hi
				pc_ctrl <= pull_hi_pc;
				addr_ctrl <= pull_ad;
				dout_ctrl <= pc_hi_dout;
				next_state <= rti_pcl_state;

			WHEN rti_pcl_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- idle sp
				left_ctrl <= sp_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				sp_ctrl <= latch_sp;
				-- pull pc low
				pc_ctrl <= pull_lo_pc;
				addr_ctrl <= pull_ad;
				dout_ctrl <= pc_lo_dout;
				next_state <= fetch_state;

				--
				-- here on interrupt
				-- iv register hold interrupt type
				--
			WHEN int_pcl_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write pc low
				addr_ctrl <= push_ad;
				dout_ctrl <= pc_lo_dout;
				next_state <= int_pch_state;

			WHEN int_pch_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write pc hi
				addr_ctrl <= push_ad;
				dout_ctrl <= pc_hi_dout;
				next_state <= int_iyl_state;

			WHEN int_iyl_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write iy low
				addr_ctrl <= push_ad;
				dout_ctrl <= iy_lo_dout;
				next_state <= int_iyh_state;

			WHEN int_iyh_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write iy hi
				addr_ctrl <= push_ad;
				dout_ctrl <= iy_hi_dout;
				next_state <= int_ixl_state;

			WHEN int_ixl_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write ix low
				addr_ctrl <= push_ad;
				dout_ctrl <= ix_lo_dout;
				next_state <= int_ixh_state;

			WHEN int_ixh_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write ix hi
				addr_ctrl <= push_ad;
				dout_ctrl <= ix_hi_dout;
				next_state <= int_acca_state;

			WHEN int_acca_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write acca
				addr_ctrl <= push_ad;
				dout_ctrl <= acca_dout;
				next_state <= int_accb_state;
			WHEN int_accb_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write accb
				addr_ctrl <= push_ad;
				dout_ctrl <= accb_dout;
				next_state <= int_cc_state;

			WHEN int_cc_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- decrement sp
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_sub16;
				cc_ctrl <= latch_cc;
				sp_ctrl <= load_sp;
				-- write cc
				addr_ctrl <= push_ad;
				dout_ctrl <= cc_dout;
				--
				-- XIRQ is level sensitive
				--
				IF (xirq = '1') AND (cc(XBIT) = '0') THEN
					iv_ctrl <= xirq_iv;
					next_state <= int_maskx_state;
				ELSE
					--
					-- IRQ is level sensitive
					--
					IF (irq = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= irq_iv;
						next_state <= int_maski_state;
					ELSIF (irq_ext3 = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= ext3_iv;
						next_state <= int_maski_state;
					ELSIF (irq_ext2 = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= ext2_iv;
						next_state <= int_maski_state;
					ELSIF (irq_ext1 = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= ext1_iv;
						next_state <= int_maski_state;
					ELSIF (irq_ext0 = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= ext0_iv;
						next_state <= int_maski_state;
					ELSE
						CASE op_code IS
							WHEN "00111110" => -- WAI (wait for interrupt)
								iv_ctrl <= latch_iv;
								next_state <= int_wai_state;
							WHEN "00111111" => -- SWI (Software interrupt)
								iv_ctrl <= swi_iv;
								next_state <= vect_hi_state;
							WHEN OTHERS => -- bogus interrupt (return)
								iv_ctrl <= latch_iv;
								next_state <= rti_state;
						END CASE;
					END IF;
				END IF;

			WHEN int_wai_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				count_ctrl <= reset_count;
				md_ctrl <= latch_md;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- enable interrupts
				left_ctrl <= sp_left;
				right_ctrl <= one_right;
				alu_ctrl <= alu_cli;
				cc_ctrl <= load_cc;
				sp_ctrl <= latch_sp;
				-- idle bus
				addr_ctrl <= idle_ad;
				dout_ctrl <= cc_dout;
				--
				-- XIRQ is level sensitive
				--
				IF (xirq = '1') AND (cc(XBIT) = '0') THEN
					iv_ctrl <= xirq_iv;
					next_state <= int_maskx_state;
				ELSE
					--
					-- IRQ is level sensitive
					--
					IF (irq = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= irq_iv;
						next_state <= int_maski_state;
					ELSIF (irq_ext3 = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= ext3_iv;
						next_state <= int_maski_state;
					ELSIF (irq_ext2 = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= ext2_iv;
						next_state <= int_maski_state;
					ELSIF (irq_ext1 = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= ext1_iv;
						next_state <= int_maski_state;
					ELSIF (irq_ext0 = '1') AND (cc(IBIT) = '0') THEN
						iv_ctrl <= ext0_iv;
						next_state <= int_maski_state;
					ELSE
						iv_ctrl <= latch_iv;
						next_state <= int_wai_state;
					END IF;
				END IF;

			WHEN int_maskx_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- Mask IRQ
				left_ctrl <= sp_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_sex;
				cc_ctrl <= load_cc;
				sp_ctrl <= latch_sp;
				-- idle bus cycle
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= vect_hi_state;

			WHEN int_maski_state =>
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- Mask IRQ
				left_ctrl <= sp_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_sei;
				cc_ctrl <= load_cc;
				sp_ctrl <= latch_sp;
				-- idle bus cycle
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= vect_hi_state;

			WHEN OTHERS => -- halt on undefine states
				-- default
				acca_ctrl <= latch_acca;
				accb_ctrl <= latch_accb;
				ix_ctrl <= latch_ix;
				iy_ctrl <= latch_iy;
				sp_ctrl <= latch_sp;
				pc_ctrl <= latch_pc;
				md_ctrl <= latch_md;
				iv_ctrl <= latch_iv;
				count_ctrl <= reset_count;
				op_ctrl <= latch_op;
				pre_ctrl <= latch_pre;
				ea_ctrl <= latch_ea;
				-- do nothing in ALU
				left_ctrl <= acca_left;
				right_ctrl <= zero_right;
				alu_ctrl <= alu_nop;
				cc_ctrl <= latch_cc;
				-- idle bus cycle
				addr_ctrl <= idle_ad;
				dout_ctrl <= md_lo_dout;
				next_state <= halt_state;
		END CASE;
	END PROCESS;

	--------------------------------
	--
	-- state machine
	--
	--------------------------------

	change_state : PROCESS (clk, rst, state)
	BEGIN
		IF rst = '1' THEN
			state <= reset_state;
		ELSIF clk'event AND clk = '0' THEN
			state <= next_state;
		END IF;
	END PROCESS;
	-- output

END;
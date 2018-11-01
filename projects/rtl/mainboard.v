module mainboard(
	input			pclk,
	
	input	[3:0]	keys,
	input	[3:0]	switches,
	
	output	[8:0]	seg_led_h,
	output	[8:0]	seg_led_l,
	output	[2:0]	rgb1,
	output	[2:0]	rgb2,
	output	[7:0]	leds,
	
	output			spi_cs,
	output			spi_sck,
	output			spi_so,
	input			spi_si,
	
	inout			i2c_scl,
	inout			i2c_sda,
	
	inout	[27:0]	gpio,
	
	input			rxd,
	output			txd
);
	parameter OSC_CLOCK = 12000000;
	parameter CPU_CLOCK = 12000000;
	parameter LED_REFRESH_CLOCK = 50;
	
	parameter LED_DIV_PERIOD = (OSC_CLOCK / LED_REFRESH_CLOCK) / 2;

	wire			clk_in = pclk;
	wire			sys_clk = pclk;
	wire			cpu_clk = pclk;
	reg				sys_res = 1;
	reg		[1:0]	led_anode;
	reg		[24:0]	led_cnt;
	reg		[3:0]	sys_res_delay = 4'b1000;
	
	wire			sys_vma;
	wire	[15:0]	AD;
	wire	[7:0]	DI;
	wire	[7:0]	DO;
	wire			simpleio_irq;
	wire			uartio_irq;
	wire			gpio_irq;


	always @ (posedge clk_in)
	begin   if (sys_res) led_anode <= 2'b01;
		else begin
			if (led_cnt == (LED_DIV_PERIOD - 1)) begin
				led_anode <= ~led_anode;
				led_cnt <= 0;
			end else led_cnt <= led_cnt + 1'b1;
		end
	end

	always @ (posedge sys_clk or negedge keys[3])
	begin
		if (!keys[3]) begin
			sys_res <= 1;
			sys_res_delay <= 4'b1000;
		end else begin
			if (sys_res_delay == 4'b0000) begin
				sys_res <= 0;
			end else sys_res_delay <= sys_res_delay - 4'b0001;
		end
	end

	assign seg_led_h[8] = led_anode[1];
	assign seg_led_l[8] = led_anode[0];

	/*
		Mapping IO
	 */

	wire DS0 = (AD[15:5] == 11'b11100110000); // $E600
	wire DS1 = (AD[15:5] == 11'b11100110001); // $E620
	wire DS2 = (AD[15:5] == 11'b11100110010); // $E640
	wire DS3 = (AD[15:5] == 11'b11100110011); // $E660
	wire DS4 = (AD[15:5] == 11'b11100110100); // $E680
	wire DS5 = (AD[15:5] == 11'b11100110101); // $E6A0
	wire DS6 = (AD[15:5] == 11'b11100110110); // $E6C0
	wire DS7 = (AD[15:5] == 11'b11100110111); // $E6E0

	/*
		Rot8x8
	 */

	wire en_rot8x8 = DS3 && (AD[4] == 1'b1); // $E670
	wire cs_rot8x8 = en_rot8x8 && sys_vma;
	wire [7:0] rot8x8d;
	rot8x8 rot8x8_1 (
		.clk(sys_clk),
		.rst(sys_res),
		.AD(AD[2:0]),
		.DI(DO),
		.DO(rot8x8d),
		.rw(sys_rw),
		.cs(cs_rot8x8)
	);

	/*
		Simple IO
	 */

	wire en_simpleio = DS5 && (AD[4] == 1'b0); // $E6A0
	wire cs_simpleio = en_simpleio && sys_vma;
	wire [7:0] simpleiod;
	simpleio simpleio1 (
		.clk(sys_clk),
		.rst(sys_res),
		.irq(simpleio_irq),
		.AD(AD[3:0]),
		.DI(DO),
		.DO(simpleiod),
		.rw(sys_rw),
		.cs(cs_simpleio),
		.clk_in(clk_in),
		.leds(leds),
		.led7hi(seg_led_h[7:0]),
		.led7lo(seg_led_l[7:0]),
		.rgb1(rgb1),
		.rgb2(rgb2),
		.switches(switches),
		.keys(keys)
	);

	/*
		UART IO
	 */

	wire en_uartio = DS5 && (AD[4] == 1'b1); // $E6B0
	wire cs_uartio = en_uartio && sys_vma;
	wire [7:0] uartiod;
	uartio uartio1 (
		.clk(sys_clk),
		.rst(sys_res),
		.irq(uartio_irq),
		.AD(AD[2:0]),
		.DI(DO),
		.DO(uartiod),
		.rw(sys_rw),
		.cs(cs_uartio),
		.clk_in(clk_in),
		.rxd(rxd),
		.txd(txd),
		.ps2clk(1'b0),
		.ps2dat(1'b0)
	);

	/*
		SPI IO
	 */

	wire en_spiio = DS6 && (AD[4] == 1'b0); // $E6C0
	wire cs_spiio = en_spiio && sys_vma;
	wire [7:0] spiiod;
	spiio spi_impl(
		.clk(sys_clk),
		.rst(sys_res),
		.AD(AD[2:0]),
		.DI(DO),
		.DO(spiiod),
		.rw(sys_rw),
		.cs(cs_spiio),
		
		.clk_in(sys_clk),
		
		.mosi(spi_so),
		.msck(spi_sck),
		.miso(spi_si),
		.mss(spi_cs)
	);

	/*
		GPIO
	 */

	wire en_gpio = DS6 && (AD[4] == 1'b1); // $E6D0
	wire cs_gpio = en_gpio && sys_vma;
	wire [7:0] gpiod;
	gpio gpio1 (
		.clk(sys_clk),
		.rst(sys_res),
		.irq(gpio_irq),
		.AD(AD[2:0]),
		.DI(DO),
		.DO(gpiod),
		.rw(sys_rw),
		.cs(cs_gpio),
		.gpio(gpio)
	);

	wire en_brom = (AD[15:11] == 5'b11111);
	wire cs_brom = en_brom && sys_vma;
	wire [7:0] bromd;
	mcu_rom brom (
		.OutClock(sys_clk),
		.Reset(sys_res),
		.OutClockEn(cs_brom),
		.Address(AD[7:0]),
		.Q(bromd)
	);

	wire [15:0] bram_ad = AD;
	wire en_bram  = bram_ad[15:12] == 4'b0000;
	wire cs_bram = en_bram && sys_vma;
	wire [7:0] bramd;
	mcu_ram bram (
		.Clock(sys_clk),
		.ClockEn(cs_bram),
		.Reset(sys_res),
		.WE((~sys_clk) & (~sys_rw)),
		.Address( bram_ad[11:0]),
		.Data(DO),
		.Q(bramd)
	);

	assign DI = 
				en_bram		? bramd:
				en_brom		? bromd:
				en_simpleio	? simpleiod:
				en_uartio	? uartiod:
				en_spiio 	? spiiod:
				en_gpio		? gpiod:
				en_rot8x8   ? rot8x8d:
				8'b11111111;

	wire cpu_irq = simpleio_irq | uartio_irq;// | gpio_irq;
	cpu11 cpu11impl(
		.clk(cpu_clk),
		.rst(sys_res),
		.rw(sys_rw),
		.vma(sys_vma),
		.address(AD),
		.data_in(DI),
		.data_out(DO),
		.irq(cpu_irq),
		.xirq(1'b0),
		.irq_ext3(1'b0),
		.irq_ext2(1'b0),
		.irq_ext1(1'b0),
		.irq_ext0(1'b0)
	);

endmodule

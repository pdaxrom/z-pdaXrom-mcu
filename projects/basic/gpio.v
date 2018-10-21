/*
	GPIO interface
	
	$00..$03 - GPIO 28 bit
	$04..$07 - GPIO Direction bits (1 - out, 0 - in)
 */
module gpio (
	input wire clk,
	input wire rst,
	input wire [3:0] AD,
	input wire [7:0] DI,
	output reg [7:0] DO,
	input wire rw,
	input wire cs,
	output wire irq,
	
	inout wire [27:0] gpio
);
	reg [27:0] gpio_dr;
	reg [27:0] gpio_out;

	assign gpio[0] = gpio_dr[0] ? gpio_out[0] : 1'bZ;
	assign gpio[1] = gpio_dr[1] ? gpio_out[1] : 1'bZ;
	assign gpio[2] = gpio_dr[2] ? gpio_out[2] : 1'bZ;
	assign gpio[3] = gpio_dr[3] ? gpio_out[3] : 1'bZ;
	assign gpio[4] = gpio_dr[4] ? gpio_out[4] : 1'bZ;
	assign gpio[5] = gpio_dr[5] ? gpio_out[5] : 1'bZ;
	assign gpio[6] = gpio_dr[6] ? gpio_out[6] : 1'bZ;
	assign gpio[7] = gpio_dr[7] ? gpio_out[7] : 1'bZ;
	
	assign gpio[8] = gpio_dr[8] ? gpio_out[8] : 1'bZ;
	assign gpio[9] = gpio_dr[9] ? gpio_out[9] : 1'bZ;
	assign gpio[10] = gpio_dr[10] ? gpio_out[10] : 1'bZ;
	assign gpio[11] = gpio_dr[11] ? gpio_out[11] : 1'bZ;
	assign gpio[12] = gpio_dr[12] ? gpio_out[12] : 1'bZ;
	assign gpio[13] = gpio_dr[13] ? gpio_out[13] : 1'bZ;
	assign gpio[14] = gpio_dr[14] ? gpio_out[14] : 1'bZ;
	assign gpio[15] = gpio_dr[15] ? gpio_out[15] : 1'bZ;
	
	assign gpio[16] = gpio_dr[16] ? gpio_out[16] : 1'bZ;
	assign gpio[17] = gpio_dr[17] ? gpio_out[17] : 1'bZ;
	assign gpio[18] = gpio_dr[18] ? gpio_out[18] : 1'bZ;
	assign gpio[19] = gpio_dr[19] ? gpio_out[19] : 1'bZ;
	assign gpio[20] = gpio_dr[20] ? gpio_out[20] : 1'bZ;
	assign gpio[21] = gpio_dr[21] ? gpio_out[21] : 1'bZ;
	assign gpio[22] = gpio_dr[22] ? gpio_out[22] : 1'bZ;
	assign gpio[23] = gpio_dr[23] ? gpio_out[23] : 1'bZ;
	
	assign gpio[24] = gpio_dr[24] ? gpio_out[24] : 1'bZ;
	assign gpio[25] = gpio_dr[25] ? gpio_out[25] : 1'bZ;
	assign gpio[26] = gpio_dr[26] ? gpio_out[26] : 1'bZ;
	assign gpio[27] = gpio_dr[27] ? gpio_out[27] : 1'bZ;

	always @ (posedge clk) begin
		if (rst) begin
			gpio_dr <= 0;
			gpio_out <= 0;
		end else begin
			if (cs) begin
				if (rw) begin
					case (AD[2:0])
					4'b000: DO <= { 4'b0, (gpio[27:24] & ~gpio_dr[27:24]) | gpio_out[27:24] };
					4'b001: DO <= (gpio[23:16] & ~gpio_dr[23:16]) | gpio_out[23:16];
					4'b010: DO <= (gpio[15:8]  & ~gpio_dr[15:8]) | gpio_out[15:8];
					4'b011: DO <= (gpio[7:0]   & ~gpio_dr[7:0] ) | gpio_out[7:0];
					4'b100: DO <= { 4'b0, gpio_dr[27:24] };
					4'b101: DO <= gpio_dr[23:16];
					4'b110: DO <= gpio_dr[15:8];
					4'b111: DO <= gpio_dr[7:0];
					endcase
				end else begin
					case (AD[2:0])
					4'b000: gpio_out[27:24] <= DI[3:0];
					4'b001: gpio_out[23:16] <= DI;
					4'b010: gpio_out[15:8]  <= DI;
					4'b011: gpio_out[7:0]   <= DI;
					4'b100: gpio_dr[27:24]  <= DI[3:0];
					4'b101: gpio_dr[23:16]  <= DI;
					4'b110: gpio_dr[15:8]   <= DI;
					4'b111: gpio_dr[7:0]    <= DI;
					endcase
				end
			end
		end
	end


endmodule

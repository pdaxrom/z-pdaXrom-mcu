/*
	Rotation 8x8 interface
	
	$00..$07 - -W input
	$00..$07 - R- output
 */
module rot8x8 (
	input wire clk,
	input wire rst,
	input wire [2:0] AD,
	input wire [7:0] DI,
	output reg [7:0] DO,
	input wire rw,
	input wire cs
);
	reg [7:0] data0;
	reg [7:0] data1;
	reg [7:0] data2;
	reg [7:0] data3;
	reg [7:0] data4;
	reg [7:0] data5;
	reg [7:0] data6;
	reg [7:0] data7;

	always @ (posedge clk) begin
		if (rst) begin
			data0 <= 8'b0;
			data1 <= 8'b0;
			data2 <= 8'b0;
			data3 <= 8'b0;
			data4 <= 8'b0;
			data5 <= 8'b0;
			data6 <= 8'b0;
			data7 <= 8'b0;
		end else begin
			if (cs) begin
				if (rw) begin
					case (AD[2:0])
					4'b000: DO <= { data7[7], data6[7], data5[7], data4[7], data3[7], data2[7], data1[7], data0[7] };
					4'b001: DO <= { data7[6], data6[6], data5[6], data4[6], data3[6], data2[6], data1[6], data0[6] };
					4'b010: DO <= { data7[5], data6[5], data5[5], data4[5], data3[5], data2[5], data1[5], data0[5] };
					4'b011: DO <= { data7[4], data6[4], data5[4], data4[4], data3[4], data2[4], data1[4], data0[4] };
					4'b100: DO <= { data7[3], data6[3], data5[3], data4[3], data3[3], data2[3], data1[3], data0[3] };
					4'b101: DO <= { data7[2], data6[2], data5[2], data4[2], data3[2], data2[2], data1[2], data0[2] };
					4'b110: DO <= { data7[1], data6[1], data5[1], data4[1], data3[1], data2[1], data1[1], data0[1] };
					4'b111: DO <= { data7[0], data6[0], data5[0], data4[0], data3[0], data2[0], data1[0], data0[0] };
					endcase
				end else begin
					case (AD[2:0])
					4'b000: data0 <= DI;
					4'b001: data1 <= DI;
					4'b010: data2 <= DI;
					4'b011: data3 <= DI;
					4'b100: data4 <= DI;
					4'b101: data5 <= DI;
					4'b110: data6 <= DI;
					4'b111: data7 <= DI;
					endcase
				end
			end
		end
	end


endmodule

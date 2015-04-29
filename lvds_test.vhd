library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- modulo
use IEEE.NUMERIC_STD.ALL;

--use work.fontRom.all;

entity sync_test is
     Port (     clkExtOsc : in STD_LOGIC;
				ledOut : out STD_LOGIC_VECTOR(7 downto 0);
                sw : in STD_LOGIC_VECTOR (2 downto 0);
                
				-- ODD columns
			   CK1IN : out  STD_LOGIC;
			   RXIN2 : out  STD_LOGIC;
			   RXIN1 : out  STD_LOGIC;
			   RXIN0 : out  STD_LOGIC;
			   
			   -- Even columns
			   ECK1IN : out  STD_LOGIC;
			   ERXIN2 : out  STD_LOGIC;
			   ERXIN1 : out  STD_LOGIC;
			   ERXIN0 : out  STD_LOGIC;
			   
			   -- Logic analyzer debug outputs
			   LCK1IN : out  STD_LOGIC;
			   LRXIN2 : out  STD_LOGIC;
			   LRXIN1 : out  STD_LOGIC;
			   LRXIN0 : out  STD_LOGIC;
			   LTRIG  : out  STD_LOGIC;
			   LTRIG2  : out  STD_LOGIC;
				
				clkOut : out STD_LOGIC				);
end sync_test;

architecture Behavioral of sync_test is

	SIGNAL  clk  : STD_LOGIC;
	signal led : STD_LOGIC_VECTOR (7 downto 0);
	signal CLK_DIV : std_logic_vector (8 downto 0);
	
		-- colors
	signal red : std_logic_vector(5 downto 0) := "000000";
	signal green : std_logic_vector(5 downto 0) := "000000";
	signal blue : std_logic_vector(5 downto 0) := "000000";

	-- which slot are we in right now?
	signal slot : integer range 0 to 6;
	
	-- control signals
	signal hsync : std_logic := '0';
	signal vsync : std_logic := '0';
	signal dataenable : std_logic := '0';
	
	-- display parameters
	constant htotal : integer := 980; -- screen size, with back porch ;1920+40 / 2
	constant hfront : integer := 12; -- front porch 24 / 2
	constant hactive : integer := 960; -- display size 1920/2
	signal hcurrent : integer range 0 to htotal := 0;
	constant vtotal : integer := 1226; -- screen size, with back porch 1200+26
	constant vfront : integer := 3; -- front porch 3 
	constant vactive : integer := 1200; -- display size
	signal vcurrent : integer range 0 to vtotal := 0;
	
	-- the signals holding the data to be sent to the lcd on each slot.
	-- this is hardwired on the RGB, hsync and vsync signals.
	signal RX0DATA : std_logic_vector(0 to 6) := "0000000";
	signal RX1DATA : std_logic_vector(0 to 6) := "0000000";
	signal RX2DATA : std_logic_vector(0 to 6) := "0000000";
	constant CK1DATA : std_logic_vector(0 to 6) := "1100011"; -- this is per spec, the clock 
																					-- is always the same 
																					
																					
	-- Moving green bar
	signal gbarpos : integer range 0 to vtotal := 0;
	
	--signal color_cur : integer range 0 to 2 := 0;
		
	component pll
		port (CLKI: in  std_logic; CLKOP: out  std_logic);
	end component;
  
	COMPONENT debounce
		PORT( clk : IN std_logic; button : IN std_logic; result : OUT std_logic );
	END COMPONENT;
	
		-- Clock multiplexer
	component DCMA
		port( CLK0 : in std_logic; 
		CLK1 : in std_logic;
		SEL : in std_logic;
		DCMOUT : out std_logic);
	end component;
	
	
  --internal oscillator
   COMPONENT OSCH
      GENERIC(
            NOM_FREQ: string := "2.08");
      PORT( 
            STDBY    : IN  STD_LOGIC;
            OSC      : OUT STD_LOGIC;
            SEDSTDBY : OUT STD_LOGIC);
   END COMPONENT;
   SIGNAL  clkRC  : STD_LOGIC;
   SIGNAL  clkPLL  : STD_LOGIC;

	 signal swDebounced : STD_LOGIC_VECTOR (2 downto 0);
begin

	-- Clock multiplexer
	I1: DCMA
	port map (CLK0 => clkRC, CLK1 => clkPLL,	SEL => '1',	DCMOUT => clk);

   --internal oscillator
   OSCInst0: OSCH
      GENERIC MAP (NOM_FREQ  => "2.08")
      PORT MAP (STDBY => '0', OSC => clkRC, SEDSTDBY => OPEN);

	myPll : pll	port map (CLKI=>clkExtOsc, CLKOP=> clkPLL);
	Inst_debounce: debounce PORT MAP( clk => clkExtOsc, button => sw(0), result => swDebounced(0) );	
	Inst_debounc2: debounce PORT MAP( clk => clkExtOsc, button => sw(1), result => swDebounced(1) );		

	--led(2 downto 0) <= sw(2 downto 0);
	--led(3) <= swDebounced(0);
	--led(4) <= swDebounced(2);
				  
	ledOut <= not led;
	
	--clkOut <= CLK_DIV(8);
	--led(5) <= CLK_DIV(8);
	
	led <= std_logic_vector( to_unsigned(gbarpos, 8) );
	
	
	-- data enable: should be high when the data is valid for display
	dataenable <= vsync and hsync;

	-- RX2DATA is (DE, vsync, hsync, blue[5:2])
	RX2DATA(0) <= dataenable;
	RX2DATA(1) <= vsync;
	RX2DATA(2) <= hsync;
	RX2DATA(3 to 6) <= blue(5 downto 2);-- when dataenable else "0000";

	-- RX1DATA is (blue[1:0], green[5:1])
	RX1DATA(0 to 1) <= blue(1 downto 0);-- when dataenable else "00";
	RX1DATA(2 to 6) <= green(5 downto 1);--  when dataenable else "00000";

	-- RX1DATA is (green[0], red[5:0])
	RX0DATA(0) <= green(0);--  when dataenable else '0';
	RX0DATA(1 to 6) <= red(5 downto 0);--  when dataenable else "000000";

-- RX2DATA synchro data

	-- connect signals with the appropriate slot
	RXIN0 <= RX0DATA(slot);
	RXIN1 <= RX1DATA(slot);
	RXIN2 <= RX2DATA(slot);
	CK1IN <= CK1DATA(slot);
	
	-- dual channel output
	ERXIN0 <= RXIN0;
	ERXIN1 <= RXIN1;
	ERXIN2 <= RXIN2;
	ECK1IN <= CK1IN;
	
	-- debug logic analyzer
	
	LCK1IN  <= CK1IN;
	LRXIN2 <= RXIN2;
	LRXIN1 <= RXIN1;
	LRXIN0 <= RXIN0;
	LTRIG  <= vsync;
	LTRIG2 <= hsync;
	
	
	process (clk) is
	begin
	
		--if rising_edge(clk) then
		--	CLK_DIV <= CLK_DIV + '1';
		--end if;
		
		if rising_edge(clk) then
		
		if hcurrent < hfront or (hcurrent >= (hfront+hactive)) then
			hsync <= '0';
		else
			hsync <= '1';
		end if;
		
		if vcurrent < vfront or (vcurrent >= (vfront+vactive)) then
			vsync <= '0';
		else
			vsync <= '1';
		end if;
		
		if slot = 6 then
			-- this is the last slot, wrap around
			slot <= 0;
			
				if (hcurrent > gbarpos and hcurrent < (gbarpos + 64)) then -- or (hcurrent > 800 and hcurrent < 1200) then -- 
					red <= "111111";
					--red <= std_logic_vector( to_unsigned(hcurrent - gbarpos, 6) );
				else 
					if hcurrent > 500 and hcurrent < 525 and vcurrent > 100 and vcurrent < 150 then
						red <= "111111";
					else
						if hcurrent = 100 and vcurrent = 100 then
							red <= "111111";
						else
							red <= "000000";
						end if;
					end if;
				end if;
				
				
				
				--if hcurrent > 100 and hcurrent < 150 and vcurrent > 100 and vcurrent < 150 then
					--blue <= "000011";
				--else
					--blue <= "000000";
				--end if;
			
			-- if this is the last pixel in the line, wrap around
			if hcurrent = htotal then
				hcurrent <= 0;
				
				if vcurrent = gbarpos then
					green <= "111111";
				else
					green <= "000000";
				end if;
				

				
				--if blue = "000000" then
					--blue <= "111000";
					--if green = "000000" then
						--green <= "111000";
						--if red = "000000" then
							--red <= "111000";
						--else
							--red <= red - 8;
						--end if;
					--else
						--green <= green - 8;
					--end if;
				--else
					--blue <= blue - 8;
				--end if;
				
				-- if this is the last line in the screen, wrap around.
				if vcurrent = vtotal then
					vcurrent <= 0;
					
					if swDebounced(0) = '1' then
						gbarpos <= gbarpos + 1;
					end if;
					
					if swDebounced(1) = '1' then
						gbarpos <= gbarpos - 1;
					end if;
					
					--gbarpos <= gbarpos + 2;
					
					--if gbarpos >= vtotal then
						--gbarpos <= 0;
					--end if;
					
					-- new screen, reset the colors
					--red <= "111000";
					--green <= "111000";
					--blue <= "111000";
				else
					vcurrent <= vcurrent + 1;
				end if;
			else
				hcurrent <= hcurrent + 1;
			end if;
			
		else
			slot <= slot + 1;
		end if;
		
	end if;
	
	end process;

end Behavioral;
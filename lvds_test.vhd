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
	
	signal nextColor : std_logic_vector(17 downto 0) := "000000000000000000";

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
	
	
	subtype subCharacterItem is integer range 0 to 1200;
type typeCharArray is array (integer range 0 to 3) of subCharacterItem;
--CONSTANT characterArray:  typeCharArray := (
shared variable segmentOffsetX:  typeCharArray := (20, 230, 500, 710);
	
	
-- 7 segment decoder array	
--subtype subSevenSegType is std_logic_vector(6 downto 0);
--type typeSevenSeg is array (integer range 0 to 9) of subSevenSegType;
--shared variable sevenSegment:  typeSevenSeg := ("0000001","1001111","0010010","0000110","1001100","0100100","0100000","0001111","0000000","0000100");


-- Color output from every segment
subtype subColorOutType is std_logic_vector(17 downto 0);
type typeColorOut is array (integer range 0 to 3) of subColorOutType;
shared variable colorOut:  typeColorOut;




-- array of 4 number on display
subtype subDisplayNumberType is integer range 0 to 9;
type typeDisplayNumber is array (integer range 0 to 3) of subDisplayNumberType;
shared variable displayNumber:  typeDisplayNumber := (0,8,3,0);


-- divider 200MHz to 1 second
	signal clockMinute: STD_LOGIC;
	signal clockSecond: STD_LOGIC;
	signal counterMinute : integer range 0 to 8000000*61 := 0;
	signal counterSecond : integer range 0 to 8000000+1 := 0;

	-- parameterized module component declaration
component ROM
    port (Address: in  std_logic_vector(3 downto 0); 
        OutClock: in  std_logic; OutClockEn: in  std_logic; 
        Reset: in  std_logic; Q: out  std_logic_vector(7 downto 0));
end component;
		
	component pll
		port (CLKI: in  std_logic; CLKOP: out  std_logic);
	end component;
  
	COMPONENT debounce
		PORT( clk : IN std_logic; button : IN std_logic; result : OUT std_logic );
	END COMPONENT;
	
	COMPONENT digit7seg
		PORT( hcurrent : IN integer; vcurrent : IN integer; offsetX : IN integer; offsetY : IN integer; dispNumber : IN integer; display : OUT std_logic ; colorIn : in std_logic_vector(17 downto 0); colorOut : out std_logic_vector(17 downto 0));
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
	 
	 signal romAddr : STD_LOGIC_VECTOR (3 downto 0) := "0000";
	 signal romOut : STD_LOGIC_VECTOR (7 downto 0);
	 signal tempRomAddr : STD_LOGIC_VECTOR (9 downto 0);
	 signal tempGbarPos : STD_LOGIC_VECTOR (9 downto 0);
	 signal tempFlag : STD_LOGIC_VECTOR (9 downto 0);
	 
	 signal digitDisplay : std_logic_vector(3 downto 0);
begin

MyROM : ROM
    port map (Address(3 downto 0)=> romAddr, OutClock=>clkPLL, OutClockEn=>'1', 
        Reset=> '0', Q(7 downto 0)=> romOut);

	-- Clock multiplexer
	--I1: DCMA
	--port map (CLK0 => clkRC, CLK1 => clkPLL,	SEL => '1',	DCMOUT => clk);

	

   --internal oscillator
   OSCInst0: OSCH
      GENERIC MAP (NOM_FREQ  => "2.08")
      PORT MAP (STDBY => '0', OSC => clkRC, SEDSTDBY => OPEN);

	myPll : pll	port map (CLKI=>clkExtOsc, CLKOP=> clkPLL);
	
	clk <= clkPLL;
	
	Inst_debounce: debounce PORT MAP( clk => clkExtOsc, button => sw(0), result => swDebounced(0) );	
	Inst_debounc2: debounce PORT MAP( clk => clkExtOsc, button => sw(1), result => swDebounced(1) );		

	digit0: digit7seg PORT MAP(vcurrent => vcurrent, hcurrent => hcurrent, offsetX => 20 , offsetY => 50, dispNumber => displayNumber(0), display => digitDisplay(0), colorIn => "100000111111000000", colorOut => colorOut(0));
	digit1: digit7seg PORT MAP(vcurrent => vcurrent, hcurrent => hcurrent, offsetX => 230, offsetY => 50, dispNumber => displayNumber(1), display => digitDisplay(1), colorIn => "000000111111100000", colorOut => colorOut(1));
	digit2: digit7seg PORT MAP(vcurrent => vcurrent, hcurrent => hcurrent, offsetX => 500, offsetY => 50, dispNumber => displayNumber(2), display => digitDisplay(2), colorIn => "100000111111100000", colorOut => colorOut(2));
	digit3: digit7seg PORT MAP(vcurrent => vcurrent, hcurrent => hcurrent, offsetX => 710, offsetY => 50, dispNumber => displayNumber(3), display => digitDisplay(3), colorIn => "000000111111000000", colorOut => colorOut(3));

	--led(2 downto 0) <= sw(2 downto 0);
	--led(3) <= swDebounced(0);
	--led(4) <= swDebounced(2);
				  
	ledOut <= not led;
	
	--clkOut <= CLK_DIV(8);
	--led(5) <= CLK_DIV(8);
	
	--led <= std_logic_vector( to_unsigned(gbarpos, 8) );
	
	
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
	
	
	
	led(0) <= clockSecond;
	led(1) <= clockMinute;
	
	
	process (slot) is
		variable offsetX : integer range 0 to 1200;
		variable digitValue : std_logic_vector(6 downto 0);
		variable actColor : std_logic_vector(17 downto 0);
		

	begin
		if (slot = 5) then
			nextColor <= "000000000000000000";
			
			if vcurrent = gbarpos then
					nextColor <= "000000111111000000";
			end if;
			
			actColor := "000000111111000000";
			
			--nextColor <= colorOut(0) OR colorOut(1) OR colorOut(2) OR colorOut(3);
	
			if( not (digitDisplay = "0000") ) then
				nextColor <= actColor;
				--nextColor <= colorOut(0) OR colorOut(1) OR colorOut(2) OR colorOut(3);
			end if;
		
		end if;
		
	end process;
	
	
	process (clkExtOsc, sw(0)) is
	begin
	
		if( sw(0) = '1' ) then
			counterMinute <= 0;
			counterSecond <= 0;
			clockSecond <= '0';
			clockMinute <= '0';
		else
	
			if rising_edge(clkExtOsc) then
				if (counterMinute = 8000000*60) then
					clockMinute <= NOT(clockMinute);
					counterMinute <= 0;
				else
					counterMinute <= counterMinute + 1;
				end if;
				
				if (counterSecond = 8000000) then
					clockSecond <= NOT(clockSecond);
					counterSecond <= 0;
				else
					counterSecond <= counterSecond + 1;
				end if;
			end if;
		
		
		end if;
	end process;
	
	
	process (clockMinute, sw(0)) is
	begin
	
				if( sw(0) = '1' ) then
						displayNumber(0) := 0;						
						displayNumber(1) := 8;
						displayNumber(2) := 3;						
						displayNumber(3) := 0;		
			else
			
		if rising_edge(clockMinute) then
		

		
			if( displayNumber(3) = 0 ) then -- minuty
				if( displayNumber(2) = 0 ) then -- desitky minut
					if( displayNumber(1) = 0 ) then -- hodiny
					
					else
						displayNumber(1) := displayNumber(1)  - 1; --dec hodiny
						displayNumber(2) := 5;						--desitky minut
						displayNumber(3) := 9;						--minuty
					end if;
				else
					-- sub minuty
					displayNumber(3) := 9;
					displayNumber(2) := displayNumber(2)  - 1;
				end if;
			else
				-- sub minuty
				displayNumber(3) := displayNumber(3)  - 1;
			end if;
		
		end if;
	
			
			end if;
	
	end process;
	
	
	process (clk) is
	begin
	
		
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
			green <= nextColor(17 downto 12);
			red <= nextColor(11 downto 6);
			blue <= nextColor(5 downto 0);
			
			-- if this is the last pixel in the line, wrap around
			if hcurrent = htotal then
				hcurrent <= 0;
				
				
				-- if this is the last line in the screen, wrap around.
				if vcurrent = vtotal then
					vcurrent <= 0;
					
					if swDebounced(0) = '1' then
						gbarpos <= gbarpos + 1;
					end if;
					
					if swDebounced(1) = '1' then
						gbarpos <= gbarpos - 1;
					end if;

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
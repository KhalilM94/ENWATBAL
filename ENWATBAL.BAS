'ENWAT2.BAS is the second module in ENWATBAL.BAS

'Use BYVAL for Microsoft Prof. BASIC ver. 7.X but eliminate it for
'QuickBASIC ver. 4.5:
'DECLARE FUNCTION ARCOS! (BYVAL xdum!)
'DECLARE FUNCTION ARSIN! (BYVAL xdum!)
'DECLARE FUNCTION AFGEN! (Table!(), BYVAL Xval!)

DECLARE FUNCTION ARCOS! (xdum!)
DECLARE FUNCTION ARSIN! (xdum!)
DECLARE FUNCTION AFGEN! (Table!(), Xval!)

DECLARE SUB SetEpson ()
DECLARE SUB RedirectOutput ()

'Variables shared with module ENWAT2.BAS:
'Strings containing file names for input data files:
COMMON InfoFile$    'This file contains file names for input data files.
COMMON IrrPrecip$   'Irrigation & precipitation data.
COMMON DripFile$    'Drip irrigation rate and time data.
COMMON Constants$   'Constants
COMMON AfGenFile$   'Tables of functional relationships.
COMMON Meteo$       'Weather data with periodicity less than 1 day.
COMMON Plant$       'Daily weather & plant growth data.
COMMON InitDayFile$ 'Soil layers & their water content & temperature, 1st day.

COMMON FErr%     'Number of error when opening file, related to file name.
COMMON Mode2$    'Mode to open output files (O=new file, A=append to old file.
COMMON Y$        'Redirection code, F or f=file, P or p=printer
COMMON HourlyOutputFlag 'If >0 then defines period in hours for output, otherwise only daily output.
'Period of averaged input data and offset, from midnight, of center of 1st mean:
COMMON Period    'Period of weather data that is less than daily [h].
COMMON Offset    'Offset of start of weather data from midnight [h].
COMMON MIndex%   '=24/Period, index to array holding weather data.
COMMON DebugDat(), di%   'Array holding debugging variables & index to array.
COMMON ErrCode  'Returned from IMPLx if error occurred.  Acted on in main program:
COMMON EndDay%  'Simulation ends at midnight of this day of year.
COMMON EndYear% 'Simulation ends in this year.
COMMON sYear%   'Year that simulation starts. File extension for Plant$.

'Constants in Constants$. See subprogram Getconstants:
COMMON Zo       'Surface roughness length (m).
COMMON Z        'Reference elevation (m) for measurement of wind speed,
		'air temperature and dew point temperature.
COMMON WPCrMx   'Maximum canopy water potential (m).
COMMON SRCr     'Specific crop hydraulic resistance (s).
COMMON LowBoundGrad  'Gradient for water flux - lower soil boundary condition.
COMMON ZeroCdNum% 'Day of year on which to invoke impeding layer.
COMMON DripFlag%     '>0 if a buried drip emitter exists.
COMMON DetCap   'Detention capacity for ponded water (m).
COMMON Lat      'Latitude in degrees.
COMMON AvBarP   'Average barometric pressure (mbar)
COMMON TStepL, TStepH 'Lower & upper limits of time step (s).
COMMON SatCon     'Saturated conductivity for surface soil layer (m/s).

COMMON StartDay%  'Starting day of year. Equals sDay% at start of simulation
    'but changes to day of restarting if simulation is stopped and restarted.
COMMON sDay%      'Starting day of year for simulation.
COMMON WMode%   'Code for source of weather data. Daily data are in file PLANT$
		'and data on intervales less than 1 d are in file METEO$.
		'daily (=0) data or data on intervals less than 1 d (=1).
COMMON Restart$, path$, dnum%, Hl, LH
COMMON cdnum%   'Current day of year.
COMMON TStep    'Current time step (s).
COMMON TimeS    'Time in seconds since midnight of current day.
COMMON sTime    'Time in decimal hours since midnight of current day.
COMMON hTime    'Cumulative time in decimal hours since simulation started.

COMMON EvTr     'Evapotranspiration rate (m/s).
COMMON TrC      'Transpiration rate (m/s).
COMMON LTr      'Transpiration rate (W/m^2).
COMMON Evs      'Rate of evaporation from soil surface (m/s).
COMMON LEvS     'Rate of evaporation from soil surface (W/m^2).
COMMON Infilt   'Infiltration rate (m/s).
COMMON GR       'Solar radiation (W/m^2).
COMMON Ta       'Air temperature (C).
COMMON Tl       'Canopy (big leaf) temperature (C).
COMMON TsBf     'Soil surface temperature for previous time step (C).
COMMON Ra       'Aerodynamic resistance to sensible & latent heat fluxes (s/m).
COMMON Rl       'Big leaf epidermal resistance (s/m).
COMMON Rs       'Soil surface turbulent and diffusive resistance (s/m).
COMMON WPSeff   'Effective soil water potential over root zone (m).
COMMON WPotCr   'Big leaf water potential (m).
COMMON NRBC     'Net radiation balance of the canopy (W/m^2).
COMMON SHCA     'Canopy - air sensible heat exchange (W/m^2).
COMMON NRBS     'Net radiation balance of the soil surface (W/m^2).
COMMON a        'Soil - air sensible heat flux (W/m^2).
COMMON S        'Soil heat flux (W/m^2).
COMMON CRH      'Canopy turbulent resistance (s/m).
COMMON CRV      'Canopy resistance to latent heat flux (s/m).
COMMON MGR      'Daily theoretical max. global clear sky radiation (MJ/m^2).
COMMON SkL      'Sky longwave radiance (W/m^2).
COMMON Runoff   'Runoff rate (m/s).
COMMON Detain   'Surface storage of water (m).

'Cumulative amounts at end of current day:
COMMON CumEvap      'Cumulative evaporation from soil (m).
COMMON CumTrans     'Cumulative transpiration from canopy (m).
COMMON CumET        'Cum. evapotranspiration (m).
COMMON CumPos       'Cum. positive root water uptake (m).
COMMON CumNeg       'Cum. negative root water uptake (m) (exudation).
COMMON CumRC        'Cum. root water flux (m).
COMMON CumInf       'Cum. infiltration (m).
COMMON CumPrec      'Cum. precipitation (m) (includes irrigation).
COMMON CumDrip      'Cum. drip irrigation (m).
COMMON CumDrain     'Cum. drainage (m).
COMMON CumRunoff    'Cum. runoff (m).
COMMON CumG         'Cum. soil heat flux (m of water equivalent).
COMMON CumRs        'Cum. solar radiation (J/m^2).
COMMON StorWater    'Water stored in soil profile (m).
COMMON CumRootUptake 'Cum. root water flux (m).

COMMON WBalance     'Soil profile water balance (m).
COMMON iWater       'Initial soil profile water storage (m).
COMMON Theta1Lim    'Vol. water content for potential of -0.1 m, used in time
		    'step setting algorithm.

'These cumulative amounts are for the end of the previous day:
COMMON CumEvBf      'Cum. evaporation from soil (m).
COMMON CumTrBf      'Cum. transpiration (m).
COMMON CumETBf      'Cum. evapotranspiration (m).
COMMON CumPosBf     'Cum. positive root water uptake (m).
COMMON CumNegBf     'Cum. negative root water uptake (m) (exudation).
COMMON CumInfBf     'Cum. infiltration (m).
COMMON CumPrecBf    'Cum. precipitation (m).
COMMON CumDripBf    'Cum. drip irrigation (m).
COMMON CumDrnBf     'Cum. drainage (m).
COMMON CumRunoffBf  'Cum. runoff (m).
COMMON CumGBf       'Cum. soil heat flux (m of water equivalent).
COMMON CumRsBf      'Cum. solar radiation (J/m^2).
COMMON CumRootUptakeBf 'Cum. root water flux (m).

COMMON nLayers%   'Number of soil layers (finite differences).

'Definitions of arrays given where arrays are dimensioned:
COMMON WInput1(), Meteo()
COMMON Dist(), BothLayerThick(), HorNum%(), Depth(), SLThick(), Porsty()
COMMON ZeroCondLayer%()
COMMON DripLayer%()
COMMON WaterOutPutFlag%()
COMMON TempOutPutFlag%()
COMMON Cond(), Kond(), ppot(), AvCond(), AvKond()
COMMON Flow(), Flux(), nFlow(), nFlux(), Theta(), VolW(), VolH(), Temp()
COMMON RF(), RC()
COMMON Header$()
COMMON ClvsWP() 'Holds data for epidermal conductance vs. leaf water potential.
COMMON ClvsGR() 'Holds data for epidermal conductance vs. solar radiation.
COMMON dummy() 'Holds data for soil water potential vs. vol. water content.
COMMON TvsP1() 'Horizon 1 data for soil water potential vs. volumetric water content.
COMMON TvsP2() 'Horizon 2 data for soil water potential vs. volumetric water content.
COMMON TvsP3() 'Horizon 3 data for soil water potential vs. volumetric water content.
COMMON TvsP4() 'Horizon 4 data for soil water potential vs. volumetric water content.
COMMON TvsP5() 'Horizon 5 data for soil water potential vs. volumetric water content.
COMMON TvsP6() 'Horizon 6 data for soil water potential vs. volumetric water content.
COMMON TvsP7() 'Horizon 7 data for soil water potential vs. volumetric water content.
COMMON TvsP8() 'Horizon 8 data for soil water potential vs. volumetric water content.
COMMON TvsP9() 'Horizon 9 data for soil water potential vs. volumetric water content.
COMMON TvsC1() 'Horizon 1 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC2() 'Horizon 2 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC3() 'Horizon 3 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC4() 'Horizon 4 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC5() 'Horizon 5 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC6() 'Horizon 6 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC7() 'Horizon 7 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC8() 'Horizon 8 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC9() 'Horizon 9 data for hydraulic conductivity vs.   "        "     ".
COMMON TevsKO() 'Holds data for soil temperature vs. heat conductivity by vapor.
COMMON SoilAL() 'Data for soil water content (m^3/m^3) vs. soil albedo.

'Constants:
    CONST Pi = 3.14159
    CONST SIGMA = 5.67E-08   'Stefan-Boltzmann constant [W/m^2/K].
    CONST KondS = 1.68       'Thermal conductivity of soil solids [W/m/C] multiplied by 0.4.
    CONST KondW = .57        '   "          "      "  water [W/m/C].
    CONST KondA = .025       '   "          "      "  air [W/m/C].
    CONST VHCapS = 1925000!  'Volumetric heat capacity of soil solids [J/m^3/C].
    CONST VHCapW = 4186000!  '   "        "      "     "  water [J/m^3/C].
    CONST GRAV = 9.81        'Gravitational constant [m/s].

FileOpenErr.2:
PRINT "Error number"; ERR; "in opening file."
SELECT CASE FErr%
CASE 1
   IF InfoFile$ = "" THEN
       PRINT "Name of file containing names of data files was not found."
   ELSE
       PRINT "File "; InfoFile$; " was not found.  Check file name and path."
   END IF
   PRINT "To run ENWATBAL you must enter the following line at the DOS prompt:"
   COLOR 15, 0
   PRINT "ENWATBAL INFOFILE"
   COLOR 7, 0
   PRINT "and press <Enter>."
   PRINT "ENWATBAL is the name of the program and INFOFILE is the name of a file"
   PRINT "containing the names of data files needed by the program.  See the"
   PRINT "documentation for the number of file names needed in file INFOFILE,"
   PRINT "the format of this file and the formats of the data files."
CASE 2
   PRINT "Improper path or file name, or the file "; Plant$; " does not exist."
CASE 3
   PRINT "Improper path or file name, or the file "; IrrPrecip$; " does not exist."
CASE 4
   PRINT "Improper path or file name, or the file "; InitDayFile$; " does not exist."
CASE 5
   PRINT "Improper path or file name, or the file "; Constants$; " does not exist."
CASE 6
   PRINT "Improper path or file name, or the file "; Restart$; " does not exist."
CASE 7
   PRINT "Incorrect path given.  Cannot open file ENWATBAL.PRN"
CASE 8
   PRINT "Cannot open printer as LPT1.  Check cable connections and make sure"
   PRINT "printer is turned on."
CASE 9
   PRINT "Improper path or file name, or file "; Meteo$; " does not exist."
CASE 10
   PRINT "Name of file containing initial conditions not found in file "; InfoFile$
CASE 11
   PRINT "Name of file containing irrigation and precipitation data"
   PRINT "for each day not found in file "; InfoFile$
CASE 12
   PRINT "Name of file containing weather data for each day"
   PRINT "not found in file "; InfoFile$
CASE 13
   PRINT "Name of file containing constants for this run"
   PRINT "not found in file "; InfoFile$
CASE 14
   PRINT "Name of file containing tables for function AFGEN"
   PRINT "not found in file "; InfoFile$
CASE 15
   PRINT "Name of file containing half-hourly weather data"
   PRINT "not found in file "; InfoFile$
CASE 16
   PRINT "Redirection code in file "; InfoFile$; " was neither F for file output"
   PRINT "nor P for printer output.  You must specify one or the other."
'ENWATBAL.BAS, converted from CSMP simulation language (program ENWATBAL.III)
'to QuickBASIC version 4.5 and Professional BASIC version 7.1
'by S.R. Evett, December 1989 - July 1994.
CONST version$ = "Version of 19 July 1994."
DECLARE SUB FinGraphFile ()
DECLARE SUB GetTheta1Lim ()
DECLARE SUB OpenGraFile ()
DECLARE SUB OutputDay (f%)
DECLARE SUB GetConstants (Constants$)
DECLARE SUB InitProfile ()
DECLARE SUB GetDayYear ()
DECLARE SUB OpenFiles ()
DECLARE SUB RedirectOutput ()
DECLARE SUB SetEpson ()
DECLARE SUB GetFileNames ()
DECLARE SUB InitDepthDist ()
DECLARE SUB InitWeather ()
DECLARE SUB InitSoilWater ()
DECLARE SUB RestartInput ()
DECLARE SUB ReStartFile ()
DECLARE SUB PrintHeader ()
DECLARE SUB GetCommandLine ()
DECLARE SUB AFGENInit ()
DECLARE FUNCTION IMPL1! ()
'Use BYVAL for Microsoft Prof. BASIC, ver. 7.X or Visual BASIC for DOS
'but eliminate BYVAL for QuickBASIC ver. 4.5:
'DECLARE SUB INTGRL (icy!, BYVAL dxdy!)
'DECLARE FUNCTION SIGN! (BYVAL a1!, BYVAL a2!)
'DECLARE FUNCTION AMIN! (BYVAL amin1!, BYVAL amin2!)
'DECLARE FUNCTION LHfT! (BYVAL temperature!)
'DECLARE FUNCTION ARSIN! (BYVAL xdum!)
'DECLARE FUNCTION ARCOS! (BYVAL xdum!)
'DECLARE FUNCTION IMPL2! (BYVAL x1!)
'DECLARE FUNCTION IMPL3! (BYVAL x1!)
'DECLARE FUNCTION AFGEN! (Table!(), BYVAL Xval!)

DECLARE SUB INTGRL (icy!, dxdy!)
DECLARE FUNCTION SIGN! (a1!, a2!)
DECLARE FUNCTION AMIN! (amin1!, amin2!)
DECLARE FUNCTION LHfT! (temperature!)
DECLARE FUNCTION ARSIN! (xdum!)
DECLARE FUNCTION ARCOS! (xdum!)
DECLARE FUNCTION IMPL2! (x1!)
DECLARE FUNCTION IMPL3! (x1!)
DECLARE FUNCTION AFGEN! (Table!(), Xval!)

'Variables shared with module ENWAT2.BAS:
'Strings containing file names for input data files:
COMMON InfoFile$    'This file contains file names for input data files.
COMMON IrrPrecip$   'Irrigation & precipitation data.
COMMON DripFile$    'Drip irrigation rate and time data.
COMMON Constants$   'Constants
COMMON AfGenFile$   'Tables of functional relationships.
COMMON Meteo$       'Weather data with periodicity less than 1 day.
COMMON Plant$       'Daily weather & plant growth data.
COMMON InitDayFile$ 'Soil layers & their water content & temperature, 1st day.

COMMON FErr%     'Number of error when opening file, related to file name.
COMMON Mode2$    'Mode to open output files (O=new file, A=append to old file.
COMMON Y$        'Redirection code, F or f=file, P or p=printer
COMMON HourlyOutputFlag 'If >0 then defines period in hours for output, otherwise only daily output.
'Period of averaged input data and offset, from midnight, of center of 1st mean:
COMMON Period    'Period of weather data that is less than daily [h].
COMMON Offset    'Offset of start of weather data from midnight [h].
COMMON MIndex%   '=1+24/Period, index to array holding weather data.
COMMON DebugDat(), di%   'Array holding debugging variables & index to array.
COMMON ErrCode  'Returned from IMPLx if error occurred.  Acted on in main program:
COMMON EndDay%  'Simulation ends at midnight of this day of year.
COMMON EndYear% 'Simulation ends in this year.
COMMON sYear%   'Year that simulation starts. File extension for Plant$.

'Constants in Constants$. See subprogram Getconstants:
COMMON Zo       'Surface roughness length (m).
COMMON Z        'Reference elevation (m) for measurement of wind speed,
		'air temperature and dew point temperature.
COMMON WPCrMx   'Maximum canopy water potential (m).
COMMON SRCr     'Specific crop hydraulic resistance (s).
COMMON LowBoundGrad  'Gradient for water flux - lower soil boundary condition.
COMMON ZeroCdNum% 'Day of year on which to invoke impeding layer.
COMMON DripFlag%  '>0 if a buried drip emitter exists.
COMMON DetCap   'Detention capacity for ponded water (m).
COMMON Lat      'Latitude in degrees.
COMMON AvBarP   'Average barometric pressure (mbar)
COMMON TStepL, TStepH 'Lower & upper limits of time step (s).
COMMON SatCon   'Saturated conductivity for surface soil layer (m/s).

COMMON StartDay%  'Starting day of year. Equals sDay% at start of simulation
	'but changes to day of restarting if simulation is stopped and restarted.
COMMON sDay%    'Starting day of year for simulation.
COMMON WMode%   'Code for source of weather data. Daily data are in file PLANT$
		'and data on intervales less than 1 d are in file METEO$.
		'daily (=0) data or data on intervals less than 1 d (=1).
COMMON Restart$, path$, dnum%, Hl, LH
COMMON cdnum%   'Current day of year.
COMMON TStep    'Current time step (s).
COMMON TimeS    'Time in seconds since midnight of current day.
COMMON sTime    'Time in decimal hours since midnight of current day.
COMMON hTime    'Cumulative time in decimal hours since simulation started.

COMMON EvTr     'Evapotranspiration rate (m/s).
COMMON TrC      'Transpiration rate (m/s).
COMMON LTr      'Transpiration rate (W/m^2).
COMMON Evs      'Rate of evaporation from soil surface (m/s).
COMMON LEvS     'Rate of evaporation from soil surface (W/m^2).
COMMON Infilt   'Infiltration rate (m/s).
COMMON GR       'Solar radiation (W/m^2).
COMMON Ta       'Air temperature (C).
COMMON Tl       'Canopy (big leaf) temperature (C).
COMMON TsBf     'Soil surface temperature for previous time step (C).
COMMON Ra       'Aerodynamic resistance to sensible & latent heat fluxes (s/m).
COMMON Rl       'Big leaf epidermal resistance (s/m).
COMMON Rs       'Soil surface turbulent and diffusive resistance (s/m).
COMMON WPSeff   'Effective soil water potential over root zone (m).
COMMON WPotCr   'Big leaf water potential (m).
COMMON NRBC     'Net radiation balance of the canopy (W/m^2).
COMMON SHCA     'Canopy - air sensible heat exchange (W/m^2).
COMMON NRBS     'Net radiation balance of the soil surface (W/m^2).
COMMON a        'Soil - air sensible heat flux (W/m^2).
COMMON S        'Soil heat flux (W/m^2).
COMMON CRH      'Canopy turbulent resistance (s/m).
COMMON CRV      'Canopy resistance to latent heat flux (s/m).
COMMON MGR      'Daily theoretical max. global clear sky radiation (MJ/m^2).
COMMON SkL      'Sky longwave radiance (W/m^2).
COMMON Runoff   'Runoff rate (m/s).
COMMON Detain   'Surface storage of water (m).

'Cumulative amounts at end of current day:
COMMON CumEvap      'Cumulative evaporation from soil (m).
COMMON CumTrans     'Cumulative transpiration from canopy (m).
COMMON CumET        'Cum. evapotranspiration (m).
COMMON CumPos       'Cum. positive root water uptake (m).
COMMON CumNeg       'Cum. negative root water uptake (m) (exudation).
COMMON CumRC        'Cum. root water flux (m).
COMMON CumInf       'Cum. infiltration (m).
COMMON CumPrec      'Cum. precipitation (m) (includes irrigation).
COMMON CumDrip      'Cum. drip irrigation (m).
COMMON CumDrain     'Cum. drainage (m).
COMMON CumRunoff    'Cum. runoff (m).
COMMON CumG         'Cum. soil heat flux (m of water equivalent).
COMMON CumRs        'Cum. solar radiation (J/m^2).
COMMON StorWater    'Water stored in soil profile (m).
COMMON CumRootUptake 'Cum. root water flux (m).

COMMON WBalance     'Soil profile water balance (m).
COMMON iWater       'Initial soil profile water storage (m).
COMMON Theta1Lim    'Vol. water content for potential of -0.1 m, used in time
			'step setting algorithm.

'These cumulative amounts are for the end of the previous day:
COMMON CumEvBf      'Cum. evaporation from soil (m).
COMMON CumTrBf      'Cum. transpiration (m).
COMMON CumETBf      'Cum. evapotranspiration (m).
COMMON CumPosBf     'Cum. positive root water uptake (m).
COMMON CumNegBf     'Cum. negative root water uptake (m) (exudation).
COMMON CumInfBf     'Cum. infiltration (m).
COMMON CumPrecBf    'Cum. precipitation (m).
COMMON CumDripBf    'Cum. drip irrigation (m).
COMMON CumDrnBf     'Cum. drainage (m).
COMMON CumRunoffBf  'Cum. runoff (m).
COMMON CumGBf       'Cum. soil heat flux (m of water equivalent).
COMMON CumRsBf      'Cum. solar radiation (J/m^2).
COMMON CumRootUptakeBf 'Cum. root water flux (m).

COMMON nLayers%   'Number of soil layers (finite differences).

'Definitions of arrays given where arrays are dimensioned below:
COMMON WInput1(), Meteo()
COMMON Dist(), BothLayerThick(), HorNum%(), Depth(), SLThick(), Porsty()
COMMON ZeroCondLayer%()
COMMON DripLayer%()
COMMON WaterOutPutFlag%()
COMMON TempOutPutFlag%()
COMMON Cond(), Kond(), ppot(), AvCond(), AvKond()
COMMON Flow(), Flux(), nFlow(), nFlux(), Theta(), VolW(), VolH(), Temp()
COMMON RF(), RC()
COMMON Header$()
COMMON ClvsWP() 'Holds data for epidermal conductance vs. leaf water potential.
COMMON ClvsGR() 'Holds data for epidermal conductance vs. solar radiation.
COMMON dummy() 'Holds data for soil water potential vs. vol. water content.
COMMON TvsP1() 'Horizon 1 data for soil water potential vs. volumetric water content.
COMMON TvsP2() 'Horizon 2 data for soil water potential vs. volumetric water content.
COMMON TvsP3() 'Horizon 3 data for soil water potential vs. volumetric water content.
COMMON TvsP4() 'Horizon 4 data for soil water potential vs. volumetric water content.
COMMON TvsP5() 'Horizon 5 data for soil water potential vs. volumetric water content.
COMMON TvsP6() 'Horizon 6 data for soil water potential vs. volumetric water content.
COMMON TvsP7() 'Horizon 7 data for soil water potential vs. volumetric water content.
COMMON TvsP8() 'Horizon 8 data for soil water potential vs. volumetric water content.
COMMON TvsP9() 'Horizon 9 data for soil water potential vs. volumetric water content.
COMMON TvsC1() 'Horizon 1 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC2() 'Horizon 2 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC3() 'Horizon 3 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC4() 'Horizon 4 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC5() 'Horizon 5 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC6() 'Horizon 6 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC7() 'Horizon 7 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC8() 'Horizon 8 data for hydraulic conductivity vs.   "        "     ".
COMMON TvsC9() 'Horizon 9 data for hydraulic conductivity vs.   "        "     ".
COMMON TevsKO() 'Holds data for soil temperature vs. heat conductivity by vapor.
COMMON SoilAL() 'Data for soil water content (m^3/m^3) vs. soil albedo.

   
'Get name of file containing data file names from command line and find out
'if this is a restart of a simulation:
	GetCommandLine
'Get names of files needed by ENWATBAL, day to stop simulation, path for
'output files, redirect some output as instructed and open output device as #6:
	GetFileNames
'Find starting day and year. If this is a restart then starting day is defined
'later:
	GetDayYear
'Open files:
	OpenFiles                          'Open some input and output files.
	PRINT "OpenFiles completed": 'sleep
	IF WMode% = 1 THEN MIndex% = 1 + 24! / Period

'Constants:
	CONST Pi = 3.14159
	CONST SIGMA = 5.67E-08   'Stefan-Boltzmann constant [W/m^2/K].
	CONST KondS = 1.68       'Thermal conductivity of soil solids [W/m/C] multiplied by 0.4.
	CONST KondW = .57        '   "          "      "  water [W/m/C].
	CONST KondA = .025       '   "          "      "  air [W/m/C].
	CONST VHCapS = 1925000!  'Volumetric heat capacity of soil solids [J/m^3/C].
	CONST VHCapW = 4186000!  '   "        "      "     "  water [J/m^3/C].
	CONST GRAV = 9.81        'Gravitational constant [m/s].

	CONST x1.1 = -50000!     'Lower limit for function IMPL1 (leaf water potential).
	CONST x2.1 = 0!          'Upper limit for IMPL1.
	CONST tol.1 = .01        'Tolerance for IMPL1.
	CONST x2.2 = 100!        'Upper limit for IMPL2, lower limit is Ta.
	CONST tol.2 = .01        'Tolerance for IMPL2.
	CONST x2.3 = 150!        'Upper limit for IMPL3, lower limit is Ta.
	CONST tol.3 = .01        'Tolerance for IMPL3.

	i% = 100
	DIM Header$(i%)           'Array to hold strings in user defined header.
	GetConstants (Constants$) 'Get constants & header that may be changed between runs.
	IF LEN(Restart$) THEN PrintHeader   'No need to print this if restarting.

	'Dimensionless constant used in aerodynamic resistance [s/m] calculations;
	'assumes wind speed measured at 2 m elevation; depends on Z & Zo from GetConstants:
	RaConst = (LOG(Z / Zo) ^ 2) / .16
	CntT = HourlyOutputFlag           'counter for number of hours.

'Definition of arrays and variables:
	'nLayers% = Number of soil layers. Changed in InitProfile.
	DIM SLThick(nLayers%)       'Thickness of soil layer i [m].
	DIM Theta(nLayers%)         'Water content of each soil layer [m^3/m^3].
	DIM Temp(nLayers%)          'Soil layer temperature [C].
	DIM HorNum%(nLayers%)       'Horizon number (numbered sequentially downward starting w/ 1).
	DIM ZeroCondLayer%(nLayers%) 'Flag for layer that has zero hydraulic conductivity at bottom.
	DIM DripLayer%(nLayers%)    'Flag for layer with drip emitter.
	DIM WaterOutPutFlag%(nLayers%)  'Flag for output of water content values in WATEROUT.
	DIM TempOutPutFlag%(nLayers%)   'Flag for output of temperature values in WATEROUT.

	'Read initial soil profile data and get values for number of layers
	'(nLayers%), layer thicknesses [m] and initial water contents [m^3/m^3]
	'and temperatures [Deg.C]:
	InitProfile
	PRINT "InitProfile done.": 'sleep
	PRINT "There were"; nLayers%; " soil layers."

	DIM RFLAIdivSRCR(nLayers%)  'Holds diurnally constant values in RC() calculation.
	DIM BothLayerThick(nLayers%)'Thickness of soil layer i plus layer i-1 [m].
	DIM Depth(nLayers% + 1)     'Depth to middle of soil layer I [m].
	DIM Dist(nLayers% + 1)      'Distance between centers of layers I and I+1 [m].

	InitDepthDist               'Initialize Depth(), Dist() & BothLayerThick() arrays.
	PRINT "InitDepthDist done."

	DIM RF(nLayers% + 1)        'Rooting fraction [dimensionless].
	DIM ppot(nLayers% + 1)      'Soil layer matric potential [m].
	DIM HPot(nLayers% + 1)      'Soil layer matric + gravitational potential [m].
	DIM Cond(nLayers% + 1)      'Soil layer hydraulic conductivity [m/s].
	DIM AvCond(nLayers% + 1)    'Average hydraulic conductivity between soil layers [m/s].
	DIM Flux(nLayers% + 1)      'Flow of water into soil layer [m/s].
	DIM RC(nLayers% + 1)        'Soil layer root uptake [m/s].  Can be + or -.
	DIM VHCap(nLayers% + 1)     'Soil layer volumetric heat capacity [J/m^3/C].    ].
	DIM Kond(nLayers% + 1)      'Soil layer thermal conductivity [J/m/s/C].
	DIM AvKond(nLayers% + 1)    'Average thermal cond. between soil layers [J/m/s/C].
	DIM Flow(nLayers% + 1)      'Flow of heat into soil layer [J/s/m^2].
	DIM Porsty(9)               'Porosity of up to 9 soil horizons.
	DIM Begin(i%)               'Begin times for irrig./precip. events in a 24 hour period.
	DIM End1(i%)                'End times for same.
	DIM PrecT(i%)               'Amounts of irrig./precip. events [mm].
	DIM BeginDrip(i%)           'Begin times for drip irrig. events in a day [h].
	DIM EndDrip(i%)             'End times for drip irrig. events in a day [h].
	DIM DripRate(i%)            'Rates of drip irrig. events [m/s].
	DIM WInput1(13, 3)          '12 diurnal variables (4 for plant growth) & 3 days (yesterday, today & tomorrow).
	DIM Meteo(9, MIndex% + 1)   '9 weather variables, MIndex%+1 1/2 h periods (1 yesterday, MIndex% today & 1 tomorrow).
	DIM DebugDat(40, 12)        '20 rows of 12 variables may be saved in a circular
				'data buffer here for debugging purposes.
'Variables that were dimensioned implicitly via the INTGRL function of the
'CSMP version are dimensioned explicitly here:
	DIM VolW(nLayers% + 1)      'Volume of water per unit area of soil layer [m].
	DIM VolH(nLayers% + 1)      'Volumetric heat content of soil layer [J/m^2].
	DIM nFlow(nLayers% + 1)     'Net flow of heat into soil layer [J/s/m^2].
	DIM nFlux(nLayers% + 1)     'Net flow of water into soil layer [m/s].
'The following arrays are passed to Function AfGen which returns a
'linear interpolation of one variable corresponding to another.
'These arrays are redimensioned and initialized in subprogram AFGENInit:
	i% = 1
	DIM ClvsWP(i%, 2) 'Holds data for epidermal conductance vs. leaf water potential.
	DIM ClvsGR(i%, 2) 'Holds data for epidermal conductance vs. solar radiation.
	DIM dummy(i%, 2) 'Holds data for soil water potential vs. vol. water content.
	DIM TvsP1(i%, 2) 'Horizon 1 data for soil water potential vs. volumetric water content.
	DIM TvsP2(i%, 2) 'Horizon 2 data for soil water potential vs. volumetric water content.
	DIM TvsP3(i%, 2) 'Horizon 3 data for soil water potential vs. volumetric water content.
	DIM TvsP4(i%, 2) 'Horizon 4 data for soil water potential vs. volumetric water content.
	DIM TvsP5(i%, 2) 'Horizon 5 data for soil water potential vs. volumetric water content.
	DIM TvsP6(i%, 2) 'Horizon 6 data for soil water potential vs. volumetric water content.
	DIM TvsP7(i%, 2) 'Horizon 7 data for soil water potential vs. volumetric water content.
	DIM TvsP8(i%, 2) 'Horizon 8 data for soil water potential vs. volumetric water content.
	DIM TvsP9(i%, 2) 'Horizon 9 data for soil water potential vs. volumetric water content.
	DIM TvsC1(i%, 2) 'Horizon 1 data for hydraulic conductivity vs.   "        "     ".
	DIM TvsC2(i%, 2) 'Horizon 2 data for hydraulic conductivity vs.   "        "     ".
	DIM TvsC3(i%, 2) 'Horizon 3 data for hydraulic conductivity vs.   "        "     ".
	DIM TvsC4(i%, 2) 'Horizon 4 data for hydraulic conductivity vs.   "        "     ".
	DIM TvsC5(i%, 2) 'Horizon 5 data for hydraulic conductivity vs.   "        "     ".
	DIM TvsC6(i%, 2) 'Horizon 6 data for hydraulic conductivity vs.   "        "     ".
	DIM TvsC7(i%, 2) 'Horizon 7 data for hydraulic conductivity vs.   "        "     ".
	DIM TvsC8(i%, 2) 'Horizon 8 data for hydraulic conductivity vs.   "        "     ".
	DIM TvsC9(i%, 2) 'Horizon 9 data for hydraulic conductivity vs.   "        "     ".
	DIM TevsKO(i%, 2) 'Holds data for soil temperature vs. heat conductivity by vapor.
	DIM SoilAL(i%, 2) 'Data for soil water content (m^3/m^3) vs. soil albedo.

'Polynomial coefficients for solar declination calculation:
	CONST c1.1 = .3964
	CONST c1.2 = 3.631
	CONST c1.3 = .03838
	CONST c1.4 = .07659
	CONST c1.5 = 0!
	CONST c1.6 = -22.97
	CONST c1.7 = -.3885
	CONST c1.8 = -.1587
	CONST c1.9 = -.01021

'Initialize previous soil surface temperature to equal temp. of 1st layer:
	TsBf = Temp(1)
	PRINT "Initial surface temperature:"; TsBf   ': SLEEP
'Initialize temperature of soil at bottom of profile:
	TsBottom = Temp(nLayers%)
'Initialize data in arrays for function AFGEN, get water content of 1st layer
'at -0.1 m for use in time step algorithm & get porosity for each horizon:
	AFGENInit
	PRINT "AFGENInit done.": 'sleep

'Find vol. water content for potential of -0.1 m:
	GetTheta1Lim
	PRINT "Theta1Lim"; Theta1Lim

'Get plant growth and weather data in arrays WInput1() & Meteo() for 1st 2 days:
	InitWeather
	PRINT "InitWeather done.": 'sleep

'    IF restart$ = "" THEN
'Initialize water content and heat capacity of soil layers:
'        InitSoilWater
'    ELSE
'Get final values saved at end of last run that will be initial values:
	IF LEN(Restart$) THEN     'If Restart$<>"".
	    RestartInput
	    PRINT "RestartInput done."
	END IF
'Initialize heat capacity and depth of water in soil layers (and get total
'water content if starting):
	PRINT #2, "Initial water depth, VHCap and Volumetric heat capacity:"
	PRINT "Initial water depth, VHCap and Volumetric heat capacity:"
	InitSoilWater
	PRINT "InitSoilWater done.": 'SLEEP

'Open files for graphing:
	CALL OpenGraFile                  'Open files for graphing.
	'Put depths (m) in water content & temperature profile output file:
	IF HourlyOutputFlag > 0 THEN
	    FOR i% = 1 TO nLayers%
		IF WaterOutPutFlag%(i%) = 1 THEN PRINT #7, Depth(i%);
	    NEXT i%
	    FOR i% = 1 TO nLayers%
		IF TempOutPutFlag%(i%) = 1 THEN PRINT #7, Depth(i%);
	    NEXT i%
	    PRINT #7,
	END IF
	PRINT "OpenGraFile completed.": 'SLEEP

'Initialize Julian day, rank day, time, leaf temperature and internal
'humidity, time step and time in seconds, hours and cumulative hours:
	cdnum% = StartDay% - 1
	IF Restart$ = "" THEN
	dnum% = 0  'rank day, day from start of simulation.
	Tl = WInput1(4, 3)  'Leaf temp. initially equals min. air temp.
	Ts = Tl
	'Leaf internal absolute humidity:
	Hl = 1.323 * EXP(17.27 * Tl / (237.3 + Tl)) / (273.16 + Tl)
	LH = LHfT(Tl)   'Latent heat of vaporization.
	TStep = 1!      'Set time step in seconds. This will change dynamically.
	TimeS = 0!      'Set so that TimeS [s] will be zero initially.
	sTime = 0!      'Decimal hour is zero initially.
	hTime = 0!      'Cumulative decimal hour is zero initially.
	END IF
	eTime = TIMER   'Get start of computation time.
	di% = 1         'Index for DebugDat().
'Initialize plant & weather data: Get Julian day, rank day, maximum clear sky
'solar radiation, daily data, rooting depths and densities and canopy optical
'properties for first day:
	cdYearBf% = sYear%
	GOSUB InitDay  'resets cdnum% to StartDay% by adding 1 to it.
	IF InputErrFlag = 1 THEN GOTO MainLoopEnd
	'If past ending day then quit:
	IF cdnum% > EndDay% AND cdYearBf% = EndYear% THEN GOTO MainLoopEnd
	IF cdnum% <= 0 THEN GOTO MainLoopEnd   'If out of data then exit program.

'Start of main loop. *********************************************************
'Flow refers to heat.  Flux refers to water:
	DO
1
'PRINT "Time"; sTime; "time step"; TStep; "Infilt"; Infilt; "Theta(1)"; Theta(1); "ThetaLim"; Theta1Lim
	 DebugDat(di%, 0) = sTime
'Update heat and water stored in each layer:
	 FOR i% = 1 TO nLayers%
		CALL INTGRL(VolW(i%), (nFlux(i%))) 'Returns VolW().
		Theta(i%) = VolW(i%) / SLThick(i%) '1st find new soil water content.
		CALL INTGRL(VolH(i%), (nFlow(i%))) 'Returns VolH().
	 NEXT i%

'Start of code defining derivative values that are integrated in INTGRL.
2
'Update hydraulic and thermal properties:
	 i% = 1 'Do for 1st layer first:
	 'Get properties for up to 9 soil horizons:
	 Pors = Porsty(HorNum%(i%))   'Soil porosity, horizon HorNum%(i%).
	 SoilVol = 1! - Pors 'Soil solids fraction volume, horizon HorNum%(i%).
	 SELECT CASE HorNum%(i%)
	 CASE 1
		 Cond(i%) = AFGEN(TvsC1(), (Theta(i%)))  'Horizon 1.
		 ppot(i%) = AFGEN(TvsP1(), (Theta(i%)))
	 CASE 2
		 Cond(i%) = AFGEN(TvsC2(), (Theta(i%)))  'Horizon 2.
		 ppot(i%) = AFGEN(TvsP2(), (Theta(i%)))
	 CASE 3
		 Cond(i%) = AFGEN(TvsC3(), (Theta(i%)))  'Horizon 3.
		 ppot(i%) = AFGEN(TvsP3(), (Theta(i%)))
	 CASE 4
		 Cond(i%) = AFGEN(TvsC4(), (Theta(i%)))  'Horizon 4.
		 ppot(i%) = AFGEN(TvsP4(), (Theta(i%)))
	 CASE 5
		 Cond(i%) = AFGEN(TvsC5(), (Theta(i%)))  'Horizon 5.
		 ppot(i%) = AFGEN(TvsP5(), (Theta(i%)))
	 CASE 6
		 Cond(i%) = AFGEN(TvsC6(), (Theta(i%)))  'Horizon 6.
		 ppot(i%) = AFGEN(TvsP6(), (Theta(i%)))
	 CASE 7
		 Cond(i%) = AFGEN(TvsC7(), (Theta(i%)))  'Horizon 7.
		 ppot(i%) = AFGEN(TvsP7(), (Theta(i%)))
	 CASE 8
		 Cond(i%) = AFGEN(TvsC8(), (Theta(i%)))  'Horizon 8.
		 ppot(i%) = AFGEN(TvsP8(), (Theta(i%)))
	 CASE 9
		 Cond(i%) = AFGEN(TvsC9(), (Theta(i%)))  'Horizon 9.
		 ppot(i%) = AFGEN(TvsP9(), (Theta(i%)))
	 CASE ELSE
		 PRINT "Horizon not selected in update of hydraulic properties."
		 PRINT "Program will end now.  Press any key ..."
		 SLEEP: END
	 END SELECT
	 'IF Temp(i%) < 0 THEN Cond(i%) = 0!   'no flux if frozen.

'Update hydraulic properties:
	 HPot(i%) = ppot(i%) - Depth(i%) 'Include gravitational potential.
'Update thermal properties, vol. heat cap., temperature & thermal cond.:
'PRINT "Ta, Ts, i, Temp(i), Theta(i):"; Ta; Ts; i%; Temp(i%); Theta(i%)
	 VHCap(i%) = VHCapW * Theta(i%) + SoilVol * VHCapS
	 Temp(i%) = VolH(i%) / (VHCap(i%) * SLThick(i%))

	 KondV = AFGEN(TevsKO(), (Temp(i%)))
	   dum = (SoilVol * KondS + Theta(i%) * KondW + (Pors - Theta(i%)) * 1.4 * (KondA + KondV))
	 Kond(i%) = dum / (SoilVol * .4 + Theta(i%) + (Pors - Theta(i%)) * 1.4)
	 'For Pullman clay loam at Bushland, TX:
'         Kond(i%) = -.068 + 3.3086 * Theta(i%)
'         IF Kond(i%) < .15 THEN Kond(i%) = .15

	 FOR i% = 2 TO nLayers%
		'Get properties for up to 9 soil horizons:
		Pors = Porsty(HorNum%(i%))   'Soil porosity, horizon HorNum%(i%).
		SoilVol = 1! - Pors 'Soil solids fraction volume, horizon HorNum%(i%).
		SELECT CASE HorNum%(i%)
		CASE 1
		 Cond(i%) = AFGEN(TvsC1(), (Theta(i%)))  'Horizon 1.
		 ppot(i%) = AFGEN(TvsP1(), (Theta(i%)))
		CASE 2
		 Cond(i%) = AFGEN(TvsC2(), (Theta(i%)))  'Horizon 2.
		 ppot(i%) = AFGEN(TvsP2(), (Theta(i%)))
		CASE 3
		 Cond(i%) = AFGEN(TvsC3(), (Theta(i%)))  'Horizon 3.
		 ppot(i%) = AFGEN(TvsP3(), (Theta(i%)))
		CASE 4
		 Cond(i%) = AFGEN(TvsC4(), (Theta(i%)))  'Horizon 4.
		 ppot(i%) = AFGEN(TvsP4(), (Theta(i%)))
		CASE 5
		 Cond(i%) = AFGEN(TvsC5(), (Theta(i%)))  'Horizon 5.
		 ppot(i%) = AFGEN(TvsP5(), (Theta(i%)))
		CASE 6
		 Cond(i%) = AFGEN(TvsC6(), (Theta(i%)))  'Horizon 6.
		 ppot(i%) = AFGEN(TvsP6(), (Theta(i%)))
		CASE 7
		 Cond(i%) = AFGEN(TvsC7(), (Theta(i%)))  'Horizon 7.
		 ppot(i%) = AFGEN(TvsP7(), (Theta(i%)))
		CASE 8
		 Cond(i%) = AFGEN(TvsC8(), (Theta(i%)))  'Horizon 8.
		 ppot(i%) = AFGEN(TvsP8(), (Theta(i%)))
		CASE 9
		 Cond(i%) = AFGEN(TvsC9(), (Theta(i%)))  'Horizon 9.
		 ppot(i%) = AFGEN(TvsP9(), (Theta(i%)))
		CASE ELSE
		 PRINT "Horizon not selected in update of hydraulic properties."
		 PRINT "Program will end now.  Press any key ..."
		 SLEEP: END
		END SELECT
		'IF Temp(i%) < 0 THEN Cond(i%) = 0!   'no flux if frozen.

'Update hydraulic properties:
		HPot(i%) = ppot(i%) - Depth(i%) 'Include gravitational potential.
'Update thermal properties, vol. heat cap., temperature & thermal cond.:
'PRINT "Ta, Ts, i, Temp(i), Theta(i):"; Ta; Ts; i%; Temp(i%); Theta(i%)
3               VHCap(i%) = VHCapW * Theta(i%) + SoilVol * VHCapS
4               Temp(i%) = VolH(i%) / (VHCap(i%) * SLThick(i%))
5               KondV = AFGEN(TevsKO(), (Temp(i%)))
		  dum = (SoilVol * KondS + Theta(i%) * KondW + (Pors - Theta(i%)) * 1.4 * (KondA + KondV))
		Kond(i%) = dum / (SoilVol * .4 + Theta(i%) + (Pors - Theta(i%)) * 1.4)
		'For Pullman clay loam at Bushland, TX:
'                Kond(i%) = -.068 + 3.3086 * Theta(i%)
'                IF Kond(i%) < .15 THEN Kond(i%) = .15

'Compute water flux and heat flow in the profile (between layers but not at
'the surface, i.e. AvCond(1) and AvKond(1) are left equal to zero; and not
'at the lower boundary, those are calculated further below:
		  dum = (Cond(i% - 1) * SLThick(i% - 1) + Cond(i%) * SLThick(i%))
		AvCond(i%) = dum / BothLayerThick(i%)
		IF ZeroCondLayer%(i% - 1) = 1 AND cdnum% > ZeroCdNum% THEN AvCond(i%) = 0
		AvKond(i%) = (Kond(i% - 1) * SLThick(i% - 1) + Kond(i%) * SLThick(i%)) / BothLayerThick(i%)
		'AvKond(i%) = BothLayerThick(i%) / (SLThick(i% - 1) / Kond(i% - 1) + SLThick(i%) / Kond(i%))
		Flow(i%) = (Temp(i% - 1) - Temp(i%)) * AvKond(i%) / Dist(i%)
		Flux(i%) = (HPot(i% - 1) - HPot(i%)) * AvCond(i%) / Dist(i%)
	 NEXT i%
	 BottomGrad = (HPot(nLayers% - 1) - HPot(nLayers%)) / Dist(nLayers%)
	 'PRINT "BottomGrad"; BottomGrad

'For water flux at lower boundary use the gradient given by LowBoundGrad.
'Typically LowBoundGrad=1 for unit gradient assumption or =0 for zero
'flux at lower boundary:
	 IF LowBoundGrad < -99 THEN
	     Flux(nLayers% + 1) = Cond(nLayers%) * BottomGrad
	 ELSE
	     Flux(nLayers% + 1) = Cond(nLayers%) * LowBoundGrad
	 END IF
'Assume constant temperature at lower boundary:
	 Flow(nLayers% + 1) = (Temp(nLayers%) - TsBottom) * Kond(nLayers%) / (SLThick(nLayers%) / 2!)

'Find soil albedo:
	 ALB = AFGEN(SoilAL(), (Theta(1)))
'Scale ABSS according to surface soil albedo:
	 ABSSa = ABSS * (1! - ALB) / .825
6
'Find drip irrigation rate [m/s]:
	 DripFlux = 0!
	 FOR i% = 1 TO NDripEvents%
	     IF sTime > BeginDrip(i%) AND sTime < EndDrip(i%) THEN
		 DripFlux = DripRate(i%) / 3600!  'convert m/h to m/s.
		 EXIT FOR
	     END IF
	 NEXT i%
'Find precipitation rate [m/s]:
	 PrecR = 0!                      'Initialize precip. rate to zero.
	 FOR i% = 1 TO NEvents%
	     'Search for correct event
	     IF sTime > Begin(i%) AND sTime < End1(i%) THEN
		  'Calculate rate and leave.
		  'UpSlop = (4! * PrecT(i%)) / ((End1(i%) - Begin(i%)) ^ 2)
		  UpSlop = 4! * PrecT(i%) / ((End1(i%) - Begin(i%)) * (End1(i%) - Begin(i%)))
		  DwSlop = -UpSlop
		  MdPnt = (Begin(i%) + End1(i%)) / 2!
		  'Calculate precipitation rate in m/s:
		  IF sTime <= MdPnt THEN
		      PrecR = (UpSlop * (sTime - Begin(i%))) / 3600000#
		  ELSE
		      PrecR = (DwSlop * (sTime - End1(i%))) / 3600000#
		  END IF
		  EXIT FOR
	     END IF
	 NEXT i%
'Calculate weather variables by interpolation:
	 IF WMode% = 1 THEN
'Calculate indices for weather array assuming half-hourly average values:
		 'Index1% = INT(sTime / Period + Offset / Period)
		 Index1% = INT(sTime * 2! + .5)
		 Index2% = Index1% + 1
		 'dTime = sTime + Offset - Index1% * Period
		 dTime = sTime + .25 - Index1% * .5
'Calculate air temperature [C]:
		 'Ta = (Meteo(4, Index2%) - Meteo(4, Index1%)) / Period * dTime + Meteo(4, Index1%)
		 Ta = (Meteo(4, Index2%) - Meteo(4, Index1%)) / .5 * dTime + Meteo(4, Index1%)
'Calculate dew point temperature [C]:
		 'DPTc = (Meteo(6, Index2%) - Meteo(6, Index1%)) / Period * dTime + Meteo(6, Index1%)
		 DPTc = (Meteo(6, Index2%) - Meteo(6, Index1%)) / .5 * dTime + Meteo(6, Index1%)
'Calculate solar radiation [W/m^2]:
		 'GR = (Meteo(8, Index2%) - Meteo(8, Index1%)) / Period * dTime + Meteo(8, Index1%)
		 GR = (Meteo(8, Index2%) - Meteo(8, Index1%)) / .5 * dTime + Meteo(8, Index1%)
'Calculate wind speed [m/s].  Add small value to avoid division by zero:
		 'SA = (Meteo(3, Index2%) - Meteo(3, Index1%)) / Period * dTime + Meteo(3, Index1%)
		 SA = (Meteo(3, Index2%) - Meteo(3, Index1%)) / .5 * dTime + Meteo(3, Index1%)
	 ELSE
'Use diurnal average values:
'Find global (solar) radiation flux density [W/m^2]:
'GRConst defined in InitDay:
		GR = GRConst * SIN(sTime * PIdivDL - Phase)
'Find average wind speed:
'UDayBf, UDay, UDayBFDif and UDayAfDif defined in InitDay:
		IF (sTime <= 12!) THEN
		 SA = UDayBF + (sTime + 12!) / 24! * (UDayBfDif)
		ELSE
		 SA = UDay + (sTime - 12!) / 24! * (UDayAfDif)
		END IF
'Calculate air temperature [Deg.C]:
'TaMax, TaMin, TaBaxBf, TaAfAmp, TaBfAmp and TaAmp defined in InitDay:
		IF sTime > 15! THEN
		 Ta = TaMax - (TaAfAmp) * (sTime - 15!) / 14!
		ELSEIF sTime < 5! THEN
		 Ta = TaMaxBf - (TaBfAmp) * (sTime + 9!) / 14!
		ELSE
		 Ta = TaMin + (TaAmp) * (sTime - 5!) / 10!
		END IF
'Calculate dew point temperature, DPTc, [Deg.C]:
'DPMax, DPMin, DPMaxBf, DPAfAmp, DPBfAMP and DPAmp defined in InitDay:
		IF sTime > 15! THEN
		 DPTc = DPMax - (DPAfAmp) * (sTime - 15!) / 14!
		ELSEIF sTime < 5! THEN
		 DPTc = DPMaxBf - (DPBfAmp) * (sTime + 9!) / 14!
		ELSE
		 DPTc = DPMin + (DPAmp) * (sTime - 5!) / 10!
		END IF
	 END IF
	 IF SA < .01 THEN SA = .01 'Set lowest possible wind speed in m/s.
	 IF GR <= 0! THEN GR = 0!  'No negative solar radiation.
	 DebugDat(di%, 1) = Ta
	 DebugDat(di%, 2) = DPTc
	 DebugDat(di%, 3) = Tl
	 TaK = Ta + 273.16
7
'Aerodynamic resistance [s/m].
	 Ra = RaConst / SA    'RaConst is given in the initialization section.
'Soil surface turbulent and diffusive resistance [s/m]:
	 Rs = Ra * LAIRsConst   'LAIRsConst is given in subroutine InitDay.
'Find explicitly related parameters:
'Air humidity, Ha, [kg/m^3]:
	 Ha = 1.323 * EXP(17.27 * DPTc / (237.3 + DPTc)) / TaK
'Sensible heat capacity of the air, SH, [J/m^3/Deg.C]:
'SHConst is given in subroutine InitDay.
	 SH = SHConst / TaK
'Sky longwave radiance, SKL, [W/m^2], SklConst defined in InitDay:
	 IF WMode% = 1 THEN
		'Use Idso's version (from CONSERVB) since other method requires
		'knowledge of global solar rad. and max. possible solar rad.:
		eo = .6178 * EXP((17.169 * DPTc) / (237.3 + DPTc))
		emsky = .7 + .000595 * eo * EXP(1500! / TaK)
		SkL = emsky * SIGMA * TaK * TaK * TaK * TaK
	 ELSE
		SkL = SIGMA * TaK * TaK * TaK * TaK * (.7 + .08241 * Ha * EXP(1500! / TaK)) * SklConst
	 END IF
8
'Calculate the effective soil water potential:
	 WPSeff = 0!
	 FOR i% = 1 TO nLayers%
		WPSeff = WPSeff + ppot(i%) * RF(i%)
	 NEXT i%
	 'Component of leaf epidermal resistance dependent on solar radiation.
	 Cl2 = AFGEN(ClvsGR(), GR)

	 IF LAI > 0! THEN
'Canopy turbulent resistance [s/m]:
	     CRH = Ra * LAICRHConst 'LAICRHConst is given in subroutine InitDay.
'Calculate Richardson number for canopy and correct CRH.  Two methods given:
	     'RI = GRAV * (Z - Zo) * (Ta - Tl) / (TaK * SA ^ 2)
	     ''Redefine CRH using stability correction from CONSERVB.
	     'IF RI > .08 THEN RI = .08   'Limit RI.
	     'CRH = CRH / (1! - 10! * RI)
	     'Redefine CRH using stability correction from Monteith, J.L.,
	     'and M.H. Unsworth. 1973. Principles of environmental
	     'physics, 2nd edn. Edward Arnold, London. P. 238.
	     'IF RI > 1! THEN RI = 1!   'Limit RI.
	     'IF RI < -.1 THEN
	     '    CRH = CRH / ((1! - 16! * RI) ^ .75)
	     'ELSE        'IF Ri >= -.1 AND Ri <= 1 THEN
	     '    CRH = CRH / ((1! - 5! * RI) * (1! - 5! * RI))
	     'END IF
'Implicitly calculate the canopy water potential, WPotCr [m], -30000 is the
'lower quess, 0.0 is the upper quess & 0.01 is the convergence criterion:
		WPotCr = IMPL1!
		IF ErrCode = 9 THEN GOSUB ErrDump
		DebugDat(di%, 4) = WPotCr
'Implicitly calculate the leaf temperature, Tl [C], and transpiration rate.
'Air temperature, Ta, is the lower guess, 100 is the upper guess & 0.01
'is the convergence criterion:
		'Tl = IMPL2!(BYVAL Ta)
		Tl = IMPL2!(Ta)
		IF ErrCode = 9 THEN GOSUB ErrDump
		DebugDat(di%, 5) = Tl
		DebugDat(di%, 6) = Hl
		TrC = LTr / (1000! * LH)               'Transpiration rate [m/s].
	 END IF
9
'Calculate Richardson number for soil surface.  Two methods given:
'         RI = GRAV * (Z - Zo) * (Ta - Ts) / (TaK * SA ^ 2)
'         'Redefine Rs using stability correction from CONSERVB.
'         IF RI > .08 THEN RI = .08   'Limit RI.
'         Rs = Rs / (1! - 10! * RI)
'         'Redefine Rs using stability correction from
'         'Monteith, J.L., and M.H. Unsworth. 1973. Principles of environmental
'         'physics, 2nd edn. Edward Arnold, London. P. 238.
'         IF RI > 1! THEN RI = 1!   'Limit RI.
'         IF RI < -.1 THEN
'                 Rs = Rs / ((1! - 16! * RI) ^ .75)
'         ELSE        'IF Ri >= -.1 AND Ri <= 1 THEN
'                 Rs = Rs / ((1! - 5! * RI) * (1! - 5! * RI))
'         END IF

'Implicitly calculate the surface temperature, Ts [C], and evaporation rate.
'Air temperature, Ta, is the lower guess, 100 is the upper guess & 0.01 is
'the convergence criterion:
	 Tg = Ta
	 'Ts = IMPL3!(BYVAL Tg)
	 Ts = IMPL3!(Ta)
	 DelTs = ABS(Ts - TsBf)
	 IF DelTs > 5! THEN
		PRINT #6, "Surface T changed"; Ts - TsBf; "Deg.C from"; TsBf; "to"; Ts;
		PRINT #6, "in"; TStep; "s, ";
		PRINT #6, sTime; " hrs, day"; cdnum%; "  Air T:"; Ta
		'IF Ts > TsBf THEN Ts = TsBf + 5! ELSE Ts = TsBf - 5!
	 END IF
	 IF DelTs > MaxDelTs THEN MaxDelTs = DelTs  'Find maximum change in surface temperature.
	 TsBf = Ts
	 IF ErrCode = 9 THEN GOSUB ErrDump
10
	 Evs = LEvS / (1000! * LH)              'Evaporation rate [m/s].
	 Flow(1) = S 'Get soil heat flux rate from surface energy balance [W m^-2].
	 G = S / (1000! * LH)  'Get soil heat flux rate [m/s].
	 EvTr = Evs + TrC  'ET rate equals Evap. rate plus transpiration rate.

	 DebugDat(di%, 7) = Temp(1)
	 DebugDat(di%, 8) = Kond(1)
	 DebugDat(di%, 9) = Ts
	 DebugDat(di%, 10) = Ta
	 DebugDat(di%, 11) = ppot(1)
	 DebugDat(di%, 12) = Theta(1)
'Calculate the root water uptake. It is zero if LAI is zero:
	 RCPos = 0!
	 RCNeg = 0!
	 RCTotal = 0!
	 IF LAI > 0 THEN
		FOR i% = 1 TO nLayers%
		 'RFLAIdivSRCR(i%) defined in InitDay:
		 RC(i%) = (WPotCr - WPCrMx - ppot(i%)) * RFLAIdivSRCR(i%)
		 'Use next 7 lines to be compatible with ENWATBAL.III:
		 'IF RC(i%) <= 0! THEN
		 '    'Calculate the total negative uptake:
		 '    RCNeg = RCNeg + RC(i%)
		 'ELSE
		 '    'Calculate the total positive uptake (exudation):
		 '    RCPos = RCPos + RC(i%)
		 'END IF
		 'Use next line as part of correction to root uptake:
		 RCTotal = RCTotal + RC(i%) 'total root uptake/exudation [m/s].
		NEXT i%
		'Use next 'for-next' loop as part of correction to root uptake:
		RCSum = 0!    'Sum of root uptake rates [m/s].
		'IF RCTotal <> 0 THEN
		DEF SEG = VARSEG(RCTotal)
		IF PEEK(VARPTR(RCTotal) + 3) <> 0 OR PEEK(VARPTR(RCTotal) + 2) <> 0 OR PEEK(VARPTR(RCTotal) + 1) <> 0 OR PEEK(VARPTR(RCTotal)) <> 0 THEN
		 FOR i% = 1 TO nLayers%
			'Correct to balance transpiration and root uptake.
			RC(i%) = RC(i%) * (-TrC / RCTotal)
			IF RC(i%) <= 0! THEN
			 'Calculate the total negative uptake:
			 RCNeg = RCNeg + RC(i%)
			ELSE
			 'Calculate the total positive uptake (exudation):
			 RCPos = RCPos + RC(i%)
			END IF
			RCSum = RCSum + RC(i%)
		 NEXT i%
		END IF
	 END IF
'Calculate infiltration and surface flux:
	 'Add drip flux to precip. flux if dripper is at surface:
	 IF DripFlag% = 1 THEN PrecR = PrecR + DripFlux
	 IF PrecR = 0! AND Detain <= 0! AND Evs >= 0! THEN
		Infilt = 0!
		Detain = 0!
		Flux(1) = -Evs
	 ELSE
		IF Evs <= 0! THEN PrecR = PrecR - Evs 'Add dew fall to precip.
		InCap = (Detain - HPot(1)) * .5 * (SatCon + Cond(1)) / Dist(1)
		'InCap = (-HPot(1)) * .5 * (SatCon + Cond(1)) / Dist(1)
		'IF Ts < 0! OR Temp(1) < 0! THEN InCap = 0! 'no intake if frozen.
		Infilt = AMIN(PrecR + Detain, InCap)
		CALL INTGRL(Detain, (PrecR - Infilt - Runoff)) 'runoff from last step.
		IF Detain < 0! THEN Detain = 0!
		Runoff = 0!
		IF Detain > DetCap AND PrecR > Infilt THEN Runoff = PrecR - Infilt
		'IF Temp(1) < 0! THEN Runoff = 0! 'no runoff if frozen.
		IF Evs < 0! THEN
		   Flux(1) = Infilt
		   PrecR = PrecR + Evs   'Restore value of PrecR
		ELSE
		   Flux(1) = Infilt - Evs 'subtract evaporation from infiltration.
		END IF
	 END IF
	 IF DripFlag% = 1 THEN PrecR = PrecR - DripFlux 'restore precip. flux.
'Calculate net Flux of water and Flow of heat:
'(Updating of water/heat content for each layer is done during integration
'at beginning of main loop.)
	 nFlux(1) = Flux(1) - Flux(2) + RC(1)  'Do 1st layer separately.
	 nFlow(1) = Flow(1) - Flow(2)          ' ditto
	 FOR i% = 2 TO nLayers%
		 nFlux(i%) = Flux(i%) - Flux(i% + 1) + RC(i%)
		 IF DripFlag% = i% THEN nFlux(i%) = nFlux(i%) + DripFlux

		 nFlow(i%) = Flow(i%) - Flow(i% + 1)   '[J/s/m^2]
		 'Statement below used in CONSERVB:
		 'nFlow(i%) = Flow(i%) - Flow(i% + 1) + nFlux(i%) * VHCapW * Temp(i%)
	 NEXT i%
'Update the output integrals:
	 CALL INTGRL(CumEvap, (Evs))
	 CALL INTGRL(CumTrans, (TrC))
	 CALL INTGRL(CumPos, (RCPos))
	 CALL INTGRL(CumNeg, (RCNeg))
	 CumRC = CumPos + CumNeg
	 CALL INTGRL(CumET, (EvTr))
	 CALL INTGRL(CumInf, (Infilt))
	 CALL INTGRL(CumPrec, (PrecR))
	 CALL INTGRL(CumDrip, (DripFlux))
	 CALL INTGRL(CumDrain, (Flux(nLayers% + 1)))
	 CALL INTGRL(CumRunoff, (Runoff))
	 CALL INTGRL(CumG, (G))      'Soil heat flux, G already in m/s.
	 CALL INTGRL(CumH, ((SHCA + a) / LH)) 'Sensible heat flux.
	 CALL INTGRL(CumRs, (GR / LH))           'Solar radiation.
	 CALL INTGRL(CumRn, ((NRBC + NRBS) / LH))         'Net radiation.
	 CALL INTGRL(CumRootUptake, (RCSum))  'Root uptake.
	 'CALL INTGRL(CumSA, (SA))       'Wind speed.
	 'CALL INTGRL(CumTa, (Ta))       'Air temperature.
	 'CALL INTGRL(CumDPTc, (DPTc))  'Dew point temperature.
	 StorWater = 0!
	 FOR i% = 1 TO nLayers%
		StorWater = StorWater + VolW(i%)
	 NEXT i%
	 IF DripFlag% = 1 THEN
	     WBalance = StorWater - iWater - CumInf + CumET + CumDrain
	 ELSE
	     WBalance = StorWater - iWater - CumInf + CumET + CumDrain - CumDrip
	 END IF
'Set new time step and increment time:
	 IF Infilt > 0! OR Theta(1) > Theta1Lim THEN
		'lower limit of time step in s:
		IF TStep > TStepL THEN
		 TStep = TStepL
		ELSE
		 IF DelTs > .1 THEN
			TStep = TStep / 10!
		 ELSEIF DelTs < .01 THEN
			TStep = TStep * 1.3
		 END IF
		 'IF TStep > TStepL THEN TStep = TStepL  'Upper limit.
		END IF
	 ELSE
		'TStep = TStepH   'upper limit of time step in s.
		IF DelTs > .1 THEN
		  TStep = TStep / 10!
		ELSEIF DelTs < .01 THEN
		 TStep = TStep * 1.3
		END IF
		IF TStep > 30! THEN TStep = 30!  'Upper limit is 30 s.
	 END IF
	 'IF TStep < .0001 THEN TStep = .0001
	 TimeS = TimeS + TStep   'Increment time by time step [s].

'Insure that each day ends at exactly midnight by reducing last time step
'if necessary:
	 IF TimeS > 86400! THEN
		TStep = 86400! - (TimeS - TStep)
		TimeS = 86400!
	 END IF
	 sTime = TimeS / 3600!   'Time in decimal hours.
'Index to circular debug buffer:
	 IF di% = 40 THEN di% = 1 ELSE di% = di% + 1
'Use next 4 lines to output hourly data:
	 IF sTime >= CntT THEN
		'LOCATE 2, 1: PRINT "Running ENWATBAL for day:"; cdnum%; "hour:"; sTime
		CntT = CntT + HourlyOutputFlag  'Increment number of hours counter.
		GOSUB GraphOutput  'Output for every hour.
	 END IF
'Comment out above 4 lines and use next line to output at noon only:
	 'IF sTime >=12 THEN GOSUB GraphOutput
'Daily update at midnight:
	 IF TimeS = 86400! THEN
		FOR i% = 1 TO 30
		 Quit$ = INKEY$    'Poll keyboard buffer for <ESC>.
		 IF Quit$ = CHR$(27) THEN EXIT FOR
		NEXT i%
		'Set hTime to integer value to avoid cumulative error:
		hTime = 24 * dnum%
		'Calculate todays output values:
		dEvap = CumEvap - CumEvBf
		dTrans = CumTrans - CumTrBf
		dPos = CumPos - CumPosBf
		dNeg = CumNeg - CumNegBf
		dET = CumET - CumETBf
		dInf = CumInf - CumInfBf
		dPrec = CumPrec - CumPrecBf
		dDrip = CumDrip - CumDripBf
		dDrain = CumDrain - CumDrnBf
		dRunoff = CumRunoff - CumRunoffBf
		dG = CumG - CumGBf
		dH = CumH - CumHBf
		dRs = CumRs - CumRsBf
		dRn = CumRn - CumRnBf
		dRC = CumRootUptake - CumRootUptakeBf  'Root uptake [mm].
		'dSA = CumSA - CumSABf
		'dTa = CumTa - CumTaBf
		'dDPTc = CumDPTc - CumDPTcBf
		'Store cumulative daily values:
		CumEvBf = CumEvap
		CumTrBf = CumTrans
		CumPosBf = CumPos
		CumNegBf = CumNeg
		CumETBf = CumET
		CumInfBf = CumInf
		CumPrecBf = CumPrec
		CumDripBf = CumDrip
		CumDrnBf = CumDrain
		CumRunoffBf = CumRunoff
		CumGBf = CumG
		CumHBf = CumH
		CumRsBf = CumRs
		CumRnBf = CumRn
		CumRootUptakeBf = CumRootUptake
		'CumSABf = CumSA
		'CumTaBf = CumTa
		'CumDPTcBf = CumDPTc
		dumTime = TIMER
		PRINT #6, "Computer time used ="; dumTime - eTime; "seconds.  ";
		PRINT #6, "Time step ="; TStep; "seconds.  Day ="; cdnum%; "  Max. surface T change ="; MaxDelTs
		'PRINT #6, "  SA:"; dSA / 86400!; "m/s.  Ta:"; dTa / 86400!; "C.  DPTc:"; dDPTc / 86400!
		eTime = dumTime
		'CALL OutputDay(3)       'Print daily output to file #3.
		'Print daily ET, water balance, etc. to file:
		GOSUB EtOutput       'Print E, T, ET, water balance, etc.
		GOSUB ProfileOutput  'Print profile water content.
		IF Quit$ = CHR$(27) OR cdnum% = EndDay% THEN
		  'Save variables to file, close files and end so program may
		  're-start at 0 hours on next day.
		  CLOSE     'Close all files.
		  OPEN "o", #3, "RESTART." + LTRIM$(STR$(cdnum%))
		  ReStartFile    'Save data for use at start of continuing run.
		  CLOSE #3
		  OPEN "o", #3, "RESTART.BAT"
		  PRINT #3, "ENWATBAL " + InfoFile$ + " RESTART." + LTRIM$(STR$(cdnum%))
		  CLOSE #3
		  IF Y$ = "F" THEN
			'Re-open file for output.
			ON ERROR GOTO FileOpenErr
			FErr% = 7
			OPEN "a", #6, path$ + "ENWATBAL.PRN"
			ON ERROR GOTO 0
		  END IF
		  PRINT #6, DATE$, TIME$
		  PRINT #6, CHR$(18)  'Cancel condensed type.
		  END   'quit here.
		END IF
'If it is midnight then get Julian day, rank day, maximum clear sky solar
'radiation, daily data, rooting depths and densities and canopy optical
'properties for next day:
		GOSUB InitDay
		'If past ending day then quit:
		IF cdnum% > EndDay% AND cdYearBf% = EndYear% THEN EXIT DO
		cdYearBf% = cdYear%
		IF cdnum% <= 0 THEN EXIT DO   'If out of data then exit program.
		'If data has ended then stop simulation.
		IF InputEndFlag = 1 THEN EXIT DO
	 END IF
	LOOP  'End of main program loop.
MainLoopEnd:
	CALL FinGraphFile  'finish graph files.
	'Re-open file:
	IF Y$ = "F" THEN
	 'Re-open file for output.
	 ON ERROR GOTO FileOpenErr
	 FErr% = 7
	 OPEN "a", #6, path$ + "ENWATBAL.PRN"
	 ON ERROR GOTO 0
	END IF
	PRINT #6, DATE$, TIME$
	PRINT #6, CHR$(18)  'Cancel condensed type.
END
'End of main program loop.  ***************************************************

'====================================================================
ErrDump:
PRINT "Error dump occurring."
GOSUB GraphOutput  'dump last values to graph files.
GOSUB EtOutput
GOSUB ProfileOutput
hTime = hTime + 1  'Increment time in hours so number of data rows is correct.
CALL FinGraphFile  'finish graph files.
'Re-open file:
IF Y$ = "F" THEN
	'Re-open file for output.
	ON ERROR GOTO FileOpenErr
	FErr% = 7
	OPEN "a", #6, path$ + "ENWATBAL.PRN"
	ON ERROR GOTO 0
END IF
CALL OutputDay(6)    'dump to device #6.
'Now dump PPot() and RF():
PRINT #6, "DEPTH (m):"
FOR i% = 1 TO nLayers%
	PRINT #6, USING " #.###"; Depth(i%);
NEXT i%
PRINT #6,
PRINT #6, "PPOT (m):"
FOR i% = 1 TO nLayers%
	PRINT #6, ppot(i%);
NEXT i%
PRINT #6,
PRINT #6, "RF:"
FOR i% = 1 TO nLayers%
	PRINT #6, RF(i%);
NEXT i%
PRINT #6,
PRINT #6, "RC:"
FOR i% = 1 TO nLayers%
	PRINT #6, RC(i%);
NEXT i%
END

'====================================================================
GraphOutput: 'Put variables in files for graphing.
hTime = 24! * (dnum% - 1) + sTime  'Calculate time in hours since start.
'                                                              Subdirectories:
PRINT #8, cdYearBf%; cdnum%; hTime; Ta; DPTc; Tl; Ts; Temp(1)                   'TEMP
PRINT #9, cdYearBf%; cdnum%; hTime; Cl; Cl1; Cl2;                                'xxCOND
PRINT #9, cdYearBf%; cdnum%; hTime; Rl; CRH; CRV                               'COND/RESIST
PRINT #11, cdYearBf%; cdnum%; hTime; Hl; Ha; Ho; Hs                             'HUM
PRINT #12, cdYearBf%; cdnum%; hTime; GR; SkL; NRBC; NRBS; LWRc; ALB             'FLUX1
PRINT #13, cdYearBf%; cdnum%; hTime; SHCA; LWRS; LEvS; a; S; LTr; RCNeg; RCPos; RCSum 'FLUX2
PRINT #14, cdYearBf%; cdnum%; hTime; WPSeff; WPotCr; ppot(1)                     'POT
PRINT #15, cdYearBf%; cdnum%; hTime; LAI; FTSR; ABSC; ABSSa; CRH / Ra; Rs / Ra   'NON
GOSUB WaterOut
RETURN

'====================================================================
WaterOut:
'Output water contents & temperatures at depths specified in profile initialization file.
PRINT #7, cdYearBf%; cdnum%; hTime;
FOR i% = 1 TO nLayers%
	IF WaterOutPutFlag%(i%) = 1 THEN PRINT #7, USING " #.###"; Theta(i%);
NEXT i%
FOR i% = 1 TO nLayers%
	IF TempOutPutFlag%(i%) = 1 THEN PRINT #7, USING " ##.##"; Temp(i%);
NEXT i%
PRINT #7,
RETURN

'====================================================================
EtOutput:
ON ERROR GOTO ETOutputErr
'File found in subdirectory ET.
OPEN "a", #16, path$ + "et\data1"
PRINT #16, cdYearBf%; cdnum%; dEvap; dTrans; dET; dG; dInf; dPrec; dDrain; dRunoff; WBalance; dRs / 1000!; dRn / 1000!; dRC; dH / 1000!; dDrip
ON ERROR GOTO 0
CLOSE #16
RETURN

ETOutputErr:
PRINT "Error no. "; ERR; " in printing ET to #16, "; path$ + "et\data1"; "."
PRINT "Check path statement in ENWATBAL.FIL, check existence of drive and path,"
PRINT "and check FILES= statement in CONFIG.SYS file."
PRINT "Press any key to continue...."
SLEEP
RESUME NEXT

'====================================================================
ProfileOutput:
ON ERROR GOTO ProfileOutputErr
'File found in subdirectory ET.
OPEN "a", #16, path$ + "profile\data1"
PRINT #16, cdYearBf%; cdnum%;
FOR i% = 1 TO nLayers%
	PRINT #16, USING " #.###"; Theta(i%);
NEXT i%
PRINT #16,
PRINT #16, cdYearBf%; cdnum%;
FOR i% = 1 TO nLayers%
	PRINT #16, USING " ##.##"; Temp(i%);
NEXT i%
ON ERROR GOTO 0
PRINT #16,
CLOSE #16
RETURN

'====================================================================
InitDay:
10000
'Get daily input data, update day, reset time and calculate values that
'are constant over a day:
	LOCATE 2, 1
	IF ChangeCdNumFlag% = 1 THEN cdnum% = 0
	CurrentDay% = cdnum% + 1
	IF CurrentDay% > 365 THEN CurrentDay% = 1
	CLS
	COLOR 15, 0
	PRINT "Running ENWATBAL.  To stop press <Esc>.  You may have to wait several minutes."
	COLOR 7, 0
	PRINT "Computing for day"; CurrentDay%
	ChangeCdNumFlag% = 0
	IF WMode% = 1 THEN
	    FOR j% = 1 TO 9
		'Put data from last period of previous day in 1st element of Meteo():
		'SWAP Meteo(j%, 48), Meteo(j%, 0)
		SWAP Meteo(j%, MIndex% - 1), Meteo(j%, 0)
		'Put data from 1st period of today in 2nd element of Meteo():
		'SWAP Meteo(j%, 49), Meteo(j%, 1)
		SWAP Meteo(j%, MIndex%), Meteo(j%, 1)
	     NEXT j%
	     'Fill next MIndex%-1 elements of Meteo() with data for last
	     'MIndex%-2 periods of today and 1st period of tomorrow:
10005
	     FOR i% = 2 TO MIndex%  '49
		IF NOT EOF(1) THEN INPUT #1, hlfYear%
		FOR j% = 1 TO 9
		   IF NOT EOF(1) THEN
		      INPUT #1, Meteo(j%, i%)
		   ELSE
		      InputEndFlag = 1 'Set flag so simulation will end.
		      'Use todays data for 1st period tomorrow:
		      FOR k% = 1 TO 9
			 'Meteo(k%, 49) = Meteo(k%, 48)
			 Meteo(k%, MIndex%) = Meteo(k%, MIndex% - 1)
		      NEXT k%
		      'Meteo(1, 49) = cdnum% + 2  'Change DOY to avoid error message.
		      Meteo(1, MIndex%) = cdnum% + 2  'Change DOY to avoid error message.
		   END IF
		NEXT j%
	     NEXT i%
	END IF
	hlfYearBf% = hlfYear%
10010
'Move previous data to rows 1 and 2 of WInput1():
	IF NOT EOF(4) THEN INPUT #4, cdYear%
	IF WMode% = 1 THEN
	   IF cdYear% <> hlfYear% THEN
	       PRINT "Years not synchronized in files "; Meteo$; " and "; Plant$
	       PRINT "Program will end now.": END
	   END IF
	END IF
	IF cdYearBf% <> cdYear% THEN ChangeCdNumFlag% = 1
	FOR j% = 1 TO 13
	    SWAP WInput1(j%, 1), WInput1(j%, 2)
	    SWAP WInput1(j%, 2), WInput1(j%, 3)
	    IF NOT EOF(4) THEN
		INPUT #4, WInput1(j%, 3)
	    ELSE
		InputEndFlag = 1  'Set flag so simulation will end.
		'Use today's data for tomorrow:
		WInput1(j%, 3) = WInput1(j%, 2)
	    END IF
	NEXT j%
	PRINT WInput1(1, 1), WInput1(1, 2), WInput1(1, 3); TAB(40); "Day of year,"; cdYear%
	PRINT WInput1(2, 1), WInput1(2, 2), WInput1(2, 3); TAB(40); "Solar radiation (MJ/m^2)"
	PRINT WInput1(3, 1), WInput1(3, 2), WInput1(3, 3); TAB(40); "Max. air temp. (C)"
	PRINT WInput1(4, 1), WInput1(4, 2), WInput1(4, 3); TAB(40); "Min. air temp. (C)"
	PRINT WInput1(5, 1), WInput1(5, 2), WInput1(5, 3); TAB(40); "Max. dew point temp. (C)"
	PRINT WInput1(6, 1), WInput1(6, 2), WInput1(6, 3); TAB(40); "Min. dew point temp. (C)"
	PRINT WInput1(7, 1), WInput1(7, 2), WInput1(7, 3); TAB(40); "Ave. wing speed (m/s)"
	PRINT WInput1(8, 1), WInput1(8, 2), WInput1(8, 3); TAB(40); "Leaf area index"
	PRINT WInput1(9, 1), WInput1(9, 2), WInput1(9, 3); TAB(40); "Rooting depth (m)"
	PRINT WInput1(10, 1), WInput1(10, 2), WInput1(10, 3); TAB(40); "Depth of max. root density (m)"
	PRINT WInput1(11, 1), WInput1(11, 2), WInput1(11, 3); TAB(40); "Depth of inflection in root density (m)"
	PRINT WInput1(12, 1), WInput1(12, 2), WInput1(12, 3); TAB(40); "Fraction of max. root density"
	PRINT WInput1(13, 1), WInput1(13, 2), WInput1(13, 3); TAB(40); "Ave. air pressure (kPa)"
	PRINT "End of swapping section in InitDay."

	TStep = .1                  'Set time step to 1 s.
	sTime = TStep / 3600!       'Set decimal hour.
	TimeS = TStep               'Set time in seconds.
	IF HourlyOutputFlag > 0 THEN
	   CntT = HourlyOutputFlag 'Set number of hours counter to period for output.
	ELSE
	   CntT = 32500         'Set arbitrarily high to inhibit hourly output.
	END IF
	dnum% = dnum% + 1           'Increment rank day.
	IF WMode% = 1 THEN
	    'IF cdnum% + 1 <> Meteo(1, 1) AND cdnum% + 2 <> Meteo(1, 49) THEN
	    IF cdnum% + 1 <> Meteo(1, 1) AND cdnum% + 2 <> Meteo(1, MIndex%) THEN
		PRINT "Day numbering not synchronized in subroutine InitDay."
		PRINT "cdnum%+1="; cdnum% + 1; " and Meteo(1,1)="; Meteo(1, 1)
		'PRINT "cdnum%+2="; cdnum% + 2; " and Meteo(1,49)="; Meteo(1, 49)
		PRINT "cdnum%+2="; cdnum% + 2; " and Meteo(1,MIndex%)="; Meteo(1, MIndex%)
		PRINT "Program will end now.  Press any key ...."
		InputErrFlag = 1
	    END IF
	END IF

	IF cdnum% + 1 <> WInput1(1, 2) THEN
	    PRINT "Day numbering not synchronized in subroutine InitDay."
	    PRINT "cdnum%+1="; cdnum% + 1; " and WInput1(1,2)="; WInput1(1, 2)
	    PRINT "Program will end now.  Press any key ...."
	    InputErrFlag = 1
	END IF

	IF InputErrFlag = 1 THEN GOTO InitDay1
	cdnum% = WInput1(1, 2)  'Get Julian day.
	MaxDelTs = 0  'Set max. surface temperature change to zero.
'Calculate the maximum (clear day) solar radiation [MJ m^-2]:
	latr = Lat * Pi / 180!      'Convert latitude to radians.
	dec = c1.1  'c1(1)
	Phi = .01721 * cdnum%
	dec = dec + c1.2 * SIN(Phi) + c1.6 * COS(Phi)
	Phi = 2! * .01721 * cdnum%
	dec = dec + c1.3 * SIN(Phi) + c1.7 * COS(Phi)
	Phi = 3! * .01721 * cdnum%
	dec = dec + c1.4 * SIN(Phi) + c1.8 * COS(Phi)
	Phi = 4! * .01721 * cdnum%
	dec = dec + c1.5 * SIN(Phi) + c1.9 * COS(Phi)
	decr = dec * Pi / 180!
	dl = 2! * ARCOS(-TAN(latr) * TAN(decr)) * 3.82
	HDAWN = 180! - dl * 7.5
	H = HDAWN
	step1 = (dl * 15!) / 100!
'Calculate daily global clear sky radiation, MGR [MJ/m^2]:
	MGR = 0!
	FOR L% = 1 TO 51
	  HR = H * Pi / 180!
	  SAN1 = ARSIN(SIN(latr) * SIN(decr) - COS(latr) * COS(decr) * COS(HR))
	  SAN = SAN1 * (180! / Pi)
	  'This is based on the tables by Avaste:
	  GRA1 = .8646 + 7.2396 * SAN + .43386 * SAN ^ 2
	  GRA = GRA1 - .0066192 * SAN ^ 3 + 2.6677E-05 * SAN ^ 4
	  MGR = MGR + GRA * step1 * 2!
	  H = H + step1
	NEXT L%
	MGR = MGR * (86400! / 360!) * .000001
	DGR = WInput1(2, 2)     'Daily total solar radiation.
	PRINT "MGR, DGR"; MGR; DGR
	GRConst = 436.33 * DGR / dl       'Used in GR generation.
	PIdivDL = Pi / dl                 'Used in GR generation.
	Phase = 12! * PIdivDL - Pi / 2!   'Used in GR generation.
	SklConst = 1.2 - .2 * DGR / MGR   'Used in SkL generation.

	TaMax = WInput1(3, 2)
	TaMaxBf = WInput1(3, 1)    'Max. air temp. yesterday.
	TaMin = WInput1(4, 2)
	TaMinAf = WInput1(4, 3)    'Min. air temp. tomorrow.
	TaAfAmp = TaMax - TaMinAf  'Used in Ta generation.
	TaBfAmp = TaMaxBf - TaMin  'Used in Ta generation.
	TaAmp = TaMax - TaMin      'Used in Ta generation.
	DPMax = WInput1(5, 2)
	DPMaxBf = WInput1(5, 1)    'Max. dew pt. temp. yesterday.
	DPMin = WInput1(6, 2)
	DPMinAF = WInput1(6, 3)    'Min. dew pt. temp. tomorrow.
	DPAfAmp = DPMax - DPMinAF  'Used in DPTc generation.
	DPBfAmp = DPMaxBf - DPMin  'Used in DPTc generation.
	DPAmp = DPMax - DPMin      'Used in DPTc generation.
	UDay = WInput1(7, 2)       'Average wind speed for today.
	UDayBF = WInput1(7, 1)     'Average wind speed for yesterday.
	UDayBfDif = UDay - UDayBF  'Used in SA generation.
	UDayAf = WInput1(7, 3)     'Average wind speed for tomorrow.
	UDayAfDif = UDayAf - UDay  'Used in SA Generation.
	LAI = WInput1(8, 2)
'Compute daily constant for calculation of CRH:
	  dum = 5.58437 - 4.56163 * LAI + 1.88167 * LAI ^ 2 - .375253 * LAI ^ 3
	LAICRHConst = dum + .0356503# * LAI ^ 4 - .00129185# * LAI ^ 5
'Compute daily constant for calculation of Rs:
	LAIRsConst = 1! + 1.144 * LAI - .16731 * LAI ^ 2 + .026752 * LAI ^ 3 + .013099 * LAI ^ 4
'Find the rooting depths, and the fractional root density in each layer:
'        RD = WInput1(9, 2)
'        RM = WInput1(10, 2)
'        RFTotal = 0!  'Sum should equal 1.
'        FOR i% = 1 TO nLayers%
'             IF (Depth(i%) <= RM) THEN
'                RF(i%) = (2! * Depth(i%) * SLThick(i%)) / (RD * RM)
'             END IF
'             IF (Depth(i%) > RM AND Depth(i%) <= RD) THEN
'                RF(i%) = (-2! * SLThick(i%) * (Depth(i%) - RD)) / (RD * (RD - RM))
'             END IF
'             IF (RF(i%) < 0!) THEN RF(i%) = 0!
'             RFTotal = RFTotal + RF(i%)
'        NEXT i%
'New RFn for wheat:
	RFTotal = 0!   'Initialize
	RD = WInput1(9, 2)   'Maximum depth of effective roots (m).
	RM = WInput1(10, 2)  'Depth of maximum root length density (RLD) (m).
	RI = WInput1(11, 2)  'Depth of 2nd inflection (if any) in RLD curve (m).
	RIFRAC = WInput1(12, 2) 'Fraction of max. root density found at inflection.
	'RIFRAC = .3   'fraction of max. RLD existing at 2nd inflection.
	FOR i% = 1 TO nLayers%
	    SELECT CASE Depth(i%)
	    CASE IS < RM
		RF(i%) = SLThick(i%) * Depth(i%) / RM
	    CASE RM TO RI
		RF(i%) = SLThick(i%) * (1 + (1! - RIFRAC) * (RM - Depth(i%)) / (RI - RM))
	    CASE RI TO RD
		RF(i%) = SLThick(i%) * (RIFRAC + RIFRAC * (RI - Depth(i%)) / (RD - RI))
	    CASE IS > RD
		RF(i%) = 0
	    CASE ELSE
		RF(i%) = 0
	    END SELECT
	RFTotal = RFTotal + RF(i%)
	NEXT i%
'Correct fractional root density so sum equals 1:
	IF RFTotal > 0 THEN
	FOR i% = 1 TO nLayers%
		RF(i%) = RF(i%) / RFTotal
		RFLAIdivSRCR(i%) = RF(i%) * LAI / SRCr 'Used in RC() calculation.
	NEXT i%
	END IF
'Calculate constant for use in calculation of SH
	SHConst = (AvBarP / 1013.2) * 1154.8 * 303.16
'Calculate the optical properties of the canopy and soil system:
	 ABSC = .5809 * LAI - .2231 * LAI ^ 2 + .0464 * LAI ^ 3
	ABSC = ABSC - .004759 * LAI ^ 4 + .0001875# * LAI ^ 5
	 ABSS = .825 - .6447 * LAI + .2646 * LAI ^ 2 - .05695 * LAI ^ 3
	ABSS = ABSS + .005937 * LAI ^ 4 - .0002355# * LAI ^ 5
	 FTSR = 1! - .678 * LAI + .2052 * LAI ^ 2 - .02799 * LAI ^ 3
	FTSR = FTSR + .001383 * LAI ^ 4
'Get irrigation/precipitation:
    DO
	'Get day of year and number of events;
	IF NOT EOF(5) THEN
		INPUT #5, PDay%, NEvents%
	ELSE
		PRINT "Out of data while reading irrigation/precipitation data ";
		PRINT "for day"; cdnum%
		PRINT "Program will end now.  Press any key ..."
		InputErrFlag = 1
	END IF
	IF InputErrFlag = 1 THEN GOTO InitDay1
	REDIM Begin(NEvents%), End1(NEvents%), PrecT(NEvents%)
	IF PDay% = cdnum% THEN PRINT PDay%, NEvents%
	FOR i% = 1 TO NEvents%
	    'Get start & stop times and depth in mm.
	    IF NOT EOF(5) THEN
		INPUT #5, Begin(i%), End1(i%), PrecT(i%)
	    ELSE
		PRINT "Out of data while reading irrigation/precipitation data ";
		PRINT "for day"; cdnum%
		PRINT "Program will end now.  Press any key ..."
		InputErrFlag = 1
	    END IF
	    IF PDay% = cdnum% THEN PRINT Begin(i%), End1(i%), PrecT(i%)
	    IF InputErrFlag = 1 THEN GOTO InitDay1
	NEXT i%
    LOOP WHILE PDay% < cdnum%
    IF PDay% <> cdnum% THEN PRINT "Error in irrig\precip. input.": SLEEP: END
    IF DripFlag% > 0 THEN
	GOSUB GetDrip
	IF PDay% <> cdnum% THEN PRINT "Error in drip irrig. input.": SLEEP: END
    END IF
'        PRINT
	'Check to see if we're on the same day:
InitDay1:
10040
RETURN

'=============================================================================
GetDrip:
'Get drip irrigation:
10020
    DO
	'Get day of year and number of events;
	IF NOT EOF(3) THEN
		INPUT #3, PDay%, NDripEvents%
	ELSE
		PRINT "Out of data while reading drip irrigation data ";
		PRINT "for day"; cdnum%
		PRINT "Program will end now.  Press any key ..."
		InputErrFlag = 1
	END IF
	IF InputErrFlag = 1 THEN GOTO InitDay1
	REDIM BeginDrip(NEvents%), EndDrip(NEvents%), DripRate(NEvents%)
	IF PDay% = cdnum% THEN PRINT PDay%, NDripEvents%; "Drip in layer no."; DripFlag%
	FOR i% = 1 TO NDripEvents%
	    'Get start & stop times and depth in mm.
	    IF NOT EOF(3) THEN
		INPUT #3, BeginDrip(i%), EndDrip(i%), DripRate(i%)
	    ELSE
		PRINT "Out of data while reading drip irrigation data ";
		PRINT "for day"; cdnum%
		PRINT "Program will end now.  Press any key ..."
		InputErrFlag = 1
	    END IF
	    IF PDay% = cdnum% THEN PRINT BeginDrip(i%), EndDrip(i%), DripRate(i%)
	    IF InputErrFlag = 1 THEN RETURN InitDay1
	NEXT i%
    LOOP WHILE PDay% < cdnum%
RETURN

'=============================================================================
FileOpenErr:
PRINT "Error number"; ERR; "in opening file."
SELECT CASE FErr%
CASE 7
   PRINT "Incorrect path given.  Cannot open file ENWATBAL.PRN"
CASE ELSE
   PRINT "Error detected but unknown."
END SELECT
PRINT "Program will end now.  Press any key."
SLEEP
END

'=============================================================================
ProfileOutputErr:
PRINT "Error no. "; ERR; " in printing profile to #16, "; path$ + "profile\data1"; "."
PRINT "Press any key to continue...."
SLEEP
RESUME NEXT

'=============================================================================
InputErr:
InputErrFlag = 1
RESUME NEXT

'==============================================================================
'FUNCTION AFGEN (Table(), BYVAL Xval)
FUNCTION AFGEN (Table(), Xval)
'Finds two entries in Table( ,1) which bracket the value of the independent
'variable, Xval, and then linearly interpolates between the two corresponding
'entries in Table( ,2) to find the corresponding value of the dependent
'variable.
	k% = UBOUND(Table, 1)
	j% = k%  'Place limit on one end of output.
	'Set limits on values the function may take.
	IF Xval < Table(1, 1) THEN AFGEN = Table(1, 2): EXIT FUNCTION
	IF Xval > Table(k%, 1) THEN AFGEN = Table(k%, 2): EXIT FUNCTION
	FOR jj% = 2 TO k%
	  IF Xval <= Table(jj%, 1) THEN j% = jj%: EXIT FOR
	NEXT jj%
	AFGEN = (Xval - Table(j% - 1, 1)) * (Table(j%, 2) - Table(j% - 1, 2)) / (Table(j%, 1) - Table(j% - 1, 1)) + Table(j% - 1, 2)
END FUNCTION

'==============================================================================
'FUNCTION AMIN! (BYVAL amin1!, BYVAL amin2!)
FUNCTION AMIN! (amin1!, amin2!)
'Find least of 2 arguments:
 IF amin1! < amin2! THEN
	AMIN! = amin1!
 ELSE
	AMIN! = amin2!
 END IF
END FUNCTION

'==============================================================================
FUNCTION IMPL1!
'Returns the root of the implicit function describing the canopy water
'potential, WPotCr.  The implicit set of equations is (where xWPCr is the
'quess for canopy water potential):
'    Cl1 = AFGEN(ClvsWP(), xWPCr)
'    Cl = 2! / (1! / Cl1 + 1! / Cl2)
'    Rl = 1! / (Cl * LAI)
'    CRV = CRH + Rl   'Resistance to latent heat flux = resistance to
'                     'sensible heat flux + leaf epidermal resistance.
'                     'This means that aerodynamic resistances to sensible
'                     'and latent heat fluxes are taken equal.
'    'Note that negative transpiration rates are possible:
'    LTr = (Hl - Ha) * LH / CRV
'    Func1 = WPSeff + WPCrMx - LTr * SRCr / (1000! * LH * LAI) - xWPCr

'The root must lie between x1 and x2 to a tolerance, tol.
'Reference: Press, W.H., B.P. Flannery, S.A. Teulosky, and W.T. Vetterling.
'1986. Numerical recipes - the art of scientific computing. Cambridge
'University Press. p. 253.
SHARED sTime, di%, DebugDat(), ErrCode, cdnum%            'For root finder.
SHARED ClvsWP(), Cl, Cl1, Cl2, LAI, Rl, CRH, Hl, Ha, CRV  'For can. water pot.
SHARED WPSeff, LH, LTr, WPCrMx, SRCr                      'For can. water pot.
SHARED Tl  'leaf temperature from previous time step.
SHARED Ta  'Air temperature [C].

itmax% = 100  'Set max. no. of iterations.
eps = 3E-08   'Machine floating point precision.
aaa = x1.1
b = x2.1
nTry% = 0     'Number of sign reversals of upper bracket value.
Impl1.1:
'Evaluate implicit set of equations with one guess, aaa:
	'Cl1 is part of epidermal resistance dependent on leaf water potential:
	'Cl2 is part of leaf epidermal resistance dependent on solar radiation.
	Cl1 = AFGEN(ClvsWP(), aaa)  'Get leaf epidermal resistance.
	Cl = 2! / (1! / Cl1 + 1! / Cl2)
	Rl = 1! / (Cl * LAI)
	CRV = CRH + Rl   'Resistance to latent heat flux = resistance to
			'sensible heat flux + leaf epidermal resistance.
			'This means that aerodynamic resistances to sensible
			'and latent heat fluxes are taken equal.
	'IF Ta <= 0 THEN CRV = 1E+27'Resistance approaches infinity if frozen.
	'Note that negative transpiration rates are possible:
	LTr = (Hl - Ha) * LH / CRV
	fa = WPSeff + WPCrMx - LTr * SRCr / (1000! * LH * LAI) - aaa
'Evaluate implicit set of equations for other guess, b:
	Cl1 = AFGEN(ClvsWP(), b)
	Cl = 2! / (1! / Cl1 + 1! / Cl2)
	Rl = 1! / (Cl * LAI)
	CRV = CRH + Rl   'Resistance to latent heat flux = resistance to
			'sensible heat flux + leaf epidermal resistance.
			'This means that aerodynamic resistances to sensible
			'and latent heat fluxes are taken equal.
	'IF Ta <= 0 THEN CRV = 1E+27'Resistance approaches infinity if frozen.
	'Note that negative transpiration rates are possible:
	LTr = (Hl - Ha) * LH / CRV
	fb = WPSeff + WPCrMx - LTr * SRCr / (1000! * LH * LAI) - b

IF fb * fa > 0! THEN
	'Root is not bracketed, try again or quit:
	nTry% = nTry% + 1
	IF nTry% < 2 THEN
	  'Reverse sign of one quess and try again:
	  'b = -x2.1
	  aaa = -x1.1  '6-9-94. SRE
	  GOTO Impl1.1
	ELSE
	  PRINT "Root must be bracketed for function IMPL1.  Fatal error."
	  PRINT "Debug data will be dumped to device #6."
	  PRINT #6, "Root must be bracketed for function IMPL1.  Fatal error."
	  PRINT #6, "Function"; nFunc%; " time"; sTime; ", day"; cdnum%
	  PRINT #6, "fa "; fa; " fb "; fb; " lower guess (aaa) "; aaa; " upper guess (b) "; b
	  PRINT #6, "DebugDat(): sTime, Ta, DPTc, Tl, WPotCr, Tl, Hl, Temp(1), Kond(1), Ts, Ta, ppot(1), theta(1)"
	  FOR i% = di% + 1 TO 40
	   FOR k% = 0 TO 12
		  PRINT #6, DebugDat(i%, k%);
	   NEXT k%
	   PRINT #6,
	  NEXT i%
	  FOR i% = 1 TO di%
	   FOR k% = 0 TO 12
		  PRINT #6, DebugDat(i%, k%);
	   NEXT k%
	   PRINT #6,
	  NEXT i%
	  ErrCode = 9
	  EXIT FUNCTION
	END IF
END IF
fc = fb
FOR iter% = 1 TO itmax%
	IF fb * fc > 0! THEN
	c = aaa                 'Rename aaa,b,c and adjust
	fc = fa                 'bounding interval d.
	d = b - aaa
	e = d
	END IF
	IF ABS(fc) < ABS(fb) THEN
	aaa = b
	b = c
	c = aaa
	fa = fb
	fb = fc
	fc = fa
	END IF
	Tol1 = 2! * eps * ABS(b) + .5 * tol.1      'Convergence check
	xm = .5 * (c - b)
	IF ABS(xm) <= Tol1 OR fb = 0! THEN
	'Convergence achieved, exit with function equal to answer:
	IMPL1! = b
	EXIT FUNCTION
	END IF
	IF ABS(e) >= Tol1 AND ABS(fa) > ABS(fb) THEN
	ss = fb / fa             'Attempt inverse quadratic interpolation.
	IF aaa = c THEN
	   P = 2! * xm * ss
	   Q = 1! - ss
	ELSE
		Q = fa / fc
		r = fb / fc
		P = ss * (2! * xm * Q * (Q - r) - (b - aaa) * (r - 1!))
		Q = (Q - 1!) * (r - 1!) * (ss - 1!)
	END IF
	IF P > 0! THEN Q = -Q   'Check whether in bounds.
	P = ABS(P)
	IF 2! * P < AMIN(3! * xm * Q - ABS(Tol1 * Q), ABS(e * Q)) THEN
		e = d               'Accept interpolation.
		d = P / Q
	ELSE
		d = xm              'Interpolation failed, use bisection.
		e = d
	END IF
	ELSE
	d = xm                  'Bounds decreasing too slowly, use bisection.
	e = d
	END IF
	aaa = b                       'Move last best quess to aaa.
	fa = fb
	IF ABS(d) > Tol1 THEN       'Evaluate new trial root.
	b = b + d
	ELSE
	b = b + SIGN!(Tol1, xm)
	END IF
'Evaluate implicit set of equations with new guess, b:
	'Cl1 is part of epidermal resistance dependent on leaf water potential:
	Cl1 = AFGEN(ClvsWP(), b)
	'Cl2 is part of leaf epidermal resistance dependent on solar radiation.
	Cl = 2! / (1! / Cl1 + 1! / Cl2)
	Rl = 1! / (Cl * LAI)
	CRV = CRH + Rl   'Resistance to latent heat flux = resistance to
			'sensible heat flux + leaf epidermal resistance.
			'This means that aerodynamic resistances to sensible
			'and latent heat fluxes are taken equal.
	'IF Ta <= 0 THEN CRV = 1E+27'Resistance approaches infinity if frozen.
	'Note that negative transpiration rates are possible:
	LTr = (Hl - Ha) * LH / CRV
	fb = WPSeff + WPCrMx - LTr * SRCr / (1000! * LH * LAI) - b

NEXT iter%
PRINT "IMPL1 exceeding maximum iterations."
PRINT #6, "IMPL1 exceeding maximum iterations."
IMPL1! = b
END FUNCTION

'==============================================================================
'FUNCTION IMPL2! (BYVAL x1!)
FUNCTION IMPL2! (x1!)
'Returns the root of the implicit set of equations describing the canopy
'(leaf) temperature, Tl.  The implicit set of equations is (xTl is the quess
'for leaf temperature):
'Note: Sigma is a defined constant that does not have to be explicitly shared.
'    LwRc = SIGMA * (xTL + 273.16) ^ 4!     'Longwave emission by crop [W/m^2].
'      'Net radiant energy input to canopy (treated as a single layer),
'      'i.e. the net radiation balance of the crop [W/m^2], p. 11:
'    NRBC = GR * ABSC + (1! - FTSR) * (SkL - LwRc)
'      'Leaf absolute humidity [kg/m^2]:
'      'Note: this is not corrected for leaf water potential.
'    Hl = 1.323 * EXP(17.27 * xTL / (237.3 + xTL)) / (273.16 + xTL)
'    LH = LHfT(xTL)                   'Latent heat of vaporization [J/kg].
'    LTr = (Hl - Ha) * LH / CRV       'Transpiration rate [W/m^2].
'    SHCA = -NRBC + LTr          'Canopy - air sensible heat exchange [W/m^2].
'    Func2 = Ta - SHCA * CRH / SH - xTL

'The root must lie between x1 and x2 to a tolerance, tol.
'Reference: Press, W.H., B.P. Flannery, S.A. Teulosky, and W.T. Vetterling.
'1986. Numerical recipes - the art of scientific computing. Cambridge
'University Press. p. 253.
SHARED sTime, di%, DebugDat(), ErrCode, cdnum%            'For root finder.
SHARED GR, ABSC, FTSR, SkL, LWRc, Hl, Ha, LH, CRV    'For leaf temperature.
SHARED CRH
SHARED SH   'Sensible heat capacity of the air, [J/m^3/Deg.C]:
SHARED Ta, SHCA, NRBC, LTr                  'For leaf temperature.
itmax% = 100  'Set max. no. of iterations.
eps = 3E-08   'Machine floating point precision.
aaa = x1!
b = x2.2
nTry% = 0     'Number of sign reversals of upper bracket value.
Impl2.1:
'Evaluate implicit set of equations with one guess, aaa:
	'LwRc = SIGMA * (aaa + 273.16) ^ 4!     'Longwave emission by crop [W/m^2].
	dum = 273.16 + aaa
	LWRc = SIGMA * dum * dum * dum * dum    'Longwave emission by crop [W/m^2].
	 'Net radiant energy input to canopy (treated as a single layer),
	 'i.e. the net radiation balance of the crop [W/m^2], p. 11:
	NRBC = GR * ABSC + (1! - FTSR) * (SkL - LWRc)
	 'Leaf absolute humidity [kg/m^2]:
	 'Note: this is not corrected for leaf water potential.
	Hl = 1.323 * EXP(17.27 * aaa / (237.3 + aaa)) / (273.16 + aaa)
	LH = LHfT(aaa)                   'Latent heat of vaporization [J/kg].
	LTr = (Hl - Ha) * LH / CRV       'Transpiration rate [W/m^2].
	SHCA = -NRBC + LTr         'Canopy - air sensible heat exchange [W/m^2].
	fa = Ta - SHCA * CRH / SH - aaa
'Evaluate implicit set of equations for other guess, b:
	'LwRc = SIGMA * (b + 273.16) ^ 4!     'Longwave emission by crop [W/m^2].
	dum = 273.16 + b
	LWRc = SIGMA * dum * dum * dum * dum    'Longwave emission by crop [W/m^2].
	 'Net radiant energy input to canopy (treated as a single layer),
	 'i.e. the net radiation balance of the crop [W/m^2], p. 11:
	NRBC = GR * ABSC + (1! - FTSR) * (SkL - LWRc)
	 'Leaf absolute humidity [kg/m^2]:
	 'Note: this is not corrected for leaf water potential.
	Hl = 1.323 * EXP(17.27 * b / (237.3 + b)) / (273.16 + b)
	LH = LHfT(b)                   'Latent heat of vaporization [J/kg].
	LTr = (Hl - Ha) * LH / CRV       'Transpiration rate [W/m^2].
	SHCA = -NRBC + LTr         'Canopy - air sensible heat exchange [W/m^2].
	fb = Ta - SHCA * CRH / SH - b

IF fb * fa > 0! THEN
	'Root is not bracketed, try again or quit:
	nTry% = nTry% + 1
	IF nTry% < 2 THEN
	  'Reverse sign of one quess and try again:
	  b = -x2.2
	  GOTO Impl2.1
	ELSE
	  PRINT "Root must be bracketed for function IMPL2.  Fatal error."
	  PRINT "Debug data will be dumped to device #6."
	  PRINT #6, "Root must be bracketed for function IMPL2.  Fatal error."
	  PRINT #6, "Function"; nFunc%; " time"; sTime; ", day"; cdnum%
	  PRINT #6, "fa "; fa; " fb "; fb; " lower guess (aaa) "; aaa; " upper guess (b) "; b
	  PRINT #6, "DebugDat(): sTime, Ta, DPTc, Tl, WPotCr, Tl, Hl, Temp(1), Kond(1), Ts, Ta, ppot(1), theta(1)"
	  FOR i% = di% + 1 TO 40
	   FOR k% = 0 TO 12
		  PRINT #6, DebugDat(i%, k%);
	   NEXT k%
	   PRINT #6,
	  NEXT i%
	  FOR i% = 1 TO di%
	   FOR k% = 0 TO 12
		  PRINT #6, DebugDat(i%, k%);
	   NEXT k%
	   PRINT #6,
	  NEXT i%
	  ErrCode = 9
	  EXIT FUNCTION
	END IF
END IF
fc = fb
FOR iter% = 1 TO itmax%
	IF fb * fc > 0! THEN
	c = aaa                 'Rename aaa,b,c and adjust
	fc = fa                 'bounding interval d.
	d = b - aaa
	e = d
	END IF
	IF ABS(fc) < ABS(fb) THEN
	aaa = b
	b = c
	c = aaa
	fa = fb
	fb = fc
	fc = fa
	END IF
	Tol1 = 2! * eps * ABS(b) + .5 * tol.2!     'Convergence check
	xm = .5 * (c - b)
	IF ABS(xm) <= Tol1 OR fb = 0! THEN
	'Convergence achieved, exit with function equal to answer:
	IMPL2! = b
	EXIT FUNCTION
	END IF
	IF ABS(e) >= Tol1 AND ABS(fa) > ABS(fb) THEN
	ss = fb / fa             'Attempt inverse quadratic interpolation.
	IF aaa = c THEN
	   P = 2! * xm * ss
	   Q = 1! - ss
	ELSE
		Q = fa / fc
		r = fb / fc
		P = ss * (2! * xm * Q * (Q - r) - (b - aaa) * (r - 1!))
		Q = (Q - 1!) * (r - 1!) * (ss - 1!)
	END IF
	IF P > 0! THEN Q = -Q   'Check whether in bounds.
	P = ABS(P)
	IF 2! * P < AMIN(3! * xm * Q - ABS(Tol1 * Q), ABS(e * Q)) THEN
		e = d               'Accept interpolation.
		d = P / Q
	ELSE
		d = xm              'Interpolation failed, use bisection.
		e = d
	END IF
	ELSE
	d = xm                  'Bounds decreasing too slowly, use bisection.
	e = d
	END IF
	aaa = b                       'Move last best quess to aaa.
	fa = fb
	IF ABS(d) > Tol1 THEN       'Evaluate new trial root.
	b = b + d
	ELSE
	b = b + SIGN!(Tol1, xm)
	END IF
'Evaluate implicit set of equations with new guess:
	'LwRc = SIGMA * (b + 273.16) ^ 4!     'Longwave emission by crop [W/m^2].
	dum = 273.16 + b
	LWRc = SIGMA * dum * dum * dum * dum    'Longwave emission by crop [W/m^2].
	 'Net radiant energy input to canopy (treated as a single layer),
	 'i.e. the net radiation balance of the crop [W/m^2], p. 11:
	NRBC = GR * ABSC + (1! - FTSR) * (SkL - LWRc)
	 'Leaf absolute humidity [kg/m^2]:
	 'Note: this is not corrected for leaf water potential.
	Hl = 1.323 * EXP(17.27 * b / (237.3 + b)) / (273.16 + b)
	LH = LHfT(b)                   'Latent heat of vaporization [J/kg].
	LTr = (Hl - Ha) * LH / CRV       'Transpiration rate [W/m^2].
	SHCA = -NRBC + LTr         'Canopy - air sensible heat exchange [W/m^2].
	fb = Ta - SHCA * CRH / SH - b

NEXT iter%
PRINT "IMPL2 exceeding maximum iterations."
PRINT #6, "IMPL2 exceeding maximum iterations."
IMPL2! = b
END FUNCTION

'==============================================================================
'FUNCTION IMPL3! (BYVAL x1!)
FUNCTION IMPL3! (x1!)
'Returns the root of the implicit set of equations describing the soil surface
'temperature, Ts.  The implicit set of equations is (xTs is the quess for
'soil surface temperature):
'Note: SIGMA is a defined constant that does not have to be explicitly shared.
'    LWRS = SIGMA * (xTs + 273.16) ^ 4      'Long wave emission by soil.
'     'Net radiation balance at soil surface [W m^-2]:
'    NRBS = GR * ABSSa + (1! - FTSR) * LwRc + FTSR * SkL - LWRS
'     'Potential humidity at soil surface temperature:
'    Ho = 1.323 * EXP(17.27 * xTs / (237.3 + xTs)) / (273.16 + xTs)
'    Hs = Ho * EXP(ppot(1) / (46.97 * (xTs + 273.16))) 'Actual humidity.
'    LH = LHfT(xTs)                         'Latent heat of vaporization.
'    LEvS = (Hs - Ha) * LH / Rs             'Latent heat flux.
'    A = (xTs - Ta) * SH / Rs               'Sensible heat flux.
'    s = NRBS - A - LEvS                    'Soil heat flux.
'    Func3 = Temp(1) + s * Dist(1) / Kond(1) - xTs

'The root must lie between x1 and x2 to a tolerance, tol.
'Reference: Press, W.H., B.P. Flannery, S.A. Teulosky, and W.T. Vetterling.
'1986. Numerical recipes - the art of scientific computing. Cambridge
'University Press. p. 253.
SHARED sTime, di%, DebugDat(), ErrCode, cdnum%            'For root finder.
SHARED LWRS, NRBS, GR, ABSSa, FTSR, LWRc, SkL       'For soil surface temp.
SHARED Ho, Hs, ppot(), LH, LEvS, Ha, Rs, Temp()     'For soil surface temp.
SHARED Dist(), Kond(), S, a, Ta
SHARED SH        'Sensible heat capacity of the air, [J/m^3/Deg.C]:
SHARED SA
SHARED TaK       'Air temp. [K]
SHARED TsBf      'Surface temperature from previous time step [C].

itmax% = 100  'Set max. no. of iterations.
eps = 3E-08   'Machine floating point precision.
aaa = x1!
b = x2.3
nTry% = 0     'Number of sign reversals of upper bracket value.
Impl3.1:
'Evaluate implicit set of equations with one guess, aaa:
	'LWRS = SIGMA * (aaa + 273.16) ^ 4      'Long wave emission by soil.
	dum = 273.16 + aaa
	LWRS = SIGMA * dum * dum * dum * dum    'Longwave emission by soil [W/m^2].
	'Net radiation balance at soil surface [W m^-2]:
	NRBS = GR * ABSSa + (1! - FTSR) * LWRc + FTSR * SkL - LWRS
	'Potential humidity at soil surface temperature:
	Ho = 1.323 * EXP(17.27 * aaa / (237.3 + aaa)) / (273.16 + aaa)
	Hs = Ho * EXP(ppot(1) / (46.97 * (aaa + 273.16))) 'Actual humidity.
	LH = LHfT(aaa)                         'Latent heat of vaporization.
	LEvS = (Hs - Ha) * LH / Rs             'Latent heat flux.
	'IF TsBf <= 0 AND LEvs > 0 THEN LEvs = 0   'No Evap. if frozen, frost possible. 6-9-94, SRE
	IF LEvS < 0 THEN LEvS = 0              'no dew or frost
	a = (aaa - Ta) * SH / Rs               'Sensible heat flux.
	S = NRBS - a - LEvS                    'Soil heat flux.
	fa = Temp(1) + S * Dist(1) / Kond(1) - aaa
'Evaluate implicit set of equations for other guess, b:
	'LWRS = SIGMA * (b + 273.16) ^ 4      'Long wave emission by soil.
	dum = 273.16 + b
	LWRS = SIGMA * dum * dum * dum * dum    'Longwave emission by soil [W/m^2].
	'Net radiation balance at soil surface [W m^-2]:
	NRBS = GR * ABSSa + (1! - FTSR) * LWRc + FTSR * SkL - LWRS
	'Potential humidity at soil surface temperature:
	Ho = 1.323 * EXP(17.27 * b / (237.3 + b)) / (273.16 + b)
	Hs = Ho * EXP(ppot(1) / (46.97 * (b + 273.16))) 'Actual humidity.
	LH = LHfT(b)                         'Latent heat of vaporization.
	LEvS = (Hs - Ha) * LH / Rs             'Latent heat flux.
	'IF TsBf < 0 AND LEvs > 0 THEN LEvs = 0   'No Evap. if frozen, frost possible. 6-9-94, SRE
	IF LEvS < 0 THEN LEvS = 0              'no dew or frost
	a = (b - Ta) * SH / Rs               'Sensible heat flux.
	S = NRBS - a - LEvS                    'Soil heat flux.
	fb = Temp(1) + S * Dist(1) / Kond(1) - b

IF fb * fa > 0! THEN
	'Root is no bracketed, try again or quit:
	nTry% = nTry% + 1
	IF nTry% < 2 THEN
	  'Reverse sign of one quess and try again:
	  b = -x2.3
	  GOTO Impl3.1
	ELSE
	  PRINT "Root must be bracketed for function IMPL3.  Fatal error."
	  PRINT "Debug data will be dumped to device #6."
	  PRINT #6, "Root must be bracketed for function IMPL3.  Fatal error."
	  PRINT #6, "Function"; nFunc%; " time"; sTime; ", day"; cdnum%
	  PRINT #6, "fa "; fa; " fb "; fb; " lower guess (aaa) "; aaa; " upper guess (b) "; b
	  PRINT #6, "DebugDat(): sTime, Ta, DPTc, Tl, WPotCr, Tl, Hl, Temp(1), Kond(1), Ts, Ta, ppot(1), theta(1)"
	  FOR i% = di% + 1 TO 40
	   FOR k% = 0 TO 12
		  PRINT #6, DebugDat(i%, k%);
	   NEXT k%
	   PRINT #6,
	  NEXT i%
	  FOR i% = 1 TO di%
	   FOR k% = 0 TO 12
		  PRINT #6, DebugDat(i%, k%);
	   NEXT k%
	   PRINT #6,
	  NEXT i%
	  ErrCode = 9
	  EXIT FUNCTION
	END IF
END IF
fc = fb
FOR iter% = 1 TO itmax%
	IF fb * fc > 0! THEN
	c = aaa                 'Rename aaa,b,c and adjust
	fc = fa                 'bounding interval d.
	d = b - aaa
	e = d
	END IF
	IF ABS(fc) < ABS(fb) THEN
	aaa = b
	b = c
	c = aaa
	fa = fb
	fb = fc
	fc = fa
	END IF
	Tol1 = 2! * eps * ABS(b) + .5 * tol.3!     'Convergence check
	xm = .5 * (c - b)
	IF ABS(xm) <= Tol1 OR fb = 0! THEN
	'Convergence achieved, exit with function equal to answer:
	IMPL3! = b
	EXIT FUNCTION
	END IF
	IF ABS(e) >= Tol1 AND ABS(fa) > ABS(fb) THEN
	ss = fb / fa             'Attempt inverse quadratic interpolation.
	IF aaa = c THEN
	   P = 2! * xm * ss
	   Q = 1! - ss
	ELSE
		Q = fa / fc
		r = fb / fc
		P = ss * (2! * xm * Q * (Q - r) - (b - aaa) * (r - 1!))
		Q = (Q - 1!) * (r - 1!) * (ss - 1!)
	END IF
	IF P > 0! THEN Q = -Q   'Check whether in bounds.
	P = ABS(P)
	IF 2! * P < AMIN(3! * xm * Q - ABS(Tol1 * Q), ABS(e * Q)) THEN
		e = d               'Accept interpolation.
		d = P / Q
	ELSE
		d = xm              'Interpolation failed, use bisection.
		e = d
	END IF
	ELSE
	d = xm                  'Bounds decreasing too slowly, use bisection.
	e = d
	END IF
	aaa = b                       'Move last best quess to aaa.
	fa = fb
	IF ABS(d) > Tol1 THEN       'Evaluate new trial root.
	b = b + d
	ELSE
	b = b + SIGN!(Tol1, xm)
	END IF
'Evaluate implicit set of equations with new guess, b:
	'LWRS = SIGMA * (b + 273.16) ^ 4      'Long wave emission by soil.
	dum = 273.16 + b
	LWRS = SIGMA * dum * dum * dum * dum    'Longwave emission by soil [W/m^2].
	'Net radiation balance at soil surface [W m^-2]:
	NRBS = GR * ABSSa + (1! - FTSR) * LWRc + FTSR * SkL - LWRS
	'Potential humidity at soil surface temperature:
	Ho = 1.323 * EXP(17.27 * b / (237.3 + b)) / (273.16 + b)
	Hs = Ho * EXP(ppot(1) / (46.97 * (b + 273.16))) 'Actual humidity.
	LH = LHfT(b)                         'Latent heat of vaporization.
	LEvS = (Hs - Ha) * LH / Rs             'Latent heat flux.
	'IF TsBf < 0 AND LEvs > 0 THEN LEvs = 0   'No Evap. if frozen, frost possible. 6-9-94, SRE
	IF LEvS < 0 THEN LEvS = 0              'no dew or frost
	a = (b - Ta) * SH / Rs               'Sensible heat flux.
	S = NRBS - a - LEvS                    'Soil heat flux.
	fb = Temp(1) + S * Dist(1) / Kond(1) - b

NEXT iter%
PRINT "IMPL3 exceeding maximum iterations."
PRINT #6, "IMPL3 exceeding maximum iterations."
IMPL3! = b
END FUNCTION

'==============================================================================
'SUB INTGRL (icy, BYVAL dxdy)
SUB INTGRL (icy, dxdy)
'Performs rectangular integration.
SHARED TStep
	icy = icy + dxdy * TStep
END SUB

'==============================================================================
'FUNCTION LHfT (BYVAL temperature!)
FUNCTION LHfT (temperature!)
	'Latent heat of vaporization [J/kg].
	LHfT = 2494630! - 2247! * temperature!
END FUNCTION

'==============================================================================
'FUNCTION SIGN! (BYVAL a1!, BYVAL a2!)
FUNCTION SIGN! (a1!, a2!)
'Returns a1! with the sign of a2!.
	IF a2! >= 0! THEN SIGN! = ABS(a1!) ELSE SIGN! = -ABS(a1!)
END FUNCTION


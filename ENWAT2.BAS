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
CASE 17
   PRINT "Improper path or file name, or the file "; DripFile$; " does not exist."
   RESUME NEXT
CASE 18
   PRINT "Name of file containing drip irrigation times and rates"
   PRINT "not found in file "; InfoFile$
   PRINT #2, "Name of file containing drip irrigation times and rates"
   PRINT #2, "not found in file "; InfoFile$
   RESUME NEXT
CASE ELSE
END SELECT
PRINT "Program will end now.  Press any key."
SLEEP
END

ConstantInputErr:
IF i% > 0 THEN RESUME 'EndConstantInput  'Probably ran out of space in string array.
PRINT "Error in reading in constants from file: "; Constants$
PRINT "Examine file ENWATBAL.PRN.  Press any key to end program ...."
SLEEP
END

'=============================================================================
MkDirErr:
IF ERR = 75 OR ERR = 53 THEN RESUME NEXT
PRINT "Error no."; ERR; "while making directory??"
SLEEP: END

'=============================================================================
OpenFileErr:
'PRINT "Error no."; ERR; "opening file."
RESUME NEXT

'==============================================================================
SUB AFGENInit
SHARED ClvsWP(), ClvsGR(), TevsKO(), SoilAL()
SHARED TvsP1(), TvsP2(), TvsP3(), TvsP4(), TvsP5(), TvsP6(), TvsP7(), TvsP8()
SHARED TvsP9()
SHARED TvsC1(), TvsC2(), TvsC3(), TvsC4(), TvsC5(), TvsC6(), TvsC7(), TvsC8()
SHARED TvsC9()
SHARED AfGenFile$, dummy(), Porsty(), SatCon
'Open file:
File% = FREEFILE
OPEN "i", File%, AfGenFile$
'Input lines and find code for table to input:
DO
        LINE INPUT #File%, in$
        PRINT in$
        id$ = LTRIM$(RTRIM$(in$))
        GOSUB SelectTable
LOOP WHILE NOT EOF(File%)
CLOSE File%
EXIT SUB

SelectTable:
SELECT CASE id$
CASE "CLVSWP"
'Leaf water potential [m] vs. epidermal conductance [m/s]:
        IF Restart$ = "" THEN PRINT #2, "Data for array ClvsWP(), leaf water potential vs. epidermal conductance:"
        INPUT #File%, np%
        REDIM ClvsWP(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, ClvsWP(i%, j%)
        IF Restart$ = "" THEN PRINT #2, ClvsWP(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "CLVSGR"
'Incident solar radiation [W m^-2] vs. epidermal conductance [m/s]:
        IF Restart$ = "" THEN PRINT #2, "Data for array ClvsGr(), solar radiation vs. epidermal conductance:"
        INPUT #File%, np%
        REDIM ClvsGR(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, ClvsGR(i%, j%)
        IF Restart$ = "" THEN PRINT #2, ClvsGR(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "TVSP1"
'Volume fraction of water vs. soil water potential [m]:
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsP1(), volumetric water content vs. ";
        PRINT #2, "soil water potential [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsP1(np%, 2)
        REDIM dummy(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
                INPUT #File%, TvsP1(i%, j%)
                IF Restart$ = "" THEN PRINT #2, TvsP1(i%, j%);
        NEXT j%
        dummy(i%, 1) = TvsP1(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP1(i%, 1)  'potential in AFGEN using dummy().
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Find theta for potential of 0.0 m and call this soil porosity:
        Porsty(1) = AFGEN(dummy(), 0!)
        REDIM dummy(0, 0)
        IF Restart$ = "" THEN
        PRINT #2, "Porosity for horizon #1:"; Porsty(1)
        END IF
CASE "TVSP2"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsP2(), volumetric water content vs. ";
        PRINT #2, "soil water potential [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsP2(np%, 2)
        REDIM dummy(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
                INPUT #File%, TvsP2(i%, j%)
                IF Restart$ = "" THEN PRINT #2, TvsP2(i%, j%);
        NEXT j%
        dummy(i%, 1) = TvsP2(i%, 2)
        dummy(i%, 2) = TvsP2(i%, 1)
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Find theta for potential of 0.0 m and call this soil porosity:
        Porsty(2) = AFGEN(dummy(), 0!)
        REDIM dummy(0, 0)
        IF Restart$ = "" THEN
        PRINT #2, "Porosity for horizon #2:"; Porsty(2)
        END IF
CASE "TVSP3"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsP3(), volumetric water content vs. ";
        PRINT #2, "soil water potential [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsP3(np%, 2)
        REDIM dummy(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsP3(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsP3(i%, j%);
        NEXT j%
        dummy(i%, 1) = TvsP3(i%, 2)
        dummy(i%, 2) = TvsP3(i%, 1)
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Find theta for potential of 0.0 m and call this soil porosity:
        Porsty(3) = AFGEN(dummy(), 0!)
        REDIM dummy(0, 0)
        IF Restart$ = "" THEN
        PRINT #2, "Porosity for horizon #3:"; Porsty(3)
        END IF
CASE "TVSP4"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsP4(), volumetric water content vs. ";
        PRINT #2, "soil water potential [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsP4(np%, 2)
        REDIM dummy(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsP4(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsP4(i%, j%);
        NEXT j%
        dummy(i%, 1) = TvsP4(i%, 2)
        dummy(i%, 2) = TvsP4(i%, 1)
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Find theta for potential of 0.0 m and call this soil porosity:
        Porsty(4) = AFGEN(dummy(), 0!)
        REDIM dummy(0, 0)
        IF Restart$ = "" THEN
        PRINT #2, "Porosity for horizon #4:"; Porsty(4)
        END IF
CASE "TVSP5"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsP5(), volumetric water content vs. ";
        PRINT #2, "soil water potential [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsP5(np%, 2)
        REDIM dummy(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsP5(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsP5(i%, j%);
        NEXT j%
        dummy(i%, 1) = TvsP5(i%, 2)
        dummy(i%, 2) = TvsP5(i%, 1)
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Find theta for potential of 0.0 m and call this soil porosity:
        Porsty(5) = AFGEN(dummy(), 0!)
        REDIM dummy(0, 0)
        IF Restart$ = "" THEN
        PRINT #2, "Porosity for horizon #5:"; Porsty(5)
        END IF
CASE "TVSP6"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsP6(), volumetric water content vs. ";
        PRINT #2, "soil water potential [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsP6(np%, 2)
        REDIM dummy(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsP6(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsP6(i%, j%);
        NEXT j%
        dummy(i%, 1) = TvsP6(i%, 2)
        dummy(i%, 2) = TvsP6(i%, 1)
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Find theta for potential of 0.0 m and call this soil porosity:
        Porsty(6) = AFGEN(dummy(), 0!)
        REDIM dummy(0, 0)
        IF Restart$ = "" THEN
        PRINT #2, "Porosity for horizon #6:"; Porsty(6)
        END IF
CASE "TVSP7"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsP7(), volumetric water content vs. ";
        PRINT #2, "soil water potential [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsP7(np%, 2)
        REDIM dummy(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsP7(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsP7(i%, j%);
        NEXT j%
        dummy(i%, 1) = TvsP7(i%, 2)
        dummy(i%, 2) = TvsP7(i%, 1)
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Find theta for potential of 0.0 m and call this soil porosity:
        Porsty(7) = AFGEN(dummy(), 0!)
        REDIM dummy(0, 0)
        IF Restart$ = "" THEN
        PRINT #2, "Porosity for horizon #7:"; Porsty(7)
        END IF
CASE "TVSP8"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsP8(), volumetric water content vs. ";
        PRINT #2, "soil water potential [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsP8(np%, 2)
        REDIM dummy(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsP8(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsP8(i%, j%);
        NEXT j%
        dummy(i%, 1) = TvsP8(i%, 2)
        dummy(i%, 2) = TvsP8(i%, 1)
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Find theta for potential of 0.0 m and call this soil porosity:
        Porsty(8) = AFGEN(dummy(), 0!)
        REDIM dummy(0, 0)
        IF Restart$ = "" THEN
        PRINT #2, "Porosity for horizon #8:"; Porsty(8)
        END IF
CASE "TVSP9"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsP9(), volumetric water content vs. ";
        PRINT #2, "soil water potential [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsP9(np%, 2)
        REDIM dummy(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsP9(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsP9(i%, j%);
        NEXT j%
        dummy(i%, 1) = TvsP9(i%, 2)
        dummy(i%, 2) = TvsP9(i%, 1)
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Find theta for potential of 0.0 m and call this soil porosity:
        Porsty(9) = AFGEN(dummy(), 0!)
        REDIM dummy(0, 0)
        IF Restart$ = "" THEN
        PRINT #2, "Porosity for horizon #9:"; Porsty(9)
        END IF
CASE "TVSC1"
'Volume fraction of water vs. soil hydraulic conductivity [m/s]:
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsC1(), volumetric water content vs. ";
        PRINT #2, "hydraulic conductivity [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsC1(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsC1(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsC1(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
'Saturated conductivity for surface soil layer, SATCON [m/s]:
        SatCon = AFGEN(TvsC1(), (Porsty(1)))
        IF Restart$ = "" THEN
        PRINT #2, "Saturated conductivity of surface soil layer (SatCon):"; SatCon; " m/s."
        END IF
CASE "TVSC2"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsC2(), volumetric water content vs. ";
        PRINT #2, "hydraulic conductivity [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsC2(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsC2(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsC2(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "TVSC3"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsC3(), volumetric water content vs. ";
        PRINT #2, "hydraulic conductivity [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsC3(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsC3(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsC3(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "TVSC4"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsC4(), volumetric water content vs. ";
        PRINT #2, "hydraulic conductivity [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsC4(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsC4(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsC4(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "TVSC5"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsC5(), volumetric water content vs. ";
        PRINT #2, "hydraulic conductivity [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsC5(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsC5(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsC5(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "TVSC6"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsC6(), volumetric water content vs. ";
        PRINT #2, "hydraulic conductivity [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsC6(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsC6(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsC6(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "TVSC7"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsC7(), volumetric water content vs. ";
        PRINT #2, "hydraulic conductivity [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsC7(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsC7(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsC7(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "TVSC8"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsC8(), volumetric water content vs. ";
        PRINT #2, "hydraulic conductivity [m]:"
        END IF
        INPUT #File%, np%
                REDIM TvsC8(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsC8(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsC8(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "TVSC9"
        IF Restart$ = "" THEN
        PRINT #2, "Data for array TvsC9(), volumetric water content vs. ";
        PRINT #2, "hydraulic conductivity [m]:"
        END IF
        INPUT #File%, np%
        REDIM TvsC9(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TvsC9(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TvsC9(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "TEVSKO"
'Soil temperature [Deg.C] vs. heat conductivity by vapor [W m^-1 C^-1]:
        IF Restart$ = "" THEN PRINT #2, "Data for array TevsKO(), soil temp. vs. heat conductivity by vapor:"
        INPUT #File%, np%
        REDIM TevsKO(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, TevsKO(i%, j%)
        IF Restart$ = "" THEN PRINT #2, TevsKO(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE "SOILAL"
'Soil water content [m^3/m^3] in first layer vs. soil albedo:
        IF Restart$ = "" THEN PRINT #2, "Data for array SoilAL(), soil water content vs. soil albedo."
        INPUT #File%, np%
        REDIM SoilAL(np%, 2)
        FOR i% = 1 TO np%
        FOR j% = 1 TO 2
        INPUT #File%, SoilAL(i%, j%)
        IF Restart$ = "" THEN PRINT #2, SoilAL(i%, j%);
        NEXT j%
        IF Restart$ = "" THEN PRINT #2,
        NEXT i%
CASE ELSE
END SELECT
RETURN

END SUB

'==============================================================================
'FUNCTION ARCOS (BYVAL xdum)
FUNCTION ARCOS (xdum)
'Returns inverse cosine.
IF ABS(xdum) > 1! THEN PRINT "Fatal error in ARCOS.": SLEEP: END
IF xdum = 0! THEN
        ARCOS = HalfPi
ELSE
        IF xdum = -1! THEN
        ARCOS = Pi
        ELSE
        IF xdum = 1 THEN
                ARCOS = 0!
        ELSE
                ARCOS = 1.570796 - ATN(xdum / SQR(1! - xdum * xdum))
        END IF
        END IF
END IF
END FUNCTION

'==============================================================================
'FUNCTION ARSIN (BYVAL xdum)
FUNCTION ARSIN (xdum)
'Returns inverse sine.
IF ABS(xdum) > 1! THEN PRINT "Error in ARSIN.": SLEEP: END
IF xdum = -1! THEN
        ARSIN = -HalfPi
ELSE
        IF xdum = 1! THEN
        ARSIN = HalfPi
        ELSE
        ARSIN = ATN(xdum / SQR(1! - xdum * xdum))
        END IF
END IF
END FUNCTION

'=============================================================================
SUB FinGraphFile
SHARED path$, hTime, EndDay%, sDay%
        PRINT "Finishing graph files."
        ON ERROR GOTO OpenFileErr
        CLOSE 'Close all files.
        IF HourlyOutputFlag > 0 THEN
        'Finish hourly output files:

         OPEN "o", #8, path$ + "Profile1\data1.tem"
         PRINT #8, "cdnum%; hTime; water contents & temperatures:"
         WRITE #8, "data1.tem"
         WRITE #8, ""
         PRINT #8, 5, hTime, 0, 0, 0
         di = 5
         DataFile$ = path$ + "Profile1\data1"
         GOSUB AddData

         OPEN "o", #8, path$ + "temp\data1.tem"
         PRINT #8, "cdnum%; hTime; Ta; DPTc; Tl; Ts; Temp(1)"
         WRITE #8, "data1.tem"
         WRITE #8, ""
         PRINT #8, 5, hTime, 0, 0, 0
         di = 5
         DataFile$ = path$ + "temp\data1"
         GOSUB AddData

         OPEN "o", #8, path$ + "cond\data1.tem"
         PRINT #8, "cdnum%; hTime; Cl; Cl1; Cl2"
         WRITE #8, "data1.tem"
         WRITE #8, ""
         PRINT #8, 3, hTime, 0, 0, 0
         di = 3
         DataFile$ = path$ + "cond\data1"
         GOSUB AddData

'         OPEN "o", #8, path$ + "resist\data1.tem"
'         PRINT #8, "cdnum%; hTime; Rl; CRH; CRV"
'         WRITE #8, "data1.tem"
'         WRITE #8, ""
'         PRINT #8, 3, hTime, 0, 0, 0
'         di = 3
'         DataFile$ = path$ + "resist\data1"
'         GOSUB AddData

         OPEN "o", #8, path$ + "hum\data1.tem"
         PRINT #8, "cdnum%; hTime; Hl; Ha; Ho; Hs"
         WRITE #8, "data1.tem"
         WRITE #8, ""
         PRINT #8, 4, hTime, 0, 0, 0
         di = 4
         DataFile$ = path$ + "hum\data1"
         GOSUB AddData

         OPEN "o", #8, path$ + "flux1\data1.tem"
         PRINT #8, "cdnum%; hTime; GR; SkL; NRBC; NRBS; LwRc; Albedo"
         WRITE #8, "data1.tem"
         WRITE #8, ""
         PRINT #8, 6, hTime, 0, 0, 0
         di = 6
         DataFile$ = path$ + "flux1\data1"
         GOSUB AddData

         OPEN "o", #8, path$ + "flux2\data1.tem"
         PRINT #8, "cdnum%; hTime; SHCA; LWRS; LEvS; a; S; LTr; RCNeg; RCPos; RCSum"  'FLUX2
         WRITE #8, "data1.tem"
         WRITE #8, ""
         PRINT #8, 5, hTime, 0, 0, 0
         di = 5
         DataFile$ = path$ + "flux2\data1"
         GOSUB AddData

         OPEN "o", #8, path$ + "pot\data1.tem"
         PRINT #8, "cdnum%; hTime; WPSeff; WPotCr; ppot(1)"
         WRITE #8, "data1.tem"
         WRITE #8, ""
         PRINT #8, 3, hTime, 0, 0, 0
         di = 3
         DataFile$ = path$ + "pot\data1"
         GOSUB AddData

         OPEN "o", #8, path$ + "non\data1.tem"
         PRINT #8, "cdnum%; hTime; LAI; FTSR; ABSC; ABSSa; CRH / Ra; Rs / Ra"
         WRITE #8, "data1.tem"
         WRITE #8, ""
         PRINT #8, 4, hTime, 0, 0, 0
         di = 4
         DataFile$ = path$ + "non\data1"
         GOSUB AddData
        END IF

        OPEN "o", #8, path$ + "et\data1.tem"
         PRINT #8, "cdnum%; dEvap; dTrans; dET; dG; dInf; dPrec; dDrain; dRunoff; WBalance; dRs"
         WRITE #8, "data1.tem"
         WRITE #8, ""
         PRINT #8, 9, EndDay% - sDay% + 1, 0, 0, 0
         di = 9
         DataFile$ = path$ + "et\data1"
         GOSUB AddData

        OPEN "o", #8, path$ + "profile\data1.tem"
         DataFile$ = path$ + "profile\data1"
         GOSUB AddData
        ON ERROR GOTO 0
EXIT SUB

'=============================================================================
AddData: 'Add data lines to graph file.
        OPEN "i", #2, DataFile$
        WHILE NOT EOF(2)
        LINE INPUT #2, in$
        PRINT #8, in$
        WEND
        'FOR i = 1 TO di
        '   PRINT #2, 0, 0
        'NEXT i
        CLOSE #2
        KILL DataFile$
        CLOSE #8
RETURN

END SUB

'==============================================================================
SUB GetCommandLine
'Get name of file containing data file names from command line:
SHARED InfoFile$, Mode2$, Restart$, FErr%, path$
    InfoFile$ = RTRIM$(COMMAND$)
    Mode2$ = "o" 'set mode to open files for output.
    ON ERROR GOTO FileOpenErr.2
    IF INSTR(InfoFile$, " ") > 0 THEN
        'there is more than one file name in command line string.
        'get rest of string.
        Restart$ = LTRIM$(MID$(InfoFile$, INSTR(InfoFile$, " ")))
        InfoFile$ = MID$(InfoFile$, 1, INSTR(InfoFile$, " ") - 1) 'get file.
        FErr% = 6
        OPEN "i", #1, Restart$
        INPUT #1, path$  'get path for output files.
        CLOSE #1
        Mode2$ = "a"   'set mode to open files for append.
    END IF
    ON ERROR GOTO 0
END SUB

'==============================================================================
SUB GetConstants (Constants$)
SHARED Zo, Z, WPCrMx, SRCr, LowBoundGrad, ZeroCdNum%
SHARED DetCap, Lat, AvBarP, FErr%, TStepL, TStepH, Header$()
    i% = 0
    PRINT #2, : PRINT #2, DATE$, TIME$
    PRINT #2, "Constants that may have changed for this run:"
    ON ERROR GOTO FileOpenErr.2
    FErr% = 5
    OPEN "i", #7, Constants$     'Constants for this run are in this file.
    ON ERROR GOTO 0
    ON ERROR GOTO ConstantInputErr
    PRINT "Header for file "; Constants$
    LINE INPUT #7, Header$(1): PRINT Header$(1)
 
    INPUT #7, Zo, Zo$     'Surface roughness length [m].
    INPUT #7, Z, Z$       'Reference elevation [m] for measurement of wind
                          'speed, air temperature & dew point temperature.
    INPUT #7, WPCrMx, WPCrMx$    'Maximum crop water potential [m].
    INPUT #7, SRCr, SRCr$      'Specific hydraulic resistance of the crop [s].
    INPUT #7, DetCap, DetCap$    'Ponded water detention capacity [m].
    INPUT #7, Lat, Lat$       'Latitude [Deg.].
    INPUT #7, AvBarP, AvBarP$    'Average barometric pressure [mbar] at Bushland.
    INPUT #7, TStepL, TStepL$    'Lower limit of time step in s.
    INPUT #7, TStepH, TStepH$    'Upper limit of time step in s.
    'Gradient to use for lower soil boundary water flux (m/m):
    INPUT #7, LowBoundGrad, LowBoundGrad$
    INPUT #7, ZeroCdNum%, ZeroCdNum$  'day of year on which to invoke impeeding layer.
    i% = 1
    DO
        i% = i% + 1
        IF NOT EOF(7) THEN LINE INPUT #7, Header$(i%) ELSE EXIT DO
    LOOP
EndConstantInput:
    ON ERROR GOTO 0
    CLOSE #7

'Print user defined header:
    FOR j% = 2 TO i% - 1
        PRINT #2, Header$(j%)
        PRINT #6, Header$(j%)
    NEXT j%
'Print constants:
    PRINT #2, Header$(1)
    PRINT #6, Header$(1)
    'Surface roughness length [m].
    PRINT #2, Zo, Zo$
    PRINT #6, Zo, Zo$
    'Reference elevation [m].
    PRINT #2, Z, Z$
    PRINT #6, Z, Z$
    'Maximum crop water potential [m].
    PRINT #2, WPCrMx, WPCrMx$
    PRINT #6, WPCrMx, WPCrMx$
    'Specific hydraulic resistance of the crop [s].
    PRINT #2, SRCr, SRCr$
    PRINT #6, SRCr, SRCr$
    'Ponded water detention capacity [m].
    PRINT #2, DetCap, DetCap$
    PRINT #6, DetCap, DetCap$
    'Latitude [Deg.].
    PRINT #2, Lat, Lat$
    PRINT #6, Lat, Lat$
    'Average barometric pressure [mbar] at Bushland.
    PRINT #2, AvBarP, AvBarP$
    PRINT #6, AvBarP, AvBarP$
    'Lower limit of time step in s.
    PRINT #2, TStepL, TStepL$
    PRINT #6, TStepL, TStepL$
    'Upper limit of time step in s.
    PRINT #2, TStepH, TStepH$
    PRINT #6, TStepH, TStepH$
    'Gradient for water flux at lower soil boundary (m/m):
    PRINT #2, LowBoundGrad, LowBoundGrad$
    PRINT #6, LowBoundGrad, LowBoundGrad$
    'Day of which to invoke impeeding layer:
    PRINT #2, ZeroCdNum%, ZeroCdNum$
    PRINT #6, ZeroCdNum%, ZeroCdNum$

    REDIM Header$(0)
    
EXIT SUB
END SUB

'==============================================================================
SUB GetDayYear
SHARED sDay%, EndDay%, InitDayFile$, sYear%, Plant$, Restart$, EndYear%
'Find starting day:
    sDay% = VAL(MID$(InitDayFile$, INSTR(InitDayFile$, ".") + 1, 3))
    IF sDay% < 1 OR sDay% > 366 THEN
        PRINT "Error in naming file containing initial conditions."
        PRINT "File name was "; InitDayFile$; "."
        PRINT "See documentation.  Program will end now.  Press any key."
        SLEEP
        END
    END IF
    sYear% = VAL(MID$(Plant$, INSTR(Plant$, ".") + 1, 3))
    IF sYear% < 1 THEN
        PRINT "Error in naming file containing weather data for each day."
        PRINT "See documentation.  Program will end now.  Press any key."
        SLEEP
        END
    END IF
    IF Restart$ = "" THEN
        PRINT "The simulation will start on day"; sDay%; " of"; sYear%
    ELSE
        PRINT "The simulation will re-start at 0 hours on day";
        PRINT 1 + VAL(MID$(Restart$, INSTR(Restart$, ".") + 1, 3));
        PRINT " of"; sYear%
        'sleep
    END IF
    IF EndDay% < sDay% AND EndYear% <= sYear% THEN
        PRINT "Ending day is before starting day.  Program will end."
        PRINT "Start Day: "; sDay%
        PRINT "EndDay:    "; EndDay%
        PRINT "Start year:"; sYear%
        PRINT "End year:  "; EndYear%
        PRINT "Press any key now...."
        SLEEP
        END
    ELSE
        PRINT "Simulation will stop on day"; EndDay%; "of"; EndYear%
    END IF
END SUB

'==============================================================================
SUB GetFileNames
'Get names of files needed by ENWATBAL:
SHARED InfoFile$, InitDayFile$, IrrPrecip$, Plant$, Constants$, AfGenFile$
SHARED DripFile$, path$, Y$, EndDay%, FErr%, Meteo$, WMode%, HourlyOutputFlag
SHARED EndYear%

    f% = FREEFILE
    ON ERROR GOTO FileOpenErr.2
    FErr% = 1
    OPEN "i", #f%, InfoFile$
    INPUT #f%, EndDay%, EndYear%, dum$ 'Day & year after which to stop simulation.
    INPUT #f%, path$, dum$   'Get path for output files.
    INPUT #f%, Y$, dum$      'Get redirection code, "P" means printer, "F" means file.
    Y$ = UCASE$(Y$)
    FErr% = 16
    IF Y$ <> "P" AND Y$ <> "F" THEN ERROR 255
    INPUT #f%, WMode%, dum$  'Get code for diurnal or 1/2 hourly weather data, 1 means half-hourly.

'Redirect some output to printer or file and open device chosen as #6:
    RedirectOutput
    SetEpson   'Set Epson printer.

'Date, time & version stamp:
    PRINT #6, DATE$, TIME$, version$

    PRINT #6, "INPUT FILES USED BY ENWATBAL.BAS:"

    'File containing initial moisture and temperature profiles and
    'soil layer thicknesses:
    FErr% = 10
    INPUT #f%, InitDayFile$, dum$
    PRINT #6, InitDayFile$, dum$

    'File containing irrigation and precipitation data for each day:
    FErr% = 11
    INPUT #f%, IrrPrecip$, dum$
    PRINT #6, IrrPrecip$, dum$

    'File containing diurnal average weather and plant growth data for each day:
    FErr% = 12
    INPUT #f%, Plant$, dum$
    PRINT #6, Plant$, dum$

    'File containing constants & header information which may be changed for each run.
    FErr% = 13
    INPUT #f%, Constants$, dum$
    PRINT #6, Constants$, dum$

    'File containing tables of data for the function generator AFGEN.
    FErr% = 14
    INPUT #f%, AfGenFile$, dum$
    PRINT #6, AfGenFile$, dum$

    'Flag equals 1 for hourly output, othewise only daily (midnight)
    'output occurs).
    INPUT #f%, HourlyOutputFlag, dum$

    'IF WMode% = 1 THEN
        'File containing 1/2 hourly weather data for each day.
        FErr% = 15
        INPUT #f%, Meteo$, dum$
        PRINT #6, Meteo$, dum$
    'END IF

    FErr% = 18
    INPUT #f%, DripFile$, dum$
    PRINT #6, DripFile$, dum$

    IF HourlyOutputFlag > 0 THEN
         PRINT #6, "Output will be every"; HourlyOutputFlag; "hours."
    ELSE
         PRINT #6, "Output will be daily at midnight."
    END IF

    ON ERROR GOTO 0

    CLOSE #f%

    PRINT "The initial conditions are found in file "; InitDayFile$
    PRINT "The irrigation and precipitation data are in file "; IrrPrecip$
    PRINT "The plant growth and other weather data are in file "; Plant$
    PRINT "The constants for this run are in file "; Constants$
    IF WMode% = 1 THEN PRINT "The 1/2 hourly weather data are in file "; Meteo$
    PRINT "The data tables for function AFGEN are found in file "; AfGenFile$
    PRINT "The rates and times for drip irrigation are found in file "; DripFile$
END SUB

SUB GetTheta1Lim
SHARED TvsP1(), TvsP2(), TvsP3(), TvsP4(), TvsP5(), TvsP6(), TvsP7(), TvsP8()
SHARED TvsP9(), HorNum%(), Theta1Lim, Restart$
i% = 1
DIM dummy(i%, 2)

'Find array that contains Theta vs. potential for 1st soil layer.
SELECT CASE HorNum%(1)
CASE 1
    np% = UBOUND(TvsP1)
    REDIM dummy(np%, 2)
    FOR i% = 1 TO np%
        dummy(i%, 1) = TvsP1(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP1(i%, 1)  'potential in AFGEN using dummy().
    NEXT i%
CASE 2
    np% = UBOUND(TvsP2)
    REDIM dummy(np%, 2)
    FOR i% = 1 TO np%
        dummy(i%, 1) = TvsP2(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP2(i%, 1)  'potential in AFGEN using dummy().
    NEXT i%
CASE 3
    np% = UBOUND(TvsP3)
    REDIM dummy(np%, 2)
    FOR i% = 1 TO np%
        dummy(i%, 1) = TvsP3(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP3(i%, 1)  'potential in AFGEN using dummy().
    NEXT i%
CASE 4
    np% = UBOUND(TvsP4)
    REDIM dummy(np%, 2)
    FOR i% = 1 TO np%
        dummy(i%, 1) = TvsP4(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP4(i%, 1)  'potential in AFGEN using dummy().
    NEXT i%
CASE 5
    np% = UBOUND(TvsP5)
    REDIM dummy(np%, 2)
    FOR i% = 1 TO np%
        dummy(i%, 1) = TvsP5(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP5(i%, 1)  'potential in AFGEN using dummy().
    NEXT i%
CASE 6
    np% = UBOUND(TvsP6)
    REDIM dummy(np%, 2)
    FOR i% = 1 TO np%
        dummy(i%, 1) = TvsP6(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP6(i%, 1)  'potential in AFGEN using dummy().
    NEXT i%
CASE 7
    np% = UBOUND(TvsP7)
    REDIM dummy(np%, 2)
    FOR i% = 1 TO np%
        dummy(i%, 1) = TvsP7(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP7(i%, 1)  'potential in AFGEN using dummy().
    NEXT i%
CASE 8
    np% = UBOUND(TvsP8)
    REDIM dummy(np%, 2)
    FOR i% = 1 TO np%
        dummy(i%, 1) = TvsP8(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP8(i%, 1)  'potential in AFGEN using dummy().
    NEXT i%
CASE 9
    np% = UBOUND(TvsP9)
    REDIM dummy(np%, 2)
    FOR i% = 1 TO np%
        dummy(i%, 1) = TvsP9(i%, 2)  'Rearrange so THETA is a function of
        dummy(i%, 2) = TvsP9(i%, 1)  'potential in AFGEN using dummy().
    NEXT i%
CASE ELSE
PRINT "Horizon not selected in search for Theta1Lim."
PRINT "Program will end now.  Press any key ..."
SLEEP: END
END SELECT

'Find theta for potential of -0.1 m:
    Theta1Lim = AFGEN(dummy(), -.1)
    REDIM dummy(0, 0)
    IF Restart$ = "" THEN
        PRINT #2, "Water content threshold for time step algorithm:"
        PRINT #2, " Theta1Lim ="; Theta1Lim
    END IF
END SUB

'==============================================================================
SUB InitDepthDist
SHARED nLayers%, Depth(), SLThick(), Dist(), BothLayerThick(), Restart$
'Initialize Dist() and Depth() arrays:
    Depth(1) = .5 * SLThick(1)
    Dist(1) = Depth(1)
    IF Restart$ = "" THEN
        PRINT #2, "Distance from surface to middle of layer I, and ";
        PRINT #2, "distance between layers I and I-1."
        PRINT #2, " 1"; Depth(1); Dist(1)
    END IF
    FOR i% = 2 TO nLayers%
        Depth(i%) = Depth(i% - 1) + .5 * (SLThick(i% - 1) + SLThick(i%))
        Dist(i%) = Depth(i%) - Depth(i% - 1)
        BothLayerThick(i%) = SLThick(i%) + SLThick(i% - 1)
        IF Restart$ = "" THEN PRINT #2, i%; Depth(i%); Dist(i%)
    NEXT i%
END SUB

'==============================================================================
SUB InitProfile
SHARED path$, InitDayFile$, nLayers%, SLThick(), Theta(), Temp(), FErr%
SHARED Restart$, HorNum%(), WaterOutPutFlag%(), TempOutPutFlag%()
SHARED ZeroCondLayer%() 'Flag for layer that has zero hydraulic conductivity at bottom.
SHARED DripLayer%()     'Flag for layer with drip emitter.
SHARED DripFlag%        '>0 if a buried drip emitter exists.

    ON ERROR GOTO FileOpenErr.2
    FErr% = 4
    OPEN "i", #7, InitDayFile$   'Initial profile data is in this file.
    ON ERROR GOTO 0
'Define soil layers and initial conditions of water content and temperature:
    'Input from file containing initial profile:
'First find number of layers and dimension arrays:
    LINE INPUT #7, dum$                           'Get header out of the way.
    nLayers% = 0: Done% = 0
    DO
        FOR i% = 1 TO 8
            IF NOT EOF(7) THEN
                INPUT #7, dum
            ELSE
                Done% = 1
                EXIT FOR
            END IF
        NEXT i%
        IF Done% = 1 THEN EXIT DO
        nLayers% = nLayers% + 1
    LOOP
    CLOSE #7
'Now redimension arrays:
    REDIM SLThick(nLayers% + 1), Theta(nLayers% + 1)
    REDIM Temp(nLayers% + 1), HorNum%(nLayers% + 1)
    REDIM WaterOutPutFlag%(nLayers%), TempOutPutFlag%(nLayers%)
    REDIM ZeroCondLayer%(nLayers%)
    REDIM DripLayer%(nLayers%)
'Now get data:
    OPEN "i", #7, InitDayFile$  'Initial profile data is in this file.
    LINE INPUT #7, dum$                           'Get header out of the way.
    IF Restart$ = "" THEN PRINT #2, "Soil layer thickness, initial water content and temperature:"
    FOR i% = 1 TO nLayers%
        INPUT #7, SLThick(i%), Theta(i%), Temp(i%), HorNum%(i%), WaterOutPutFlag%(i%)
        INPUT #7, TempOutPutFlag%(i%), ZeroCondLayer%(i%), DripLayer%(i%)
        IF DripLayer%(i%) > 0 THEN DripFlag% = i%
        IF Restart$ = "" THEN PRINT #2, SLThick(i%); Theta(i%); Temp(i%); HorNum%(i%); WaterOutPutFlag%(i%); TempOutPutFlag%(i%), ZeroCondLayer%(i%), DripLayer%(i%)
    NEXT i%
    CLOSE #7
END SUB

'===============================================================================
SUB InitSoilWater
SHARED iWater, nLayers%, Depth(), Porsty()
SHARED VolW(), VolH(), Theta(), SLThick(), Temp(), HorNum%()
SHARED Restart$
'Note VHCapW and VHCapS are constants that are passed implicitly.
'Initialize water content and heat capacity of soil layers:
    IF Restart$ = "" THEN iWater = 0!  'Initialize iWater if not re-starting.
    FOR i% = 1 TO nLayers%
        Pors = Porsty(HorNum%(i%))   'Soil porosity, horizon HorNum%(i%).
        SoilVol = 1! - Pors 'Soil solids fraction volume, horizon HorNum%(i%).
        'Calculate depth of water in this layer:
        VolW(i%) = Theta(i%) * SLThick(i%) 'SRE changed to initialize VolW()
        'Calculate iWater if not re-starting, i.e. when program first starts:
        IF Restart$ = "" THEN iWater = iWater + VolW(i%)
        iVHCap = VHCapW * Theta(i%) + SoilVol * VHCapS 'Volumetric heat capacity.
        VolH(i%) = Temp(i%) * SLThick(i%) * iVHCap    'SRE changed. Vol. heat.
        PRINT #2, USING "### "; i%;
        PRINT #2, USING "#.#### "; VolW(i%);
        PRINT #2, USING "#.####^^^^ "; iVHCap; VolH(i%)
    NEXT i%
END SUB

'==============================================================================
SUB InitWeather
SHARED StartDay%, sDay%, Restart$, WInput1(), WMode%, Meteo(), MIndex%
'Get first 2 days daily data needed for simulation.  Put data for first day
'in row 3 and data for previous day in row 2 of WInput1().  In subroutine
'InitDay the data for today will be put in row 2, data for yesterday in
'row 1 and data for tomorrow in row 3:

'Initialize weather data array Meteo() if WMode%=1.  Data for last time period
'(1/2 h) of previous day will be element MIndex%-1 in Meteo() (elements are numbered
'0 to MIndex%), element MIndex% will be data for 1st period of today.  In subroutine
'InitDay the data in the element MIndex%-1 will be placed in element 0 and data in
'element MIndex% will be placed in element 1, and next MIndex%-2 elements will be filled
'with rest of data for today, then element MIndex% will be filled with data from 1st
'period tomorrow.
    StartDay% = sDay%
    IF LEN(Restart$) THEN        'IF Restart$ <> "" THEN
        'Re-evaluate starting day.
        'Add 1 to day indicated by suffix of file RESTART.XXX:
        StartDay% = VAL(MID$(Restart$, INSTR(Restart$, ".") + 1, 3)) + 1
        PRINT "Start Day for restart will be:"; StartDay%
    END IF
    DO
        IF NOT EOF(4) THEN INPUT #4, cdYear%
        FOR j% = 1 TO 13
            SWAP WInput1(j%, 2), WInput1(j%, 3)
            IF NOT EOF(4) THEN
                INPUT #4, WInput1(j%, 3)
            ELSE
                PRINT "Error in reading plant growth and weather data."
                PRINT "Program will end now.  Press any key ....."
                SLEEP: END
            END IF
        NEXT j%
    LOOP WHILE WInput1(1, 3) < StartDay%
    IF WMode% = 1 THEN
        'Initialize 1/2 hourly weather data:
        DO
            IF NOT EOF(1) THEN INPUT #1, hlfYear%
            FOR j% = 1 TO 9
                'IF Meteo(1, 49) = StartDay% THEN
                IF Meteo(1, MIndex%) = StartDay% THEN
                    PRINT "Meteorological data on 1/2 hr for day"; StartDay%; ":"
                END IF
                'SWAP Meteo(j%, 48), Meteo(j%, 49)
                SWAP Meteo(j%, MIndex% - 1), Meteo(j%, MIndex%)
                IF NOT EOF(1) THEN
                    'INPUT #1, Meteo(j%, 49)
                    INPUT #1, Meteo(j%, MIndex%)
                    'IF Meteo(1, 49) = StartDay% THEN
                    IF Meteo(1, MIndex%) = StartDay% THEN
                        'PRINT Meteo(j%, 49)
                        PRINT Meteo(j%, MIndex%)
                    END IF
                ELSE
                    PRINT "Error reading 1/2 hourly weather data."
                    PRINT "Program will end now.  Press any key ..."
                    SLEEP: END
                END IF
            NEXT j%
        'LOOP WHILE Meteo(1, 49) < StartDay%   'Stop when 1st period of today is found.
        LOOP WHILE Meteo(1, MIndex%) < StartDay%   'Stop when 1st period of today is found.
    END IF
    IF cdYear% <> hlfYear% THEN
        PRINT "Years do not match in files "; Plant$; " and "; Meteo$
        PRINT "Please read documentation. Program will end now."
        END
    END IF
END SUB

'==============================================================================
SUB OpenFiles
SHARED FErr%, Plant$, Mode2$, path$, IrrPrecip$, Meteo$, DripFile$
SHARED WMode%, Period, Offset

    ON ERROR GOTO FileOpenErr.2
    IF WMode% = 1 THEN
        FErr% = 9
        'Daily weather data (1/2 hour periods) is in this file.
        OPEN "i", #1, Meteo$
        PRINT "Header for file "; Meteo$; ":"
        FOR i% = 1 TO 2
            LINE INPUT #1, in$: PRINT in$
        NEXT i%
        'Period is the time (h) between data values, Offset is the time (h)
        'from midnight to the 1st data value.  These are not currently used.
        INPUT #1, Period, Offset, Period$
        PRINT Period$
        PRINT Period, Offset
    END IF

    FErr% = 2
    OPEN "i", #4, Plant$    'Daily mean plant growth & weather data is in this file.
    PRINT "Header for file "; Plant$; ":"
    FOR i% = 1 TO 4
        LINE INPUT #4, in$: PRINT in$
    NEXT i%

    'OPEN Mode2$, #3, path$ + "test.out"
    'OPEN "scrn:" FOR OUTPUT AS #1

    FErr% = 3
    OPEN "i", #5, IrrPrecip$     'Daily rainfall is in this file.
    PRINT "Header for file "; IrrPrecip$
    LINE INPUT #5, in$: PRINT in$

    'File showing initial conditions, etc.
    OPEN Mode2$, #2, path$ + "enwatbal.deb"

    FErr% = 17
    OPEN "i", #3, DripFile$    'Daily drip rates and times are in this file.
    PRINT "Header for file "; DripFile$
    LINE INPUT #3, in$
    PRINT in$

    ON ERROR GOTO 0
    FErr% = 0
END SUB

'=============================================================================
SUB OpenGraFile
'Open files for graphing:
SHARED path$, HourlyOutputFlag, Mode2$, Restart$, nLayers%, Depth()
ON ERROR GOTO MkDirErr
    IF HourlyOutputFlag > 0 THEN
    'Create subdirectories for hourly output files.
        MKDIR path$ + "temp"
        MKDIR path$ + "cond"
'        MKDIR path$ + "resist"
        MKDIR path$ + "hum"
        MKDIR path$ + "flux1"
        MKDIR path$ + "flux2"
        MKDIR path$ + "pot"
        MKDIR path$ + "non"
        MKDIR path$ + "profile1"
    END IF
    MKDIR path$ + "et"
    MKDIR path$ + "profile"
    'Erase previous files if present.
    KILL path$ + "temp\DATA1.TEM"
    KILL path$ + "cond\DATA1.TEM"
'    KILL path$ + "resist\DATA1.TEM"
    KILL path$ + "hum\DATA1.TEM"
    KILL path$ + "flux1\DATA1.TEM"
    KILL path$ + "flux2\DATA1.TEM"
    KILL path$ + "pot\DATA1.TEM"
    KILL path$ + "non\DATA1.TEM"
    KILL path$ + "theta\DATA1.TEM"
    KILL path$ + "et\DATA1.TEM"
    KILL path$ + "profile\DATA1.TEM"
    ON ERROR GOTO 0
    ON ERROR GOTO OpenFileErr
    IF HourlyOutputFlag > 0 THEN
    'Open files for hourly output.
        OPEN Mode2$, #7, path$ + "Profile1\data1"
        IF Restart$ = "" THEN   'no need to initialize files if restarting.
            PRINT #7, " 0  0  0  2"
            WRITE #7, "1"
        END IF
        OPEN Mode2$, #8, path$ + "temp\data1"
        IF Restart$ = "" THEN   'no need to initialize files if restarting.
            PRINT #8, " 0  0  0  2"
            WRITE #8, "1"
            PRINT #8, "0  0  0  3"
            WRITE #8, "1"
            PRINT #8, "0  0  0  4"
            WRITE #8, "1"
            PRINT #8, "0  0  0  5"
            WRITE #8, "1"
            PRINT #8, "0  0  0  6"
            WRITE #8, "1"
        END IF
        OPEN Mode2$, #9, path$ + "cond\data1"
        IF Restart$ = "" THEN   'no need to initialize files if restarting.
            PRINT #9, " 0  0  0  2"
            WRITE #9, "1"
            PRINT #9, "0  0  0  3"
            WRITE #9, "1"
            PRINT #9, "0  0  0  4"
            WRITE #9, "1"
        END IF
        'OPEN Mode2$, #10, path$ + "resist\data1"
        'IF Restart$ = "" THEN   'no need to initialize files if restarting.
        '    PRINT #10, " 0  0  0  2"
        '    WRITE #10, "1"
        '    PRINT #10, "0  0  0  3"
        '    WRITE #10, "1"
        '    PRINT #10, "0  0  0  4"
        '    WRITE #10, "1"
        'END IF
        OPEN Mode2$, #11, path$ + "hum\data1"
        IF Restart$ = "" THEN   'no need to initialize files if restarting.
            PRINT #11, " 0  0  0  2"
            WRITE #11, "1"
            PRINT #11, "0  0  0  3"
            WRITE #11, "1"
            PRINT #11, "0  0  0  4"
            WRITE #11, "1"
            PRINT #11, "0  0  0  5"
            WRITE #11, "1"
        END IF
        OPEN Mode2$, #12, path$ + "flux1\data1"
        IF Restart$ = "" THEN   'no need to initialize files if restarting.
            PRINT #12, " 0  0  0  1"
            WRITE #12, "1"
            PRINT #12, "0  0  0  2"
            WRITE #12, "1"
            PRINT #12, " 0  0  0  3"
            WRITE #12, "1"
            PRINT #12, "0  0  0  4"
            WRITE #12, "1"
            PRINT #12, "0  0  0  5"
            WRITE #12, "1"
            PRINT #12, "0  0  0  6"
            WRITE #12, "1"
        END IF
        OPEN Mode2$, #13, path$ + "flux2\data1"
        IF Restart$ = "" THEN   'no need to initialize files if restarting.
            PRINT #13, " 0  0  0  2"
            WRITE #13, "1"
            PRINT #13, "0  0  0  3"
            WRITE #13, "1"
            PRINT #13, "0  0  0  4"
            WRITE #13, "1"
            PRINT #13, "0  0  0  5"
            WRITE #13, "1"
            PRINT #13, "0  0  0  6"
            WRITE #13, "1"
        END IF
        OPEN Mode2$, #14, path$ + "pot\data1"
        IF Restart$ = "" THEN   'no need to initialize files if restarting.
            PRINT #14, " 0  0  0  2"
            WRITE #14, "1"
            PRINT #14, "0  0  0  3"
            WRITE #14, "1"
            PRINT #14, "0  0  0  4"
            WRITE #14, "1"
        END IF
        OPEN Mode2$, #15, path$ + "non\data1"
        IF Restart$ = "" THEN   'no need to initialize files if restarting.
            PRINT #15, " 0  0  0  1"
            WRITE #15, "1"
            PRINT #15, "0  0  0  2"
            WRITE #15, "1"
            PRINT #15, "0  0  0  3"
            WRITE #15, "1"
            PRINT #15, "0  0  0  4"
            WRITE #15, "1"
            PRINT #15, "0  0  0  5"
            WRITE #15, "1"
            PRINT #15, "0  0  0  6"
            WRITE #15, "1"
        END IF
    END IF
    OPEN Mode2$, #16, path$ + "et\data1"
    IF Restart$ = "" THEN   'no need to initialize files if restarting.
         PRINT #16, " 0  0  0  2"
         WRITE #16, "1"
         PRINT #16, "0  0  0  3"
         WRITE #16, "1"
         PRINT #16, "0  0  0  4"
         WRITE #16, "1"
         PRINT #16, "0  0  0  5"
         WRITE #16, "1"
         PRINT #16, "0  0  4  2"
         WRITE #16, "1"
         PRINT #16, "0  0  4  3"
         WRITE #16, "1"
         PRINT #16, "0  0  4  4"
         WRITE #16, "1"
         PRINT #16, "0  0  4  5"
         WRITE #16, "1"
         PRINT #16, "0  0  4  6"
         WRITE #16, "1"
    END IF
    CLOSE #16
    OPEN Mode2$, #16, path$ + "profile\data1"
    IF Restart$ = "" THEN   'no need to initialize files if restarting.
        FOR i% = 1 TO nLayers%
            PRINT #16, USING "### "; i%;
        NEXT i%
        PRINT #16,
        FOR i% = 1 TO nLayers%
            PRINT #16, Depth(i%);
        NEXT i%
        PRINT #16,
    END IF
    CLOSE #16
    ON ERROR GOTO 0
END SUB

'====================================================================
SUB OutputDay (f%)
'f% is the output device number.
SHARED cdnum%, sTime, EvTr, TrC, Evs, Infilt, GR, Ta, Tl, Ts, Ra, Rl, Rs
SHARED WPSeff, WPotCr, NRBC, LTr, SHCA, NRBS, LEvS, a, S, Runoff, Detain
SHARED CRH, CRV, MGR, SkL, Depth(), Theta(), Temp(), ppot(), Flux(), nFlux()
SHARED Flow(), nFlow(), RF(), RC()
SHARED CumEvap, dEvap, CumTrans, dTrans, CumPos, dPos
SHARED CumNeg, dNeg, CumG, dG, CumRC, CumET, dET, CumInf, dInf, CumPrec
SHARED dPrec, CumDrain, dDrain, CumRunoff, dRunoff, StorWater, WBalance
SHARED SLThick(), Cond(), Kond(), AvCond(), AvKond()
    PRINT #f%, "Day         sTime     EVTR      TRC       EVS       INFILT    GR"
    PRINT #f%, USING "####       "; cdnum%;
    PRINT #f%, USING "##.##^^^^ "; sTime; EvTr; TrC; Evs; Infilt; GR
    PRINT #f%, "Ta          Tl         Ts         Ra         Rl         Rs"
    PRINT #f%, USING "##.##^^^^  "; Ta; Tl; Ts; Ra; Rl; Rs
    PRINT #f%, "WPSeff      WPotCr"
    PRINT #f%, USING "##.##^^^^  "; WPSeff; WPotCr
    PRINT #f%, "NRBC        LTR        SHCA       NRBS       LEVS       A";
    PRINT #f%, "          S"
    PRINT #f%, USING "##.##^^^^  "; NRBC; LTr; SHCA; NRBS; LEvS; a; S
    PRINT #f%, "RUNFF       DETAIN     CRH        CRV        MGR        SKL"
    PRINT #f%, USING "##.##^^^^  "; Runoff; Detain; CRH; CRV; MGR; SkL
    PRINT #f%,
    PRINT #f%, "No.  DEPTH        THETA        TEMP         PPOT         FLUX";
    PRINT #f%, "        NET FLUX     FLOW         NET FLOW     ROOTF";
    PRINT #f%, "        ROOTUPTAKE"
    FOR i% = 1 TO nLayers% + 1
        PRINT #f%, USING "### "; i%;
        PRINT #f%, USING "##.####^^^^  "; Depth(i%); Theta(i%); Temp(i%);
        PRINT #f%, USING "##.####^^^^  "; ppot(i%); Flux(i%); nFlux(i%);
        PRINT #f%, USING "##.####^^^^  "; Flow(i%); nFlow(i%); RF(i%); RC(i%)
    NEXT i%
    PRINT #f%,
    PRINT #f%, "CumEvap   = "; CumEvap; TAB(40); "dEvap    = "; dEvap
    PRINT #f%, "CumTrans  = "; CumTrans; TAB(40); "dTrans   = "; dTrans
    PRINT #f%, "CumPos    = "; CumPos; TAB(40); "dPos     = "; dPos
    PRINT #f%, "CumNeg    = "; CumNeg; TAB(40); "dNeg     = "; dNeg
    PRINT #f%, "CumG      = "; CumG; TAB(40); "dG       = "; dG
    PRINT #f%, "CumRC     = "; CumRC
    PRINT #f%, "CumET     = "; CumET; TAB(40); "dET      = "; dET
    PRINT #f%, "CumInf    = "; CumInf; TAB(40); "dInf     = "; dInf
    PRINT #f%, "CumPrec   = "; CumPrec; TAB(40); "dPrec    = "; dPrec
    PRINT #f%, "CumDrain  = "; CumDrain; TAB(40); "dDrain   = "; dDrain
    PRINT #f%, "CumRunoff = "; CumRunoff; TAB(40); "dRunoff  = "; dRunoff
    PRINT #f%, "StorWater  = "; StorWater; TAB(40); "WBalance = "; WBalance
    PRINT #f%, " Depth     SLThick   Cond      Kond      PPot      AvCond    ";
    PRINT #f%, "AvKond    Flow      Flux      nFlow     nFlux     Theta"
    FOR i% = 0 TO nLayers% + 1
          PRINT #f%, USING "##.##^^^^ "; Depth(i%); SLThick(i%); Cond(i%); Kond(i%);
          PRINT #f%, USING "##.##^^^^ "; ppot(i%); AvCond(i%); AvKond(i%); Flow(i%);
          PRINT #f%, USING "##.##^^^^ "; Flux(i%); nFlow(i%); nFlux(i%); Theta(i%)
    NEXT i%
'    PRINT #f%, CHR$(12)  'Form feed.
END SUB

'==============================================================================
SUB PrintHeader
SHARED Header$()
        PRINT #2, DATE$, TIME$, version$
        PRINT #2, "ENWATBAL - a generic model to calculate evaporation and"
        PRINT #2, "transpiration from a crop, and profiles of soil water content"
        PRINT #2, "and temperature."
        PRINT #2, "Re-written in QuickBASIC v. 4.5 by S.R. Evett, 1989 - 1991"
        PRINT #2, "and named ENWATBAL.BAS."
        PRINT #2,
        PRINT #2, "Main reference: Van Bavel, C.H.M., and R.J. Lascano. 1987."
        PRINT #2, "ENWATBAL a numerical method to compute the water loss from a"
        PRINT #2, "crop by transpiration and evaporation.  Soil and Crop Sciences"
        PRINT #2, "Department, Texas Agricultural Experiment Station, Texas A&M"
        PRINT #2, "University."
        PRINT #2,
        PRINT #2,
END SUB

'==============================================================================
SUB RedirectOutput
SHARED Y$, path$, FErr%, Mode2$
    ON ERROR GOTO FileOpenErr.2
    IF Y$ = "F" THEN
        'Open file for output.
        FErr% = 7
        OPEN Mode2$, #6, path$ + "ENWATBAL.PRN"
        PRINT "Output will be to file " + path$ + "ENWATBAL.PRN."
    ELSE
        'Open printer for output.
        FErr% = 8
        OPEN "lpt1:" FOR OUTPUT AS #6
    END IF
    ON ERROR GOTO 0
END SUB

'==============================================================================
SUB ReStartFile
'Save data at end of run for use at start of continuing run:
SHARED Restart$, path$, dnum%, Hl, LH, TStep, TimeS, sTime, hTime, CumEvap
SHARED CumTrans, CumPos, CumNeg, CumRC, CumET, CumInf, CumPrec, CumDrain
SHARED CumRunoff, CumG, CumRs, StorWater, CumRootUptake, WBalance, iWater
SHARED CumDrip
SHARED nLayers%
SHARED CumEvBf, CumTrBf, CumPosBf, CumNegBf, CumETBf, CumInfBf, CumPrecBf
SHARED CumDrnBf, CumRunoffBf, CumGBf, CumRsBf, CumRootUptakeBf
SHARED CumDripBf
SHARED HorNum%(), Depth(), SLThick(), Cond(), Kond(), ppot(), AvCond(), AvKond()
SHARED Flow(), Flux(), nFlow(), nFlux(), Theta(), VolW(), VolH(), Temp()
SHARED cdnum%, EvTr, TrC, Evs, Infilt, GR, Ta, Tl, TsBf, Ra, Rl, Rs
SHARED WPSeff, WPotCr, NRBC, LTr, SHCA, NRBS, LEvS, a, S
SHARED Runoff, Detain, CRH, CRV, MGR, SkL
    WRITE #3, path$  'path for output files.
    PRINT #3, dnum%  'Number of simulated days since start.
    PRINT #3, Hl
    PRINT #3, LH
    PRINT #3, TStep
    PRINT #3, TimeS
    PRINT #3, sTime
    PRINT #3, hTime
    PRINT #3, CumEvap
    PRINT #3, CumTrans
    PRINT #3, CumPos
    PRINT #3, CumNeg
    PRINT #3, CumRC
    PRINT #3, CumET
    PRINT #3, CumInf
    PRINT #3, CumPrec
    PRINT #3, CumDrip
    PRINT #3, CumDrain
    PRINT #3, CumRunoff
    PRINT #3, CumG
    PRINT #3, CumRs
    PRINT #3, StorWater
    PRINT #3, CumRootUptake
    PRINT #3, WBalance
    PRINT #3, iWater     'initial profile water.
    PRINT #3, nLayers%   'No. of soil layers.
    PRINT #3, " Horizon No.  Depth     SLThick   Cond      Kond      PPot      AvCond";
    PRINT #3, "    AvKond    Flow      Flux      nFlow     nFlux     Theta";
    PRINT #3, "    VolW      VolH      Temp"
    FOR i% = 0 TO nLayers% + 1
        PRINT #3, HorNum%(i%); Depth(i%); SLThick(i%); Cond(i%);
        PRINT #3, Kond(i%); ppot(i%); AvCond(i%);
        PRINT #3, AvKond(i%); Flow(i%); Flux(i%);
        PRINT #3, nFlow(i%); nFlux(i%); Theta(i%);
        PRINT #3, VolW(i%); VolH(i%); Temp(i%)
    NEXT i%
    PRINT #3, "Day         sTime     EVTR      TRC       EVS       INFILT";
    PRINT #3, "    GR"
    PRINT #3, USING "####       "; cdnum%;
    'PRINT #3, USING "##.##^^^^ "; sTime; EvTr; TrC; Evs; Infilt; GR
    PRINT #3, sTime; EvTr; TrC; Evs; Infilt; GR
    PRINT #3, "Ta          Tl         Ts         Ra         Rl         Rs       "
    'PRINT #3, USING "##.##^^^^  "; Ta; Tl; TsBf; Ra; Rl; Rs
    PRINT #3, Ta; Tl; TsBf; Ra; Rl; Rs
    PRINT #3, "WPSeff      WPotCr"
    'PRINT #3, USING "##.##^^^^  "; WPSeff; WPotCr
    PRINT #3, WPSeff; WPotCr
    PRINT #3, "NRBC        LTR        SHCA       NRBS       LEVS       A          S"
    'PRINT #3, USING "##.##^^^^  "; NRBC; LTr; SHCA; NRBS; LEvS; A; s
    PRINT #3, NRBC; LTr; SHCA; NRBS; LEvS; a; S
    PRINT #3, "RUNFF       DETAIN     CRH        CRV        MGR        SKL"
    'PRINT #3, USING "##.##^^^^  "; Runoff; Detain; CRH; CRV; MGR; SkL
    PRINT #3, Runoff; Detain; CRH; CRV; MGR; SkL
    PRINT #3, "--------------"
END SUB

'==============================================================================
SUB RestartInput
'Get variables needed to resume simulation where it was ended by used:
SHARED Restart$, path$, dnum%, Hl, LH, TStep, TimeS, sTime, hTime, CumEvap
SHARED CumTrans, CumPos, CumNeg, CumRC, CumET, CumInf, CumPrec, CumDrain
SHARED CumRunoff, CumG, CumRs, StorWater, CumRootUptake, WBalance, iWater
SHARED CumDrip
SHARED nLayers%
SHARED CumEvBf, CumTrBf, CumPosBf, CumNegBf, CumETBf, CumInfBf, CumPrecBf
SHARED CumDrnBf, CumRunoffBf, CumGBf, CumRsBf, CumRootUptakeBf
SHARED CumDripBf
SHARED HorNum%(), Depth(), SLThick(), Cond(), Kond(), ppot(), AvCond(), AvKond()
SHARED Flow(), Flux(), nFlow(), nFlux(), Theta(), VolW(), VolH(), Temp()
SHARED cdnum%, EvTr, TrC, Evs, Infilt, GR, Ta, Tl, TsBf, Ra, Rl, Rs
SHARED WPSeff, WPotCr, NRBC, LTr, SHCA, NRBS, LEvS, a, S
SHARED Runoff, Detain, CRH, CRV, MGR, SkL
    'CLOSE #1
    f% = FREEFILE
    OPEN "i", #f%, Restart$
    INPUT #f%, path$  'path for output files.
    INPUT #f%, dnum%  'Number of simulated days since start.
    INPUT #f%, Hl
    INPUT #f%, LH
    INPUT #f%, TStep
    INPUT #f%, TimeS
    INPUT #f%, sTime
    INPUT #f%, hTime
    INPUT #f%, CumEvap
    INPUT #f%, CumTrans
    INPUT #f%, CumPos
    INPUT #f%, CumNeg
    INPUT #f%, CumRC
    INPUT #f%, CumET
    INPUT #f%, CumInf
    INPUT #f%, CumPrec
    INPUT #f%, CumDrip
    INPUT #f%, CumDrain
    INPUT #f%, CumRunoff
    INPUT #f%, CumG
    INPUT #f%, CumRs
    INPUT #f%, StorWater
    INPUT #f%, CumRootUptake
    INPUT #f%, WBalance
    INPUT #f%, iWater
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
    CumRsBf = CumRs
    CumRootUptakeBf = CumRootUptake
    INPUT #f%, nLayers%   'No. of soil layers.
    LINE INPUT #f%, dum$
    FOR i% = 0 TO nLayers% + 1
        INPUT #f%, HorNum%(i%), Depth(i%), SLThick(i%), Cond(i%)
        INPUT #f%, Kond(i%), ppot(i%), AvCond(i%)
        INPUT #f%, AvKond(i%), Flow(i%), Flux(i%)
        INPUT #f%, nFlow(i%), nFlux(i%), Theta(i%)
        INPUT #f%, VolW(i%), VolH(i%), Temp(i%)
    NEXT i%
    LINE INPUT #f%, dum$
    INPUT #f%, cdnum%
    INPUT #f%, sTime, EvTr, TrC, Evs, Infilt, GR
    LINE INPUT #f%, dum$
    INPUT #f%, Ta, Tl, TsBf, Ra, Rl, Rs
    LINE INPUT #f%, dum$
    INPUT #f%, WPSeff, WPotCr
    LINE INPUT #f%, dum$
    INPUT #f%, NRBC, LTr, SHCA, NRBS, LEvS, a, S
    LINE INPUT #f%, dum$
    INPUT #f%, Runoff, Detain, CRH, CRV, MGR, SkL
    CLOSE #f%
    PRINT #6, "Contents of restart file "; Restart$
    PRINT #2, "Contents of restart file "; Restart$
    OPEN "i", #f%, Restart$
    WHILE NOT EOF(f%)
       LINE INPUT #f%, in$
       PRINT #6, in$
       PRINT #2, in$
    WEND
    CLOSE #f%
    'OPEN "scrn:" FOR OUTPUT AS #1
END SUB

'==============================================================================
SUB SetEpson
'Set up Epson printer:
    'print #6, CHR$(27); "@";              'Reset printer.
'    WIDTH #6, 160                          'Set printer width to 160.
'    PRINT #6, CHR$(27); "0";               'Set line spacing to 1/8 inch.
'    PRINT #6, CHR$(27); "M";               'Select condensed elite type.
'    PRINT #6, CHR$(15);                    'Select condensed elite type.
'    PRINT #6, CHR$(27); "l"; CHR$(10);     'Set left margin to 10 spaces.
END SUB


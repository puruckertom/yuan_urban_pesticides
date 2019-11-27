L1: Header information (Pesticide Water Calculator (PWC), Version 1.59)
L2:Chemical ID (Optional)
L3:Degrade to daughter/gradnddaughter. if Parent only it is 1, parent, daughter 2
L4:True/False; True if Koc 
L5:Koc/Kd
L6:Water Column Metabolism Halflife
L7:Water Reference Temperature
L8:Benthic Metabolism Half-life
L9:Benthic Reference Temperature
L10:Aqueous Photolysis Half-life (days)
L11:Photolysis Reference Latitude (°C)
L12:Hydrolysis Half-life (days)
L13:Surface Soil Half-life (days)
L14:Soil Reference Temperature (°C)
L15:Foliar Degradation Half-life (days)
L16:MWT (g/mol)
L17:Vapor Pressure (torr)
L18:Solubility (mg/L)   
L19:Water Column Metabolism
L20:Bethin Metabolism
L21:Photolysis
L22:Hydrolysis
L23:Soil
L24:Foliar
L25:Air Diffusion Coefficient
L26:Henry’s Constant
L27:Heat of Henry   
L28: Factor by which temperature dependent process increase for every 10 increase temperature
L29:number of application
L30:Day (AbosluteDate)#times application number (max 50)
L31:Month(AbosluteDate)#times application number (max 50)
L32: Application Amount (kg/ha)#times application number (max 50)
L33:Specify years#times application number (max 50)
L34:Years (AbosluteDate)#times application number (max 50)
L35:Application Method 1-Below crop, 2-Above Crop, 3-Uniform etc.#times application number (max 50)
L36:application rate efficiency Reservoir#times application number (max 50)
L37:Drift fraction Reservoir #times application number (max 50)
L38:application rate efficiency pond#times application number (max 50)
L39:Drift fraction pond #times application number (max 50)
L40:application rate efficiency custom#times application number (max 50)
L41:Drift fraction custom #times application number (max 50)
L42:Depth(cm)#times application number (max 50)
L43:Day since#times application number (max 50)
L44:Dates are relative to Emerge (True, 1); Maturity(False, 2); Harvest(False, 3)
L45:T-BandSplit
L46:Application Window Batch Analysis (?)
L47:Application Window Batch Analysis (?)
L48:Application Window Batch Analysis (?)
L49:Scenario ID
L50:Weather file location
L51:Scenario Latitude (oN)
L52:EPA Pond, EPA reservoir, Varying Voulne &flowthrough, Constant Volume no flowthrough, Constant Volume with flowthrough,ground water,no water body (PRZM only) (True when selected otherwise False)
L53:Use flow averaging days(True), averaging days, flow averaging days for constant vloume
L54:Sediment accounting (No Burial=False, Burial=True)
L55:User define field area(m2)
L56:User define water body area(m2)
L57:User define initial depth(m)
L58:User define max depth(m2)
L59:Mass Xfer Coeff(m/s)
L60:Distribution of eroded pesticide (Varying=True, Constant Fraction=False, fraction)
L61:Benthic Depth(m)
L62:Benthic porosity
L63:Bulk density(g/cm3)
L64:Benthic foc
L65:Benthic DOC
L66:Benthic Biomass
L67:DFAC
L68:WAter columnSS(mg/l)
L69:Chlorophyll(mg/L)
L70:Water column foc
L71:Water Column DOC(mg/L)
L72:Water Column Biomass (mg/L)
L73:USEPP/OPP defaults (True if selected)
L74:Cropped area fraction (reservoir, user define)
L75:Blanck space
L76:******** start of PRZM information ******************
L77:crop cycles <1 year, evergreen, crop cycle>1 year(True if selected)
L78:crop cycle per year
L79:Simple (True if selected; more options-False)
L80:Emerge day, month, mature day, month, harvest day, month, root depth, cover%, height(cm)holdup(cm,post harvest foliage(surface applied=1, removed=2, left on plant=3), planting frequency, lag from start(yrs)
L81:Emerge day, month, mature day, month, harvest day, month, root depth, cover%, height(cm)holdup(cm,post harvest foliage(surface applied=1, removed=2, left on plant=3), planting frequency, lag from start(yrs)
L82:Emerge day, month, mature day, month, harvest day, month, root depth, cover%, height(cm)holdup(cm,post harvest foliage(surface applied=1, removed=2, left on plant=3), planting frequency, lag from start(yrs)
L83:Emerge day, month, mature day, month, harvest day, month, root depth, cover%, height(cm)holdup(cm,post harvest foliage(surface applied=1, removed=2, left on plant=3), planting frequency, lag from start(yrs)
L84:Emerge day, month, mature day, month, harvest day, month, root depth, cover%, height(cm)holdup(cm,post harvest foliage(surface applied=1, removed=2, left on plant=3), planting frequency, lag from start(yrs)
L85:Emerge day, month, mature day, month, harvest day, month, root depth, cover%, height(cm)holdup(cm,post harvest foliage(surface applied=1, removed=2, left on plant=3), planting frequency, lag from start(yrs)
L86:Emerge day, month, mature day, month, harvest day, month, root depth, cover%, height(cm)holdup(cm,post harvest foliage(surface applied=1, removed=2, left on plant=3), planting frequency, lag from start(yrs)
L87:if evergreen selected(root depth,canopy cover%,Canopy height(cm), canopy hold up(cm)
L88:if crop cycle>1 selected (emerge day,month, post emergence mature day, post emergence harvest day )
L89:PET adjustment factor, sno melt factor, evaporation depth(cm)
L90:*** irrigation information start ***
L91:none=0, over canopy=1, under canopy=2
L92:extra water fraction, allowed depletion,max rate (cm/day)
L93:soil irrigation depth(root zone=True, user specified depth=False, depth)
L94:*** spare line for expansion
L95:*** spare line for expansion
L96:*** Soil Information ***
L97:USLE K, USLE LS, USLE P
L98:IREG, slope of the hydraulic path, hydraulic length (m)
L99:*** Horizon Info *******
L100:Soil # of horizons
L101:Soil thick (cm)
L102:Soil porocity(g/cm3)
L103:Soil water content max cap.
L104:Soil water content min cap.
L105:Soil oc%
L106:Soil number of compartment
L107:Soil Sand(%)
L108:Soil Clay(%)
L109:*** Horizon End, Temperature Start ********
L110:lower bc temperature, albedo
L111:simulate temperature(True if selected)
L112:***spare line for expansion
L113:***spare line for expansion
L114:*** Erosion & Curve Number Info **********
L115:No of varying time factors
L116:CN Day interval
L117:CN Month interval
L118:CN values. PRZM accepts 52 CN values, so you can use monthly CN (12 values), bimonthly (24 values;usually one value on the 1st, and one on the 16th), so there are usually 24 values
L119:USLE_C
L120:N(?)
L121:Distribution of Runoff in Surface R-Depth (cm),Distribution of Runoff in Surface R-Decline (1/cm),Distribution of Runoff in Surface efficiency
L122:E depth,E decline (1/cm),efficiency
L123:specify year(True if selected)
L124:years
L125: Boundary layer thickness for volatilization(cm)
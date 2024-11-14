centered_eight <- tibble::tibble(
  chain = rep(seq(0, 3, by = 1), each = 2L),
  draw = rep(c(0, 1), 4),
  `('posterior', 'mu')` = c(
    7.871796366146925, 3.3845543101939555, 4.315816567869714, 4.59770112192212,
    4.527603359725329, 5.862627575440984, 7.17950438342677, 6.539698962089988
  ),
  `('posterior', 'theta[0]', 'Choate')` = c(
    12.320685578094814, 11.285623224544112, 5.596638657101518, 4.965409919503547,
    11.814169821009967, 4.743465360465412, 8.344508335471398, 8.931150446361723
  ),
  `('posterior', 'theta[1]', 'Deerfield')` = c(
    9.905366892588605, 9.12932368629303, 6.959605234225929, 1.7793580917411844,
    -0.04047735261673568, 10.175683926243252, 5.390855243467483,
    6.8529685349829865
  ),
  `('posterior', 'theta[2]', 'Phillips Andover')` = c(
    14.9516154956564, 3.13926326548402, 3.241292296110392, 6.820788657106035,
    1.5628255430347562, 4.2654131945410985, 4.716152759670026, 10.071299622022751
  ),
  `('posterior', 'theta[3]', 'Phillips Exeter')` = c(
    11.011484941973162, 9.433210735550723, 0x1.9cd2b84373e0fp+1, 6.09657359042451,
    3.686324119507447, 5.9374205867817285, 10.764078173151155, 1.2883138299483257
  ),
  `('posterior', 'theta[4]', 'Hotchkiss')` = c(
    5.5796015919074735, 0x1.f3efdef096bb2p+2, 3.499912316576655,
    6.358517372311167, 6.983836302017994, 2.290436248761338, -9.085543022489707,
    12.550403510288136
  ),
  `('posterior', 'theta[5]', 'Lawrenceville')` = c(
    16.901795293711004, 2.393088086819214, 5.57310380423347951,
    4.6334585755240285, 2.72540024385122726, 7.420480444810058, 7.875592180011446,
    0.11111666426507288
  ),
  r"[('posterior', 'theta[6]', "St. Paul's")]" = c(
    13.198059333176934, 10.05522282338696, 7.148854286297191, 3.2891612804419523,
    0.31808711197342643, 11.271711207127083, 12.468139944010456,
    7.013971375229047
  ),
  `('posterior', 'theta[7]', 'Mt. Hermon')` = c(
    15.06136583596694, 6.176724213468094, 3.790900159021772, 5.937310767513992,
    0.6613040102606602, 7.121140237470946, 12.607796514193785, 5.136297243320389
  ),
  `('posterior', 'tau')` = c(
    4.725740062893666, 3.908993614449338, 1.9708301084727995, 2.049029126948731,
    3.501276892570984, 2.8932431724224044, 6.073259824253877, 3.7718670219982746
  ),
  `('posterior_predictive', 'obs[0]', 'Choate')` = c(
    15.902310419994926, 33.69767576990228, 7.972253376273402, 0.7208819797350845,
    6.122760671478147, 19.62545434144316, 0x1.a31b01d39445p+4, -8.453864494754733
  ),
  `('posterior_predictive', 'obs[1]', 'Deerfield')` = c(
    -1.730024435095055, 28.195894198562435, 4.360767653720398, -8.877172058493748,
    -8.635382456942253, 0x1.8d4e96bb52a86p+4, 16.187260289160818,
    1.9822566633830832
  ),
  `('posterior_predictive', 'obs[2]', 'Phillips Andover')` = c(
    27.994237510214024, 8.19918249327672, 15.519627941342575, -18.3505584647182,
    -13.29084938135121, -15.421568982399664, 0.10268608545652567,
    8.88816558927523
  ),
  `('posterior_predictive', 'obs[3]', 'Phillips Exeter')` = c(
    -5.5196146631929, 6.556496643281584, 11.96189718034375, -11.58574549902234,
    8.668419201024054, 11.006709949034349, -3.263353864151087, 4.76145115030144
  ),
  `('posterior_predictive', 'obs[4]', 'Hotchkiss')` = c(
    16.218308739056457, 10.10133419031865, 0.335943337308219, -11.873401442677302,
    4.568207228940278, -1.0118339876260216, -9.47896618942527, 18.593831230461888
  ),
  `('posterior_predictive', 'obs[5]', 'Lawrenceville')` = c(
    3.194622909943247, -0.3313854959220719, 5.105001816810512,
    0.31792912343234825, -13.446738269331634, 13.05511592162002, 11.0300197012572,
    6.145254309750488
  ),
  r"[('posterior_predictive', 'obs[6]', "St. Paul's")]" = c(
    32.1454096928973, 10.978769017724234, 19.94626533034414, 5.1429267551603335,
    -1.3402041989184332, 10.05298867752316, 25.40306217274672, 5.335196139631773
  ),
  `('posterior_predictive', 'obs[7]', 'Mt. Hermon')` = c(
    -11.88120590399083, 0x1.636bc96c459f2p+4, 11.744169040607192,
    13.26485697134283, -8.430385876625586, -0.9533437643806576, 53.51253370444853,
    33.67134174923218
  ),
  `('log_likelihood', 'obs[0]', 'Choate')` = c(
    -4.173301847064581, -4.247811825399992, -4.742345621996834,
    -4.806082823588007, -4.209168953380448, -4.828914075284377,
    -4.485518406694335, -4.4350354527497515278
  ),
  `('log_likelihood', 'obs[1]', 'Deerfield')` = c(
    -3.2396757411755823, -3.2279004861408307, -3.226935732541969,
    -3.415005554952648, -3.544770006488432, -3.2451916289332847,
    -3.255561808001424, -3.228102032107414
  ),
  `('log_likelihood', 'obs[2]', 'Phillips Andover')` = c(
    -4.320942292366116, -3.76514161763766, -3.7676087584239206,
    -3.879902040302936, -3.732190101022912, -3.79462535874018, -3.80781439101196,
    -4.025235993352055
  ),
  `('log_likelihood', 'obs[3]', 'Phillips Exeter')` = c(
    -3.3833297210430326, -3.341298741885685, -3.3757150500932447,
    -3.3202064476456816, -3.362207557411961, -3.3214994052154205,
    -3.375380436141857, -3.451641074206182
  ),
  `('log_likelihood', 'obs[4]', 'Hotchkiss')` = c(
    -3.383392475406518, -3.59543969314517931, -3.2411582392870546,
    -3.450408653248332, -3.5096300370681783, -3.1829962630542417,
    -3.5197187029392376, -4.24957937777317
  ),
  `('log_likelihood', 'obs[5]', 'Lawrenceville')` = c(
    -4.361739151305066, -0x1.a994ca307fb5ap+1, -3.403252311805914,
    -3.371387612697461, -0x1.aa211c8d363adp+1, -3.4871750008054745,
    -3.512179954043682, -3.3200987381705938
  ),
  r"[('log_likelihood', 'obs[6]', "St. Paul's")]" = c(
    -3.336816797037164, -3.537121048128876, -3.810260442698773, -4.30356750536296,
    -4.78477384309752, -3.447872976600215, -3.374531004593982, -3.824987750920152
  ),
  `('log_likelihood', 'obs[7]', 'Mt. Hermon')` = c(
    -3.8237731935416175, -3.861641372715048, -3.913306155605696,
    -3.8660328230911176, -4.007714653365854, -3.8460437364440856,
    -0x1.e7aa29058f0c4p+1, -3.8820115496379537
  ),
  `('sample_stats', 'max_energy_error')` = c(
    -0.6450440932589032, 0.17819376551381794, 9.65932681365966,
    0.41296718947592126, 0.3576135081650378, -0.5869131343912386,
    0.5885192660454805, -0.1981698967473804
  ),
  `('sample_stats', 'energy_error')` = c(
    0.00138256860302021, 0.16224423945229205, 0, 0.04334993661606035,
    0.20664234113456817, -0.3768957880137265, -0.22023349461957764,
    -0.1981698967473804
  ),
  `('sample_stats', 'lp')` = c(
    -60.32696164275558, -57.7975626614451, -49.45813160374509, -50.55823503564649,
    -57.1163667256529948, -53.59614784610748, -60.33564654087028,
    -57.3676310482188
  ),
  `('sample_stats', 'index_in_trajectory')` = c(-17, -12, 0, -4, 3, 6, 6, 11),
  `('sample_stats', 'acceptance_rate')` = c(
    0.9942352227418988, 0.96350246119641, 0.00036777521798557, 0.7922073627479339,
    0.8408966121088914, 0.9946435201881332, 0.8793484090960393, 0.994801407790898
  ),
  `('sample_stats', 'diverging')` = logical(8),
  `('sample_stats', 'process_time_diff')` = c(
    0.00527251900000003, 0.00276631100000024, 0.00070993900000004,
    0.00135272000000075, 0.00141739000000029, 0.00280187500000028,
    0.00267999699999954, 0.00301764699999917
  ),
  `('sample_stats', 'n_steps')` = c(31, 15, 3, 7, 7, 15, 15, 15),
  `('sample_stats', 'perf_counter_start')` = c(
    6648.486410695, 6648.491881364, 6648.365810688, 6648.366702306,
    6650.032407256, 6650.034062115, 6648.163519647, 6648.166406988
  ),
  `('sample_stats', 'largest_eigval')` = rep(NA, 8L),
  `('sample_stats', 'smallest_eigval')` = rep(NA, 8L),
  `('sample_stats', 'step_size_bar')` = rep(
    c(0.25725828552159075, 0.2642742800894639, 0x1.fb9f9ba7dbe6dp-3, 0.2685199946126546),
    each = 2L
  ),
  `('sample_stats', 'step_size')` = rep(
    c(
      0.4435329719894675, 0.17867700218511812, 0.27527526115073647,
      0x1.2ccfbcea6dc36p-2
    ),
    each = 2L
  ),
  `('sample_stats', 'energy')` = c(
    69.11626324613901, 0x1.f99ea6e66061cp+5, 56.01061577094224, 55.15067207357029,
    60.04699012887901, 58.24958346033797, 66.03987544761695, 63.66165976460394
  ),
  `('sample_stats', 'tree_depth')` = c(5, 4, 2, 3, 3, 4, 4, 4),
  `('sample_stats', 'perf_counter_diff')` = c(
    0.00527234499986661, 0.00276631199994881, 0.00070973399942886,
    0.00135247700018226, 0.00142459100061387, 0.00280113100052403,
    0.00267974800044612, 0.00301715499972488
  ),
  `('prior', 'tau')` = rep(c(1.9406020199429952, 3.387860558393447352, NA), c(1L, 1L, 6L)),
  `('prior', 'theta[0]', 'Choate')` = rep(c(4.866208560240019, 4.379140985603659, NA), c(1L, 1L, 6L)),
  `('prior', 'theta[1]', 'Deerfield')` = rep(c(4.589567277177157, 5.969299089114928, NA), c(1L, 1L, 6L)),
  `('prior', 'theta[2]', 'Phillips Andover')` = rep(c(-0.7404405653630124, -2.973677885620807, NA), c(1L, 1L, 6L)),
  `('prior', 'theta[3]', 'Phillips Exeter')` = rep(c(0.4425628718705053, 2.521584268292495, NA), c(1L, 1L, 6L)),
  `('prior', 'theta[4]', 'Hotchkiss')` = rep(c(7.105975338755183, -0.9214494851820336, NA), c(1L, 1L, 6L)),
  `('prior', 'theta[5]', 'Lawrenceville')` = rep(c(3.96103810354463048, 11.030558720921178, NA), c(1L, 1L, 6L)),
  r"[('prior', 'theta[6]', "St. Paul's")]" = rep(c(2.654932489345134, 5.052848205636366, NA), c(1L, 1L, 6L)),
  `('prior', 'theta[7]', 'Mt. Hermon')` = rep(c(6.018632573308046, 2.6761000118676943, NA), c(1L, 1L, 6L)),
  `('prior', 'mu')` = rep(c(3.902531019271695, 3.9150065984060656, NA), c(1L, 1L, 6L)),
  `('prior_predictive', 'obs[0]', 'Choate')` = rep(c(12.39188812494254, 20.95975787606969476, NA), c(1L, 1L, 6L)),
  `('prior_predictive', 'obs[1]', 'Deerfield')` = rep(c(-10.104596951599184, 8.860897431254003, NA), c(1L, 1L, 6L)),
  `('prior_predictive', 'obs[2]', 'Phillips Andover')` = rep(c(-12.086240472712175, 9.90521097559478, NA), c(1L, 1L, 6L)),
  `('prior_predictive', 'obs[3]', 'Phillips Exeter')` = rep(c(-16.44544080961726, 11.904551315704673, NA), c(1L, 1L, 6L)),
  `('prior_predictive', 'obs[4]', 'Hotchkiss')` = rep(c(-5.330991595807697, 10.314622816260211, NA), c(1L, 1L, 6L)),
  `('prior_predictive', 'obs[5]', 'Lawrenceville')` = rep(c(-3.5986862965267803, 15.407882045562015, NA), c(1L, 1L, 6L)),
  r"[('prior_predictive', 'obs[6]', "St. Paul's")]" = rep(c(-8.777780652349213, 0.04052430976767773, NA), c(1L, 1L, 6L)),
  `('prior_predictive', 'obs[7]', 'Mt. Hermon')` = rep(c(-9.6932891937216, -18.906914494458967, NA), c(1L, 1L, 6L)),
) |>
  structure(
    spec = list(
      cols = list(
        chain = list() |>
          structure(class = c("collector_double", "collector")),
        draw = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior', 'mu')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior', 'theta[0]', 'Choate')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior', 'theta[1]', 'Deerfield')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior', 'theta[2]', 'Phillips Andover')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior', 'theta[3]', 'Phillips Exeter')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior', 'theta[4]', 'Hotchkiss')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior', 'theta[5]', 'Lawrenceville')` = list() |>
          structure(class = c("collector_double", "collector")),
        r"[('posterior', 'theta[6]', "St. Paul's")]" = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior', 'theta[7]', 'Mt. Hermon')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior', 'tau')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior_predictive', 'obs[0]', 'Choate')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior_predictive', 'obs[1]', 'Deerfield')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior_predictive', 'obs[2]', 'Phillips Andover')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior_predictive', 'obs[3]', 'Phillips Exeter')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior_predictive', 'obs[4]', 'Hotchkiss')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior_predictive', 'obs[5]', 'Lawrenceville')` = list() |>
          structure(class = c("collector_double", "collector")),
        r"[('posterior_predictive', 'obs[6]', "St. Paul's")]" = list() |>
          structure(class = c("collector_double", "collector")),
        `('posterior_predictive', 'obs[7]', 'Mt. Hermon')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('log_likelihood', 'obs[0]', 'Choate')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('log_likelihood', 'obs[1]', 'Deerfield')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('log_likelihood', 'obs[2]', 'Phillips Andover')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('log_likelihood', 'obs[3]', 'Phillips Exeter')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('log_likelihood', 'obs[4]', 'Hotchkiss')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('log_likelihood', 'obs[5]', 'Lawrenceville')` = list() |>
          structure(class = c("collector_double", "collector")),
        r"[('log_likelihood', 'obs[6]', "St. Paul's")]" = list() |>
          structure(class = c("collector_double", "collector")),
        `('log_likelihood', 'obs[7]', 'Mt. Hermon')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'max_energy_error')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'energy_error')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'lp')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'index_in_trajectory')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'acceptance_rate')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'diverging')` = list() |>
          structure(class = c("collector_logical", "collector")),
        `('sample_stats', 'process_time_diff')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'n_steps')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'perf_counter_start')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'largest_eigval')` = list() |>
          structure(class = c("collector_logical", "collector")),
        `('sample_stats', 'smallest_eigval')` = list() |>
          structure(class = c("collector_logical", "collector")),
        `('sample_stats', 'step_size_bar')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'step_size')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'energy')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'tree_depth')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('sample_stats', 'perf_counter_diff')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior', 'tau')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior', 'theta[0]', 'Choate')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior', 'theta[1]', 'Deerfield')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior', 'theta[2]', 'Phillips Andover')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior', 'theta[3]', 'Phillips Exeter')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior', 'theta[4]', 'Hotchkiss')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior', 'theta[5]', 'Lawrenceville')` = list() |>
          structure(class = c("collector_double", "collector")),
        r"[('prior', 'theta[6]', "St. Paul's")]" = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior', 'theta[7]', 'Mt. Hermon')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior', 'mu')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior_predictive', 'obs[0]', 'Choate')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior_predictive', 'obs[1]', 'Deerfield')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior_predictive', 'obs[2]', 'Phillips Andover')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior_predictive', 'obs[3]', 'Phillips Exeter')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior_predictive', 'obs[4]', 'Hotchkiss')` = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior_predictive', 'obs[5]', 'Lawrenceville')` = list() |>
          structure(class = c("collector_double", "collector")),
        r"[('prior_predictive', 'obs[6]', "St. Paul's")]" = list() |>
          structure(class = c("collector_double", "collector")),
        `('prior_predictive', 'obs[7]', 'Mt. Hermon')` = list() |>
          structure(class = c("collector_double", "collector"))
      ),
      default = list() |>
        structure(class = c("collector_guess", "collector")),
      delim = ","
    ) |>
      structure(class = "col_spec"),
    problems = constructive::.xptr("0x600001325c60"),
    class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame")
  )

usethis::use_data(centered_eight,
                  overwrite = TRUE,
                  ascii = TRUE
)

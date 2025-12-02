//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  List of Areas.
  Generated from EPSG 9.1 dataset.
}


procedure Init_CSAreaList(
  const _cs : TGIS_CSAreaList
) ;

procedure prepare_1;
begin
  _cs.Add(   1024, 'Afghanistan',
           60.50, 29.40, 74.92, 38.48
         );
  _cs.Add(   1025, 'Albania',
           18.46, 39.63, 21.06, 42.67
         );
  _cs.Add(   1026, 'Algeria',
           -8.67, 18.97, 11.99, 38.80
         );
  _cs.Add(   1027, 'American Samoa',
           -173.75, -17.56, -165.20, -10.02
         );
  _cs.Add(   1028, 'Andorra',
           1.42, 42.43, 1.79, 42.66
         );
  _cs.Add(   1029, 'Angola',
           8.20, -18.02, 24.09, -4.38
         );
  _cs.Add(   1030, 'Anguilla',
           -63.90, 17.94, -60.68, 21.93
         );
  _cs.Add(   1031, 'Antarctica',
           -180.00, -90.00, 180.00, -60.00
         );
  _cs.Add(   1271, 'Africa - Eritrea, Ethiopia, South Sudan and Sudan',
           21.82, 3.40, 47.99, 22.24
         );
  _cs.Add(   1276, 'Africa - Botswana, Malawi, Zambia, Zimbabwe',
           19.99, -26.88, 35.93, -8.19
         );
  _cs.Add(   1277, 'Africa - Burundi, Kenya, Rwanda, Tanzania and Uganda',
           28.85, -11.75, 41.91, 4.63
         );
  _cs.Add(   1288, 'Angola - Angola proper',
           8.20, -18.02, 24.09, -5.82
         );
  _cs.Add(   1290, 'Africa - Botswana, Eswatini, Lesotho and South Africa',
           16.45, -34.88, 32.95, -17.78
         );
  _cs.Add(   1317, 'Angola - Cabinda offshore',
           10.53, -6.04, 12.18, -5.05
         );
  _cs.Add(   1318, 'Angola - Cabinda',
           10.53, -6.04, 13.10, -4.38
         );
  _cs.Add(   1365, 'Algeria - north of 32'#$00b0'N',
           -2.95, 31.99, 9.09, 37.14
         );
  _cs.Add(   1366, 'Africa - Algeria, Morocco and Tunisia',
           -13.17, 18.98, 11.99, 37.34
         )         .MarkDeprecated( True );
  _cs.Add(   1552, 'Africa - Eritrea, Ethiopia and Sudan - 36'#$00b0'E to 42'#$00b0'E',
           36.00, 3.40, 42.00, 22.01
         );
  _cs.Add(   1575, 'Africa - Botswana and Zambia - west of 24'#$00b0'E',
           19.99, -26.88, 24.00, -10.86
         );
  _cs.Add(   1576, 'Africa - Botswana, Zambia and Zimbabwe - 24'#$00b0'E to 30'#$00b0'E',
           24.00, -25.84, 30.00, -8.31
         );
  _cs.Add(   1577, 'Africa - Malawi, Zambia and Zimbabwe - east of 30'#$00b0'E',
           30.00, -22.42, 35.93, -8.19
         );
  _cs.Add(   1579, 'Africa - Tanzania and Uganda - south of equator and west of 30'#$00b0'E',
           29.34, -6.81, 30.00, 0.00
         );
  _cs.Add(   1580, 'Africa - Kenya and Uganda - north of equator and 30'#$00b0'E to 36'#$00b0'E',
           29.99, 0.00, 36.00, 4.63
         );
  _cs.Add(   1581, 'Africa - Kenya, Tanzania and Uganda - south of equator and 30'#$00b0'E to 36'#$00b0'E',
           29.99, -11.61, 36.00, 0.01
         );
  _cs.Add(   1583, 'Africa - Kenya and Tanzania - south of equator and east of 36'#$00b0'E',
           36.00, -11.75, 41.60, 0.00
         );
  _cs.Add(   1604, 'Angola - Angola proper - offshore',
           8.20, -17.26, 13.86, -6.01
         );
  _cs.Add(   1605, 'Angola - offshore block 15',
           10.83, -6.59, 11.67, -6.03
         );
  _cs.Add(   1606, 'Angola - Angola proper - offshore - west of 12'#$00b0'E',
           8.20, -17.26, 12.00, -6.03
         );
  _cs.Add(   1607, 'Angola - Angola proper - 12'#$00b0'E to 18'#$00b0'E',
           12.00, -17.44, 18.00, -5.82
         );
  _cs.Add(   1728, 'Algeria - north of 34'#$00b0'39''N',
           -2.22, 34.64, 8.64, 37.14
         );
  _cs.Add(   1729, 'Algeria - 31'#$00b0'30''N to 34'#$00b0'39''N',
           -3.85, 31.49, 9.22, 34.66
         );
  _cs.Add(   1735, 'Algeria - west of 6'#$00b0'W',
           -8.67, 25.73, -6.00, 29.85
         );
  _cs.Add(   1736, 'Algeria - 6'#$00b0'W to 0'#$00b0'W onshore',
           -6.00, 21.82, 0.00, 35.96
         );
  _cs.Add(   1737, 'Algeria - 0'#$00b0'E to 6'#$00b0'E onshore',
           0.00, 18.97, 6.01, 36.97
         );
  _cs.Add(   1738, 'Algeria - east of 6'#$00b0'E onshore',
           6.00, 19.60, 11.99, 37.14
         );
  _cs.Add(   2288, 'American Samoa - Tutuila island',
           -170.88, -14.43, -170.51, -14.20
         );
  _cs.Add(   2289, 'American Samoa - Ofu, Olesega and Ta''u islands',
           -169.73, -14.31, -169.38, -14.11
         );
  _cs.Add(   2311, 'Africa - Kenya and Tanzania',
           29.34, -11.75, 41.91, 4.63
         );
  _cs.Add(   2312, 'Africa - Botswana, Eswatini, Lesotho, Malawi, Zambia, Zimbabwe',
           19.99, -30.66, 35.93, -8.19
         );
  _cs.Add(   2316, 'Angola - offshore block 5',
           12.58, -8.59, 13.40, -7.75
         );
  _cs.Add(   2317, 'Angola - offshore block 2',
           12.08, -7.01, 12.84, -6.01
         );
  _cs.Add(   2318, 'Angola - offshore block 3',
           11.74, -7.34, 12.50, -6.66
         );
  _cs.Add(   2319, 'Angola - offshore block 7',
           12.66, -10.09, 13.39, -9.41
         );
  _cs.Add(   2320, 'Angola - offshore blocks 7 8 24 + WGC spec',
           8.20, -17.26, 13.86, -6.01
         );
  _cs.Add(   2321, 'Angola - offshore blocks 1 and 16',
           11.08, -7.26, 12.09, -6.03
         );
  _cs.Add(   2322, 'Angola - offshore blocks 3 7 15 and 17',
           10.83, -10.09, 13.39, -6.03
         );
  _cs.Add(   2323, 'Angola - offshore blocks 1 16 and 18',
           11.08, -8.34, 12.75, -6.03
         );
  _cs.Add(   2324, 'Angola - offshore blocks 2 3 17-18 and 31-33',
           10.41, -8.59, 12.84, -6.01
         );
  _cs.Add(   2347, 'Algeria - north of 31.5'#$00b0'N',
           -3.85, 31.50, 9.22, 37.14
         );
  _cs.Add(   2393, 'Algeria - Hassi Messaoud',
           5.59, 31.48, 6.50, 32.09
         );
  _cs.Add(   2548, 'Africa - AOF west of 10'#$00b0'W',
           -17.59, 8.29, -10.00, 26.01
         );
  _cs.Add(   2549, 'Africa - AOF 10'#$00b0'W to 3.5'#$00b0'W',
           -10.00, 4.29, -3.49, 27.30
         );
  _cs.Add(   2550, 'Africa - AOF 3.5'#$00b0'W to 4'#$00b0'E',
           -3.50, 5.03, 4.00, 24.18
         );
  _cs.Add(   2551, 'Africa - AOF 4'#$00b0'E to 9'#$00b0'E',
           4.00, 12.82, 9.00, 21.79
         );
  _cs.Add(   2552, 'Africa - AEF 9'#$00b0'E to 14'#$00b0'E',
           8.45, -5.06, 14.00, 23.53
         );
  _cs.Add(   2553, 'Africa - AEF 14'#$00b0'E to 21'#$00b0'E',
           14.00, -4.91, 21.00, 23.46
         );
  _cs.Add(   2554, 'Africa - AEF east of 21'#$00b0'E',
           21.00, 4.12, 27.46, 21.06
         );
  _cs.Add(   2598, 'Algeria - District 3',
           1.00, 25.00, 3.30, 32.00
         );
  _cs.Add(   2599, 'Algeria - In Amenas',
           8.83, 27.50, 9.92, 28.30
         );
  _cs.Add(   2600, 'Algeria - Hassi Bir Reikaz',
           7.16, 31.75, 8.00, 32.42
         );
  _cs.Add(   2790, 'Africa - 12th parallel N',
           -17.19, 10.26, 30.42, 15.70
         );
  _cs.Add(   2791, 'Africa - Burkina Faso and SW Niger',
           -4.64, 11.83, 4.00, 14.23
         );
  _cs.Add(   2825, 'Africa - Ethiopia and Sudan - 30'#$00b0'E to 36'#$00b0'E',
           29.99, 3.49, 36.00, 22.24
         );
  _cs.Add(   2826, 'Africa - South Sudan and Sudan - west of 24'#$00b0'E',
           21.82, 8.70, 24.01, 15.76
         );
  _cs.Add(   2827, 'Africa - South Sudan and Sudan - 24'#$00b0'E to 30'#$00b0'E',
           23.99, 4.21, 30.01, 22.01
         );
  _cs.Add(   2991, 'Antarctica - 60'#$00b0'S to 64'#$00b0'S, 72'#$00b0'W to 60'#$00b0'W (SP19-20)',
           -72.00, -64.00, -60.00, -60.00
         );
  _cs.Add(   2992, 'Antarctica - 60'#$00b0'S to 64'#$00b0'S, 60'#$00b0'W to 48'#$00b0'W (SP21-22)',
           -60.00, -64.00, -48.00, -60.00
         );
  _cs.Add(   2993, 'Antarctica - 60'#$00b0'S to 64'#$00b0'S, 48'#$00b0'W to 36'#$00b0'W (SP23-24)',
           -48.00, -64.00, -36.00, -60.00
         );
  _cs.Add(   2994, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 180'#$00b0'W to 168'#$00b0'W (SQ01-02)',
           -180.00, -68.00, -168.00, -64.00
         );
  _cs.Add(   2997, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 36'#$00b0'E to 48'#$00b0'E (SQ37-38)',
           36.00, -68.00, 48.00, -64.00
         );
  _cs.Add(   2998, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 48'#$00b0'E to 60'#$00b0'E (SQ39-40)',
           48.00, -68.00, 60.00, -64.00
         );
  _cs.Add(   3003, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 108'#$00b0'E to 120'#$00b0'E (SQ49-50)',
           108.00, -68.00, 120.00, -64.00
         );
  _cs.Add(   3004, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 120'#$00b0'E to 132'#$00b0'E (SQ51-52)',
           120.00, -68.00, 132.00, -64.00
         );
  _cs.Add(   3005, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 132'#$00b0'E to 144'#$00b0'E (SQ53-54)',
           132.00, -68.00, 144.00, -64.00
         );
  _cs.Add(   3006, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 144'#$00b0'E to 156'#$00b0'E (SQ55-56)',
           144.00, -68.00, 156.00, -64.00
         );
  _cs.Add(   3007, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 156'#$00b0'E to 168'#$00b0'E (SQ57-58)',
           156.00, -68.00, 168.00, -64.00
         );
  _cs.Add(   3109, 'American Samoa - 2 main island groups',
           -170.88, -14.43, -169.38, -14.11
         );
  _cs.Add(   3110, 'American Samoa - 2 main island groups and Rose Island',
           -170.88, -14.59, -168.09, -14.11
         );
  _cs.Add(   3179, 'Africa - Angola (Cabinda) and DR Congo (Zaire) - coastal',
           10.53, -6.04, 13.10, -4.38
         );
  _cs.Add(   3180, 'Africa - Angola (Cabinda) and DR Congo (Zaire) - offshore',
           10.53, -6.04, 12.37, -5.05
         );
  _cs.Add(   3211, 'Aaland Islands',
           19.24, 59.84, 21.19, 60.48
         );
  _cs.Add(   3212, 'Albania - onshore',
           19.22, 39.64, 21.06, 42.67
         );
  _cs.Add(   3213, 'Algeria - onshore',
           -8.67, 18.97, 11.99, 37.14
         );
  _cs.Add(   3214, 'Anguilla - onshore',
           -63.22, 18.11, -62.92, 18.33
         );
  _cs.Add(   3311, 'Africa - South Sudan and Sudan onshore',
           21.82, 3.49, 38.66, 22.24
         );
  _cs.Add(   3402, 'Algeria - Hassi Mouina licence area',
           0.00, 29.25, 1.25, 31.00
         );
  _cs.Add(   3917, 'Algeria - Ahnet licence area',
           1.26, 26.06, 2.92, 27.51
         );
  _cs.Add(   3952, 'Algeria - 6'#$00b0'W to 0'#$00b0'W',
           -6.00, 21.82, 0.00, 37.01
         );
  _cs.Add(   3953, 'Algeria - 0'#$00b0'E to 6'#$00b0'E',
           0.00, 18.97, 6.01, 38.77
         );
  _cs.Add(   3954, 'Algeria - east of 6'#$00b0'E',
           6.00, 19.60, 11.99, 38.80
         );
  _cs.Add(   4025, 'Angola - offshore north of 8'#$00b0'S',
           10.41, -8.01, 12.84, -5.05
         );
  _cs.Add(   4382, 'Algeria - Ain Tsila',
           7.66, 27.40, 8.27, 28.10
         );
  _cs.Add(   4446, 'Albania - north of 41'#$00b0'18''N and east of 19'#$00b0'09''E',
           19.14, 41.30, 20.63, 42.67
         );
  _cs.Add(   4469, 'Angola - onshore',
           11.67, -18.02, 24.09, -4.38
         );
  _cs.Add(   4519, 'Algeria - 32'#$00b0'N to 34'#$00b0'39''N',
           -2.95, 31.99, 9.09, 34.66
         );
  _cs.Add(   4539, 'Angola - east of 18'#$00b0'E',
           17.99, -18.02, 24.09, -6.91
         );
  _cs.Add(   4540, 'Africa - South Africa, Lesotho and Eswatini.',
           13.33, -50.32, 42.85, -22.13
         );
  _cs.Add(   4551, 'Angola - west of 12'#$00b0'E',
           8.20, -17.28, 12.01, -5.03
         );
  _cs.Add(   4555, 'Angola - 12'#$00b0'E to 18'#$00b0'E',
           12.00, -17.44, 18.00, -4.38
         );
  _cs.Add(   4581, 'Africa - Morocco and Western Sahara - onshore',
           -17.16, 20.71, -1.01, 35.97
         );
end;

procedure prepare_2;
begin
  _cs.Add(   1032, 'Antigua and Barbuda',
           -62.76, 16.61, -58.37, 20.88
         );
  _cs.Add(   1273, 'Antigua - onshore',
           -61.95, 16.94, -61.61, 17.22
         );
  _cs.Add(   1278, 'Antarctica - Australian sector',
           45.00, -90.00, 160.00, -60.00
         );
  _cs.Add(   2817, 'Antarctica - Adelie Land - Petrels island',
           139.44, -66.78, 141.50, -66.10
         );
  _cs.Add(   2818, 'Antarctica - Adelie Land coastal area',
           136.00, -67.13, 142.00, -65.61
         );
  _cs.Add(   2880, 'Antarctica - Australian sector north of 80'#$00b0'S',
           45.00, -80.00, 160.00, -60.00
         );
  _cs.Add(   2995, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 72'#$00b0'W to 60'#$00b0'W (SQ19-20)',
           -72.00, -68.00, -60.00, -64.00
         );
  _cs.Add(   2996, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 60'#$00b0'W to 48'#$00b0'W (SQ21-22)',
           -60.00, -68.00, -48.00, -64.00
         );
  _cs.Add(   2999, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 60'#$00b0'E to 72'#$00b0'E (SQ41-42)',
           60.00, -68.00, 72.00, -64.00
         );
  _cs.Add(   3000, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 72'#$00b0'E to 84'#$00b0'E (SQ43-44)',
           72.00, -68.00, 84.00, -64.00
         );
  _cs.Add(   3001, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 84'#$00b0'E to 96'#$00b0'E (SQ45-46)',
           84.00, -68.00, 96.00, -64.00
         );
  _cs.Add(   3002, 'Antarctica - 64'#$00b0'S to 68'#$00b0'S, 96'#$00b0'E to 108'#$00b0'E (SQ47-48)',
           96.00, -68.00, 108.00, -64.00
         );
  _cs.Add(   3008, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 108'#$00b0'W to 96'#$00b0'W (SR13-14)',
           -108.00, -72.00, -96.00, -68.00
         );
  _cs.Add(   3009, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 96'#$00b0'W to 84'#$00b0'W (SR15-16)',
           -96.00, -72.00, -84.00, -68.00
         );
  _cs.Add(   3010, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 84'#$00b0'W to 72'#$00b0'W (SR17-18)',
           -84.00, -72.00, -72.00, -68.00
         );
  _cs.Add(   3011, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 72'#$00b0'W to 60'#$00b0'W (SR19-20)',
           -72.00, -72.00, -60.00, -68.00
         );
  _cs.Add(   3012, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 24'#$00b0'W to 12'#$00b0'W (SR27-28)',
           -24.00, -72.00, -12.00, -68.00
         );
  _cs.Add(   3013, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 12'#$00b0'W to 0'#$00b0'W (SR29-30)',
           -12.00, -72.00, 0.00, -68.00
         );
  _cs.Add(   3014, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 0'#$00b0'E to 12'#$00b0'E (SR31-32)',
           0.00, -72.00, 12.00, -68.00
         );
  _cs.Add(   3015, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 12'#$00b0'E to 24'#$00b0'E (SR33-34)',
           12.00, -72.00, 24.00, -68.00
         );
  _cs.Add(   3016, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 24'#$00b0'E to 36'#$00b0'E (SR35-36)',
           24.00, -72.00, 36.00, -68.00
         );
  _cs.Add(   3017, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 36'#$00b0'E to 48'#$00b0'E (SR37-38)',
           36.00, -72.00, 48.00, -68.00
         );
  _cs.Add(   3018, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 48'#$00b0'E to 60'#$00b0'E (SR39-40)',
           48.00, -72.00, 60.00, -68.00
         );
  _cs.Add(   3019, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 60'#$00b0'E to 72'#$00b0'E (SR41-42)',
           60.00, -72.00, 72.00, -68.00
         );
  _cs.Add(   3020, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 72'#$00b0'E to 84'#$00b0'E (SR43-44)',
           72.00, -72.00, 84.00, -68.00
         );
  _cs.Add(   3021, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 84'#$00b0'E to 96'#$00b0'E (SR45-46)',
           84.00, -72.00, 96.00, -68.00
         );
  _cs.Add(   3022, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 96'#$00b0'E to 108'#$00b0'E (SR47-48)',
           96.00, -72.00, 108.00, -68.00
         );
  _cs.Add(   3023, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 108'#$00b0'E to 120'#$00b0'E (SR49-50)',
           108.00, -72.00, 120.00, -68.00
         );
  _cs.Add(   3024, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 120'#$00b0'E to 132'#$00b0'E (SR51-52)',
           120.00, -72.00, 132.00, -68.00
         );
  _cs.Add(   3025, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 132'#$00b0'E to 144'#$00b0'E (SR53-54)',
           132.00, -72.00, 144.00, -68.00
         );
  _cs.Add(   3026, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 144'#$00b0'E to 156'#$00b0'E (SR55-56)',
           144.00, -72.00, 156.00, -68.00
         );
  _cs.Add(   3027, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 156'#$00b0'E to 168'#$00b0'E (SR57-58)',
           156.00, -72.00, 168.00, -68.00
         );
  _cs.Add(   3028, 'Antarctica - 68'#$00b0'S to 72'#$00b0'S, 168'#$00b0'E to 180'#$00b0'E (SR59-60)',
           168.00, -72.00, 180.00, -68.00
         );
  _cs.Add(   3029, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 162'#$00b0'W to 144'#$00b0'W (SS04-06)',
           -162.00, -76.00, -144.00, -72.00
         );
  _cs.Add(   3030, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 144'#$00b0'W to 126'#$00b0'W (SS07-09)',
           -144.00, -76.00, -126.00, -72.00
         );
  _cs.Add(   3031, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 126'#$00b0'W to 108'#$00b0'W (SS10-12)',
           -126.00, -76.00, -108.00, -72.00
         );
  _cs.Add(   3032, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 108'#$00b0'W to 90'#$00b0'W (SS13-15)',
           -108.00, -76.00, -90.00, -72.00
         );
  _cs.Add(   3069, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 30'#$00b0'E to 60'#$00b0'E (SU36-40)',
           30.00, -84.00, 60.00, -80.00
         );
  _cs.Add(   3070, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 60'#$00b0'E to 90'#$00b0'E (SU41-45)',
           60.00, -84.00, 90.00, -80.00
         );
  _cs.Add(   3071, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 90'#$00b0'E to 120'#$00b0'E (SU46-50)',
           90.00, -84.00, 120.00, -80.00
         );
  _cs.Add(   3072, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 120'#$00b0'E to 150'#$00b0'E (SU51-55)',
           120.00, -84.00, 150.00, -80.00
         );
  _cs.Add(   3073, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 150'#$00b0'E to 180'#$00b0' (SU56-60)',
           150.00, -84.00, 180.00, -80.00
         );
  _cs.Add(   3074, 'Antarctica - 84'#$00b0'S to 88'#$00b0'S, 180'#$00b0'W to 120'#$00b0'W (SV01-10)',
           -180.00, -88.00, -120.00, -84.00
         );
  _cs.Add(   3075, 'Antarctica - 84'#$00b0'S to 88'#$00b0'S, 120'#$00b0'W to 60'#$00b0'W (SV11-20)',
           -120.00, -88.00, -60.00, -84.00
         );
  _cs.Add(   3076, 'Antarctica - 84'#$00b0'S to 88'#$00b0'S, 60'#$00b0'W to 0'#$00b0'W (SV21-30)',
           -60.00, -88.00, 0.00, -84.00
         );
  _cs.Add(   3077, 'Antarctica - 84'#$00b0'S to 88'#$00b0'S, 0'#$00b0'E to 60'#$00b0'E (SV31-40)',
           0.00, -88.00, 60.00, -84.00
         );
  _cs.Add(   3078, 'Antarctica - 84'#$00b0'S to 88'#$00b0'S, 60'#$00b0'E to 120'#$00b0'E (SV41-50)',
           60.00, -88.00, 120.00, -84.00
         );
  _cs.Add(   3079, 'Antarctica - 84'#$00b0'S to 88'#$00b0'S, 120'#$00b0'E to 180'#$00b0'E (SV51-60)',
           120.00, -88.00, 180.00, -84.00
         );
  _cs.Add(   3080, 'Antarctica - 88'#$00b0'S to 90'#$00b0'S, 180'#$00b0'W to 180'#$00b0'E (SW01-60)',
           -180.00, -90.00, 180.00, -88.00
         );
  _cs.Add(   3081, 'Antarctica - Transantarctic mountains north of 80'#$00b0'S',
           149.83, -80.00, 174.01, -68.60
         );
  _cs.Add(   3204, 'Antarctica - Deception Island',
           -60.89, -63.08, -60.35, -62.82
         );
  _cs.Add(   3205, 'Antarctica - Camp McMurdo area',
           165.73, -77.94, 167.43, -77.17
         );
  _cs.Add(   3558, 'Antarctica - Ross Sea Region',
           144.99, -90.00, -144.99, -59.99
         );
  _cs.Add(   3592, 'Antarctica - Darwin Glacier region',
           145.00, -81.00, 169.00, -76.00
         );
  _cs.Add(   3853, 'Antarctica - McMurdo Sound region',
           153.00, -81.00, 173.00, -76.00
         );
  _cs.Add(   3854, 'Antarctica - Borchgrevink Coast region',
           157.00, -76.00, 173.00, -73.00
         );
  _cs.Add(   4033, 'Arctic - 79'#$00b0'N to 67'#$00b0'N, 84'#$00b0'W to 6'#$00b0'E',
           -84.00, 67.00, 6.01, 79.01
         );
  _cs.Add(   4034, 'Arctic - 79'#$00b0'N to 67'#$00b0'N, 12'#$00b0'W to 78'#$00b0'E',
           -12.00, 67.00, 78.01, 79.01
         );
  _cs.Add(   4036, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 95'#$00b0'W to 55'#$00b0'W',
           -95.00, 79.50, -55.00, 84.51
         );
  _cs.Add(   4037, 'Arctic - 79'#$00b0'N to 67'#$00b0'N, 60'#$00b0'E to 150'#$00b0'E',
           60.00, 67.00, 150.01, 79.01
         );
  _cs.Add(   4038, 'Arctic - 79'#$00b0'N to 67'#$00b0'N, 132'#$00b0'E to 138'#$00b0'W',
           132.00, 67.00, -138.00, 79.01
         );
  _cs.Add(   4039, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 72'#$00b0'W to 32'#$00b0'W',
           -72.00, 79.50, -32.00, 84.51
         );
  _cs.Add(   4040, 'Arctic - 71'#$00b0'N to 59'#$00b0'N, 156'#$00b0'W to 66'#$00b0'W',
           -156.00, 59.00, -66.00, 71.00
         );
  _cs.Add(   4041, 'Arctic - 71'#$00b0'N to 59'#$00b0'N, 84'#$00b0'W to 6'#$00b0'E',
           -84.00, 59.00, 6.00, 71.00
         );
  _cs.Add(   4042, 'Arctic - 71'#$00b0'N to 59'#$00b0'N, 12'#$00b0'W to 78'#$00b0'E',
           -12.00, 59.00, 78.01, 71.00
         );
  _cs.Add(   4043, 'Arctic - 71'#$00b0'N to 59'#$00b0'N, 60'#$00b0'E to 150'#$00b0'E',
           60.00, 59.00, 150.01, 71.00
         );
  _cs.Add(   4044, 'Arctic - 87'#$00b0'50''N to 82'#$00b0'50''N, 180'#$00b0'W to 120'#$00b0'W',
           -180.00, 82.83, -120.00, 87.84
         );
  _cs.Add(   4045, 'Arctic - 71'#$00b0'N to 59'#$00b0'N, 132'#$00b0'E to 138'#$00b0'W',
           132.00, 59.00, -138.00, 71.00
         );
  _cs.Add(   4046, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 32'#$00b0'W to 8'#$00b0'E',
           -32.00, 79.50, 8.01, 84.51
         );
  _cs.Add(   4047, 'Arctic - 87'#$00b0'50''N to 82'#$00b0'50''N, 120'#$00b0'W to 60'#$00b0'W',
           -120.00, 82.83, -60.00, 87.84
         );
  _cs.Add(   4048, 'Arctic - 87'#$00b0'50''N to 82'#$00b0'50''N, 60'#$00b0'W to 0'#$00b0'E',
           -60.00, 82.83, 0.00, 87.84
         );
  _cs.Add(   4049, 'Arctic - 87'#$00b0'50''N to 82'#$00b0'50''N, 0'#$00b0'E to 60'#$00b0'E',
           0.00, 82.83, 60.01, 87.84
         );
  _cs.Add(   4050, 'Arctic - 87'#$00b0'50''N to 82'#$00b0'50''N, 60'#$00b0'E to 120'#$00b0'E',
           60.00, 82.83, 120.01, 87.84
         );
  _cs.Add(   4051, 'Arctic - 87'#$00b0'50''N to 82'#$00b0'50''N, 120'#$00b0'E to 180'#$00b0'E',
           120.00, 82.83, 180.00, 87.83
         );
  _cs.Add(   4052, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 174'#$00b0'W to 135'#$00b0'W',
           -174.00, 79.50, -134.99, 84.51
         );
  _cs.Add(   4053, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 4'#$00b0'W to 36'#$00b0'E',
           -4.00, 79.50, 36.01, 84.51
         );
  _cs.Add(   4054, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 33'#$00b0'E to 73'#$00b0'E',
           33.00, 79.50, 73.01, 84.51
         );
  _cs.Add(   4055, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 73'#$00b0'E to 113'#$00b0'E',
           73.00, 79.50, 113.01, 84.51
         );
  _cs.Add(   4056, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 113'#$00b0'E to 153'#$00b0'E',
           113.00, 79.50, 153.01, 84.51
         );
  _cs.Add(   4057, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 146'#$00b0'E to 174'#$00b0'W',
           146.00, 79.50, -173.99, 84.51
         );
  _cs.Add(   4058, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 4'#$00b0'W to 38'#$00b0'E',
           -3.35, 76.16, 38.00, 81.17
         );
  _cs.Add(   4059, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 35'#$00b0'E to 67'#$00b0'E',
           34.99, 76.16, 67.01, 81.17
         );
  _cs.Add(   4060, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 67'#$00b0'E to 98'#$00b0'E',
           67.00, 76.16, 98.01, 81.17
         );
  _cs.Add(   4061, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 98'#$00b0'E to 129'#$00b0'E',
           98.00, 76.16, 129.01, 81.17
         );
  _cs.Add(   4062, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 129'#$00b0'E to 160'#$00b0'E',
           129.00, 76.16, 160.01, 81.17
         );
  _cs.Add(   4063, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 160'#$00b0'E to 169'#$00b0'W',
           160.00, 76.16, -168.99, 81.17
         );
  _cs.Add(   4064, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 169'#$00b0'W to 138'#$00b0'W',
           -169.00, 76.16, -138.00, 81.17
         );
  _cs.Add(   4065, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 144'#$00b0'W to 114'#$00b0'W',
           -144.00, 76.16, -114.00, 81.17
         );
  _cs.Add(   4070, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 114'#$00b0'W to 84'#$00b0'W',
           -114.00, 76.16, -84.00, 81.17
         );
  _cs.Add(   4071, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 84'#$00b0'W to 54'#$00b0'W',
           -84.00, 76.16, -54.00, 81.17
         );
  _cs.Add(   4072, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, Canada east of 84'#$00b0'W',
           -84.00, 76.16, -64.78, 81.17
         );
  _cs.Add(   4073, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, Greenland west of 54'#$00b0'W',
           -75.00, 76.16, -54.00, 81.17
         );
  _cs.Add(   4074, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 54'#$00b0'W to 24'#$00b0'W',
           -54.00, 76.16, -24.00, 81.17
         );
  _cs.Add(   4075, 'Arctic - 81'#$00b0'10''N to 76'#$00b0'10''N, 24'#$00b0'W to 3'#$00b0'E',
           -24.00, 76.16, 1.89, 81.17
         );
  _cs.Add(   4076, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 169'#$00b0'W to 141'#$00b0'W',
           -169.00, 72.83, -141.00, 77.84
         );
  _cs.Add(   4077, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 141'#$00b0'W to 116'#$00b0'W',
           -141.00, 72.83, -116.00, 77.84
         );
  _cs.Add(   4078, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 116'#$00b0'W to 91'#$00b0'W',
           -116.00, 72.83, -91.00, 77.84
         );
  _cs.Add(   4079, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 91'#$00b0'W to 67'#$00b0'W',
           -91.00, 72.83, -67.00, 77.84
         );
  _cs.Add(   4080, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 76'#$00b0'W to 51'#$00b0'W',
           -76.00, 72.83, -51.00, 77.84
         );
  _cs.Add(   4081, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 51'#$00b0'W to 26'#$00b0'W',
           -51.00, 72.83, -26.00, 77.84
         );
end;

procedure prepare_3;
begin
  _cs.Add(   4082, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 26'#$00b0'W to 2'#$00b0'W',
           -26.00, 72.83, -2.00, 77.84
         );
  _cs.Add(   4083, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 2'#$00b0'W to 22'#$00b0'E',
           -2.00, 72.83, 22.01, 77.84
         );
  _cs.Add(   4084, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 22'#$00b0'E to 46'#$00b0'E',
           22.00, 72.83, 46.01, 77.84
         );
  _cs.Add(   4085, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 46'#$00b0'E to 70'#$00b0'E',
           46.00, 72.83, 70.01, 77.84
         );
  _cs.Add(   4086, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 70'#$00b0'E to 94'#$00b0'E',
           70.00, 72.83, 94.01, 77.84
         );
  _cs.Add(   4087, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 94'#$00b0'E to 118'#$00b0'E',
           94.00, 72.83, 118.01, 77.84
         );
  _cs.Add(   4088, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 118'#$00b0'E to 142'#$00b0'E',
           118.00, 72.83, 142.01, 77.84
         );
  _cs.Add(   4089, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 142'#$00b0'E to 166'#$00b0'E',
           142.00, 72.83, 166.01, 77.84
         );
  _cs.Add(   4090, 'Arctic - 77'#$00b0'50''N to 72'#$00b0'50''N, 166'#$00b0'E to 169'#$00b0'W',
           166.00, 72.83, -168.99, 77.84
         );
  _cs.Add(   4091, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 4'#$00b0'E to 24'#$00b0'E',
           4.00, 69.50, 24.01, 74.51
         );
  _cs.Add(   4092, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 24'#$00b0'E to 44'#$00b0'E',
           24.00, 69.50, 44.01, 74.51
         );
  _cs.Add(   4093, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 44'#$00b0'E to 64'#$00b0'E',
           44.00, 69.50, 64.01, 74.51
         );
  _cs.Add(   4094, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 64'#$00b0'E to 85'#$00b0'E',
           64.00, 69.50, 85.01, 74.51
         );
  _cs.Add(   4095, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 85'#$00b0'E to 106'#$00b0'E',
           85.00, 69.50, 106.01, 74.51
         );
  _cs.Add(   4096, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 106'#$00b0'E to 127'#$00b0'E',
           106.00, 69.50, 127.01, 74.51
         );
  _cs.Add(   4097, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 127'#$00b0'E to 148'#$00b0'E',
           127.00, 69.50, 148.00, 74.51
         );
  _cs.Add(   4098, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 148'#$00b0'E to 169'#$00b0'E',
           148.00, 69.50, 169.01, 74.51
         );
  _cs.Add(   4099, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 169'#$00b0'E to 169'#$00b0'W',
           169.00, 69.50, -169.00, 74.51
         );
  _cs.Add(   4100, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 173'#$00b0'W to 153'#$00b0'W',
           -173.00, 69.50, -153.00, 74.51
         );
  _cs.Add(   4101, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 157'#$00b0'W to 137'#$00b0'W',
           -157.00, 69.50, -137.00, 74.51
         );
  _cs.Add(   4102, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 141'#$00b0'W to 121'#$00b0'W',
           -141.00, 69.50, -121.00, 74.51
         );
  _cs.Add(   4103, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 121'#$00b0'W to 101'#$00b0'W',
           -121.00, 69.50, -101.00, 74.51
         );
  _cs.Add(   4104, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 101'#$00b0'W to 81'#$00b0'W',
           -101.00, 69.50, -81.00, 74.51
         );
  _cs.Add(   4105, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 81'#$00b0'W to 61'#$00b0'W',
           -81.00, 69.50, -61.00, 74.51
         );
  _cs.Add(   4106, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 72'#$00b0'W to 52'#$00b0'W',
           -71.89, 69.48, -51.99, 74.51
         );
  _cs.Add(   4107, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 52'#$00b0'W to 32'#$00b0'W',
           -52.00, 69.50, -32.00, 74.51
         );
  _cs.Add(   4108, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 32'#$00b0'W to 12'#$00b0'W',
           -32.00, 69.50, -12.00, 74.51
         );
  _cs.Add(   4109, 'Arctic - 74'#$00b0'30''N to 69'#$00b0'30''N, 15'#$00b0'W to 5'#$00b0'E',
           -15.00, 69.50, 5.01, 74.51
         );
  _cs.Add(   4110, 'Arctic - 71'#$00b0'10''N to 66'#$00b0'10''N, 174'#$00b0'W to 156'#$00b0'W',
           -174.00, 66.16, -156.00, 71.17
         );
  _cs.Add(   4111, 'Arctic - 71'#$00b0'10''N to 66'#$00b0'10''N, 156'#$00b0'W to 138'#$00b0'W',
           -156.00, 66.16, -138.00, 71.17
         );
  _cs.Add(   4112, 'Arctic - 71'#$00b0'10''N to 66'#$00b0'10''N, 141'#$00b0'W to 122'#$00b0'W',
           -141.00, 66.16, -122.00, 71.17
         );
  _cs.Add(   4113, 'Arctic - 71'#$00b0'10''N to 66'#$00b0'10''N, 122'#$00b0'W to 103'#$00b0'W',
           -122.00, 66.16, -103.00, 71.17
         );
  _cs.Add(   4114, 'Arctic - 71'#$00b0'10''N to 66'#$00b0'10''N, 103'#$00b0'W to 84'#$00b0'W',
           -103.00, 66.16, -84.00, 71.17
         );
  _cs.Add(   4115, 'Arctic - 71'#$00b0'10''N to 66'#$00b0'10''N, 84'#$00b0'W to 65'#$00b0'W',
           -84.00, 66.16, -65.00, 71.17
         );
  _cs.Add(   4116, 'Arctic - 71'#$00b0'10''N to 66'#$00b0'10''N, 65'#$00b0'W to 47'#$00b0'W',
           -65.00, 66.16, -47.00, 71.17
         );
  _cs.Add(   4117, 'Arctic - 71'#$00b0'10''N to 66'#$00b0'10''N, 47'#$00b0'W to 29'#$00b0'W',
           -47.00, 66.16, -29.00, 71.17
         );
  _cs.Add(   4118, 'Arctic - 71'#$00b0'10''N to 66'#$00b0'10''N, 29'#$00b0'W to 11'#$00b0'W',
           -29.00, 66.16, -11.00, 71.17
         );
  _cs.Add(   4563, 'Argentina - 42.5'#$00b0'S to 50.3'#$00b0'S',
           -73.59, -50.34, -65.47, -42.49
         );
  _cs.Add(   4564, 'Argentina - 42.5'#$00b0'S to 50.3'#$00b0'S and west of 70.5'#$00b0'W',
           -73.59, -50.34, -70.50, -44.94
         );
  _cs.Add(   4565, 'Argentina - 42.5'#$00b0'S to 50.3'#$00b0'S and east of 67.5'#$00b0'W',
           -67.50, -49.05, -65.47, -43.58
         );
  _cs.Add(   4572, 'Argentina - 44'#$00b0'S to 47.5'#$00b0'S',
           -72.36, -47.50, -63.24, -43.99
         );
  _cs.Add(   4578, 'Argentina - 67.5'#$00b0'W to 64.5'#$00b0'W mainland onshore',
           -67.50, -49.05, -64.49, -21.78
         );
  _cs.Add(   4579, 'Argentina - 64.5'#$00b0'W to 61.5'#$00b0'W mainland onshore',
           -64.50, -43.14, -61.49, -21.99
         );
  _cs.Add(   1034, 'Armenia',
           43.45, 38.84, 46.63, 41.30
         );
  _cs.Add(   1035, 'Aruba',
           -70.42, 12.14, -69.31, 15.42
         );
  _cs.Add(   1265, 'Argentina - Comodoro Rivadavia',
           -69.50, -46.70, -67.10, -45.19
         );
  _cs.Add(   1272, 'Asia - Middle East - Bahrain, Kuwait and Saudi Arabia',
           34.51, 16.37, 55.67, 32.16
         );
  _cs.Add(   1291, 'Asia - FSU - Caspian Sea',
           46.95, 37.35, 53.93, 46.97
         );
  _cs.Add(   1292, 'Argentina - Neuquen province',
           -71.96, -41.10, -68.01, -36.14
         );
  _cs.Add(   1302, 'Asia - Cambodia and Vietnam - mainland',
           102.14, 8.33, 109.53, 23.40
         );
  _cs.Add(   1304, 'Asia - Myanmar and Thailand onshore',
           92.20, 5.63, 105.64, 28.55
         );
  _cs.Add(   1307, 'Asia - Bangladesh; India; Myanmar; Pakistan - onshore',
           60.86, 8.02, 101.17, 37.07
         );
  _cs.Add(   1308, 'Asia - Bangladesh; India; Myanmar; Pakistan - onshore',
           60.86, 8.02, 101.17, 37.07
         );
  _cs.Add(   1309, 'Asia - Malaysia (west) and Singapore',
           99.59, 1.13, 104.60, 6.72
         );
  _cs.Add(   1351, 'Asia - Middle East - Qatar offshore and UAE',
           50.55, 22.63, 57.13, 27.05
         );
  _cs.Add(   1356, 'Asia - Middle East - Israel, Jordan and Palestine onshore',
           34.17, 29.18, 39.31, 33.38
         );
  _cs.Add(   1362, 'Asia - Brunei and East Malaysia',
           109.31, 0.85, 119.61, 7.67
         );
  _cs.Add(   2663, 'Asia - FSU - 49.5'#$00b0'E to 52.5'#$00b0'E onshore',
           49.50, 37.66, 52.50, 81.22
         );
  _cs.Add(   2664, 'Asia - FSU - 52.5'#$00b0'E to 55.5'#$00b0'E onshore',
           52.50, 37.33, 55.50, 81.41
         );
  _cs.Add(   2665, 'Asia - FSU - 55.5'#$00b0'E to 58.5'#$00b0'E onshore',
           55.50, 37.64, 58.50, 81.89
         );
  _cs.Add(   2666, 'Asia - FSU - 58.5'#$00b0'E to 61.5'#$00b0'E onshore',
           58.50, 35.51, 61.50, 81.91
         );
  _cs.Add(   2667, 'Asia - FSU - 61.5'#$00b0'E to 64.5'#$00b0'E onshore',
           61.50, 35.14, 64.50, 81.77
         );
  _cs.Add(   2668, 'Asia - FSU - 64.5'#$00b0'E to 67.5'#$00b0'E onshore',
           64.50, 36.27, 67.50, 81.25
         );
  _cs.Add(   2669, 'Asia - FSU - 67.5'#$00b0'E to 70.5'#$00b0'E onshore',
           67.50, 36.93, 70.50, 77.07
         );
  _cs.Add(   2670, 'Asia - FSU - 70.5'#$00b0'E to 73.5'#$00b0'E onshore',
           70.50, 36.67, 73.50, 73.57
         );
  _cs.Add(   2671, 'Asia - FSU - 73.5'#$00b0'E to 76.5'#$00b0'E onshore',
           73.50, 37.22, 76.50, 79.71
         );
  _cs.Add(   2672, 'Asia - FSU - 76.5'#$00b0'E to 79.5'#$00b0'E onshore',
           76.50, 40.44, 79.50, 81.03
         );
  _cs.Add(   2673, 'Asia - FSU - 79.5'#$00b0'E to 82.5'#$00b0'E onshore',
           79.50, 41.82, 82.50, 81.03
         );
  _cs.Add(   2674, 'Asia - FSU - 82.5'#$00b0'E to 85.5'#$00b0'E onshore',
           82.50, 45.11, 85.50, 77.56
         );
  _cs.Add(   2675, 'Asia - FSU - 85.5'#$00b0'E to 88.5'#$00b0'E onshore',
           85.50, 47.05, 88.50, 77.16
         );
  _cs.Add(   2772, 'Asia - FSU - CS63 zone A1',
           39.99, 41.37, 43.04, 43.59
         );
  _cs.Add(   2773, 'Asia - FSU - CS63 zone A2',
           43.03, 38.87, 46.04, 43.05
         );
  _cs.Add(   2774, 'Asia - FSU - CS63 zone A3',
           46.03, 38.38, 49.04, 42.10
         );
  _cs.Add(   2775, 'Asia - FSU - CS63 zone A4',
           49.03, 37.89, 51.73, 42.59
         );
  _cs.Add(   2776, 'Asia - FSU - CS63 zone K2',
           49.26, 41.15, 52.27, 51.77
         );
  _cs.Add(   2777, 'Asia - FSU - CS63 zone K3',
           52.26, 41.46, 55.27, 51.79
         );
  _cs.Add(   2778, 'Asia - FSU - CS63 zone K4',
           55.26, 41.26, 58.27, 51.14
         );
  _cs.Add(   2876, 'Asia - Middle East - Iraq-Kuwait boundary',
           46.36, 29.06, 48.61, 30.32
         );
  _cs.Add(   2896, 'Asia - Middle East - Israel, Palestine Territory, Turkey - offshore',
           28.03, 31.35, 41.47, 43.45
         );
  _cs.Add(   2897, 'Asia - Middle East - Israel and Palestine Territory offshore',
           33.50, 31.33, 35.10, 33.10
         )         .MarkDeprecated( True );
  _cs.Add(   3215, 'Argentina - mainland onshore',
           -73.59, -52.43, -53.65, -21.78
         );
  _cs.Add(   3216, 'Aruba - onshore',
           -70.11, 12.36, -69.82, 12.68
         );
  _cs.Add(   3386, 'Asia - Middle East - Iraq and Kuwait 42'#$00b0'E to 48'#$00b0'E',
           42.00, 28.53, 48.00, 37.39
         )         .MarkDeprecated( True );
  _cs.Add(   3390, 'Asia - Middle East -SE Iraq and SW Iran',
           44.30, 29.06, 51.06, 33.50
         );
  _cs.Add(   3713, 'Asia - Korea N and S - west of 126'#$00b0'E',
           124.27, 33.99, 126.00, 40.90
         );
  _cs.Add(   3716, 'Asia - Korea N and S - 126'#$00b0'E to 128'#$00b0'E',
           126.00, 33.14, 128.00, 41.80
         );
  _cs.Add(   3726, 'Asia - Korea N and S - 128'#$00b0'E to 130'#$00b0'E',
           128.00, 34.49, 130.00, 43.01
         );
  _cs.Add(   3727, 'Asia - Korea N and S - east of 130'#$00b0'E',
           130.00, 37.39, 131.01, 42.98
         );
  _cs.Add(   3843, 'Argentina - mainland onshore and offshore TdF',
           -73.59, -54.93, -53.65, -21.78
         );
  _cs.Add(   4007, 'Asia - Cambodia and Vietnam - onshore & Cuu Long basin',
           102.14, 7.99, 110.00, 23.40
         );
  _cs.Add(   4175, 'Australasia - Australia and Norfolk Island - 162'#$00b0'E to 168'#$00b0'E',
           162.00, -59.39, 168.00, -25.94
         );
  _cs.Add(   4176, 'Australasia - Australia and Christmas Island - 108'#$00b0'E to 114'#$00b0'E',
           108.00, -37.84, 114.01, -10.72
         );
  _cs.Add(   4191, 'Australasia - Australia and Christmas Island - west of 108'#$00b0'E',
           102.14, -34.68, 108.01, -8.87
         );
  _cs.Add(   4223, 'Asia - Malaysia (west including SCS) and Singapore',
           99.59, 1.13, 105.82, 7.81
         );
  _cs.Add(   4238, 'Asia - Middle East - Iraq and SW Iran',
           38.79, 29.06, 51.06, 37.39
         );
  _cs.Add(   4561, 'Argentina - Mendoza - Cuyo basin',
           -69.40, -36.37, -66.42, -31.96
         );
  _cs.Add(   4562, 'Argentina - Mendoza and Neuquen',
           -72.14, -43.41, -65.86, -31.91
         );
  _cs.Add(   4569, 'Argentina - south Santa Cruz',
           -73.28, -52.43, -68.30, -50.33
         );
  _cs.Add(   4570, 'Argentina - south Santa Cruz west of 70.5'#$00b0'W',
           -73.28, -52.00, -70.50, -50.33
         );
  _cs.Add(   4571, 'Argentina - south Santa Cruz east of 70.5'#$00b0'W',
           -70.50, -52.43, -68.30, -50.33
         );
end;

procedure prepare_4;
begin
  _cs.Add(   4573, 'Argentina - onshore',
           -73.59, -55.11, -53.65, -21.78
         );
  _cs.Add(   4577, 'Argentina - 70.5'#$00b0'W to 67.5'#$00b0'W mainland onshore',
           -70.50, -52.43, -67.49, -24.08
         );
  _cs.Add(   4610, 'Argentina - Buenos Aires city',
           -58.54, -34.71, -58.29, -34.50
         );
  _cs.Add(   4614, 'Argentina - Comodoro Rivadavia - west of 67.5'#$00b0'W',
           -69.50, -46.70, -67.50, -45.19
         );
  _cs.Add(   1036, 'Australia - onshore and EEZ',
           109.23, -47.20, 163.20, -8.88
         );
  _cs.Add(   1279, 'Australasia - Australia and PNG - AGD66',
           109.23, -47.20, 163.20, -1.30
         );
  _cs.Add(   1280, 'Australia - Western Australia',
           112.85, -35.19, 129.01, -13.67
         );
  _cs.Add(   1281, 'Australia - mainland',
           112.85, -39.20, 153.69, -10.65
         );
  _cs.Add(   1282, 'Australia - Tasmania',
           143.77, -43.70, 148.55, -39.52
         );
  _cs.Add(   1556, 'Australia - 102'#$00b0'E to 108'#$00b0'E',
           102.00, -56.00, 108.00, -10.00
         )         .MarkDeprecated( True );
  _cs.Add(   1557, 'Australia - 108'#$00b0'E to 114'#$00b0'E (EEZ)',
           109.23, -37.84, 114.00, -17.19
         );
  _cs.Add(   1558, 'Australia - 114'#$00b0'E to 120'#$00b0'E (EEZ)',
           114.00, -38.53, 120.00, -12.61
         );
  _cs.Add(   1559, 'Australia - 120'#$00b0'E to 126'#$00b0'E',
           120.00, -38.07, 126.01, -10.46
         );
  _cs.Add(   1560, 'Australia - 126'#$00b0'E to 132'#$00b0'E',
           125.99, -37.38, 132.00, -9.10
         );
  _cs.Add(   1561, 'Australia - 132'#$00b0'E to 138'#$00b0'E',
           132.00, -40.71, 138.01, -8.88
         );
  _cs.Add(   1562, 'Australia - 138'#$00b0'E to 144'#$00b0'E',
           138.00, -48.19, 144.01, -9.08
         );
  _cs.Add(   1563, 'Australia - 144'#$00b0'E to 150'#$00b0'E',
           144.00, -50.89, 150.01, -9.23
         );
  _cs.Add(   1564, 'Australia - 150'#$00b0'E to 156'#$00b0'E',
           150.00, -58.96, 156.00, -13.87
         );
  _cs.Add(   1565, 'Australia - 156'#$00b0'E to 162'#$00b0'E',
           156.00, -35.13, 162.01, -14.08
         );
  _cs.Add(   1566, 'Australia - EEZ east of 162'#$00b0'E',
           162.00, -34.22, 163.20, -27.25
         );
  _cs.Add(   2283, 'Australia - Australian Capital Territory',
           148.76, -35.93, 149.40, -35.12
         );
  _cs.Add(   2284, 'Australia - Northern Territory',
           128.99, -26.01, 138.00, -10.86
         );
  _cs.Add(   2285, 'Australia - Victoria',
           140.96, -39.20, 150.04, -33.98
         );
  _cs.Add(   2286, 'Australia - New South Wales and Victoria',
           140.96, -39.20, 153.69, -28.15
         );
  _cs.Add(   2287, 'Australia - SE Australia (ACT NSW Vic)',
           140.96, -39.20, 153.69, -28.15
         );
  _cs.Add(   2291, 'Australasia - Australia and PNG - 150'#$00b0'E to 156'#$00b0'E',
           150.00, -46.44, 156.00, -2.32
         );
  _cs.Add(   2575, 'Australia - onshore',
           112.85, -43.70, 153.69, -9.86
         );
  _cs.Add(   2576, 'Australia - AGD84',
           109.23, -38.53, 153.61, -9.37
         );
  _cs.Add(   2899, 'Australia - 126'#$00b0'E to 132'#$00b0'E, 8'#$00b0'S to 12'#$00b0'S (SC52) onshore',
           131.02, -12.00, 132.00, -11.07
         );
  _cs.Add(   2900, 'Australia - 132'#$00b0'E to 138'#$00b0'E, 8'#$00b0'S to 12'#$00b0'S (SC53) onshore',
           132.00, -12.00, 136.83, -10.92
         );
  _cs.Add(   2901, 'Australia - 138'#$00b0'E to 144'#$00b0'E, 8'#$00b0'S to 12'#$00b0'S (SC54) onshore',
           141.77, -12.00, 143.31, -10.65
         );
  _cs.Add(   2902, 'Australia - 120'#$00b0'E to 126'#$00b0'E, 12'#$00b0'S to 16'#$00b0'S (SD51) onshore',
           124.17, -16.00, 126.00, -13.87
         );
  _cs.Add(   2903, 'Australia - 126'#$00b0'E to 132'#$00b0'E, 12'#$00b0'S to 16'#$00b0'S (SD52) onshore',
           126.00, -16.00, 132.00, -12.00
         );
  _cs.Add(   2904, 'Australia - 132'#$00b0'E to 138'#$00b0'E, 12'#$00b0'S to 16'#$00b0'S (SD53) onshore',
           132.00, -16.00, 137.28, -12.00
         );
  _cs.Add(   2905, 'Australia - 138'#$00b0'E to 144'#$00b0'E, 12'#$00b0'S to 16'#$00b0'S (SD54) onshore',
           141.34, -16.00, 144.01, -12.00
         );
  _cs.Add(   2906, 'Australia - 144'#$00b0'E to 150'#$00b0'E, 12'#$00b0'S to 16'#$00b0'S (SD55) onshore',
           144.00, -16.00, 145.49, -14.01
         );
  _cs.Add(   2907, 'Australia - 114'#$00b0'E to 120'#$00b0'E, 16'#$00b0'S to 20'#$00b0'S (SE50) onshore',
           118.94, -20.00, 120.00, -19.88
         );
  _cs.Add(   2908, 'Australia - 120'#$00b0'E to 126'#$00b0'E, 16'#$00b0'S to 20'#$00b0'S (SE51) onshore',
           120.00, -20.00, 126.00, -16.00
         );
  _cs.Add(   2909, 'Australia - 126'#$00b0'E to 132'#$00b0'E, 16'#$00b0'S to 20'#$00b0'S (SE52)',
           126.00, -20.00, 132.00, -16.00
         );
  _cs.Add(   2910, 'Australia - 132'#$00b0'E to 138'#$00b0'E, 16'#$00b0'S to 20'#$00b0'S (SE53) onshore',
           132.00, -20.00, 138.00, -16.00
         );
  _cs.Add(   2911, 'Australia - 138'#$00b0'E to 144'#$00b0'E, 16'#$00b0'S to 20'#$00b0'S (SE54) onshore',
           138.00, -20.00, 144.00, -16.00
         );
  _cs.Add(   2912, 'Australia - 144'#$00b0'E to 150'#$00b0'E, 16'#$00b0'S to 20'#$00b0'S (SE55) onshore',
           144.00, -20.00, 148.97, -16.00
         );
  _cs.Add(   2913, 'Australia - 108'#$00b0'E to 114'#$00b0'E, 20'#$00b0'S to 24'#$00b0'S (SF49) onshore',
           113.39, -24.00, 114.00, -21.80
         );
  _cs.Add(   2914, 'Australia - 114'#$00b0'E to 120'#$00b0'E, 20'#$00b0'S to 24'#$00b0'S (SF50) onshore',
           114.00, -24.00, 120.00, -20.00
         );
  _cs.Add(   2915, 'Australia - 120'#$00b0'E to 126'#$00b0'E, 20'#$00b0'S to 24'#$00b0'S (SF51)',
           120.00, -24.00, 126.00, -20.00
         );
  _cs.Add(   2916, 'Australia - 126'#$00b0'E to 132'#$00b0'E, 20'#$00b0'S to 24'#$00b0'S (SF52)',
           126.00, -24.00, 132.00, -20.00
         );
  _cs.Add(   2917, 'Australia - 132'#$00b0'E to 138'#$00b0'E, 20'#$00b0'S to 24'#$00b0'S (SF53)',
           132.00, -24.00, 138.00, -20.00
         );
  _cs.Add(   2918, 'Australia - 138'#$00b0'E to 144'#$00b0'E, 20'#$00b0'S to 24'#$00b0'S (SF54)',
           138.00, -24.00, 144.00, -20.00
         );
  _cs.Add(   2919, 'Australia - 144'#$00b0'E to 150'#$00b0'E, 20'#$00b0'S to 24'#$00b0'S (SF55) onshore',
           144.00, -24.00, 150.00, -20.00
         );
  _cs.Add(   2920, 'Australia - 150'#$00b0'E to 156'#$00b0'E, 20'#$00b0'S to 24'#$00b0'S (SF56) onshore',
           150.00, -24.00, 151.82, -22.00
         );
  _cs.Add(   2921, 'Australia - 108'#$00b0'E to 114'#$00b0'E, 24'#$00b0'S to 28'#$00b0'S (SG49) onshore',
           112.85, -27.44, 114.00, -24.00
         );
  _cs.Add(   2922, 'Australia - 114'#$00b0'E to 120'#$00b0'E, 24'#$00b0'S to 28'#$00b0'S (SG50) onshore',
           114.00, -28.00, 120.00, -24.00
         );
  _cs.Add(   2923, 'Australia - 120'#$00b0'E to 126'#$00b0'E, 24'#$00b0'S to 28'#$00b0'S (SG51)',
           120.00, -28.00, 126.00, -24.00
         );
  _cs.Add(   2924, 'Australia - 126'#$00b0'E to 132'#$00b0'E, 24'#$00b0'S to 28'#$00b0'S (SG52)',
           126.00, -28.00, 132.00, -24.00
         );
  _cs.Add(   2925, 'Australia - 132'#$00b0'E to 138'#$00b0'E, 24'#$00b0'S to 28'#$00b0'S (SG53)',
           132.00, -28.00, 138.00, -24.00
         );
  _cs.Add(   2926, 'Australia - 138'#$00b0'E to 144'#$00b0'E, 24'#$00b0'S to 28'#$00b0'S (SG54)',
           138.00, -28.00, 144.00, -24.00
         );
  _cs.Add(   2927, 'Australia - 144'#$00b0'E to 150'#$00b0'E, 24'#$00b0'S to 28'#$00b0'S (SG55)',
           144.00, -28.00, 150.00, -24.00
         );
  _cs.Add(   2928, 'Australia - 150'#$00b0'E to 156'#$00b0'E, 24'#$00b0'S to 28'#$00b0'S (SG56) onshore',
           150.00, -28.00, 153.60, -24.00
         );
  _cs.Add(   2929, 'Australia - 108'#$00b0'E to 114'#$00b0'E, 28'#$00b0'S to 32'#$00b0'S (SH49)',
           108.00, -32.00, 114.00, -28.00
         )         .MarkDeprecated( True );
  _cs.Add(   2930, 'Australia - 114'#$00b0'E to 120'#$00b0'E, 28'#$00b0'S to 32'#$00b0'S (SH50) onshore',
           114.07, -32.00, 120.00, -28.00
         );
  _cs.Add(   2931, 'Australia - 120'#$00b0'E to 126'#$00b0'E, 28'#$00b0'S to 32'#$00b0'S (SH51)',
           120.00, -32.00, 126.00, -28.00
         );
  _cs.Add(   2932, 'Australia - 126'#$00b0'E to 132'#$00b0'E, south of 28'#$00b0'S (SH52) onshore',
           126.00, -32.37, 132.00, -28.00
         );
  _cs.Add(   2933, 'Australia - 132'#$00b0'E to 138'#$00b0'E, 28'#$00b0'S to 32'#$00b0'S (SH53) onshore',
           132.00, -32.00, 138.00, -28.00
         );
  _cs.Add(   2934, 'Australia - 138'#$00b0'E to 144'#$00b0'E, 28'#$00b0'S to 32'#$00b0'S (SH54)',
           138.00, -32.00, 144.00, -28.00
         );
  _cs.Add(   2935, 'Australia - 144'#$00b0'E to 150'#$00b0'E, 28'#$00b0'S to 32'#$00b0'S (SH55)',
           144.00, -32.00, 150.00, -28.00
         );
  _cs.Add(   2936, 'Australia - 150'#$00b0'E to 156'#$00b0'E, 28'#$00b0'S to 32'#$00b0'S (SH56) onshore',
           150.00, -32.00, 153.69, -28.00
         );
  _cs.Add(   2937, 'Australia - 114'#$00b0'E to 120'#$00b0'E, 32'#$00b0'S to 36'#$00b0'S (SI50) onshore',
           114.89, -35.19, 120.00, -32.00
         );
  _cs.Add(   2938, 'Australia - 120'#$00b0'E to 126'#$00b0'E, 32'#$00b0'S to 36'#$00b0'S (SI51) onshore',
           120.00, -34.16, 126.00, -32.00
         );
  _cs.Add(   2939, 'Australia - 132'#$00b0'E to 138'#$00b0'E, 32'#$00b0'S to 36'#$00b0'S (SI53) onshore',
           132.08, -36.00, 138.00, -32.00
         );
  _cs.Add(   2940, 'Australia - 138'#$00b0'E to 144'#$00b0'E, 32'#$00b0'S to 36'#$00b0'S (SI54) onshore',
           138.00, -36.00, 144.00, -32.00
         );
  _cs.Add(   2941, 'Australia - 144'#$00b0'E to 150'#$00b0'E, 32'#$00b0'S to 36'#$00b0'S (SI55)',
           144.00, -36.00, 150.00, -32.00
         );
  _cs.Add(   2942, 'Australia - 150'#$00b0'E to 156'#$00b0'E, 32'#$00b0'S to 36'#$00b0'S (SI56) onshore',
           150.00, -36.00, 152.66, -32.00
         );
  _cs.Add(   2943, 'Australia - 132'#$00b0'E to 138'#$00b0'E, 36'#$00b0'S to 40'#$00b0'S (SJ53) onshore',
           136.57, -36.14, 137.68, -36.00
         );
  _cs.Add(   2944, 'Australia - 138'#$00b0'E to 144'#$00b0'E, 36'#$00b0'S to 40'#$00b0'S (SJ54) onshore',
           139.38, -38.91, 144.00, -36.00
         );
  _cs.Add(   2945, 'Australia - 144'#$00b0'E to 150'#$00b0'E, 36'#$00b0'S to 40'#$00b0'S (SJ55) onshore',
           144.00, -39.20, 150.00, -36.00
         );
  _cs.Add(   2946, 'Australia - 150'#$00b0'E to 156'#$00b0'E, 36'#$00b0'S to 40'#$00b0'S (SJ56) onshore',
           150.00, -37.57, 150.22, -36.00
         );
  _cs.Add(   2947, 'Australia - Tasmania mainland onshore',
           144.55, -43.70, 148.44, -40.24
         );
  _cs.Add(   2986, 'Australia - SA',
           128.99, -38.13, 141.01, -25.99
         );
  _cs.Add(   2990, 'Australia - Brisbane',
           151.19, -29.88, 153.69, -24.64
         );
  _cs.Add(   3139, 'Australia - New South Wales',
           140.99, -37.53, 153.69, -28.15
         );
  _cs.Add(   3559, 'Australia - offshore',
           109.23, -47.20, 163.20, -8.88
         );
  _cs.Add(   3687, 'Australia - SA and WA 126'#$00b0'E to 132'#$00b0'E',
           125.99, -37.05, 132.01, -9.37
         );
  _cs.Add(   3688, 'Australia - SA 132'#$00b0'E to 138'#$00b0'E',
           132.00, -36.14, 138.00, -25.99
         );
  _cs.Add(   3689, 'Australia - SA and Qld 138'#$00b0'E to 144'#$00b0'E',
           138.00, -38.13, 144.01, -9.86
         );
  _cs.Add(   3690, 'Australia - Qld 144'#$00b0'E to 150'#$00b0'E',
           144.00, -29.01, 150.00, -14.01
         );
  _cs.Add(   3691, 'Australia - Qld east of 150'#$00b0'E',
           150.00, -29.19, 153.61, -22.00
         );
  _cs.Add(   4004, 'Australia - Northern Territory mainland',
           128.99, -26.01, 138.00, -10.92
         );
  _cs.Add(   4017, 'Australia - SE Australia (ACT NSW)',
           140.99, -37.53, 153.69, -28.15
         );
  _cs.Add(   4021, 'Australia - Queensland mainland',
           137.99, -29.19, 153.61, -10.65
         );
  _cs.Add(   4174, 'Australia',
           105.80, -60.55, 164.69, -8.88
         );
  _cs.Add(   4177, 'Australia - GDA',
           93.41, -60.55, 173.34, -8.47
         );
  _cs.Add(   4178, 'Australia - 114'#$00b0'E to 120'#$00b0'E',
           114.00, -38.53, 120.01, -12.06
         );
  _cs.Add(   4196, 'Australia - 156'#$00b0'E to 162'#$00b0'E including Macquarie',
           156.00, -60.55, 162.00, -14.08
         );
  _cs.Add(   4437, 'Australia - Western Australia - Busselton',
           115.18, -33.75, 115.87, -33.40
         );
  _cs.Add(   4438, 'Australia - Western Australia - Barrow',
           114.90, -22.20, 115.59, -20.21
         );
  _cs.Add(   4439, 'Australia - Western Australia - Albany',
           117.55, -35.21, 118.22, -34.75
         );
  _cs.Add(   4440, 'Australia - Western Australia - Jurien Bay',
           114.83, -30.74, 115.34, -29.08
         );
  _cs.Add(   4441, 'Australia - Western Australia - Broome',
           122.08, -18.09, 122.62, -16.75
         );
  _cs.Add(   4442, 'Australia - Western Australia - Carnarvon',
           113.33, -25.50, 114.00, -23.00
         );
  _cs.Add(   4443, 'Australia - Western Australia - Collie',
           115.73, -34.67, 116.40, -33.25
         );
end;

procedure prepare_5;
begin
  _cs.Add(   4445, 'Australia - Western Australia - Esperance',
           121.56, -34.50, 122.20, -33.33
         );
  _cs.Add(   4448, 'Australia - Western Australia - Exmouth',
           113.75, -22.42, 114.25, -21.75
         );
  _cs.Add(   4449, 'Australia - Western Australia - Geraldton',
           114.51, -29.10, 115.00, -28.48
         );
  _cs.Add(   4491, 'Australia - Queensland - Weipa',
           141.50, -13.50, 142.51, -11.49
         );
  _cs.Add(   1037, 'Austria',
           9.53, 46.40, 17.17, 49.02
         );
  _cs.Add(   1038, 'Azerbaijan',
           44.77, 37.89, 51.73, 42.59
         );
  _cs.Add(   1039, 'Bahamas',
           -81.22, 20.36, -70.63, 30.36
         );
  _cs.Add(   1040, 'Bahrain',
           50.26, 25.53, 51.13, 27.17
         );
  _cs.Add(   1041, 'Bangladesh',
           88.01, 18.56, 92.67, 26.64
         );
  _cs.Add(   1042, 'Barbados',
           -60.39, 10.68, -55.99, 16.00
         );
  _cs.Add(   1043, 'Belarus',
           23.16, 51.25, 32.75, 56.17
         );
  _cs.Add(   1044, 'Belgium',
           2.23, 49.50, 6.40, 51.88
         );
  _cs.Add(   1045, 'Belize',
           -89.22, 15.88, -86.11, 18.49
         );
  _cs.Add(   1046, 'Benin',
           0.77, 2.99, 3.86, 12.40
         );
  _cs.Add(   1047, 'Bermuda',
           -68.83, 28.91, -60.70, 35.73
         );
  _cs.Add(   1048, 'Bhutan',
           88.74, 26.70, 92.13, 28.33
         );
  _cs.Add(   1049, 'Bolivia',
           -69.66, -22.91, -57.52, -9.67
         );
  _cs.Add(   1050, 'Bosnia and Herzegovina',
           15.74, 42.56, 19.62, 45.27
         );
  _cs.Add(   1051, 'Botswana',
           19.99, -26.88, 29.38, -17.78
         );
  _cs.Add(   1052, 'Bouvet Island',
           -2.38, -57.80, 9.21, -51.06
         );
  _cs.Add(   1053, 'Brazil',
           -74.01, -35.71, -25.28, 7.04
         );
  _cs.Add(   1347, 'Belgium - onshore',
           2.50, 49.50, 6.40, 51.51
         );
  _cs.Add(   1543, 'Austria - Styria',
           13.58, 46.64, 16.17, 47.84
         );
  _cs.Add(   1572, 'Brazil - 54'#$00b0'W to 48'#$00b0'W and Aratu',
           -53.38, -35.71, -47.99, -25.01
         );
  _cs.Add(   1573, 'Brazil - 48'#$00b0'W to 42'#$00b0'W and Aratu',
           -48.01, -33.50, -41.99, 0.00
         );
  _cs.Add(   1574, 'Brazil - 42'#$00b0'W to 36'#$00b0'W and Aratu',
           -42.01, -26.35, -36.00, 0.01
         );
  _cs.Add(   1615, 'Botswana - west of 24'#$00b0'E',
           19.99, -26.88, 24.00, -17.99
         );
  _cs.Add(   1616, 'Botswana - 21'#$00b0'E to 27'#$00b0'E',
           21.00, -26.88, 27.00, -17.78
         )         .MarkDeprecated( True );
  _cs.Add(   1617, 'Botswana - east of 24'#$00b0'E',
           24.00, -25.84, 29.38, -17.78
         );
  _cs.Add(   1674, 'Bangladesh - onshore west of 90'#$00b0'E',
           88.01, 21.59, 90.00, 26.64
         );
  _cs.Add(   1675, 'Bangladesh - onshore east of 90'#$00b0'E',
           90.00, 20.52, 92.67, 25.29
         );
  _cs.Add(   1706, 'Austria - west of 11'#$00b0'50''E',
           9.53, 46.77, 11.84, 47.61
         );
  _cs.Add(   1707, 'Austria - 11'#$00b0'50''E to 14'#$00b0'50''E',
           11.83, 46.40, 14.84, 48.79
         );
  _cs.Add(   1708, 'Austria - east of 14'#$00b0'50''E',
           14.83, 46.56, 17.17, 49.02
         );
  _cs.Add(   1754, 'Brazil - Amazon cone shelf',
           -51.64, -1.05, -48.00, 5.60
         );
  _cs.Add(   1761, 'Bolivia - 66'#$00b0'W to 60'#$00b0'W',
           -66.00, -22.87, -60.00, -9.67
         );
  _cs.Add(   1817, 'Brazil - 48'#$00b0'W to 42'#$00b0'W',
           -48.00, -26.30, -42.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1818, 'Brazil - 42'#$00b0'W to 36'#$00b0'W onshore',
           -42.00, -22.96, -36.00, -2.68
         );
  _cs.Add(   1819, 'Brazil - 36'#$00b0'W to 30'#$00b0'W',
           -36.00, -10.05, -30.00, -5.15
         )         .MarkDeprecated( True );
  _cs.Add(   2400, 'Bolivia - Madidi',
           -68.96, -14.43, -67.79, -13.56
         );
  _cs.Add(   2401, 'Bolivia - Block 20',
           -63.44, -21.71, -62.95, -21.09
         );
  _cs.Add(   2413, 'Bahamas - main islands onshore',
           -79.04, 20.86, -72.68, 27.29
         );
  _cs.Add(   2414, 'Bahamas (San Salvador Island) - onshore',
           -74.60, 23.90, -74.37, 24.19
         );
  _cs.Add(   2592, 'Azerbaijan - offshore and Sangachal',
           48.66, 37.89, 51.73, 42.59
         );
  _cs.Add(   2594, 'Azerbaijan - coastal area Baku to Astara',
           48.93, 38.31, 50.40, 40.33
         );
  _cs.Add(   3176, 'Brazil - 54'#$00b0'W to 48'#$00b0'W and south of 15'#$00b0'S',
           -54.00, -33.78, -48.00, -15.00
         );
  _cs.Add(   3177, 'Brazil - 48'#$00b0'W to 42'#$00b0'W and south of 15'#$00b0'S',
           -48.00, -25.29, -42.00, -15.00
         );
  _cs.Add(   3217, 'Bangladesh - onshore',
           88.01, 20.52, 92.67, 26.64
         );
  _cs.Add(   3218, 'Barbados - onshore',
           -59.71, 13.00, -59.37, 13.39
         );
  _cs.Add(   3219, 'Belize - onshore',
           -89.22, 15.88, -87.72, 18.49
         );
  _cs.Add(   3220, 'Benin - onshore',
           0.77, 6.17, 3.86, 12.40
         );
  _cs.Add(   3221, 'Bermuda - onshore',
           -64.89, 32.21, -64.61, 32.43
         );
  _cs.Add(   3222, 'Bouvet Island - onshore',
           3.29, -54.52, 3.54, -54.33
         );
  _cs.Add(   3445, 'Brazil - 48'#$00b0'W to 42'#$00b0'W',
           -48.00, -33.50, -42.00, 5.13
         );
  _cs.Add(   3446, 'Brazil - 42'#$00b0'W to 36'#$00b0'W',
           -42.00, -26.35, -36.00, 0.74
         );
  _cs.Add(   3447, 'Brazil - 36'#$00b0'W to 30'#$00b0'W',
           -36.00, -23.80, -29.99, 4.19
         );
  _cs.Add(   3733, 'Bolivia - east of 60'#$00b0'W',
           -60.00, -20.17, -57.52, -16.27
         );
  _cs.Add(   3734, 'Bhutan - Bumthang district',
           90.46, 27.33, 91.02, 28.09
         );
  _cs.Add(   3737, 'Bhutan - Chhukha district',
           89.26, 26.71, 89.83, 27.32
         );
  _cs.Add(   3738, 'Bhutan - Dagana district',
           89.63, 26.70, 90.08, 27.29
         );
  _cs.Add(   3740, 'Bhutan - Gasa district',
           89.44, 27.72, 90.47, 28.33
         );
  _cs.Add(   3742, 'Bhutan - Ha district',
           88.90, 27.02, 89.39, 27.62
         );
  _cs.Add(   3743, 'Bhutan - Lhuentse district',
           90.77, 27.39, 91.49, 28.09
         );
  _cs.Add(   3745, 'Bhutan - Mongar district',
           90.95, 26.93, 91.50, 27.61
         );
  _cs.Add(   3746, 'Bhutan - Paro district',
           89.12, 27.18, 89.56, 27.79
         );
  _cs.Add(   3747, 'Bhutan - Pemagatshel district',
           91.00, 26.78, 91.56, 27.18
         );
  _cs.Add(   3749, 'Bhutan - Punakha district',
           89.63, 27.46, 90.08, 27.87
         );
  _cs.Add(   3750, 'Bhutan - Samdrup Jongkhar district',
           91.39, 26.79, 92.13, 27.25
         );
  _cs.Add(   3751, 'Bhutan - Samtse district',
           88.74, 26.80, 89.38, 27.28
         );
  _cs.Add(   3752, 'Bhutan - Sarpang district',
           90.01, 26.73, 90.78, 27.23
         );
  _cs.Add(   3753, 'Bhutan - Thimphu district',
           89.22, 27.14, 89.77, 28.01
         );
  _cs.Add(   3754, 'Bhutan - Trashigang district',
           91.37, 27.01, 92.13, 27.49
         );
  _cs.Add(   3755, 'Bhutan - Trongsa district',
           90.26, 27.13, 90.76, 27.79
         );
  _cs.Add(   3757, 'Bhutan - Tsirang district',
           90.00, 26.81, 90.35, 27.20
         );
  _cs.Add(   3758, 'Bhutan - Wangdue Phodrang district',
           89.71, 27.11, 90.54, 28.08
         );
  _cs.Add(   3760, 'Bhutan - Yangtse district',
           91.34, 27.37, 91.77, 28.00
         );
  _cs.Add(   3761, 'Bhutan - Zhemgang district',
           90.53, 26.77, 91.19, 27.39
         );
  _cs.Add(   3805, 'Bonaire, St Eustatius and Saba',
           -69.09, 11.66, -62.76, 17.96
         );
  _cs.Add(   3808, 'Brazil - 36'#$00b0'W to 30'#$00b0'W offshore',
           -36.00, -20.11, -30.00, 0.00
         );
  _cs.Add(   3820, 'Bonaire - St Eustatius and Saba',
           -64.02, 16.68, -62.76, 17.96
         );
  _cs.Add(   3821, 'Bonaire',
           -69.09, 11.66, -67.98, 15.30
         );
  _cs.Add(   3822, 'Bonaire - onshore',
           -68.47, 11.97, -68.14, 12.36
         );
  _cs.Add(   3827, 'Bolivia - west of 66'#$00b0'W',
           -69.66, -22.91, -66.00, -9.77
         );
  _cs.Add(   3851, 'Brazil - 36'#$00b0'W to 30'#$00b0'W SAD69',
           -36.00, -20.10, -30.00, -0.49
         )         .MarkDeprecated( True );
  _cs.Add(   3877, 'Brazil - 42'#$00b0'W to 36'#$00b0'W and south of 15'#$00b0'S onshore',
           -42.00, -22.96, -38.82, -14.99
         );
  _cs.Add(   3878, 'Brazil - 54'#$00b0'W to 48'#$00b0'W and SAD69',
           -54.01, -35.71, -47.99, 7.04
         );
  _cs.Add(   3881, 'Brazil - 60'#$00b0'W to 54'#$00b0'W',
           -60.00, -31.91, -53.99, 4.51
         );
  _cs.Add(   3943, 'Bahrain - onshore',
           50.39, 25.53, 50.85, 26.34
         );
  _cs.Add(   4014, 'Australia - Western Australia mainland',
           112.85, -35.19, 129.01, -13.67
         );
  _cs.Add(   4024, 'Brazil - 72'#$00b0'W to 66'#$00b0'W',
           -72.00, -11.14, -65.99, 2.15
         );
  _cs.Add(   4026, 'Brazil - 66'#$00b0'W to 60'#$00b0'W',
           -66.00, -16.28, -59.99, 5.28
         );
  _cs.Add(   4129, 'Brazil - 48'#$00b0'W to 42'#$00b0'W and north of 0'#$00b0'N',
           -48.00, 0.00, -41.99, 5.13
         );
  _cs.Add(   4133, 'Brazil - 42'#$00b0'W to 36'#$00b0'W and north of 0'#$00b0'N',
           -42.00, 0.00, -38.22, 0.74
         );
  _cs.Add(   4436, 'Australia - Western Australia - Kalgoorlie',
           121.00, -32.25, 121.84, -28.75
         );
  _cs.Add(   4444, 'Australia - Western Australia - Kalbarri',
           113.90, -28.50, 114.75, -27.16
         );
  _cs.Add(   4451, 'Australia - Western Australia - Karratha',
           116.58, -20.92, 117.25, -20.25
         );
  _cs.Add(   4452, 'Australia - Western Australia - Kununurra',
           128.50, -16.75, 129.00, -14.75
         );
  _cs.Add(   4453, 'Australia - Western Australia - Lancelin',
           115.15, -31.49, 115.62, -30.71
         );
  _cs.Add(   4457, 'Australia - Western Australia - Margaret River',
           114.96, -34.42, 115.24, -33.51
         );
  _cs.Add(   4462, 'Australia - Western Australia - Perth',
           115.44, -33.42, 116.09, -31.33
         );
end;

procedure prepare_6;
begin
  _cs.Add(   4466, 'Australia - Western Australia - Port Hedland',
           118.25, -20.79, 118.97, -20.10
         );
  _cs.Add(   4493, 'Australia Christmas and Cocos - onshore',
           96.76, -43.70, 153.69, -9.86
         );
  _cs.Add(   4576, 'Brazil - 54'#$00b0'W to 48'#$00b0'W and 15'#$00b0'S to 27'#$00b0'30''S',
           -54.00, -27.50, -47.99, -14.99
         );
  _cs.Add(   4585, 'Austria - Vienna',
           16.18, 48.12, 16.59, 48.34
         );
  _cs.Add(   1054, 'British Indian Ocean Territory',
           67.88, -10.80, 75.86, -2.28
         );
  _cs.Add(   1055, 'Brunei',
           112.37, 4.01, 115.37, 6.31
         );
  _cs.Add(   1056, 'Bulgaria',
           22.36, 41.24, 31.35, 44.23
         );
  _cs.Add(   1057, 'Burkina Faso',
           -5.53, 9.39, 2.40, 15.09
         );
  _cs.Add(   1058, 'Burundi',
           28.98, -4.45, 30.86, -2.30
         );
  _cs.Add(   1059, 'Cambodia',
           101.33, 8.78, 107.64, 14.73
         );
  _cs.Add(   1060, 'Cameroon',
           8.32, 1.65, 16.21, 13.09
         );
  _cs.Add(   1061, 'Canada',
           -141.01, 38.21, -40.73, 86.46
         );
  _cs.Add(   1274, 'Brazil - Aratu',
           -53.38, -35.71, -26.00, 4.26
         );
  _cs.Add(   1283, 'Canada - Maritime Provinces',
           -69.05, 43.41, -59.73, 48.07
         );
  _cs.Add(   1289, 'Canada - CGVD28',
           -141.01, 41.67, -59.73, 69.80
         );
  _cs.Add(   1293, 'Brazil - Corrego Alegre 1970-1972',
           -58.16, -33.78, -34.74, -2.68
         );
  _cs.Add(   1336, 'Canada - CSRS98',
           -120.00, 44.61, -57.10, 62.56
         )         .MarkDeprecated( True );
  _cs.Add(   1447, 'Canada - New Brunswick',
           -69.05, 44.56, -63.70, 48.07
         );
  _cs.Add(   1448, 'Canada - 72'#$00b0'W to 66'#$00b0'W, south of 62'#$00b0'N',
           -72.00, 44.60, -66.00, 61.50
         )         .MarkDeprecated( True );
  _cs.Add(   1449, 'Canada - 66'#$00b0'W to 60'#$00b0'W, south of 60'#$00b0'N',
           -66.00, 43.20, -60.00, 60.00
         )         .MarkDeprecated( True );
  _cs.Add(   1506, 'Canada - 108'#$00b0'W to 102'#$00b0'W, south of 60'#$00b0'N',
           -108.00, 49.00, -102.00, 60.00
         )         .MarkDeprecated( True );
  _cs.Add(   1507, 'Canada - 114'#$00b0'W to 108'#$00b0'W, south of 60'#$00b0'N',
           -114.00, 49.00, -108.00, 60.00
         )         .MarkDeprecated( True );
  _cs.Add(   1508, 'Canada - 120'#$00b0'W to 114'#$00b0'W, south of 60'#$00b0'N',
           -120.00, 49.00, -114.00, 60.00
         )         .MarkDeprecated( True );
  _cs.Add(   1531, 'Canada - Maritime Provinces - west of 66'#$00b0'W',
           -69.00, 43.64, -66.00, 48.07
         );
  _cs.Add(   1532, 'Canada - Maritime Provinces - east of 66'#$00b0'W',
           -66.00, 43.41, -59.73, 47.98
         );
  _cs.Add(   1621, 'Brazil - Corrego Alegre - west of 42'#$00b0'W',
           -45.00, -3.90, -42.00, -1.50
         )         .MarkDeprecated( True );
  _cs.Add(   1622, 'Brazil - Corrego Alegre - east of 42'#$00b0'W',
           -42.00, -4.00, -40.00, -2.70
         )         .MarkDeprecated( True );
  _cs.Add(   2151, 'Canada - 60'#$00b0'W to 54'#$00b0'W',
           -60.00, 38.56, -54.00, 84.00
         );
  _cs.Add(   2152, 'Canada - 54'#$00b0'W to 48'#$00b0'W',
           -54.00, 39.50, -48.00, 57.65
         );
  _cs.Add(   2153, 'Canada - 48'#$00b0'W to 42'#$00b0'W',
           -48.00, 39.85, -42.00, 54.47
         );
  _cs.Add(   2307, 'Brazil - Campos; Espirito Santo and Santos basins',
           -48.80, -28.41, -35.18, -17.59
         );
  _cs.Add(   2308, 'Brazil - Tucano basin north',
           -39.04, -9.80, -37.09, -8.39
         );
  _cs.Add(   2309, 'Brazil - Tucano basin central',
           -39.14, -10.61, -37.99, -9.79
         );
  _cs.Add(   2310, 'Brazil - Tucano basin south',
           -39.07, -12.27, -37.98, -10.60
         );
  _cs.Add(   2348, 'Brunei - offshore',
           112.50, 4.58, 115.24, 6.37
         )         .MarkDeprecated( True );
  _cs.Add(   2349, 'Brunei - onshore',
           114.09, 4.01, 115.37, 5.11
         );
  _cs.Add(   2376, 'Canada - Alberta',
           -120.00, 48.99, -109.98, 60.00
         );
  _cs.Add(   2384, 'Canada - Alberta and British Columbia',
           -139.04, 48.25, -109.98, 60.01
         );
  _cs.Add(   2415, 'Canada - Manitoba and Ontario',
           -102.00, 41.67, -74.35, 60.01
         );
  _cs.Add(   2416, 'Canada - eastern provinces',
           -79.85, 43.41, -52.54, 62.62
         );
  _cs.Add(   2555, 'Cameroon - coastal area',
           8.45, 2.16, 10.40, 4.99
         );
  _cs.Add(   2590, 'Cameroon - Garoua area',
           12.90, 8.92, 14.19, 9.87
         );
  _cs.Add(   2591, 'Cameroon - N''Djamena area',
           14.17, 11.70, 15.09, 12.77
         );
  _cs.Add(   2831, 'Canada - Atlantic offshore',
           -67.75, 40.00, -44.00, 64.21
         );
  _cs.Add(   2832, 'Canada - British Columbia',
           -139.04, 48.25, -114.08, 60.01
         );
  _cs.Add(   2962, 'Brazil - Santos',
           -48.80, -28.41, -40.20, -22.66
         );
  _cs.Add(   2963, 'Brazil - Campos',
           -42.04, -25.91, -37.11, -20.45
         );
  _cs.Add(   2964, 'Brazil - Espirito Santo and Mucuri',
           -40.37, -22.04, -35.18, -17.59
         );
  _cs.Add(   2965, 'Brazil - Pelotas',
           -53.38, -35.71, -44.71, -28.11
         );
  _cs.Add(   2966, 'Brazil - offshore 18'#$00b0'S to 34'#$00b0'S',
           -53.38, -34.00, -35.19, -18.00
         );
  _cs.Add(   3178, 'Brazil - east of 36'#$00b0'W onshore',
           -36.00, -10.10, -34.74, -4.99
         );
  _cs.Add(   3189, 'British Indian Ocean Territory - Diego Garcia',
           72.30, -7.49, 72.55, -7.18
         );
  _cs.Add(   3223, 'Brazil - onshore',
           -74.01, -33.78, -34.74, 5.28
         );
  _cs.Add(   3224, 'Bulgaria - onshore',
           22.36, 41.24, 28.68, 44.23
         );
  _cs.Add(   3225, 'Cambodia - onshore',
           102.34, 10.39, 107.64, 14.73
         );
  _cs.Add(   3226, 'Cameroon - onshore',
           8.45, 1.65, 16.21, 13.09
         );
  _cs.Add(   3355, 'Brazil - west of 54'#$00b0'W and south of 18'#$00b0'S',
           -58.16, -31.91, -54.00, -17.99
         );
  _cs.Add(   3409, 'Canada - 144'#$00b0'W to 138'#$00b0'W',
           -141.01, 52.05, -138.00, 72.53
         );
  _cs.Add(   3410, 'Canada - 138'#$00b0'W to 132'#$00b0'W',
           -138.00, 48.06, -132.00, 79.42
         );
  _cs.Add(   3411, 'Canada - 132'#$00b0'W to 126'#$00b0'W',
           -132.00, 46.52, -126.00, 80.93
         );
  _cs.Add(   3412, 'Canada - 126'#$00b0'W to 120'#$00b0'W',
           -126.00, 48.13, -120.00, 81.80
         );
  _cs.Add(   3413, 'Canada - 102'#$00b0'W to 96'#$00b0'W',
           -102.00, 48.99, -96.00, 84.00
         );
  _cs.Add(   3414, 'Canada - 96'#$00b0'W to 90'#$00b0'W',
           -96.00, 48.03, -90.00, 84.00
         );
  _cs.Add(   3415, 'Canada - 90'#$00b0'W to 84'#$00b0'W',
           -90.00, 46.11, -84.00, 84.00
         );
  _cs.Add(   3416, 'Canada - 84'#$00b0'W to 78'#$00b0'W',
           -84.00, 41.67, -78.00, 84.00
         );
  _cs.Add(   3417, 'Canada - 78'#$00b0'W to 72'#$00b0'W',
           -78.00, 43.63, -72.00, 84.00
         );
  _cs.Add(   3524, 'Canada - 72'#$00b0'W to 66'#$00b0'W',
           -72.00, 40.80, -66.00, 84.00
         );
  _cs.Add(   3525, 'Canada - 66'#$00b0'W to 60'#$00b0'W',
           -66.00, 40.04, -60.00, 84.00
         );
  _cs.Add(   3526, 'Canada - 108'#$00b0'W to 102'#$00b0'W',
           -108.00, 48.99, -102.00, 84.00
         );
  _cs.Add(   3527, 'Canada - 114'#$00b0'W to 108'#$00b0'W',
           -114.00, 48.99, -108.00, 84.00
         );
  _cs.Add(   3528, 'Canada - 120'#$00b0'W to 114'#$00b0'W',
           -120.00, 48.99, -114.00, 83.50
         );
  _cs.Add(   3540, 'Canada - Alberta - west of 118.5'#$00b0'W',
           -120.00, 52.88, -118.50, 60.00
         );
  _cs.Add(   3541, 'Canada - Alberta - 118.5'#$00b0'W to 115.5'#$00b0'W',
           -118.50, 50.77, -115.50, 60.00
         );
  _cs.Add(   3542, 'Canada - Alberta - 115.5'#$00b0'W to 112.5'#$00b0'W',
           -115.50, 48.99, -112.50, 60.00
         );
  _cs.Add(   3543, 'Canada - Alberta - east of 112.5'#$00b0'W',
           -112.50, 48.99, -109.98, 60.00
         );
  _cs.Add(   3619, 'Brazil - Distrito Federal',
           -48.10, -15.94, -47.10, -15.37
         );
  _cs.Add(   3692, 'Brazil - Reconcavo and Jacuipe',
           -39.09, -13.57, -35.31, -11.18
         );
  _cs.Add(   3693, 'Brazil - Tucano and Jatoba',
           -39.14, -12.27, -37.09, -8.39
         );
  _cs.Add(   3696, 'Brazil - Sergipe and Alagoas',
           -37.34, -13.58, -32.01, -8.73
         );
  _cs.Add(   3697, 'Brazil - Paraiba-Pernambuco',
           -35.10, -10.17, -29.13, -4.60
         );
  _cs.Add(   3698, 'Brazil - Potiguar, Ceara and Barreirinhas',
           -44.79, -6.50, -26.00, 4.26
         );
  _cs.Add(   3699, 'Brazil - Cumuruxatiba, Jequitinhonha and Camamu-Almada',
           -39.22, -17.70, -34.60, -13.01
         );
  _cs.Add(   3700, 'Brazil - Santos and Pelotas',
           -53.38, -35.71, -40.20, -22.66
         );
  _cs.Add(   3842, 'Brazil - east of 30'#$00b0'W',
           -30.00, -23.86, -25.28, 4.26
         );
  _cs.Add(   3845, 'Brazil - SAD69',
           -60.57, -35.71, -29.03, 7.04
         )         .MarkDeprecated( True );
  _cs.Add(   3865, 'Canada - Labrador - 66'#$00b0'W to 63'#$00b0'W',
           -66.00, 51.58, -63.00, 60.52
         );
  _cs.Add(   3874, 'Brazil - Corrego Alegre 1961',
           -58.16, -27.50, -38.82, -14.99
         );
  _cs.Add(   3875, 'Canada - Labrador - 63'#$00b0'W to 60'#$00b0'W',
           -63.00, 52.00, -60.00, 58.92
         );
  _cs.Add(   3880, 'Canada - Labrador - west of 66'#$00b0'W',
           -67.81, 52.05, -66.00, 55.34
         );
  _cs.Add(   3884, 'Brazil - SAD69 onshore south of 4'#$00b0'30''S',
           -60.57, -33.78, -34.74, -4.50
         )         .MarkDeprecated( True );
  _cs.Add(   3887, 'Brazil - onshore south of 14'#$00b0'S and east of 53'#$00b0'W',
           -60.58, -33.78, -34.74, 4.43
         );
  _cs.Add(   3891, 'Canada - 60'#$00b0'W to 54'#$00b0'W and NAD27',
           -60.00, 40.57, -54.00, 68.93
         );
  _cs.Add(   3896, 'Brazil - equatorial margin',
           -51.64, -5.74, -32.43, 7.04
         );
  _cs.Add(   4023, 'Brazil - west of 72'#$00b0'W',
           -74.01, -10.01, -71.99, -4.59
         );
  _cs.Add(   4426, 'Bulgaria - east of 30'#$00b0'E',
           30.00, 42.56, 31.35, 43.67
         );
  _cs.Add(   4427, 'Bulgaria - 24'#$00b0'E to 30'#$00b0'E',
           24.00, 41.24, 30.01, 44.15
         );
  _cs.Add(   4428, 'Bulgaria - west of 24'#$00b0'E',
           22.36, 41.32, 24.00, 44.23
         );
  _cs.Add(   4517, 'Canada - NAD27',
           -141.01, 40.00, -44.00, 83.17
         );
  _cs.Add(   4533, 'Canada - British Columbia - CRD',
           -124.52, 48.25, -123.00, 49.06
         );
  _cs.Add(   4534, 'Canada - British Columbia - north Vancouver Is',
           -128.50, 48.48, -123.49, 50.93
         );
end;

procedure prepare_7;
begin
  _cs.Add(   4535, 'Canada - British Columbia - mainland',
           -139.04, 48.99, -114.08, 60.01
         );
  _cs.Add(   4574, 'Brazil - west of 54'#$00b0'W and between 18'#$00b0'S and 27'#$00b0'30''S',
           -58.16, -27.50, -54.00, -17.99
         );
  _cs.Add(   4617, 'Canada - east of 42'#$00b0'W',
           -42.00, 45.53, -40.73, 49.53
         );
  _cs.Add(   4618, 'Canada - 41'#$00b0'N to 85'#$00b0'N, west of 50'#$00b0'W',
           -141.01, 41.00, -50.00, 85.00
         );
  _cs.Add(   1062, 'Cape Verde',
           -28.85, 11.47, -19.53, 20.54
         );
  _cs.Add(   1063, 'Cayman Islands',
           -83.60, 17.58, -78.72, 20.68
         );
  _cs.Add(   1064, 'Central African Republic',
           14.41, 2.22, 27.46, 11.01
         );
  _cs.Add(   1065, 'Chad',
           13.46, 7.45, 24.01, 23.46
         );
  _cs.Add(   1066, 'Chile',
           -113.21, -59.87, -65.72, -17.50
         );
  _cs.Add(   1067, 'China',
           73.62, 16.70, 134.77, 53.56
         );
  _cs.Add(   1335, 'Caribbean - Puerto Rico and Virgin Islands - onshore',
           -67.97, 17.62, -64.25, 18.78
         );
  _cs.Add(   1367, 'Canada - Ontario',
           -95.16, 41.67, -74.35, 56.90
         );
  _cs.Add(   1368, 'Canada - Quebec',
           -79.85, 44.99, -57.10, 62.62
         );
  _cs.Add(   1420, 'Canada - Quebec - east of 57'#$00b0'W',
           -57.00, 46.60, -54.00, 53.76
         )         .MarkDeprecated( True );
  _cs.Add(   1421, 'Canada - Quebec - 60'#$00b0'W to 57'#$00b0'W',
           -60.00, 50.10, -57.10, 52.00
         )         .MarkDeprecated( True );
  _cs.Add(   1422, 'Canada - Quebec - 63'#$00b0'W to 60'#$00b0'W',
           -63.00, 47.16, -60.00, 52.01
         );
  _cs.Add(   1423, 'Canada - Quebec - 66'#$00b0'W to 63'#$00b0'W',
           -66.00, 47.95, -63.00, 60.42
         );
  _cs.Add(   1424, 'Canada - Quebec - 69'#$00b0'W to 66'#$00b0'W',
           -69.00, 47.31, -66.00, 59.00
         );
  _cs.Add(   1425, 'Canada - Quebec - 72'#$00b0'W to 69'#$00b0'W',
           -72.00, 45.01, -69.00, 61.80
         );
  _cs.Add(   1426, 'Canada - Quebec - 75'#$00b0'W to 72'#$00b0'W',
           -75.00, 44.99, -72.00, 62.53
         );
  _cs.Add(   1427, 'Canada - Quebec - 78'#$00b0'W to 75'#$00b0'W',
           -78.00, 45.37, -75.00, 62.62
         );
  _cs.Add(   1428, 'Canada - Quebec - west of 78'#$00b0'W',
           -79.85, 46.23, -78.00, 62.45
         );
  _cs.Add(   1429, 'Canada - Ontario - east of 75'#$00b0'W',
           -75.00, 44.98, -74.35, 45.65
         );
  _cs.Add(   1430, 'Canada - Ontario - 78'#$00b0'W to 75'#$00b0'W',
           -78.00, 43.63, -75.00, 46.25
         );
  _cs.Add(   1431, 'Canada - Ontario - MTM zone 10',
           -81.00, 42.26, -77.99, 47.33
         );
  _cs.Add(   1432, 'Canada - Ontario - MTM zone 11',
           -83.60, 41.67, -81.00, 46.00
         );
  _cs.Add(   1433, 'Canada - Ontario - MTM zone 12',
           -82.50, 46.00, -79.50, 55.21
         );
  _cs.Add(   1434, 'Canada - Ontario - MTM zone 13',
           -85.50, 46.00, -82.50, 55.59
         );
  _cs.Add(   1435, 'Canada - Ontario - 88.5'#$00b0'W to 85.5'#$00b0'W',
           -88.50, 47.17, -85.50, 56.70
         );
  _cs.Add(   1436, 'Canada - Ontario - 91.5'#$00b0'W to 88.5'#$00b0'W',
           -91.50, 47.97, -88.50, 56.90
         );
  _cs.Add(   1437, 'Canada - Ontario - 94.5'#$00b0'W to 91.5'#$00b0'W',
           -94.50, 48.06, -91.50, 55.20
         );
  _cs.Add(   1438, 'Canada - Ontario - west of 94.5'#$00b0'W',
           -95.16, 48.69, -94.50, 53.24
         );
  _cs.Add(   1439, 'Canada - Ontario - west of 90'#$00b0'W',
           -95.16, 48.03, -90.00, 56.20
         );
  _cs.Add(   1440, 'Canada - Ontario - 90'#$00b0'W to 84'#$00b0'W',
           -90.00, 46.11, -84.00, 56.90
         );
  _cs.Add(   1441, 'Canada - Ontario - 84'#$00b0'W to 78'#$00b0'W',
           -84.00, 41.67, -78.00, 55.37
         );
  _cs.Add(   1442, 'Canada - Ontario - east of 78'#$00b0'W',
           -78.00, 43.63, -74.35, 46.25
         );
  _cs.Add(   1443, 'Canada - Quebec - 78'#$00b0'W to 72'#$00b0'W',
           -78.00, 44.99, -72.00, 62.62
         );
  _cs.Add(   1444, 'Canada - Quebec - 72'#$00b0'W to 66'#$00b0'W',
           -72.00, 45.01, -66.00, 61.80
         );
  _cs.Add(   1445, 'Canada - Quebec - 66'#$00b0'W to 60'#$00b0'W',
           -66.00, 47.16, -60.00, 60.42
         );
  _cs.Add(   1446, 'Canada - Quebec - east of 60'#$00b0'W',
           -60.00, 50.20, -57.10, 52.01
         );
  _cs.Add(   1533, 'Canada - Prince Edward Island',
           -64.49, 45.90, -61.90, 47.09
         );
  _cs.Add(   1534, 'Canada - Nova Scotia - east of 63'#$00b0'W',
           -63.00, 44.64, -59.73, 47.08
         );
  _cs.Add(   1535, 'Canada - Nova Scotia - west of 63'#$00b0'W',
           -66.28, 43.41, -63.00, 46.02
         );
  _cs.Add(   1592, 'China - 102'#$00b0'E to 108'#$00b0'E onshore',
           102.00, 21.53, 108.00, 42.47
         );
  _cs.Add(   1593, 'China - 108'#$00b0'E to 114'#$00b0'E onshore',
           108.00, 18.11, 114.00, 45.11
         );
  _cs.Add(   1594, 'China - 114'#$00b0'E to 120'#$00b0'E onshore',
           114.00, 22.14, 120.00, 51.52
         );
  _cs.Add(   2226, 'Canada - Newfoundland - east of 54.5'#$00b0'W',
           -54.50, 46.56, -52.54, 49.89
         );
  _cs.Add(   2227, 'Canada - Newfoundland and Labrador - 57.5'#$00b0'W to 54.5'#$00b0'W',
           -57.50, 46.81, -54.50, 54.71
         );
  _cs.Add(   2251, 'Caribbean - Puerto Rico and US Virgin Islands',
           -68.49, 14.92, -63.88, 21.86
         );
  _cs.Add(   2275, 'Canada - Newfoundland and Labrador - 60'#$00b0'W to 57.5'#$00b0'W',
           -59.48, 47.50, -57.50, 50.54
         );
  _cs.Add(   2276, 'Canada - Quebec and Labrador - 63'#$00b0'W to 60'#$00b0'W',
           -63.00, 47.16, -60.00, 58.92
         );
  _cs.Add(   2277, 'Canada - Quebec and Labrador - 66'#$00b0'W to 63'#$00b0'W',
           -66.00, 47.95, -63.00, 60.52
         );
  _cs.Add(   2278, 'Canada - Quebec and Labrador - 69'#$00b0'W to 66'#$00b0'W',
           -69.00, 47.31, -66.00, 59.00
         );
  _cs.Add(   2279, 'Canada - Quebec and Ontario - 75'#$00b0'W to 72'#$00b0'W',
           -75.00, 44.98, -72.00, 62.53
         );
  _cs.Add(   2280, 'Canada - Quebec and Ontario - 78'#$00b0'W to 75'#$00b0'W',
           -78.00, 43.63, -75.00, 62.65
         );
  _cs.Add(   2281, 'Canada - Quebec and Ontario - MTM zone 10',
           -81.00, 42.26, -78.00, 62.45
         );
  _cs.Add(   2290, 'Canada - Quebec, Newfoundland and Labrador - MTM zone 3',
           -60.00, 47.50, -57.10, 55.38
         );
  _cs.Add(   2295, 'Caribbean - Windward and Leeward Islands',
           -63.22, 11.94, -59.37, 18.33
         );
  _cs.Add(   2313, 'Canada - Nova Scotia',
           -66.28, 43.41, -59.73, 47.08
         );
  _cs.Add(   2375, 'Canada - Saskatchewan',
           -110.00, 49.00, -101.34, 60.01
         );
  _cs.Add(   2402, 'Chile - onshore north of 21'#$00b0'30''S',
           -70.49, -21.51, -68.18, -17.50
         );
  _cs.Add(   2403, 'Chile - onshore 39'#$00b0'S to 43'#$00b0'30''S',
           -74.48, -43.50, -71.38, -38.99
         );
  _cs.Add(   2410, 'Canada - NWT; Nunavut; Saskatchewan',
           -136.46, 49.00, -60.72, 83.17
         );
  _cs.Add(   2417, 'Canada - Yukon',
           -141.01, 59.99, -123.91, 69.70
         );
  _cs.Add(   2418, 'Caribbean - central (DMA tfm)',
           -85.01, 13.00, -59.37, 23.25
         );
  _cs.Add(   2419, 'Central America - Belize to Costa Rica',
           -92.29, 7.98, -82.53, 18.49
         );
  _cs.Add(   2720, 'China - 100.5'#$00b0'E to 103.5'#$00b0'E',
           100.50, 21.13, 103.50, 42.69
         );
  _cs.Add(   2721, 'China - 103.5'#$00b0'E to 106.5'#$00b0'E',
           103.50, 22.50, 106.51, 42.21
         );
  _cs.Add(   2722, 'China - 106.5'#$00b0'E to 109.5'#$00b0'E onshore',
           106.50, 18.19, 109.51, 42.47
         );
  _cs.Add(   2723, 'China - 109.5'#$00b0'E to 112.5'#$00b0'E onshore',
           109.50, 18.11, 112.50, 45.11
         );
  _cs.Add(   2724, 'China - 112.5'#$00b0'E to 115.5'#$00b0'E onshore',
           112.50, 21.52, 115.50, 45.45
         );
  _cs.Add(   2725, 'China - 115.5'#$00b0'E to 118.5'#$00b0'E onshore',
           115.50, 22.60, 118.50, 49.88
         );
  _cs.Add(   2726, 'China - 118.5'#$00b0'E to 121.5'#$00b0'E onshore',
           118.50, 24.43, 121.50, 53.33
         );
  _cs.Add(   2784, 'Canada - south of 60'#$00b0'N',
           -139.05, 41.68, -52.62, 60.00
         )         .MarkDeprecated( True );
  _cs.Add(   2805, 'Chile - Tierra del Fuego',
           -74.83, -55.96, -66.33, -51.99
         );
  _cs.Add(   2824, 'Caribbean - French Antilles',
           -63.66, 14.08, -57.52, 18.53
         );
  _cs.Add(   2988, 'Channel Islands - Jersey, Les Ecrehos and Les Minquiers',
           -2.72, 48.77, -1.81, 49.44
         );
  _cs.Add(   2989, 'Channel Islands - Guernsey, Alderney, Sark',
           -3.73, 49.11, -2.02, 50.16
         );
  _cs.Add(   3185, 'Cayman Islands - Grand Cayman',
           -81.46, 19.21, -81.04, 19.41
         );
  _cs.Add(   3186, 'Cayman Islands - Little Cayman and Cayman Brac',
           -80.14, 19.63, -79.69, 19.78
         );
  _cs.Add(   3188, 'Chile - Easter Island onshore',
           -109.51, -27.25, -109.16, -27.01
         );
  _cs.Add(   3207, 'Cayman Islands - Cayman Brac',
           -79.92, 19.66, -79.69, 19.78
         );
  _cs.Add(   3227, 'Chile - onshore north of 45'#$00b0'S',
           -75.22, -45.00, -67.00, -17.50
         );
  _cs.Add(   3481, 'Canada - NWT',
           -136.46, 59.98, -102.00, 78.81
         );
  _cs.Add(   3532, 'Channel Islands - Jersey',
           -2.30, 48.93, -1.98, 49.34
         )         .MarkDeprecated( True );
  _cs.Add(   1069, 'Cocos (Keeling) Islands - onshore',
           96.76, -12.27, 96.99, -11.76
         );
  _cs.Add(   1070, 'Colombia',
           -84.77, -4.23, -66.87, 15.51
         );
  _cs.Add(   1071, 'Comoros',
           41.93, -14.43, 45.79, -8.01
         );
  _cs.Add(   1072, 'Congo',
           8.84, -6.91, 18.65, 3.72
         );
  _cs.Add(   1118, 'China - Hong Kong',
           113.76, 22.13, 114.51, 22.58
         );
  _cs.Add(   1147, 'China - Macao',
           113.52, 22.06, 113.68, 22.23
         );
  _cs.Add(   1259, 'Congo DR (Zaire)',
           11.79, -13.46, 31.31, 5.39
         );
  _cs.Add(   1587, 'China - west of 78'#$00b0'E',
           73.62, 35.42, 78.01, 41.07
         );
  _cs.Add(   1588, 'China - 78'#$00b0'E to 84'#$00b0'E',
           77.98, 29.16, 84.00, 47.23
         );
  _cs.Add(   1589, 'China - 84'#$00b0'E to 90'#$00b0'E',
           84.00, 27.32, 90.00, 49.18
         );
  _cs.Add(   1590, 'China - 90'#$00b0'E to 96'#$00b0'E',
           90.00, 27.71, 96.01, 47.90
         );
  _cs.Add(   1591, 'China - 96'#$00b0'E to 102'#$00b0'E',
           96.00, 21.13, 102.01, 43.18
         );
  _cs.Add(   1595, 'China - 120'#$00b0'E to 126'#$00b0'E onshore',
           120.00, 26.34, 126.00, 53.56
         );
  _cs.Add(   1596, 'China - 126'#$00b0'E to 132'#$00b0'E onshore',
           126.00, 40.89, 132.00, 52.79
         );
  _cs.Add(   1597, 'China - east of 132'#$00b0'E',
           132.00, 45.02, 134.77, 48.40
         );
end;

procedure prepare_8;
begin
  _cs.Add(   1598, 'Colombia - west of 75'#$00b0'35''W',
           -79.10, 0.03, -75.58, 10.21
         );
  _cs.Add(   1599, 'Colombia - 75'#$00b0'35''W to 72'#$00b0'35''W',
           -75.59, -2.51, -72.58, 11.82
         );
  _cs.Add(   1600, 'Colombia - 72'#$00b0'35''W to 69'#$00b0'35''W',
           -72.59, -4.23, -69.58, 12.52
         );
  _cs.Add(   1601, 'Colombia - east of 69'#$00b0'35''W',
           -69.59, -2.25, -66.87, 6.31
         );
  _cs.Add(   1602, 'Colombia - offshore west of 78'#$00b0'W',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1603, 'Colombia - offshore Caribbean west of 72'#$00b0'W',
           -77.37, 7.90, -72.00, 13.68
         );
  _cs.Add(   2315, 'Colombia - Cusiana',
           -73.00, 4.75, -72.25, 5.68
         );
  _cs.Add(   2574, 'Congo - coastal area and offshore',
           8.84, -6.91, 12.34, -3.55
         );
  _cs.Add(   2711, 'China - west of 76.5'#$00b0'E',
           73.62, 35.81, 76.50, 40.65
         );
  _cs.Add(   2712, 'China - 76.5'#$00b0'E to 79.5'#$00b0'E',
           76.50, 31.03, 79.50, 41.83
         );
  _cs.Add(   2713, 'China - 79.5'#$00b0'E to 82.5'#$00b0'E',
           79.50, 29.95, 82.51, 45.88
         );
  _cs.Add(   2714, 'China - 82.5'#$00b0'E to 85.5'#$00b0'E',
           82.50, 28.26, 85.50, 47.23
         );
  _cs.Add(   2715, 'China - 85.5'#$00b0'E to 88.5'#$00b0'E',
           85.50, 27.80, 88.50, 49.18
         );
  _cs.Add(   2716, 'China - 88.5'#$00b0'E to 91.5'#$00b0'E',
           88.49, 27.32, 91.51, 48.42
         );
  _cs.Add(   2717, 'China - 91.5'#$00b0'E to 94.5'#$00b0'E',
           91.50, 27.71, 94.50, 45.13
         );
  _cs.Add(   2718, 'China - 94.5'#$00b0'E to 97.5'#$00b0'E',
           94.50, 28.23, 97.51, 44.50
         );
  _cs.Add(   2719, 'China - 97.5'#$00b0'E to 100.5'#$00b0'E',
           97.50, 21.43, 100.50, 42.76
         );
  _cs.Add(   2727, 'China - 121.5'#$00b0'E to 124.5'#$00b0'E onshore',
           121.50, 28.22, 124.50, 53.56
         );
  _cs.Add(   2728, 'China - 124.5'#$00b0'E to 127.5'#$00b0'E onshore',
           124.50, 40.19, 127.50, 53.20
         );
  _cs.Add(   2729, 'China - 127.5'#$00b0'E to 130.5'#$00b0'E',
           127.50, 41.37, 130.50, 50.25
         );
  _cs.Add(   2730, 'China - 130.5'#$00b0'E to 133.5'#$00b0'E',
           130.50, 42.42, 133.50, 48.88
         );
  _cs.Add(   2731, 'China - east of 133.5'#$00b0'E',
           133.50, 45.85, 134.77, 48.40
         );
  _cs.Add(   2807, 'Comoros - Njazidja (Grande Comore)',
           43.16, -11.99, 43.55, -11.31
         );
  _cs.Add(   2808, 'Comoros - Nzwani',
           44.16, -12.43, 44.59, -12.02
         );
  _cs.Add(   2809, 'Comoros - Mwali',
           43.57, -12.44, 43.92, -12.19
         );
  _cs.Add(   3082, 'Colombia region 1',
           -73.00, 9.80, -71.06, 12.52
         );
  _cs.Add(   3083, 'Colombia region 2',
           -76.08, 9.39, -73.00, 11.59
         );
  _cs.Add(   3084, 'Colombia region 3',
           -77.48, 8.00, -74.39, 9.40
         );
  _cs.Add(   3085, 'Colombia region 4',
           -74.40, 5.00, -71.99, 9.40
         );
  _cs.Add(   3086, 'Colombia region 5',
           -77.92, 5.00, -74.39, 8.01
         );
  _cs.Add(   3087, 'Colombia region 6',
           -77.68, 3.00, -74.39, 5.01
         );
  _cs.Add(   3088, 'Colombia region 7',
           -79.10, -1.13, -74.00, 3.01
         );
  _cs.Add(   3089, 'Colombia region 8',
           -74.40, -4.23, -66.87, 7.10
         );
  _cs.Add(   3090, 'Colombia - 78'#$00b0'35''W to 75'#$00b0'35''W',
           -78.59, 0.03, -75.58, 10.21
         );
  _cs.Add(   3091, 'Colombia - west of 78'#$00b0'35''W',
           -79.10, 1.23, -78.58, 2.48
         );
  _cs.Add(   3228, 'China - onshore',
           73.62, 18.11, 134.77, 53.56
         );
  _cs.Add(   3229, 'Colombia - mainland',
           -79.10, -4.23, -66.87, 12.52
         );
  _cs.Add(   3230, 'Congo - onshore',
           11.11, -5.06, 18.65, 3.72
         );
  _cs.Add(   3334, 'China - Hong Kong - onshore',
           113.82, 22.19, 114.39, 22.56
         );
  _cs.Add(   3335, 'China - Hong Kong - offshore',
           113.76, 22.13, 114.51, 22.58
         );
  _cs.Add(   3466, 'China - Ordos basin',
           107.00, 35.00, 110.01, 39.00
         );
  _cs.Add(   3469, 'China - offshore - Yellow Sea',
           119.23, 31.23, 125.06, 37.40
         );
  _cs.Add(   3470, 'China - offshore - Pearl River basin',
           110.13, 18.31, 116.76, 22.89
         );
  _cs.Add(   3507, 'China - Tarim - 77.5'#$00b0'E to 88'#$00b0'E and 37'#$00b0'N to 42'#$00b0'N',
           77.45, 37.00, 88.00, 41.99
         );
  _cs.Add(   3561, 'China - offshore - Bei Bu',
           107.15, 17.81, 110.17, 21.69
         );
  _cs.Add(   3686, 'Colombia - mainland and offshore Caribbean',
           -79.10, -4.23, -66.87, 13.68
         );
  _cs.Add(   3947, 'China - 120'#$00b0'E to 126'#$00b0'E',
           120.00, 24.64, 126.00, 53.56
         );
  _cs.Add(   3948, 'China - 126'#$00b0'E to 132'#$00b0'E',
           126.00, 29.70, 132.00, 52.79
         );
  _cs.Add(   3974, 'Cocos (Keeling) Islands',
           93.41, -15.56, 100.34, -8.47
         );
  _cs.Add(   4122, 'Colombia - Arauca city',
           -70.78, 7.05, -70.71, 7.10
         );
  _cs.Add(   4128, 'Colombia - Santa Marta city',
           -74.24, 11.16, -74.13, 11.30
         );
  _cs.Add(   4130, 'Colombia - Sucre city',
           -74.74, 8.79, -74.69, 8.83
         );
  _cs.Add(   4131, 'Colombia - Tunja city',
           -73.39, 5.50, -73.30, 5.61
         );
  _cs.Add(   4132, 'Colombia - Armenia city',
           -75.73, 4.50, -75.65, 4.55
         );
  _cs.Add(   4134, 'Colombia - Barranquilla city',
           -74.87, 10.85, -74.75, 11.06
         );
  _cs.Add(   4135, 'Colombia - Bogota city',
           -74.27, 4.45, -73.98, 4.85
         );
  _cs.Add(   4136, 'Colombia - Bucaramanga city',
           -73.19, 7.03, -73.06, 7.17
         );
  _cs.Add(   4137, 'Colombia - Cali city',
           -76.61, 3.32, -76.42, 3.56
         );
  _cs.Add(   4138, 'Colombia - Cartagena city',
           -75.57, 10.27, -75.42, 10.47
         );
  _cs.Add(   4139, 'Colombia - Cucuta city',
           -72.56, 7.81, -72.46, 7.97
         );
  _cs.Add(   4140, 'Colombia - Florencia city',
           -75.63, 1.57, -75.59, 1.65
         );
  _cs.Add(   4141, 'Colombia - Ibague city',
           -75.27, 4.39, -75.11, 4.47
         );
  _cs.Add(   4142, 'Colombia - Inirida city',
           -67.94, 3.80, -67.88, 3.90
         );
  _cs.Add(   4143, 'Colombia - Leticia city',
           -69.98, -4.23, -69.92, -4.17
         );
  _cs.Add(   4144, 'Colombia - Manizales city',
           -75.54, 5.02, -75.44, 5.11
         );
  _cs.Add(   4145, 'Colombia - Medellin city',
           -75.66, 6.12, -75.50, 6.37
         );
  _cs.Add(   4146, 'Colombia - Mitu city',
           -70.25, 1.23, -70.21, 1.29
         );
  _cs.Add(   4147, 'Colombia - Mocoa city',
           -76.66, 1.12, -76.63, 1.18
         );
  _cs.Add(   4148, 'Colombia - Monteria city',
           -75.94, 8.70, -75.82, 8.81
         );
  _cs.Add(   4149, 'Colombia - Neiva city',
           -75.32, 2.87, -75.23, 2.99
         );
  _cs.Add(   4150, 'Colombia - Pasto city',
           -77.32, 1.16, -77.23, 1.26
         );
  _cs.Add(   4151, 'Colombia - Pereira city',
           -75.77, 4.78, -75.64, 4.87
         );
  _cs.Add(   4152, 'Colombia - Popayan city',
           -76.65, 2.41, -76.55, 2.49
         );
  _cs.Add(   4153, 'Colombia - Puerto Carreno city',
           -67.70, 5.98, -67.41, 6.30
         );
  _cs.Add(   4154, 'Colombia - Quibdo city',
           -76.67, 5.66, -76.63, 5.72
         );
  _cs.Add(   4155, 'Colombia - Riohacha city',
           -72.96, 11.42, -72.84, 11.58
         );
  _cs.Add(   4156, 'Colombia - San Andres city',
           -81.82, 12.43, -81.60, 12.65
         );
  _cs.Add(   4157, 'Colombia - San Jose del Guaviare city',
           -72.66, 2.54, -72.60, 2.61
         );
  _cs.Add(   4158, 'Colombia - Valledupar city',
           -73.30, 10.39, -73.19, 10.51
         );
  _cs.Add(   4159, 'Colombia - Villavicencio city',
           -73.69, 4.07, -73.56, 4.21
         );
  _cs.Add(   4160, 'Colombia - Yopal city',
           -72.43, 5.30, -72.35, 5.37
         );
  _cs.Add(   4169, 'Christmas Island - onshore',
           105.48, -10.63, 105.77, -10.36
         );
  _cs.Add(   4189, 'Cocos (Keeling) Islands - west of 96'#$00b0'E',
           93.41, -15.46, 96.01, -8.57
         );
  _cs.Add(   4190, 'Cocos (Keeling) Islands - east of 96'#$00b0'E',
           96.00, -15.56, 100.34, -8.47
         );
  _cs.Add(   1073, 'Cook Islands',
           -168.53, -25.28, -154.80, -5.85
         );
  _cs.Add(   1074, 'Costa Rica',
           -90.45, 2.15, -81.43, 11.77
         );
  _cs.Add(   1075, 'Cote d''Ivoire (Ivory Coast)',
           -8.61, 1.02, -2.48, 10.74
         );
  _cs.Add(   1076, 'Croatia',
           13.00, 41.62, 19.43, 46.54
         );
  _cs.Add(   1077, 'Cuba',
           -87.01, 18.83, -73.57, 25.51
         );
  _cs.Add(   1078, 'Cyprus',
           29.95, 32.88, 35.20, 36.21
         );
  _cs.Add(   1079, 'Czechia',
           12.09, 48.58, 18.86, 51.06
         );
  _cs.Add(   1080, 'Denmark',
           3.24, 54.36, 16.51, 58.27
         );
  _cs.Add(   1081, 'Djibouti',
           41.75, 10.94, 44.15, 12.72
         );
  _cs.Add(   1082, 'Dominica',
           -62.82, 14.48, -57.52, 16.62
         );
  _cs.Add(   1083, 'Dominican Republic',
           -73.46, 14.96, -66.82, 22.42
         );
  _cs.Add(   1084, 'East Timor',
           123.92, -10.47, 127.97, -7.97
         );
  _cs.Add(   1085, 'Ecuador',
           -95.35, -5.01, -75.21, 5.00
         );
  _cs.Add(   1086, 'Egypt',
           24.70, 21.89, 37.91, 33.82
         );
  _cs.Add(   1450, 'Cote d''Ivoire (Ivory Coast) - east of 6'#$00b0'W',
           -6.00, 4.92, -2.48, 10.46
         );
  _cs.Add(   1451, 'Cote d''Ivoire (Ivory Coast) - west of 6'#$00b0'W',
           -8.61, 4.29, -6.00, 10.74
         );
end;

procedure prepare_9;
begin
  _cs.Add(   1487, 'Cuba - onshore north of 21'#$00b0'30''N',
           -85.01, 21.38, -76.91, 23.25
         );
  _cs.Add(   1488, 'Cuba - onshore south of 21'#$00b0'30''N',
           -78.69, 19.77, -74.07, 21.50
         );
  _cs.Add(   1642, 'Egypt - east of 33'#$00b0'E onshore',
           33.00, 21.97, 36.95, 31.36
         );
  _cs.Add(   1643, 'Egypt - 29'#$00b0'E to 33'#$00b0'E',
           29.00, 21.99, 34.27, 33.82
         );
  _cs.Add(   1644, 'Egypt - west of 29'#$00b0'E; north of 28'#$00b0'11''N',
           24.70, 28.18, 29.00, 31.68
         );
  _cs.Add(   1645, 'Egypt - west of 29'#$00b0'E; south of 28'#$00b0'11''N',
           24.99, 21.99, 29.01, 28.19
         );
  _cs.Add(   2282, 'Cote d''Ivoire (Ivory Coast) - Abidjan area',
           -4.22, 5.15, -3.85, 5.54
         );
  _cs.Add(   2296, 'Cote d''Ivoire (Ivory Coast) - offshore',
           -7.55, 1.02, -3.11, 5.19
         );
  _cs.Add(   2341, 'Egypt - Gulf of Suez',
           32.34, 27.19, 34.27, 30.01
         );
  _cs.Add(   2356, 'Ecuador - Galapagos onshore',
           -91.72, -1.41, -89.19, 0.18
         );
  _cs.Add(   2531, 'Denmark - onshore Jutland and Funen',
           8.00, 54.67, 11.29, 57.80
         );
  _cs.Add(   2532, 'Denmark - onshore Zealand and Lolland',
           10.79, 54.51, 12.69, 56.79
         );
  _cs.Add(   2533, 'Denmark - onshore Bornholm',
           14.59, 54.94, 15.25, 55.38
         );
  _cs.Add(   2595, 'Egypt - Western Desert',
           24.70, 25.71, 30.00, 31.68
         );
  _cs.Add(   3147, 'Congo DR (Zaire) - Katanga',
           21.74, -13.46, 30.78, -4.99
         );
  _cs.Add(   3148, 'Congo DR (Zaire) - Kasai - SE',
           21.50, -7.31, 26.26, -5.01
         );
  _cs.Add(   3149, 'Congo DR (Zaire) - 6th parallel south',
           12.17, -7.36, 29.64, -3.29
         );
  _cs.Add(   3150, 'Congo DR (Zaire) - 11'#$00b0'E to 13'#$00b0'E onshore',
           12.17, -6.04, 13.01, -4.67
         );
  _cs.Add(   3151, 'Congo DR (Zaire) - 13'#$00b0'E to 15'#$00b0'E',
           13.00, -5.91, 15.01, -4.28
         );
  _cs.Add(   3152, 'Congo DR (Zaire) - 15'#$00b0'E to 17'#$00b0'E',
           14.99, -7.31, 17.01, -1.13
         );
  _cs.Add(   3153, 'Congo DR (Zaire) - 17'#$00b0'E to 19'#$00b0'E',
           17.00, -8.11, 19.01, 4.77
         );
  _cs.Add(   3154, 'Congo DR (Zaire) - 19'#$00b0'E to 21'#$00b0'E',
           18.99, -8.00, 21.01, 5.16
         );
  _cs.Add(   3155, 'Congo DR (Zaire) - 21'#$00b0'E to 23'#$00b0'E',
           20.99, -11.24, 23.01, 4.84
         );
  _cs.Add(   3156, 'Congo DR (Zaire) - 23'#$00b0'E to 25'#$00b0'E',
           22.99, -11.47, 25.01, 5.12
         );
  _cs.Add(   3157, 'Congo DR (Zaire) - 25'#$00b0'E to 27'#$00b0'E',
           24.99, -11.99, 27.00, 5.39
         );
  _cs.Add(   3158, 'Congo DR (Zaire) - 27'#$00b0'E to 29'#$00b0'E',
           26.99, -13.39, 29.01, 5.21
         );
  _cs.Add(   3159, 'Congo DR (Zaire) - east of 29'#$00b0'E',
           29.00, -13.46, 31.31, 4.69
         );
  _cs.Add(   3160, 'Congo DR (Zaire) - 6th parallel south 15'#$00b0'E to 17'#$00b0'E',
           15.00, -5.87, 17.00, -3.29
         );
  _cs.Add(   3161, 'Congo DR (Zaire) - 6th parallel south 17'#$00b0'E to 19'#$00b0'E',
           17.00, -5.38, 19.00, -3.40
         );
  _cs.Add(   3162, 'Congo DR (Zaire) - 6th parallel south 19'#$00b0'E to 21'#$00b0'E',
           19.00, -7.29, 21.00, -4.01
         );
  _cs.Add(   3163, 'Congo DR (Zaire) - 6th parallel south 21'#$00b0'E to 23'#$00b0'E',
           21.00, -7.31, 23.00, -5.31
         );
  _cs.Add(   3164, 'Congo DR (Zaire) - 6th parallel south 23'#$00b0'E to 25'#$00b0'E',
           23.00, -6.99, 25.00, -5.01
         );
  _cs.Add(   3165, 'Congo DR (Zaire) - 6th parallel south 25'#$00b0'E to 27'#$00b0'E',
           25.00, -7.26, 27.00, -4.23
         );
  _cs.Add(   3166, 'Congo DR (Zaire) - 6th parallel south 27'#$00b0'E to 29'#$00b0'E',
           27.00, -7.36, 29.00, -4.24
         );
  _cs.Add(   3167, 'Congo DR (Zaire) - 6th parallel south 29'#$00b0'E to 31'#$00b0'E',
           29.00, -6.04, 29.64, -4.34
         );
  _cs.Add(   3171, 'Congo DR (Zaire) - Bas Congo',
           12.17, -6.04, 16.28, -4.28
         );
  _cs.Add(   3231, 'Congo DR (Zaire) - onshore',
           12.17, -13.46, 31.31, 5.39
         );
  _cs.Add(   3232, 'Costa Rica - onshore',
           -85.97, 7.98, -82.53, 11.22
         );
  _cs.Add(   3233, 'Cote d''Ivoire (Ivory Coast) - onshore',
           -8.61, 4.29, -2.48, 10.74
         );
  _cs.Add(   3234, 'Croatia - onshore',
           13.43, 42.34, 19.43, 46.54
         );
  _cs.Add(   3235, 'Cuba - onshore',
           -85.01, 19.77, -74.07, 23.25
         );
  _cs.Add(   3236, 'Cyprus - onshore',
           32.20, 34.59, 34.65, 35.74
         );
  _cs.Add(   3237, 'Denmark - onshore',
           8.00, 54.51, 15.24, 57.80
         );
  _cs.Add(   3238, 'Djibouti - onshore',
           41.75, 10.94, 43.48, 12.72
         );
  _cs.Add(   3239, 'Dominica - onshore',
           -61.55, 15.14, -61.20, 15.69
         );
  _cs.Add(   3240, 'Dominican Republic - onshore',
           -72.01, 17.55, -68.27, 19.99
         );
  _cs.Add(   3241, 'Ecuador - mainland onshore',
           -81.03, -5.01, -75.21, 1.45
         );
  _cs.Add(   3242, 'Egypt - onshore',
           24.70, 21.97, 36.95, 31.68
         );
  _cs.Add(   3318, 'East Timor - onshore',
           124.03, -9.50, 127.36, -8.08
         );
  _cs.Add(   3471, 'Denmark - onshore west of 12'#$00b0'E',
           8.00, 54.51, 12.00, 57.80
         );
  _cs.Add(   3472, 'Denmark - onshore east of 12'#$00b0'E',
           12.00, 54.51, 15.25, 56.18
         );
  _cs.Add(   3538, 'Croatia - east of 18'#$00b0'E',
           18.00, 41.62, 19.43, 45.92
         );
  _cs.Add(   3539, 'Croatia - west of 18'#$00b0'E',
           13.00, 41.63, 18.00, 46.54
         );
  _cs.Add(   3609, 'Congo DR (Zaire) - Katanga west of 25.5'#$00b0'E',
           21.74, -11.72, 25.51, -6.32
         );
  _cs.Add(   3610, 'Congo DR (Zaire) - Katanga 24.5'#$00b0'E to 27.5'#$00b0'E',
           24.50, -12.08, 27.50, -4.99
         );
  _cs.Add(   3611, 'Congo DR (Zaire) - Katanga 26.5'#$00b0'E to 29.5'#$00b0'E',
           26.50, -13.44, 29.50, -4.99
         );
  _cs.Add(   3612, 'Congo DR (Zaire) - Katanga east of 28.5'#$00b0'E',
           28.50, -13.46, 30.78, -4.99
         );
  _cs.Add(   3613, 'Congo DR (Zaire) - south',
           11.79, -13.46, 29.81, -3.41
         );
  _cs.Add(   3614, 'Congo DR (Zaire) - Katanga - Lubumbashi area',
           26.38, -12.01, 27.75, -11.13
         );
  _cs.Add(   3617, 'Congo DR (Zaire) - south and 15'#$00b0'E to 17'#$00b0'E',
           15.00, -7.31, 17.00, -3.41
         );
  _cs.Add(   3618, 'Congo DR (Zaire) - south and 17'#$00b0'E and 19'#$00b0'E',
           17.00, -8.11, 19.00, -3.43
         );
  _cs.Add(   3620, 'Congo DR (Zaire) - south and 19'#$00b0'E to 21'#$00b0'E',
           19.00, -8.00, 21.00, -3.77
         );
  _cs.Add(   3621, 'Congo DR (Zaire) - south and 21'#$00b0'E to 23'#$00b0'E',
           21.00, -11.24, 23.01, -4.18
         );
  _cs.Add(   3622, 'Congo DR (Zaire) - south and 23'#$00b0'E to 25'#$00b0'E',
           23.00, -11.47, 25.00, -4.58
         );
  _cs.Add(   3623, 'Congo DR (Zaire) - south and 25'#$00b0'E to 27'#$00b0'E',
           25.00, -11.99, 27.00, -4.99
         );
  _cs.Add(   3624, 'Congo DR (Zaire) - south and 27'#$00b0'E to 29'#$00b0'E',
           27.00, -13.39, 29.00, -6.43
         );
  _cs.Add(   3626, 'Congo DR (Zaire) - south and 12'#$00b0'E to 18'#$00b0'E',
           12.00, -8.11, 18.01, -3.41
         );
  _cs.Add(   3627, 'Congo DR (Zaire) - south and 18'#$00b0'E to 24'#$00b0'E',
           18.00, -11.24, 24.00, -3.57
         );
  _cs.Add(   3628, 'Congo DR (Zaire) - south and 24'#$00b0'E to 30'#$00b0'E',
           24.00, -13.46, 29.81, -4.79
         );
  _cs.Add(   3631, 'Denmark - onshore Jutland west of 10'#$00b0'E',
           8.00, 54.80, 10.00, 57.64
         );
  _cs.Add(   3632, 'Denmark - onshore Jutland east of 9'#$00b0'E and Funen',
           9.00, 54.67, 11.29, 57.80
         );
  _cs.Add(   3807, 'Curacao',
           -69.55, 11.66, -68.54, 15.35
         );
  _cs.Add(   3823, 'Curacao - onshore',
           -69.22, 11.99, -68.69, 12.44
         );
  _cs.Add(   3849, 'Costa Rica - onshore and offshore east of 86'#$00b0'30''W',
           -86.50, 2.21, -81.43, 11.77
         );
  _cs.Add(   3869, 'Costa Rica - onshore north of 9'#$00b0'32''N',
           -85.97, 9.53, -82.53, 11.22
         );
  _cs.Add(   3870, 'Costa Rica - onshore south of 9'#$00b0'56''N',
           -85.74, 7.98, -82.53, 9.94
         );
  _cs.Add(   3937, 'Congo DR (Zaire) - 11'#$00b0'E to 13'#$00b0'E',
           11.79, -6.04, 13.00, -4.67
         );
  _cs.Add(   4012, 'Congo DR (Zaire) - Bas Congo east of 15'#$00b0'E',
           14.99, -5.87, 16.28, -4.42
         );
  _cs.Add(   4018, 'Congo DR (Zaire) - south and 29'#$00b0'E to 31'#$00b0'E',
           29.00, -13.46, 29.81, -12.15
         );
  _cs.Add(   4447, 'DR Congo (Zaire) - offshore',
           11.79, -6.04, 12.37, -5.79
         );
  _cs.Add(   4531, 'Costa Rica - offshore Caribbean',
           -83.60, 9.60, -81.43, 11.77
         );
  _cs.Add(   4532, 'Costa Rica - offshore Pacific',
           -90.45, 2.15, -82.92, 11.11
         );
  _cs.Add(   4575, 'Denmark - onshore Jutland, Funen, Zealand and Lolland',
           8.00, 54.51, 12.69, 57.80
         );
  _cs.Add(   4587, 'Congo DR (Zaire) - 6th parallel south 21.5'#$00b0'E to 23'#$00b0'E',
           21.50, -7.31, 23.01, -5.31
         );
  _cs.Add(   1087, 'El Salvador',
           -91.43, 9.97, -87.65, 14.44
         );
  _cs.Add(   1088, 'Equatorial Guinea',
           2.26, -4.80, 11.36, 4.13
         );
  _cs.Add(   1089, 'Eritrea',
           36.44, 12.36, 43.31, 18.10
         );
  _cs.Add(   1090, 'Estonia',
           20.37, 57.52, 28.20, 60.00
         );
  _cs.Add(   1091, 'Ethiopia',
           32.99, 3.40, 47.99, 14.89
         );
  _cs.Add(   1224, 'Eswatini',
           30.79, -27.32, 32.14, -25.72
         );
  _cs.Add(   1296, 'Europe - ED50 by country',
           -16.10, 25.71, 48.61, 84.73
         );
  _cs.Add(   1298, 'Europe - ETRF by country',
           -16.10, 32.88, 40.18, 84.73
         );
  _cs.Add(   1299, 'Europe - EVRF2000',
           -9.56, 35.95, 31.59, 71.21
         );
  _cs.Add(   1306, 'Europe - Czechoslovakia',
           12.09, 47.73, 22.56, 51.06
         );
  _cs.Add(   1321, 'Europe - Austria and former Yugoslavia onshore',
           9.53, 40.85, 23.04, 49.02
         );
  _cs.Add(   1354, 'Europe - British Isles - UK and Ireland onshore',
           -10.56, 49.81, 1.84, 60.90
         );
  _cs.Add(   1357, 'Europe - eastern and FSU',
           9.87, 35.10, -169.73, 78.00
         )         .MarkDeprecated( True );
  _cs.Add(   1513, 'Europe - 10.5'#$00b0'E to 13.5'#$00b0'E onshore by country',
           10.50, 48.97, 13.50, 54.74
         );
  _cs.Add(   1514, 'Europe - 13.5'#$00b0'E to 16.5'#$00b0'E onshore and S-42(83) by country',
           13.50, 46.54, 16.50, 54.72
         );
  _cs.Add(   1553, 'Ethiopia - east of 42'#$00b0'E',
           42.00, 4.11, 47.99, 12.85
         );
end;

procedure prepare_10;
begin
  _cs.Add(   1631, 'Europe - 18'#$00b0'W to 12'#$00b0'W and ED50 by country',
           -16.10, 48.43, -12.00, 56.57
         );
  _cs.Add(   1632, 'Europe - 12'#$00b0'W to 6'#$00b0'W and ED50 by country',
           -12.00, 36.13, -6.00, 62.41
         );
  _cs.Add(   1633, 'Europe - 6'#$00b0'W to 0'#$00b0'W and ED50 by country',
           -6.00, 35.26, 0.01, 80.49
         );
  _cs.Add(   1634, 'Europe - 0'#$00b0'E to 6'#$00b0'E and ED50 by country',
           0.00, 38.56, 6.01, 82.45
         );
  _cs.Add(   1635, 'Europe - 6'#$00b0'E to 12'#$00b0'E and ED50 by country',
           5.99, 36.53, 12.00, 84.33
         );
  _cs.Add(   1636, 'Europe - 12'#$00b0'E to 18'#$00b0'E and ED50 by country',
           12.00, 34.49, 18.00, 84.42
         );
  _cs.Add(   1637, 'Europe - 18'#$00b0'E to 24'#$00b0'E and ED50 by country',
           18.00, 33.59, 24.01, 84.54
         );
  _cs.Add(   1638, 'Europe - 24'#$00b0'E to 30'#$00b0'E and ED50 by country',
           24.00, 25.71, 30.01, 84.73
         );
  _cs.Add(   1639, 'Europe - 30'#$00b0'E to 36'#$00b0'E and ED50 by country',
           30.00, 29.19, 36.00, 84.70
         );
  _cs.Add(   1640, 'Europe - 36'#$00b0'E to 42'#$00b0'E and ED50 by country',
           36.00, 29.18, 42.00, 79.07
         );
  _cs.Add(   1641, 'Europe - 42'#$00b0'E to 48'#$00b0'E and ED50 by country',
           42.00, 36.97, 44.83, 41.60
         );
  _cs.Add(   1646, 'Europe - Estonia; Latvia; Lithuania',
           19.02, 53.89, 28.24, 60.00
         );
  _cs.Add(   1709, 'Europe - former Yugoslavia onshore west of 16.5'#$00b0'E',
           13.38, 42.95, 16.50, 46.88
         );
  _cs.Add(   1710, 'Europe - former Yugoslavia onshore 16.5'#$00b0'E to 19.5'#$00b0'E',
           16.50, 41.79, 19.51, 46.55
         );
  _cs.Add(   1711, 'Europe - former Yugoslavia onshore 19.5'#$00b0'E to 22.5'#$00b0'E',
           19.50, 40.85, 22.51, 46.19
         );
  _cs.Add(   1712, 'Europe - former Yugoslavia onshore east of 22.5'#$00b0'E',
           22.50, 41.11, 23.04, 44.70
         );
  _cs.Add(   1792, 'Europe - 12'#$00b0'E to 18'#$00b0'E onshore and S-42(58) by country',
           12.00, 45.78, 18.00, 54.89
         );
  _cs.Add(   1805, 'Europe - 6'#$00b0'E to 12'#$00b0'E and Pulkovo by country',
           9.93, 50.21, 12.00, 54.18
         )         .MarkDeprecated( True );
  _cs.Add(   2122, 'Europe - 18'#$00b0'W to 12'#$00b0'W and ETRS89 by country',
           -16.10, 34.93, -11.99, 72.44
         );
  _cs.Add(   2123, 'Europe - 12'#$00b0'W to 6'#$00b0'W and ETRS89 by country',
           -12.00, 34.91, -6.00, 74.13
         );
  _cs.Add(   2124, 'Europe - 6'#$00b0'W to 0'#$00b0'W and ETRS89 by country',
           -6.00, 35.26, 0.01, 80.49
         );
  _cs.Add(   2125, 'Europe - 0'#$00b0'E to 6'#$00b0'E and ETRS89 by country',
           0.00, 37.00, 6.01, 82.45
         );
  _cs.Add(   2126, 'Europe - 6'#$00b0'E to 12'#$00b0'E and ETRS89 by country',
           6.00, 38.76, 12.00, 84.33
         );
  _cs.Add(   2127, 'Europe - 12'#$00b0'E to 18'#$00b0'E and ETRS89 by country',
           12.00, 46.40, 18.00, 84.42
         );
  _cs.Add(   2128, 'Europe - 18'#$00b0'E to 24'#$00b0'E and ETRS89 by country',
           18.00, 58.84, 24.01, 84.54
         );
  _cs.Add(   2129, 'Europe - 24'#$00b0'E to 30'#$00b0'E and ETRS89 by country',
           24.00, 59.64, 30.01, 84.73
         );
  _cs.Add(   2130, 'Europe - 30'#$00b0'E to 36'#$00b0'E and ETRS89 by country',
           30.00, 61.73, 36.00, 84.70
         );
  _cs.Add(   2131, 'Europe - 36'#$00b0'E to 42'#$00b0'E and ETRS89 by country',
           36.00, 72.99, 38.00, 79.07
         );
  _cs.Add(   2132, 'Europe - 42'#$00b0'E to 48'#$00b0'E and ETRS89 by country',
           42.00, 37.00, 48.00, 41.65
         )         .MarkDeprecated( True );
  _cs.Add(   2342, 'Europe - common offshore',
           -16.10, 47.42, 10.86, 63.89
         );
  _cs.Add(   2343, 'Europe - British Isles and Channel Islands onshore',
           -10.56, 49.11, 1.84, 60.90
         );
  _cs.Add(   2344, 'Europe - Finland and Norway - onshore',
           4.68, 57.93, 31.59, 71.21
         );
  _cs.Add(   2370, 'Europe - former Yugoslavia onshore',
           13.38, 40.85, 23.04, 46.88
         );
  _cs.Add(   2653, 'Europe - FSU - 19.5'#$00b0'E to 22.5'#$00b0'E onshore',
           19.57, 48.24, 22.50, 59.10
         );
  _cs.Add(   2654, 'Europe - FSU - 22.5'#$00b0'E to 25.5'#$00b0'E onshore',
           22.50, 47.71, 25.50, 59.75
         );
  _cs.Add(   2655, 'Europe - FSU - 25.5'#$00b0'E to 28.5'#$00b0'E onshore',
           25.49, 45.26, 28.51, 68.93
         );
  _cs.Add(   2656, 'Europe - FSU - 28.5'#$00b0'E to 31.5'#$00b0'E onshore',
           28.50, 45.18, 31.50, 69.85
         );
  _cs.Add(   2657, 'Europe - FSU - 31.5'#$00b0'E to 34.5'#$00b0'E onshore',
           31.50, 44.32, 34.50, 70.02
         );
  _cs.Add(   2658, 'Europe - FSU - 34.5'#$00b0'E to 37.5'#$00b0'E onshore',
           34.50, 44.61, 37.50, 69.39
         );
  _cs.Add(   2659, 'Europe - FSU - 37.5'#$00b0'E to 40.5'#$00b0'E onshore',
           37.50, 43.07, 40.50, 68.80
         );
  _cs.Add(   2660, 'Europe - FSU - 40.5'#$00b0'E to 43.5'#$00b0'E onshore',
           40.50, 41.01, 43.51, 68.74
         );
  _cs.Add(   2661, 'Europe - FSU - 43.5'#$00b0'E to 46.5'#$00b0'E onshore',
           43.50, 38.84, 46.50, 80.80
         );
  _cs.Add(   2662, 'Europe - FSU - 46.5'#$00b0'E to 49.5'#$00b0'E onshore',
           46.50, 38.31, 49.50, 80.91
         );
  _cs.Add(   2804, 'Europe - Austria and FR Yugoslavia',
           9.53, 40.86, 23.03, 51.05
         )         .MarkDeprecated( True );
  _cs.Add(   2854, 'Europe - 36'#$00b0'W to 30'#$00b0'W',
           -36.00, 39.30, -30.00, 39.85
         )         .MarkDeprecated( True );
  _cs.Add(   2855, 'Europe - 30'#$00b0'W to 24'#$00b0'W',
           -30.00, 25.10, -24.00, 65.80
         )         .MarkDeprecated( True );
  _cs.Add(   2856, 'Europe - 24'#$00b0'W to 18'#$00b0'W',
           -24.00, 27.60, -18.00, 66.50
         )         .MarkDeprecated( True );
  _cs.Add(   2857, 'Europe - 18'#$00b0'W to 12'#$00b0'W',
           -18.00, 27.60, -12.00, 66.55
         )         .MarkDeprecated( True );
  _cs.Add(   2858, 'Europe - 12'#$00b0'W to 6'#$00b0'W',
           -12.00, 36.00, -6.00, 62.33
         )         .MarkDeprecated( True );
  _cs.Add(   2859, 'Europe - 6'#$00b0'W to 0'#$00b0'W',
           -6.00, 34.75, 0.00, 62.33
         )         .MarkDeprecated( True );
  _cs.Add(   2863, 'Europe - 18'#$00b0'E to 24'#$00b0'E',
           18.00, 34.80, 24.00, 75.00
         )         .MarkDeprecated( True );
  _cs.Add(   2864, 'Europe - 24'#$00b0'E to 30'#$00b0'E',
           24.00, 34.80, 30.00, 75.00
         )         .MarkDeprecated( True );
  _cs.Add(   2865, 'Europe - 30'#$00b0'E to 36'#$00b0'E',
           30.00, 34.50, 36.00, 75.00
         )         .MarkDeprecated( True );
  _cs.Add(   2866, 'Europe - 36'#$00b0'E to 42'#$00b0'E',
           36.00, 35.75, 42.00, 75.00
         )         .MarkDeprecated( True );
  _cs.Add(   2867, 'Europe - 42'#$00b0'E to 48'#$00b0'E',
           42.00, 36.95, 48.00, 75.00
         )         .MarkDeprecated( True );
  _cs.Add(   2868, 'Europe - 48'#$00b0'E to 54'#$00b0'E',
           48.00, 36.00, 54.00, 75.00
         )         .MarkDeprecated( True );
  _cs.Add(   3111, 'Europe - ED79 tfm',
           -8.95, 36.00, 31.60, 71.05
         )         .MarkDeprecated( True );
  _cs.Add(   3173, 'Europe - FSU - CS63 zone C0',
           19.57, 54.17, 23.45, 59.27
         );
  _cs.Add(   3174, 'Europe - FSU - CS63 zone C1',
           23.45, 53.89, 26.45, 59.72
         );
  _cs.Add(   3243, 'El Salvador - onshore',
           -90.11, 13.10, -87.69, 14.44
         );
  _cs.Add(   3244, 'Equatorial Guinea - onshore',
           8.37, 0.93, 11.36, 3.82
         );
  _cs.Add(   3245, 'Eritrea - onshore',
           36.44, 12.36, 43.18, 18.01
         );
  _cs.Add(   4608, 'Europe - EVRF2019',
           -9.56, 35.95, 69.15, 77.07
         );
  _cs.Add(   4609, 'Europe - ETRF EVRF2019',
           -9.56, 35.95, 31.59, 71.21
         );
  _cs.Add(   1092, 'Falkland Islands',
           -65.01, -56.25, -52.31, -47.68
         );
  _cs.Add(   1093, 'Faroe Islands',
           -13.91, 59.94, -0.48, 65.70
         );
  _cs.Add(   1094, 'Fiji - onshore',
           176.81, -20.81, -178.15, -12.42
         );
  _cs.Add(   1095, 'Finland',
           19.08, 58.84, 31.59, 70.09
         );
  _cs.Add(   1096, 'France',
           -9.86, 41.15, 10.38, 51.56
         );
  _cs.Add(   1097, 'French Guiana',
           -54.61, 2.11, -49.45, 8.88
         );
  _cs.Add(   1284, 'Europe - FSU, Czechoslovakia - onshore',
           12.09, 35.14, -169.57, 77.79
         );
  _cs.Add(   1286, 'Europe - Liechtenstein and Switzerland',
           5.96, 45.82, 10.49, 47.81
         );
  _cs.Add(   1297, 'Europe - west',
           -10.56, 34.88, 38.00, 84.73
         );
  _cs.Add(   1305, 'Europe - Ireland (Republic and Ulster) - onshore',
           -10.56, 51.39, -5.34, 55.43
         );
  _cs.Add(   1326, 'France - mainland onshore',
           -4.87, 42.33, 8.23, 51.14
         );
  _cs.Add(   1327, 'France - Corsica onshore',
           8.50, 41.31, 9.63, 43.07
         );
  _cs.Add(   1353, 'France - onshore',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1369, 'France - Alsace',
           6.84, 47.42, 8.23, 49.07
         );
  _cs.Add(   1536, 'Finland - 19.5'#$00b0'E to 22.5'#$00b0'E onshore',
           19.50, 59.76, 22.50, 69.33
         );
  _cs.Add(   1537, 'Finland - 22.5'#$00b0'E to 25.5'#$00b0'E onshore',
           22.50, 59.75, 25.50, 68.90
         );
  _cs.Add(   1538, 'Finland - 25.5'#$00b0'E to 28.5'#$00b0'E onshore',
           25.50, 59.75, 28.50, 70.09
         );
  _cs.Add(   1539, 'Finland - 28.5'#$00b0'E to 31.5'#$00b0'E',
           28.50, 60.94, 31.50, 69.81
         );
  _cs.Add(   1731, 'France - mainland north of 48.15'#$00b0'N',
           -4.87, 48.14, 8.23, 51.14
         );
  _cs.Add(   1732, 'France - mainland 45.45'#$00b0'N to 48.15'#$00b0'N',
           -4.80, 45.44, 7.63, 48.15
         );
  _cs.Add(   1733, 'France - mainland south of 45.45'#$00b0'N',
           -1.79, 42.33, 7.71, 45.46
         );
  _cs.Add(   1734, 'France - mainland 45.45'#$00b0'N to 48.15'#$00b0'N. Also all mainland.',
           -4.87, 42.33, 8.23, 51.14
         );
  _cs.Add(   1793, 'Europe - FSU onshore 18'#$00b0'E to 24'#$00b0'E and S-42 by country',
           19.57, 47.95, 24.00, 59.44
         );
  _cs.Add(   1794, 'Europe - FSU onshore 24'#$00b0'E to 30'#$00b0'E and S-42 by country',
           24.00, 45.18, 30.01, 69.47
         );
  _cs.Add(   1795, 'Europe - FSU onshore 30'#$00b0'E to 36'#$00b0'E',
           30.00, 44.32, 36.00, 70.02
         );
  _cs.Add(   1796, 'Europe - FSU onshore 36'#$00b0'E to 42'#$00b0'E',
           36.00, 41.43, 42.00, 69.23
         );
  _cs.Add(   1797, 'Europe - FSU onshore 42'#$00b0'E to 48'#$00b0'E',
           42.00, 38.84, 48.01, 80.91
         );
  _cs.Add(   1820, 'Falkland Islands - onshore west of 60'#$00b0'W',
           -61.55, -52.33, -59.99, -50.96
         );
  _cs.Add(   1821, 'Falkland Islands - onshore east of 60'#$00b0'W',
           -60.00, -52.51, -57.60, -51.13
         );
  _cs.Add(   2330, 'Europe - North Sea',
           -5.05, 51.03, 11.14, 62.01
         );
  _cs.Add(   2338, 'Europe - Portugal and Spain',
           -9.56, 35.26, 3.39, 43.82
         );
  _cs.Add(   2355, 'Falkland Islands - East Falkland Island',
           -59.98, -52.51, -57.61, -51.16
         );
  _cs.Add(   2420, 'Europe - west (DMA ED50 mean)',
           -9.56, 34.88, 31.59, 71.21
         );
  _cs.Add(   2421, 'Europe - west central (by country)',
           -4.87, 42.33, 17.17, 57.80
         );
  _cs.Add(   2423, 'Europe - FSU onshore',
           19.57, 35.14, -168.97, 81.91
         );
  _cs.Add(   2881, 'Europe - LCC & LAEA',
           -35.58, 24.60, 44.83, 84.73
         );
end;

procedure prepare_11;
begin
  _cs.Add(   3092, 'Finland - west of 19.5'#$00b0'E onshore',
           19.24, 60.08, 19.50, 60.34
         );
  _cs.Add(   3093, 'Finland - 19.5'#$00b0'E to 20.5'#$00b0'E onshore',
           19.50, 59.92, 20.50, 60.48
         );
  _cs.Add(   3094, 'Finland - 20.5'#$00b0'E to 21.5'#$00b0'E onshore',
           20.50, 59.84, 21.50, 69.33
         );
  _cs.Add(   3095, 'Finland - 21.5'#$00b0'E to 22.5'#$00b0'E onshore',
           21.50, 59.76, 22.50, 69.31
         );
  _cs.Add(   3096, 'Finland - 22.5'#$00b0'E to 23.5'#$00b0'E onshore',
           22.50, 59.75, 23.50, 68.74
         );
  _cs.Add(   3097, 'Finland - 23.5'#$00b0'E to 24.5'#$00b0'E onshore',
           23.50, 59.86, 24.50, 68.84
         );
  _cs.Add(   3098, 'Finland - 24.5'#$00b0'E to 25.5'#$00b0'E onshore',
           24.50, 59.94, 25.50, 68.90
         );
  _cs.Add(   3099, 'Finland - 25.5'#$00b0'E to 26.5'#$00b0'E onshore',
           25.50, 60.18, 26.50, 69.94
         );
  _cs.Add(   3100, 'Finland - 26.5'#$00b0'E to 27.5'#$00b0'E onshore',
           26.50, 60.36, 27.50, 70.05
         );
  _cs.Add(   3101, 'Finland - 27.5'#$00b0'E to 28.5'#$00b0'E onshore',
           27.50, 60.42, 28.50, 70.09
         );
  _cs.Add(   3102, 'Finland - 28.5'#$00b0'E to 29.5'#$00b0'E',
           28.50, 60.94, 29.50, 69.81
         );
  _cs.Add(   3103, 'Finland - 29.5'#$00b0'E to 30.5'#$00b0'E',
           29.50, 61.43, 30.50, 67.98
         );
  _cs.Add(   3104, 'Finland - east of 30.5'#$00b0'E',
           30.50, 62.08, 31.59, 64.27
         );
  _cs.Add(   3105, 'French Guiana - coastal area',
           -54.45, 3.43, -51.61, 5.81
         );
  _cs.Add(   3144, 'French Guiana - east of 54'#$00b0'W',
           -54.00, 2.17, -49.45, 8.88
         );
  _cs.Add(   3146, 'French Guiana - onshore',
           -54.61, 2.11, -51.61, 5.81
         );
  _cs.Add(   3175, 'Europe - FSU - CS63 zone C2',
           26.45, 55.15, 28.24, 59.61
         );
  _cs.Add(   3195, 'Fiji - Viti Levu',
           177.19, -18.32, 178.75, -17.25
         );
  _cs.Add(   3247, 'Falkland Islands - onshore',
           -61.55, -52.51, -57.60, -50.96
         );
  _cs.Add(   3248, 'Faroe Islands - onshore',
           -7.49, 61.33, -6.33, 62.41
         );
  _cs.Add(   3333, 'Finland - onshore',
           19.24, 59.75, 31.59, 70.09
         );
  _cs.Add(   3385, 'Finland - east of 31.5'#$00b0'E',
           31.50, 62.83, 31.59, 63.00
         );
  _cs.Add(   3398, 'Fiji - main islands',
           176.81, -19.22, -179.77, -16.10
         );
  _cs.Add(   3399, 'Fiji - main islands - west of 180'#$00b0'',
           176.81, -19.22, 180.00, -16.10
         );
  _cs.Add(   3400, 'Fiji - main islands - east of 180'#$00b0'',
           -180.00, -17.04, -179.77, -16.10
         );
  _cs.Add(   3401, 'Fiji - Vanua Levu and Taveuni',
           178.42, -17.07, -179.77, -16.10
         );
  _cs.Add(   3545, 'France - mainland south of 43'#$00b0'N and Corsica',
           -1.06, 41.31, 9.63, 43.07
         );
  _cs.Add(   3546, 'France - mainland south of 44'#$00b0'N',
           -1.79, 42.33, 7.65, 44.01
         );
  _cs.Add(   3547, 'France - mainland - 43'#$00b0'N to 45'#$00b0'N',
           -1.79, 42.92, 7.71, 45.00
         );
  _cs.Add(   3548, 'France - mainland - 44'#$00b0'N to 46'#$00b0'N',
           -1.46, 44.00, 7.71, 46.00
         );
  _cs.Add(   3549, 'France - mainland - 45'#$00b0'N to 47'#$00b0'N',
           -2.21, 45.00, 7.16, 47.00
         );
  _cs.Add(   3550, 'France - mainland - 46'#$00b0'N to 48'#$00b0'N',
           -4.77, 46.00, 7.63, 48.00
         );
  _cs.Add(   3551, 'France - mainland - 47'#$00b0'N to 49'#$00b0'N',
           -4.87, 47.00, 8.23, 49.00
         );
  _cs.Add(   3552, 'France - mainland - 48'#$00b0'N to 50'#$00b0'N',
           -4.87, 48.00, 8.23, 50.00
         );
  _cs.Add(   3553, 'France - mainland north of 49'#$00b0'N',
           -2.03, 49.00, 8.08, 51.14
         );
  _cs.Add(   3574, 'Europe - onshore - eastern - S-42(58)',
           9.92, 39.63, 31.41, 54.89
         );
  _cs.Add(   3595, 'Finland - west of 19.5'#$00b0'E onshore nominal',
           19.24, 60.08, 19.50, 60.34
         );
  _cs.Add(   3596, 'Finland - 19.5'#$00b0'E to 20.5'#$00b0'E onshore nominal',
           19.50, 59.92, 20.50, 60.48
         );
  _cs.Add(   3597, 'Finland - 20.5'#$00b0'E to 21.5'#$00b0'E onshore nominal',
           20.50, 59.84, 21.50, 69.33
         );
  _cs.Add(   3598, 'Finland - 21.5'#$00b0'E to 22.5'#$00b0'E onshore nominal',
           21.50, 59.76, 22.50, 69.31
         );
  _cs.Add(   3599, 'Finland - 22.5'#$00b0'E to 23.5'#$00b0'E onshore nominal',
           22.50, 59.75, 23.50, 68.74
         );
  _cs.Add(   3600, 'Finland - 23.5'#$00b0'E to 24.5'#$00b0'E onshore nominal',
           23.50, 59.86, 24.50, 68.84
         );
  _cs.Add(   3601, 'Finland - 24.5'#$00b0'E to 25.5'#$00b0'E onshore nominal',
           24.50, 59.94, 25.50, 68.90
         );
  _cs.Add(   3602, 'Finland - 25.5'#$00b0'E to 26.5'#$00b0'E onshore nominal',
           25.50, 60.18, 26.50, 69.94
         );
  _cs.Add(   3603, 'Finland - 26.5'#$00b0'E to 27.5'#$00b0'E onshore nominal',
           26.50, 60.36, 27.50, 70.05
         );
  _cs.Add(   3604, 'Finland - 27.5'#$00b0'E to 28.5'#$00b0'E onshore nominal',
           27.50, 60.42, 28.50, 70.09
         );
  _cs.Add(   3605, 'Finland - 28.5'#$00b0'E to 29.5'#$00b0'E nominal',
           28.50, 60.94, 29.50, 69.81
         );
  _cs.Add(   3606, 'Finland - 29.5'#$00b0'E to 30.5'#$00b0'E nominal',
           29.50, 61.43, 30.50, 67.98
         );
  _cs.Add(   3607, 'Finland - east of 30.5'#$00b0'E nominal',
           30.50, 62.08, 31.59, 64.27
         );
  _cs.Add(   3694, 'France - onshore - mainland and Corsica',
           -4.87, 41.31, 9.63, 51.14
         );
  _cs.Add(   3744, 'Europe - Ireland (Republic and Ulster) - on- and offshore',
           -10.60, 51.33, -5.33, 55.40
         )         .MarkDeprecated( True );
  _cs.Add(   3765, 'French Guiana - coastal area west of 54'#$00b0'W',
           -54.45, 4.84, -54.00, 5.69
         );
  _cs.Add(   3766, 'French Guiana - coastal area east of 54'#$00b0'W',
           -54.00, 3.43, -51.61, 5.81
         );
  _cs.Add(   3886, 'Finland - onshore west of 19.5'#$00b0'E',
           19.30, 60.00, 19.50, 60.42
         )         .MarkDeprecated( True );
  _cs.Add(   3897, 'France - offshore Mediterranean',
           3.04, 41.15, 10.38, 43.74
         );
  _cs.Add(   3899, 'Europe - South Permian basin',
           -1.67, 50.50, 22.00, 56.00
         );
  _cs.Add(   3900, 'Europe - onshore - eastern - S-42(83)',
           9.92, 41.24, 28.68, 54.74
         );
  _cs.Add(   3999, 'Fiji',
           172.76, -25.09, -176.27, -9.78
         );
  _cs.Add(   4172, 'Falkland Islands - offshore 63'#$00b0'W to 57'#$00b0'W',
           -63.01, -56.25, -56.99, -47.68
         );
  _cs.Add(   4455, 'Europe - Upper Austria, Salzburg and Bohemia',
           12.07, 46.93, 16.83, 51.06
         );
  _cs.Add(   4456, 'Europe - Lower Austria and Moravia',
           14.41, 47.42, 18.86, 50.45
         );
  _cs.Add(   4522, 'Finland - mainland south of 66'#$00b0'N',
           20.95, 59.75, 31.59, 66.73
         );
  _cs.Add(   4566, 'Europe - offshore North Sea - Germany and Netherlands east of 5'#$00b0'E',
           3.34, 53.49, 8.88, 55.92
         );
  _cs.Add(   4613, 'Europe - Lyon-Turin',
           4.65, 44.87, 7.88, 45.89
         );
  _cs.Add(   1098, 'French Polynesia',
           -158.13, -31.24, -131.97, -4.52
         );
  _cs.Add(   1099, 'French Southern Territories',
           37.55, -53.24, 81.83, -10.65
         );
  _cs.Add(   1100, 'Gabon',
           7.03, -6.37, 14.52, 2.32
         );
  _cs.Add(   1101, 'Gambia',
           -20.19, 13.05, -13.79, 13.83
         );
  _cs.Add(   1102, 'Georgia',
           38.97, 41.04, 46.72, 43.59
         );
  _cs.Add(   1103, 'Germany',
           3.34, 47.27, 15.04, 55.92
         );
  _cs.Add(   1295, 'Germany - DHDN',
           5.87, 47.27, 15.03, 55.06
         )         .MarkDeprecated( True );
  _cs.Add(   1343, 'Germany - East Germany all states',
           9.92, 50.20, 15.04, 54.74
         );
  _cs.Add(   1512, 'Germany - East Germany - west of 10.5'#$00b0'E',
           9.92, 50.35, 10.50, 51.56
         );
  _cs.Add(   1624, 'Germany - West Germany - west of 7.5'#$00b0'E',
           5.86, 49.11, 7.50, 53.81
         );
  _cs.Add(   1626, 'Germany - West Germany - 10.5'#$00b0'E to 13.5'#$00b0'E',
           10.50, 47.39, 13.51, 54.59
         );
  _cs.Add(   1627, 'Germany - West Germany - east of 13.5'#$00b0'E',
           13.50, 48.51, 13.84, 48.98
         );
  _cs.Add(   1696, 'Gabon - north of equator and west of 12'#$00b0'E onshore',
           9.25, 0.00, 12.00, 2.32
         );
  _cs.Add(   1697, 'Gabon - west of 12'#$00b0'E',
           7.03, -6.37, 12.01, 2.32
         );
  _cs.Add(   2326, 'Germany - West Germany all states',
           5.87, 47.27, 13.84, 55.09
         );
  _cs.Add(   2544, 'Germany - Thuringen',
           9.92, 50.20, 12.56, 51.64
         );
  _cs.Add(   2545, 'Germany - Saxony',
           11.89, 50.20, 15.04, 51.66
         );
  _cs.Add(   2810, 'French Polynesia - Marquesas Islands - Nuku Hiva',
           -140.31, -9.01, -139.96, -8.72
         );
  _cs.Add(   2811, 'French Polynesia - Society Islands - Moorea and Tahiti',
           -150.00, -17.93, -149.11, -17.41
         );
  _cs.Add(   2812, 'French Polynesia - Society Islands - Bora Bora, Huahine, Raiatea, Tahaa',
           -151.91, -16.96, -150.89, -16.17
         );
  _cs.Add(   2816, 'French Southern Territories - Kerguelen onshore',
           68.69, -49.78, 70.62, -48.60
         );
  _cs.Add(   2861, 'Germany - 6'#$00b0'E to 12'#$00b0'E',
           6.00, 47.27, 12.00, 55.47
         );
  _cs.Add(   2862, 'Germany - east of 12'#$00b0'E',
           11.57, 47.46, 15.04, 55.03
         );
  _cs.Add(   2879, 'Germany - offshore North Sea',
           3.34, 53.60, 8.88, 55.92
         );
  _cs.Add(   2898, 'Germany - Berlin',
           13.09, 52.33, 13.76, 52.65
         );
  _cs.Add(   3120, 'French Polynesia - west of 150'#$00b0'W',
           -158.13, -26.70, -150.00, -12.50
         );
  _cs.Add(   3121, 'French Polynesia - 150'#$00b0'W to 144'#$00b0'W',
           -150.00, -31.20, -144.00, -7.29
         );
  _cs.Add(   3122, 'French Polynesia - 144'#$00b0'W to 138'#$00b0'W',
           -144.00, -31.24, -138.00, -4.52
         );
  _cs.Add(   3123, 'French Polynesia - east of 138'#$00b0'W',
           -138.00, -26.58, -131.97, -5.52
         );
  _cs.Add(   3124, 'French Polynesia - Society Islands - Tahiti',
           -149.70, -17.93, -149.09, -17.44
         );
  _cs.Add(   3125, 'French Polynesia - Society Islands - Moorea',
           -150.00, -17.63, -149.73, -17.41
         );
  _cs.Add(   3126, 'French Polynesia - Society Islands - Maupiti',
           -152.39, -16.57, -152.14, -16.34
         );
  _cs.Add(   3127, 'French Polynesia - Marquesas Islands - Ua Huka',
           -139.66, -9.00, -139.44, -8.81
         );
  _cs.Add(   3128, 'French Polynesia - Marquesas Islands - Ua Pou',
           -140.21, -9.57, -139.95, -9.27
         );
  _cs.Add(   3129, 'French Polynesia - Marquesas Islands - Nuku Hiva, Ua Huka and Ua Pou',
           -140.31, -9.57, -139.44, -8.72
         );
  _cs.Add(   3130, 'French Polynesia - Marquesas Islands - Hiva Oa and Tahuata',
           -139.23, -10.08, -138.75, -9.64
         );
end;

procedure prepare_12;
begin
  _cs.Add(   3131, 'French Polynesia - Marquesas Islands - Hiva Oa',
           -139.23, -9.89, -138.75, -9.64
         );
  _cs.Add(   3132, 'French Polynesia - Marquesas Islands - Tahuata',
           -139.19, -10.08, -138.98, -9.86
         );
  _cs.Add(   3133, 'French Polynesia - Marquesas Islands - Fatu Hiva',
           -138.75, -10.60, -138.54, -10.36
         );
  _cs.Add(   3134, 'French Polynesia - Society Islands - main islands',
           -152.39, -17.93, -149.09, -16.17
         );
  _cs.Add(   3135, 'French Polynesia - Society Islands - Huahine',
           -151.11, -16.87, -150.89, -16.63
         );
  _cs.Add(   3136, 'French Polynesia - Society Islands - Raiatea',
           -151.55, -16.96, -151.30, -16.68
         );
  _cs.Add(   3137, 'French Polynesia - Society Islands - Bora Bora',
           -151.86, -16.62, -151.61, -16.39
         );
  _cs.Add(   3138, 'French Polynesia - Society Islands - Tahaa',
           -151.63, -16.72, -151.36, -16.50
         );
  _cs.Add(   3145, 'French Guiana - west of 54'#$00b0'W',
           -54.61, 2.11, -54.00, 5.69
         );
  _cs.Add(   3249, 'Gabon - onshore',
           8.65, -3.98, 14.52, 2.32
         );
  _cs.Add(   3250, 'Gambia - onshore',
           -16.88, 13.05, -13.79, 13.83
         );
  _cs.Add(   3251, 'Georgia - onshore',
           39.99, 41.04, 46.72, 43.59
         );
  _cs.Add(   3339, 'Germany - onshore',
           5.86, 47.27, 15.04, 55.09
         );
  _cs.Add(   3392, 'Germany - Thuringen - west of 10.5'#$00b0'E',
           9.92, 50.35, 10.50, 51.56
         );
  _cs.Add(   3393, 'Germany - Thuringen - east of 10.5'#$00b0'E',
           10.50, 50.20, 12.56, 51.64
         );
  _cs.Add(   3394, 'Germany - Saxony - east of 13.5'#$00b0'E',
           13.50, 50.62, 15.04, 51.58
         );
  _cs.Add(   3395, 'Germany - Saxony - west of 13.5'#$00b0'E',
           11.89, 50.20, 13.51, 51.66
         );
  _cs.Add(   3396, 'Germany - Mecklenburg-Vorpommern and Sachsen-Anhalt',
           10.54, 50.94, 14.43, 53.05
         )         .MarkDeprecated( True );
  _cs.Add(   3575, 'Germany - East Germany - west of 12'#$00b0'E',
           9.92, 50.20, 12.00, 54.23
         );
  _cs.Add(   3643, 'Germany - Schleswig-Holstein',
           7.80, 53.35, 11.35, 55.06
         )         .MarkDeprecated( True );
  _cs.Add(   3644, 'Germany - Schleswig-Holstein - east of 10.5'#$00b0'E',
           10.49, 53.36, 11.40, 54.59
         );
  _cs.Add(   3659, 'Germany - Schleswig-Holstein - west of 10.5'#$00b0'E',
           7.80, 53.37, 10.50, 55.09
         );
  _cs.Add(   3703, 'Germany - Lower Saxony west of 7.5'#$00b0'E',
           6.58, 52.23, 7.50, 53.81
         );
  _cs.Add(   3705, 'Germany - Lower Saxony east of 10.5'#$00b0'E',
           10.50, 51.55, 11.59, 53.38
         );
  _cs.Add(   3707, 'Germany - Lower Saxony 7.5'#$00b0'E to 10.5'#$00b0'E',
           7.50, 51.28, 10.51, 53.95
         );
  _cs.Add(   3731, 'Georgia - offshore',
           38.97, 41.54, 41.71, 43.33
         );
  _cs.Add(   3879, 'Germany - onshore east of 12'#$00b0'E',
           12.00, 47.46, 15.04, 54.74
         );
  _cs.Add(   3892, 'Germany - offshore North Sea west of 4.5'#$00b0'E',
           3.34, 55.24, 4.50, 55.92
         );
  _cs.Add(   3901, 'Germany - onshore west of 6'#$00b0'E',
           5.87, 50.97, 6.00, 51.83
         );
  _cs.Add(   3904, 'Germany - onshore between 6'#$00b0'E and 12'#$00b0'E',
           6.00, 47.27, 12.00, 55.09
         );
  _cs.Add(   3916, 'French Southern Territories - Amsterdam onshore',
           77.45, -37.93, 77.67, -37.74
         );
  _cs.Add(   3918, 'French Southern Territories - Kerguelen',
           62.96, -53.24, 75.66, -45.11
         );
  _cs.Add(   3919, 'French Southern Territories - Crozet',
           45.37, -49.82, 57.16, -42.61
         );
  _cs.Add(   3920, 'French Southern Territories - Crozet onshore',
           50.09, -46.53, 52.36, -45.87
         );
  _cs.Add(   3921, 'French Southern Territories - Amsterdam & St Paul',
           73.24, -42.08, 81.83, -34.47
         );
  _cs.Add(   3922, 'French Southern Territories - St Paul onshore',
           77.44, -38.79, 77.63, -38.63
         );
  _cs.Add(   3923, 'French Southern Territories - Tromelin',
           52.45, -18.69, 57.18, -12.59
         );
  _cs.Add(   3924, 'French Southern Territories - Tromelin onshore',
           54.46, -15.96, 54.60, -15.82
         );
  _cs.Add(   3925, 'French Southern Territories - Glorieuses',
           45.76, -12.80, 48.49, -10.65
         );
  _cs.Add(   3926, 'French Southern Territories - Glorieuses onshore',
           47.22, -11.63, 47.36, -11.50
         );
  _cs.Add(   3927, 'French Southern Territories - Juan de Nova',
           40.94, -19.21, 43.46, -15.35
         );
  _cs.Add(   3930, 'French Southern Territories - Juan de Nova onshore',
           42.67, -17.13, 42.82, -17.00
         );
  _cs.Add(   3932, 'French Southern Territories - Bassas da India',
           37.55, -23.21, 41.59, -19.07
         );
  _cs.Add(   3933, 'French Southern Territories - Bassas da India onshore',
           39.57, -21.58, 39.82, -21.37
         );
  _cs.Add(   3934, 'French Southern Territories - Europa',
           37.98, -25.70, 41.82, -20.91
         );
  _cs.Add(   3936, 'French Southern Territories - Europa onshore',
           40.26, -22.46, 40.46, -22.27
         );
  _cs.Add(   3993, 'Germany - onshore 7.5'#$00b0'E to 10.5'#$00b0'E',
           7.50, 47.27, 10.51, 55.09
         );
  _cs.Add(   3996, 'Germany - onshore 10.5'#$00b0'E to 13.5'#$00b0'E',
           10.50, 47.39, 13.51, 54.74
         );
  _cs.Add(   3997, 'Germany - East Germany - 10.5'#$00b0'E to 13.5'#$00b0'E onshore',
           10.50, 50.20, 13.51, 54.74
         );
  _cs.Add(   3998, 'Germany - onshore east of 13.5'#$00b0'E',
           13.50, 48.51, 15.04, 54.72
         );
  _cs.Add(   4000, 'Germany - onshore 10.5'#$00b0'E to 12'#$00b0'E',
           10.50, 47.39, 12.00, 54.59
         );
  _cs.Add(   4001, 'Germany - onshore 12'#$00b0'E to 13.5'#$00b0'E',
           12.00, 47.46, 13.51, 54.74
         );
  _cs.Add(   4003, 'Germany - East Germany - east of 13.5'#$00b0'E onshore',
           13.50, 50.62, 15.04, 54.72
         );
  _cs.Add(   4245, 'French Southern Territories - Crozet west of 48'#$00b0'E',
           45.37, -49.38, 48.01, -43.12
         );
  _cs.Add(   4246, 'French Southern and Antarctic Territories',
           37.98, -67.13, 142.00, -20.91
         );
  _cs.Add(   4247, 'French Southern Territories - Crozet 48'#$00b0'E to 54'#$00b0'E',
           48.00, -49.82, 54.01, -42.61
         );
  _cs.Add(   4248, 'French Southern Territories - Crozet east of 54'#$00b0'E',
           54.00, -49.61, 57.16, -43.30
         );
  _cs.Add(   4249, 'French Southern Territories - 60'#$00b0'E to 66'#$00b0'E',
           62.96, -53.03, 66.00, -45.73
         );
  _cs.Add(   4250, 'French Southern Territories - 66'#$00b0'E to 72'#$00b0'E',
           66.00, -53.24, 72.01, -45.11
         );
  _cs.Add(   4251, 'French Southern Territories - 72'#$00b0'E to 78'#$00b0'E',
           72.00, -51.19, 78.01, -34.47
         );
  _cs.Add(   4252, 'French Southern Territories - east of 78'#$00b0'E',
           78.00, -42.04, 81.83, -34.50
         );
  _cs.Add(   4490, 'Germany - Hamburg',
           8.33, 53.36, 10.41, 53.99
         );
  _cs.Add(   4580, 'Germany - Baden-Wurttemberg',
           7.51, 47.54, 10.50, 49.80
         );
  _cs.Add(   4584, 'Germany - Saarland',
           6.35, 49.11, 7.41, 49.64
         );
  _cs.Add(   1104, 'Ghana',
           -3.79, 1.40, 2.10, 11.16
         );
  _cs.Add(   1105, 'Gibraltar',
           -5.42, 36.00, -4.89, 36.16
         );
  _cs.Add(   1106, 'Greece',
           18.26, 33.26, 30.23, 41.75
         );
  _cs.Add(   1107, 'Greenland',
           -75.00, 56.38, 7.99, 87.02
         );
  _cs.Add(   1108, 'Grenada',
           -63.28, 11.36, -60.82, 13.40
         );
  _cs.Add(   1109, 'Guadeloupe',
           -62.81, 15.06, -57.54, 18.53
         );
  _cs.Add(   1110, 'Guam',
           141.19, 10.95, 148.18, 15.91
         );
  _cs.Add(   1111, 'Guatemala',
           -94.57, 10.60, -88.16, 17.83
         );
  _cs.Add(   1112, 'Guinea',
           -18.26, 7.19, -7.65, 12.68
         );
  _cs.Add(   1113, 'Guinea-Bissau',
           -19.80, 8.64, -13.64, 12.69
         );
  _cs.Add(   1114, 'Guyana',
           -61.39, 1.18, -55.77, 10.70
         );
  _cs.Add(   1115, 'Haiti',
           -75.73, 14.73, -71.62, 20.72
         );
  _cs.Add(   1116, 'Heard Island and McDonald Islands',
           62.92, -59.02, 83.57, -49.50
         );
  _cs.Add(   1117, 'Honduras',
           -89.36, 12.98, -79.87, 19.59
         );
  _cs.Add(   1119, 'Hungary',
           16.11, 45.74, 22.90, 48.58
         );
  _cs.Add(   1120, 'Iceland',
           -30.87, 59.96, -5.55, 69.59
         );
  _cs.Add(   1250, 'Holy See (Vatican City State)',
           12.44, 41.90, 12.46, 41.91
         );
  _cs.Add(   1468, 'Guinea - west of 12'#$00b0'W',
           -15.13, 9.01, -12.00, 12.68
         );
  _cs.Add(   1469, 'Guinea - east of 12'#$00b0'W',
           -12.00, 7.19, -7.65, 12.51
         );
  _cs.Add(   1505, 'Ghana - offshore',
           -3.79, 1.40, 2.10, 6.06
         );
  _cs.Add(   1551, 'Grenada and southern Grenadines - onshore',
           -61.84, 11.94, -61.35, 12.57
         );
  _cs.Add(   1625, 'Germany - West-Germany - 7.5'#$00b0'E to 10.5'#$00b0'E',
           7.50, 47.27, 10.51, 55.09
         );
  _cs.Add(   1628, 'Germany - west of 4.5'#$00b0'E',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   2120, 'Guatemala - north of 15'#$00b0'51''30"N',
           -91.86, 15.85, -88.34, 17.83
         );
  _cs.Add(   2121, 'Guatemala - south of 15'#$00b0'51''30"N',
           -92.29, 13.69, -88.19, 15.86
         );
  _cs.Add(   2386, 'Greenland - Hayes Peninsula',
           -73.29, 75.86, -60.98, 79.20
         );
  _cs.Add(   2407, 'Greenland - south of 72'#$00b0'N',
           -55.00, 59.75, -40.00, 72.00
         )         .MarkDeprecated( True );
  _cs.Add(   2541, 'Germany - West Germany N',
           6.58, 52.33, 11.59, 55.09
         );
  _cs.Add(   2542, 'Germany - West Germany C',
           5.86, 50.33, 12.03, 52.34
         );
  _cs.Add(   2543, 'Germany - West Germany S',
           6.11, 47.27, 13.84, 50.34
         );
  _cs.Add(   2556, 'Greenland - north of 81'#$00b0'N',
           -65.06, 81.00, -11.81, 83.67
         );
  _cs.Add(   2557, 'Greenland - east - 78'#$00b0'N to 81'#$00b0'N',
           -44.00, 78.00, -14.33, 81.00
         );
  _cs.Add(   2558, 'Greenland - east - 75'#$00b0'N to 78'#$00b0'N',
           -44.00, 75.00, -17.12, 78.00
         );
  _cs.Add(   2559, 'Greenland - east - 72'#$00b0'N to 75'#$00b0'N',
           -38.00, 72.00, -17.21, 75.00
         );
  _cs.Add(   2560, 'Greenland - east - 69'#$00b0'N to 72'#$00b0'N',
           -38.00, 69.00, -21.32, 72.00
         );
  _cs.Add(   3118, 'Grenada - main island - onshore',
           -61.84, 11.94, -61.54, 12.29
         );
end;

procedure prepare_13;
begin
  _cs.Add(   3119, 'Greenland - onshore',
           -73.29, 59.74, -11.81, 83.67
         );
  _cs.Add(   3252, 'Ghana - onshore',
           -3.25, 4.67, 1.23, 11.16
         );
  _cs.Add(   3253, 'Gibraltar - onshore',
           -5.42, 36.07, -5.27, 36.16
         );
  _cs.Add(   3254, 'Greece - onshore',
           19.57, 34.88, 28.30, 41.75
         );
  _cs.Add(   3255, 'Guam - onshore',
           144.58, 13.18, 145.01, 13.70
         );
  _cs.Add(   3256, 'Guatemala - onshore',
           -92.29, 13.69, -88.19, 17.83
         );
  _cs.Add(   3257, 'Guinea - onshore',
           -15.13, 7.19, -7.65, 12.68
         );
  _cs.Add(   3258, 'Guinea-Bissau - onshore',
           -16.77, 10.87, -13.64, 12.69
         );
  _cs.Add(   3259, 'Guyana - onshore',
           -61.39, 1.18, -56.47, 8.58
         );
  _cs.Add(   3260, 'Haiti - onshore',
           -74.52, 17.97, -71.62, 20.15
         );
  _cs.Add(   3261, 'Honduras - onshore',
           -89.36, 12.98, -83.08, 16.49
         );
  _cs.Add(   3362, 'Greenland - west coast',
           -73.29, 59.74, -42.52, 79.00
         );
  _cs.Add(   3363, 'Greenland - west coast - 63'#$00b0'N to 66'#$00b0'N',
           -53.70, 63.00, -49.17, 66.00
         );
  _cs.Add(   3364, 'Greenland - west coast - 66'#$00b0'N to 69'#$00b0'N',
           -54.09, 66.00, -49.40, 69.00
         );
  _cs.Add(   3365, 'Greenland - west coast - 69'#$00b0'N to 72'#$00b0'N',
           -56.06, 69.00, -49.11, 72.00
         );
  _cs.Add(   3366, 'Greenland - west coast - 72'#$00b0'N to 75'#$00b0'N',
           -58.21, 72.00, -52.02, 75.00
         );
  _cs.Add(   3367, 'Greenland - west coast - 75'#$00b0'N to 78'#$00b0'N',
           -72.79, 75.00, -56.31, 78.00
         );
  _cs.Add(   3368, 'Greenland - west coast - 78'#$00b0'N to 79'#$00b0'N',
           -73.29, 78.00, -65.24, 79.00
         );
  _cs.Add(   3369, 'Greenland - east coast - 68'#$00b0'N to 69'#$00b0'N',
           -26.99, 68.66, -25.14, 69.00
         );
  _cs.Add(   3370, 'Greenland - east coast - 69'#$00b0'N to 72'#$00b0'N',
           -29.69, 69.00, -21.32, 72.00
         );
  _cs.Add(   3371, 'Greenland - east coast - 72'#$00b0'N to 75'#$00b0'N',
           -29.15, 72.00, -19.89, 74.58
         );
  _cs.Add(   3449, 'Greenland - west of 72'#$00b0'W',
           -75.00, 74.52, -72.00, 79.04
         );
  _cs.Add(   3450, 'Greenland - 72'#$00b0'W to 66'#$00b0'W',
           -72.00, 73.24, -66.00, 80.90
         );
  _cs.Add(   3451, 'Greenland - 66'#$00b0'W to 60'#$00b0'W',
           -66.00, 68.92, -60.00, 82.22
         );
  _cs.Add(   3452, 'Greenland - 60'#$00b0'W to 54'#$00b0'W',
           -60.00, 58.91, -54.00, 84.00
         );
  _cs.Add(   3453, 'Greenland - 54'#$00b0'W to 48'#$00b0'W',
           -54.00, 56.90, -48.00, 84.00
         );
  _cs.Add(   3454, 'Greenland - 48'#$00b0'W to 42'#$00b0'W',
           -48.00, 56.38, -42.00, 84.00
         );
  _cs.Add(   3455, 'Greenland - 42'#$00b0'W to 36'#$00b0'W',
           -42.00, 56.56, -36.00, 84.00
         );
  _cs.Add(   3456, 'Greenland - 36'#$00b0'W to 30'#$00b0'W',
           -36.00, 60.16, -30.00, 84.00
         );
  _cs.Add(   3457, 'Greenland - 30'#$00b0'W to 24'#$00b0'W',
           -30.00, 64.96, -24.00, 84.00
         );
  _cs.Add(   3458, 'Greenland - 24'#$00b0'W to 18'#$00b0'W',
           -24.00, 67.70, -18.00, 84.00
         );
  _cs.Add(   3459, 'Greenland - 18'#$00b0'W to 12'#$00b0'W',
           -18.00, 68.67, -12.00, 84.00
         );
  _cs.Add(   3460, 'Greenland - 12'#$00b0'W to 6'#$00b0'W',
           -12.00, 72.43, -6.00, 84.00
         );
  _cs.Add(   3846, 'Greenland - southwest coast south of 63'#$00b0'N',
           -50.72, 59.74, -42.52, 63.00
         );
  _cs.Add(   3848, 'Honduras - onshore north of 14'#$00b0'38''30"N',
           -89.23, 14.64, -83.08, 16.49
         );
  _cs.Add(   3850, 'Honduras - onshore south of 14'#$00b0'38''30"N',
           -89.36, 12.98, -84.40, 14.65
         );
  _cs.Add(   4173, 'Heard Island and McDonald Islands - west of 66'#$00b0'E',
           62.92, -57.50, 66.00, -53.43
         );
  _cs.Add(   4181, 'Heard Island and McDonald Islands - 66'#$00b0'E to 72'#$00b0'E',
           66.00, -58.96, 72.01, -51.18
         );
  _cs.Add(   4184, 'Heard Island and McDonald Islands - 72'#$00b0'E to 78'#$00b0'E',
           72.00, -59.02, 78.01, -49.50
         );
  _cs.Add(   4185, 'Heard Island and McDonald Islands - east of 78'#$00b0'E',
           78.00, -58.47, 83.57, -50.65
         );
  _cs.Add(   4454, 'Greenland - 58'#$00b0'N to 85'#$00b0'N',
           -75.00, 58.00, -6.99, 85.01
         );
  _cs.Add(   4461, 'Greenland - 59'#$00b0'N to 84'#$00b0'N',
           -75.00, 59.00, -10.00, 84.01
         );
  _cs.Add(   1121, 'India',
           65.60, 3.87, 97.42, 35.51
         );
  _cs.Add(   1122, 'Indonesia',
           92.01, -13.95, 141.46, 7.79
         );
  _cs.Add(   1285, 'Indonesia - Bali, Java and western Sumatra onshore',
           95.16, -8.91, 115.77, 5.97
         );
  _cs.Add(   1287, 'Indonesia - Banga & Belitung Islands',
           105.07, -3.30, 108.35, -1.44
         );
  _cs.Add(   1584, 'Indonesia - Java and Java Sea - west of 108'#$00b0'E',
           105.06, -7.79, 108.00, -4.07
         );
  _cs.Add(   1585, 'Indonesia - Java and Java Sea - east of 114'#$00b0'E',
           114.00, -8.91, 117.01, -5.33
         );
  _cs.Add(   1586, 'Indonesia - Java and Java Sea - 108'#$00b0'E to 114'#$00b0'E',
           108.00, -8.67, 114.00, -4.27
         );
  _cs.Add(   1649, 'Indonesia - 96'#$00b0'E to 102'#$00b0'E, N hemisphere',
           96.00, 0.00, 102.00, 7.49
         );
  _cs.Add(   1650, 'Indonesia - 96'#$00b0'E to 102'#$00b0'E, S hemisphere',
           96.00, -8.86, 102.00, 0.00
         );
  _cs.Add(   1651, 'Indonesia - 102'#$00b0'E to 108'#$00b0'E, N hemisphere',
           102.00, 0.00, 108.00, 6.94
         );
  _cs.Add(   1652, 'Indonesia - 102'#$00b0'E to 108'#$00b0'E, S hemisphere',
           102.00, -10.73, 108.01, 0.00
         );
  _cs.Add(   1653, 'Indonesia - 108'#$00b0'E to 114'#$00b0'E, N hemisphere',
           108.00, 0.00, 114.00, 7.37
         );
  _cs.Add(   1654, 'Indonesia - 108'#$00b0'E to 114'#$00b0'E, S hemisphere',
           108.00, -12.07, 114.00, 0.00
         );
  _cs.Add(   1655, 'Indonesia - 114'#$00b0'E to 120'#$00b0'E, N hemisphere',
           114.00, 0.00, 120.00, 4.37
         );
  _cs.Add(   1656, 'Indonesia - 114'#$00b0'E to 120'#$00b0'E, S hemisphere',
           114.00, -13.06, 120.01, 0.00
         );
  _cs.Add(   1657, 'Indonesia - 120'#$00b0'E to 126'#$00b0'E, N hemisphere',
           120.00, 0.00, 126.00, 5.48
         );
  _cs.Add(   1658, 'Indonesia - 120'#$00b0'E to 126'#$00b0'E, S hemisphere',
           120.00, -13.95, 126.01, 0.01
         );
  _cs.Add(   1659, 'Indonesia - 126'#$00b0'E to 132'#$00b0'E, N hemisphere',
           126.00, 0.00, 132.00, 6.68
         );
  _cs.Add(   1660, 'Indonesia - 126'#$00b0'E to 132'#$00b0'E, S hemisphere',
           126.00, -9.45, 132.00, 0.00
         );
  _cs.Add(   1661, 'Indonesia - 132'#$00b0'E to 138'#$00b0'E, N hemisphere',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1662, 'Indonesia - 132'#$00b0'E to 138'#$00b0'E, S hemisphere',
           132.00, -10.06, 138.01, 0.00
         );
  _cs.Add(   1663, 'Indonesia - east of 138'#$00b0'E, S hemisphere',
           138.00, -10.84, 141.46, 0.00
         );
  _cs.Add(   1672, 'India - onshore 15'#$00b0'N to 21'#$00b0'N',
           70.14, 15.00, 87.15, 21.01
         );
  _cs.Add(   1673, 'India - mainland south of 15'#$00b0'N',
           73.94, 8.02, 80.40, 15.01
         );
  _cs.Add(   1676, 'India - north of 28'#$00b0'N',
           70.35, 28.00, 81.64, 35.51
         );
  _cs.Add(   1677, 'India - onshore 21'#$00b0'N to 28'#$00b0'N and west of 82'#$00b0'E',
           68.13, 21.00, 82.01, 28.01
         );
  _cs.Add(   1678, 'India - onshore north of 21'#$00b0'N and east of 82'#$00b0'E',
           82.00, 21.00, 97.42, 29.47
         );
  _cs.Add(   1679, 'India - onshore west of 72'#$00b0'E',
           68.13, 20.64, 72.01, 28.22
         );
  _cs.Add(   1680, 'India - mainland 72'#$00b0'E to 78'#$00b0'E',
           72.00, 8.02, 78.01, 35.51
         );
  _cs.Add(   1681, 'India - onshore 78'#$00b0'E to 84'#$00b0'E',
           78.00, 8.29, 84.01, 35.50
         );
  _cs.Add(   1682, 'India - onshore 84'#$00b0'E to 90'#$00b0'E',
           84.00, 18.18, 90.01, 28.14
         );
  _cs.Add(   1683, 'India - mainland 90'#$00b0'E to 96'#$00b0'E',
           90.00, 21.94, 96.01, 29.42
         );
  _cs.Add(   1684, 'India - east of 96'#$00b0'E',
           96.00, 27.10, 97.42, 29.47
         );
  _cs.Add(   2577, 'Indonesia - Java Sea - offshore northwest Java',
           105.77, -6.89, 110.01, -4.07
         );
  _cs.Add(   2588, 'Indonesia - Bali Sea west',
           112.80, -8.46, 117.01, -6.80
         );
  _cs.Add(   2851, 'Iceland - onshore west of 24'#$00b0'W',
           -24.66, 64.71, -24.00, 65.86
         );
  _cs.Add(   2852, 'Iceland - onshore 24'#$00b0'W to 18'#$00b0'W',
           -24.00, 63.34, -18.00, 66.52
         );
  _cs.Add(   2853, 'Iceland - onshore east of 18'#$00b0'W',
           -18.00, 63.45, -13.38, 66.59
         );
  _cs.Add(   2561, 'Greenland - east - 66'#$00b0'N to 69'#$00b0'N',
           -42.00, 66.00, -25.14, 69.00
         );
  _cs.Add(   2562, 'Greenland - east - 63'#$00b0'N to 66'#$00b0'N',
           -46.00, 63.00, -36.00, 66.00
         );
  _cs.Add(   2563, 'Greenland - west - 78'#$00b0'N to 81'#$00b0'N',
           -73.29, 78.00, -44.00, 81.00
         );
  _cs.Add(   2564, 'Greenland - west - 75'#$00b0'N to 78'#$00b0'N',
           -72.79, 75.00, -44.00, 78.00
         );
  _cs.Add(   2565, 'Greenland - west - 72'#$00b0'N to 75'#$00b0'N',
           -58.21, 72.00, -38.00, 75.00
         );
  _cs.Add(   2566, 'Greenland - west - 69'#$00b0'N to 72'#$00b0'N',
           -56.06, 69.00, -38.00, 72.00
         );
  _cs.Add(   2567, 'Greenland - west - 66'#$00b0'N to 69'#$00b0'N',
           -54.09, 66.00, -42.00, 69.00
         );
  _cs.Add(   2568, 'Greenland - west - 63'#$00b0'N to 66'#$00b0'N',
           -53.70, 63.00, -46.00, 66.00
         );
  _cs.Add(   2569, 'Greenland - south of 63'#$00b0'N',
           -50.72, 59.74, -41.33, 63.00
         );
  _cs.Add(   2570, 'Greenland - Scoresbysund area',
           -29.69, 68.66, -19.89, 74.58
         );
  _cs.Add(   2571, 'Greenland - Ammassalik area',
           -38.86, 65.52, -36.81, 65.91
         );
  _cs.Add(   2572, 'Greenland - southwest coast east of 48'#$00b0'W',
           -48.00, 59.74, -42.52, 62.05
         );
  _cs.Add(   2573, 'Greenland - southwest coast 54'#$00b0'W to 48'#$00b0'W',
           -54.00, 60.63, -48.00, 73.05
         );
  _cs.Add(   2828, 'Guadeloupe - St Martin and St Barthelemy - onshore',
           -63.21, 17.82, -62.73, 18.17
         );
  _cs.Add(   2829, 'Guadeloupe - Grande-Terre and surrounding islands - onshore',
           -61.85, 15.80, -60.97, 16.55
         );
  _cs.Add(   2860, 'Germany - west of 6'#$00b0'E',
           3.34, 50.97, 6.00, 55.92
         );
  _cs.Add(   2890, 'Guadeloupe - St Martin - onshore',
           -63.21, 18.01, -62.96, 18.17
         );
  _cs.Add(   2891, 'Guadeloupe - St Barthelemy - onshore',
           -62.92, 17.82, -62.73, 17.98
         );
  _cs.Add(   2892, 'Guadeloupe - Grande-Terre and Basse-Terre - onshore',
           -61.85, 15.88, -61.15, 16.55
         );
  _cs.Add(   2893, 'Guadeloupe - La Desirade - onshore',
           -61.13, 16.26, -60.97, 16.38
         );
end;

procedure prepare_14;
begin
  _cs.Add(   2894, 'Guadeloupe - Marie-Galante - onshore',
           -61.39, 15.80, -61.13, 16.05
         );
  _cs.Add(   2895, 'Guadeloupe - Les Saintes - onshore',
           -61.68, 15.80, -61.52, 15.94
         );
  _cs.Add(   3262, 'Iceland - onshore',
           -24.66, 63.34, -13.38, 66.59
         );
  _cs.Add(   3341, 'India - mainland',
           68.13, 8.02, 97.42, 35.51
         );
  _cs.Add(   3510, 'Indonesia - 96'#$00b0'E to 99'#$00b0'E onshore',
           96.00, -1.81, 99.00, 5.42
         );
  _cs.Add(   3511, 'Indonesia - 99'#$00b0'E to 102'#$00b0'E onshore',
           99.00, -3.57, 102.00, 3.71
         );
  _cs.Add(   3512, 'Indonesia - 102'#$00b0'E to 105'#$00b0'E onshore',
           102.00, -5.99, 105.00, 1.68
         );
  _cs.Add(   3513, 'Indonesia - 105'#$00b0'E to 108'#$00b0'E onshore',
           105.00, -7.79, 108.00, 4.11
         );
  _cs.Add(   3514, 'Indonesia - 108'#$00b0'E to 111'#$00b0'E onshore',
           108.00, -8.31, 111.00, 4.25
         );
  _cs.Add(   3515, 'Indonesia - 111'#$00b0'E to 114'#$00b0'E onshore',
           111.00, -8.67, 114.00, 1.59
         );
  _cs.Add(   3516, 'Indonesia - 114'#$00b0'E to 117'#$00b0'E onshore',
           114.00, -9.15, 117.01, 4.37
         );
  _cs.Add(   3517, 'Indonesia - 117'#$00b0'E to 120'#$00b0'E onshore',
           117.00, -10.15, 120.00, 4.36
         );
  _cs.Add(   3518, 'Indonesia - 120'#$00b0'E to 123'#$00b0'E onshore',
           120.00, -10.98, 123.00, 1.40
         );
  _cs.Add(   3519, 'Indonesia - 123'#$00b0'E to 126'#$00b0'E onshore',
           123.00, -10.92, 126.00, 3.84
         );
  _cs.Add(   3520, 'Indonesia - 126'#$00b0'E to 129'#$00b0'E onshore',
           126.00, -8.32, 129.00, 4.59
         );
  _cs.Add(   3521, 'Indonesia - 129'#$00b0'E to 132'#$00b0'E onshore',
           129.00, -8.41, 132.00, 0.10
         );
  _cs.Add(   3522, 'Indonesia - 132'#$00b0'E to 135'#$00b0'E onshore',
           132.00, -7.30, 135.00, -0.29
         );
  _cs.Add(   3523, 'Indonesia - 135'#$00b0'E to 138'#$00b0'E onshore',
           135.00, -8.49, 138.00, -0.58
         );
  _cs.Add(   3975, 'Indonesia - east of 138'#$00b0'E onshore',
           138.00, -9.19, 141.01, -1.49
         );
  _cs.Add(   3978, 'Indonesia - 96'#$00b0'E to 102'#$00b0'E, N hemisphere onshore',
           96.00, 0.00, 102.00, 5.42
         );
  _cs.Add(   3979, 'Indonesia - 102'#$00b0'E to 108'#$00b0'E, N hemisphere onshore',
           102.00, 0.00, 108.00, 4.11
         );
  _cs.Add(   3980, 'Indonesia - 108'#$00b0'E to 114'#$00b0'E, N hemisphere onshore',
           108.00, 0.00, 114.00, 4.25
         );
  _cs.Add(   3981, 'Indonesia - 114'#$00b0'E to 120'#$00b0'E, N hemisphere onshore',
           114.00, 0.00, 120.00, 4.37
         );
  _cs.Add(   3983, 'Indonesia - 120'#$00b0'E to 126'#$00b0'E, N hemisphere onshore',
           120.00, 0.00, 125.71, 3.84
         );
  _cs.Add(   3984, 'Indonesia - 126'#$00b0'E to 132'#$00b0'E, N hemisphere onshore',
           126.55, 0.00, 131.00, 4.59
         );
  _cs.Add(   3985, 'Indonesia - 96'#$00b0'E to 102'#$00b0'E, S hemisphere onshore',
           98.24, -3.57, 102.00, 0.00
         );
  _cs.Add(   3986, 'Indonesia - 102'#$00b0'E to 108'#$00b0'E, S hemisphere onshore',
           102.00, -7.79, 108.00, 0.00
         );
  _cs.Add(   3987, 'Indonesia - 108'#$00b0'E to 114'#$00b0'E, S hemisphere onshore',
           108.00, -8.67, 114.00, 0.00
         );
  _cs.Add(   3988, 'Indonesia - 114'#$00b0'E to 120'#$00b0'E, S hemisphere onshore',
           114.00, -10.15, 120.00, 0.00
         );
  _cs.Add(   3989, 'Indonesia - 120'#$00b0'E to 126'#$00b0'E, S hemisphere onshore',
           120.00, -10.98, 126.00, 0.00
         );
  _cs.Add(   3990, 'Indonesia - 126'#$00b0'E to 132'#$00b0'E, S hemisphere onshore',
           126.00, -8.41, 132.00, 0.00
         );
  _cs.Add(   3991, 'Indonesia - 132'#$00b0'E to 138'#$00b0'E, S hemisphere onshore',
           132.00, -8.49, 138.00, -0.29
         );
  _cs.Add(   4392, 'India - northeast',
           89.69, 21.94, 97.42, 29.47
         );
  _cs.Add(   4394, 'India - Andhra Pradesh and Telangana',
           76.75, 12.61, 84.81, 19.92
         );
  _cs.Add(   4395, 'India - Arunachal Pradesh',
           91.55, 26.65, 97.42, 29.47
         );
  _cs.Add(   4396, 'India - Assam',
           89.69, 24.13, 96.03, 27.98
         );
  _cs.Add(   4397, 'India - Bihar',
           83.31, 24.28, 88.30, 27.86
         );
  _cs.Add(   4398, 'India - Chhattisgarh',
           80.23, 17.78, 84.39, 24.11
         );
  _cs.Add(   4399, 'India - Goa',
           73.61, 14.86, 74.35, 15.80
         );
  _cs.Add(   4400, 'India - Gujarat',
           68.13, 20.05, 74.48, 24.71
         );
  _cs.Add(   4401, 'India - Haryana',
           74.46, 27.65, 77.60, 30.94
         );
  _cs.Add(   4402, 'India - Himachal Pradesh',
           75.57, 30.38, 79.00, 33.26
         );
  _cs.Add(   4403, 'India - Jammu and Kashmir',
           73.76, 32.27, 79.57, 35.51
         );
  _cs.Add(   4404, 'India - Jharkhand',
           83.32, 21.96, 87.98, 25.35
         );
  _cs.Add(   4405, 'India - Karnataka',
           74.00, 11.57, 78.58, 18.46
         );
  _cs.Add(   4406, 'India - Kerala',
           74.81, 8.25, 77.40, 12.80
         );
  _cs.Add(   4407, 'India - Madhya Pradesh',
           74.03, 21.07, 82.81, 26.88
         );
  _cs.Add(   4408, 'India - Maharashtra',
           72.60, 15.60, 80.90, 22.04
         );
  _cs.Add(   4409, 'India - Manipur',
           92.97, 23.84, 94.76, 25.70
         );
  _cs.Add(   4410, 'India - Meghalaya',
           89.82, 25.03, 92.81, 26.12
         );
  _cs.Add(   4411, 'India - Mizoram',
           92.25, 21.94, 93.45, 24.53
         );
  _cs.Add(   4412, 'India - Nagaland',
           93.33, 25.20, 95.25, 27.05
         );
  _cs.Add(   4413, 'India - Odisha',
           81.38, 17.80, 87.50, 22.57
         );
  _cs.Add(   4414, 'India - Punjab',
           73.87, 29.54, 76.94, 32.58
         );
  _cs.Add(   4415, 'India - Rajasthan',
           69.48, 23.06, 78.27, 30.20
         );
  _cs.Add(   4416, 'India - Sikkim',
           88.01, 27.08, 88.92, 28.14
         );
  _cs.Add(   4417, 'India - Tamil Nadu',
           76.22, 8.02, 80.40, 13.59
         );
  _cs.Add(   4418, 'India - Tripura',
           91.15, 22.94, 92.34, 24.54
         );
  _cs.Add(   4419, 'India - Uttar Pradesh',
           77.08, 23.87, 84.64, 30.42
         );
  _cs.Add(   4420, 'India - Uttarakhand',
           77.56, 28.71, 81.02, 31.48
         );
  _cs.Add(   4421, 'India - West Bengal',
           85.82, 21.49, 89.88, 27.23
         );
  _cs.Add(   4422, 'India - Delhi',
           76.83, 28.40, 77.34, 28.89
         );
  _cs.Add(   4423, 'India - Andaman and Nicobar Islands',
           92.15, 6.70, 94.33, 13.73
         );
  _cs.Add(   4424, 'India - Lakshadweep',
           72.04, 8.21, 73.76, 11.76
         );
  _cs.Add(   1123, 'Iran',
           44.03, 23.34, 63.34, 39.78
         );
  _cs.Add(   1124, 'Iraq',
           38.79, 29.06, 48.75, 37.39
         );
  _cs.Add(   1125, 'Ireland',
           -16.10, 48.18, -5.24, 56.57
         );
  _cs.Add(   1126, 'Israel',
           32.99, 29.45, 35.69, 33.53
         );
  _cs.Add(   1127, 'Italy',
           5.93, 34.76, 18.99, 47.10
         );
  _cs.Add(   1128, 'Jamaica',
           -80.60, 14.08, -74.51, 19.36
         );
  _cs.Add(   1129, 'Japan',
           122.38, 17.09, 157.65, 46.05
         );
  _cs.Add(   1300, 'Iran - FD58',
           47.13, 26.21, 53.61, 33.22
         );
  _cs.Add(   1316, 'Indonesia - Sulawesi SW',
           118.71, -6.54, 120.78, -1.88
         );
  _cs.Add(   1328, 'Indonesia - Kalimantan - Mahakam delta',
           116.72, -1.24, 117.99, 0.00
         );
  _cs.Add(   1338, 'Iran - Taheri refinery',
           52.50, 27.39, 52.71, 27.61
         );
  _cs.Add(   1355, 'Indonesia - Sumatra',
           95.16, -5.99, 106.13, 5.97
         );
  _cs.Add(   1359, 'Indonesia - Kalimantan SE',
           114.55, -4.24, 117.99, 0.00
         );
  _cs.Add(   1360, 'Indonesia - Kalimantan E',
           114.55, -4.24, 119.06, 4.29
         );
  _cs.Add(   1464, 'Iran - west of 48'#$00b0'E',
           44.03, 30.99, 48.00, 39.78
         );
  _cs.Add(   1465, 'Iran - 48'#$00b0'E to 54'#$00b0'E',
           48.00, 25.47, 54.00, 39.71
         );
  _cs.Add(   1466, 'Iran - 54'#$00b0'E to 60'#$00b0'E',
           54.00, 25.32, 60.00, 38.29
         );
  _cs.Add(   1467, 'Iran - east of 60'#$00b0'E onshore',
           60.00, 25.02, 63.34, 37.06
         );
  _cs.Add(   1647, 'Indonesia - west of 96'#$00b0'E, N hemisphere',
           92.01, 0.00, 96.00, 7.79
         );
  _cs.Add(   1648, 'Indonesia - west of 96'#$00b0'E, S hemisphere',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1718, 'Italy - west of 12'#$00b0'E',
           5.94, 36.53, 12.00, 47.04
         );
  _cs.Add(   1719, 'Italy - east of 12'#$00b0'E',
           12.00, 34.76, 18.99, 47.10
         );
  _cs.Add(   2339, 'Italy - Sardinia onshore',
           8.08, 38.82, 9.89, 41.31
         );
  _cs.Add(   2340, 'Italy - Sicily onshore',
           12.36, 36.59, 15.71, 38.35
         );
  _cs.Add(   2354, 'Indonesia - Kalimantan',
           108.79, -4.24, 119.06, 4.37
         );
  _cs.Add(   2362, 'Iran - Kangan district',
           51.80, 27.30, 53.01, 28.20
         );
  _cs.Add(   2372, 'Italy - mainland',
           6.62, 37.86, 18.58, 47.10
         );
  _cs.Add(   2524, 'Japan - 31'#$00b0'20''N to 32'#$00b0'N; 130'#$00b0'E to 131'#$00b0'E',
           130.10, 31.33, 131.00, 32.00
         );
  _cs.Add(   2525, 'Japan - 31'#$00b0'20''N to 32'#$00b0'N; 131'#$00b0'E to 132'#$00b0'E',
           131.00, 31.33, 131.55, 32.00
         );
  _cs.Add(   2589, 'Indonesia - West Papua - Tangguh',
           131.89, -2.94, 133.82, -1.97
         );
  _cs.Add(   2770, 'Indonesia - northeast Kalimantan',
           116.96, -0.07, 119.06, 4.29
         );
  _cs.Add(   2781, 'Iran - Kharg Island',
           50.22, 29.16, 50.42, 29.39
         );
  _cs.Add(   2782, 'Iran - Lavan Island and Balal field',
           52.49, 26.21, 53.43, 26.87
         );
  _cs.Add(   2783, 'Iran - South Pars blocks 2 and 3',
           52.07, 26.58, 52.28, 26.71
         );
  _cs.Add(   2803, 'Isle of Man - onshore',
           -4.87, 54.02, -4.27, 54.44
         );
  _cs.Add(   2806, 'Iran - northern Gulf coast',
           47.75, 29.00, 50.83, 31.67
         )         .MarkDeprecated( True );
end;

procedure prepare_15;
begin
  _cs.Add(   2869, 'Jan Mayen - onshore',
           -9.17, 70.75, -7.87, 71.24
         );
  _cs.Add(   2882, 'Italy - Adriatic - North Ancona',
           12.22, 43.62, 13.96, 45.73
         );
  _cs.Add(   2883, 'Italy - Adriatic - South Ancona / North Gargano',
           13.61, 41.95, 16.14, 44.04
         );
  _cs.Add(   2884, 'Italy - Adriatic - South Gargano',
           15.95, 40.72, 18.63, 42.28
         );
  _cs.Add(   2885, 'Italy - Otranto channel',
           17.95, 39.77, 18.99, 41.03
         );
  _cs.Add(   2886, 'Italy - north Ionian Sea',
           16.55, 37.67, 18.93, 40.47
         );
  _cs.Add(   2887, 'Italy - Sicily Strait east of 13'#$00b0'E',
           13.00, 35.22, 15.16, 37.48
         );
  _cs.Add(   2888, 'Italy - Sicily Strait west of 13'#$00b0'E',
           10.68, 35.28, 13.01, 38.45
         );
  _cs.Add(   2951, 'Japan - 120'#$00b0'E to 126'#$00b0'E onshore',
           123.62, 23.98, 125.51, 24.94
         );
  _cs.Add(   2952, 'Japan - 126'#$00b0'E to 132'#$00b0'E onshore',
           126.63, 24.40, 132.00, 34.90
         );
  _cs.Add(   2953, 'Japan - 132'#$00b0'E to 138'#$00b0'E onshore',
           132.00, 20.37, 138.00, 37.58
         );
  _cs.Add(   2954, 'Japan - 138'#$00b0'E to 144'#$00b0'E onshore',
           138.00, 24.67, 144.00, 45.54
         );
  _cs.Add(   2955, 'Japan - 144'#$00b0'E to 150'#$00b0'E onshore',
           144.00, 42.84, 145.87, 44.40
         );
  _cs.Add(   2961, 'Ireland - Corrib and Errigal',
           -12.50, 53.75, -9.49, 55.76
         );
  _cs.Add(   3140, 'Iran - South Pars block 11',
           52.22, 26.46, 52.41, 26.64
         );
  _cs.Add(   3141, 'Iran - Tombak LNG plant',
           52.09, 27.63, 52.26, 27.81
         );
  _cs.Add(   3336, 'Iran - onshore',
           44.03, 25.02, 63.34, 39.78
         );
  _cs.Add(   3342, 'Jamaica - onshore',
           -78.43, 17.64, -76.17, 18.58
         );
  _cs.Add(   3343, 'Italy - including San Marino and Vatican',
           5.93, 34.76, 18.99, 47.10
         );
  _cs.Add(   3387, 'Iraq - west of 42'#$00b0'E',
           38.79, 31.14, 42.00, 36.75
         );
  _cs.Add(   3388, 'Iraq - 42'#$00b0'E to 48'#$00b0'E',
           42.00, 29.06, 48.00, 37.39
         );
  _cs.Add(   3389, 'Iraq - east of 48'#$00b0'E onshore',
           48.00, 29.87, 48.61, 31.00
         );
  _cs.Add(   3397, 'Iraq - Basra area',
           46.46, 29.87, 48.61, 31.09
         );
  _cs.Add(   3478, 'Jamaica - west of 78'#$00b0'W',
           -80.60, 14.16, -77.99, 19.36
         );
  _cs.Add(   3479, 'Jamaica - east of 78'#$00b0'W',
           -78.00, 14.08, -74.51, 19.20
         );
  _cs.Add(   3625, 'Iraq - onshore',
           38.79, 29.06, 48.61, 37.39
         );
  _cs.Add(   3666, 'Indonesia - Java, Java Sea and western Sumatra',
           95.16, -8.91, 117.01, 5.97
         );
  _cs.Add(   3695, 'Iraq - 31.4'#$00b0'N to 33'#$00b0'N, 43.9'#$00b0'E to 46.1'#$00b0'E (map 16)',
           43.92, 31.36, 46.08, 32.99
         );
  _cs.Add(   3701, 'Iraq - 34.6'#$00b0'N to 36.2'#$00b0'N, west of 42.8'#$00b0'E (map 5)',
           41.09, 34.55, 42.82, 36.22
         );
  _cs.Add(   3702, 'Iraq - SE',
           43.98, 29.06, 48.61, 32.51
         );
  _cs.Add(   3704, 'Iraq - 31.4'#$00b0'N to 33'#$00b0'N, east of 46.1'#$00b0'E (map 17)',
           46.05, 31.33, 47.87, 32.99
         );
  _cs.Add(   3706, 'Iraq - 29.7 to 31.4'#$00b0'N, 42'#$00b0'E to 43.9'#$00b0'E (map 19)',
           42.00, 29.75, 43.97, 31.37
         );
  _cs.Add(   3708, 'Iraq - 29.7'#$00b0'N to 31.4'#$00b0'N, 43.9'#$00b0'E to 46.1'#$00b0'E (map 20)',
           43.94, 29.73, 46.06, 31.37
         );
  _cs.Add(   3709, 'Iraq - 31.4'#$00b0'N to 33'#$00b0'N, 42'#$00b0'E to 43.9'#$00b0'E (map 15)',
           42.00, 31.32, 43.95, 32.99
         );
  _cs.Add(   3710, 'Iraq - 29.7'#$00b0'N to 31.4'#$00b0'N, 46.1'#$00b0'E to 48'#$00b0'E (map 21)',
           46.03, 29.72, 48.00, 31.37
         );
  _cs.Add(   3711, 'Iraq - south of 29.7'#$00b0'N, west of 46.1'#$00b0'E (map 24)',
           43.99, 29.09, 46.04, 29.75
         );
  _cs.Add(   3712, 'Iraq - south of 29.7'#$00b0'N, east of 46.1'#$00b0'E (map 25)',
           46.02, 29.06, 47.02, 29.74
         );
  _cs.Add(   3714, 'Iraq - north of 36.2'#$00b0'N, west of 42'#$00b0'E (map 1)',
           41.27, 36.19, 42.00, 36.75
         );
  _cs.Add(   3715, 'Iraq - 34.6'#$00b0'N to 36.2'#$00b0'N, 42.8'#$00b0'E to 45'#$00b0'E (map 6)',
           42.77, 34.59, 45.00, 36.24
         );
  _cs.Add(   3717, 'Iraq - north of 36.2'#$00b0'N, 42'#$00b0'E to 43.9'#$00b0'E (map 2)',
           42.00, 36.19, 43.89, 37.39
         );
  _cs.Add(   3718, 'Iraq - 34.6'#$00b0'N to 36.2'#$00b0'N, east of 45'#$00b0'E (map 7)',
           45.00, 34.60, 46.35, 36.24
         );
  _cs.Add(   3719, 'Iraq - north of 36.2'#$00b0'N, east of 43.9'#$00b0'E (map 3)',
           43.87, 36.22, 45.33, 37.33
         );
  _cs.Add(   3722, 'Iraq - 33'#$00b0'N to 34.6'#$00b0'N, west of 40.1'#$00b0'E (map 8)',
           38.79, 32.98, 40.09, 33.99
         );
  _cs.Add(   3723, 'Iraq - 33'#$00b0'N to 34.6'#$00b0'N, 40.1'#$00b0'E to 42'#$00b0'E (map 9)',
           40.07, 32.95, 42.00, 34.60
         );
  _cs.Add(   3724, 'Iraq - 33'#$00b0'N to 34.6'#$00b0'N, 42'#$00b0'E to 43.9'#$00b0'E (map 10)',
           42.00, 32.95, 43.93, 34.61
         );
  _cs.Add(   3725, 'Iraq - 33'#$00b0'N to 34.6'#$00b0'N, 43.9'#$00b0'E to 46.1'#$00b0'E (map 11)',
           43.90, 32.98, 46.20, 34.62
         );
  _cs.Add(   3728, 'Iraq - 31.4'#$00b0'N to 33'#$00b0'N, 40.1'#$00b0'E to 42'#$00b0'E (map 14)',
           40.05, 31.32, 42.00, 32.99
         );
  _cs.Add(   3729, 'Iraq - 31.4'#$00b0'N to 33'#$00b0'N, west of 40.1'#$00b0'E (map 13)',
           38.92, 32.00, 40.08, 32.99
         );
  _cs.Add(   3736, 'Italy - mainland and Sicily',
           6.62, 36.59, 18.58, 47.10
         );
  _cs.Add(   3767, 'Ireland - onshore',
           -10.56, 51.39, -5.93, 55.43
         );
  _cs.Add(   3956, 'Iraq - east of 48'#$00b0'E',
           48.00, 29.60, 48.75, 31.00
         );
  _cs.Add(   3959, 'Japan - 120'#$00b0'E to 126'#$00b0'E',
           122.38, 21.10, 126.00, 29.71
         );
  _cs.Add(   3960, 'Japan - 126'#$00b0'E to 132'#$00b0'E',
           126.00, 21.12, 132.00, 38.63
         );
  _cs.Add(   3961, 'Japan - 132'#$00b0'E to 138'#$00b0'E',
           132.00, 17.09, 138.00, 43.55
         );
  _cs.Add(   3962, 'Japan - 138'#$00b0'E to 144'#$00b0'E',
           138.00, 17.63, 144.00, 46.05
         );
  _cs.Add(   3963, 'Japan - 144'#$00b0'E to 150'#$00b0'E',
           144.00, 23.03, 147.86, 45.65
         );
  _cs.Add(   3976, 'Indonesia - west of 96'#$00b0'E onshore',
           95.16, 2.55, 96.00, 5.97
         );
  _cs.Add(   4005, 'Indonesia - Kalimantan W - coastal',
           108.79, 0.06, 109.78, 2.13
         );
  _cs.Add(   4020, 'Indonesia - onshore',
           95.16, -10.98, 141.01, 5.97
         );
  _cs.Add(   4035, 'Italy - Emilia-Romagna',
           9.19, 43.73, 12.76, 45.14
         );
  _cs.Add(   4186, 'Italy - 12'#$00b0'E to 18'#$00b0'E',
           12.00, 34.79, 18.00, 47.10
         );
  _cs.Add(   4187, 'Italy - east of 18'#$00b0'E',
           17.99, 34.76, 18.99, 41.64
         );
  _cs.Add(   4619, 'Italy - 6'#$00b0'22''E to 18'#$00b0'40''E and north of 35'#$00b0'16''N; San Marino, Vatican City State',
           6.36, 35.26, 18.67, 47.10
         );
  _cs.Add(   4659, 'Italy - mainland west of 16'#$00b0'E',
           6.62, 39.65, 16.52, 47.09
         );
  _cs.Add(   1872, 'Japan - Minamitori-shima (Marcus Island) - onshore',
           153.91, 24.22, 154.05, 24.35
         );
  _cs.Add(   2425, 'Japan - 45'#$00b0'20''N to 46'#$00b0'N; 141'#$00b0'E to 142'#$00b0'E',
           141.56, 45.33, 142.00, 45.54
         );
  _cs.Add(   2426, 'Japan - 45'#$00b0'20''N to 46'#$00b0'N; 142'#$00b0'E to 143'#$00b0'E',
           142.00, 45.33, 142.27, 45.54
         );
  _cs.Add(   2427, 'Japan - 44'#$00b0'40''N to 45'#$00b0'20''N; 141'#$00b0'E to 142'#$00b0'E',
           141.50, 44.66, 142.00, 45.34
         );
  _cs.Add(   2428, 'Japan - 44'#$00b0'40''N to 45'#$00b0'20''N; 142'#$00b0'E to 143'#$00b0'E',
           142.00, 44.66, 142.97, 45.34
         );
  _cs.Add(   2429, 'Japan - 44'#$00b0'N to 44'#$00b0'40''N; 141'#$00b0'E to 142'#$00b0'E',
           141.58, 43.99, 142.00, 44.67
         );
  _cs.Add(   2430, 'Japan - 44'#$00b0'N to 44'#$00b0'40''N; 142'#$00b0'E to 143'#$00b0'E',
           142.00, 43.99, 143.00, 44.67
         );
  _cs.Add(   2431, 'Japan - 44'#$00b0'N to 44'#$00b0'40''N; 143'#$00b0'E to 144'#$00b0'E',
           143.00, 43.99, 144.00, 44.65
         );
  _cs.Add(   2432, 'Japan - 44'#$00b0'N to 44'#$00b0'40''N; 144'#$00b0'E to 145'#$00b0'E',
           144.00, 43.99, 145.00, 44.19
         );
  _cs.Add(   2433, 'Japan - 43'#$00b0'20''N to 44'#$00b0'N; 141'#$00b0'E to 142'#$00b0'E',
           141.26, 43.33, 142.00, 44.00
         );
  _cs.Add(   2434, 'Japan - 43'#$00b0'20''N to 44'#$00b0'N; 142'#$00b0'E to 143'#$00b0'E',
           142.00, 43.33, 143.00, 44.00
         );
  _cs.Add(   2435, 'Japan - 43'#$00b0'20''N to 44'#$00b0'N; 143'#$00b0'E to 144'#$00b0'E',
           143.00, 43.33, 144.00, 44.00
         );
  _cs.Add(   2436, 'Japan - 43'#$00b0'20''N to 44'#$00b0'N; 144'#$00b0'E to 145'#$00b0'E',
           144.00, 43.33, 145.00, 44.00
         );
  _cs.Add(   2438, 'Japan - 42'#$00b0'40''N to 43'#$00b0'25''N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 42.66, 141.00, 43.42
         );
  _cs.Add(   2439, 'Japan - 42'#$00b0'40''N to 43'#$00b0'20''N; 141'#$00b0'E to 142'#$00b0'E',
           141.00, 42.66, 142.00, 43.34
         );
  _cs.Add(   2440, 'Japan - 42'#$00b0'40''N to 43'#$00b0'20''N; 142'#$00b0'E to 143'#$00b0'E',
           142.00, 42.66, 143.00, 43.34
         );
  _cs.Add(   2441, 'Japan - 42'#$00b0'40''N to 43'#$00b0'20''N; 143'#$00b0'E to 144'#$00b0'E',
           143.00, 42.66, 144.00, 43.34
         );
  _cs.Add(   2442, 'Japan - 42'#$00b0'40''N to 43'#$00b0'20''N; 144'#$00b0'E to 145'#$00b0'E',
           144.00, 42.84, 145.00, 43.34
         );
  _cs.Add(   2443, 'Japan - 42'#$00b0'40''N to 43'#$00b0'20''N; 145'#$00b0'E to 146'#$00b0'E',
           145.00, 42.93, 145.87, 43.34
         );
  _cs.Add(   2444, 'Japan - north of 42'#$00b0'N; west of 140'#$00b0'E',
           139.70, 42.05, 140.00, 42.73
         );
  _cs.Add(   2445, 'Japan - 42'#$00b0'N to 42'#$00b0'40''N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 41.99, 141.00, 42.67
         );
  _cs.Add(   2446, 'Japan - 42'#$00b0'N to 42'#$00b0'40''N; 141'#$00b0'E to 142'#$00b0'E',
           141.00, 42.24, 142.00, 42.67
         );
  _cs.Add(   2447, 'Japan - 42'#$00b0'N to 42'#$00b0'40''N; 142'#$00b0'E to 143'#$00b0'E',
           142.00, 42.02, 143.00, 42.67
         );
  _cs.Add(   2449, 'Japan - 41'#$00b0'20''N to 42'#$00b0'N; west of 141'#$00b0'E',
           139.91, 41.33, 141.00, 42.00
         );
  _cs.Add(   2450, 'Japan - 41'#$00b0'20''N to 42'#$00b0'N; 141'#$00b0'E to 142'#$00b0'E',
           141.00, 41.33, 141.53, 41.96
         );
  _cs.Add(   2451, 'Japan - 40'#$00b0'40''N to 41'#$00b0'20''N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 40.66, 141.00, 41.34
         );
  _cs.Add(   2452, 'Japan - 40'#$00b0'40''N to 41'#$00b0'20''N; 141'#$00b0'E to 142'#$00b0'E',
           141.00, 40.66, 141.53, 41.34
         );
  _cs.Add(   2453, 'Japan - 40'#$00b0'N to 40'#$00b0'48''N; 139'#$00b0'E to 140'#$00b0'E',
           139.63, 39.99, 140.00, 40.80
         );
  _cs.Add(   2454, 'Japan - 40'#$00b0'N to 40'#$00b0'40''N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 39.99, 141.00, 40.67
         );
  _cs.Add(   2455, 'Japan - 40'#$00b0'N to 40'#$00b0'40''N; 141'#$00b0'E to 142'#$00b0'E',
           141.00, 39.99, 142.00, 40.67
         );
  _cs.Add(   2456, 'Japan - 39'#$00b0'20''N to 40'#$00b0'N; 139'#$00b0'E to 140'#$00b0'E',
           139.63, 39.33, 140.00, 40.00
         );
  _cs.Add(   2457, 'Japan - 39'#$00b0'20''N to 40'#$00b0'N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 39.33, 141.00, 40.00
         );
  _cs.Add(   2458, 'Japan - 39'#$00b0'20''N to 40'#$00b0'N; east of 141'#$00b0'E',
           141.00, 39.33, 142.14, 40.00
         );
  _cs.Add(   2459, 'Japan - 38'#$00b0'40''N to 39'#$00b0'20''N; 139'#$00b0'E to 140'#$00b0'E',
           139.55, 38.66, 140.00, 39.34
         );
  _cs.Add(   2460, 'Japan - 38'#$00b0'40''N to 39'#$00b0'20''N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 38.66, 141.00, 39.34
         );
  _cs.Add(   2461, 'Japan - 38'#$00b0'40''N to 39'#$00b0'20''N; 141'#$00b0'E to 142'#$00b0'E',
           141.00, 38.66, 141.99, 39.34
         );
end;

procedure prepare_16;
begin
  _cs.Add(   2462, 'Japan - 38'#$00b0'N to 38'#$00b0'40''N; 139'#$00b0'E to 140'#$00b0'E',
           139.11, 37.99, 140.00, 38.67
         );
  _cs.Add(   2463, 'Japan - 38'#$00b0'N to 38'#$00b0'40''N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 37.99, 141.00, 38.67
         );
  _cs.Add(   2464, 'Japan - 38'#$00b0'N to 38'#$00b0'40''N; 141'#$00b0'E to 142'#$00b0'E',
           141.00, 38.08, 141.62, 38.67
         );
  _cs.Add(   2465, 'Japan - 37'#$00b0'20''N to 38'#$00b0'N; 136'#$00b0'E to 137'#$00b0'E',
           136.67, 37.33, 137.00, 37.47
         );
  _cs.Add(   2466, 'Japan - 37'#$00b0'20''N to 38'#$00b0'N; 137'#$00b0'E to 138'#$00b0'E',
           137.00, 37.33, 137.43, 37.58
         );
  _cs.Add(   2467, 'Japan - 37'#$00b0'20''N to 38'#$00b0'N; 138'#$00b0'E to 139'#$00b0'E',
           138.39, 37.33, 139.00, 37.97
         );
  _cs.Add(   2468, 'Japan - 37'#$00b0'20''N to 38'#$00b0'N; 139'#$00b0'E to 140'#$00b0'E',
           139.00, 37.33, 140.00, 38.00
         );
  _cs.Add(   2469, 'Japan - 37'#$00b0'20''N to 38'#$00b0'N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 37.33, 141.00, 38.00
         );
  _cs.Add(   2470, 'Japan - 37'#$00b0'20''N to 38'#$00b0'N; 141'#$00b0'E to 142'#$00b0'E',
           141.00, 37.33, 141.11, 37.87
         );
  _cs.Add(   2515, 'Japan - 33'#$00b0'20''N to 34'#$00b0'N; 136'#$00b0'E to 137'#$00b0'E',
           136.00, 33.54, 136.34, 34.00
         );
  _cs.Add(   2516, 'Japan - 32'#$00b0'40''N to 33'#$00b0'20''N; 129'#$00b0'E to 130'#$00b0'E',
           129.30, 32.51, 130.00, 33.34
         );
  _cs.Add(   2517, 'Japan - 32'#$00b0'40''N to 33'#$00b0'20''N; 130'#$00b0'E to 131'#$00b0'E',
           130.00, 32.66, 131.00, 33.34
         );
  _cs.Add(   2518, 'Japan - 32'#$00b0'40''N to 33'#$00b0'20''N; 131'#$00b0'E to 132'#$00b0'E',
           131.00, 32.66, 132.00, 33.34
         );
  _cs.Add(   2519, 'Japan - 32'#$00b0'40''N to 33'#$00b0'20''N; 132'#$00b0'E to 133'#$00b0'E',
           132.00, 32.69, 133.00, 33.34
         );
  _cs.Add(   2520, 'Japan - 32'#$00b0'40''N to 33'#$00b0'20''N; 133'#$00b0'E to 134'#$00b0'E',
           133.00, 32.70, 134.00, 33.34
         );
  _cs.Add(   2521, 'Japan - 32'#$00b0'40''N to 33'#$00b0'20''N; 134'#$00b0'E to 135'#$00b0'E',
           134.00, 33.19, 134.27, 33.34
         );
  _cs.Add(   2522, 'Japan - 32'#$00b0'N to 32'#$00b0'40''N; 129'#$00b0'54''E to 131'#$00b0'E',
           129.89, 31.99, 131.00, 32.67
         );
  _cs.Add(   2523, 'Japan - 32'#$00b0'N to 32'#$00b0'40''N; 131'#$00b0'E to 132'#$00b0'E',
           131.00, 31.99, 131.91, 32.67
         );
  _cs.Add(   3200, 'Japan - Iwo Jima',
           141.20, 24.67, 141.42, 24.89
         );
  _cs.Add(   3203, 'Japan - Minamitori-shima (Marcus Island)',
           153.95, 24.26, 154.01, 24.32
         )         .MarkDeprecated( True );
  _cs.Add(   1130, 'Jordan',
           34.88, 29.18, 39.31, 33.38
         );
  _cs.Add(   1131, 'Kazakhstan',
           46.49, 40.59, 87.35, 55.45
         );
  _cs.Add(   1132, 'Kenya',
           33.90, -4.90, 44.28, 4.63
         );
  _cs.Add(   1133, 'Kiribati',
           167.81, -13.84, -146.82, 7.92
         );
  _cs.Add(   1134, 'Korea, Democratic People''s Republic of (North Korea)',
           123.54, 37.10, 132.82, 43.01
         );
  _cs.Add(   1135, 'Korea, Republic of (South Korea)',
           122.71, 28.60, 134.28, 40.27
         );
  _cs.Add(   1136, 'Kuwait',
           46.54, 28.53, 49.53, 30.09
         );
  _cs.Add(   1137, 'Kyrgyzstan',
           69.24, 39.19, 80.29, 43.22
         );
  _cs.Add(   1138, 'Laos',
           100.09, 13.92, 107.64, 22.50
         );
  _cs.Add(   1139, 'Latvia',
           19.06, 55.67, 28.24, 58.09
         );
  _cs.Add(   1140, 'Lebanon',
           33.75, 33.06, 36.63, 34.84
         );
  _cs.Add(   1141, 'Lesotho',
           27.01, -30.66, 29.46, -28.57
         );
  _cs.Add(   1142, 'Liberia',
           -13.59, 1.02, -7.36, 8.52
         );
  _cs.Add(   1143, 'Libya',
           9.31, 19.50, 26.21, 35.23
         );
  _cs.Add(   1310, 'Kuwait - Kuwait City',
           47.78, 29.17, 48.16, 29.45
         );
  _cs.Add(   1471, 'Libya - 10'#$00b0'E to 12'#$00b0'E onshore',
           10.00, 23.51, 12.00, 33.23
         );
  _cs.Add(   1472, 'Libya - 12'#$00b0'E to 14'#$00b0'E onshore',
           12.00, 22.80, 14.00, 33.06
         );
  _cs.Add(   1473, 'Libya - 14'#$00b0'E to 16'#$00b0'E onshore',
           14.00, 22.61, 16.00, 32.79
         );
  _cs.Add(   1474, 'Libya - 16'#$00b0'E to 18'#$00b0'E onshore',
           16.00, 22.51, 18.01, 31.34
         );
  _cs.Add(   1475, 'Libya - 18'#$00b0'E to 20'#$00b0'E onshore',
           18.00, 21.54, 20.00, 32.17
         );
  _cs.Add(   1476, 'Libya - 20'#$00b0'E to 22'#$00b0'E onshore',
           20.00, 20.54, 22.00, 33.00
         );
  _cs.Add(   1477, 'Libya - 22'#$00b0'E to 24'#$00b0'E onshore',
           22.00, 19.50, 24.00, 32.97
         );
  _cs.Add(   1478, 'Libya - east of 24'#$00b0'E onshore',
           24.00, 19.99, 25.21, 32.15
         );
  _cs.Add(   1480, 'Libya - 12'#$00b0'E to 18'#$00b0'E onshore',
           12.00, 22.51, 18.01, 33.06
         );
  _cs.Add(   1481, 'Libya - 18'#$00b0'E to 24'#$00b0'E onshore',
           17.99, 19.50, 24.00, 33.00
         );
  _cs.Add(   2471, 'Japan - 36'#$00b0'40''N to 37'#$00b0'20''N; 136'#$00b0'E to 137'#$00b0'E',
           136.58, 36.66, 137.00, 37.34
         );
  _cs.Add(   2472, 'Japan - 36'#$00b0'40''N to 37'#$00b0'20''N; 137'#$00b0'E to 138'#$00b0'E',
           137.00, 36.66, 138.00, 37.34
         );
  _cs.Add(   2473, 'Japan - 36'#$00b0'40''N to 37'#$00b0'20''N; 138'#$00b0'E to 139'#$00b0'E',
           138.00, 36.66, 139.00, 37.34
         );
  _cs.Add(   2474, 'Japan - 36'#$00b0'40''N to 37'#$00b0'20''N; 139'#$00b0'E to 140'#$00b0'E',
           139.00, 36.66, 140.00, 37.34
         );
  _cs.Add(   2475, 'Japan - 36'#$00b0'40''N to 37'#$00b0'20''N; east of 140'#$00b0'E',
           140.00, 36.66, 141.10, 37.34
         );
  _cs.Add(   2476, 'Japan - 36'#$00b0'N to 36'#$00b0'40''N; west of137'#$00b0'E',
           135.90, 35.99, 137.00, 36.67
         );
  _cs.Add(   2477, 'Japan - 36'#$00b0'N to 36'#$00b0'40''N; 137'#$00b0'E to 138'#$00b0'E',
           137.00, 35.99, 138.00, 36.67
         );
  _cs.Add(   2478, 'Japan - 36'#$00b0'N to 36'#$00b0'40''N; 138'#$00b0'E to 139'#$00b0'E',
           138.00, 35.99, 139.00, 36.67
         );
  _cs.Add(   2479, 'Japan - 36'#$00b0'N to 36'#$00b0'40''N; 139'#$00b0'E to 140'#$00b0'E',
           139.00, 35.99, 140.00, 36.67
         );
  _cs.Add(   2480, 'Japan - 36'#$00b0'N to 36'#$00b0'40''N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 35.99, 140.77, 36.67
         );
  _cs.Add(   2481, 'Japan - 35'#$00b0'20''N to 36'#$00b0'N; 132'#$00b0'E to 133'#$00b0'E',
           132.56, 35.33, 133.00, 35.58
         );
  _cs.Add(   2482, 'Japan - 35'#$00b0'20''N to 36'#$00b0'N; 133'#$00b0'E to 134'#$00b0'E',
           133.00, 35.33, 134.00, 35.64
         );
  _cs.Add(   2483, 'Japan - 35'#$00b0'20''N to 36'#$00b0'N; 134'#$00b0'E to 135'#$00b0'E',
           134.00, 35.33, 135.00, 35.73
         );
  _cs.Add(   2484, 'Japan - 35'#$00b0'20''N to 36'#$00b0'N; 135'#$00b0'E to 136'#$00b0'E',
           135.00, 35.33, 136.00, 36.00
         );
  _cs.Add(   2485, 'Japan - 35'#$00b0'20''N to 36'#$00b0'N; 136'#$00b0'E to 137'#$00b0'E',
           136.00, 35.33, 137.00, 36.00
         );
  _cs.Add(   2486, 'Japan - 35'#$00b0'20''N to 36'#$00b0'N; 137'#$00b0'E to 138'#$00b0'E',
           137.00, 35.33, 138.00, 36.00
         );
  _cs.Add(   2487, 'Japan - 35'#$00b0'20''N to 36'#$00b0'N; 138'#$00b0'E to 139'#$00b0'E',
           138.00, 35.33, 139.00, 36.00
         );
  _cs.Add(   2488, 'Japan - 35'#$00b0'20''N to 36'#$00b0'N; 139'#$00b0'E to 140'#$00b0'E',
           139.00, 35.33, 140.00, 36.00
         );
  _cs.Add(   2489, 'Japan - 35'#$00b0'20''N to 36'#$00b0'N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 35.33, 140.90, 36.00
         );
  _cs.Add(   2490, 'Japan - 34'#$00b0'40''N to 35'#$00b0'20''N; 132'#$00b0'E to 133'#$00b0'E',
           132.00, 34.66, 133.00, 35.34
         );
  _cs.Add(   2491, 'Japan - 34'#$00b0'40''N to 35'#$00b0'20''N; 133'#$00b0'E to 134'#$00b0'E',
           133.00, 34.66, 134.00, 35.34
         );
  _cs.Add(   2492, 'Japan - 34'#$00b0'40''N to 35'#$00b0'20''N; 134'#$00b0'E to 135'#$00b0'E',
           134.00, 34.66, 135.00, 35.34
         );
  _cs.Add(   2493, 'Japan - 34'#$00b0'40N'' to 35'#$00b0'20''N; 135'#$00b0'E to 136'#$00b0'E',
           135.00, 34.66, 136.00, 35.34
         );
  _cs.Add(   2494, 'Japan - 34'#$00b0'40''N to 35'#$00b0'20''N; 136'#$00b0'E to 137'#$00b0'E',
           136.00, 34.66, 137.00, 35.34
         );
  _cs.Add(   2495, 'Japan - 34'#$00b0'40''N to 35'#$00b0'20''N; 137'#$00b0'E to 138'#$00b0'E',
           137.00, 34.66, 138.00, 35.34
         );
  _cs.Add(   2496, 'Japan - 34'#$00b0'40''N to 35'#$00b0'20''N; 138'#$00b0'E to 139'#$00b0'E',
           138.00, 34.66, 139.00, 35.34
         );
  _cs.Add(   2497, 'Japan - 34'#$00b0'40''N to 35'#$00b0'20''N; 139'#$00b0'E to 140'#$00b0'E',
           139.00, 34.66, 140.00, 35.34
         );
  _cs.Add(   2498, 'Japan - 34'#$00b0'40''N to 35'#$00b0'20''N; 140'#$00b0'E to 141'#$00b0'E',
           140.00, 34.87, 140.48, 35.34
         );
  _cs.Add(   2499, 'Japan - 34'#$00b0'N to 34'#$00b0'40''N; 130'#$00b0'E to 131'#$00b0'E',
           130.81, 33.99, 131.00, 34.48
         );
  _cs.Add(   2500, 'Japan - north of 34'#$00b0'N; 131'#$00b0'E to 132'#$00b0'E',
           131.00, 33.99, 132.00, 34.90
         );
  _cs.Add(   2501, 'Japan - 34'#$00b0'N to 34'#$00b0'40''N; 132'#$00b0'E to 133'#$00b0'E',
           132.00, 33.99, 133.00, 34.67
         );
  _cs.Add(   2502, 'Japan - 34'#$00b0'N to 34'#$00b0'40''N; 133'#$00b0'E to 134'#$00b0'E',
           133.00, 33.99, 134.00, 34.67
         );
  _cs.Add(   2503, 'Japan - 34'#$00b0'N to 34'#$00b0'40''N; 134'#$00b0'E to 135'#$00b0'E',
           134.00, 33.99, 135.00, 34.67
         );
  _cs.Add(   2504, 'Japan - 34'#$00b0'N to 34'#$00b0'40''N; 135'#$00b0'E to 136'#$00b0'E',
           135.00, 33.99, 136.00, 34.67
         );
  _cs.Add(   2505, 'Japan - 34'#$00b0'N to 34'#$00b0'40''N; 136'#$00b0'E to 137'#$00b0'E',
           136.00, 33.99, 137.00, 34.67
         );
  _cs.Add(   2506, 'Japan - 34'#$00b0'N to 34'#$00b0'40''N; 137'#$00b0'E to 138'#$00b0'E',
           137.00, 34.51, 138.00, 34.67
         );
  _cs.Add(   2507, 'Japan - 34'#$00b0'N to 34'#$00b0'40''N; 138'#$00b0'E to 139'#$00b0'E',
           138.00, 34.54, 139.00, 34.67
         );
  _cs.Add(   2508, 'Japan - 33'#$00b0'20''N to 34'#$00b0'N; 129'#$00b0'E to 130'#$00b0'E',
           129.38, 33.33, 130.00, 33.59
         );
  _cs.Add(   2509, 'Japan - 33'#$00b0'20''N to 34'#$00b0'N; 130'#$00b0'E to 131'#$00b0'E',
           130.00, 33.33, 131.00, 34.00
         );
  _cs.Add(   2510, 'Japan - 33'#$00b0'20''N to 34'#$00b0'N; 131'#$00b0'E to 132'#$00b0'E',
           131.00, 33.33, 132.00, 34.00
         );
  _cs.Add(   2511, 'Japan - 33'#$00b0'20''N to 34'#$00b0'N; 132'#$00b0'E to 133'#$00b0'E',
           132.00, 33.33, 133.00, 34.00
         );
  _cs.Add(   2512, 'Japan - 33'#$00b0'20''N to 34'#$00b0'N; 133'#$00b0'E to 134'#$00b0'E',
           133.00, 33.33, 134.00, 34.00
         );
  _cs.Add(   2513, 'Japan - 33'#$00b0'20''N to 34'#$00b0'N; 134'#$00b0'E to 135'#$00b0'E',
           134.00, 33.33, 134.81, 34.00
         );
  _cs.Add(   2514, 'Japan - 33'#$00b0'20''N to 34'#$00b0'N; 135'#$00b0'E to 136'#$00b0'E',
           135.00, 33.40, 136.00, 34.00
         );
  _cs.Add(   3280, 'Morocco - onshore',
           -13.24, 27.66, -1.01, 35.97
         );
  _cs.Add(   3281, 'Mozambique - onshore',
           30.21, -26.87, 40.90, -10.42
         );
  _cs.Add(   3340, 'Mayotte - onshore',
           44.98, -13.05, 45.35, -12.61
         );
  _cs.Add(   3361, 'Mexico - offshore GoM - Tampico area',
           -98.10, 21.51, -96.89, 22.75
         );
  _cs.Add(   3376, 'Malaysia - West Malaysia - Johor',
           102.44, 1.21, 104.60, 2.95
         );
  _cs.Add(   3377, 'Malaysia - West Malaysia - Sembilan and Melaka',
           101.70, 2.03, 102.71, 3.28
         );
  _cs.Add(   3378, 'Malaysia - West Malaysia - Pahang',
           101.33, 2.45, 103.67, 4.78
         );
  _cs.Add(   3379, 'Malaysia - West Malaysia - Selangor',
           100.76, 2.54, 101.97, 3.87
         );
  _cs.Add(   3380, 'Malaysia - West Malaysia - Terengganu',
           102.38, 3.89, 103.72, 5.90
         );
  _cs.Add(   3381, 'Malaysia - West Malaysia - Pulau Pinang',
           100.12, 5.12, 100.56, 5.59
         );
  _cs.Add(   3382, 'Malaysia - West Malaysia - Kedah and Perlis',
           99.59, 5.08, 101.12, 6.72
         );
end;

procedure prepare_17;
begin
  _cs.Add(   3383, 'Malaysia - West Malaysia - Perak',
           100.07, 3.66, 102.00, 5.92
         );
  _cs.Add(   3384, 'Malaysia - West Malaysia - Kelantan',
           101.33, 4.54, 102.67, 6.29
         );
  _cs.Add(   3423, 'Mexico - west of 114'#$00b0'W',
           -122.19, 15.01, -114.00, 32.72
         );
  _cs.Add(   3424, 'Mexico - 114'#$00b0'W to 108'#$00b0'W',
           -114.00, 15.09, -108.00, 32.27
         );
  _cs.Add(   3425, 'Mexico - 108'#$00b0'W to 102'#$00b0'W',
           -108.00, 14.05, -102.00, 31.79
         );
  _cs.Add(   3426, 'Mexico - 102'#$00b0'W to 96'#$00b0'W',
           -102.00, 12.30, -96.00, 29.81
         );
  _cs.Add(   3461, 'Mexico - offshore GoM - Campeche area N',
           -94.33, 20.87, -88.67, 23.01
         );
  _cs.Add(   3462, 'Mexico - offshore GoM - Campeche area S',
           -94.79, 17.85, -89.75, 20.89
         );
  _cs.Add(   3535, 'Montenegro',
           18.02, 41.27, 20.38, 43.56
         );
  _cs.Add(   3536, 'Montenegro - onshore',
           18.45, 41.79, 20.38, 43.56
         );
  _cs.Add(   3615, 'Moldova - west of 30'#$00b0'E',
           26.63, 45.44, 30.00, 48.47
         );
  _cs.Add(   3616, 'Moldova - east of 30'#$00b0'E',
           30.00, 46.37, 30.13, 46.47
         );
  _cs.Add(   3633, 'Mexico - 96'#$00b0'W to 90'#$00b0'W',
           -96.00, 12.10, -90.00, 26.00
         );
  _cs.Add(   3635, 'Mexico - east of 90'#$00b0'W',
           -90.00, 17.81, -84.64, 25.77
         );
  _cs.Add(   3931, 'Mozambique - 36'#$00b0'E to 42'#$00b0'E',
           36.00, -27.71, 42.00, -10.09
         );
  _cs.Add(   3935, 'Mozambique - east of 42'#$00b0'E',
           42.00, -15.83, 43.03, -10.09
         );
  _cs.Add(   3938, 'Mauritania - 18'#$00b0'W to 12'#$00b0'W',
           -18.00, 14.72, -12.00, 23.46
         );
  _cs.Add(   3939, 'Mauritania - 12'#$00b0'W to 6'#$00b0'W',
           -12.00, 14.73, -6.00, 27.31
         )         .MarkDeprecated( True );
  _cs.Add(   3940, 'Mauritania - east of 6'#$00b0'W',
           -6.00, 14.73, 0.00, 27.31
         )         .MarkDeprecated( True );
  _cs.Add(   3949, 'Libya - west of 12'#$00b0'E',
           9.31, 23.51, 12.00, 33.92
         );
  _cs.Add(   3955, 'Malaysia - West Malaysia',
           98.02, 1.13, 105.82, 7.81
         );
  _cs.Add(   3977, 'Malaysia - East Malaysia',
           109.31, 0.85, 119.61, 7.67
         );
  _cs.Add(   3994, 'Madagascar',
           40.31, -28.92, 53.39, -10.28
         );
  _cs.Add(   4611, 'Malaysia - East Malaysia - Sarawak onshore',
           109.54, 0.85, 115.69, 5.03
         );
  _cs.Add(   1168, 'Myanmar (Burma)',
           89.61, 9.48, 101.17, 28.55
         );
  _cs.Add(   1169, 'Namibia',
           8.24, -30.64, 25.27, -16.95
         );
  _cs.Add(   1170, 'Nauru',
           163.58, -3.91, 168.60, 2.69
         );
  _cs.Add(   1171, 'Nepal',
           80.06, 26.34, 88.21, 30.43
         );
  _cs.Add(   1172, 'Netherlands',
           2.53, 50.75, 7.22, 55.77
         );
  _cs.Add(   1173, 'Netherlands Antilles',
           -69.59, 11.67, -62.82, 18.07
         )         .MarkDeprecated( True );
  _cs.Add(   3674, 'Norway - onshore - 29'#$00b0'E to 30'#$00b0'E',
           29.00, 69.02, 30.00, 70.93
         );
  _cs.Add(   3675, 'Paraguay - north of 22'#$00b0'S',
           -62.57, -22.00, -57.81, -19.29
         );
  _cs.Add(   3676, 'Norway - onshore - east of 30'#$00b0'E',
           30.00, 69.46, 31.22, 70.77
         );
  _cs.Add(   3836, 'Peru - east of 72'#$00b0'W',
           -72.00, -20.44, -68.67, -2.14
         );
  _cs.Add(   3837, 'Peru - 84'#$00b0'W to 78'#$00b0'W',
           -84.00, -17.33, -78.00, -3.11
         );
  _cs.Add(   3838, 'Peru - 78'#$00b0'W to 72'#$00b0'W',
           -78.00, -21.05, -72.00, -0.03
         );
  _cs.Add(   3882, 'Papua New Guinea - west of 144'#$00b0'E',
           139.20, -11.15, 144.00, 2.31
         );
  _cs.Add(   3885, 'Papua New Guinea - 144'#$00b0'E to 150'#$00b0'E',
           144.00, -13.88, 150.01, 2.58
         );
  _cs.Add(   3888, 'Papua New Guinea - 150'#$00b0'E to 156'#$00b0'E',
           150.00, -14.75, 156.00, 1.98
         );
  _cs.Add(   3958, 'Philippines - zone I onshore',
           116.89, 7.75, 118.00, 9.32
         );
  _cs.Add(   3964, 'Philippines - zone II onshore',
           118.00, 8.82, 120.07, 11.58
         );
  _cs.Add(   3965, 'Philippines - zone III onshore',
           119.70, 4.99, 122.21, 19.45
         );
  _cs.Add(   3966, 'Philippines - zone IV onshore',
           121.74, 6.35, 124.29, 18.58
         );
  _cs.Add(   3967, 'Philippines - zone V onshore',
           123.73, 5.50, 126.65, 14.15
         );
  _cs.Add(   3969, 'Philippines - onshore',
           116.89, 4.99, 126.65, 19.45
         );
  _cs.Add(   4008, 'Oman - mainland east of 54'#$00b0'E',
           54.00, 16.89, 59.91, 26.42
         );
  _cs.Add(   4009, 'Oman - mainland',
           51.99, 16.59, 59.91, 26.42
         );
  _cs.Add(   4013, 'Papua New Guinea - PFTB',
           142.24, -8.28, 144.75, -5.59
         );
  _cs.Add(   4066, 'Norway - onshore - 6'#$00b0'E to 12'#$00b0'E',
           6.00, 57.93, 12.00, 65.76
         );
  _cs.Add(   4162, 'Pacific - US interests Pacific plate',
           157.47, -17.56, -151.27, 31.80
         );
  _cs.Add(   4167, 'Pacific - US interests Mariana plate',
           129.48, 1.64, 149.55, 23.90
         );
  _cs.Add(   4214, 'Papua New Guinea - mainland onshore',
           140.85, -10.76, 150.96, -2.53
         );
  _cs.Add(   4216, 'Papua New Guinea - North Fly',
           140.89, -6.60, 141.54, -5.05
         );
  _cs.Add(   4322, 'Oman - west of 54'#$00b0'E',
           51.99, 14.94, 54.01, 19.67
         );
  _cs.Add(   4323, 'Oman - 54'#$00b0'E to 60'#$00b0'E',
           54.00, 14.33, 60.01, 26.74
         );
  _cs.Add(   4324, 'Oman - east of 60'#$00b0'E',
           60.00, 16.37, 63.38, 24.09
         );
  _cs.Add(   4383, 'Papua New Guinea - onshore south of 5'#$00b0'S and west of 144'#$00b0'E',
           140.85, -9.35, 144.01, -5.00
         );
  _cs.Add(   4384, 'Papua New Guinea - 0'#$00b0'N to 12'#$00b0'S and 140'#$00b0'E to 158'#$00b0'E',
           140.00, -12.00, 158.01, 0.01
         );
  _cs.Add(   4425, 'Papua New Guinea - onshore - Central province and Gulf province east of 144'#$00b0'E',
           144.40, -10.42, 149.67, -6.67
         );
  _cs.Add(   4514, 'Pacific - Guam and NMI west of 144'#$00b0'E',
           141.19, 10.95, 144.01, 23.90
         );
  _cs.Add(   4518, 'Pacific - Guam and NMI east of 144'#$00b0'E',
           144.00, 11.05, 149.55, 23.90
         );
  _cs.Add(   4525, 'Pacific - Guam and NMI - onshore',
           144.58, 13.18, 146.12, 20.61
         );
  _cs.Add(   4615, 'Norway, Svalbard and Jan Mayen - offshore',
           -13.63, 56.08, 38.00, 84.73
         );
  _cs.Add(   4653, 'Papua New Guinea - 156'#$00b0'E to 162'#$00b0'E',
           156.00, -14.26, 162.00, -1.11
         );
  _cs.Add(   4654, 'Papua New Guinea - east of 162'#$00b0'E',
           162.00, -4.35, 162.80, -2.34
         );
  _cs.Add(   1193, 'Portugal',
           -35.58, 29.24, -6.19, 43.07
         );
  _cs.Add(   1194, 'Puerto Rico',
           -68.49, 14.92, -65.04, 21.86
         );
  _cs.Add(   1195, 'Qatar',
           50.55, 24.55, 53.04, 27.05
         );
  _cs.Add(   1196, 'Reunion',
           37.58, -25.92, 58.27, -10.60
         )         .MarkDeprecated( True );
  _cs.Add(   1781, 'Russia - 126'#$00b0'E to 132'#$00b0'E onshore',
           126.00, 42.25, 132.00, 73.61
         );
  _cs.Add(   1782, 'Russia - 132'#$00b0'E to 138'#$00b0'E onshore',
           132.00, 42.63, 138.00, 76.15
         );
  _cs.Add(   1783, 'Russia - 138'#$00b0'E to 144'#$00b0'E onshore',
           138.00, 45.84, 144.00, 76.27
         );
  _cs.Add(   1784, 'Russia - 144'#$00b0'E to 150'#$00b0'E onshore',
           144.00, 43.60, 150.00, 76.82
         );
  _cs.Add(   1785, 'Russia - 150'#$00b0'E to 156'#$00b0'E onshore',
           150.00, 45.77, 156.00, 76.26
         );
  _cs.Add(   1786, 'Russia - 156'#$00b0'E to 162'#$00b0'E onshore',
           156.00, 50.27, 162.00, 77.20
         );
  _cs.Add(   1787, 'Russia - 162'#$00b0'E to 168'#$00b0'E onshore',
           162.00, 54.47, 168.00, 70.03
         );
  _cs.Add(   1788, 'Russia - 168'#$00b0'E to 174'#$00b0'E onshore',
           168.00, 54.45, 174.00, 70.19
         );
  _cs.Add(   1789, 'Russia - 174'#$00b0'E to 180'#$00b0'E onshore',
           174.00, 61.65, 180.00, 71.59
         );
  _cs.Add(   1790, 'Russia - 180'#$00b0' to 174'#$00b0'W onshore',
           -180.00, 64.35, -174.00, 71.65
         );
  _cs.Add(   2406, 'Qatar - offshore',
           50.55, 24.64, 53.04, 27.05
         );
  _cs.Add(   2546, 'Romania - offshore',
           28.64, 43.44, 31.41, 45.20
         );
  _cs.Add(   2680, 'Russia - 100.5'#$00b0'E to 103.5'#$00b0'E onshore',
           100.50, 50.17, 103.50, 79.71
         );
  _cs.Add(   2681, 'Russia - 103.5'#$00b0'E to 106.5'#$00b0'E onshore',
           103.50, 50.13, 106.50, 79.21
         );
  _cs.Add(   2682, 'Russia - 106.5'#$00b0'E to 109.5'#$00b0'E onshore',
           106.50, 49.25, 109.50, 78.40
         );
  _cs.Add(   2683, 'Russia - 109.5'#$00b0'E to 112.5'#$00b0'E onshore',
           109.50, 49.14, 112.50, 76.81
         );
  _cs.Add(   2684, 'Russia - 112.5'#$00b0'E to 115.5'#$00b0'E onshore',
           112.50, 49.49, 115.50, 76.70
         );
  _cs.Add(   2685, 'Russia - 115.5'#$00b0'E to 118.5'#$00b0'E onshore',
           115.50, 49.51, 118.50, 74.43
         );
  _cs.Add(   2686, 'Russia - 118.5'#$00b0'E to 121.5'#$00b0'E onshore',
           118.50, 49.87, 121.50, 73.63
         );
  _cs.Add(   2687, 'Russia - 121.5'#$00b0'E to 124.5'#$00b0'E onshore',
           121.50, 53.18, 124.50, 74.00
         );
  _cs.Add(   2688, 'Russia - 124.5'#$00b0'E to 127.5'#$00b0'E onshore',
           124.50, 49.88, 127.50, 74.00
         );
  _cs.Add(   2689, 'Russia - 127.5'#$00b0'E to 130.5'#$00b0'E onshore',
           127.50, 42.67, 130.50, 73.59
         );
  _cs.Add(   1496, 'Korea, Republic of (South Korea) - east of 128'#$00b0'E onshore',
           128.00, 34.49, 129.65, 38.64
         );
  _cs.Add(   1497, 'Korea, Republic of (South Korea) - 126'#$00b0'E to 128'#$00b0'E onshore',
           126.00, 33.14, 128.00, 38.33
         );
  _cs.Add(   1498, 'Korea, Republic of (South Korea) - 124'#$00b0'E to 126'#$00b0'E onshore',
           124.53, 33.99, 126.00, 38.04
         );
  _cs.Add(   1582, 'Kenya - north of equator and east of 36'#$00b0'E',
           36.00, 0.00, 41.91, 4.49
         );
  _cs.Add(   1739, 'Kuwait - west of 48'#$00b0'E onshore',
           46.54, 28.53, 48.00, 30.09
         );
  _cs.Add(   1740, 'Kuwait - east of 48'#$00b0'E onshore',
           48.00, 28.54, 48.48, 30.04
         );
  _cs.Add(   1854, 'Japan - zone I',
           128.17, 26.96, 130.46, 34.74
         );
  _cs.Add(   1855, 'Japan - zone II',
           129.76, 30.18, 132.05, 33.99
         );
  _cs.Add(   1856, 'Japan - zone III',
           130.81, 33.72, 133.49, 36.38
         );
end;

procedure prepare_18;
begin
  _cs.Add(   1857, 'Japan - zone IV',
           131.95, 32.69, 134.81, 34.45
         );
  _cs.Add(   1858, 'Japan - zone V',
           133.13, 34.13, 135.47, 35.71
         );
  _cs.Add(   1859, 'Japan - zone VI',
           134.86, 33.40, 136.99, 36.33
         );
  _cs.Add(   1860, 'Japan - zone VII',
           136.22, 34.51, 137.84, 37.58
         );
  _cs.Add(   1861, 'Japan - zone VIII',
           137.32, 34.54, 139.91, 38.58
         );
  _cs.Add(   1862, 'Japan - zone IX',
           138.40, 29.31, 141.11, 37.98
         );
  _cs.Add(   1863, 'Japan - zone X',
           139.49, 37.73, 142.14, 41.58
         );
  _cs.Add(   1864, 'Japan - zone XI',
           139.34, 41.34, 141.46, 43.42
         );
  _cs.Add(   1865, 'Japan - zone XII',
           140.89, 42.15, 143.61, 45.54
         );
  _cs.Add(   1866, 'Japan - zone XIII',
           142.61, 41.87, 145.87, 44.40
         );
  _cs.Add(   1867, 'Japan - zone XIV',
           141.20, 24.67, 142.33, 27.80
         );
  _cs.Add(   1868, 'Japan - zone XV',
           126.63, 26.02, 128.40, 26.91
         );
  _cs.Add(   1869, 'Japan - zone XVI',
           122.83, 23.98, 125.51, 24.94
         );
  _cs.Add(   1870, 'Japan - zone XVII',
           131.12, 24.40, 131.38, 26.01
         );
  _cs.Add(   1871, 'Japan - zone XVIII',
           136.02, 20.37, 136.16, 20.48
         );
  _cs.Add(   2405, 'Kazakhstan - Caspian Sea',
           48.90, 41.15, 53.15, 46.97
         );
  _cs.Add(   2408, 'Japan - Okinawa',
           122.83, 23.98, 131.38, 26.91
         );
  _cs.Add(   2437, 'Japan - north of 43'#$00b0'20''N; 145'#$00b0'E to 146'#$00b0'E',
           145.00, 43.33, 145.87, 44.40
         );
  _cs.Add(   2448, 'Japan - south of 42'#$00b0'40''N; 143'#$00b0'E to 144'#$00b0'E',
           143.00, 41.87, 143.76, 42.67
         );
  _cs.Add(   2526, 'Japan - onshore mainland south of 31'#$00b0'20''N',
           130.14, 30.94, 131.19, 31.34
         );
  _cs.Add(   2786, 'Libya - Mabruk',
           17.13, 29.61, 17.51, 30.07
         );
  _cs.Add(   2956, 'Kuwait - north of 29.25'#$00b0'N',
           46.54, 29.10, 48.42, 30.09
         );
  _cs.Add(   2957, 'Kuwait - south of 29.25'#$00b0'N',
           46.54, 28.53, 48.48, 29.45
         );
  _cs.Add(   2987, 'Libya - Amal',
           20.80, 29.10, 21.40, 29.80
         );
  _cs.Add(   3196, 'Kiribati - Phoenix Islands',
           -174.60, -4.76, -170.66, -2.68
         );
  _cs.Add(   3201, 'Johnston Island',
           -169.59, 16.67, -169.47, 16.79
         );
  _cs.Add(   3263, 'Japan - onshore mainland',
           129.30, 30.94, 145.87, 45.54
         );
  _cs.Add(   3264, 'Kenya - onshore',
           33.90, -4.72, 41.91, 4.63
         );
  _cs.Add(   3265, 'Korea, Democratic People''s Republic of (North Korea) - onshore',
           124.27, 37.62, 130.75, 43.01
         );
  _cs.Add(   3266, 'Korea, Republic of (South Korea) - onshore',
           124.53, 33.14, 131.01, 38.64
         );
  _cs.Add(   3267, 'Kuwait - onshore',
           46.54, 28.53, 48.48, 30.09
         );
  _cs.Add(   3268, 'Latvia - onshore',
           20.87, 55.67, 28.24, 58.09
         );
  _cs.Add(   3269, 'Lebanon - onshore',
           35.04, 33.06, 36.63, 34.65
         );
  _cs.Add(   3270, 'Liberia - onshore',
           -11.52, 4.29, -7.36, 8.52
         );
  _cs.Add(   3418, 'Latin America - SIRGAS 2000 by country',
           -122.19, -59.87, -25.28, 32.72
         );
  _cs.Add(   3421, 'Latin America - 84'#$00b0'W to 78'#$00b0'West; N hemisphere and SIRGAS by country',
           -84.00, 0.00, -78.00, 19.54
         );
  _cs.Add(   3422, 'Latin America - 78'#$00b0'W to 72'#$00b0'West; N hemisphere and SIRGAS by country',
           -78.00, 0.00, -72.00, 15.04
         );
  _cs.Add(   3427, 'Latin America - 96'#$00b0'W to 90'#$00b0'W; N hemisphere and SIRGAS 2000 by country',
           -96.00, 0.00, -90.00, 26.00
         );
  _cs.Add(   3428, 'Latin America - 90'#$00b0'W to 84'#$00b0'W; N hemisphere and SIRGAS 2000 by country',
           -90.00, 0.00, -83.99, 25.77
         );
  _cs.Add(   3477, 'Libya - Cyrenaica',
           22.49, 32.00, 23.00, 32.80
         );
  _cs.Add(   3720, 'Korea, Republic of (South Korea) - 130'#$00b0'E to 132'#$00b0'E onshore',
           130.71, 37.39, 131.01, 37.62
         );
  _cs.Add(   3721, 'Korea, Republic of (South Korea) - 126'#$00b0'E to 128'#$00b0'E Jeju',
           126.09, 33.14, 127.01, 33.61
         );
  _cs.Add(   3730, 'Korea, Republic of (South Korea) - 126'#$00b0'E to 128'#$00b0'E mainland',
           126.00, 33.96, 128.00, 38.33
         );
  _cs.Add(   3739, 'Korea, Republic of (South Korea) - mainland',
           125.75, 33.96, 129.65, 38.64
         );
  _cs.Add(   3748, 'Latin America - 120'#$00b0'W to 114'#$00b0'W',
           -120.00, 15.01, -114.00, 32.72
         );
  _cs.Add(   3756, 'Latin America - 114'#$00b0'W to 108'#$00b0'W',
           -114.00, 15.09, -108.00, 32.27
         );
  _cs.Add(   3759, 'Latin America - 108'#$00b0'W to 102'#$00b0'W',
           -108.00, 14.05, -102.00, 31.79
         );
  _cs.Add(   3763, 'Latin America - 102'#$00b0'W to 96'#$00b0'W',
           -102.01, 12.30, -96.00, 29.81
         );
  _cs.Add(   3941, 'Libya - east of 24'#$00b0'E',
           24.00, 19.99, 26.21, 33.60
         );
  _cs.Add(   3950, 'Libya - 12'#$00b0'E to 18'#$00b0'E',
           12.00, 22.51, 18.00, 35.23
         );
  _cs.Add(   3951, 'Libya - 18'#$00b0'E to 24'#$00b0'E',
           17.99, 19.50, 24.01, 35.03
         );
  _cs.Add(   3957, 'Japan - onshore',
           122.83, 20.37, 154.05, 45.54
         );
  _cs.Add(   3995, 'Japan - onshore mainland and adjacent islands',
           128.31, 30.18, 145.87, 45.54
         );
  _cs.Add(   4163, 'Japan excluding northern main province',
           122.38, 17.09, 157.65, 46.05
         );
  _cs.Add(   4165, 'Japan - onshore mainland excluding eastern main province',
           129.30, 30.94, 145.87, 45.54
         );
  _cs.Add(   4166, 'Japan - onshore - Honshu, Shikoku, Kyushu',
           129.30, 30.94, 142.14, 41.58
         );
  _cs.Add(   4168, 'Japan - onshore - Hokkaido',
           139.70, 41.34, 145.87, 45.54
         );
  _cs.Add(   4170, 'Japan - northern Honshu',
           135.42, 34.84, 142.14, 41.58
         );
  _cs.Add(   4194, 'Japan onshore excluding northern main province',
           122.83, 20.37, 154.05, 45.54
         );
  _cs.Add(   4385, 'Kyrgyzstan - west of 70'#$00b0'01''E',
           69.24, 39.51, 70.02, 40.22
         );
  _cs.Add(   4386, 'Kyrgyzstan - 70'#$00b0'01''E to 73'#$00b0'01''E',
           70.01, 39.19, 73.02, 42.83
         );
  _cs.Add(   4387, 'Kyrgyzstan - 73'#$00b0'01''E to 76'#$00b0'01''E',
           73.01, 39.35, 76.02, 43.22
         );
  _cs.Add(   4388, 'Kyrgyzstan - 76'#$00b0'01''E to 79'#$00b0'01''E',
           76.01, 40.35, 79.02, 43.00
         );
  _cs.Add(   4389, 'Kyrgyzstan - east of 79'#$00b0'01''E',
           79.01, 41.66, 80.29, 42.80
         );
  _cs.Add(   4530, 'Latin America - Central America and South America',
           -122.19, -59.87, -25.28, 32.72
         );
  _cs.Add(   4542, 'Kosovo',
           19.97, 41.85, 21.80, 43.25
         );
  _cs.Add(   1144, 'Liechtenstein',
           9.47, 47.05, 9.64, 47.28
         );
  _cs.Add(   1145, 'Lithuania',
           19.02, 53.89, 26.82, 56.45
         );
  _cs.Add(   1146, 'Luxembourg',
           5.73, 49.44, 6.53, 50.19
         );
  _cs.Add(   1149, 'Madagascar - onshore and nearshore',
           42.53, -26.59, 51.03, -11.69
         );
  _cs.Add(   1150, 'Malawi',
           32.68, -17.14, 35.93, -9.37
         );
  _cs.Add(   1151, 'Malaysia',
           98.02, 0.85, 119.61, 7.81
         );
  _cs.Add(   1152, 'Maldives',
           69.29, -3.47, 77.08, 8.10
         );
  _cs.Add(   1153, 'Mali',
           -12.25, 10.14, 4.26, 25.01
         );
  _cs.Add(   1154, 'Malta',
           13.41, 34.49, 18.06, 36.56
         );
  _cs.Add(   1155, 'Marshall Islands',
           157.47, 1.77, 175.52, 17.88
         );
  _cs.Add(   1156, 'Martinique',
           -62.82, 14.08, -57.52, 16.36
         );
  _cs.Add(   1157, 'Mauritania',
           -20.04, 14.72, -4.80, 27.30
         );
  _cs.Add(   1158, 'Mauritius',
           53.80, -23.81, 67.05, -8.43
         );
  _cs.Add(   1159, 'Mayotte',
           43.68, -14.49, 46.70, -11.33
         );
  _cs.Add(   1160, 'Mexico',
           -122.19, 12.10, -84.64, 32.72
         );
  _cs.Add(   1161, 'Micronesia',
           135.27, -1.19, 165.68, 13.43
         );
  _cs.Add(   1162, 'Moldova',
           26.63, 45.44, 30.13, 48.47
         );
  _cs.Add(   1163, 'Monaco',
           7.39, 42.94, 7.76, 43.77
         );
  _cs.Add(   1164, 'Mongolia',
           87.76, 41.58, 119.94, 52.15
         );
  _cs.Add(   1165, 'Montserrat',
           -63.05, 15.84, -61.82, 17.04
         );
  _cs.Add(   1166, 'Morocco',
           -13.86, 27.66, -1.01, 36.00
         );
  _cs.Add(   1167, 'Mozambique',
           30.21, -27.71, 43.03, -10.09
         );
  _cs.Add(   1329, 'Mozambique - south',
           31.29, -26.87, 35.65, -19.84
         );
  _cs.Add(   1470, 'Libya - west of 10'#$00b0'E',
           9.31, 25.37, 10.01, 30.49
         );
  _cs.Add(   1479, 'Libya - west of 12'#$00b0'E onshore',
           9.31, 23.51, 12.00, 33.23
         );
  _cs.Add(   1482, 'Libya - west of 15'#$00b0'E onshore',
           9.31, 22.61, 15.00, 33.23
         );
  _cs.Add(   1540, 'Mozambique - onshore west of 36'#$00b0'E',
           30.21, -26.87, 36.00, -11.41
         );
  _cs.Add(   1541, 'Mozambique - onshore east of 36'#$00b0'E',
           35.99, -18.98, 40.90, -10.42
         );
  _cs.Add(   1690, 'Malaysia - West Malaysia - onshore',
           99.59, 1.21, 104.60, 6.72
         );
  _cs.Add(   1691, 'Malaysia - West Malaysia - onshore west of 102'#$00b0'E',
           99.59, 2.29, 102.01, 6.72
         );
  _cs.Add(   1692, 'Malaysia - West Malaysia - east of 102'#$00b0'E',
           102.00, 1.21, 105.82, 7.81
         );
  _cs.Add(   1703, 'Morocco - north of 31.5'#$00b0'N',
           -9.85, 31.49, -1.01, 35.97
         );
  _cs.Add(   1704, 'Morocco - 27.9'#$00b0'N to 31.5'#$00b0'N',
           -13.00, 27.90, -3.50, 31.50
         )         .MarkDeprecated( True );
  _cs.Add(   1705, 'Morocco - south of 27.9'#$00b0'N',
           -17.00, 21.06, -8.67, 27.90
         )         .MarkDeprecated( True );
end;

procedure prepare_19;
begin
  _cs.Add(   1726, 'Mozambique - offshore',
           32.64, -27.71, 43.03, -10.09
         );
  _cs.Add(   1848, 'Madagascar - nearshore - west of 48'#$00b0'E',
           42.53, -26.59, 48.00, -13.00
         );
  _cs.Add(   1849, 'Madagascar - nearshore - east of 48'#$00b0'E',
           48.00, -24.21, 51.03, -11.69
         );
  _cs.Add(   1851, 'Malaysia - East Malaysia onshore',
           109.54, 0.85, 119.33, 7.41
         );
  _cs.Add(   2350, 'Mozambique - A',
           31.91, -26.87, 34.50, -23.91
         );
  _cs.Add(   2351, 'Mozambique - B',
           31.29, -24.91, 35.65, -19.74
         );
  _cs.Add(   2352, 'Mozambique - C',
           30.21, -19.91, 39.18, -14.01
         );
  _cs.Add(   2353, 'Mozambique - D',
           34.36, -16.94, 40.90, -10.42
         );
  _cs.Add(   2780, 'Malaysia - East Malaysia - offshore SCS',
           109.31, 1.56, 117.31, 7.67
         );
  _cs.Add(   2785, 'Libya - Murzuq and En Naga',
           18.37, 27.32, 18.72, 27.67
         );
  _cs.Add(   2787, 'Morocco - south of 31.5'#$00b0'N',
           -13.24, 27.66, -3.59, 31.51
         );
  _cs.Add(   2967, 'Mauritania - north coast',
           -17.08, 19.37, -15.88, 21.34
         );
  _cs.Add(   2968, 'Mauritania - central coast',
           -16.57, 16.81, -15.59, 19.41
         );
  _cs.Add(   2969, 'Mauritania - east of 6'#$00b0'W',
           -6.00, 15.49, -4.80, 25.74
         );
  _cs.Add(   2970, 'Mauritania - 12'#$00b0'W to 6'#$00b0'W',
           -12.00, 14.75, -6.00, 27.30
         );
  _cs.Add(   2971, 'Mauritania - west of 12'#$00b0'W onshore',
           -17.08, 14.72, -12.00, 23.46
         );
  _cs.Add(   2972, 'Mauritania - Nouakchutt',
           -16.11, 17.89, -15.83, 18.25
         );
  _cs.Add(   3108, 'Micronesia - Yap Islands',
           137.99, 9.39, 138.27, 9.69
         );
  _cs.Add(   3142, 'Libya - Sirte NC192',
           21.25, 27.50, 21.59, 28.07
         );
  _cs.Add(   3192, 'Micronesia - Kosrae (Kusaie)',
           162.85, 5.21, 163.10, 5.43
         );
  _cs.Add(   3202, 'Midway Islands - Sand and Eastern Islands',
           -177.45, 28.13, -177.31, 28.28
         );
  _cs.Add(   3209, 'Mauritius - mainland',
           57.25, -20.57, 57.85, -19.94
         );
  _cs.Add(   3271, 'Libya - onshore',
           9.31, 19.50, 25.21, 33.23
         );
  _cs.Add(   3272, 'Lithuania - onshore',
           20.86, 53.89, 26.82, 56.45
         );
  _cs.Add(   3273, 'Madagascar - onshore',
           43.18, -25.64, 50.56, -11.89
         );
  _cs.Add(   3274, 'Maldives - onshore',
           72.81, -0.69, 73.69, 7.08
         );
  _cs.Add(   3275, 'Malta - onshore',
           14.27, 35.74, 14.63, 36.05
         );
  _cs.Add(   3276, 'Martinique - onshore',
           -61.29, 14.35, -60.76, 14.93
         );
  _cs.Add(   3277, 'Mauritania - onshore',
           -17.08, 14.72, -4.80, 27.30
         );
  _cs.Add(   3278, 'Mexico - onshore',
           -118.47, 14.51, -86.68, 32.72
         );
  _cs.Add(   3279, 'Montserrat - onshore',
           -62.29, 16.62, -62.08, 16.87
         );
  _cs.Add(   1174, 'New Caledonia',
           156.25, -26.45, 174.28, -14.83
         );
  _cs.Add(   1175, 'New Zealand',
           160.60, -55.95, -171.20, -25.88
         );
  _cs.Add(   1275, 'Netherlands - onshore',
           3.20, 50.75, 7.22, 53.70
         );
  _cs.Add(   1315, 'Mozambique - west - Tete province',
           30.21, -17.76, 35.37, -14.01
         );
  _cs.Add(   1454, 'Namibia - Walvis Bay',
           14.35, -23.15, 14.60, -22.68
         );
  _cs.Add(   1500, 'New Zealand - North Island',
           171.99, -41.67, 178.63, -34.10
         );
  _cs.Add(   1501, 'New Zealand - South Island',
           166.37, -46.86, 174.46, -40.44
         );
  _cs.Add(   1502, 'New Zealand - offshore 162'#$00b0'E to168'#$00b0'E',
           162.00, -55.89, 168.00, -39.68
         );
  _cs.Add(   1503, 'New Zealand - offshore 168'#$00b0'E to 174'#$00b0'E',
           168.00, -55.95, 174.00, -30.78
         );
  _cs.Add(   1504, 'New Zealand - offshore 174'#$00b0'E to 180'#$00b0'E',
           174.00, -54.32, 180.00, -26.42
         );
  _cs.Add(   1630, 'Netherlands - offshore',
           2.53, 51.45, 6.41, 55.77
         );
  _cs.Add(   1664, 'Myanmar (Burma) - onshore west of 96'#$00b0'E',
           92.20, 15.66, 96.01, 27.14
         );
  _cs.Add(   1822, 'Namibia - offshore',
           8.24, -30.64, 16.46, -17.24
         );
  _cs.Add(   1838, 'Namibia - west of 12'#$00b0'E',
           11.66, -18.53, 12.00, -17.15
         );
  _cs.Add(   1839, 'Namibia - 12'#$00b0'E to 14'#$00b0'E',
           12.00, -21.90, 14.01, -16.95
         );
  _cs.Add(   1840, 'Namibia - 14'#$00b0'E to 16'#$00b0'E',
           14.00, -28.30, 16.00, -17.38
         );
  _cs.Add(   1841, 'Namibia - 16'#$00b0'E to 18'#$00b0'E',
           15.99, -28.83, 18.01, -17.38
         );
  _cs.Add(   1842, 'Namibia - 18'#$00b0'E to 20'#$00b0'E',
           18.00, -28.97, 20.00, -17.38
         );
  _cs.Add(   1843, 'Namibia - 20'#$00b0'E to 22'#$00b0'E',
           19.99, -22.01, 22.00, -17.85
         );
  _cs.Add(   1844, 'Namibia - 22'#$00b0'E to 24'#$00b0'E',
           21.99, -18.49, 24.00, -17.52
         );
  _cs.Add(   1845, 'Namibia - east of 24'#$00b0'E',
           24.00, -18.18, 25.27, -17.47
         );
  _cs.Add(   2293, 'Myanmar (Burma) - onshore south of 15'#$00b0'N',
           97.74, 9.78, 99.66, 15.00
         );
  _cs.Add(   2361, 'Myanmar (Burma) - Moattama area',
           93.94, 9.48, 99.66, 17.87
         );
  _cs.Add(   2813, 'New Caledonia - Ouvea',
           166.44, -20.77, 166.71, -20.34
         );
  _cs.Add(   2814, 'New Caledonia - Lifou',
           166.98, -21.24, 167.52, -20.62
         );
  _cs.Add(   2819, 'New Caledonia - Mare',
           167.75, -21.71, 168.19, -21.32
         );
  _cs.Add(   2820, 'New Caledonia - Ile des Pins',
           167.36, -22.73, 167.61, -22.49
         );
  _cs.Add(   2821, 'New Caledonia - Belep',
           163.54, -19.85, 163.75, -19.50
         );
  _cs.Add(   2822, 'New Caledonia - Grande Terre',
           163.92, -22.45, 167.09, -20.03
         );
  _cs.Add(   2823, 'New Caledonia - Grande Terre - Noumea',
           166.35, -22.37, 166.54, -22.19
         );
  _cs.Add(   2889, 'New Zealand - Chatham Islands group',
           -177.25, -44.64, -175.54, -43.30
         );
  _cs.Add(   3282, 'Myanmar (Burma) - onshore',
           92.20, 9.78, 101.17, 28.55
         );
  _cs.Add(   3283, 'Namibia - onshore',
           11.66, -28.97, 25.27, -16.95
         );
  _cs.Add(   3284, 'Netherlands - onshore',
           3.33, 50.78, 7.30, 53.60
         )         .MarkDeprecated( True );
  _cs.Add(   3285, 'New Zealand - onshore and nearshore',
           165.87, -47.65, 179.27, -33.89
         );
  _cs.Add(   3344, 'New Zealand - South and Stewart Islands',
           166.37, -47.33, 174.46, -40.44
         );
  _cs.Add(   3430, 'New Caledonia - Belep, Grande Terre, Ile des Pins, Loyalty Islands',
           163.54, -22.73, 168.19, -19.50
         );
  _cs.Add(   3431, 'New Caledonia - west of 162'#$00b0'E',
           156.25, -26.03, 162.01, -15.34
         );
  _cs.Add(   3432, 'New Caledonia - 162'#$00b0'E to 168'#$00b0'E',
           162.00, -26.45, 168.00, -14.83
         );
  _cs.Add(   3433, 'New Caledonia - east of 168'#$00b0'E',
           168.00, -25.95, 174.28, -19.75
         );
  _cs.Add(   3434, 'New Caledonia - Mare - west of 168'#$00b0'E',
           167.75, -21.71, 168.00, -21.32
         );
  _cs.Add(   3435, 'New Caledonia - Mare - east of 168'#$00b0'E',
           168.00, -21.71, 168.19, -21.35
         );
  _cs.Add(   3508, 'New Zealand - offshore Pacific Ocean, Southern Ocean',
           155.00, -60.00, -170.00, -25.00
         );
  _cs.Add(   3554, 'New Zealand - Snares and Auckland Islands',
           165.55, -51.13, 166.93, -47.80
         );
  _cs.Add(   3555, 'New Zealand - Campbell Island',
           168.65, -52.83, 169.60, -52.26
         );
  _cs.Add(   3556, 'New Zealand - Antipodes and Bounty Islands',
           178.40, -49.92, 179.37, -47.54
         );
  _cs.Add(   3557, 'New Zealand - Raoul and Kermadec Islands',
           -179.07, -31.56, -177.62, -29.03
         );
  _cs.Add(   3593, 'New Zealand - offshore',
           160.60, -55.95, -171.20, -25.88
         );
  _cs.Add(   3762, 'New Zealand - North Island - One Tree vcrs',
           172.61, -36.41, 174.83, -34.36
         );
  _cs.Add(   3764, 'New Zealand - North Island - Auckland vcrs',
           174.00, -37.67, 176.17, -36.12
         );
  _cs.Add(   3768, 'New Zealand - North Island - Moturiki vcrs',
           174.57, -40.59, 177.26, -37.52
         );
  _cs.Add(   3769, 'New Zealand - North Island - Taranaki vcrs',
           173.68, -39.92, 174.95, -38.41
         );
  _cs.Add(   3771, 'New Zealand - North Island - Gisborne vcrs',
           176.41, -39.04, 178.63, -37.49
         );
  _cs.Add(   3772, 'New Zealand - North Island - Hawkes Bay mc Napier vcrs',
           175.80, -40.57, 178.07, -38.87
         );
  _cs.Add(   3773, 'New Zealand - North Island - Wellington vcrs',
           174.52, -41.67, 176.56, -40.12
         );
  _cs.Add(   3774, 'New Zealand - North Island - Wellington mc',
           174.52, -41.50, 175.36, -40.91
         );
  _cs.Add(   3775, 'New Zealand - North Island - Wairarapa mc',
           175.01, -41.67, 176.55, -40.29
         );
  _cs.Add(   3776, 'New Zealand - North Island - Wanganui mc',
           174.40, -40.97, 176.27, -39.46
         );
  _cs.Add(   3777, 'New Zealand - North Island - Taranaki mc',
           173.68, -39.78, 175.44, -38.40
         );
  _cs.Add(   3778, 'New Zealand - North Island - Tuhirangi mc',
           174.88, -39.55, 176.33, -38.87
         );
  _cs.Add(   3779, 'New Zealand - North Island - Bay of Plenty mc',
           175.75, -39.13, 177.23, -37.22
         );
  _cs.Add(   3780, 'New Zealand - North Island - Poverty Bay mc',
           176.73, -39.04, 178.63, -37.49
         );
  _cs.Add(   3781, 'New Zealand - North Island - Mount Eden mc',
           171.99, -39.01, 176.12, -34.10
         );
  _cs.Add(   3782, 'New Zealand - South Island - Collingwood mc',
           172.16, -41.22, 173.13, -40.44
         );
  _cs.Add(   3783, 'New Zealand - South Island - Karamea mc',
           171.96, -41.49, 172.70, -40.75
         );
  _cs.Add(   3784, 'New Zealand - South Island - Nelson mc',
           172.40, -42.18, 174.08, -40.66
         );
  _cs.Add(   3785, 'New Zealand - South Island - Marlborough mc',
           172.95, -42.65, 174.46, -40.85
         );
  _cs.Add(   3786, 'New Zealand - South Island - Buller mc',
           171.27, -42.19, 172.41, -41.42
         );
  _cs.Add(   3787, 'New Zealand - South Island - Grey mc',
           171.15, -42.74, 172.75, -41.50
         );
end;

procedure prepare_20;
begin
  _cs.Add(   3788, 'New Zealand - South Island - Amuri mc',
           171.88, -42.95, 173.55, -42.09
         );
  _cs.Add(   3789, 'New Zealand - South Island - Hokitika mc',
           170.39, -43.23, 171.89, -42.41
         );
  _cs.Add(   3790, 'New Zealand - South Island - Mount Pleasant mc',
           171.11, -43.96, 173.38, -42.69
         );
  _cs.Add(   3791, 'New Zealand - South Island - Okarito mc',
           169.21, -43.85, 170.89, -43.00
         );
  _cs.Add(   3792, 'New Zealand - South Island - Gawler mc',
           170.68, -44.25, 172.26, -43.13
         );
  _cs.Add(   3794, 'New Zealand - South Island - Jacksons Bay mc',
           168.02, -44.40, 170.01, -43.67
         );
  _cs.Add(   3795, 'New Zealand - South Island - Lindis Peak mc',
           168.62, -45.40, 170.24, -43.71
         );
  _cs.Add(   3796, 'New Zealand - South Island - Observation Point mc',
           169.77, -45.82, 171.24, -44.61
         );
  _cs.Add(   3797, 'New Zealand - South Island - Mount Nicholas mc',
           167.72, -45.58, 169.11, -44.29
         );
  _cs.Add(   3798, 'New Zealand - South Island - North Taieri mc',
           168.64, -46.73, 170.87, -45.23
         );
  _cs.Add(   3799, 'New Zealand - South Island - Mount York mc',
           166.37, -46.33, 168.21, -44.53
         );
  _cs.Add(   3800, 'New Zealand - South and Stewart Islands - Bluff mc',
           167.29, -47.33, 168.97, -45.33
         );
  _cs.Add(   3801, 'New Zealand - South Island - Bluff vcrs',
           168.01, -46.71, 168.86, -46.26
         );
  _cs.Add(   3802, 'New Zealand - South Island - Nelson vcrs',
           171.82, -42.44, 174.46, -40.44
         );
  _cs.Add(   3803, 'New Zealand - South Island - Dunedin vcrs',
           167.73, -46.40, 171.28, -43.82
         );
  _cs.Add(   3804, 'New Zealand - South Island - Lyttleton vcrs',
           168.95, -44.92, 173.77, -41.60
         );
  _cs.Add(   3806, 'New Zealand - South Island - Dunedin-Bluff vcrs',
           166.37, -46.73, 169.95, -44.52
         );
  _cs.Add(   3818, 'New Zealand - North Island - Tararu vcrs',
           175.44, -37.21, 175.99, -36.78
         );
  _cs.Add(   3894, 'New Zealand - Chatham Island onshore',
           -176.92, -44.18, -176.20, -43.67
         );
  _cs.Add(   3929, 'Mozambique - west of 36'#$00b0'E',
           30.21, -27.58, 36.00, -11.41
         );
  _cs.Add(   3970, 'New Zealand - nearshore west of 168'#$00b0'E',
           165.87, -47.65, 168.00, -42.59
         );
  _cs.Add(   3971, 'New Zealand - nearshore 168'#$00b0'E to 174'#$00b0'E',
           168.00, -47.64, 174.00, -33.89
         );
  _cs.Add(   3972, 'New Zealand - nearshore east of 174'#$00b0'E',
           174.00, -44.13, 179.27, -34.24
         );
  _cs.Add(   3973, 'New Zealand - onshore',
           166.37, -47.33, 178.63, -34.10
         );
  _cs.Add(   3992, 'New Zealand - offshore 180'#$00b0'W to 174'#$00b0'W',
           -180.00, -52.97, -174.00, -25.88
         );
  _cs.Add(   1148, 'North Macedonia',
           20.45, 40.85, 23.04, 42.36
         );
  _cs.Add(   1176, 'Nicaragua',
           -89.43, 9.75, -79.76, 16.26
         );
  _cs.Add(   1177, 'Niger',
           0.16, 11.69, 16.00, 23.53
         );
  _cs.Add(   1178, 'Nigeria',
           2.66, 1.92, 14.65, 13.90
         );
  _cs.Add(   1179, 'Niue',
           -172.01, -22.49, -166.31, -16.58
         );
  _cs.Add(   1180, 'Norfolk Island',
           163.36, -32.48, 173.35, -25.61
         );
  _cs.Add(   1181, 'Northern Mariana Islands',
           141.33, 12.38, 149.55, 23.90
         );
  _cs.Add(   1182, 'Norway',
           -3.34, 56.08, 38.00, 84.72
         );
  _cs.Add(   1325, 'North America - Canada and USA (CONUS, Alaska mainland)',
           -172.54, 23.81, -47.74, 86.46
         );
  _cs.Add(   1349, 'North America - NAD27',
           167.65, 7.15, -47.74, 83.17
         );
  _cs.Add(   1350, 'North America - NAD83',
           167.65, 14.92, -47.74, 86.46
         );
  _cs.Add(   1352, 'Norway - onshore',
           4.68, 57.93, 31.22, 71.21
         );
  _cs.Add(   1713, 'Nigeria - east of 10.5'#$00b0'E',
           10.49, 6.43, 14.65, 13.72
         );
  _cs.Add(   1714, 'Nigeria - 6.5'#$00b0'E to 10.5'#$00b0'E',
           6.50, 3.57, 10.51, 13.53
         );
  _cs.Add(   1715, 'Nigeria - west of 6.5'#$00b0'E',
           2.69, 3.57, 6.50, 13.90
         );
  _cs.Add(   1716, 'Nigeria - offshore deep water - west of 6'#$00b0'E',
           2.66, 1.92, 6.00, 6.14
         );
  _cs.Add(   1717, 'Nigeria - offshore deep water',
           2.66, 1.92, 7.82, 6.14
         );
  _cs.Add(   1730, 'North America - NAVD88',
           -168.00, 25.00, -52.00, 58.00
         )         .MarkDeprecated( True );
  _cs.Add(   2137, 'North America - 144'#$00b0'W to 138'#$00b0'W and NAD27 by country',
           -144.00, 53.47, -138.00, 73.59
         );
  _cs.Add(   2138, 'North America - 138'#$00b0'W to 132'#$00b0'W and NAD27 by country - onshore',
           -138.00, 52.58, -132.00, 73.04
         );
  _cs.Add(   2139, 'North America - 132'#$00b0'W to 126'#$00b0'W and NAD27 by country - onshore',
           -132.00, 49.18, -126.00, 72.03
         );
  _cs.Add(   2140, 'North America - 126'#$00b0'W to 120'#$00b0'W and NAD27 by country - onshore',
           -126.00, 34.40, -120.00, 77.13
         );
  _cs.Add(   2141, 'North America - 120'#$00b0'W to 114'#$00b0'W and NAD27 by country - onshore',
           -120.00, 26.93, -114.00, 78.13
         );
  _cs.Add(   2142, 'North America - 114'#$00b0'W to 108'#$00b0'W and NAD27 by country',
           -114.00, 18.66, -108.00, 78.81
         );
  _cs.Add(   2143, 'North America - 108'#$00b0'W to 102'#$00b0'W and NAD27 by country',
           -108.00, 17.86, -102.00, 79.42
         );
  _cs.Add(   2144, 'North America - 102'#$00b0'W to 96'#$00b0'W and NAD27 by country',
           -102.00, 15.59, -96.00, 80.74
         );
  _cs.Add(   2145, 'North America - 96'#$00b0'W to 90'#$00b0'W and NAD27 by country',
           -96.00, 13.63, -90.00, 81.96
         );
  _cs.Add(   2146, 'North America - 90'#$00b0'W to 84'#$00b0'W and NAD27 by country',
           -90.01, 9.27, -84.00, 82.54
         );
  _cs.Add(   2147, 'North America - 84'#$00b0'W to 78'#$00b0'W and NAD27 by country',
           -84.00, 7.98, -78.00, 83.03
         );
  _cs.Add(   2148, 'North America - 78'#$00b0'W to 72'#$00b0'W and NAD27 by country',
           -78.00, 18.83, -72.00, 83.16
         );
  _cs.Add(   2149, 'North America - 72'#$00b0'W to 66'#$00b0'W and NAD27 by country',
           -72.00, 33.61, -66.00, 83.17
         );
  _cs.Add(   2150, 'North America - 66'#$00b0'W to 60'#$00b0'W and NAD27 by country',
           -66.00, 38.21, -60.00, 82.97
         );
  _cs.Add(   2331, 'Norway - offshore north of 65'#$00b0'N',
           -0.50, 65.00, 32.02, 72.00
         )         .MarkDeprecated( True );
  _cs.Add(   2332, 'Norway - offshore north of 65'#$00b0'N; Svalbard',
           -3.35, 65.00, 38.00, 84.73
         );
  _cs.Add(   2333, 'Norway - offshore 62'#$00b0'N to 65'#$00b0'N and west of 5'#$00b0'E',
           -0.49, 62.00, 5.00, 65.00
         );
  _cs.Add(   2334, 'Norway - North Sea - offshore south of 62'#$00b0'N',
           1.37, 56.08, 11.14, 62.00
         );
  _cs.Add(   2371, 'Nigeria - south',
           4.35, 4.22, 9.45, 6.95
         );
  _cs.Add(   2601, 'Norway - offshore north of 62'#$00b0'N; Svalbard',
           -3.35, 62.00, 38.00, 84.73
         );
  _cs.Add(   2771, 'Niger - southeast',
           7.81, 12.80, 14.90, 16.70
         );
  _cs.Add(   2981, 'Nigeria - offshore',
           2.66, 1.92, 8.49, 6.38
         );
  _cs.Add(   3113, 'Nigeria - OML 58',
           6.53, 5.05, 6.84, 5.36
         );
  _cs.Add(   3114, 'North America - 96'#$00b0'W to 90'#$00b0'W and NAD83 by country',
           -96.00, 25.61, -90.00, 84.00
         );
  _cs.Add(   3115, 'North America - 90'#$00b0'W to 84'#$00b0'W and NAD83 by country',
           -90.00, 23.97, -84.00, 84.00
         );
  _cs.Add(   3116, 'North America - 84'#$00b0'W to 78'#$00b0'W and NAD83 by country',
           -84.00, 23.81, -78.00, 84.00
         );
  _cs.Add(   3117, 'North America - 78'#$00b0'W to 72'#$00b0'W and NAD83 by country',
           -78.00, 28.28, -72.00, 84.00
         );
  _cs.Add(   3206, 'North America - Bahamas and USA - Florida - onshore',
           -82.33, 20.86, -72.68, 30.83
         );
  _cs.Add(   3286, 'Nicaragua - onshore',
           -87.74, 10.70, -83.08, 15.03
         );
  _cs.Add(   3287, 'Nigeria - onshore',
           2.69, 4.22, 14.65, 13.90
         );
  _cs.Add(   3338, 'New Zealand - Stewart Island',
           167.29, -47.33, 168.34, -46.63
         );
  _cs.Add(   3404, 'North America - 120'#$00b0'W to 114'#$00b0'W and NAD83 by country',
           -120.00, 30.88, -114.00, 83.50
         );
  _cs.Add(   3405, 'North America - 114'#$00b0'W to 108'#$00b0'W and NAD83 by country',
           -114.00, 31.33, -108.00, 84.00
         );
  _cs.Add(   3406, 'North America - 108'#$00b0'W to 102'#$00b0'W and NAD83 by country',
           -108.00, 28.98, -102.00, 84.00
         );
  _cs.Add(   3407, 'North America - 102'#$00b0'W to 96'#$00b0'W and NAD83 by country',
           -102.00, 25.83, -96.00, 84.00
         );
  _cs.Add(   3419, 'North America - 72'#$00b0'W to 66'#$00b0'W and NAD83 by country',
           -72.00, 14.92, -66.00, 84.00
         );
  _cs.Add(   3420, 'North America - 66'#$00b0'W to 60'#$00b0'W and NAD83 by country',
           -66.00, 15.63, -60.00, 84.00
         );
  _cs.Add(   3467, 'North America - Great Lakes basin',
           -93.17, 40.99, -74.47, 50.74
         );
  _cs.Add(   3468, 'North America - Great Lakes basin and St Lawrence Seaway',
           -93.17, 40.99, -54.75, 52.22
         );
  _cs.Add(   3590, 'Nigeria - 4'#$00b0'N to 5'#$00b0'N, 6'#$00b0'E to 8'#$00b0'E',
           5.99, 3.99, 8.01, 5.01
         );
  _cs.Add(   3650, 'Norway - onshore - 10'#$00b0'E to 11'#$00b0'E',
           10.00, 58.90, 11.00, 65.04
         );
  _cs.Add(   3651, 'Norway - onshore - 11'#$00b0'E to 12'#$00b0'E',
           11.00, 58.88, 12.00, 65.76
         );
  _cs.Add(   3653, 'Norway - onshore - 12'#$00b0'E to 13'#$00b0'E',
           12.00, 59.88, 13.00, 68.15
         );
  _cs.Add(   3654, 'Norway - onshore - 13'#$00b0'E to 14'#$00b0'E',
           13.00, 64.01, 14.00, 68.37
         );
  _cs.Add(   3655, 'Norway - onshore - 14'#$00b0'E to 15'#$00b0'E',
           14.00, 64.03, 15.00, 69.05
         );
  _cs.Add(   3656, 'Norway - onshore - 15'#$00b0'E to 16'#$00b0'E',
           15.00, 66.14, 16.00, 69.35
         );
  _cs.Add(   3657, 'Norway - onshore - 16'#$00b0'E to 17'#$00b0'E',
           16.00, 66.88, 17.00, 69.45
         );
  _cs.Add(   3658, 'Norway - onshore - 17'#$00b0'E to 18'#$00b0'E',
           17.00, 67.94, 18.00, 69.68
         );
  _cs.Add(   3660, 'Norway - onshore - 18'#$00b0'E to 19'#$00b0'E',
           18.00, 68.04, 19.00, 70.27
         );
  _cs.Add(   3661, 'Norway - onshore - 19'#$00b0'E to 20'#$00b0'E',
           19.00, 68.33, 20.00, 70.34
         );
  _cs.Add(   3662, 'Norway - onshore - 20'#$00b0'E to 21'#$00b0'E',
           20.00, 68.37, 21.00, 70.29
         );
  _cs.Add(   3663, 'Norway - onshore - 21'#$00b0'E to 22'#$00b0'E',
           21.00, 69.03, 22.00, 70.71
         );
  _cs.Add(   3665, 'Norway - onshore - 22'#$00b0'E to 23'#$00b0'E',
           22.00, 68.69, 23.00, 70.81
         );
  _cs.Add(   3667, 'Norway - onshore - 23'#$00b0'E to 24'#$00b0'E',
           23.00, 68.62, 24.00, 71.08
         );
  _cs.Add(   3668, 'Norway - onshore - 24'#$00b0'E to 25'#$00b0'E',
           24.00, 68.58, 25.00, 71.16
         );
  _cs.Add(   3669, 'Norway - onshore - 25'#$00b0'E to 26'#$00b0'E',
           25.00, 68.59, 26.00, 71.21
         );
  _cs.Add(   3671, 'Norway - onshore - 26'#$00b0'E to 27'#$00b0'E',
           26.00, 69.71, 27.00, 71.17
         );
end;

procedure prepare_21;
begin
  _cs.Add(   3793, 'New Zealand - South Island - Timaru mc',
           169.82, -44.98, 171.55, -43.35
         );
  _cs.Add(   3812, 'Nigeria - offshore deep water - east of 6'#$00b0'E',
           6.00, 2.61, 7.82, 3.68
         );
  _cs.Add(   3813, 'Nigeria - offshore blocks OPL 209, 219 and 220',
           4.01, 3.25, 6.96, 5.54
         );
  _cs.Add(   3814, 'Nigeria - offshore blocks OML 99-102 and OPL 222 and 223',
           7.16, 3.25, 8.25, 4.51
         );
  _cs.Add(   3815, 'Nigeria - offshore blocks OPL 209-213 and 316',
           3.83, 4.22, 5.17, 6.31
         );
  _cs.Add(   3816, 'Nigeria - offshore blocks OPL 217-223',
           5.58, 3.24, 8.00, 3.86
         );
  _cs.Add(   3817, 'Nigeria - offshore blocks OPL 210, 213, 217 and 218',
           4.41, 3.24, 6.29, 5.54
         );
  _cs.Add(   3819, 'Nigeria - offshore blocks OPL 215 and 221',
           5.02, 3.25, 7.31, 4.23
         );
  _cs.Add(   3824, 'Nigeria - Gongola Basin',
           9.41, 8.78, 12.13, 11.63
         );
  _cs.Add(   3844, 'Nicaragua - onshore north of 12'#$00b0'48''N',
           -87.74, 12.80, -83.08, 15.03
         );
  _cs.Add(   3847, 'Nicaragua - onshore south of 12'#$00b0'48''N',
           -87.63, 10.70, -83.42, 12.80
         );
  _cs.Add(   3864, 'North America - 126'#$00b0'W to 120'#$00b0'W and NAD83 by country',
           -126.00, 30.54, -119.99, 81.80
         );
  _cs.Add(   3866, 'North America - 132'#$00b0'W to 126'#$00b0'W and NAD83 by country',
           -132.00, 35.38, -126.00, 80.93
         );
  _cs.Add(   3867, 'North America - 138'#$00b0'W to 132'#$00b0'W and NAD83 by country',
           -138.00, 48.06, -132.00, 79.42
         );
  _cs.Add(   3872, 'North America - 144'#$00b0'W to 138'#$00b0'W and NAD83 by country',
           -144.00, 52.05, -137.99, 73.59
         );
  _cs.Add(   4067, 'Norway - onshore - 12'#$00b0'E to 18'#$00b0'E',
           12.00, 59.88, 18.01, 69.68
         );
  _cs.Add(   4068, 'Norway - onshore - 18'#$00b0'E to 24'#$00b0'E',
           18.00, 68.04, 24.01, 71.08
         );
  _cs.Add(   4069, 'Norway - onshore - 24'#$00b0'E to 30'#$00b0'E',
           24.00, 68.58, 30.00, 71.21
         );
  _cs.Add(   4127, 'Nigeria - OML 124',
           6.72, 5.56, 6.97, 5.74
         );
  _cs.Add(   4161, 'North America - Mexico and USA - onshore',
           172.42, 14.51, -66.91, 71.40
         );
  _cs.Add(   4171, 'Northern Mariana Islands - Rota, Saipan and Tinian',
           145.06, 14.06, 145.89, 15.35
         );
  _cs.Add(   4179, 'Norfolk Island - east of 168'#$00b0'E',
           168.00, -32.48, 173.35, -25.61
         );
  _cs.Add(   4521, 'Northern Mariana Islands - onshore',
           144.83, 14.06, 146.12, 20.61
         );
  _cs.Add(   4544, 'North America - Canada, US (Conus+AK), PRVI',
           167.65, 14.92, -47.74, 86.46
         );
  _cs.Add(   4656, 'Norway - inshore and nearshore',
           4.08, 57.75, 31.77, 71.38
         );
  _cs.Add(   1183, 'Oman',
           51.99, 14.33, 63.38, 26.74
         );
  _cs.Add(   1184, 'Pakistan',
           60.86, 21.05, 77.83, 37.07
         );
  _cs.Add(   1185, 'Palau',
           129.48, 1.64, 136.98, 11.45
         );
  _cs.Add(   1186, 'Panama',
           -84.32, 5.00, -77.04, 12.51
         );
  _cs.Add(   1187, 'Papua New Guinea',
           139.20, -14.75, 162.81, 2.58
         );
  _cs.Add(   1188, 'Paraguay',
           -62.65, -27.59, -54.24, -19.29
         );
  _cs.Add(   1189, 'Peru',
           -84.68, -21.05, -68.67, -0.03
         );
  _cs.Add(   1190, 'Philippines',
           116.04, 3.00, 129.95, 22.18
         );
  _cs.Add(   1191, 'Pitcairn',
           -133.43, -28.41, -121.11, -20.58
         );
  _cs.Add(   1192, 'Poland',
           14.14, 49.00, 24.15, 55.93
         );
  _cs.Add(   1263, 'Not specified',
           -180.00, -90.00, 180.00, 90.00
         );
  _cs.Add(   1521, 'Poland - 16.5'#$00b0'E to 19.5'#$00b0'E',
           16.50, 49.39, 19.50, 55.93
         );
  _cs.Add(   1522, 'Poland - 19.5'#$00b0'E to 22.5'#$00b0'E',
           19.50, 49.09, 22.50, 54.55
         );
  _cs.Add(   1523, 'Poland - east of 22.5'#$00b0'E',
           22.50, 49.00, 24.15, 54.41
         );
  _cs.Add(   1544, 'Oman - onshore west of 54'#$00b0'E',
           51.99, 16.59, 54.00, 19.67
         );
  _cs.Add(   1545, 'Oman - onshore east of 54'#$00b0'E',
           54.00, 16.89, 59.91, 26.58
         );
  _cs.Add(   1668, 'Pakistan - north of 35'#$00b0'35''N',
           71.18, 35.58, 77.01, 37.07
         );
  _cs.Add(   1685, 'Pakistan - 28'#$00b0'N to 35'#$00b0'35''N',
           60.86, 28.00, 77.83, 35.59
         );
  _cs.Add(   1686, 'Pakistan - onshore south of 28'#$00b0'N',
           61.59, 23.64, 71.91, 28.01
         );
  _cs.Add(   1687, 'Pakistan - onshore west of 66'#$00b0'E',
           60.86, 24.98, 66.01, 29.87
         );
  _cs.Add(   1688, 'Pakistan - onshore 66'#$00b0'E to 72'#$00b0'E',
           66.00, 23.64, 72.01, 36.56
         );
  _cs.Add(   1689, 'Pakistan - east of 72'#$00b0'E',
           72.00, 28.21, 77.83, 37.07
         );
  _cs.Add(   1698, 'Philippines - zone I',
           116.04, 6.21, 118.00, 18.64
         );
  _cs.Add(   1699, 'Philippines - zone II',
           118.00, 3.02, 120.07, 20.42
         );
  _cs.Add(   1700, 'Philippines - zone III',
           119.70, 3.00, 122.21, 21.62
         );
  _cs.Add(   1701, 'Philippines - zone IV',
           121.74, 3.44, 124.29, 22.18
         );
  _cs.Add(   1702, 'Philippines - zone V',
           123.73, 4.76, 126.65, 21.97
         );
  _cs.Add(   1741, 'Norway - zone I',
           4.68, 57.93, 7.23, 63.06
         );
  _cs.Add(   1742, 'Norway - zone II',
           7.22, 57.95, 9.56, 63.87
         );
  _cs.Add(   1743, 'Norway - zone III',
           9.55, 58.84, 11.98, 65.76
         );
  _cs.Add(   1744, 'Norway - zone IV',
           11.97, 59.88, 15.06, 69.06
         );
  _cs.Add(   1745, 'Norway - zone V',
           15.05, 66.15, 18.89, 70.19
         );
  _cs.Add(   1746, 'Norway - zone VI',
           18.88, 68.33, 22.89, 70.81
         );
  _cs.Add(   1747, 'Norway - zone VII',
           22.88, 68.58, 26.98, 71.21
         );
  _cs.Add(   1748, 'Norway - zone VIII',
           26.97, 69.02, 31.22, 71.17
         );
  _cs.Add(   1751, 'Peru - east of 73'#$00b0'W',
           -73.00, -18.39, -68.67, -2.14
         );
  _cs.Add(   1752, 'Peru - 79'#$00b0'W to 73'#$00b0'W',
           -79.00, -16.57, -73.00, -0.03
         );
  _cs.Add(   1753, 'Peru - west of 79'#$00b0'W',
           -81.41, -8.32, -79.00, -3.38
         );
  _cs.Add(   2364, 'Philippines - onshore excluding Mindanao',
           116.89, 7.75, 125.88, 19.45
         );
  _cs.Add(   2365, 'Philippines - Mindanao onshore',
           119.76, 4.99, 126.65, 10.52
         );
  _cs.Add(   2385, 'Panama - Canal Zone',
           -80.07, 8.82, -79.46, 9.45
         );
  _cs.Add(   2391, 'Oman - Masirah Island',
           58.58, 20.12, 59.01, 20.74
         );
  _cs.Add(   2404, 'Oman - block 4',
           56.50, 19.58, 59.02, 21.17
         );
  _cs.Add(   2602, 'Palestine Territory',
           34.17, 31.21, 35.58, 32.55
         );
  _cs.Add(   2982, 'Pakistan - Karachi',
           66.83, 24.69, 68.00, 25.76
         );
  _cs.Add(   2983, 'Pakistan - East Sind',
           68.27, 24.16, 71.14, 28.61
         );
  _cs.Add(   2984, 'Pakistan - Badin and Mehran',
           67.74, 24.00, 69.87, 25.64
         );
  _cs.Add(   2985, 'Pakistan - offshore Indus',
           64.00, 21.05, 68.24, 25.39
         );
  _cs.Add(   3169, 'Poland - 18'#$00b0'E to 24'#$00b0'E',
           18.00, 49.03, 24.00, 55.95
         )         .MarkDeprecated( True );
  _cs.Add(   3172, 'Pacific Ocean',
           98.69, -60.00, -68.00, 66.67
         );
  _cs.Add(   3191, 'Pacific - Marshall Islands, Wake - onshore',
           162.27, 8.66, 167.82, 19.38
         );
  _cs.Add(   3208, 'Pitcairn - Pitcairn Island',
           -130.16, -25.14, -130.01, -25.00
         );
  _cs.Add(   3288, 'Oman - onshore',
           51.99, 16.59, 59.91, 26.58
         );
  _cs.Add(   3289, 'Pakistan - onshore',
           60.86, 23.64, 77.83, 37.07
         );
  _cs.Add(   3290, 'Panama - onshore',
           -83.04, 7.15, -77.19, 9.68
         );
  _cs.Add(   3291, 'Papua New Guinea - onshore',
           140.85, -11.70, 156.02, -1.30
         );
  _cs.Add(   3292, 'Peru - onshore',
           -81.41, -18.35, -68.67, -0.03
         );
  _cs.Add(   3589, 'Pakistan - Gambat',
           68.24, 25.88, 69.30, 27.67
         );
  _cs.Add(   3636, 'Norway - onshore - west of 6'#$00b0'E',
           4.68, 58.32, 6.00, 62.49
         );
  _cs.Add(   3639, 'Norway - onshore - 6'#$00b0'E to 7'#$00b0'E',
           6.00, 57.93, 7.00, 63.02
         );
  _cs.Add(   3647, 'Norway - onshore - 7'#$00b0'E to 8'#$00b0'E',
           7.00, 57.93, 8.00, 63.52
         );
  _cs.Add(   3648, 'Norway - onshore - 8'#$00b0'E to 9'#$00b0'E',
           8.00, 58.03, 9.00, 63.87
         );
  _cs.Add(   3649, 'Norway - onshore - 9'#$00b0'E to 10'#$00b0'E',
           9.00, 58.52, 10.00, 64.16
         );
  _cs.Add(   3672, 'Norway - onshore - 27'#$00b0'E to 28'#$00b0'E',
           27.00, 69.90, 28.00, 71.17
         );
  _cs.Add(   3673, 'Norway - onshore - 28'#$00b0'E to 29'#$00b0'E',
           28.00, 69.03, 29.00, 71.13
         );
  _cs.Add(   1197, 'Romania',
           20.26, 43.44, 31.41, 48.27
         );
  _cs.Add(   1198, 'Russia',
           18.92, 39.87, -168.97, 85.19
         );
  _cs.Add(   1294, 'Portugal - mainland - onshore',
           -9.56, 36.95, -6.19, 42.16
         );
  _cs.Add(   1301, 'Portugal - Azores C - onshore',
           -28.90, 38.32, -26.97, 39.14
         );
  _cs.Add(   1314, 'Portugal - Madeira archipelago onshore',
           -17.31, 32.35, -16.23, 33.15
         );
  _cs.Add(   1344, 'Portugal - Azores W - onshore',
           -31.34, 39.30, -31.02, 39.77
         );
  _cs.Add(   1345, 'Portugal - Azores E - onshore',
           -25.92, 36.87, -24.62, 37.96
         );
  _cs.Add(   1346, 'Qatar - onshore',
           50.69, 24.55, 51.68, 26.20
         );
  _cs.Add(   1515, 'Poland - zone I',
           18.00, 49.00, 24.15, 52.34
         );
  _cs.Add(   1516, 'Poland - zone II',
           19.00, 51.33, 23.95, 54.51
         );
end;

procedure prepare_22;
begin
  _cs.Add(   1517, 'Poland - zone III',
           14.14, 52.16, 20.00, 54.89
         );
  _cs.Add(   1518, 'Poland - zone IV',
           14.14, 49.39, 19.09, 53.34
         );
  _cs.Add(   1519, 'Poland - zone V',
           18.33, 49.39, 19.67, 51.34
         );
  _cs.Add(   1520, 'Poland - west of 16.5'#$00b0'E',
           14.14, 50.26, 16.50, 55.35
         );
  _cs.Add(   1764, 'Russia - 24'#$00b0'E to 30'#$00b0'E onshore',
           26.61, 55.69, 30.00, 69.47
         );
  _cs.Add(   1765, 'Russia - 30'#$00b0'E to 36'#$00b0'E onshore',
           30.00, 50.34, 36.00, 70.02
         );
  _cs.Add(   1777, 'Russia - 102'#$00b0'E to 108'#$00b0'E onshore',
           102.00, 49.64, 108.00, 79.48
         );
  _cs.Add(   1778, 'Russia - 108'#$00b0'E to 114'#$00b0'E onshore',
           108.00, 49.14, 114.00, 76.81
         );
  _cs.Add(   1779, 'Russia - 114'#$00b0'E to 120'#$00b0'E onshore',
           114.00, 49.51, 120.00, 75.96
         );
  _cs.Add(   1780, 'Russia - 120'#$00b0'E to 126'#$00b0'E onshore',
           120.00, 51.51, 126.00, 74.00
         );
  _cs.Add(   2690, 'Russia - 130.5'#$00b0'E to 133.5'#$00b0'E onshore',
           130.50, 42.25, 133.50, 71.99
         );
  _cs.Add(   2691, 'Russia - 133.5'#$00b0'E to 136.5'#$00b0'E onshore',
           133.50, 42.74, 136.50, 75.90
         );
  _cs.Add(   2692, 'Russia - 136.5'#$00b0'E to 139.5'#$00b0'E onshore',
           136.50, 44.76, 139.50, 76.27
         );
  _cs.Add(   2693, 'Russia - 139.5'#$00b0'E to 142.5'#$00b0'E onshore',
           139.50, 45.84, 142.50, 76.23
         );
  _cs.Add(   2694, 'Russia - 142.5'#$00b0'E to 145.5'#$00b0'E onshore',
           142.50, 43.61, 145.50, 75.98
         );
  _cs.Add(   2695, 'Russia - 145.5'#$00b0'E to 148.5'#$00b0'E onshore',
           145.50, 43.60, 148.50, 76.76
         );
  _cs.Add(   2696, 'Russia - 148.5'#$00b0'E to 151.5'#$00b0'E onshore',
           148.50, 45.21, 151.50, 76.82
         );
  _cs.Add(   2697, 'Russia - 151.5'#$00b0'E to 154.5'#$00b0'E onshore',
           151.50, 46.72, 154.50, 76.26
         );
  _cs.Add(   2698, 'Russia - 154.5'#$00b0'E to 157.5'#$00b0'E onshore',
           154.50, 49.02, 157.50, 77.20
         );
  _cs.Add(   2699, 'Russia - 157.5'#$00b0'E to 160.5'#$00b0'E onshore',
           157.50, 51.36, 160.50, 71.12
         );
  _cs.Add(   2700, 'Russia - 160.5'#$00b0'E to 163.5'#$00b0'E onshore',
           160.50, 54.34, 163.50, 70.98
         );
  _cs.Add(   2701, 'Russia - 163.5'#$00b0'E to 166.5'#$00b0'E onshore',
           163.50, 54.69, 166.50, 69.82
         );
  _cs.Add(   2702, 'Russia - 166.5'#$00b0'E to 169.5'#$00b0'E onshore',
           166.50, 54.45, 169.50, 70.07
         );
  _cs.Add(   2703, 'Russia - 169.5'#$00b0'E to 172.5'#$00b0'E onshore',
           169.50, 59.86, 172.50, 70.19
         );
  _cs.Add(   2704, 'Russia - 172.5'#$00b0'E to 175.5'#$00b0'E onshore',
           172.50, 60.99, 175.50, 70.02
         );
  _cs.Add(   2705, 'Russia - 175.5'#$00b0'E to 178.5'#$00b0'E onshore',
           175.50, 62.09, 178.50, 71.10
         );
  _cs.Add(   2706, 'Russia - 178.5'#$00b0'E to 178.5'#$00b0'W onshore',
           178.50, 62.24, -178.50, 71.65
         );
  _cs.Add(   2707, 'Russia - 178.5'#$00b0'W to 175.5'#$00b0'W onshore',
           -178.50, 64.74, -175.50, 71.61
         );
  _cs.Add(   2708, 'Russia - 175.5'#$00b0'W to 172.5'#$00b0'W onshore',
           -175.50, 64.20, -172.50, 67.78
         );
  _cs.Add(   2709, 'Russia - 172.5'#$00b0'W to 169.5'#$00b0'W onshore',
           -172.50, 64.35, -169.57, 67.06
         );
  _cs.Add(   2747, 'Russia - 19.5'#$00b0'E to 22.5'#$00b0'E onshore',
           19.57, 54.32, 22.50, 55.32
         );
  _cs.Add(   2748, 'Russia - 22.5'#$00b0'E to 25.5'#$00b0'E onshore',
           22.50, 54.34, 22.87, 55.07
         );
  _cs.Add(   2749, 'Russia - 25.5'#$00b0'E to 28.5'#$00b0'E onshore',
           26.61, 56.05, 28.51, 68.93
         );
  _cs.Add(   2750, 'Russia - 28.5'#$00b0'E to 31.5'#$00b0'E onshore',
           28.50, 52.85, 31.50, 69.85
         );
  _cs.Add(   2751, 'Russia - 31.5'#$00b0'E to 34.5'#$00b0'E onshore',
           31.50, 51.24, 34.50, 70.02
         );
  _cs.Add(   2779, 'Portugal - Selvagens onshore',
           -16.11, 29.98, -15.79, 30.21
         );
  _cs.Add(   2870, 'Portugal - Madeira and Porto Santo islands onshore',
           -17.31, 32.58, -16.23, 33.15
         );
  _cs.Add(   2871, 'Portugal - Azores E - S Miguel onshore',
           -25.92, 37.65, -25.08, 37.96
         );
  _cs.Add(   2872, 'Portugal - Azores C - Terceira onshore',
           -27.44, 38.57, -26.97, 38.86
         );
  _cs.Add(   2873, 'Portugal - Azores C - Faial onshore',
           -28.90, 38.46, -28.54, 38.70
         );
  _cs.Add(   2874, 'Portugal - Azores C - Pico onshore',
           -28.61, 38.32, -27.98, 38.61
         );
  _cs.Add(   2875, 'Portugal - Azores C - S Jorge onshore',
           -28.37, 38.48, -27.71, 38.80
         );
  _cs.Add(   3168, 'Poland - west of 18'#$00b0'E',
           14.14, 49.98, 18.00, 54.85
         )         .MarkDeprecated( True );
  _cs.Add(   3170, 'Poland - east of 24'#$00b0'E',
           24.00, 50.40, 24.16, 50.90
         )         .MarkDeprecated( True );
  _cs.Add(   3293, 'Poland - onshore',
           14.14, 49.00, 24.15, 54.89
         );
  _cs.Add(   3294, 'Puerto Rico - onshore',
           -67.97, 17.87, -65.19, 18.57
         );
  _cs.Add(   3295, 'Romania - onshore',
           20.26, 43.62, 29.74, 48.27
         );
  _cs.Add(   3337, 'Reunion - onshore',
           55.16, -21.42, 55.91, -20.81
         );
  _cs.Add(   3537, 'Portugal - mainland - offshore',
           -13.87, 34.91, -7.24, 41.88
         );
  _cs.Add(   3670, 'Portugal - Azores and Madeira',
           -35.58, 29.24, -12.48, 43.07
         );
  _cs.Add(   3677, 'Portugal - Azores 30'#$00b0'W to 24'#$00b0'W',
           -30.00, 33.52, -24.00, 42.96
         );
  _cs.Add(   1228, 'Taiwan',
           114.32, 17.36, 123.61, 26.96
         );
  _cs.Add(   1229, 'Tajikistan',
           67.36, 36.67, 75.19, 41.05
         );
  _cs.Add(   1230, 'Tanzania',
           29.34, -11.75, 43.29, -0.99
         );
  _cs.Add(   1231, 'Thailand',
           95.52, 5.63, 105.64, 20.46
         );
  _cs.Add(   1232, 'Togo',
           -0.15, 2.91, 2.42, 11.14
         );
  _cs.Add(   1233, 'Tokelau',
           -175.86, -11.04, -167.98, -6.46
         );
  _cs.Add(   1361, 'Sudan - south',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1666, 'Thailand - east of 102'#$00b0'E',
           102.00, 6.02, 105.64, 18.44
         );
  _cs.Add(   1667, 'Thailand - onshore and GoT 96'#$00b0'E to102'#$00b0'E',
           97.34, 5.63, 102.01, 20.46
         );
  _cs.Add(   1727, 'Suriname - offshore',
           -57.25, 5.34, -52.66, 9.35
         );
  _cs.Add(   1846, 'Sudan - south - west of 30'#$00b0'E',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1847, 'Sudan - south - east of 30'#$00b0'E',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   2327, 'Syria - Al Whaleed area',
           39.15, 35.33, 40.41, 35.90
         );
  _cs.Add(   2328, 'Syria - Shaddadeh area',
           40.50, 35.79, 41.39, 36.50
         );
  _cs.Add(   2329, 'Syria - Deir area',
           39.30, 34.49, 40.81, 35.90
         );
  _cs.Add(   2336, 'Spain - mainland except northwest',
           -7.54, 35.26, 3.39, 43.56
         );
  _cs.Add(   2337, 'Spain - mainland northwest',
           -9.37, 41.50, -4.50, 43.82
         );
  _cs.Add(   2358, 'Thailand - Bongkot field',
           102.16, 6.74, 103.05, 8.16
         );
  _cs.Add(   2366, 'Spain - mainland onshore',
           -9.37, 35.95, 3.39, 43.82
         );
  _cs.Add(   2367, 'Spain - mainland northeast',
           -9.37, 39.96, 3.39, 43.82
         );
  _cs.Add(   2368, 'Spain - mainland southwest',
           -7.54, 35.95, 0.28, 41.98
         );
  _cs.Add(   2833, 'Sweden - 12 00',
           10.93, 56.74, 13.11, 60.13
         );
  _cs.Add(   2834, 'Sweden - 13 30',
           12.12, 55.28, 14.79, 62.28
         );
  _cs.Add(   2835, 'Sweden - 15 00',
           13.54, 55.95, 16.15, 61.62
         );
  _cs.Add(   2836, 'Sweden - 16 30',
           15.41, 56.15, 17.63, 62.26
         );
  _cs.Add(   2837, 'Sweden - 18 00',
           17.08, 58.66, 19.61, 60.70
         );
  _cs.Add(   2838, 'Sweden - 14 15',
           11.93, 61.55, 15.55, 64.39
         );
  _cs.Add(   2839, 'Sweden - 15 45',
           13.66, 60.44, 17.01, 65.13
         );
  _cs.Add(   2840, 'Sweden - 17 15',
           14.31, 62.12, 19.04, 67.19
         );
  _cs.Add(   2841, 'Sweden - 18 45',
           17.18, 56.86, 20.22, 66.17
         );
  _cs.Add(   2842, 'Sweden - 20 15',
           16.08, 63.45, 23.28, 69.07
         );
  _cs.Add(   2843, 'Sweden - 21 45',
           19.63, 65.01, 22.91, 66.43
         );
  _cs.Add(   2844, 'Sweden - 23 15',
           21.85, 65.49, 24.17, 68.14
         );
  _cs.Add(   2845, 'Sweden - 7.5 gon W',
           10.93, 57.29, 12.91, 59.73
         );
  _cs.Add(   2846, 'Sweden - 5 gon W',
           11.81, 55.28, 15.44, 64.39
         );
  _cs.Add(   2847, 'Sweden - 2.5 gon W',
           13.66, 55.95, 17.73, 67.18
         );
  _cs.Add(   2848, 'Sweden - 0 gon',
           16.08, 56.86, 20.22, 68.54
         );
  _cs.Add(   2849, 'Sweden - 2.5 gon E',
           18.40, 63.37, 22.20, 69.07
         );
  _cs.Add(   2850, 'Sweden - 5 gon E',
           21.34, 65.24, 24.17, 68.58
         );
  _cs.Add(   3182, 'St Helena - Ascension Island',
           -14.46, -8.03, -14.24, -7.83
         );
  _cs.Add(   3183, 'St Helena - St Helena Island',
           -5.85, -16.08, -5.58, -15.85
         );
  _cs.Add(   3184, 'St Helena - Tristan da Cunha',
           -12.76, -40.42, -9.80, -37.00
         );
  _cs.Add(   3199, 'Spain - Canary Islands',
           -21.93, 24.60, -11.75, 32.76
         );
  _cs.Add(   3297, 'St Kitts and Nevis - onshore',
           -62.92, 17.06, -62.50, 17.46
         );
  _cs.Add(   3298, 'St Lucia - onshore',
           -61.13, 13.66, -60.82, 14.16
         );
  _cs.Add(   3299, 'St Pierre and Miquelon - onshore',
           -56.48, 46.69, -56.07, 47.19
         );
  _cs.Add(   3300, 'St Vincent and the Grenadines - onshore',
           -61.52, 12.54, -61.07, 13.44
         );
  _cs.Add(   3310, 'Sri Lanka - onshore',
           79.64, 5.86, 81.95, 9.88
         );
  _cs.Add(   3312, 'Suriname - onshore',
           -58.08, 1.83, -53.95, 6.06
         );
end;

procedure prepare_23;
begin
  _cs.Add(   3313, 'Sweden - onshore',
           10.93, 55.28, 24.17, 69.07
         );
  _cs.Add(   3314, 'Syria - onshore',
           35.61, 32.31, 42.38, 37.30
         );
  _cs.Add(   3315, 'Taiwan - onshore - mainland and Penghu',
           119.25, 21.87, 122.06, 25.34
         );
  _cs.Add(   3316, 'Tanzania - onshore',
           29.34, -11.75, 40.48, -0.99
         );
  _cs.Add(   3317, 'Thailand - onshore',
           97.34, 5.63, 105.64, 20.46
         );
  _cs.Add(   3678, 'Portugal - Madeira and EEZ E of 18'#$00b0'W',
           -18.00, 29.24, -12.48, 36.46
         );
  _cs.Add(   3679, 'Portugal - Madeira island onshore',
           -17.31, 32.58, -16.66, 32.93
         );
  _cs.Add(   3680, 'Portugal - Porto Santo island onshore',
           -16.44, 32.96, -16.23, 33.15
         );
  _cs.Add(   3681, 'Portugal - Azores C - Graciosa onshore',
           -28.13, 38.97, -27.88, 39.14
         );
  _cs.Add(   3682, 'Portugal - Azores - west of 30'#$00b0'W',
           -35.58, 35.25, -30.00, 43.07
         );
  _cs.Add(   3683, 'Portugal - Azores E - Santa Maria onshore',
           -25.26, 36.87, -24.96, 37.08
         );
  _cs.Add(   3684, 'Portugal - Azores W - Flores onshore',
           -31.34, 39.30, -31.07, 39.58
         );
  _cs.Add(   3685, 'Portugal - Azores W - Corvo onshore',
           -31.18, 39.63, -31.02, 39.77
         );
  _cs.Add(   3902, 'Reunion',
           51.83, -24.72, 58.24, -18.28
         );
  _cs.Add(   3911, 'Reunion - east of 54'#$00b0'E',
           54.00, -24.72, 58.24, -18.28
         );
  _cs.Add(   3915, 'Reunion - west of 54'#$00b0'E',
           51.83, -24.37, 54.00, -18.52
         );
  _cs.Add(   4125, 'Portugal - Madeira and Desertas islands onshore',
           -17.31, 32.35, -16.40, 32.93
         );
  _cs.Add(   4126, 'Portugal - Azores E onshore - Santa Maria and Formigas',
           -25.26, 36.87, -24.72, 37.34
         );
  _cs.Add(   1199, 'Rwanda',
           28.85, -2.83, 30.90, -1.05
         );
  _cs.Add(   1203, 'Samoa',
           -174.54, -15.84, -170.51, -10.94
         );
  _cs.Add(   1204, 'San Marino',
           12.40, 43.89, 12.52, 43.99
         );
  _cs.Add(   1205, 'Sao Tome and Principe',
           3.20, -1.49, 8.56, 2.72
         );
  _cs.Add(   1206, 'Saudi Arabia',
           34.44, 16.29, 55.67, 32.16
         );
  _cs.Add(   1207, 'Senegal',
           -20.22, 10.64, -11.36, 16.70
         );
  _cs.Add(   1208, 'Seychelles',
           43.19, -12.72, 59.66, -0.37
         );
  _cs.Add(   1209, 'Sierra Leone',
           -16.57, 4.22, -10.26, 10.00
         );
  _cs.Add(   1210, 'Singapore',
           103.59, 1.13, 104.07, 1.47
         );
  _cs.Add(   1211, 'Slovakia',
           16.84, 47.73, 22.56, 49.61
         );
  _cs.Add(   1212, 'Slovenia',
           13.38, 45.42, 16.61, 46.88
         );
  _cs.Add(   1342, 'Sierra Leone - Freetown Peninsula',
           -13.34, 8.32, -13.13, 8.55
         );
  _cs.Add(   1509, 'Sierra Leone - west of 12'#$00b0'W',
           -13.35, 7.15, -12.00, 9.94
         );
  _cs.Add(   1510, 'Sierra Leone - east of 12'#$00b0'W',
           -12.00, 6.88, -10.26, 10.00
         );
  _cs.Add(   1569, 'Saudi Arabia - onshore 36'#$00b0'E to 42'#$00b0'E',
           36.00, 16.59, 42.00, 32.16
         );
  _cs.Add(   1763, 'Russia - west of 24'#$00b0'E onshore',
           19.57, 54.32, 22.87, 55.32
         );
  _cs.Add(   1766, 'Russia - 36'#$00b0'E to 42'#$00b0'E onshore',
           36.00, 43.18, 42.01, 69.23
         );
  _cs.Add(   1767, 'Russia - 42'#$00b0'E to 48'#$00b0'E onshore',
           42.00, 41.19, 48.00, 80.91
         );
  _cs.Add(   1768, 'Russia - 48'#$00b0'E to 54'#$00b0'E onshore',
           48.00, 41.39, 54.00, 81.40
         );
  _cs.Add(   1769, 'Russia - 54'#$00b0'E to 60'#$00b0'E onshore',
           54.00, 50.47, 60.00, 81.91
         );
  _cs.Add(   1770, 'Russia - 60'#$00b0'E to 66'#$00b0'E onshore',
           60.00, 50.66, 66.00, 81.77
         );
  _cs.Add(   1771, 'Russia - 66'#$00b0'E to 72'#$00b0'E onshore',
           66.00, 54.10, 72.00, 77.07
         );
  _cs.Add(   1772, 'Russia - 72'#$00b0'E to 78'#$00b0'E onshore',
           72.00, 53.17, 78.00, 79.71
         );
  _cs.Add(   1773, 'Russia - 78'#$00b0'E to 84'#$00b0'E onshore',
           78.00, 50.69, 84.00, 81.03
         );
  _cs.Add(   1774, 'Russia - 84'#$00b0'E to 90'#$00b0'E onshore',
           84.00, 49.07, 90.00, 81.27
         );
  _cs.Add(   1775, 'Russia - 90'#$00b0'E to 96'#$00b0'E onshore',
           90.00, 49.89, 96.00, 81.35
         );
  _cs.Add(   1776, 'Russia - 96'#$00b0'E to 102'#$00b0'E onshore',
           96.00, 49.73, 102.00, 81.32
         );
  _cs.Add(   1791, 'Russia - east of 174'#$00b0'W onshore',
           -174.00, 64.20, -168.97, 67.18
         );
  _cs.Add(   2369, 'Seychelles - Mahe Island',
           55.30, -4.86, 55.59, -4.50
         );
  _cs.Add(   2422, 'Slovenia - Gorenjska - central',
           14.01, 46.14, 14.61, 46.45
         );
  _cs.Add(   2547, 'Serbia and Montenegro - Montenegro',
           18.44, 41.82, 20.37, 43.53
         )         .MarkDeprecated( True );
  _cs.Add(   2579, 'Slovenia - Gorica',
           13.47, 45.77, 14.11, 46.18
         );
  _cs.Add(   2582, 'Slovenia - Karst',
           13.57, 45.64, 14.12, 45.90
         );
  _cs.Add(   2583, 'Slovenia - Littoral onshore',
           13.50, 45.44, 14.24, 45.73
         );
  _cs.Add(   2584, 'Slovenia - Bela Krajina',
           15.06, 45.42, 15.36, 45.74
         );
  _cs.Add(   2585, 'Slovenia - Pohorje',
           15.15, 46.28, 15.54, 46.58
         );
  _cs.Add(   2586, 'Slovenia - lower Posavje',
           15.20, 45.81, 15.73, 46.17
         );
  _cs.Add(   2587, 'Slovenia - Maribor and Ptuj',
           15.44, 46.33, 16.00, 46.74
         );
  _cs.Add(   2676, 'Russia - 88.5'#$00b0'E to 91.5'#$00b0'E onshore',
           88.50, 49.44, 91.50, 81.28
         );
  _cs.Add(   2677, 'Russia - 91.5'#$00b0'E to 94.5'#$00b0'E onshore',
           91.50, 50.16, 94.50, 81.26
         );
  _cs.Add(   2678, 'Russia - 94.5'#$00b0'E to 97.5'#$00b0'E onshore',
           94.50, 49.73, 97.50, 81.35
         );
  _cs.Add(   2679, 'Russia - 97.5'#$00b0'E to 100.5'#$00b0'E onshore',
           97.50, 49.79, 100.50, 80.90
         );
  _cs.Add(   2710, 'Russia - east of 169.5'#$00b0'W onshore',
           -169.22, 65.70, -168.97, 65.86
         );
  _cs.Add(   2752, 'Russia - 34.5'#$00b0'E to 37.5'#$00b0'E onshore',
           34.50, 44.61, 37.51, 69.39
         );
  _cs.Add(   2753, 'Russia - 37.5'#$00b0'E to 40.5'#$00b0'E onshore',
           37.50, 43.33, 40.50, 68.80
         );
  _cs.Add(   2754, 'Russia - 40.5'#$00b0'E to 43.5'#$00b0'E onshore',
           40.50, 42.87, 43.50, 68.74
         );
  _cs.Add(   2755, 'Russia - 43.5'#$00b0'E to 46.5'#$00b0'E onshore',
           43.50, 41.89, 46.50, 80.80
         );
  _cs.Add(   2756, 'Russia - 46.5'#$00b0'E to 49.5'#$00b0'E onshore',
           46.50, 41.19, 49.50, 80.91
         );
  _cs.Add(   2757, 'Russia - 49.5'#$00b0'E to 52.5'#$00b0'E onshore',
           49.50, 42.36, 52.50, 81.22
         );
  _cs.Add(   2758, 'Russia - 52.5'#$00b0'E to 55.5'#$00b0'E onshore',
           52.50, 50.52, 55.50, 81.41
         );
  _cs.Add(   2759, 'Russia - 55.5'#$00b0'E to 58.5'#$00b0'E onshore',
           55.50, 50.53, 58.50, 81.89
         );
  _cs.Add(   2760, 'Russia - 58.5'#$00b0'E to 61.5'#$00b0'E onshore',
           58.50, 50.47, 61.50, 81.91
         );
  _cs.Add(   2761, 'Russia - 61.5'#$00b0'E to 64.5'#$00b0'E onshore',
           61.50, 51.03, 64.50, 81.77
         );
  _cs.Add(   2762, 'Russia - 64.5'#$00b0'E to 67.5'#$00b0'E onshore',
           64.50, 54.31, 67.50, 81.25
         );
  _cs.Add(   2763, 'Russia - 67.5'#$00b0'E to 70.5'#$00b0'E onshore',
           67.50, 54.85, 70.50, 77.07
         );
  _cs.Add(   2764, 'Russia - 70.5'#$00b0'E to 73.5'#$00b0'E onshore',
           70.50, 53.43, 73.50, 73.57
         );
  _cs.Add(   2765, 'Russia - 73.5'#$00b0'E to 76.5'#$00b0'E onshore',
           73.50, 53.47, 76.50, 79.71
         );
  _cs.Add(   2766, 'Russia - 76.5'#$00b0'E to 79.5'#$00b0'E onshore',
           76.50, 51.49, 79.50, 81.03
         );
  _cs.Add(   2767, 'Russia - 79.5'#$00b0'E to 82.5'#$00b0'E onshore',
           79.50, 50.70, 82.50, 81.03
         );
  _cs.Add(   2768, 'Russia - 82.5'#$00b0'E to 85.5'#$00b0'E onshore',
           82.50, 49.58, 85.50, 77.56
         );
  _cs.Add(   2769, 'Russia - 85.5'#$00b0'E to 88.5'#$00b0'E onshore',
           85.50, 49.07, 88.50, 77.16
         );
  _cs.Add(   2877, 'Slovenia - Slovenska Bistrica',
           15.31, 46.14, 16.00, 46.46
         );
  _cs.Add(   3106, 'Saudi Arabia - east of 54'#$00b0'E',
           54.00, 19.66, 55.67, 22.77
         );
  _cs.Add(   3107, 'Saudi Arabia - onshore west of 36'#$00b0'E',
           34.51, 26.82, 36.00, 29.38
         );
  _cs.Add(   3210, 'Serbia and Montenegro',
           18.44, 41.82, 23.05, 46.23
         )         .MarkDeprecated( True );
  _cs.Add(   3296, 'Russia - onshore',
           19.58, 41.19, -168.97, 81.91
         );
  _cs.Add(   3301, 'Samoa - onshore',
           -172.84, -14.11, -171.37, -13.41
         );
  _cs.Add(   3302, 'Sao Tome and Principe - onshore',
           6.41, -0.04, 7.52, 1.76
         );
  _cs.Add(   3303, 'Saudi Arabia - onshore',
           34.51, 16.37, 55.67, 32.16
         );
  _cs.Add(   3304, 'Senegal - onshore',
           -17.59, 12.29, -11.36, 16.70
         );
  _cs.Add(   3305, 'Serbia and Montenegro - onshore',
           18.44, 41.82, 23.05, 46.23
         )         .MarkDeprecated( True );
  _cs.Add(   3306, 'Sierra Leone - onshore',
           -13.35, 6.88, -10.26, 10.00
         );
  _cs.Add(   3307, 'Slovenia - onshore',
           13.38, 45.42, 16.61, 46.88
         );
  _cs.Add(   3345, 'Slovenia - Ljubljana east',
           14.45, 45.85, 15.01, 46.32
         );
  _cs.Add(   3346, 'Slovenia - Prekmurje',
           15.98, 46.47, 16.61, 46.88
         );
  _cs.Add(   3347, 'Slovenia - Novo Mesto',
           15.06, 45.66, 15.47, 45.87
         );
  _cs.Add(   3348, 'Slovenia - Gorenjska - upper',
           13.70, 46.09, 14.18, 46.53
         );
  _cs.Add(   3349, 'Slovenia - Ljubliana west',
           13.95, 45.86, 14.52, 46.21
         );
  _cs.Add(   3350, 'Slovenia - Notranjska',
           14.08, 45.47, 14.59, 45.88
         );
  _cs.Add(   3351, 'Slovenia - Kocevje',
           14.60, 45.46, 15.12, 45.76
         );
  _cs.Add(   3352, 'Slovenia - Koroska',
           14.83, 46.46, 15.51, 46.66
         );
  _cs.Add(   3403, 'Russia - west of 19.5'#$00b0'E',
           19.20, 54.32, 19.50, 55.30
         )         .MarkDeprecated( True );
end;

procedure prepare_24;
begin
  _cs.Add(   3534, 'Serbia and Kosovo',
           18.81, 41.85, 23.01, 46.19
         );
  _cs.Add(   3560, 'Slovenia - Dolenjska - central',
           14.97, 45.81, 15.43, 46.01
         );
  _cs.Add(   3565, 'Slovenia - NE',
           14.54, 46.14, 16.61, 46.88
         );
  _cs.Add(   3566, 'Slovenia - SE',
           14.55, 45.42, 15.73, 46.22
         );
  _cs.Add(   3568, 'Slovenia - Dolenjska',
           14.47, 45.70, 15.73, 46.12
         );
  _cs.Add(   3570, 'Slovenia - Pomurje',
           15.96, 46.47, 16.61, 46.88
         );
  _cs.Add(   3571, 'Slovenia - Gorenjska and N Primorsko',
           13.38, 46.05, 14.82, 46.53
         );
  _cs.Add(   3572, 'Slovenia - Primorska and Notranjska onshore',
           13.47, 45.44, 14.58, 46.08
         );
  _cs.Add(   3573, 'Slovenia - central',
           14.21, 45.91, 15.28, 46.31
         );
  _cs.Add(   3645, 'Sao Tome and Principe - onshore - Sao Tome',
           6.41, -0.04, 6.82, 0.46
         );
  _cs.Add(   3646, 'Sao Tome and Principe - onshore - Principe',
           7.27, 1.48, 7.52, 1.76
         );
  _cs.Add(   3968, 'Saudi Arabia - onshore Gulf coast',
           47.95, 24.63, 50.81, 28.57
         );
  _cs.Add(   4183, 'Seychelles - Seychelles Bank',
           55.15, -4.86, 56.01, -3.66
         );
  _cs.Add(   4524, 'Saudi Arabia - west of 36'#$00b0'E',
           34.44, 24.92, 36.01, 29.38
         );
  _cs.Add(   4526, 'Saudi Arabia - 36'#$00b0'E to 42'#$00b0'E',
           36.00, 16.29, 42.00, 32.16
         );
  _cs.Add(   4527, 'Saudi Arabia - 42'#$00b0'E to 48'#$00b0'E',
           41.99, 16.35, 48.00, 31.15
         );
  _cs.Add(   4528, 'Saudi Arabia - 48'#$00b0'E to 54'#$00b0'E',
           47.99, 17.94, 54.01, 28.94
         );
  _cs.Add(   4543, 'Serbia',
           18.81, 42.23, 23.01, 46.19
         );
  _cs.Add(   1213, 'Solomon Islands - onshore main islands',
           155.62, -10.90, 162.44, -6.55
         );
  _cs.Add(   1214, 'Somalia',
           40.98, -3.61, 54.43, 13.50
         );
  _cs.Add(   1215, 'South Africa',
           13.33, -50.32, 42.85, -22.13
         );
  _cs.Add(   1216, 'South Georgia and the South Sandwich Islands',
           -48.06, -62.79, -19.84, -50.15
         );
  _cs.Add(   1217, 'Spain',
           -21.93, 24.60, 6.30, 46.26
         );
  _cs.Add(   1303, 'South America - Tierra del Fuego',
           -74.83, -55.96, -63.73, -51.65
         );
  _cs.Add(   1341, 'South America by country',
           -82.00, -56.15, -34.00, 13.00
         )         .MarkDeprecated( True );
  _cs.Add(   1348, 'South America - PSAD56 by country',
           -81.41, -43.50, -47.99, 12.68
         );
  _cs.Add(   1358, 'South America - SAD69 by country',
           -91.72, -55.96, -25.28, 12.52
         );
  _cs.Add(   1455, 'South Africa - west of 18'#$00b0'E',
           16.45, -33.10, 18.00, -28.03
         );
  _cs.Add(   1456, 'South Africa - 18'#$00b0'E to 20'#$00b0'E',
           17.99, -34.88, 20.00, -28.38
         );
  _cs.Add(   1457, 'South Africa - 20'#$00b0'E to 22'#$00b0'E',
           19.99, -34.88, 22.01, -24.76
         );
  _cs.Add(   1458, 'South Africa - 22'#$00b0'E to 24'#$00b0'E',
           22.00, -34.26, 24.01, -25.26
         );
  _cs.Add(   1459, 'South Africa - 24'#$00b0'E to 26'#$00b0'E',
           24.00, -34.26, 26.01, -24.71
         );
  _cs.Add(   1460, 'South Africa - 26'#$00b0'E to 28'#$00b0'E',
           26.00, -33.83, 28.00, -22.92
         );
  _cs.Add(   1461, 'South Africa - 28'#$00b0'E to 30'#$00b0'E',
           27.99, -33.03, 30.00, -22.13
         );
  _cs.Add(   1462, 'South Africa - 30'#$00b0'E to 32'#$00b0'E',
           29.99, -31.38, 32.02, -22.22
         );
  _cs.Add(   1463, 'South Africa - east of 32'#$00b0'E',
           31.95, -28.94, 32.95, -26.80
         );
  _cs.Add(   1554, 'Somalia - 42'#$00b0'E to 48'#$00b0'E, N hemisphere onshore',
           42.00, 0.00, 48.00, 11.52
         );
  _cs.Add(   1555, 'Somalia - onshore east of 48'#$00b0'E',
           48.00, 4.44, 51.47, 12.03
         );
  _cs.Add(   1755, 'South America - 84'#$00b0'W to 78'#$00b0'W, S hemisphere and PSAD56 by country',
           -81.41, -10.53, -78.00, 0.00
         );
  _cs.Add(   1756, 'South America - 78'#$00b0'W to 72'#$00b0'W, N hemisphere and PSAD56 by country',
           -78.00, 0.00, -71.99, 11.62
         );
  _cs.Add(   1757, 'South America - 78'#$00b0'W to 72'#$00b0'W, S hemisphere and PSAD56 by country',
           -78.00, -43.50, -71.99, 0.00
         );
  _cs.Add(   1758, 'South America - 72'#$00b0'W to 66'#$00b0'W, N hemisphere and PSAD56 by country',
           -72.00, 0.73, -66.00, 12.68
         );
  _cs.Add(   1759, 'South America - 72'#$00b0'W to 66'#$00b0'W, S hemisphere and PSAD56 by country',
           -72.00, -43.50, -66.00, -2.14
         );
  _cs.Add(   1760, 'South America - 66'#$00b0'W to 60'#$00b0'W, N hemisphere and PSAD56 by country',
           -66.00, 0.64, -59.99, 11.23
         );
  _cs.Add(   1762, 'South America - 60'#$00b0'W to 54'#$00b0'W, N hemisphere and PSAD56 by country',
           -60.00, 1.18, -56.47, 8.58
         );
  _cs.Add(   1806, 'South America - UTM 17S and SAD69 by country',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1807, 'South America - UTM 18N and SAD69 by country',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1808, 'South America - UTM 18S and SAD69 by country',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1809, 'South America - UTM 19N and SAD69 by country',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1810, 'South America - UTM 19S and SAD69 by country',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1811, 'South America - UTM 20N and SAD69 by country',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1812, 'South America - UTM 20S and SAD69 by country',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1813, 'South America - UTM 21N and SAD69',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1814, 'South America - 60'#$00b0'W to 54'#$00b0'W, S hemisphere and SAD69 by country',
           -60.00, -38.91, -53.99, 4.51
         );
  _cs.Add(   1815, 'South America - 54'#$00b0'W to 48'#$00b0'W, N hemisphere and SAD69 by country',
           -54.00, 1.68, -46.65, 5.81
         );
  _cs.Add(   1816, 'South America - 54'#$00b0'W to 48'#$00b0'W, S hemisphere and SAD69 by country',
           -54.00, -35.71, -47.99, 7.04
         );
  _cs.Add(   1823, 'South America - 84'#$00b0'W to 78'#$00b0'W, N hemisphere and SIRGAS95 by country',
           -84.00, 0.90, -78.00, 15.51
         );
  _cs.Add(   1824, 'South America - 84'#$00b0'W to 78'#$00b0'W, S hemisphere',
           -84.00, -56.45, -78.00, 0.00
         );
  _cs.Add(   1825, 'South America - 78'#$00b0'W to 72'#$00b0'W, N hemisphere',
           -78.00, 0.00, -72.00, 15.04
         );
  _cs.Add(   1826, 'South America - 78'#$00b0'W to 72'#$00b0'W, S hemisphere and SIRGAS 1995 by country',
           -78.00, -59.36, -71.99, 0.00
         );
  _cs.Add(   1827, 'South America - 72'#$00b0'W to 66'#$00b0'W, N hemisphere',
           -72.00, 0.00, -66.00, 15.64
         );
  _cs.Add(   1828, 'South America - 72'#$00b0'W to 66'#$00b0'W, S hemisphere',
           -72.00, -59.87, -66.00, 0.00
         );
  _cs.Add(   1829, 'South America - 66'#$00b0'W to 60'#$00b0'W, N hemisphere',
           -66.00, 0.00, -60.00, 16.75
         );
  _cs.Add(   1830, 'South America - 66'#$00b0'W to 60'#$00b0'W, S hemisphere',
           -66.00, -58.39, -60.00, 0.00
         );
  _cs.Add(   1831, 'South America - 60'#$00b0'W to 54'#$00b0'W, N hemisphere',
           -60.00, 0.00, -54.00, 10.70
         );
  _cs.Add(   1832, 'South America - 60'#$00b0'W to 54'#$00b0'W, S hemisphere',
           -60.00, -44.82, -54.00, 0.00
         );
  _cs.Add(   1833, 'South America - 54'#$00b0'W to 48'#$00b0'W, N hemisphere',
           -54.00, 0.00, -48.00, 9.24
         );
  _cs.Add(   1834, 'South America - 54'#$00b0'W to 48'#$00b0'W, S hemisphere',
           -54.00, -39.95, -48.00, 0.00
         );
  _cs.Add(   1835, 'South America - 48'#$00b0'W to 42'#$00b0'W',
           -48.00, -33.50, -42.00, 0.00
         );
  _cs.Add(   1836, 'South America - 42'#$00b0'W to 36'#$00b0'W',
           -42.00, -26.35, -36.00, 0.00
         );
  _cs.Add(   1837, 'South America - 36'#$00b0'W to 30'#$00b0'W',
           -36.00, -20.11, -30.00, 0.00
         );
  _cs.Add(   2335, 'Spain - Balearic Islands',
           1.12, 38.59, 4.39, 40.15
         );
  _cs.Add(   2399, 'South America - Bolivia; Chile; Ecuador; Guyana; Peru; Venezuela',
           -81.41, -43.50, -56.47, 12.25
         );
  _cs.Add(   2578, 'Slovenia - upper Soca Valley',
           13.38, 46.16, 13.87, 46.49
         );
  _cs.Add(   2580, 'Slovenia - upper Savinja',
           14.55, 46.21, 15.00, 46.56
         );
  _cs.Add(   2581, 'Slovenia - Suha Krajina',
           14.57, 45.66, 15.08, 45.89
         );
  _cs.Add(   2878, 'Slovenia - Slovenske Gorice',
           15.90, 46.29, 16.30, 46.76
         );
  _cs.Add(   3112, 'South America - 84'#$00b0'W to 78'#$00b0'W, N hemisphere and PSAD56 by country',
           -80.18, 0.00, -78.00, 1.45
         );
  _cs.Add(   3187, 'South Georgia and the South Sandwich Islands - onshore',
           -42.14, -59.53, -26.19, -53.49
         );
  _cs.Add(   3197, 'Solomon Islands - Guadalcanal Island',
           159.55, -9.98, 160.88, -9.20
         );
  _cs.Add(   3198, 'Solomon Islands - New Georgia - Ghizo (Gizo)',
           156.44, -8.86, 158.20, -7.52
         );
  _cs.Add(   3308, 'Somalia - onshore',
           40.99, -1.71, 51.47, 12.03
         );
  _cs.Add(   3309, 'South Africa - onshore',
           16.45, -34.88, 32.95, -22.13
         );
  _cs.Add(   3353, 'Slovenia - Velenje',
           14.90, 46.28, 15.26, 46.51
         );
  _cs.Add(   3354, 'Slovenia - Zasavje',
           14.84, 46.00, 15.35, 46.32
         );
  _cs.Add(   3356, 'South America - Brazil - south of 18'#$00b0'S and west of 54'#$00b0'W + DF; N Paraguay',
           -62.57, -31.91, -47.10, -15.37
         );
  _cs.Add(   3436, 'South America - 72'#$00b0'W to 66'#$00b0'W, N hemisphere and SIRGAS 2000 by country',
           -72.00, 0.00, -66.00, 15.64
         );
  _cs.Add(   3437, 'South America - 66'#$00b0'W to 60'#$00b0'W, N hemisphere and SIRGAS 2000 by country',
           -66.00, 0.64, -59.99, 16.75
         );
  _cs.Add(   3438, 'South America - 60'#$00b0'W to 54'#$00b0'W, N hemisphere and SIRGAS 2000 by country',
           -60.00, 1.18, -54.00, 12.19
         );
  _cs.Add(   3439, 'South America - 54'#$00b0'W to 48'#$00b0'W, N hemisphere and SIRGAS 2000 by country',
           -54.00, 0.00, -47.99, 9.24
         );
  _cs.Add(   3440, 'South America - 78'#$00b0'W to 72'#$00b0'W, S hemisphere and SIRGAS 2000 by country',
           -78.00, -59.36, -71.99, 0.00
         );
  _cs.Add(   3441, 'South America - 72'#$00b0'W to 66'#$00b0'W, S hemisphere and SIRGAS 2000 by country',
           -72.00, -59.87, -66.00, 2.15
         );
  _cs.Add(   3442, 'South America - 66'#$00b0'W to 60'#$00b0'W, S hemisphere and SIRGAS 2000 by country',
           -66.00, -58.39, -60.00, 5.28
         );
  _cs.Add(   3443, 'South America - 60'#$00b0'W to 54'#$00b0'W, S hemisphere and SIRGAS 2000 by country',
           -60.00, -44.82, -54.00, 4.51
         );
  _cs.Add(   3444, 'South America - 54'#$00b0'W to 48'#$00b0'W, S hemisphere and SIRGAS 2000 by country',
           -54.00, -54.18, -47.99, 7.04
         );
  _cs.Add(   3448, 'South America - SIRGAS 1995 by country',
           -113.21, -59.87, -26.00, 16.75
         );
  _cs.Add(   3529, 'South Georgia - onshore',
           -38.08, -54.95, -35.74, -53.93
         );
  _cs.Add(   3564, 'Slovenia - west of 14'#$00b0'30''E onshore',
           13.38, 45.44, 14.58, 46.53
         );
  _cs.Add(   3567, 'Slovenia - southeastern',
           14.53, 45.42, 15.36, 45.77
         );
  _cs.Add(   3569, 'Slovenia - Stajerska',
           14.74, 46.10, 16.27, 46.76
         );
end;

procedure prepare_25;
begin
  _cs.Add(   3638, 'South America - 84'#$00b0'W to 78'#$00b0'W, S hemisphere and SIRGAS95 by country',
           -84.00, -56.45, -75.21, 1.45
         );
  _cs.Add(   3830, 'South America - 84'#$00b0'W to 78'#$00b0'W, N hemisphere and SAD69 by country',
           -80.18, 0.00, -78.00, 2.70
         );
  _cs.Add(   3831, 'South America - 84'#$00b0'W to 78'#$00b0'W, S hemisphere and SAD69 by country',
           -81.41, -10.53, -78.00, 0.00
         );
  _cs.Add(   3832, 'South America - 78'#$00b0'W to 72'#$00b0'W, N hemisphere and SAD69 by country',
           -78.00, 0.00, -72.00, 12.31
         );
  _cs.Add(   3833, 'South America - 78'#$00b0'W to 72'#$00b0'W, S hemisphere and SAD69 by country',
           -78.00, -45.00, -71.99, 0.00
         );
  _cs.Add(   3834, 'South America - 72'#$00b0'W to 66'#$00b0'W, N hemisphere onshore',
           -72.00, 0.00, -66.00, 12.52
         );
  _cs.Add(   3835, 'South America - 72'#$00b0'W to 66'#$00b0'W, S hemisphere onshore',
           -72.00, -45.00, -65.99, 2.15
         );
  _cs.Add(   3839, 'South America - 66'#$00b0'W to 60'#$00b0'W, N hemisphere and SAD69 by country',
           -66.00, 0.64, -59.99, 11.23
         );
  _cs.Add(   3840, 'South America - 66'#$00b0'W to 60'#$00b0'W, S hemisphere and SAD69 by country',
           -66.00, -45.00, -59.99, 5.28
         );
  _cs.Add(   3841, 'South America - 60'#$00b0'W to 54'#$00b0'W, N hemisphere and SAD69 by country',
           -60.00, 1.18, -54.00, 8.60
         );
  _cs.Add(   3942, 'South Sudan',
           24.14, 3.49, 35.94, 12.22
         );
  _cs.Add(   4010, 'Solomon Islands',
           154.58, -16.13, 173.58, -4.14
         );
  _cs.Add(   4016, 'South America - onshore north of 45'#$00b0'S',
           -81.41, -45.00, -34.74, 12.52
         );
  _cs.Add(   4567, 'South Africa - mainland - onshore and offshore',
           13.33, -38.17, 36.54, -22.13
         );
  _cs.Add(   4568, 'South Africa - Prince Edward islands - onshore and offshore',
           32.71, -50.32, 42.85, -43.26
         );
  _cs.Add(   4602, 'Spain - Balearic Islands - Mallorca',
           2.23, 39.07, 3.55, 40.02
         );
  _cs.Add(   4603, 'Spain - Balearic Islands - Menorca',
           3.73, 39.75, 4.39, 40.15
         );
  _cs.Add(   4604, 'Spain - Balearic Islands - Ibiza and Formentera',
           1.12, 38.59, 1.68, 39.17
         );
  _cs.Add(   1200, 'St Kitts and Nevis',
           -63.63, 16.34, -62.20, 17.67
         );
  _cs.Add(   1201, 'St Lucia',
           -62.50, 13.23, -59.99, 14.28
         );
  _cs.Add(   1202, 'St Vincent and the Grenadines',
           -63.38, 12.03, -60.28, 14.09
         );
  _cs.Add(   1218, 'Sri Lanka',
           77.02, 2.58, 85.24, 11.45
         );
  _cs.Add(   1219, 'St Helena, Ascension and Tristan da Cunha',
           -17.79, -43.71, -2.16, -4.55
         );
  _cs.Add(   1220, 'St Pierre and Miquelon',
           -57.10, 43.41, -55.90, 47.37
         );
  _cs.Add(   1221, 'Sudan',
           21.82, 8.64, 39.76, 22.24
         );
  _cs.Add(   1222, 'Suriname',
           -58.08, 1.83, -52.66, 9.35
         );
  _cs.Add(   1223, 'Svalbard and Jan Mayen',
           -16.22, 66.24, 35.02, 85.05
         )         .MarkDeprecated( True );
  _cs.Add(   1225, 'Sweden',
           10.03, 54.96, 24.17, 69.07
         );
  _cs.Add(   1226, 'Switzerland',
           5.96, 45.82, 10.49, 47.81
         );
  _cs.Add(   1227, 'Syria',
           34.96, 32.31, 42.38, 37.30
         );
  _cs.Add(   3319, 'Togo - onshore',
           -0.15, 6.05, 1.80, 11.14
         );
  _cs.Add(   3320, 'Tokelau - onshore',
           -171.92, -9.27, -171.79, -9.12
         );
  _cs.Add(   3408, 'Sweden - Stockholm',
           17.75, 59.22, 18.20, 59.43
         );
  _cs.Add(   3429, 'Spain - mainland and Balearic Islands onshore',
           -9.37, 35.26, 4.39, 43.82
         );
  _cs.Add(   3562, 'Taiwan - 120'#$00b0'E to 122'#$00b0'E',
           119.99, 20.41, 122.06, 26.72
         );
  _cs.Add(   3563, 'Taiwan - 118'#$00b0'E to 120'#$00b0'E',
           118.00, 18.63, 120.00, 24.65
         );
  _cs.Add(   3591, 'Taiwan - onshore - Penghu',
           119.25, 23.12, 119.78, 23.82
         );
  _cs.Add(   3608, 'Sweden - Stockholm county',
           17.25, 58.69, 19.61, 60.27
         );
  _cs.Add(   3629, 'Spain - Canary Islands - west of 18'#$00b0'W',
           -21.93, 24.60, -18.00, 31.19
         );
  _cs.Add(   3630, 'Spain - Canary Islands - east of 18'#$00b0'W',
           -18.00, 25.25, -11.75, 32.76
         );
  _cs.Add(   3732, 'Spain - Catalonia onshore',
           0.16, 40.49, 3.39, 42.86
         );
  _cs.Add(   3735, 'Thailand - onshore east of 102'#$00b0'E',
           102.00, 6.02, 105.64, 18.44
         );
  _cs.Add(   3741, 'Thailand - onshore and Gulf of Thailand',
           97.34, 5.63, 105.64, 20.46
         );
  _cs.Add(   3809, 'St Maarten',
           -63.30, 17.81, -62.92, 18.07
         );
  _cs.Add(   3873, 'Spain - Canary Islands onshore',
           -18.22, 27.58, -13.37, 29.47
         );
  _cs.Add(   3928, 'Sudan - onshore',
           21.82, 8.64, 38.66, 22.24
         );
  _cs.Add(   3982, 'Taiwan - onshore - mainland',
           119.99, 21.87, 122.06, 25.34
         );
  _cs.Add(   4188, 'Spain and Gibraltar - onshore',
           -9.37, 35.95, 3.39, 43.82
         );
  _cs.Add(   4393, 'To be specified',
           -180.00, -90.00, 180.00, 90.00
         );
  _cs.Add(   4590, 'Spain - Ceuta',
           -5.40, 35.82, -5.24, 35.97
         );
  _cs.Add(   4591, 'Spain - Canary Islands - Lanzarote',
           -13.95, 28.78, -13.37, 29.47
         );
  _cs.Add(   4592, 'Spain - Canary Islands - Fuerteventura',
           -14.58, 27.99, -13.75, 28.81
         );
  _cs.Add(   4593, 'Spain - Canary Islands - Gran Canaria',
           -15.88, 27.68, -15.31, 28.23
         );
  _cs.Add(   4594, 'Spain - Canary Islands - Tenerife',
           -16.96, 27.93, -16.08, 28.63
         );
  _cs.Add(   4595, 'Spain - Canary Islands - La Gomera',
           -17.39, 27.95, -17.03, 28.26
         );
  _cs.Add(   4596, 'Spain - Canary Islands - La Palma',
           -18.06, 28.40, -17.66, 28.90
         );
  _cs.Add(   4597, 'Spain - Canary Islands - El Hierro',
           -18.22, 27.58, -17.83, 27.90
         );
  _cs.Add(   4598, 'Spain - Canary Islands western',
           -18.22, 27.58, -16.08, 28.90
         );
  _cs.Add(   4599, 'Spain - Canary Islands - El Hierro west of 18'#$00b0'W',
           -18.22, 27.59, -18.00, 27.88
         );
  _cs.Add(   4600, 'Spain - Canary Islands onshore east of 18'#$00b0'W',
           -18.06, 27.58, -13.37, 29.47
         );
  _cs.Add(   4601, 'Spain - Canary Islands western east of 18'#$00b0'W',
           -18.06, 27.58, -16.08, 28.90
         );
  _cs.Add(   4605, 'Spain - mainland onshore and Ceuta',
           -9.37, 35.82, 3.39, 43.82
         );
  _cs.Add(   4657, 'St Martin',
           -63.66, 17.87, -62.73, 18.19
         );
  _cs.Add(   4658, 'St Barthelemy',
           -63.10, 17.64, -62.21, 18.31
         );
  _cs.Add(   1234, 'Tonga',
           -179.08, -25.68, -171.28, -14.14
         );
  _cs.Add(   1235, 'Trinidad and Tobago',
           -62.09, 9.83, -57.28, 12.34
         );
  _cs.Add(   1236, 'Tunisia',
           7.49, 30.23, 13.67, 38.41
         );
  _cs.Add(   1237, 'Turkey',
           25.62, 34.42, 44.83, 43.45
         );
  _cs.Add(   1238, 'Turkmenistan',
           51.30, 35.14, 66.68, 42.80
         );
  _cs.Add(   1239, 'Turks and Caicos Islands',
           -72.82, 20.54, -67.66, 25.07
         );
  _cs.Add(   1240, 'Tuvalu',
           172.73, -13.24, -176.71, -4.00
         );
  _cs.Add(   1241, 'Uganda',
           29.57, -1.48, 35.01, 4.23
         );
  _cs.Add(   1242, 'Ukraine',
           22.15, 43.18, 40.18, 52.38
         );
  _cs.Add(   1243, 'UAE',
           51.50, 22.63, 57.13, 26.27
         );
  _cs.Add(   1244, 'UK',
           -14.89, 47.42, 3.40, 63.89
         );
  _cs.Add(   1264, 'UK - Great Britain onshore and nearshore; Isle of Man',
           -8.82, 49.80, 1.92, 60.94
         );
  _cs.Add(   1322, 'Trinidad and Tobago - Tobago - onshore',
           -60.90, 11.08, -60.44, 11.41
         );
  _cs.Add(   1339, 'Trinidad and Tobago - Trinidad',
           -62.09, 9.83, -60.00, 11.51
         );
  _cs.Add(   1363, 'UAE - Abu Dhabi and Dubai - onshore',
           51.56, 22.63, 56.03, 25.34
         );
  _cs.Add(   1489, 'Tunisia - offshore',
           7.81, 33.22, 13.67, 38.41
         );
  _cs.Add(   1524, 'Turkey - west of 28.5'#$00b0'E onshore',
           25.62, 36.50, 28.50, 42.11
         );
  _cs.Add(   1525, 'Turkey - 28.5'#$00b0'E to 31.5'#$00b0'E onshore',
           28.50, 36.06, 31.50, 41.46
         );
  _cs.Add(   1526, 'Turkey - 31.5'#$00b0'E to 34.5'#$00b0'E onshore',
           31.50, 35.97, 34.50, 42.07
         );
  _cs.Add(   1527, 'Turkey - 34.5'#$00b0'E to 37.5'#$00b0'E onshore',
           34.50, 35.81, 37.50, 42.15
         );
  _cs.Add(   1528, 'Turkey - 37.5'#$00b0'E to 40.5'#$00b0'E onshore',
           37.50, 36.66, 40.50, 41.19
         );
  _cs.Add(   1529, 'Turkey - 40.5'#$00b0'E to 43.5'#$00b0'E onshore',
           40.50, 37.02, 43.50, 41.60
         );
  _cs.Add(   1530, 'Turkey - east of 43.5'#$00b0'E',
           43.50, 36.97, 44.83, 41.02
         );
  _cs.Add(   1578, 'Uganda - north of equator and west of 30'#$00b0'E',
           29.71, 0.00, 30.00, 0.86
         );
  _cs.Add(   1618, 'Tunisia - onshore',
           7.49, 30.23, 11.59, 37.40
         );
  _cs.Add(   1619, 'Tunisia - north of 34'#$00b0'39''N',
           8.18, 34.65, 11.37, 37.40
         );
  _cs.Add(   1620, 'Tunisia - south of 34'#$00b0'39''N',
           7.49, 30.23, 11.59, 34.66
         );
  _cs.Add(   1629, 'UK - offshore - North Sea',
           -5.05, 51.03, 3.40, 62.03
         );
  _cs.Add(   1750, 'UAE - east of 54'#$00b0'E',
           54.00, 22.63, 57.13, 26.27
         );
  _cs.Add(   1850, 'UAE - Abu Dhabi - onshore west of 54'#$00b0'E',
           51.56, 22.76, 54.01, 24.32
         );
  _cs.Add(   2392, 'UAE - Abu al Bu Khoosh',
           53.03, 25.33, 53.40, 25.54
         );
  _cs.Add(   2394, 'UK - Great Britain and UKCS',
           -6.00, 49.15, 3.40, 63.83
         )         .MarkDeprecated( True );
  _cs.Add(   2395, 'UK - England',
           -6.50, 49.81, 1.84, 55.85
         );
  _cs.Add(   2396, 'UK - England and Wales, Isle of Man',
           -6.50, 49.81, 1.84, 55.85
         );
  _cs.Add(   2397, 'UK - Scotland',
           -8.74, 54.57, -0.65, 60.90
         );
  _cs.Add(   2398, 'UK - Wales',
           -5.34, 51.28, -2.65, 53.48
         );
end;

procedure prepare_26;
begin
  _cs.Add(   2530, 'UK - Northern Ireland - onshore',
           -8.18, 53.96, -5.34, 55.36
         );
  _cs.Add(   2792, 'UK - Great Britain mainland onshore',
           -7.06, 49.93, 1.80, 58.71
         );
  _cs.Add(   2793, 'UK - Orkney Islands onshore',
           -3.48, 58.72, -2.34, 59.41
         );
  _cs.Add(   2794, 'UK - Fair Isle onshore',
           -1.76, 59.45, -1.50, 59.60
         );
  _cs.Add(   2795, 'UK - Shetland Islands onshore',
           -1.78, 59.83, -0.67, 60.87
         );
  _cs.Add(   2796, 'UK - Foula onshore',
           -2.21, 60.06, -1.95, 60.20
         );
  _cs.Add(   2797, 'UK - Sule Skerry onshore',
           -4.50, 59.05, -4.30, 59.13
         );
  _cs.Add(   2798, 'UK - North Rona onshore',
           -5.92, 59.07, -5.73, 59.19
         );
  _cs.Add(   2799, 'UK - Outer Hebrides onshore',
           -7.72, 56.76, -6.10, 58.54
         );
  _cs.Add(   2800, 'UK - St. Kilda onshore',
           -8.74, 57.74, -8.41, 57.93
         );
  _cs.Add(   2801, 'UK - Flannan Isles onshore',
           -7.75, 58.21, -7.46, 58.35
         );
  _cs.Add(   2802, 'UK - Scilly Isles onshore',
           -6.41, 49.86, -6.23, 49.99
         );
  _cs.Add(   3143, 'Trinidad and Tobago - Trinidad - onshore',
           -61.98, 9.99, -60.85, 10.90
         );
  _cs.Add(   3321, 'Tonga - onshore',
           -175.42, -21.32, -173.85, -18.51
         );
  _cs.Add(   3322, 'Turkey - onshore',
           25.62, 35.81, 44.83, 42.15
         );
  _cs.Add(   3323, 'Turks and Caicos Islands - onshore',
           -72.09, 21.38, -71.07, 22.01
         );
  _cs.Add(   3325, 'UAE - onshore',
           51.56, 22.63, 56.44, 26.10
         );
  _cs.Add(   3509, 'UAE - Abu Dhabi - offshore',
           51.50, 24.00, 54.85, 25.64
         );
  _cs.Add(   3530, 'UAE - Dubai - offshore',
           54.06, 24.94, 55.30, 25.80
         );
  _cs.Add(   3531, 'UAE - Dubai municipality',
           54.84, 24.85, 55.55, 25.34
         );
  _cs.Add(   3893, 'UK - Wytch Farm',
           -2.20, 50.53, -1.68, 50.80
         );
  _cs.Add(   3898, 'Ukraine - 24'#$00b0'E to 30'#$00b0'E',
           24.00, 45.10, 30.00, 51.96
         );
  _cs.Add(   3903, 'Ukraine - 30'#$00b0'E to 36'#$00b0'E',
           30.00, 43.18, 36.00, 52.38
         );
  _cs.Add(   3905, 'Ukraine - east of 36'#$00b0'E',
           36.00, 43.43, 40.18, 50.44
         );
  _cs.Add(   3907, 'Ukraine - 22.5'#$00b0'E to 25.5'#$00b0'E',
           22.50, 47.71, 25.50, 51.96
         );
  _cs.Add(   3908, 'Ukraine - 25.5'#$00b0'E to 28.5'#$00b0'E',
           25.50, 45.26, 28.50, 51.94
         );
  _cs.Add(   3909, 'Ukraine - 28.5'#$00b0'E to 31.5'#$00b0'E',
           28.50, 43.42, 31.50, 52.12
         );
  _cs.Add(   3910, 'Ukraine - 31.5'#$00b0'E to 34.5'#$00b0'E',
           31.50, 43.18, 34.50, 52.38
         );
  _cs.Add(   3912, 'Ukraine - 34.5'#$00b0'E to 37.5'#$00b0'E',
           34.50, 43.24, 37.50, 51.25
         );
  _cs.Add(   3913, 'Ukraine - east of 37.5'#$00b0'E',
           37.50, 46.77, 40.18, 50.39
         );
  _cs.Add(   4011, 'Tuvalu - onshore',
           176.24, -8.62, 179.29, -6.03
         );
  _cs.Add(   4022, 'UAE - Abu Dhabi and Dubai - onshore east of 54'#$00b0'E',
           53.99, 22.63, 56.03, 25.34
         );
  _cs.Add(   4225, 'UAE - Abu Dhabi - onshore',
           51.56, 22.63, 56.03, 24.95
         );
  _cs.Add(   4226, 'UAE - Abu Dhabi',
           51.50, 22.63, 56.03, 25.64
         );
  _cs.Add(   4227, 'UAE - Abu Dhabi - onshore east of 54'#$00b0'E',
           53.99, 22.63, 56.03, 24.95
         );
  _cs.Add(   4229, 'UAE - Abu Dhabi island',
           54.20, 24.24, 54.71, 24.64
         );
  _cs.Add(   4390, 'UK - Britain and UKCS 49'#$00b0'45''N to 61'#$00b0'N, 9'#$00b0'W to 2'#$00b0'E',
           -9.00, 49.75, 2.01, 61.01
         );
  _cs.Add(   4391, 'UK - offshore 49'#$00b0'45''N to 61'#$00b0'N, 9'#$00b0'W to 2'#$00b0'E',
           -9.00, 49.75, 2.01, 61.01
         );
  _cs.Add(   4429, 'Ukraine - 25'#$00b0'E to 28'#$00b0'E',
           25.00, 47.72, 28.01, 51.96
         );
  _cs.Add(   4430, 'Ukraine - 28'#$00b0'E to 31'#$00b0'E onshore',
           28.00, 45.18, 31.01, 52.09
         );
  _cs.Add(   4431, 'Ukraine - 31'#$00b0'E to 34'#$00b0'E onshore',
           31.00, 44.32, 34.01, 52.38
         );
  _cs.Add(   4432, 'Ukraine - 34'#$00b0'E to 37'#$00b0'E onshore',
           34.00, 44.33, 37.01, 52.25
         );
  _cs.Add(   4433, 'Ukraine - 37'#$00b0'E to 40'#$00b0'E onshore',
           37.00, 46.80, 40.01, 50.44
         );
  _cs.Add(   4434, 'Ukraine - east of 40'#$00b0'E',
           40.00, 48.80, 40.18, 49.62
         );
  _cs.Add(   4467, 'Trinidad and Tobago - offshore west of 60'#$00b0'W',
           -62.09, 9.83, -59.99, 12.34
         );
  _cs.Add(   4468, 'Trinidad and Tobago - offshore east of 60'#$00b0'W',
           -60.00, 9.95, -57.28, 12.19
         );
  _cs.Add(   4582, 'UK - London to Birmingham and Crewe',
           -2.75, 51.45, 0.00, 53.30
         );
  _cs.Add(   4583, 'UK - Liverpool to Leeds',
           -3.14, 53.32, -1.35, 53.90
         );
  _cs.Add(   4588, 'UK - London to Sheffield',
           -1.89, 51.46, 0.15, 53.41
         );
  _cs.Add(   4589, 'UK - Aberdeen to Inverness',
           -4.30, 57.10, -2.10, 57.70
         );
  _cs.Add(   4607, 'UK - Glasgow to Kilmarnock',
           -4.65, 55.55, -4.05, 55.95
         );
  _cs.Add(   4616, 'UK - Great Britain onshore; Isle of Man',
           -8.69, 49.81, 1.90, 60.93
         );
  _cs.Add(   4620, 'UK - Tweedmouth to Aberdeen',
           -3.55, 55.55, -1.95, 57.20
         );
  _cs.Add(   4621, 'UK - Newcastle to Ashington',
           -1.90, 54.85, -1.30, 55.30
         );
  _cs.Add(   4622, 'UK - Oxford to Bedford',
           -1.43, 51.70, -0.36, 52.24
         );
  _cs.Add(   4624, 'Ukraine - Cherkasy oblast',
           29.60, 48.45, 32.89, 50.23
         );
  _cs.Add(   4625, 'Ukraine - Chernihiv oblast',
           30.48, 50.34, 33.51, 52.38
         );
  _cs.Add(   4626, 'Ukraine - Chernivtsi oblast',
           24.90, 47.72, 27.53, 48.68
         );
  _cs.Add(   4627, 'Ukraine - Dnipropetrovsk oblast',
           32.95, 47.45, 36.94, 49.19
         );
  _cs.Add(   4628, 'Ukraine - Donetsk oblast',
           36.54, 46.86, 39.09, 49.23
         );
  _cs.Add(   4629, 'Ukraine - Ivano-Frankivsk oblast',
           23.53, 47.72, 25.65, 49.56
         );
  _cs.Add(   4648, 'Ukraine - Crimea',
           32.47, 44.38, 36.65, 46.24
         );
  _cs.Add(   4652, 'UK - Cardiff to Lincoln',
           -3.27, 51.35, -0.36, 53.26
         );
  _cs.Add(   4655, 'UK - Manchester to Dore',
           -2.40, 53.25, -1.39, 53.55
         );
  _cs.Add(   1245, 'USA',
           167.65, 15.56, -65.69, 74.71
         );
  _cs.Add(   1246, 'United States Minor Outlying Islands',
           163.07, -3.74, -157.41, 31.80
         );
  _cs.Add(   1247, 'Uruguay',
           -58.49, -37.77, -50.01, -30.09
         );
  _cs.Add(   1330, 'USA - Alaska',
           172.42, 51.30, -129.99, 71.40
         );
  _cs.Add(   1331, 'USA - Alaska - St. George Island',
           -169.88, 56.49, -169.38, 56.67
         );
  _cs.Add(   1332, 'USA - Alaska - St. Lawrence Island',
           -171.97, 62.89, -168.59, 63.84
         );
  _cs.Add(   1333, 'USA - Alaska - St. Paul Island',
           -170.51, 57.06, -170.04, 57.28
         );
  _cs.Add(   1372, 'USA - Alabama',
           -88.48, 30.14, -84.89, 35.02
         );
  _cs.Add(   1373, 'USA - Arizona',
           -114.81, 31.33, -109.04, 37.01
         );
  _cs.Add(   2133, 'USA - 168'#$00b0'W to 162'#$00b0'W - AK, OCS',
           -168.00, 49.52, -161.99, 74.29
         );
  _cs.Add(   2134, 'USA - 162'#$00b0'W to 156'#$00b0'W - AK, OCS',
           -162.00, 50.98, -155.99, 74.71
         );
  _cs.Add(   2135, 'USA - 156'#$00b0'W to 150'#$00b0'W - AK, OCS',
           -156.00, 52.15, -149.99, 74.71
         );
  _cs.Add(   2136, 'USA - 150'#$00b0'W to 144'#$00b0'W - AK, OCS',
           -150.00, 54.05, -143.99, 74.13
         );
  _cs.Add(   2154, 'USA - Alabama - SPCS - E',
           -86.79, 30.99, -84.89, 35.00
         );
  _cs.Add(   2155, 'USA - Alabama - SPCS - W',
           -88.48, 30.14, -86.30, 35.02
         );
  _cs.Add(   2156, 'USA - Alaska - Panhandle',
           -141.00, 54.61, -129.99, 60.35
         );
  _cs.Add(   2157, 'USA - Alaska - Aleutian Islands',
           172.42, 51.30, -164.84, 54.34
         );
  _cs.Add(   2158, 'USA - Alaska - 144'#$00b0'W to 141'#$00b0'W',
           -144.01, 59.72, -140.98, 70.16
         );
  _cs.Add(   2159, 'USA - Alaska - 148'#$00b0'W to 144'#$00b0'W',
           -148.00, 59.72, -144.00, 70.38
         );
  _cs.Add(   2160, 'USA - Alaska - 152'#$00b0'W to 148'#$00b0'W',
           -152.01, 59.11, -147.99, 70.63
         );
  _cs.Add(   2161, 'USA - Alaska - 156'#$00b0'W to 152'#$00b0'W',
           -156.00, 55.72, -151.86, 71.28
         );
  _cs.Add(   2162, 'USA - Alaska - 160'#$00b0'W to 156'#$00b0'W',
           -160.00, 54.89, -155.99, 71.40
         );
  _cs.Add(   2163, 'USA - Alaska - 164'#$00b0'W to 160'#$00b0'W',
           -164.01, 54.32, -160.00, 70.74
         );
  _cs.Add(   2164, 'USA - Alaska - north of 54.5'#$00b0'N; 168'#$00b0'W to 164'#$00b0'W',
           -168.26, 54.34, -164.00, 69.05
         );
  _cs.Add(   2165, 'USA - Alaska - north of 54.5'#$00b0'N; west of 168'#$00b0'W',
           -173.16, 56.49, -168.00, 65.82
         );
  _cs.Add(   2373, 'USA - Alaska including EEZ',
           167.65, 47.88, -129.99, 74.71
         );
  _cs.Add(   2387, 'USA - Alaska - Aleutian Islands east of 180'#$00b0'E',
           -178.30, 51.54, -164.84, 54.34
         );
  _cs.Add(   2388, 'USA - Alaska - Aleutian Islands west of 180'#$00b0'W',
           172.42, 51.30, 179.86, 53.07
         );
  _cs.Add(   2412, 'USA - Alaska mainland',
           -168.26, 54.34, -129.99, 71.40
         );
  _cs.Add(   3324, 'Ukraine - onshore',
           22.15, 44.32, 40.18, 52.38
         );
  _cs.Add(   3326, 'Uruguay - onshore',
           -58.49, -35.00, -53.09, -30.09
         );
  _cs.Add(   3373, 'USA - 174'#$00b0'E to 180'#$00b0'E - AK, OCS',
           174.00, 47.92, 180.00, 56.67
         );
  _cs.Add(   3374, 'USA - 180'#$00b0'W to 174'#$00b0'W - AK, OCS',
           -180.00, 47.88, -173.99, 63.21
         );
  _cs.Add(   3375, 'USA - 174'#$00b0'W to 168'#$00b0'W - AK, OCS',
           -174.00, 48.66, -167.99, 73.05
         );
  _cs.Add(   3483, 'USA - 174'#$00b0'E to 180'#$00b0'E - AK',
           174.00, 48.00, 180.00, 59.80
         )         .MarkDeprecated( True );
  _cs.Add(   3484, 'USA - 180'#$00b0'W to 174'#$00b0'W - AK',
           -180.00, 48.00, -174.00, 63.30
         )         .MarkDeprecated( True );
end;

procedure prepare_27;
begin
  _cs.Add(   3485, 'USA - 174'#$00b0'W to 168'#$00b0'W - AK',
           -174.00, 48.70, -168.00, 73.00
         )         .MarkDeprecated( True );
  _cs.Add(   3486, 'USA - 168'#$00b0'W to 162'#$00b0'W - AK',
           -168.00, 49.60, -162.00, 74.30
         )         .MarkDeprecated( True );
  _cs.Add(   3487, 'USA - 162'#$00b0'W to 156'#$00b0'W - AK',
           -162.00, 51.10, -156.00, 74.70
         )         .MarkDeprecated( True );
  _cs.Add(   3488, 'USA - 162'#$00b0'W to 156'#$00b0'W onshore - HI',
           -160.30, 19.51, -156.00, 22.29
         );
  _cs.Add(   3489, 'USA - 162'#$00b0'W to 156'#$00b0'W - AK, HI',
           -162.00, 15.57, -155.99, 74.71
         );
  _cs.Add(   3490, 'USA - 156'#$00b0'W to 150'#$00b0'W - AK',
           -156.00, 52.10, -150.00, 74.70
         )         .MarkDeprecated( True );
  _cs.Add(   3491, 'USA - 156'#$00b0'W to 150'#$00b0'W onshore - HI',
           -156.00, 18.87, -154.74, 20.86
         );
  _cs.Add(   3492, 'USA - 156'#$00b0'W to 150'#$00b0'W - AK, HI',
           -156.00, 15.56, -149.99, 74.71
         );
  _cs.Add(   3493, 'USA - 150'#$00b0'W to 144'#$00b0'W - AK',
           -150.00, 54.00, -144.00, 74.20
         )         .MarkDeprecated( True );
  _cs.Add(   3494, 'USA - 144'#$00b0'W to 138'#$00b0'W',
           -144.00, 53.47, -137.99, 73.59
         );
  _cs.Add(   3495, 'USA - 138'#$00b0'W to 132'#$00b0'W',
           -138.00, 53.60, -131.99, 73.04
         );
  _cs.Add(   3496, 'USA - 132'#$00b0'W to 126'#$00b0'W',
           -132.00, 35.38, -126.00, 56.84
         );
  _cs.Add(   3497, 'USA - 126'#$00b0'W to 120'#$00b0'W',
           -126.00, 30.54, -119.99, 49.09
         );
  _cs.Add(   3498, 'USA - 120'#$00b0'W to 114'#$00b0'W',
           -120.00, 30.88, -114.00, 49.01
         );
  _cs.Add(   3499, 'USA - 114'#$00b0'W to 108'#$00b0'W',
           -114.00, 31.33, -108.00, 49.01
         );
  _cs.Add(   3500, 'USA - 108'#$00b0'W to 102'#$00b0'W',
           -108.00, 28.98, -102.00, 49.01
         );
  _cs.Add(   3501, 'USA - 102'#$00b0'W to 96'#$00b0'W',
           -102.00, 25.83, -96.00, 49.01
         );
  _cs.Add(   3502, 'USA - 96'#$00b0'W to 90'#$00b0'W',
           -96.01, 25.61, -90.00, 49.38
         );
  _cs.Add(   3503, 'USA - 90'#$00b0'W to 84'#$00b0'W',
           -90.00, 23.97, -84.00, 48.32
         );
  _cs.Add(   3504, 'USA - 84'#$00b0'W to 78'#$00b0'W',
           -84.00, 23.81, -78.00, 46.13
         );
  _cs.Add(   3505, 'USA - 78'#$00b0'W to 72'#$00b0'W',
           -78.00, 28.28, -72.00, 45.03
         );
  _cs.Add(   3506, 'USA - 72'#$00b0'W to 66'#$00b0'W',
           -72.00, 33.61, -65.99, 47.47
         );
  _cs.Add(   3637, 'USA - 102'#$00b0'W to 96'#$00b0'W and GoM OCS',
           -102.00, 25.83, -95.87, 49.01
         );
  _cs.Add(   3640, 'USA - 96'#$00b0'W to 90'#$00b0'W and GoM OCS',
           -96.01, 25.61, -89.86, 49.38
         );
  _cs.Add(   3641, 'USA - 90'#$00b0'W to 84'#$00b0'W and GoM OCS',
           -90.01, 23.95, -83.91, 48.32
         );
  _cs.Add(   3642, 'USA - 84'#$00b0'W to 78'#$00b0'W and GoM OCS',
           -84.09, 23.81, -77.99, 46.13
         );
  _cs.Add(   3826, 'Uruguay - west of 54'#$00b0'W',
           -58.49, -36.63, -54.00, -30.09
         );
  _cs.Add(   3828, 'Uruguay - east of 54'#$00b0'W',
           -54.00, -37.77, -50.01, -31.90
         );
  _cs.Add(   3852, 'USA - 120'#$00b0'W to 114'#$00b0'W onshore',
           -120.00, 32.26, -114.00, 49.01
         );
  _cs.Add(   3857, 'USA - 126'#$00b0'W to 120'#$00b0'W onshore',
           -124.79, 33.85, -119.99, 49.05
         );
  _cs.Add(   3860, 'USA - 102'#$00b0'W to 96'#$00b0'W onshore',
           -102.00, 25.83, -95.99, 49.01
         );
  _cs.Add(   3861, 'USA - 96'#$00b0'W to 90'#$00b0'W onshore',
           -96.00, 28.42, -89.99, 49.38
         );
  _cs.Add(   3862, 'USA - 90'#$00b0'W to 84'#$00b0'W onshore',
           -90.00, 28.85, -83.99, 48.32
         );
  _cs.Add(   3863, 'USA - 84'#$00b0'W to 78'#$00b0'W onshore',
           -84.01, 24.41, -78.00, 46.13
         );
  _cs.Add(   3868, 'USA - 78'#$00b0'W to 72'#$00b0'W onshore',
           -78.00, 33.84, -72.00, 45.03
         );
  _cs.Add(   3871, 'USA - 72'#$00b0'W to 66'#$00b0'W onshore',
           -72.00, 40.96, -66.91, 47.47
         );
  _cs.Add(   3895, 'Ukraine - west of 24'#$00b0'E',
           22.15, 47.95, 24.00, 51.66
         );
  _cs.Add(   3906, 'Ukraine - west of 22.5'#$00b0'E',
           22.15, 48.24, 22.50, 48.98
         );
  _cs.Add(   4435, 'Ukraine - west of 25'#$00b0'E',
           22.15, 47.71, 25.01, 51.92
         );
  _cs.Add(   4460, 'USA - Arizona - Pima county central',
           -112.52, 31.50, -111.56, 32.51
         );
  _cs.Add(   4472, 'USA - Arizona - Pima county east',
           -111.57, 31.43, -110.44, 32.52
         );
  _cs.Add(   4473, 'USA - Arizona - Pima county Mt. Lemmon',
           -110.87, 32.33, -110.61, 32.49
         );
  _cs.Add(   4623, 'Ukraine - Kyiv city',
           30.23, 50.21, 30.83, 50.59
         );
  _cs.Add(   4630, 'Ukraine - Kharkiv oblast',
           34.84, 48.52, 38.10, 50.46
         );
  _cs.Add(   4631, 'Ukraine - Kherson oblast',
           31.51, 45.75, 35.10, 47.60
         );
  _cs.Add(   4632, 'Ukraine - Khmelnytskyi oblast',
           26.13, 48.45, 27.90, 50.59
         );
  _cs.Add(   4633, 'Ukraine - Kyiv oblast',
           29.26, 49.17, 32.16, 51.55
         );
  _cs.Add(   4634, 'Ukraine - Kirovohrad oblast',
           29.74, 47.74, 33.90, 49.25
         );
  _cs.Add(   4635, 'Ukraine - Luhansk oblast',
           37.83, 47.82, 40.23, 50.09
         );
  _cs.Add(   4636, 'Ukraine - Lviv oblast',
           22.64, 48.71, 25.42, 50.65
         );
  _cs.Add(   4637, 'Ukraine - Mykolaiv oblast',
           30.20, 46.36, 33.18, 48.23
         );
  _cs.Add(   4638, 'Ukraine - Odessa oblast',
           28.21, 45.20, 31.29, 48.23
         );
  _cs.Add(   4639, 'Ukraine - Poltava oblast',
           32.08, 48.74, 35.48, 50.55
         );
  _cs.Add(   4640, 'Ukraine - Rivne oblast',
           25.08, 50.00, 27.73, 51.95
         );
  _cs.Add(   4641, 'Ukraine - Sumy oblast',
           32.93, 50.10, 35.67, 52.37
         );
  _cs.Add(   4642, 'Ukraine - Ternopil oblast',
           24.71, 48.50, 26.44, 50.27
         );
  _cs.Add(   4643, 'Ukraine - Vinnytsia oblast',
           27.37, 48.06, 30.02, 49.89
         );
  _cs.Add(   4644, 'Ukraine - Volyn oblast',
           23.60, 50.28, 26.10, 51.97
         );
  _cs.Add(   4645, 'Ukraine - Zakarpattia oblast',
           22.13, 47.89, 24.62, 49.10
         );
  _cs.Add(   4646, 'Ukraine - Zaporizhzhia oblast',
           34.17, 46.07, 37.24, 48.14
         );
  _cs.Add(   4647, 'Ukraine - Zhytomyr oblast',
           27.19, 49.58, 29.73, 51.68
         );
  _cs.Add(   4649, 'Ukraine - Sevastopol',
           33.37, 44.38, 33.92, 44.84
         );
  _cs.Add(   4650, 'Ukraine - Kyiv city and oblast',
           29.26, 49.17, 32.16, 51.55
         );
  _cs.Add(   4651, 'Ukraine - Rivne and Khmelnytskyi oblasts',
           25.08, 48.45, 27.90, 51.95
         );
  _cs.Add(   1323, 'USA - CONUS - onshore',
           -124.79, 24.41, -66.91, 49.38
         );
  _cs.Add(   1334, 'USA - Hawaii - onshore',
           -160.30, 18.87, -154.74, 22.29
         );
  _cs.Add(   1337, 'USA - HARN',
           144.58, -14.59, -64.51, 71.40
         );
  _cs.Add(   1374, 'USA - Arkansas',
           -94.62, 33.01, -89.64, 36.50
         );
  _cs.Add(   1375, 'USA - California',
           -124.45, 32.53, -114.12, 42.01
         );
  _cs.Add(   1376, 'USA - Colorado',
           -109.06, 36.98, -102.04, 41.01
         );
  _cs.Add(   1377, 'USA - Connecticut',
           -73.73, 40.98, -71.78, 42.05
         );
  _cs.Add(   1378, 'USA - Delaware',
           -75.80, 38.44, -74.97, 39.85
         );
  _cs.Add(   1379, 'USA - Florida',
           -87.63, 24.41, -79.97, 31.01
         );
  _cs.Add(   1380, 'USA - Georgia',
           -85.61, 30.36, -80.77, 35.01
         );
  _cs.Add(   1381, 'USA - Idaho',
           -117.24, 41.99, -111.04, 49.01
         );
  _cs.Add(   1382, 'USA - Illinois',
           -91.52, 36.98, -87.02, 42.51
         );
  _cs.Add(   1383, 'USA - Indiana',
           -88.06, 37.77, -84.78, 41.77
         );
  _cs.Add(   1511, 'USA - CONUS and Alaska; PRVI',
           167.65, 14.92, -63.88, 74.71
         );
  _cs.Add(   1546, 'USA - Hawaii - island of Hawaii - onshore',
           -156.10, 18.87, -154.74, 20.33
         );
  _cs.Add(   1547, 'USA - Hawaii - Maui; Kahoolawe; Lanai; Molokai - onshore',
           -157.36, 20.45, -155.93, 21.26
         );
  _cs.Add(   1548, 'USA - Hawaii - Oahu - onshore',
           -158.33, 21.20, -157.61, 21.75
         );
  _cs.Add(   1549, 'USA - Hawaii - Kauai - onshore',
           -159.85, 21.81, -159.23, 22.29
         );
  _cs.Add(   1550, 'USA - Hawaii - Niihau - onshore',
           -160.30, 21.73, -159.99, 22.07
         );
  _cs.Add(   2166, 'USA - Arizona - SPCS - C',
           -113.35, 31.33, -110.44, 37.01
         );
  _cs.Add(   2167, 'USA - Arizona - SPCS - E',
           -111.71, 31.33, -109.04, 37.01
         );
  _cs.Add(   2168, 'USA - Arizona - SPCS - W',
           -114.81, 32.05, -112.52, 37.00
         );
  _cs.Add(   2169, 'USA - Arkansas - SPCS - N',
           -94.62, 34.67, -89.64, 36.50
         );
  _cs.Add(   2170, 'USA - Arkansas - SPCS - S',
           -94.48, 33.01, -90.40, 35.10
         );
  _cs.Add(   2171, 'USA - GoM OCS - west of 96'#$00b0'W',
           -97.22, 25.97, -95.87, 28.43
         );
  _cs.Add(   2172, 'USA - GoM OCS - 96'#$00b0'W to 90'#$00b0'W',
           -96.00, 25.61, -89.86, 29.73
         );
  _cs.Add(   2173, 'USA - GoM OCS - 90'#$00b0'W to 84'#$00b0'W',
           -90.01, 23.95, -83.91, 30.25
         );
  _cs.Add(   2174, 'USA - GoM OCS - east of 84'#$00b0'W',
           -84.09, 23.82, -81.17, 29.94
         );
  _cs.Add(   2175, 'USA - California - SPCS - 1',
           -124.45, 39.59, -119.99, 42.01
         );
  _cs.Add(   2176, 'USA - California - SPCS - 2',
           -124.06, 38.02, -119.54, 40.16
         );
  _cs.Add(   2177, 'USA - California - SPCS - 3',
           -123.02, 36.73, -117.83, 38.71
         );
  _cs.Add(   2178, 'USA - California - SPCS - 4',
           -122.01, 35.78, -115.62, 37.58
         );
  _cs.Add(   2179, 'USA - California - SPCS27 - 5',
           -121.43, 32.76, -114.12, 35.81
         );
  _cs.Add(   2180, 'USA - California - SPCS - 6',
           -118.15, 32.53, -114.42, 34.08
         );
  _cs.Add(   2181, 'USA - California - SPCS27 - 7',
           -118.96, 33.66, -117.63, 34.83
         );
  _cs.Add(   2182, 'USA - California - SPCS83 - 5',
           -121.42, 32.76, -114.12, 35.81
         );
end;

procedure prepare_28;
begin
  _cs.Add(   2183, 'USA - Colorado - SPCS - C',
           -109.06, 38.14, -102.04, 40.09
         );
  _cs.Add(   2184, 'USA - Colorado - SPCS - N',
           -109.06, 39.56, -102.04, 41.01
         );
  _cs.Add(   2185, 'USA - Colorado - SPCS - S',
           -109.06, 36.98, -102.04, 38.68
         );
  _cs.Add(   2186, 'USA - Florida - SPCS - E',
           -82.33, 24.41, -79.97, 30.83
         );
  _cs.Add(   2187, 'USA - Florida - SPCS - N',
           -87.63, 29.21, -82.04, 31.01
         );
  _cs.Add(   2188, 'USA - Florida - SPCS - W',
           -83.34, 26.27, -81.13, 29.60
         );
  _cs.Add(   2189, 'USA - Georgia - SPCS - E',
           -83.47, 30.36, -80.77, 34.68
         );
  _cs.Add(   2190, 'USA - Georgia - SPCS - W',
           -85.61, 30.62, -82.99, 35.01
         );
  _cs.Add(   2191, 'USA - Idaho - SPCS - C',
           -115.30, 41.99, -112.68, 45.70
         );
  _cs.Add(   2192, 'USA - Idaho - SPCS - E',
           -113.24, 41.99, -111.04, 44.75
         );
  _cs.Add(   2193, 'USA - Idaho - SPCS - W',
           -117.24, 41.99, -114.32, 49.01
         );
  _cs.Add(   2194, 'USA - Illinois - SPCS - E',
           -89.28, 37.06, -87.02, 42.50
         );
  _cs.Add(   2195, 'USA - Illinois - SPCS - W',
           -91.52, 36.98, -88.93, 42.51
         );
  _cs.Add(   2297, 'USA - California - north of 36.5'#$00b0'N',
           -124.44, 36.50, -116.54, 42.01
         );
  _cs.Add(   2298, 'USA - California - south of 36.5'#$00b0'N',
           -121.98, 32.53, -114.12, 36.50
         );
  _cs.Add(   2374, 'USA - CONUS including EEZ',
           -129.16, 23.81, -65.69, 49.38
         );
  _cs.Add(   2377, 'USA - Delaware and Maryland',
           -79.49, 37.97, -74.97, 39.85
         );
  _cs.Add(   2382, 'USA - Idaho and Montana - east of 113'#$00b0'W',
           -113.00, 41.99, -104.04, 49.01
         );
  _cs.Add(   2383, 'USA - Idaho and Montana - west of 113'#$00b0'W',
           -117.24, 41.99, -113.00, 49.01
         );
  _cs.Add(   2389, 'USA - CONUS east of Mississippi River - onshore',
           -97.22, 24.41, -66.91, 49.38
         );
  _cs.Add(   2390, 'USA - CONUS west of Mississippi River - onshore',
           -124.79, 25.83, -89.64, 49.05
         );
  _cs.Add(   2424, 'USA - HARN',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   2948, 'USA - CONUS east of 89'#$00b0'W - onshore',
           -89.00, 24.41, -66.91, 48.32
         );
  _cs.Add(   2949, 'USA - CONUS 89'#$00b0'W-107'#$00b0'W - onshore',
           -107.00, 25.83, -89.00, 49.38
         );
  _cs.Add(   2950, 'USA - CONUS west of 107'#$00b0'W - onshore',
           -124.79, 31.33, -107.00, 49.05
         );
  _cs.Add(   2973, 'USA - CONUS south of 41'#$00b0'N, west of 112'#$00b0'W - onshore',
           -124.45, 31.64, -112.00, 41.00
         );
  _cs.Add(   2974, 'USA - CONUS south of 41'#$00b0'N, 112'#$00b0'W to 95'#$00b0'W - onshore',
           -112.00, 25.83, -94.99, 41.01
         );
  _cs.Add(   2975, 'USA - CONUS south of 41'#$00b0'N, 95'#$00b0'W to 78'#$00b0'W - onshore',
           -95.00, 24.41, -77.99, 41.01
         );
  _cs.Add(   2976, 'USA - CONUS south of 41'#$00b0'N, east of 78'#$00b0'W - onshore',
           -78.00, 33.84, -71.90, 41.01
         );
  _cs.Add(   2977, 'USA - CONUS north of 41'#$00b0'N, west of 112'#$00b0'W - onshore',
           -124.79, 41.00, -112.00, 49.05
         );
  _cs.Add(   2978, 'USA - CONUS north of 41'#$00b0'N, 112'#$00b0'W to 95'#$00b0'W',
           -112.00, 41.00, -95.00, 49.38
         );
  _cs.Add(   2979, 'USA - CONUS north of 41'#$00b0'N, 95'#$00b0'W to 78'#$00b0'W',
           -95.00, 41.00, -77.99, 49.37
         );
  _cs.Add(   2980, 'USA - CONUS north of 41'#$00b0'N, east of 78'#$00b0'W - onshore',
           -78.00, 41.00, -66.91, 47.47
         );
  _cs.Add(   3181, 'USA - Hawaii - Tern Island and Sorel Atoll',
           -166.36, 23.69, -166.03, 23.93
         );
  _cs.Add(   3357, 'USA - GoM OCS',
           -97.22, 23.82, -81.17, 30.25
         );
  _cs.Add(   3358, 'USA - GoM - east of 87.25'#$00b0'W',
           -87.25, 23.82, -81.17, 30.25
         );
  _cs.Add(   3359, 'USA - GoM - 95'#$00b0'W to 87.25'#$00b0'W',
           -95.00, 25.61, -87.25, 30.23
         );
  _cs.Add(   3360, 'USA - GoM - west of 95'#$00b0'W',
           -97.22, 25.97, -95.00, 28.97
         );
  _cs.Add(   3664, 'USA - CONUS and Alaska - onshore',
           -168.26, 24.41, -66.91, 71.40
         );
  _cs.Add(   3883, 'USA - Hawaii - main islands',
           -163.74, 15.56, -151.27, 25.58
         );
  _cs.Add(   4228, 'USA - California - San Francisco Bay Area',
           -123.56, 36.85, -121.20, 38.87
         );
  _cs.Add(   4256, 'USA - Indiana - Benton',
           -87.53, 40.47, -87.09, 40.74
         );
  _cs.Add(   4258, 'USA - Indiana - Carroll',
           -86.78, 40.43, -86.37, 40.74
         );
  _cs.Add(   4260, 'USA - Indiana - Clinton',
           -86.70, 40.17, -86.24, 40.44
         );
  _cs.Add(   4263, 'USA - Indiana - Boone and Hendricks',
           -86.70, 39.60, -86.24, 40.19
         );
  _cs.Add(   4265, 'USA - Indiana - Clay',
           -87.25, 39.16, -86.93, 39.61
         );
  _cs.Add(   4269, 'USA - Indiana - Daviess and Greene',
           -87.28, 38.49, -86.68, 39.18
         );
  _cs.Add(   4271, 'USA - Indiana - Dubois and Martin',
           -87.08, 38.20, -86.67, 38.91
         );
  _cs.Add(   4272, 'USA - Indiana - Crawford, Lawrence, Orange',
           -86.69, 38.10, -86.24, 39.00
         );
  _cs.Add(   4280, 'USA - Indiana - Elkhart, Kosciusko, Wabash',
           -86.08, 40.65, -85.63, 41.77
         );
  _cs.Add(   4283, 'USA - Indiana - DeKalb',
           -85.20, 41.26, -84.80, 41.54
         );
  _cs.Add(   4285, 'USA - Indiana - Allen',
           -85.34, 40.91, -84.80, 41.28
         );
  _cs.Add(   4286, 'USA - Indiana - Cass',
           -86.59, 40.56, -86.16, 40.92
         );
  _cs.Add(   4289, 'USA - Indiana - Adams',
           -85.08, 40.56, -84.80, 40.93
         );
  _cs.Add(   4291, 'USA - Indiana - Blackford and Delaware',
           -85.58, 40.07, -85.20, 40.57
         );
  _cs.Add(   4299, 'USA - Indiana - Decatur and Rush',
           -85.69, 39.13, -85.29, 39.79
         );
  _cs.Add(   4300, 'USA - Indiana - Fayette, Franklin, Union',
           -85.31, 39.26, -84.81, 39.79
         );
  _cs.Add(   4301, 'USA - Indiana - Brown',
           -86.39, 39.04, -86.07, 39.35
         );
  _cs.Add(   4302, 'USA - Indiana - Bartholomew',
           -86.09, 39.03, -85.68, 39.36
         );
  _cs.Add(   4306, 'USA - Indiana - Dearborn, Ohio, Switzerland',
           -85.21, 38.68, -84.78, 39.31
         );
  _cs.Add(   4308, 'USA - Indiana - Clark, Floyd, Scott',
           -86.04, 38.17, -85.41, 38.84
         );
  _cs.Add(   4450, 'USA - Arizona - Pima county west',
           -113.33, 31.80, -112.51, 32.51
         );
  _cs.Add(   4515, 'USA - FBN',
           144.58, -14.59, -64.51, 49.38
         );
  _cs.Add(   4516, 'USA - CONUS and GoM',
           -124.79, 23.82, -66.91, 49.38
         );
  _cs.Add(   1384, 'USA - Iowa',
           -96.65, 40.37, -90.14, 43.51
         );
  _cs.Add(   1385, 'USA - Kansas',
           -102.06, 36.99, -94.58, 40.01
         );
  _cs.Add(   1386, 'USA - Kentucky',
           -89.57, 36.49, -81.95, 39.15
         );
  _cs.Add(   1387, 'USA - Louisiana',
           -94.05, 28.85, -88.75, 33.03
         );
  _cs.Add(   1388, 'USA - Maine',
           -71.09, 43.04, -66.91, 47.47
         );
  _cs.Add(   1389, 'USA - Maryland',
           -79.49, 37.97, -74.97, 39.73
         );
  _cs.Add(   1390, 'USA - Massachusetts',
           -73.50, 41.19, -69.86, 42.89
         );
  _cs.Add(   1391, 'USA - Michigan',
           -90.42, 41.69, -82.13, 48.32
         );
  _cs.Add(   1720, 'USA - Michigan - SPCS - E',
           -84.87, 41.69, -82.13, 46.04
         );
  _cs.Add(   1723, 'USA - Michigan - SPCS - N',
           -90.42, 45.08, -83.44, 48.32
         );
  _cs.Add(   1724, 'USA - Michigan - SPCS - C',
           -87.06, 43.80, -82.27, 45.92
         );
  _cs.Add(   2196, 'USA - Indiana - SPCS - E',
           -86.59, 37.95, -84.78, 41.77
         );
  _cs.Add(   2197, 'USA - Indiana - SPCS - W',
           -88.06, 37.77, -86.24, 41.77
         );
  _cs.Add(   2198, 'USA - Iowa - SPCS - N',
           -96.65, 41.85, -90.15, 43.51
         );
  _cs.Add(   2199, 'USA - Iowa - SPCS - S',
           -96.14, 40.37, -90.14, 42.04
         );
  _cs.Add(   2200, 'USA - Kansas - SPCS - N',
           -102.06, 38.52, -94.58, 40.01
         );
  _cs.Add(   2201, 'USA - Kansas - SPCS - S',
           -102.05, 36.99, -94.60, 38.88
         );
  _cs.Add(   2202, 'USA - Kentucky - SPCS - N',
           -85.96, 37.71, -82.47, 39.15
         );
  _cs.Add(   2203, 'USA - Kentucky - SPCS - S',
           -89.57, 36.49, -81.95, 38.17
         );
  _cs.Add(   2204, 'USA - Louisiana - SPCS - N',
           -94.05, 30.85, -90.86, 33.03
         );
  _cs.Add(   2205, 'USA - Louisiana - SPCS27 - S',
           -93.94, 27.82, -87.76, 31.07
         );
  _cs.Add(   2206, 'USA - Maine - SPCS - E',
           -70.03, 43.88, -66.91, 47.47
         );
  _cs.Add(   2207, 'USA - Maine - SPCS - W',
           -71.09, 43.04, -69.26, 46.58
         );
  _cs.Add(   2208, 'USA - Massachusetts - SPCS - islands',
           -70.91, 41.19, -69.89, 41.51
         );
  _cs.Add(   2209, 'USA - Massachusetts - SPCS - mainland',
           -73.50, 41.46, -69.86, 42.89
         );
  _cs.Add(   2529, 'USA - Louisiana - SPCS83 - S',
           -93.94, 28.85, -88.75, 31.07
         );
  _cs.Add(   2958, 'USA - Maine - CS2000 - W',
           -71.09, 43.07, -69.61, 46.58
         );
  _cs.Add(   2959, 'USA - Maine - CS2000 - C',
           -70.03, 43.75, -68.33, 47.47
         );
  _cs.Add(   2960, 'USA - Maine - CS2000 - E',
           -68.58, 44.18, -66.91, 47.37
         );
  _cs.Add(   4164, 'USA - Iowa - Spencer',
           -96.60, 42.90, -93.97, 43.51
         );
  _cs.Add(   4219, 'USA - Iowa - Mason City',
           -93.98, 42.90, -91.60, 43.51
         );
  _cs.Add(   4230, 'USA - Iowa - Elkader',
           -91.62, 42.29, -90.89, 43.51
         );
  _cs.Add(   4233, 'USA - Iowa - Sioux City-Iowa Falls',
           -96.65, 42.20, -93.00, 42.92
         );
  _cs.Add(   4234, 'USA - Iowa - Waterloo',
           -93.03, 42.20, -91.59, 43.09
         );
  _cs.Add(   4235, 'USA - Iowa - Council Bluffs',
           -96.37, 40.58, -95.04, 42.22
         );
  _cs.Add(   4236, 'USA - Iowa - Carroll-Atlantic',
           -95.16, 41.15, -94.16, 42.22
         );
end;

procedure prepare_29;
begin
  _cs.Add(   4237, 'USA - Iowa - Ames-Des Moines',
           -94.29, 41.15, -93.23, 42.22
         );
  _cs.Add(   4239, 'USA - Iowa - Newton',
           -93.35, 41.16, -92.29, 42.30
         );
  _cs.Add(   4240, 'USA - Iowa - Cedar Rapids',
           -92.31, 41.42, -90.89, 42.30
         );
  _cs.Add(   4241, 'USA - Iowa - Dubuque-Davenport',
           -91.14, 41.44, -90.14, 42.68
         );
  _cs.Add(   4242, 'USA - Iowa - Red Oak-Ottumwa',
           -95.39, 40.57, -92.17, 41.17
         );
  _cs.Add(   4243, 'USA - Iowa - Fairfield',
           -92.42, 40.59, -91.48, 41.52
         );
  _cs.Add(   4244, 'USA - Iowa - Burlington',
           -91.72, 40.37, -90.78, 41.60
         );
  _cs.Add(   4253, 'USA - Indiana - Lake and Newton',
           -87.53, 40.73, -87.21, 41.77
         );
  _cs.Add(   4254, 'USA - Indiana - Jasper and Porter',
           -87.28, 40.73, -86.92, 41.77
         );
  _cs.Add(   4255, 'USA - Indiana - LaPorte, Pulaski, Starke',
           -86.94, 40.90, -86.46, 41.77
         );
  _cs.Add(   4257, 'USA - Indiana - Tippecanoe and White',
           -87.10, 40.21, -86.58, 40.92
         );
  _cs.Add(   4259, 'USA - Indiana - Fountain and Warren',
           -87.54, 39.95, -87.09, 40.48
         );
  _cs.Add(   4261, 'USA - Indiana - Parke and Vermillion',
           -87.54, 39.60, -87.00, 40.15
         );
  _cs.Add(   4262, 'USA - Indiana - Montgomery and Putnam',
           -87.10, 39.47, -86.64, 40.22
         );
  _cs.Add(   4264, 'USA - Indiana - Vigo',
           -87.62, 39.25, -87.19, 39.61
         );
  _cs.Add(   4266, 'USA - Indiana - Owen',
           -87.06, 39.16, -86.63, 39.48
         );
  _cs.Add(   4267, 'USA - Indiana - Monroe and Morgan',
           -86.69, 38.99, -86.24, 39.64
         );
  _cs.Add(   4268, 'USA - Indiana - Sullivan',
           -87.65, 38.90, -87.24, 39.26
         );
  _cs.Add(   4270, 'USA - Indiana - Knox',
           -87.75, 38.41, -87.09, 38.91
         );
  _cs.Add(   4273, 'USA - Indiana - Gibson',
           -87.98, 38.16, -87.31, 38.54
         );
  _cs.Add(   4274, 'USA - Indiana - Pike and Warrick',
           -87.48, 37.87, -87.01, 38.56
         );
  _cs.Add(   4275, 'USA - Indiana - Posey',
           -88.06, 37.77, -87.68, 38.24
         );
  _cs.Add(   4276, 'USA - Indiana - Vanderburgh',
           -87.71, 37.82, -87.44, 38.17
         );
  _cs.Add(   4277, 'USA - Indiana - Spencer',
           -87.27, 37.78, -86.76, 38.21
         );
  _cs.Add(   4278, 'USA - Indiana - Perry',
           -86.82, 37.84, -86.43, 38.27
         );
  _cs.Add(   4279, 'USA - Indiana - Fulton, Marshall, St Joseph',
           -86.53, 40.90, -85.94, 41.77
         );
  _cs.Add(   4281, 'USA - Indiana - LaGrange and Noble',
           -85.66, 41.26, -85.19, 41.77
         );
  _cs.Add(   4282, 'USA - Indiana - Steuben',
           -85.20, 41.52, -84.80, 41.77
         );
  _cs.Add(   4284, 'USA - Indiana - Huntington and Whitley',
           -85.69, 40.65, -85.30, 41.30
         );
  _cs.Add(   4287, 'USA - Indiana - Howard and Miami',
           -86.38, 40.37, -85.86, 41.00
         );
  _cs.Add(   4288, 'USA - Indiana - Wells',
           -85.45, 40.56, -85.06, 40.92
         );
  _cs.Add(   4290, 'USA - Indiana - Grant',
           -85.87, 40.37, -85.44, 40.66
         );
  _cs.Add(   4292, 'USA - Indiana - Jay',
           -85.22, 40.30, -84.80, 40.58
         );
  _cs.Add(   4293, 'USA - Indiana - Hamilton and Tipton',
           -86.25, 39.92, -85.86, 40.41
         );
  _cs.Add(   4294, 'USA - Indiana - Hancock and Madison',
           -85.96, 39.69, -85.57, 40.38
         );
  _cs.Add(   4295, 'USA - Indiana - Randolph and Wayne',
           -85.23, 39.71, -84.80, 40.32
         );
  _cs.Add(   4296, 'USA - Indiana - Henry',
           -85.60, 39.78, -85.20, 40.08
         );
  _cs.Add(   4297, 'USA - Indiana - Johnson and Marion',
           -86.33, 39.34, -85.93, 39.93
         );
  _cs.Add(   4298, 'USA - Indiana - Shelby',
           -85.96, 39.34, -85.62, 39.70
         );
  _cs.Add(   4303, 'USA - Indiana - Jackson',
           -86.32, 38.72, -85.79, 39.08
         );
  _cs.Add(   4304, 'USA - Indiana - Jennings',
           -85.80, 38.80, -85.43, 39.20
         );
  _cs.Add(   4305, 'USA - Indiana - Ripley',
           -85.45, 38.91, -85.06, 39.32
         );
  _cs.Add(   4307, 'USA - Indiana - Harrison and Washington',
           -86.33, 37.95, -85.84, 38.79
         );
  _cs.Add(   4309, 'USA - Indiana - Jefferson',
           -85.69, 38.58, -85.20, 38.92
         );
  _cs.Add(   4494, 'USA - Kansas - Hays',
           -100.19, 38.69, -99.03, 40.01
         );
  _cs.Add(   4495, 'USA - Kansas - Goodland',
           -102.06, 38.69, -101.38, 40.01
         );
  _cs.Add(   4496, 'USA - Kansas - Colby',
           -101.49, 38.69, -100.71, 40.01
         );
  _cs.Add(   4497, 'USA - Kansas - Oberlin',
           -100.82, 38.69, -100.14, 40.01
         );
  _cs.Add(   4498, 'USA - Kansas - Great Bend',
           -99.07, 38.26, -98.47, 40.01
         );
  _cs.Add(   4499, 'USA - Kansas - Beloit',
           -98.51, 38.15, -97.92, 40.01
         );
  _cs.Add(   4500, 'USA - Kansas - Salina',
           -97.94, 38.08, -96.80, 40.01
         );
  _cs.Add(   4501, 'USA - Kansas - Manhattan',
           -96.97, 38.73, -95.94, 39.57
         );
  _cs.Add(   4502, 'USA - Kansas - Emporia',
           -96.94, 38.08, -95.94, 38.88
         );
  _cs.Add(   4503, 'USA - Kansas - Atchison',
           -96.81, 39.21, -94.85, 40.01
         );
  _cs.Add(   4504, 'USA - Kansas - Kansas City',
           -96.04, 38.73, -94.58, 39.43
         );
  _cs.Add(   4505, 'USA - Kansas - Ulysses',
           -102.05, 36.99, -101.06, 38.71
         );
  _cs.Add(   4506, 'USA - Kansas - Garden City',
           -101.13, 36.99, -100.08, 38.71
         );
  _cs.Add(   4507, 'USA - Kansas - Dodge City',
           -100.25, 36.99, -99.54, 38.70
         );
  _cs.Add(   4508, 'USA - Kansas - Larned',
           -99.59, 36.99, -98.91, 38.70
         );
  _cs.Add(   4509, 'USA - Kansas - Pratt',
           -99.03, 36.99, -98.34, 38.27
         );
  _cs.Add(   4510, 'USA - Kansas - Wichita',
           -98.48, 37.38, -96.52, 38.18
         );
  _cs.Add(   4511, 'USA - Kansas - Arkansas City',
           -98.35, 36.99, -96.52, 37.48
         );
  _cs.Add(   4512, 'USA - Kansas - Coffeyville',
           -96.53, 36.99, -95.50, 38.88
         );
  _cs.Add(   4513, 'USA - Kansas - Pittsburg',
           -95.53, 36.99, -94.60, 38.74
         );
  _cs.Add(   1392, 'USA - Minnesota',
           -97.22, 43.49, -89.49, 49.38
         );
  _cs.Add(   1393, 'USA - Mississippi',
           -91.65, 30.01, -88.09, 35.01
         );
  _cs.Add(   1394, 'USA - Missouri',
           -95.77, 35.98, -89.10, 40.61
         );
  _cs.Add(   1395, 'USA - Montana',
           -116.07, 44.35, -104.04, 49.01
         );
  _cs.Add(   1396, 'USA - Nebraska',
           -104.06, 39.99, -95.30, 43.01
         );
  _cs.Add(   1397, 'USA - Nevada',
           -120.00, 34.99, -114.03, 42.00
         );
  _cs.Add(   1398, 'USA - New Hampshire',
           -72.56, 42.69, -70.63, 45.31
         );
  _cs.Add(   1399, 'USA - New Jersey',
           -75.60, 38.87, -73.88, 41.36
         );
  _cs.Add(   1400, 'USA - New Mexico',
           -109.06, 31.33, -102.99, 37.00
         );
  _cs.Add(   1401, 'USA - New York',
           -79.77, 40.47, -71.80, 45.02
         );
  _cs.Add(   1402, 'USA - North Carolina',
           -84.33, 33.83, -75.38, 36.59
         );
  _cs.Add(   1403, 'USA - North Dakota',
           -104.07, 45.93, -96.55, 49.01
         );
  _cs.Add(   1404, 'USA - Ohio',
           -84.83, 38.40, -80.51, 42.33
         );
  _cs.Add(   1405, 'USA - Oklahoma',
           -103.00, 33.62, -94.42, 37.01
         );
  _cs.Add(   1406, 'USA - Oregon',
           -124.60, 41.98, -116.47, 46.26
         );
  _cs.Add(   1721, 'USA - Michigan - SPCS - old central',
           -87.61, 41.75, -84.60, 46.11
         );
  _cs.Add(   1722, 'USA - Michigan - SPCS - W',
           0.00, 0.00, 0.00, 0.00
         )         .MarkDeprecated( True );
  _cs.Add(   1725, 'USA - Michigan - SPCS - S',
           -87.20, 41.69, -82.13, 44.22
         );
  _cs.Add(   2210, 'USA - Montana - SPCS27 - C',
           -116.06, 46.17, -104.04, 48.26
         );
  _cs.Add(   2211, 'USA - Montana - SPCS27 - N',
           -116.07, 47.41, -104.04, 49.01
         );
  _cs.Add(   2212, 'USA - Montana - SPCS27 - S',
           -114.57, 44.35, -104.04, 46.87
         );
  _cs.Add(   2213, 'USA - Minnesota - SPCS - C',
           -96.86, 45.28, -92.29, 47.48
         );
  _cs.Add(   2214, 'USA - Minnesota - SPCS - N',
           -97.22, 46.64, -89.49, 49.38
         );
  _cs.Add(   2215, 'USA - Minnesota - SPCS - S',
           -96.85, 43.49, -91.21, 45.59
         );
  _cs.Add(   2216, 'USA - Mississippi - SPCS - E',
           -89.97, 30.01, -88.09, 35.01
         );
  _cs.Add(   2217, 'USA - Mississippi - SPCS - W',
           -91.65, 31.00, -89.37, 35.01
         );
  _cs.Add(   2218, 'USA - Missouri - SPCS - C',
           -93.79, 36.48, -91.41, 40.61
         );
  _cs.Add(   2219, 'USA - Missouri - SPCS - E',
           -91.97, 35.98, -89.10, 40.61
         );
  _cs.Add(   2220, 'USA - Missouri - SPCS - W',
           -95.77, 36.48, -93.48, 40.59
         );
  _cs.Add(   2221, 'USA - Nebraska - SPCS27 - N',
           -104.06, 41.68, -96.07, 43.01
         );
  _cs.Add(   2222, 'USA - Nebraska - SPCS27 - S',
           -104.06, 39.99, -95.30, 42.01
         );
  _cs.Add(   2223, 'USA - Nevada - SPCS - C',
           -118.19, 36.00, -114.99, 41.00
         );
  _cs.Add(   2224, 'USA - Nevada - SPCS - E',
           -117.01, 34.99, -114.03, 42.00
         );
  _cs.Add(   2225, 'USA - Nevada - SPCS - W',
           -120.00, 36.95, -116.99, 42.00
         );
  _cs.Add(   2228, 'USA - New Mexico - SPCS - E',
           -105.72, 32.00, -102.99, 37.00
         );
  _cs.Add(   2229, 'USA - New Mexico - SPCS27 - C',
           -107.73, 31.78, -104.83, 37.00
         );
end;

procedure prepare_30;
begin
  _cs.Add(   2230, 'USA - New Mexico - SPCS27 - W',
           -109.06, 31.33, -106.32, 37.00
         );
  _cs.Add(   2231, 'USA - New Mexico - SPCS83 - C',
           -107.73, 31.78, -104.84, 37.00
         );
  _cs.Add(   2232, 'USA - New Mexico - SPCS83 - W',
           -109.06, 31.33, -106.32, 37.00
         );
  _cs.Add(   2233, 'USA - New York - SPCS - C',
           -77.75, 41.99, -75.04, 44.41
         );
  _cs.Add(   2234, 'USA - New York - SPCS - E',
           -75.87, 40.88, -73.23, 45.02
         );
  _cs.Add(   2235, 'USA - New York - SPCS - Long Island',
           -74.26, 40.47, -71.80, 41.30
         );
  _cs.Add(   2236, 'USA - New York - SPCS - W',
           -79.77, 41.99, -77.36, 43.64
         );
  _cs.Add(   2237, 'USA - North Dakota - SPCS - N',
           -104.07, 47.15, -96.83, 49.01
         );
  _cs.Add(   2238, 'USA - North Dakota - SPCS - S',
           -104.05, 45.93, -96.55, 47.83
         );
  _cs.Add(   2239, 'USA - Ohio - SPCS - N',
           -84.81, 40.10, -80.51, 42.33
         );
  _cs.Add(   2240, 'USA - Ohio - SPCS - S',
           -84.83, 38.40, -80.70, 40.36
         );
  _cs.Add(   2241, 'USA - Oklahoma - SPCS - N',
           -103.00, 35.27, -94.42, 37.01
         );
  _cs.Add(   2242, 'USA - Oklahoma - SPCS - S',
           -100.00, 33.62, -94.42, 35.57
         );
  _cs.Add(   2243, 'USA - Oregon - SPCS - N',
           -124.17, 43.95, -116.47, 46.26
         );
  _cs.Add(   2244, 'USA - Oregon - SPCS - S',
           -124.60, 41.98, -116.90, 44.56
         );
  _cs.Add(   2378, 'USA - New England - south (CT, MA, NH, RI, VT)',
           -73.73, 40.98, -69.86, 45.31
         );
  _cs.Add(   3652, 'USA - Michigan - SPCS - W',
           -90.42, 45.09, -83.44, 48.32
         );
  _cs.Add(   4180, 'USA - Oregon - Baker City',
           -118.15, 44.60, -117.37, 45.19
         );
  _cs.Add(   4182, 'USA - Oregon - Bend-Burns',
           -120.95, 43.34, -118.80, 44.28
         );
  _cs.Add(   4192, 'USA - Oregon - Bend-Klamath Falls',
           -122.45, 41.88, -120.77, 43.89
         );
  _cs.Add(   4195, 'USA - Oregon - Bend-Redmond-Prineville',
           -121.88, 43.76, -119.79, 44.98
         );
  _cs.Add(   4197, 'USA - Oregon - Eugene',
           -123.80, 43.74, -122.18, 44.71
         );
  _cs.Add(   4198, 'USA - Oregon - Grants Pass-Ashland',
           -123.95, 41.88, -122.37, 42.85
         );
  _cs.Add(   4199, 'USA - Oregon - Canyonville-Grants Pass',
           -123.83, 42.49, -122.43, 43.24
         );
  _cs.Add(   4200, 'USA - Oregon - Columbia River East',
           -122.05, 45.49, -118.89, 46.08
         );
  _cs.Add(   4201, 'USA - Oregon - Gresham-Warm Springs',
           -122.43, 45.02, -121.68, 45.55
         );
  _cs.Add(   4202, 'USA - Oregon - Columbia River West',
           -124.33, 45.17, -121.47, 46.56
         );
  _cs.Add(   4203, 'USA - Oregon - Cottage Grove-Canyonville',
           -123.96, 42.82, -122.40, 43.88
         );
  _cs.Add(   4204, 'USA - Oregon - Dufur-Madras',
           -121.95, 44.63, -120.46, 45.55
         );
  _cs.Add(   4206, 'USA - Oregon - La Grande',
           -118.17, 45.13, -117.14, 45.80
         );
  _cs.Add(   4207, 'USA - Oregon - Ontario',
           -117.90, 43.41, -116.70, 44.65
         );
  _cs.Add(   4208, 'USA - Oregon - Oregon Coast',
           -124.84, 41.89, -123.35, 46.40
         );
  _cs.Add(   4209, 'USA - Oregon - Pendleton',
           -119.36, 45.46, -118.17, 46.02
         );
  _cs.Add(   4210, 'USA - Oregon - Pendleton-La Grande',
           -118.64, 45.13, -118.09, 45.64
         );
  _cs.Add(   4211, 'USA - Oregon - Portland',
           -123.53, 45.23, -122.11, 46.01
         );
  _cs.Add(   4212, 'USA - Oregon - Salem',
           -123.73, 44.32, -121.89, 45.30
         );
  _cs.Add(   4213, 'USA - Oregon - Sweet Home-Sisters',
           -122.51, 44.10, -121.69, 44.66
         );
  _cs.Add(   4310, 'USA - Montana - St Mary valley',
           -113.97, 48.55, -113.00, 49.01
         );
  _cs.Add(   4311, 'USA - Montana - Blackfeet reservation',
           -113.84, 48.00, -112.00, 49.01
         );
  _cs.Add(   4312, 'USA - Montana - Milk River',
           -112.50, 47.78, -108.74, 49.01
         );
  _cs.Add(   4313, 'USA - Montana - Fort Belknap',
           -110.84, 47.78, -106.99, 49.01
         );
  _cs.Add(   4314, 'USA - Montana - Fort Peck higher areas',
           -107.57, 48.01, -104.78, 48.88
         );
  _cs.Add(   4315, 'USA - Montana - Fort Peck lower areas',
           -107.76, 47.75, -104.04, 49.01
         );
  _cs.Add(   4316, 'USA - Montana - Crow reservation',
           -108.84, 44.99, -106.66, 46.09
         );
  _cs.Add(   4317, 'USA - Montana - Three Forks',
           -112.34, 45.36, -110.75, 46.59
         );
  _cs.Add(   4318, 'USA - Montana - Billings',
           -109.42, 44.99, -107.99, 47.41
         );
  _cs.Add(   4458, 'USA - Oregon - Riley-Lakeview',
           -120.97, 41.88, -119.15, 43.45
         );
  _cs.Add(   4459, 'USA - Oregon - Burns-Harper',
           -118.61, 43.49, -117.49, 44.19
         );
  _cs.Add(   4463, 'USA - Oregon - Siskiyou Pass',
           -122.71, 41.95, -121.96, 42.46
         );
  _cs.Add(   4464, 'USA - onshore - AZ MI MT ND OR SC',
           -124.60, 31.33, -78.52, 49.01
         );
  _cs.Add(   4465, 'USA - Oregon - Canyon City-Burns',
           -119.22, 43.52, -118.52, 44.36
         );
  _cs.Add(   4471, 'USA - Oregon - Coast Range North',
           -123.81, 45.40, -123.01, 45.98
         );
  _cs.Add(   4474, 'USA - Oregon - Dayville-Prairie City',
           -119.89, 44.24, -118.61, 44.94
         );
  _cs.Add(   4475, 'USA - Oregon - Denio-Burns',
           -119.41, 41.88, -117.67, 43.54
         );
  _cs.Add(   4476, 'USA - Oregon - Halfway',
           -117.61, 44.61, -116.63, 45.28
         );
  _cs.Add(   4477, 'USA - Oregon - Medford-Diamond Lake',
           -122.73, 42.53, -121.71, 43.34
         );
  _cs.Add(   4478, 'USA - Oregon - Mitchell',
           -120.56, 44.38, -119.82, 44.78
         );
  _cs.Add(   4479, 'USA - Oregon - North Central',
           -121.79, 44.89, -119.03, 45.95
         );
  _cs.Add(   4481, 'USA - Oregon - Ochoco Summit',
           -121.01, 44.21, -120.31, 44.61
         );
  _cs.Add(   4482, 'USA - Oregon - Owyhee',
           -118.14, 41.88, -116.85, 43.54
         );
  _cs.Add(   4483, 'USA - Oregon - Pilot Rock-Ukiah',
           -119.65, 44.99, -118.11, 46.04
         );
  _cs.Add(   4484, 'USA - Oregon - Prairie City-Brogan',
           -118.77, 44.16, -117.48, 45.02
         );
  _cs.Add(   4485, 'USA - Nevada - Las Vegas',
           -115.50, 35.88, -114.71, 36.43
         );
  _cs.Add(   4487, 'USA - Nevada - Las Vegas high elevation',
           -115.50, 35.88, -114.71, 36.43
         );
  _cs.Add(   1407, 'USA - Pennsylvania',
           -80.53, 39.71, -74.70, 42.53
         );
  _cs.Add(   1408, 'USA - Rhode Island',
           -71.85, 41.13, -71.08, 42.02
         );
  _cs.Add(   1409, 'USA - South Carolina',
           -83.36, 32.05, -78.52, 35.21
         );
  _cs.Add(   1410, 'USA - South Dakota',
           -104.07, 42.48, -96.43, 45.95
         );
  _cs.Add(   1411, 'USA - Tennessee',
           -90.31, 34.98, -81.65, 36.68
         );
  _cs.Add(   1412, 'USA - Texas',
           -106.66, 25.83, -93.50, 36.50
         );
  _cs.Add(   1413, 'USA - Utah',
           -114.05, 36.99, -109.04, 42.01
         );
  _cs.Add(   1414, 'USA - Vermont',
           -73.44, 42.72, -71.50, 45.03
         );
  _cs.Add(   1415, 'USA - Virginia',
           -83.68, 36.54, -75.31, 39.46
         );
  _cs.Add(   1416, 'USA - Washington',
           -124.79, 45.54, -116.91, 49.05
         );
  _cs.Add(   1417, 'USA - West Virginia',
           -82.65, 37.20, -77.72, 40.64
         );
  _cs.Add(   1418, 'USA - Wisconsin',
           -92.89, 42.48, -86.25, 47.31
         );
  _cs.Add(   2245, 'USA - Pennsylvania - SPCS - N',
           -80.53, 40.60, -74.70, 42.53
         );
  _cs.Add(   2246, 'USA - Pennsylvania - SPCS - S',
           -80.53, 39.71, -74.72, 41.18
         );
  _cs.Add(   2247, 'USA - South Carolina - SPCS27 - N',
           -83.36, 33.46, -78.52, 35.21
         );
  _cs.Add(   2248, 'USA - South Carolina - SPCS27 - S',
           -82.03, 32.05, -78.95, 33.95
         );
  _cs.Add(   2249, 'USA - South Dakota - SPCS - N',
           -104.07, 44.14, -96.45, 45.95
         );
  _cs.Add(   2250, 'USA - South Dakota - SPCS - S',
           -104.06, 42.48, -96.43, 44.79
         );
  _cs.Add(   2252, 'USA - Texas - SPCS - C',
           -106.66, 29.78, -93.50, 32.27
         );
  _cs.Add(   2253, 'USA - Texas - SPCS - N',
           -103.03, 34.30, -99.99, 36.50
         );
  _cs.Add(   2254, 'USA - Texas - SPCS - NC',
           -103.07, 31.72, -94.00, 34.58
         );
  _cs.Add(   2255, 'USA - Texas - SPCS27 - S',
           -100.20, 25.83, -95.37, 28.21
         );
  _cs.Add(   2256, 'USA - Texas - SPCS27 - SC',
           -105.00, 27.78, -93.41, 30.67
         );
  _cs.Add(   2257, 'USA - Utah - SPCS - C',
           -114.05, 38.49, -109.04, 41.08
         );
  _cs.Add(   2258, 'USA - Utah - SPCS - N',
           -114.04, 40.55, -109.04, 42.01
         );
  _cs.Add(   2259, 'USA - Utah - SPCS - S',
           -114.05, 36.99, -109.04, 38.58
         );
  _cs.Add(   2260, 'USA - Virginia - SPCS - N',
           -80.06, 37.77, -76.51, 39.46
         );
  _cs.Add(   2261, 'USA - Virginia - SPCS - S',
           -83.68, 36.54, -75.31, 38.28
         );
  _cs.Add(   2262, 'USA - Washington - SPCS27 - N',
           -124.79, 47.08, -117.02, 49.05
         );
  _cs.Add(   2263, 'USA - Washington - SPCS27 - S',
           -124.40, 45.54, -116.91, 47.96
         );
  _cs.Add(   2264, 'USA - West Virginia - SPCS - N',
           -81.76, 38.76, -77.72, 40.64
         );
  _cs.Add(   2265, 'USA - West Virginia - SPCS - S',
           -82.65, 37.20, -79.05, 39.17
         );
  _cs.Add(   2266, 'USA - Wisconsin - SPCS - C',
           -92.89, 43.98, -86.25, 45.80
         );
  _cs.Add(   2267, 'USA - Wisconsin - SPCS - N',
           -92.89, 45.37, -88.05, 47.31
         );
  _cs.Add(   2268, 'USA - Wisconsin - SPCS - S',
           -91.43, 42.48, -86.95, 44.33
         );
  _cs.Add(   2273, 'USA - Washington - SPCS83 - N',
           -124.79, 47.08, -117.02, 49.05
         );
end;

procedure prepare_31;
begin
  _cs.Add(   2274, 'USA - Washington - SPCS83 - S',
           -124.40, 45.54, -116.91, 47.61
         );
  _cs.Add(   2379, 'USA - Texas east of 100'#$00b0'W',
           -100.00, 25.83, -93.50, 34.58
         );
  _cs.Add(   2380, 'USA - Texas west of 100'#$00b0'W',
           -106.66, 28.04, -100.00, 36.50
         );
  _cs.Add(   2381, 'USA - Oregon and Washington',
           -124.79, 41.98, -116.47, 49.05
         );
  _cs.Add(   2527, 'USA - Texas - SPCS83 - SC',
           -105.00, 27.78, -93.76, 30.67
         );
  _cs.Add(   2528, 'USA - Texas - SPCS83 - S',
           -100.20, 25.83, -96.85, 28.21
         );
  _cs.Add(   3372, 'USA - west of 174'#$00b0'E - AK, OCS',
           167.65, 49.01, 174.01, 56.28
         );
  _cs.Add(   3482, 'USA - west of 174'#$00b0'E - AK',
           168.00, 49.00, 174.00, 56.30
         )         .MarkDeprecated( True );
  _cs.Add(   4320, 'USA - Wisconsin - Ashland',
           -90.93, 45.98, -90.30, 47.09
         );
  _cs.Add(   4321, 'USA - Wisconsin - Bayfield',
           -91.56, 46.15, -90.75, 47.01
         );
  _cs.Add(   4325, 'USA - Wisconsin - Burnett',
           -92.89, 45.63, -92.03, 46.16
         );
  _cs.Add(   4326, 'USA - Wisconsin - Douglas',
           -92.30, 46.15, -91.55, 46.76
         );
  _cs.Add(   4327, 'USA - Wisconsin - Florence',
           -88.69, 45.71, -88.05, 46.03
         );
  _cs.Add(   4328, 'USA - Wisconsin - Forest',
           -89.05, 45.37, -88.42, 46.08
         );
  _cs.Add(   4329, 'USA - Wisconsin - Iron',
           -90.56, 45.98, -89.92, 46.60
         );
  _cs.Add(   4330, 'USA - Wisconsin - Oneida',
           -90.05, 45.46, -89.04, 45.91
         );
  _cs.Add(   4331, 'USA - Wisconsin - Barron',
           -92.16, 45.20, -91.53, 45.65
         );
  _cs.Add(   4332, 'USA - Wisconsin - Price',
           -90.68, 45.37, -90.04, 45.99
         );
  _cs.Add(   4333, 'USA - Wisconsin - Sawyer',
           -91.56, 45.63, -90.67, 46.16
         );
  _cs.Add(   4334, 'USA - Wisconsin - Vilas',
           -90.05, 45.85, -88.93, 46.30
         );
  _cs.Add(   4336, 'USA - Wisconsin - Brown',
           -88.26, 44.24, -87.76, 44.68
         );
  _cs.Add(   4337, 'USA - Wisconsin - Buffalo',
           -92.09, 44.02, -91.52, 44.60
         );
  _cs.Add(   4338, 'USA - Wisconsin - Chippewa',
           -91.67, 44.85, -90.92, 45.30
         );
  _cs.Add(   4339, 'USA - Wisconsin - Clark',
           -90.93, 44.42, -90.31, 45.04
         );
  _cs.Add(   4340, 'USA - Wisconsin - Door',
           -87.74, 44.67, -86.80, 45.43
         );
  _cs.Add(   4341, 'USA - Wisconsin - Dunn',
           -92.16, 44.68, -91.64, 45.21
         );
  _cs.Add(   4342, 'USA - Wisconsin - Eau Claire',
           -91.66, 44.59, -90.92, 44.86
         );
  _cs.Add(   4343, 'USA - Wisconsin - Jackson',
           -91.17, 44.07, -90.31, 44.60
         );
  _cs.Add(   4344, 'USA - Wisconsin - Langlade',
           -89.43, 45.02, -88.63, 45.48
         );
  _cs.Add(   4345, 'USA - Wisconsin - Lincoln',
           -90.05, 45.11, -89.42, 45.56
         );
  _cs.Add(   4346, 'USA - Wisconsin - Marathon',
           -90.32, 44.68, -89.22, 45.13
         );
  _cs.Add(   4347, 'USA - Wisconsin - Marinette',
           -88.43, 44.96, -87.48, 45.80
         );
  _cs.Add(   4348, 'USA - Wisconsin - Menominee',
           -88.99, 44.85, -88.48, 45.12
         );
  _cs.Add(   4349, 'USA - Wisconsin - Oconto',
           -88.69, 44.67, -87.76, 45.38
         );
  _cs.Add(   4350, 'USA - Wisconsin - Pepin and Pierce',
           -92.81, 44.40, -91.65, 44.87
         );
  _cs.Add(   4351, 'USA - Wisconsin - Polk',
           -92.89, 45.20, -92.15, 45.73
         );
  _cs.Add(   4352, 'USA - Wisconsin - Portage',
           -89.85, 44.24, -89.22, 44.69
         );
  _cs.Add(   4353, 'USA - Wisconsin - Rusk',
           -91.55, 45.29, -90.67, 45.64
         );
  _cs.Add(   4354, 'USA - Wisconsin - Shawano',
           -89.23, 44.58, -88.24, 45.03
         );
  _cs.Add(   4355, 'USA - Wisconsin - St. Croix',
           -92.81, 44.85, -92.13, 45.22
         );
  _cs.Add(   4356, 'USA - Wisconsin - Taylor',
           -90.93, 45.03, -90.04, 45.39
         );
  _cs.Add(   4357, 'USA - Wisconsin - Trempealeau',
           -91.62, 43.98, -91.15, 44.60
         );
  _cs.Add(   4360, 'USA - Wisconsin - Adams and Juneau',
           -90.32, 43.64, -89.59, 44.25
         );
  _cs.Add(   4361, 'USA - Wisconsin - Calumet, Fond du Lac, Outagamie and Winnebago',
           -88.89, 43.54, -88.04, 44.60
         );
  _cs.Add(   4362, 'USA - Wisconsin - Columbia',
           -89.79, 43.28, -89.00, 43.65
         );
  _cs.Add(   4363, 'USA - Wisconsin - Crawford',
           -91.22, 42.98, -90.66, 43.43
         );
  _cs.Add(   4364, 'USA - Wisconsin - Dane',
           -89.84, 42.84, -89.00, 43.30
         );
  _cs.Add(   4365, 'USA - Wisconsin - Dodge and Jefferson',
           -89.02, 42.84, -88.40, 43.64
         );
  _cs.Add(   4366, 'USA - Wisconsin - Grant',
           -91.16, 42.50, -90.42, 43.22
         );
  _cs.Add(   4367, 'USA - Wisconsin - Green and Lafayette',
           -90.43, 42.50, -89.36, 42.86
         );
  _cs.Add(   4368, 'USA - Wisconsin - Green Lake and Marquette',
           -89.60, 43.63, -88.88, 43.99
         );
  _cs.Add(   4369, 'USA - Wisconsin - Iowa',
           -90.43, 42.81, -89.83, 43.21
         );
  _cs.Add(   4370, 'USA - Wisconsin - Kenosha, Milwaukee, Ozaukee and Racine',
           -88.31, 42.49, -87.75, 43.55
         );
  _cs.Add(   4371, 'USA - Wisconsin - Kewaunee, Manitowoc and Sheboygan',
           -88.17, 43.54, -87.37, 44.68
         );
  _cs.Add(   4372, 'USA - Wisconsin - La Crosse',
           -91.43, 43.72, -90.91, 44.10
         );
  _cs.Add(   4373, 'USA - Wisconsin - Monroe',
           -90.98, 43.72, -90.31, 44.17
         );
  _cs.Add(   4374, 'USA - Wisconsin - Richland',
           -90.68, 43.16, -90.19, 43.56
         );
  _cs.Add(   4375, 'USA - Wisconsin - Rock',
           -89.37, 42.49, -88.77, 42.85
         );
  _cs.Add(   4376, 'USA - Wisconsin - Sauk',
           -90.32, 43.14, -89.59, 43.65
         );
  _cs.Add(   4377, 'USA - Wisconsin - Vernon',
           -91.28, 43.42, -90.31, 43.74
         );
  _cs.Add(   4470, 'USA - Oregon - Ukiah-Fox',
           -119.35, 44.52, -118.64, 45.21
         );
  _cs.Add(   4480, 'USA - Oregon - Wallowa',
           -118.16, 45.27, -116.42, 46.05
         );
  _cs.Add(   4486, 'USA - Oregon - Warner Highway',
           -120.42, 41.95, -119.30, 42.43
         );
  _cs.Add(   4488, 'USA - Oregon - Willamette Pass',
           -122.26, 42.99, -121.48, 44.18
         );
  _cs.Add(   1248, 'Uzbekistan',
           55.99, 37.18, 73.17, 45.58
         );
  _cs.Add(   1249, 'Vanuatu',
           163.16, -21.65, 173.59, -12.27
         );
  _cs.Add(   1251, 'Venezuela',
           -73.38, 0.64, -58.95, 16.75
         );
  _cs.Add(   1252, 'Vietnam',
           102.14, 5.67, 112.55, 23.40
         );
  _cs.Add(   1253, 'Virgin Islands, British',
           -65.85, 17.96, -63.29, 22.09
         );
  _cs.Add(   1254, 'Virgin Islands, US',
           -66.05, 16.22, -63.88, 21.83
         );
  _cs.Add(   1255, 'Wallis and Futuna',
           179.49, -15.94, -174.27, -9.84
         );
  _cs.Add(   1256, 'Western Sahara',
           -20.68, 18.98, -8.66, 27.67
         );
  _cs.Add(   1262, 'World',
           -180.00, -90.00, 180.00, 90.00
         );
  _cs.Add(   1266, 'Venezuela - Puerto La Cruz',
           -64.70, 10.00, -64.50, 10.31
         );
  _cs.Add(   1267, 'Venezuela - Barinas',
           -71.49, 7.31, -67.58, 9.07
         );
  _cs.Add(   1268, 'Venezuela - Falcon state',
           -71.30, 10.30, -68.19, 12.25
         );
  _cs.Add(   1269, 'Venezuela - Pedregal area of Falcon state',
           -70.40, 10.80, -69.69, 11.26
         );
  _cs.Add(   1270, 'Venezuela - Maracaibo south',
           -72.40, 8.72, -70.78, 10.01
         );
  _cs.Add(   1311, 'Venezuela - Cabimas',
           -71.55, 10.14, -71.29, 10.61
         );
  _cs.Add(   1312, 'Venezuela - Lake Maracaibo',
           -72.40, 8.72, -70.78, 11.04
         );
  _cs.Add(   1313, 'Venezuela - north of 7'#$00b0'45''N',
           -73.38, 7.75, -59.80, 12.25
         );
  _cs.Add(   1319, 'Venezuela - Maracaibo area',
           -72.25, 10.00, -71.50, 11.00
         );
  _cs.Add(   1320, 'Venezuela - Maturin',
           -64.30, 9.10, -63.00, 10.13
         );
  _cs.Add(   1324, 'USA (all states)',
           167.65, 15.56, -65.69, 74.71
         );
  _cs.Add(   1370, 'Venezuela - Deltana',
           -62.80, 7.89, -59.80, 10.46
         );
  _cs.Add(   1371, 'Venezuela - Guarico state',
           -68.00, 7.54, -64.76, 10.03
         );
  _cs.Add(   1419, 'USA - Wyoming',
           -111.06, 40.99, -104.05, 45.01
         );
  _cs.Add(   1452, 'Vietnam - west of 108'#$00b0'E onshore',
           102.14, 8.33, 108.00, 23.40
         );
  _cs.Add(   1453, 'Vietnam - east of 108'#$00b0'E onshore',
           108.00, 10.43, 109.53, 21.56
         );
  _cs.Add(   1494, 'Vietnam - onshore Vung Tau area',
           105.49, 9.03, 107.58, 11.04
         );
  _cs.Add(   1495, 'Vietnam - offshore Cuu Long basin',
           106.54, 7.99, 110.00, 11.15
         );
  _cs.Add(   1499, 'Venezuela - Maracaibo - blocks I II and III',
           -71.50, 10.00, -71.17, 10.51
         );
  _cs.Add(   1693, 'Venezuela - west of 72'#$00b0'W',
           -73.38, 7.02, -71.99, 11.62
         );
  _cs.Add(   1694, 'Venezuela - 72'#$00b0'W and 66'#$00b0'W onshore',
           -72.00, 0.73, -66.00, 12.25
         );
  _cs.Add(   1695, 'Venezuela - east of 66'#$00b0'W onshore',
           -66.00, 0.64, -59.80, 11.23
         );
  _cs.Add(   1933, 'World - N hemisphere - 0'#$00b0'E to 6'#$00b0'E',
           0.00, 0.00, 6.00, 84.00
         );
  _cs.Add(   2269, 'USA - Wyoming - SPCS - E',
           -106.33, 40.99, -104.05, 45.01
         );
  _cs.Add(   2270, 'USA - Wyoming - SPCS - EC',
           -108.63, 40.99, -106.00, 45.01
         );
  _cs.Add(   2271, 'USA - Wyoming - SPCS - W',
           -111.06, 40.99, -109.04, 44.67
         );
  _cs.Add(   2272, 'USA - Wyoming - SPCS - WC',
           -111.06, 40.99, -107.50, 45.01
         );
end;

procedure prepare_32;
begin
  _cs.Add(   2359, 'Vietnam - 14'#$00b0'N to 18'#$00b0'N onshore',
           105.61, 14.00, 109.36, 18.01
         );
  _cs.Add(   2360, 'Vietnam - Con Son Island',
           106.48, 8.57, 106.80, 8.83
         );
  _cs.Add(   2363, 'Venezuela - east',
           -67.49, 3.56, -59.80, 10.80
         );
  _cs.Add(   2732, 'World - 6-degree CM 081'#$00b0'W',
           -84.00, -80.00, -78.00, 84.00
         );
  _cs.Add(   2733, 'World - 6-degree CM 075'#$00b0'W',
           -78.00, -80.00, -72.00, 84.00
         );
  _cs.Add(   2734, 'World - 6-degree CM 069'#$00b0'W',
           -72.00, -80.00, -66.00, 84.00
         );
  _cs.Add(   2735, 'World - 6-degree CM 063'#$00b0'W',
           -66.00, -80.00, -60.00, 84.00
         );
  _cs.Add(   2736, 'World - 6-degree CM 057'#$00b0'W',
           -60.00, -80.00, -54.00, 84.00
         );
  _cs.Add(   2737, 'World - 6-degree CM 051'#$00b0'W',
           -54.00, -80.00, -48.00, 84.00
         );
  _cs.Add(   2738, 'World - 6-degree CM 045'#$00b0'W',
           -48.00, -80.00, -42.00, 84.00
         );
  _cs.Add(   2739, 'World - 6-degree CM 039'#$00b0'W',
           -42.00, -80.00, -36.00, 84.00
         );
  _cs.Add(   2740, 'World - 6-degree CM 033'#$00b0'W',
           -36.00, -80.00, -30.00, 84.00
         );
  _cs.Add(   2741, 'World - 6-degree CM 009'#$00b0'E',
           6.00, -80.00, 12.00, 84.00
         );
  _cs.Add(   2742, 'World - 6-degree CM 015'#$00b0'E',
           12.00, -80.00, 18.00, 84.00
         );
  _cs.Add(   2743, 'World - 6-degree CM 021'#$00b0'E',
           18.00, -80.00, 24.00, 84.00
         );
  _cs.Add(   2744, 'World - 6-degree CM 027'#$00b0'E',
           24.00, -80.00, 30.00, 84.00
         );
  _cs.Add(   2745, 'World - 6-degree CM 033'#$00b0'E',
           30.00, -80.00, 36.00, 84.00
         );
  _cs.Add(   2746, 'World - 6-degree CM 039'#$00b0'E',
           36.00, -80.00, 42.00, 84.00
         );
  _cs.Add(   2788, 'Western Sahara - north of 24.3'#$00b0'N',
           -15.42, 24.29, -8.66, 27.67
         );
  _cs.Add(   2789, 'Western Sahara - south of 24.3'#$00b0'N',
           -17.16, 20.71, -12.00, 24.31
         );
  _cs.Add(   2815, 'Wallis and Futuna - Wallis',
           -176.25, -13.41, -176.07, -13.16
         );
  _cs.Add(   3190, 'Wake - onshore',
           166.55, 19.22, 166.72, 19.38
         );
  _cs.Add(   3193, 'Vanuatu - southern islands',
           168.09, -20.31, 169.95, -17.37
         );
  _cs.Add(   3194, 'Vanuatu - northern islands',
           166.47, -17.32, 168.71, -14.57
         );
  _cs.Add(   3327, 'Venezuela - onshore',
           -73.38, 0.64, -59.80, 12.25
         );
  _cs.Add(   3328, 'Vietnam - onshore',
           102.14, 8.33, 109.53, 23.40
         );
  _cs.Add(   3329, 'Virgin Islands, British - onshore',
           -64.88, 18.28, -64.25, 18.78
         );
  _cs.Add(   3330, 'Virgin Islands, US - onshore',
           -65.09, 17.62, -64.51, 18.44
         );
  _cs.Add(   3331, 'Western Sahara - onshore',
           -17.16, 20.71, -8.66, 27.67
         );
  _cs.Add(   3391, 'World - between 80'#$00b0'S and 84'#$00b0'N',
           -180.00, -80.00, 180.00, 84.00
         );
  _cs.Add(   3463, 'World - 86'#$00b0'S to 86'#$00b0'N',
           -180.00, -86.00, 180.00, 86.00
         );
  _cs.Add(   3544, 'World - 85'#$00b0'S to 85'#$00b0'N',
           -180.00, -85.06, 180.00, 85.06
         );
  _cs.Add(   3770, 'Vietnam - DBSCL 02 and 03',
           104.24, 9.35, 107.11, 11.04
         );
  _cs.Add(   3858, 'Venezuela - east of 66'#$00b0'W',
           -66.00, 0.64, -58.95, 16.75
         );
  _cs.Add(   3859, 'Venezuela - 72'#$00b0'W and 66'#$00b0'W',
           -72.00, 0.73, -66.00, 15.64
         );
  _cs.Add(   4015, 'Vietnam - mainland',
           102.14, 8.33, 109.53, 23.40
         );
  _cs.Add(   4193, 'Vietnam - west of 103'#$00b0'30''E onshore',
           102.14, 9.20, 103.51, 22.82
         );
  _cs.Add(   4215, 'Vietnam - 103.5'#$00b0'E to 106.5'#$00b0'E onshore',
           103.50, 8.33, 106.51, 23.40
         );
  _cs.Add(   4217, 'Vietnam - east of 106.5'#$00b0'E onshore',
           106.50, 8.57, 109.53, 22.95
         );
  _cs.Add(   4218, 'Vietnam - Quang Ninh; Da Nang, Quang Nam; Ba Ria-Vung Tau, Dong Nai, Lam Dong',
           106.43, 8.57, 108.76, 21.67
         );
  _cs.Add(   4319, 'USA - Wyoming - Wind River reservation',
           -109.50, 42.69, -107.94, 43.86
         );
  _cs.Add(   4335, 'USA - Wisconsin - Washburn',
           -92.06, 45.63, -91.54, 46.16
         );
  _cs.Add(   4358, 'USA - Wisconsin - Waupaca',
           -89.23, 44.24, -88.60, 44.69
         );
  _cs.Add(   4359, 'USA - Wisconsin - Wood',
           -90.32, 44.24, -89.72, 44.69
         );
  _cs.Add(   4378, 'USA - Wisconsin - Walworth',
           -88.78, 42.49, -88.30, 42.85
         );
  _cs.Add(   4379, 'USA - Wisconsin - Washington',
           -88.42, 43.19, -88.03, 43.55
         );
  _cs.Add(   4380, 'USA - Wisconsin - Waukesha',
           -88.55, 42.84, -88.06, 43.20
         );
  _cs.Add(   4381, 'USA - Wisconsin - Waushara',
           -89.60, 43.98, -88.88, 44.25
         );
  _cs.Add(   4538, 'Vietnam - Son La',
           103.21, 20.57, 105.03, 22.04
         );
  _cs.Add(   4541, 'Vietnam - Dien Bien and Lai Chau',
           102.14, 20.89, 103.99, 22.82
         );
  _cs.Add(   4545, 'Vietnam - Ca Mau and Kien Giang',
           103.40, 8.33, 105.54, 10.55
         );
  _cs.Add(   4546, 'Vietnam - An Giang, Lao Cai, Nghe An, Phu Tho, Yen Bai',
           103.53, 10.18, 105.86, 22.85
         );
  _cs.Add(   4547, 'Vietnam - 104'#$00b0'20''E to 106'#$00b0'10''E by province - Hanoi',
           104.37, 8.97, 106.19, 21.58
         );
  _cs.Add(   4548, 'Vietnam - 104'#$00b0'20''E to 106'#$00b0'40''E by province',
           104.33, 9.19, 106.69, 23.40
         );
  _cs.Add(   4549, 'Vietnam - 105'#$00b0'15''E to 107'#$00b0'50''E by province - HCMC',
           105.26, 9.75, 107.80, 23.12
         );
  _cs.Add(   4550, 'Vietnam - Hoa Binh, Quang Binh and Tuyen Quang',
           104.83, 16.92, 107.03, 22.70
         );
  _cs.Add(   4552, 'Vietnam - Binh Phuoc and Quang Tri',
           106.41, 11.30, 107.43, 17.22
         );
  _cs.Add(   4553, 'Vietnam - Bac Kan and Thai Nguyen',
           105.43, 21.32, 106.25, 22.75
         );
  _cs.Add(   4554, 'Vietnam - Bac Giang and Thua Thien-Hue',
           105.88, 15.99, 108.24, 21.63
         );
  _cs.Add(   4556, 'Vietnam - Lang Son',
           106.09, 21.32, 107.37, 22.47
         );
  _cs.Add(   4557, 'Vietnam - Kon Tum',
           107.33, 13.92, 108.55, 15.42
         );
  _cs.Add(   4558, 'Vietnam - Quang Ngai',
           108.23, 14.53, 109.20, 15.49
         );
  _cs.Add(   4559, 'Vietnam - Binh Dinh, Khanh Hoa, Ninh Thuan',
           108.55, 11.25, 109.53, 14.71
         );
  _cs.Add(   4560, 'Vietnam - Binh Thuan, Dak Lak, Dak Nong, Gia Lai, Phu Yen',
           107.20, 10.43, 109.52, 14.61
         );
  _cs.Add(   1873, 'World - N hemisphere - 180'#$00b0'W to 174'#$00b0'W',
           -180.00, 0.00, -174.00, 84.00
         );
  _cs.Add(   1875, 'World - N hemisphere - 174'#$00b0'W to 168'#$00b0'W',
           -174.00, 0.00, -168.00, 84.00
         );
  _cs.Add(   1877, 'World - N hemisphere - 168'#$00b0'W to 162'#$00b0'W',
           -168.00, 0.00, -162.00, 84.00
         );
  _cs.Add(   1879, 'World - N hemisphere - 162'#$00b0'W to 156'#$00b0'W',
           -162.00, 0.00, -156.00, 84.00
         );
  _cs.Add(   1881, 'World - N hemisphere - 156'#$00b0'W to 150'#$00b0'W',
           -156.00, 0.00, -150.00, 84.00
         );
  _cs.Add(   1883, 'World - N hemisphere - 150'#$00b0'W to 144'#$00b0'W',
           -150.00, 0.00, -144.00, 84.00
         );
  _cs.Add(   1885, 'World - N hemisphere - 144'#$00b0'W to 138'#$00b0'W',
           -144.00, 0.00, -138.00, 84.00
         );
  _cs.Add(   1887, 'World - N hemisphere - 138'#$00b0'W to 132'#$00b0'W',
           -138.00, 0.00, -132.00, 84.00
         );
  _cs.Add(   1889, 'World - N hemisphere - 132'#$00b0'W to 126'#$00b0'W',
           -132.00, 0.00, -126.00, 84.00
         );
  _cs.Add(   1891, 'World - N hemisphere - 126'#$00b0'W to 120'#$00b0'W',
           -126.00, 0.00, -120.00, 84.00
         );
  _cs.Add(   1893, 'World - N hemisphere - 120'#$00b0'W to 114'#$00b0'W',
           -120.00, 0.00, -114.00, 84.00
         );
  _cs.Add(   1895, 'World - N hemisphere - 114'#$00b0'W to 108'#$00b0'W',
           -114.00, 0.00, -108.00, 84.00
         );
  _cs.Add(   1897, 'World - N hemisphere - 108'#$00b0'W to 102'#$00b0'W',
           -108.00, 0.00, -102.00, 84.00
         );
  _cs.Add(   1899, 'World - N hemisphere - 102'#$00b0'W to 96'#$00b0'W',
           -102.00, 0.00, -96.00, 84.00
         );
  _cs.Add(   1921, 'World - N hemisphere - 36'#$00b0'W to 30'#$00b0'W',
           -36.00, 0.00, -30.00, 84.00
         );
  _cs.Add(   1923, 'World - N hemisphere - 30'#$00b0'W to 24'#$00b0'W',
           -30.00, 0.00, -24.00, 84.00
         );
  _cs.Add(   1925, 'World - N hemisphere - 24'#$00b0'W to 18'#$00b0'W',
           -24.00, 0.00, -18.00, 84.00
         );
  _cs.Add(   1927, 'World - N hemisphere - 18'#$00b0'W to 12'#$00b0'W',
           -18.00, 0.00, -12.00, 84.00
         );
  _cs.Add(   1929, 'World - N hemisphere - 12'#$00b0'W to 6'#$00b0'W',
           -12.00, 0.00, -6.00, 84.00
         );
  _cs.Add(   1937, 'World - N hemisphere - 12'#$00b0'E to 18'#$00b0'E',
           12.00, 0.00, 18.00, 84.00
         );
  _cs.Add(   1939, 'World - N hemisphere - 18'#$00b0'E to 24'#$00b0'E',
           18.00, 0.00, 24.00, 84.00
         );
  _cs.Add(   1941, 'World - N hemisphere - 24'#$00b0'E to 30'#$00b0'E',
           24.00, 0.00, 30.00, 84.00
         );
  _cs.Add(   1943, 'World - N hemisphere - 30'#$00b0'E to 36'#$00b0'E',
           30.00, 0.00, 36.00, 84.00
         );
  _cs.Add(   1945, 'World - N hemisphere - 36'#$00b0'E to 42'#$00b0'E',
           36.00, 0.00, 42.00, 84.00
         );
  _cs.Add(   1967, 'World - N hemisphere - 102'#$00b0'E to 108'#$00b0'E',
           102.00, 0.00, 108.00, 84.00
         );
  _cs.Add(   1969, 'World - N hemisphere - 108'#$00b0'E to 114'#$00b0'E',
           108.00, 0.00, 114.00, 84.00
         );
  _cs.Add(   1971, 'World - N hemisphere - 114'#$00b0'E to 120'#$00b0'E',
           114.00, 0.00, 120.00, 84.00
         );
  _cs.Add(   1973, 'World - N hemisphere - 120'#$00b0'E to 126'#$00b0'E',
           120.00, 0.00, 126.00, 84.00
         );
  _cs.Add(   1975, 'World - N hemisphere - 126'#$00b0'E to 132'#$00b0'E',
           126.00, 0.00, 132.00, 84.00
         );
  _cs.Add(   1977, 'World - N hemisphere - 132'#$00b0'E to 138'#$00b0'E',
           132.00, 0.00, 138.00, 84.00
         );
  _cs.Add(   1979, 'World - N hemisphere - 138'#$00b0'E to 144'#$00b0'E',
           138.00, 0.00, 144.00, 84.00
         );
  _cs.Add(   1981, 'World - N hemisphere - 144'#$00b0'E to 150'#$00b0'E',
           144.00, 0.00, 150.00, 84.00
         );
  _cs.Add(   1983, 'World - N hemisphere - 150'#$00b0'E to 156'#$00b0'E',
           150.00, 0.00, 156.00, 84.00
         );
  _cs.Add(   1985, 'World - N hemisphere - 156'#$00b0'E to 162'#$00b0'E',
           156.00, 0.00, 162.00, 84.00
         );
  _cs.Add(   1987, 'World - N hemisphere - 162'#$00b0'E to 168'#$00b0'E',
           162.00, 0.00, 168.00, 84.00
         );
  _cs.Add(   1989, 'World - N hemisphere - 168'#$00b0'E to 174'#$00b0'E',
           168.00, 0.00, 174.00, 84.00
         );
end;

procedure prepare_33;
begin
  _cs.Add(   1991, 'World - N hemisphere - 174'#$00b0'E to 180'#$00b0'E',
           174.00, 0.00, 180.00, 84.00
         );
  _cs.Add(   1993, 'World - N hemisphere - 102'#$00b0'E to 108'#$00b0'E - by country and WGS 72BE',
           102.00, 0.00, 108.00, 84.00
         );
  _cs.Add(   1994, 'World - N hemisphere - 108'#$00b0'E to 114'#$00b0'E - by country and WGS 72BE',
           108.00, 0.00, 114.00, 84.00
         );
  _cs.Add(   1998, 'World - N hemisphere - 0'#$00b0'N to 84'#$00b0'N',
           -180.00, 0.00, 180.00, 84.00
         );
  _cs.Add(   2000, 'World - N hemisphere - 180'#$00b0'W to 174'#$00b0'W - by country',
           -180.00, 0.00, -174.00, 84.00
         );
  _cs.Add(   2002, 'World - N hemisphere - 174'#$00b0'W to 168'#$00b0'W - by country',
           -174.00, 0.00, -168.00, 84.00
         );
  _cs.Add(   2004, 'World - N hemisphere - 168'#$00b0'W to 162'#$00b0'W - by country',
           -168.00, 0.00, -162.00, 84.00
         );
  _cs.Add(   2006, 'World - N hemisphere - 162'#$00b0'W to 156'#$00b0'W - by country',
           -162.00, 0.00, -156.00, 84.00
         );
  _cs.Add(   2008, 'World - N hemisphere - 156'#$00b0'W to 150'#$00b0'W - by country',
           -156.00, 0.00, -150.00, 84.00
         );
  _cs.Add(   2010, 'World - N hemisphere - 150'#$00b0'W to 144'#$00b0'W - by country',
           -150.00, 0.00, -144.00, 84.00
         );
  _cs.Add(   2012, 'World - N hemisphere - 144'#$00b0'W to 138'#$00b0'W - by country',
           -144.00, 0.00, -138.00, 84.00
         );
  _cs.Add(   2014, 'World - N hemisphere - 138'#$00b0'W to 132'#$00b0'W - by country',
           -138.00, 0.00, -132.00, 84.00
         );
  _cs.Add(   2016, 'World - N hemisphere - 132'#$00b0'W to 126'#$00b0'W - by country',
           -132.00, 0.00, -126.00, 84.00
         );
  _cs.Add(   2018, 'World - N hemisphere - 126'#$00b0'W to 120'#$00b0'W - by country',
           -126.00, 0.00, -120.00, 84.00
         );
  _cs.Add(   2020, 'World - N hemisphere - 120'#$00b0'W to 114'#$00b0'W - by country',
           -120.00, 0.00, -114.00, 84.00
         );
  _cs.Add(   2022, 'World - N hemisphere - 114'#$00b0'W to 108'#$00b0'W - by country',
           -114.00, 0.00, -108.00, 84.00
         );
  _cs.Add(   2024, 'World - N hemisphere - 108'#$00b0'W to 102'#$00b0'W - by country',
           -108.00, 0.00, -102.00, 84.00
         );
  _cs.Add(   2026, 'World - N hemisphere - 102'#$00b0'W to 96'#$00b0'W - by country',
           -102.00, 0.00, -96.00, 84.00
         );
  _cs.Add(   2048, 'World - N hemisphere - 36'#$00b0'W to 30'#$00b0'W - by country',
           -36.00, 0.00, -30.00, 84.00
         );
  _cs.Add(   2050, 'World - N hemisphere - 30'#$00b0'W to 24'#$00b0'W - by country',
           -30.00, 0.00, -24.00, 84.00
         );
  _cs.Add(   2052, 'World - N hemisphere - 24'#$00b0'W to 18'#$00b0'W - by country',
           -24.00, 0.00, -18.00, 84.00
         );
  _cs.Add(   2054, 'World - N hemisphere - 18'#$00b0'W to 12'#$00b0'W - by country',
           -18.00, 0.00, -12.00, 84.00
         );
  _cs.Add(   2056, 'World - N hemisphere - 12'#$00b0'W to 6'#$00b0'W - by country',
           -12.00, 0.00, -6.00, 84.00
         );
  _cs.Add(   2060, 'World - N hemisphere - 0'#$00b0'E to 6'#$00b0'E - by country',
           0.00, 0.00, 6.00, 84.00
         );
  _cs.Add(   2064, 'World - N hemisphere - 12'#$00b0'E to 18'#$00b0'E - by country',
           12.00, 0.00, 18.00, 84.00
         );
  _cs.Add(   2066, 'World - N hemisphere - 18'#$00b0'E to 24'#$00b0'E - by country',
           18.00, 0.00, 24.00, 84.00
         );
  _cs.Add(   2302, 'World - N hemisphere - 3-degree CM 012'#$00b0'E',
           10.50, 0.00, 13.50, 84.00
         );
  _cs.Add(   2303, 'World - N hemisphere - 3-degree CM 015'#$00b0'E',
           13.50, 0.00, 16.50, 84.00
         );
  _cs.Add(   2304, 'World - N hemisphere - 3-degree CM 018'#$00b0'E',
           16.50, 0.00, 19.50, 84.00
         );
  _cs.Add(   2305, 'World - N hemisphere - 3-degree CM 021'#$00b0'E',
           19.50, 0.00, 22.50, 84.00
         );
  _cs.Add(   2306, 'World - N hemisphere - 3-degree CM 024'#$00b0'E',
           22.50, 0.00, 25.50, 84.00
         );
  _cs.Add(   2534, 'World - N hemisphere - 3-degree CM 027'#$00b0'E',
           25.50, 0.00, 28.50, 84.00
         );
  _cs.Add(   2535, 'World - N hemisphere - 3-degree CM 030'#$00b0'E',
           28.50, 0.00, 31.50, 84.00
         );
  _cs.Add(   2536, 'World - N hemisphere - 3-degree CM 033'#$00b0'E',
           31.50, 0.00, 34.50, 84.00
         );
  _cs.Add(   2537, 'World - N hemisphere - 3-degree CM 036'#$00b0'E',
           34.50, 0.00, 37.50, 84.00
         );
  _cs.Add(   2538, 'World - N hemisphere - 3-degree CM 039'#$00b0'E',
           37.50, 0.00, 40.50, 84.00
         );
  _cs.Add(   2539, 'World - N hemisphere - 3-degree CM 042'#$00b0'E',
           40.50, 0.00, 43.50, 84.00
         );
  _cs.Add(   2540, 'World - N hemisphere - 3-degree CM 045'#$00b0'E',
           43.50, 0.00, 46.50, 84.00
         );
  _cs.Add(   2604, 'World - N hemisphere - 3-degree CM 048'#$00b0'E',
           46.50, 0.00, 49.50, 84.00
         );
  _cs.Add(   2605, 'World - N hemisphere - 3-degree CM 051'#$00b0'E',
           49.50, 0.00, 52.50, 84.00
         );
  _cs.Add(   2606, 'World - N hemisphere - 3-degree CM 054'#$00b0'E',
           52.50, 0.00, 55.50, 84.00
         );
  _cs.Add(   2607, 'World - N hemisphere - 3-degree CM 057'#$00b0'E',
           55.50, 0.00, 58.50, 84.00
         );
  _cs.Add(   3464, 'World - N hemisphere - 12'#$00b0'E to 18'#$00b0'E - by country and WGS 72BE',
           12.00, 0.00, 18.00, 84.00
         );
  _cs.Add(   3465, 'World - N hemisphere - 18'#$00b0'E to 24'#$00b0'E - by country and WGS 72BE',
           18.00, 0.00, 24.00, 84.00
         );
  _cs.Add(   3914, 'World - N hemisphere - 3'#$00b0'E to 9'#$00b0'E - by country',
           2.66, 1.92, 10.38, 43.74
         );
  _cs.Add(   1900, 'World - S hemisphere - 102'#$00b0'W to 96'#$00b0'W',
           -102.00, -80.00, -96.00, 0.00
         );
  _cs.Add(   1901, 'World - N hemisphere - 96'#$00b0'W to 90'#$00b0'W',
           -96.00, 0.00, -90.00, 84.00
         );
  _cs.Add(   1903, 'World - N hemisphere - 90'#$00b0'W to 84'#$00b0'W',
           -90.00, 0.00, -84.00, 84.00
         );
  _cs.Add(   1905, 'World - N hemisphere - 84'#$00b0'W to 78'#$00b0'W',
           -84.00, 0.00, -78.00, 84.00
         );
  _cs.Add(   1907, 'World - N hemisphere - 78'#$00b0'W to 72'#$00b0'W',
           -78.00, 0.00, -72.00, 84.00
         );
  _cs.Add(   1909, 'World - N hemisphere - 72'#$00b0'W to 66'#$00b0'W',
           -72.00, 0.00, -66.00, 84.00
         );
  _cs.Add(   1911, 'World - N hemisphere - 66'#$00b0'W to 60'#$00b0'W',
           -66.00, 0.00, -60.00, 84.00
         );
  _cs.Add(   1913, 'World - N hemisphere - 60'#$00b0'W to 54'#$00b0'W',
           -60.00, 0.00, -54.00, 84.00
         );
  _cs.Add(   1915, 'World - N hemisphere - 54'#$00b0'W to 48'#$00b0'W',
           -54.00, 0.00, -48.00, 84.00
         );
  _cs.Add(   1917, 'World - N hemisphere - 48'#$00b0'W to 42'#$00b0'W',
           -48.00, 0.00, -42.00, 84.00
         );
  _cs.Add(   1919, 'World - N hemisphere - 42'#$00b0'W to 36'#$00b0'W',
           -42.00, 0.00, -36.00, 84.00
         );
  _cs.Add(   1931, 'World - N hemisphere - 6'#$00b0'W to 0'#$00b0'W',
           -6.00, 0.00, 0.00, 84.00
         );
  _cs.Add(   1934, 'World - S hemisphere - 0'#$00b0'E to 6'#$00b0'E',
           0.00, -80.00, 6.00, 0.00
         );
  _cs.Add(   1935, 'World - N hemisphere - 6'#$00b0'E to 12'#$00b0'E',
           6.00, 0.00, 12.00, 84.00
         );
  _cs.Add(   1947, 'World - N hemisphere - 42'#$00b0'E to 48'#$00b0'E',
           42.00, 0.00, 48.00, 84.00
         );
  _cs.Add(   1949, 'World - N hemisphere - 48'#$00b0'E to 54'#$00b0'E',
           48.00, 0.00, 54.00, 84.00
         );
  _cs.Add(   1951, 'World - N hemisphere - 54'#$00b0'E to 60'#$00b0'E',
           54.00, 0.00, 60.00, 84.00
         );
  _cs.Add(   1953, 'World - N hemisphere - 60'#$00b0'E to 66'#$00b0'E',
           60.00, 0.00, 66.00, 84.00
         );
  _cs.Add(   1955, 'World - N hemisphere - 66'#$00b0'E to 72'#$00b0'E',
           66.00, 0.00, 72.00, 84.00
         );
  _cs.Add(   1957, 'World - N hemisphere - 72'#$00b0'E to 78'#$00b0'E',
           72.00, 0.00, 78.00, 84.00
         );
  _cs.Add(   1959, 'World - N hemisphere - 78'#$00b0'E to 84'#$00b0'E',
           78.00, 0.00, 84.00, 84.00
         );
  _cs.Add(   1961, 'World - N hemisphere - 84'#$00b0'E to 90'#$00b0'E',
           84.00, 0.00, 90.00, 84.00
         );
  _cs.Add(   1963, 'World - N hemisphere - 90'#$00b0'E to 96'#$00b0'E',
           90.00, 0.00, 96.00, 84.00
         );
  _cs.Add(   1965, 'World - N hemisphere - 96'#$00b0'E to 102'#$00b0'E',
           96.00, 0.00, 102.00, 84.00
         );
  _cs.Add(   1968, 'World - S hemisphere - 102'#$00b0'E to 108'#$00b0'E',
           102.00, -80.00, 108.00, 0.00
         );
  _cs.Add(   1996, 'World - N hemisphere - north of 60'#$00b0'N',
           -180.00, 60.00, 180.00, 90.00
         );
  _cs.Add(   1999, 'World - S hemisphere - 0'#$00b0'N to 80'#$00b0'S',
           -180.00, -80.00, 180.00, 0.00
         );
  _cs.Add(   2027, 'World - S hemisphere - 102'#$00b0'W to 96'#$00b0'W - by country',
           -102.00, -80.00, -96.00, 0.00
         );
  _cs.Add(   2028, 'World - N hemisphere - 96'#$00b0'W to 90'#$00b0'W - by country',
           -96.00, 0.00, -90.00, 84.00
         );
  _cs.Add(   2030, 'World - N hemisphere - 90'#$00b0'W to 84'#$00b0'W - by country',
           -90.00, 0.00, -84.00, 84.00
         );
  _cs.Add(   2032, 'World - N hemisphere - 84'#$00b0'W to 78'#$00b0'W - by country',
           -84.00, 0.00, -78.00, 84.00
         );
  _cs.Add(   2034, 'World - N hemisphere - 78'#$00b0'W to 72'#$00b0'W - by country',
           -78.00, 0.00, -72.00, 84.00
         );
  _cs.Add(   2036, 'World - N hemisphere - 72'#$00b0'W to 66'#$00b0'W - by country',
           -72.00, 0.00, -66.00, 84.00
         );
  _cs.Add(   2038, 'World - N hemisphere - 66'#$00b0'W to 60'#$00b0'W - by country',
           -66.00, 0.00, -60.00, 84.00
         );
  _cs.Add(   2040, 'World - N hemisphere - 60'#$00b0'W to 54'#$00b0'W - by country',
           -60.00, 0.00, -54.00, 84.00
         );
  _cs.Add(   2042, 'World - N hemisphere - 54'#$00b0'W to 48'#$00b0'W - by country',
           -54.00, 0.00, -48.00, 84.00
         );
  _cs.Add(   2044, 'World - N hemisphere - 48'#$00b0'W to 42'#$00b0'W - by country',
           -48.00, 0.00, -42.00, 84.00
         );
  _cs.Add(   2046, 'World - N hemisphere - 42'#$00b0'W to 36'#$00b0'W - by country',
           -42.00, 0.00, -36.00, 84.00
         );
  _cs.Add(   2058, 'World - N hemisphere - 6'#$00b0'W to 0'#$00b0'W - by country',
           -6.00, 0.00, 0.00, 84.00
         );
  _cs.Add(   2061, 'World - S hemisphere - 0'#$00b0'E to 6'#$00b0'E - by country',
           0.00, -80.00, 6.00, 0.00
         );
  _cs.Add(   2062, 'World - N hemisphere - 6'#$00b0'E to 12'#$00b0'E - by country',
           6.00, 0.00, 12.00, 84.00
         );
  _cs.Add(   2074, 'World - N hemisphere - 42'#$00b0'E to 48'#$00b0'E - by country',
           42.00, 0.00, 48.00, 84.00
         );
  _cs.Add(   2076, 'World - N hemisphere - 48'#$00b0'E to 54'#$00b0'E - by country',
           48.00, 0.00, 54.00, 84.00
         );
  _cs.Add(   2078, 'World - N hemisphere - 54'#$00b0'E to 60'#$00b0'E - by country',
           54.00, 0.00, 60.00, 84.00
         );
  _cs.Add(   2080, 'World - N hemisphere - 60'#$00b0'E to 66'#$00b0'E - by country',
           60.00, 0.00, 66.00, 84.00
         );
  _cs.Add(   2082, 'World - N hemisphere - 66'#$00b0'E to 72'#$00b0'E - by country',
           66.00, 0.00, 72.00, 84.00
         );
  _cs.Add(   2084, 'World - N hemisphere - 72'#$00b0'E to 78'#$00b0'E - by country',
           72.00, 0.00, 78.00, 84.00
         );
  _cs.Add(   2086, 'World - N hemisphere - 78'#$00b0'E to 84'#$00b0'E - by country',
           78.00, 0.00, 84.00, 84.00
         );
  _cs.Add(   2088, 'World - N hemisphere - 84'#$00b0'E to 90'#$00b0'E - by country',
           84.00, 0.00, 90.00, 84.00
         );
  _cs.Add(   2090, 'World - N hemisphere - 90'#$00b0'E to 96'#$00b0'E - by country',
           90.00, 0.00, 96.00, 84.00
         );
  _cs.Add(   2092, 'World - N hemisphere - 96'#$00b0'E to 102'#$00b0'E - by country',
           96.00, 0.00, 102.00, 84.00
         );
  _cs.Add(   2095, 'World - S hemisphere - 102'#$00b0'E to 108'#$00b0'E - by country',
           102.00, -80.00, 108.00, 0.00
         );
  _cs.Add(   2608, 'World - N hemisphere - 3-degree CM 060'#$00b0'E',
           58.50, 0.00, 61.50, 84.00
         );
  _cs.Add(   2609, 'World - N hemisphere - 3-degree CM 063'#$00b0'E',
           61.50, 0.00, 64.50, 84.00
         );
  _cs.Add(   2610, 'World - N hemisphere - 3-degree CM 066'#$00b0'E',
           64.50, 0.00, 67.50, 84.00
         );
end;

procedure prepare_34;
begin
  _cs.Add(   2611, 'World - N hemisphere - 3-degree CM 069'#$00b0'E',
           67.50, 0.00, 70.50, 84.00
         );
  _cs.Add(   2612, 'World - N hemisphere - 3-degree CM 072'#$00b0'E',
           70.50, 0.00, 73.50, 84.00
         );
  _cs.Add(   2613, 'World - N hemisphere - 3-degree CM 075'#$00b0'E',
           73.50, 0.00, 76.50, 84.00
         );
  _cs.Add(   2614, 'World - N hemisphere - 3-degree CM 078'#$00b0'E',
           76.50, 0.00, 79.50, 84.00
         );
  _cs.Add(   2615, 'World - N hemisphere - 3-degree CM 081'#$00b0'E',
           79.50, 0.00, 82.50, 84.00
         );
  _cs.Add(   2616, 'World - N hemisphere - 3-degree CM 084'#$00b0'E',
           82.50, 0.00, 85.50, 84.00
         );
  _cs.Add(   2617, 'World - N hemisphere - 3-degree CM 087'#$00b0'E',
           85.50, 0.00, 88.50, 84.00
         );
  _cs.Add(   2618, 'World - N hemisphere - 3-degree CM 090'#$00b0'E',
           88.50, 0.00, 91.50, 84.00
         );
  _cs.Add(   2619, 'World - N hemisphere - 3-degree CM 093'#$00b0'E',
           91.50, 0.00, 94.50, 84.00
         );
  _cs.Add(   2620, 'World - N hemisphere - 3-degree CM 096'#$00b0'E',
           94.50, 0.00, 97.50, 84.00
         );
  _cs.Add(   2621, 'World - N hemisphere - 3-degree CM 099'#$00b0'E',
           97.50, 0.00, 100.50, 84.00
         );
  _cs.Add(   2622, 'World - N hemisphere - 3-degree CM 102'#$00b0'E',
           100.50, 0.00, 103.50, 84.00
         );
  _cs.Add(   2623, 'World - N hemisphere - 3-degree CM 105'#$00b0'E',
           103.50, 0.00, 106.50, 84.00
         );
  _cs.Add(   2624, 'World - N hemisphere - 3-degree CM 108'#$00b0'E',
           106.50, 0.00, 109.50, 84.00
         );
  _cs.Add(   2625, 'World - N hemisphere - 3-degree CM 111'#$00b0'E',
           109.50, 0.00, 112.50, 84.00
         );
  _cs.Add(   2626, 'World - N hemisphere - 3-degree CM 114'#$00b0'E',
           112.50, 0.00, 115.50, 84.00
         );
  _cs.Add(   2627, 'World - N hemisphere - 3-degree CM 117'#$00b0'E',
           115.50, 0.00, 118.50, 84.00
         );
  _cs.Add(   2628, 'World - N hemisphere - 3-degree CM 120'#$00b0'E',
           118.50, 0.00, 121.50, 84.00
         );
  _cs.Add(   2629, 'World - N hemisphere - 3-degree CM 123'#$00b0'E',
           121.50, 0.00, 124.50, 84.00
         );
  _cs.Add(   2630, 'World - N hemisphere - 3-degree CM 126'#$00b0'E',
           124.50, 0.00, 127.50, 84.00
         );
  _cs.Add(   2631, 'World - N hemisphere - 3-degree CM 129'#$00b0'E',
           127.50, 0.00, 130.50, 84.00
         );
  _cs.Add(   2632, 'World - N hemisphere - 3-degree CM 132'#$00b0'E',
           130.50, 0.00, 133.50, 84.00
         );
  _cs.Add(   2633, 'World - N hemisphere - 3-degree CM 135'#$00b0'E',
           133.50, 0.00, 136.50, 84.00
         );
  _cs.Add(   2634, 'World - N hemisphere - 3-degree CM 138'#$00b0'E',
           136.50, 0.00, 139.50, 84.00
         );
  _cs.Add(   2635, 'World - N hemisphere - 3-degree CM 141'#$00b0'E',
           139.50, 0.00, 142.50, 84.00
         );
  _cs.Add(   2636, 'World - N hemisphere - 3-degree CM 144'#$00b0'E',
           142.50, 0.00, 145.50, 84.00
         );
  _cs.Add(   2637, 'World - N hemisphere - 3-degree CM 147'#$00b0'E',
           145.50, 0.00, 148.50, 84.00
         );
  _cs.Add(   2638, 'World - N hemisphere - 3-degree CM 150'#$00b0'E',
           148.50, 0.00, 151.50, 84.00
         );
  _cs.Add(   2639, 'World - N hemisphere - 3-degree CM 153'#$00b0'E',
           151.50, 0.00, 154.50, 84.00
         );
  _cs.Add(   2640, 'World - N hemisphere - 3-degree CM 156'#$00b0'E',
           154.50, 0.00, 157.50, 84.00
         );
  _cs.Add(   2641, 'World - N hemisphere - 3-degree CM 159'#$00b0'E',
           157.50, 0.00, 160.50, 84.00
         );
  _cs.Add(   2642, 'World - N hemisphere - 3-degree CM 162'#$00b0'E',
           160.50, 0.00, 163.50, 84.00
         );
  _cs.Add(   2643, 'World - N hemisphere - 3-degree CM 165'#$00b0'E',
           163.50, 0.00, 166.50, 84.00
         );
  _cs.Add(   2644, 'World - N hemisphere - 3-degree CM 168'#$00b0'E',
           166.50, 0.00, 169.50, 84.00
         );
  _cs.Add(   2645, 'World - N hemisphere - 3-degree CM 171'#$00b0'E',
           169.50, 0.00, 172.50, 84.00
         );
  _cs.Add(   2646, 'World - N hemisphere - 3-degree CM 174'#$00b0'E',
           172.50, 0.00, 175.50, 84.00
         );
  _cs.Add(   2647, 'World - N hemisphere - 3-degree CM 177'#$00b0'E',
           175.50, 0.00, 178.50, 84.00
         );
  _cs.Add(   2648, 'World - N hemisphere - 3-degree CM 180'#$00b0'E',
           178.50, 0.00, -178.50, 84.00
         );
  _cs.Add(   2649, 'World - N hemisphere - 3-degree CM 177'#$00b0'W',
           -178.50, 0.00, -175.50, 84.00
         );
  _cs.Add(   2650, 'World - N hemisphere - 3-degree CM 174'#$00b0'W',
           -175.50, 0.00, -172.50, 84.00
         );
  _cs.Add(   2651, 'World - N hemisphere - 3-degree CM 171'#$00b0'W',
           -172.50, 0.00, -169.50, 84.00
         );
  _cs.Add(   2652, 'World - N hemisphere - 3-degree CM 168'#$00b0'W',
           -169.50, 0.00, -166.50, 84.00
         );
  _cs.Add(   3475, 'World - north of 0'#$00b0'N',
           -180.00, 0.00, 180.00, 90.00
         );
  _cs.Add(   3476, 'World - north of 30'#$00b0'N',
           -180.00, 30.00, 180.00, 90.00
         )         .MarkDeprecated( True );
  _cs.Add(   3480, 'World - north of 45'#$00b0'N',
           -180.00, 45.00, 180.00, 90.00
         );
  _cs.Add(   1874, 'World - S hemisphere - 180'#$00b0'W to 174'#$00b0'W',
           -180.00, -80.00, -174.00, 0.00
         );
  _cs.Add(   1876, 'World - S hemisphere - 174'#$00b0'W to 168'#$00b0'W',
           -174.00, -80.00, -168.00, 0.00
         );
  _cs.Add(   1878, 'World - S hemisphere - 168'#$00b0'W to 162'#$00b0'W',
           -168.00, -80.00, -162.00, 0.00
         );
  _cs.Add(   1880, 'World - S hemisphere - 162'#$00b0'W to 156'#$00b0'W',
           -162.00, -80.00, -156.00, 0.00
         );
  _cs.Add(   1882, 'World - S hemisphere - 156'#$00b0'W to 150'#$00b0'W',
           -156.00, -80.00, -150.00, 0.00
         );
  _cs.Add(   1884, 'World - S hemisphere - 150'#$00b0'W to 144'#$00b0'W',
           -150.00, -80.00, -144.00, 0.00
         );
  _cs.Add(   1886, 'World - S hemisphere - 144'#$00b0'W to 138'#$00b0'W',
           -144.00, -80.00, -138.00, 0.00
         );
  _cs.Add(   1888, 'World - S hemisphere - 138'#$00b0'W to 132'#$00b0'W',
           -138.00, -80.00, -132.00, 0.00
         );
  _cs.Add(   1890, 'World - S hemisphere - 132'#$00b0'W to 126'#$00b0'W',
           -132.00, -80.00, -126.00, 0.00
         );
  _cs.Add(   1892, 'World - S hemisphere - 126'#$00b0'W to 120'#$00b0'W',
           -126.00, -80.00, -120.00, 0.00
         );
  _cs.Add(   1894, 'World - S hemisphere - 120'#$00b0'W to 114'#$00b0'W',
           -120.00, -80.00, -114.00, 0.00
         );
  _cs.Add(   1896, 'World - S hemisphere - 114'#$00b0'W to 108'#$00b0'W',
           -114.00, -80.00, -108.00, 0.00
         );
  _cs.Add(   1898, 'World - S hemisphere - 108'#$00b0'W to 102'#$00b0'W',
           -108.00, -80.00, -102.00, 0.00
         );
  _cs.Add(   1910, 'World - S hemisphere - 72'#$00b0'W to 66'#$00b0'W',
           -72.00, -80.00, -66.00, 0.00
         );
  _cs.Add(   1912, 'World - S hemisphere - 66'#$00b0'W to 60'#$00b0'W',
           -66.00, -80.00, -60.00, 0.00
         );
  _cs.Add(   1914, 'World - S hemisphere - 60'#$00b0'W to 54'#$00b0'W',
           -60.00, -80.00, -54.00, 0.00
         );
  _cs.Add(   1916, 'World - S hemisphere - 54'#$00b0'W to 48'#$00b0'W',
           -54.00, -80.00, -48.00, 0.00
         );
  _cs.Add(   1918, 'World - S hemisphere - 48'#$00b0'W to 42'#$00b0'W',
           -48.00, -80.00, -42.00, 0.00
         );
  _cs.Add(   1920, 'World - S hemisphere - 42'#$00b0'W to 36'#$00b0'W',
           -42.00, -80.00, -36.00, 0.00
         );
  _cs.Add(   1922, 'World - S hemisphere - 36'#$00b0'W to 30'#$00b0'W',
           -36.00, -80.00, -30.00, 0.00
         );
  _cs.Add(   1924, 'World - S hemisphere - 30'#$00b0'W to 24'#$00b0'W',
           -30.00, -80.00, -24.00, 0.00
         );
  _cs.Add(   1926, 'World - S hemisphere - 24'#$00b0'W to 18'#$00b0'W',
           -24.00, -80.00, -18.00, 0.00
         );
  _cs.Add(   1928, 'World - S hemisphere - 18'#$00b0'W to 12'#$00b0'W',
           -18.00, -80.00, -12.00, 0.00
         );
  _cs.Add(   1930, 'World - S hemisphere - 12'#$00b0'W to 6'#$00b0'W',
           -12.00, -80.00, -6.00, 0.00
         );
  _cs.Add(   1932, 'World - S hemisphere - 6'#$00b0'W to 0'#$00b0'W',
           -6.00, -80.00, 0.00, 0.00
         );
  _cs.Add(   1936, 'World - S hemisphere - 6'#$00b0'E to 12'#$00b0'E',
           6.00, -80.00, 12.00, 0.00
         );
  _cs.Add(   1938, 'World - S hemisphere - 12'#$00b0'E to 18'#$00b0'E',
           12.00, -80.00, 18.00, 0.00
         );
  _cs.Add(   1940, 'World - S hemisphere - 18'#$00b0'E to 24'#$00b0'E',
           18.00, -80.00, 24.00, 0.00
         );
  _cs.Add(   1942, 'World - S hemisphere - 24'#$00b0'E to 30'#$00b0'E',
           24.00, -80.00, 30.00, 0.00
         );
  _cs.Add(   1944, 'World - S hemisphere - 30'#$00b0'E to 36'#$00b0'E',
           30.00, -80.00, 36.00, 0.00
         );
  _cs.Add(   1946, 'World - S hemisphere - 36'#$00b0'E to 42'#$00b0'E',
           36.00, -80.00, 42.00, 0.00
         );
  _cs.Add(   1948, 'World - S hemisphere - 42'#$00b0'E to 48'#$00b0'E',
           42.00, -80.00, 48.00, 0.00
         );
  _cs.Add(   1950, 'World - S hemisphere - 48'#$00b0'E to 54'#$00b0'E',
           48.00, -80.00, 54.00, 0.00
         );
  _cs.Add(   1952, 'World - S hemisphere - 54'#$00b0'E to 60'#$00b0'E',
           54.00, -80.00, 60.00, 0.00
         );
  _cs.Add(   1954, 'World - S hemisphere - 60'#$00b0'E to 66'#$00b0'E',
           60.00, -80.00, 66.00, 0.00
         );
  _cs.Add(   1956, 'World - S hemisphere - 66'#$00b0'E to 72'#$00b0'E',
           66.00, -80.00, 72.00, 0.00
         );
  _cs.Add(   1958, 'World - S hemisphere - 72'#$00b0'E to 78'#$00b0'E',
           72.00, -80.00, 78.00, 0.00
         );
  _cs.Add(   1960, 'World - S hemisphere - 78'#$00b0'E to 84'#$00b0'E',
           78.00, -80.00, 84.00, 0.00
         );
  _cs.Add(   1970, 'World - S hemisphere - 108'#$00b0'E to 114'#$00b0'E',
           108.00, -80.00, 114.00, 0.00
         );
  _cs.Add(   2068, 'World - N hemisphere - 24'#$00b0'E to 30'#$00b0'E - by country',
           24.00, 0.00, 30.00, 84.00
         );
  _cs.Add(   2070, 'World - N hemisphere - 30'#$00b0'E to 36'#$00b0'E - by country',
           30.00, 0.00, 36.00, 84.00
         );
  _cs.Add(   2072, 'World - N hemisphere - 36'#$00b0'E to 42'#$00b0'E - by country',
           36.00, 0.00, 42.00, 84.00
         );
  _cs.Add(   2094, 'World - N hemisphere - 102'#$00b0'E to 108'#$00b0'E - by country',
           102.00, 0.00, 108.00, 84.00
         );
  _cs.Add(   2096, 'World - N hemisphere - 108'#$00b0'E to 114'#$00b0'E - by country',
           108.00, 0.00, 114.00, 84.00
         );
  _cs.Add(   2098, 'World - N hemisphere - 114'#$00b0'E to 120'#$00b0'E - by country',
           114.00, 0.00, 120.00, 84.00
         );
  _cs.Add(   2100, 'World - N hemisphere - 120'#$00b0'E to 126'#$00b0'E - by country',
           120.00, 0.00, 126.00, 84.00
         );
  _cs.Add(   2102, 'World - N hemisphere - 126'#$00b0'E to 132'#$00b0'E - by country',
           126.00, 0.00, 132.00, 84.00
         );
  _cs.Add(   2104, 'World - N hemisphere - 132'#$00b0'E to 138'#$00b0'E - by country',
           132.00, 0.00, 138.00, 84.00
         );
  _cs.Add(   2106, 'World - N hemisphere - 138'#$00b0'E to 144'#$00b0'E - by country',
           138.00, 0.00, 144.00, 84.00
         );
  _cs.Add(   2108, 'World - N hemisphere - 144'#$00b0'E to 150'#$00b0'E - by country',
           144.00, 0.00, 150.00, 84.00
         );
  _cs.Add(   2110, 'World - N hemisphere - 150'#$00b0'E to 156'#$00b0'E - by country',
           150.00, 0.00, 156.00, 84.00
         );
  _cs.Add(   2112, 'World - N hemisphere - 156'#$00b0'E to 162'#$00b0'E - by country',
           156.00, 0.00, 162.00, 84.00
         );
  _cs.Add(   2114, 'World - N hemisphere - 162'#$00b0'E to 168'#$00b0'E - by country',
           162.00, 0.00, 168.00, 84.00
         );
  _cs.Add(   2116, 'World - N hemisphere - 168'#$00b0'E to 174'#$00b0'E - by country',
           168.00, 0.00, 174.00, 84.00
         );
  _cs.Add(   2118, 'World - N hemisphere - 174'#$00b0'E to 180'#$00b0'E - by country',
           174.00, 0.00, 180.00, 84.00
         );
end;

procedure prepare_35;
begin
  _cs.Add(   2299, 'World - N hemisphere - 3-degree CM 003'#$00b0'E',
           1.50, 0.00, 4.50, 84.00
         );
  _cs.Add(   2300, 'World - N hemisphere - 3-degree CM 006'#$00b0'E',
           4.50, 0.00, 7.50, 84.00
         );
  _cs.Add(   2301, 'World - N hemisphere - 3-degree CM 009'#$00b0'E',
           7.50, 0.00, 10.50, 84.00
         );
  _cs.Add(   1972, 'World - S hemisphere - 114'#$00b0'E to 120'#$00b0'E',
           114.00, -80.00, 120.00, 0.00
         );
  _cs.Add(   1974, 'World - S hemisphere - 120'#$00b0'E to 126'#$00b0'E',
           120.00, -80.00, 126.00, 0.00
         );
  _cs.Add(   1976, 'World - S hemisphere - 126'#$00b0'E to 132'#$00b0'E',
           126.00, -80.00, 132.00, 0.00
         );
  _cs.Add(   1978, 'World - S hemisphere - 132'#$00b0'E to 138'#$00b0'E',
           132.00, -80.00, 138.00, 0.00
         );
  _cs.Add(   1980, 'World - S hemisphere - 138'#$00b0'E to 144'#$00b0'E',
           138.00, -80.00, 144.00, 0.00
         );
  _cs.Add(   1982, 'World - S hemisphere - 144'#$00b0'E to 150'#$00b0'E',
           144.00, -80.00, 150.00, 0.00
         );
  _cs.Add(   1984, 'World - S hemisphere - 150'#$00b0'E to 156'#$00b0'E',
           150.00, -80.00, 156.00, 0.00
         );
  _cs.Add(   1986, 'World - S hemisphere - 156'#$00b0'E to 162'#$00b0'E',
           156.00, -80.00, 162.00, 0.00
         );
  _cs.Add(   1988, 'World - S hemisphere - 162'#$00b0'E to 168'#$00b0'E',
           162.00, -80.00, 168.00, 0.00
         );
  _cs.Add(   1990, 'World - S hemisphere - 168'#$00b0'E to 174'#$00b0'E',
           168.00, -80.00, 174.00, 0.00
         );
  _cs.Add(   1992, 'World - S hemisphere - 174'#$00b0'E to 180'#$00b0'E',
           174.00, -80.00, 180.00, 0.00
         );
  _cs.Add(   1995, 'World - S hemisphere - 108'#$00b0'E to 114'#$00b0'E - by country and WGS 72BE',
           108.00, -80.00, 114.00, 0.00
         );
  _cs.Add(   2001, 'World - S hemisphere - 180'#$00b0'W to 174'#$00b0'W - by country',
           -180.00, -80.00, -174.00, 0.00
         );
  _cs.Add(   2003, 'World - S hemisphere - 174'#$00b0'W to 168'#$00b0'W - by country',
           -174.00, -80.00, -168.00, 0.00
         );
  _cs.Add(   2005, 'World - S hemisphere - 168'#$00b0'W to 162'#$00b0'W - by country',
           -168.00, -80.00, -162.00, 0.00
         );
  _cs.Add(   2007, 'World - S hemisphere - 162'#$00b0'W to 156'#$00b0'W - by country',
           -162.00, -80.00, -156.00, 0.00
         );
  _cs.Add(   2009, 'World - S hemisphere - 156'#$00b0'W to 150'#$00b0'W - by country',
           -156.00, -80.00, -150.00, 0.00
         );
  _cs.Add(   2011, 'World - S hemisphere - 150'#$00b0'W to 144'#$00b0'W - by country',
           -150.00, -80.00, -144.00, 0.00
         );
  _cs.Add(   2013, 'World - S hemisphere - 144'#$00b0'W to 138'#$00b0'W - by country',
           -144.00, -80.00, -138.00, 0.00
         );
  _cs.Add(   2015, 'World - S hemisphere - 138'#$00b0'W to 132'#$00b0'W - by country',
           -138.00, -80.00, -132.00, 0.00
         );
  _cs.Add(   2017, 'World - S hemisphere - 132'#$00b0'W to 126'#$00b0'W - by country',
           -132.00, -80.00, -126.00, 0.00
         );
  _cs.Add(   2019, 'World - S hemisphere - 126'#$00b0'W to 120'#$00b0'W - by country',
           -126.00, -80.00, -120.00, 0.00
         );
  _cs.Add(   2021, 'World - S hemisphere - 120'#$00b0'W to 114'#$00b0'W - by country',
           -120.00, -80.00, -114.00, 0.00
         );
  _cs.Add(   2023, 'World - S hemisphere - 114'#$00b0'W to 108'#$00b0'W - by country',
           -114.00, -80.00, -108.00, 0.00
         );
  _cs.Add(   2025, 'World - S hemisphere - 108'#$00b0'W to 102'#$00b0'W - by country',
           -108.00, -80.00, -102.00, 0.00
         );
  _cs.Add(   2037, 'World - S hemisphere - 72'#$00b0'W to 66'#$00b0'W - by country',
           -72.00, -80.00, -66.00, 0.00
         );
  _cs.Add(   2039, 'World - S hemisphere - 66'#$00b0'W to 60'#$00b0'W - by country',
           -66.00, -80.00, -60.00, 0.00
         );
  _cs.Add(   2041, 'World - S hemisphere - 60'#$00b0'W to 54'#$00b0'W - by country',
           -60.00, -80.00, -54.00, 0.00
         );
  _cs.Add(   2043, 'World - S hemisphere - 54'#$00b0'W to 48'#$00b0'W - by country',
           -54.00, -80.00, -48.00, 0.00
         );
  _cs.Add(   2045, 'World - S hemisphere - 48'#$00b0'W to 42'#$00b0'W - by country',
           -48.00, -80.00, -42.00, 0.00
         );
  _cs.Add(   2047, 'World - S hemisphere - 42'#$00b0'W to 36'#$00b0'W - by country',
           -42.00, -80.00, -36.00, 0.00
         );
  _cs.Add(   2049, 'World - S hemisphere - 36'#$00b0'W to 30'#$00b0'W - by country',
           -36.00, -80.00, -30.00, 0.00
         );
  _cs.Add(   2051, 'World - S hemisphere - 30'#$00b0'W to 24'#$00b0'W - by country',
           -30.00, -80.00, -24.00, 0.00
         );
  _cs.Add(   2053, 'World - S hemisphere - 24'#$00b0'W to 18'#$00b0'W - by country',
           -24.00, -80.00, -18.00, 0.00
         );
  _cs.Add(   2055, 'World - S hemisphere - 18'#$00b0'W to 12'#$00b0'W - by country',
           -18.00, -80.00, -12.00, 0.00
         );
  _cs.Add(   2057, 'World - S hemisphere - 12'#$00b0'W to 6'#$00b0'W - by country',
           -12.00, -80.00, -6.00, 0.00
         );
  _cs.Add(   2059, 'World - S hemisphere - 6'#$00b0'W to 0'#$00b0'W - by country',
           -6.00, -80.00, 0.00, 0.00
         );
  _cs.Add(   2063, 'World - S hemisphere - 6'#$00b0'E to 12'#$00b0'E - by country',
           6.00, -80.00, 12.00, 0.00
         );
  _cs.Add(   2113, 'World - S hemisphere - 156'#$00b0'E to 162'#$00b0'E - by country',
           156.00, -80.00, 162.00, 0.00
         );
  _cs.Add(   2115, 'World - S hemisphere - 162'#$00b0'E to 168'#$00b0'E - by country',
           162.00, -80.00, 168.00, 0.00
         );
  _cs.Add(   2117, 'World - S hemisphere - 168'#$00b0'E to 174'#$00b0'E - by country',
           168.00, -80.00, 174.00, 0.00
         );
  _cs.Add(   2119, 'World - S hemisphere - 174'#$00b0'E to 180'#$00b0'E - by country',
           174.00, -80.00, 180.00, 0.00
         );
  _cs.Add(   1257, 'Yemen',
           41.08, 8.95, 57.96, 19.00
         );
  _cs.Add(   1258, 'Yugoslavia - Serbia and Montenegro',
           18.44, 41.82, 23.05, 46.23
         )         .MarkDeprecated( True );
  _cs.Add(   1260, 'Zambia',
           21.99, -18.08, 33.71, -8.19
         );
  _cs.Add(   1261, 'Zimbabwe',
           25.23, -22.42, 33.08, -15.61
         );
  _cs.Add(   1340, 'Yemen - South Yemen - mainland',
           43.37, 12.54, 53.14, 19.00
         );
  _cs.Add(   1490, 'Yemen - 42'#$00b0'E to 48'#$00b0'E',
           42.00, 11.57, 48.01, 17.95
         );
  _cs.Add(   1491, 'Yemen - 48'#$00b0'E to 54'#$00b0'E',
           48.00, 9.45, 54.01, 19.00
         );
  _cs.Add(   1492, 'Yemen - South Yemen - mainland west of 48'#$00b0'E',
           43.37, 12.54, 48.01, 17.95
         );
  _cs.Add(   1493, 'Yemen - South Yemen - mainland east of 48'#$00b0'E',
           48.00, 13.94, 53.14, 19.00
         );
  _cs.Add(   1902, 'World - S hemisphere - 96'#$00b0'W to 90'#$00b0'W',
           -96.00, -80.00, -90.00, 0.00
         );
  _cs.Add(   1904, 'World - S hemisphere - 90'#$00b0'W to 84'#$00b0'W',
           -90.00, -80.00, -84.00, 0.00
         );
  _cs.Add(   1906, 'World - S hemisphere - 84'#$00b0'W to 78'#$00b0'W',
           -84.00, -80.00, -78.00, 0.00
         );
  _cs.Add(   1908, 'World - S hemisphere - 78'#$00b0'W to 72'#$00b0'W',
           -78.00, -80.00, -72.00, 0.00
         );
  _cs.Add(   1962, 'World - S hemisphere - 84'#$00b0'E to 90'#$00b0'E',
           84.00, -80.00, 90.00, 0.00
         );
  _cs.Add(   1964, 'World - S hemisphere - 90'#$00b0'E to 96'#$00b0'E',
           90.00, -80.00, 96.00, 0.00
         );
  _cs.Add(   1966, 'World - S hemisphere - 96'#$00b0'E to 102'#$00b0'E',
           96.00, -80.00, 102.00, 0.00
         );
  _cs.Add(   1997, 'World - S hemisphere - south of 60'#$00b0'S',
           -180.00, -90.00, 180.00, -60.00
         );
  _cs.Add(   2029, 'World - S hemisphere - 96'#$00b0'W to 90'#$00b0'W - by country',
           -96.00, -80.00, -90.00, 0.00
         );
  _cs.Add(   2031, 'World - S hemisphere - 90'#$00b0'W to 84'#$00b0'W - by country',
           -90.00, -80.00, -84.00, 0.00
         );
  _cs.Add(   2033, 'World - S hemisphere - 84'#$00b0'W to 78'#$00b0'W - by country',
           -84.00, -80.00, -78.00, 0.00
         );
  _cs.Add(   2035, 'World - S hemisphere - 78'#$00b0'W to 72'#$00b0'W - by country',
           -78.00, -80.00, -72.00, 0.00
         );
  _cs.Add(   2087, 'World - S hemisphere - 78'#$00b0'E to 84'#$00b0'E - by country',
           78.00, -80.00, 84.00, 0.00
         );
  _cs.Add(   2089, 'World - S hemisphere - 84'#$00b0'E to 90'#$00b0'E - by country',
           84.00, -80.00, 90.00, 0.00
         );
  _cs.Add(   2091, 'World - S hemisphere - 90'#$00b0'E to 96'#$00b0'E - by country',
           90.00, -80.00, 96.00, 0.00
         );
  _cs.Add(   2093, 'World - S hemisphere - 96'#$00b0'E to 102'#$00b0'E - by country',
           96.00, -80.00, 102.00, 0.00
         );
  _cs.Add(   2346, 'World - WGS72 BE to WGS 84 - by country',
           -180.00, -90.00, 180.00, 90.00
         );
  _cs.Add(   2830, 'World (by country)',
           -180.00, -90.00, 180.00, 90.00
         );
  _cs.Add(   3332, 'Yemen - onshore',
           41.80, 12.09, 54.53, 19.00
         );
  _cs.Add(   3473, 'World - south of 40'#$00b0'S',
           -180.00, -90.00, 180.00, -40.00
         )         .MarkDeprecated( True );
  _cs.Add(   3474, 'World - south of 0'#$00b0'N',
           -180.00, -90.00, 180.00, 0.00
         );
  _cs.Add(   4002, 'Yemen - east of 54'#$00b0'E',
           54.00, 8.95, 57.96, 14.95
         );
  _cs.Add(   4006, 'Yemen - west of 42'#$00b0'E',
           41.08, 14.73, 42.00, 16.36
         );
  _cs.Add(   4520, 'World centred on 90'#$00b0'W',
           90.01, -90.00, 89.99, 90.00
         );
  _cs.Add(   4523, 'World centred on 150'#$00b0'E',
           -29.99, -90.00, -30.01, 90.00
         );
  _cs.Add(   4586, 'World - south of 50'#$00b0'S',
           -180.00, -90.00, 180.00, -50.00
         );
  _cs.Add(   2065, 'World - S hemisphere - 12'#$00b0'E to 18'#$00b0'E - by country',
           12.00, -80.00, 18.00, 0.00
         );
  _cs.Add(   2067, 'World - S hemisphere - 18'#$00b0'E to 24'#$00b0'E - by country',
           18.00, -80.00, 24.00, 0.00
         );
  _cs.Add(   2069, 'World - S hemisphere - 24'#$00b0'E to 30'#$00b0'E - by country',
           24.00, -80.00, 30.00, 0.00
         );
  _cs.Add(   2071, 'World - S hemisphere - 30'#$00b0'E to 36'#$00b0'E - by country',
           30.00, -80.00, 36.00, 0.00
         );
  _cs.Add(   2073, 'World - S hemisphere - 36'#$00b0'E to 42'#$00b0'E - by country',
           36.00, -80.00, 42.00, 0.00
         );
  _cs.Add(   2075, 'World - S hemisphere - 42'#$00b0'E to 48'#$00b0'E - by country',
           42.00, -80.00, 48.00, 0.00
         );
  _cs.Add(   2077, 'World - S hemisphere - 48'#$00b0'E to 54'#$00b0'E - by country',
           48.00, -80.00, 54.00, 0.00
         );
  _cs.Add(   2079, 'World - S hemisphere - 54'#$00b0'E to 60'#$00b0'E - by country',
           54.00, -80.00, 60.00, 0.00
         );
  _cs.Add(   2081, 'World - S hemisphere - 60'#$00b0'E to 66'#$00b0'E - by country',
           60.00, -80.00, 66.00, 0.00
         );
  _cs.Add(   2083, 'World - S hemisphere - 66'#$00b0'E to 72'#$00b0'E - by country',
           66.00, -80.00, 72.00, 0.00
         );
  _cs.Add(   2085, 'World - S hemisphere - 72'#$00b0'E to 78'#$00b0'E - by country',
           72.00, -80.00, 78.00, 0.00
         );
  _cs.Add(   2097, 'World - S hemisphere - 108'#$00b0'E to 114'#$00b0'E - by country',
           108.00, -80.00, 114.00, 0.00
         );
  _cs.Add(   2099, 'World - S hemisphere - 114'#$00b0'E to 120'#$00b0'E - by country',
           114.00, -80.00, 120.00, 0.00
         );
  _cs.Add(   2101, 'World - S hemisphere - 120'#$00b0'E to 126'#$00b0'E - by country',
           120.00, -80.00, 126.00, 0.00
         );
  _cs.Add(   2103, 'World - S hemisphere - 126'#$00b0'E to 132'#$00b0'E - by country',
           126.00, -80.00, 132.00, 0.00
         );
  _cs.Add(   2105, 'World - S hemisphere - 132'#$00b0'E to 138'#$00b0'E - by country',
           132.00, -80.00, 138.00, 0.00
         );
  _cs.Add(   2107, 'World - S hemisphere - 138'#$00b0'E to 144'#$00b0'E - by country',
           138.00, -80.00, 144.00, 0.00
         );
  _cs.Add(   2109, 'World - S hemisphere - 144'#$00b0'E to 150'#$00b0'E - by country',
           144.00, -80.00, 150.00, 0.00
         );
  _cs.Add(   2111, 'World - S hemisphere - 150'#$00b0'E to 156'#$00b0'E - by country',
           150.00, -80.00, 156.00, 0.00
         );
  _cs.Add(   3033, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 90'#$00b0'W to 72'#$00b0'W (SS16-18)',
           -90.00, -76.00, -72.00, -72.00
         );
end;

procedure prepare_36;
begin
  _cs.Add(   3034, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 72'#$00b0'W to 54'#$00b0'W (SS19-21)',
           -72.00, -76.00, -54.00, -72.00
         );
  _cs.Add(   3035, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 36'#$00b0'W to 18'#$00b0'W (SS25-27)',
           -36.00, -76.00, -18.00, -72.00
         );
  _cs.Add(   3036, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 18'#$00b0'W to 0'#$00b0'W (SS28-30)',
           -18.00, -76.00, 0.00, -72.00
         );
  _cs.Add(   3037, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 0'#$00b0'E to 18'#$00b0'E (SS31-33)',
           0.00, -76.00, 18.00, -72.00
         );
  _cs.Add(   3038, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 18'#$00b0'E to 36'#$00b0'E (SS34-36)',
           18.00, -76.00, 36.00, -72.00
         );
  _cs.Add(   3039, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 36'#$00b0'E to 54'#$00b0'E (SS37-39)',
           36.00, -76.00, 54.00, -72.00
         );
  _cs.Add(   3040, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 54'#$00b0'E to 72'#$00b0'E (SS40-42)',
           54.00, -76.00, 72.00, -72.00
         );
  _cs.Add(   3041, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 72'#$00b0'E to 90'#$00b0'E (SS43-45)',
           72.00, -76.00, 90.00, -72.00
         );
  _cs.Add(   3042, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 90'#$00b0'E to 108'#$00b0'E (SS46-48)',
           90.00, -76.00, 108.00, -72.00
         );
  _cs.Add(   3043, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 108'#$00b0'E to 126'#$00b0'E (SS49-51)',
           108.00, -76.00, 126.00, -72.00
         );
  _cs.Add(   3044, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 126'#$00b0'E to 144'#$00b0'E (SS52-54)',
           126.00, -76.00, 144.00, -72.00
         );
  _cs.Add(   3045, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 144'#$00b0'E to 162'#$00b0'E (SS55-57)',
           144.00, -76.00, 162.00, -72.00
         );
  _cs.Add(   3046, 'Antarctica - 72'#$00b0'S to 76'#$00b0'S, 162'#$00b0'E to 180'#$00b0'E (SS58-60)',
           162.00, -76.00, 180.00, -72.00
         );
  _cs.Add(   3047, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 180'#$00b0'W to 156'#$00b0'W (ST01-04)',
           -180.00, -80.00, -156.00, -76.00
         );
  _cs.Add(   3048, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 156'#$00b0'W to 132'#$00b0'W (ST05-08)',
           -156.00, -80.00, -132.00, -76.00
         );
  _cs.Add(   3049, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 132'#$00b0'W to 108'#$00b0'W (ST09-12)',
           -132.00, -80.00, -108.00, -76.00
         );
  _cs.Add(   3050, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 108'#$00b0'W to 84'#$00b0'W (ST13-16)',
           -108.00, -80.00, -84.00, -76.00
         );
  _cs.Add(   3051, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 84'#$00b0'W to 60'#$00b0'W (ST17-20)',
           -84.00, -80.00, -60.00, -76.00
         );
  _cs.Add(   3052, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 60'#$00b0'W to 36'#$00b0'W (ST21-24)',
           -60.00, -80.00, -36.00, -76.00
         );
  _cs.Add(   3053, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 36'#$00b0'W to 12'#$00b0'W (ST25-28)',
           -36.00, -80.00, -12.00, -76.00
         );
  _cs.Add(   3054, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 12'#$00b0'W to 12'#$00b0'E (ST29-32)',
           -12.00, -80.00, 12.00, -76.00
         );
  _cs.Add(   3055, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 12'#$00b0'E to 36'#$00b0'E (ST33-36)',
           12.00, -80.00, 36.00, -76.00
         );
  _cs.Add(   3056, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 36'#$00b0'E to 60'#$00b0'E (ST37-40)',
           36.00, -80.00, 60.00, -76.00
         );
  _cs.Add(   3057, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 60'#$00b0'E to 84'#$00b0'E (ST41-44)',
           60.00, -80.00, 84.00, -76.00
         );
  _cs.Add(   3058, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 84'#$00b0'E to 108'#$00b0'E (ST45-48)',
           84.00, -80.00, 108.00, -76.00
         );
  _cs.Add(   3059, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 108'#$00b0'E to 132'#$00b0'E (ST49-52)',
           108.00, -80.00, 132.00, -76.00
         );
  _cs.Add(   3060, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 132'#$00b0'E to 156'#$00b0'E (ST53-56)',
           132.00, -80.00, 156.00, -76.00
         );
  _cs.Add(   3061, 'Antarctica - 76'#$00b0'S to 80'#$00b0'S, 156'#$00b0'E to 180'#$00b0'E (ST57-60)',
           156.00, -80.00, 180.00, -76.00
         );
  _cs.Add(   3062, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 180'#$00b0'W to 150'#$00b0'W (SU01-05)',
           -180.00, -84.00, -150.00, -80.00
         );
  _cs.Add(   3063, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 150'#$00b0'W to 120'#$00b0'W (SU06-10)',
           -150.00, -84.00, -120.00, -80.00
         );
  _cs.Add(   3064, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 120'#$00b0'W to 90'#$00b0'W (SU11-15)',
           -120.00, -84.00, -90.00, -80.00
         );
  _cs.Add(   3065, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 90'#$00b0'W to 60'#$00b0'W (SU16-20)',
           -90.00, -84.00, -60.00, -80.00
         );
  _cs.Add(   3066, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 60'#$00b0'W to 30'#$00b0'W (SU21-25)',
           -60.00, -84.00, -30.00, -80.00
         );
  _cs.Add(   3067, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 30'#$00b0'W to 0'#$00b0'W (SU26-30)',
           -30.00, -84.00, 0.00, -80.00
         );
  _cs.Add(   3068, 'Antarctica - 80'#$00b0'S to 84'#$00b0'S, 0'#$00b0'E to 30'#$00b0'E (SU31-35)',
           0.00, -84.00, 30.00, -80.00
         );
  _cs.Add(   3855, 'Antarctica - Pennell Coast region',
           160.00, -73.00, 172.00, -69.50
         );
  _cs.Add(   3856, 'Antarctica - Ross Ice Shelf Region',
           150.00, -90.00, -150.00, -76.00
         );
  _cs.Add(   4119, 'Arctic - 67'#$00b0'50''N to 62'#$00b0'50''N, 59'#$00b0'W to 42'#$00b0'W',
           -59.00, 62.83, -42.00, 67.84
         );
  _cs.Add(   4120, 'Arctic - 67'#$00b0'50''N to 62'#$00b0'50''N, 42'#$00b0'W to 25'#$00b0'W',
           -42.00, 62.83, -25.00, 67.84
         );
  _cs.Add(   4123, 'Arctic - 64'#$00b0'30''N to 59'#$00b0'30''N, 59'#$00b0'W to 44'#$00b0'W',
           -59.00, 59.50, -44.00, 64.51
         );
  _cs.Add(   4124, 'Arctic - 64'#$00b0'30''N to 59'#$00b0'30''N, 44'#$00b0'W to 29'#$00b0'W',
           -44.00, 59.50, -29.00, 64.51
         );
  _cs.Add(   4489, 'Antarctica - Adelie Land coastal area west of 138'#$00b0'E',
           136.00, -66.73, 138.01, -65.61
         );
  _cs.Add(   4492, 'Antarctica - Adelie Land coastal area east of 138'#$00b0'E',
           138.00, -67.13, 142.00, -66.10
         );
  _cs.Add(   1033, 'Argentina',
           -73.59, -58.41, -52.63, -21.78
         );
  _cs.Add(   1484, 'Argentina - 42.5'#$00b0'S to 50.3'#$00b0'S and 70.5'#$00b0'W to 67.5'#$00b0'W',
           -70.50, -50.34, -67.49, -42.49
         );
  _cs.Add(   1610, 'Argentina - 67.5'#$00b0'W to 64.5'#$00b0'W onshore',
           -67.50, -55.11, -64.49, -21.78
         );
  _cs.Add(   1611, 'Argentina - 64.5'#$00b0'W to 61.5'#$00b0'W onshore',
           -64.50, -54.91, -61.50, -21.99
         );
  _cs.Add(   1612, 'Argentina - 61.5'#$00b0'W to 58.5'#$00b0'W onshore',
           -61.51, -39.06, -58.50, -23.37
         );
  _cs.Add(   1613, 'Argentina - 58.5'#$00b0'W to 55.5'#$00b0'W onshore',
           -58.50, -38.59, -55.49, -24.84
         );
  _cs.Add(   4019, 'Arctic - 87'#$00b0'N to 75'#$00b0'N, 156'#$00b0'W to 66'#$00b0'W',
           -156.00, 75.00, -66.00, 87.01
         );
  _cs.Add(   4027, 'Arctic - 87'#$00b0'N to 75'#$00b0'N, 84'#$00b0'W to 6'#$00b0'E',
           -84.00, 75.00, 6.01, 87.01
         );
  _cs.Add(   4028, 'Arctic - 87'#$00b0'N to 75'#$00b0'N, 12'#$00b0'W to 78'#$00b0'E',
           -12.00, 75.00, 78.01, 87.01
         );
  _cs.Add(   4029, 'Arctic - 87'#$00b0'N to 75'#$00b0'N, 60'#$00b0'E to 150'#$00b0'E',
           60.00, 75.00, 150.01, 87.01
         );
  _cs.Add(   4030, 'Arctic - 84'#$00b0'30''N to 79'#$00b0'30''N, 135'#$00b0'W to 95'#$00b0'W',
           -135.00, 79.50, -95.00, 84.51
         );
  _cs.Add(   4031, 'Arctic - 87'#$00b0'N to 75'#$00b0'N, 132'#$00b0'E to 138'#$00b0'W',
           132.00, 75.00, -138.00, 87.01
         );
  _cs.Add(   4032, 'Arctic - 79'#$00b0'N to 67'#$00b0'N, 156'#$00b0'W to 66'#$00b0'W',
           -156.00, 67.00, -66.00, 79.01
         );
  _cs.Add(   1364, 'Asia - Japan and Korea',
           122.83, 20.37, 154.05, 45.54
         );
  _cs.Add(   1483, 'Argentina - Mendoza and Neuquen 70.5'#$00b0'W to 67.5'#$00b0'W',
           -70.51, -43.41, -67.49, -31.91
         );
  _cs.Add(   1485, 'Argentina - Tierra del Fuego onshore west of 67.5'#$00b0'W',
           -68.64, -54.90, -67.50, -52.59
         );
  _cs.Add(   1486, 'Argentina - Tierra del Fuego offshore',
           -68.62, -54.93, -61.49, -51.36
         );
  _cs.Add(   1542, 'Asia - Cambodia and Vietnam - west of 108'#$00b0'E',
           102.14, 8.33, 108.00, 23.40
         );
  _cs.Add(   1567, 'Australasia - Australia and PNG - 138'#$00b0'E to 144'#$00b0'E',
           138.00, -46.63, 144.01, -2.53
         );
  _cs.Add(   1568, 'Australasia - Australia and PNG - 144'#$00b0'E to 150'#$00b0'E',
           144.00, -47.20, 150.01, -1.30
         );
  _cs.Add(   1570, 'Asia - Middle East - Kuwait and Saudi - 48'#$00b0'E to 54'#$00b0'E',
           47.99, 17.94, 54.01, 30.04
         );
  _cs.Add(   1571, 'Asia - Middle East - Kuwait and Saudi - 42'#$00b0'E to 48'#$00b0'E',
           42.00, 16.37, 48.01, 31.15
         );
  _cs.Add(   1608, 'Argentina - west of 70.5'#$00b0'W',
           -73.59, -52.00, -70.50, -36.16
         );
  _cs.Add(   1609, 'Argentina - 70.5'#$00b0'W to 67.5'#$00b0'W onshore',
           -70.50, -54.90, -67.49, -24.08
         );
  _cs.Add(   1614, 'Argentina - east of 55.5'#$00b0'W onshore',
           -55.50, -28.11, -53.65, -25.49
         );
  _cs.Add(   1623, 'Asia - Middle East - Lebanon and Syria onshore',
           35.04, 32.31, 42.38, 37.30
         );
  _cs.Add(   1665, 'Asia - Myanmar and Thailand - 96'#$00b0'E to 102'#$00b0'E',
           95.99, 5.63, 102.01, 28.55
         );
  _cs.Add(   1669, 'Asia - India; Pakistan - 28'#$00b0'N to 35'#$00b0'35''N',
           60.86, 28.00, 81.64, 35.59
         );
  _cs.Add(   1670, 'Asia - India; Pakistan - onshore 21'#$00b0'N to 28'#$00b0'N and west of 82'#$00b0'E',
           61.59, 21.00, 82.01, 28.01
         );
  _cs.Add(   1671, 'Asia - Bangladesh; India; Myanmar; Pakistan - zone Ilb',
           82.00, 21.00, 101.17, 29.47
         );
  _cs.Add(   1749, 'Asia - Middle East - Qatar offshore and UAE west of 54'#$00b0'E',
           50.55, 22.76, 54.01, 27.05
         );
  _cs.Add(   1798, 'Asia - FSU onshore 48'#$00b0'E to 54'#$00b0'E',
           48.00, 37.34, 54.00, 81.40
         );
  _cs.Add(   1799, 'Asia - FSU onshore 54'#$00b0'E to 60'#$00b0'E',
           54.00, 37.05, 60.00, 81.91
         );
  _cs.Add(   1800, 'Asia - FSU onshore 60'#$00b0'E to 66'#$00b0'E',
           60.00, 35.14, 66.00, 81.77
         );
  _cs.Add(   1801, 'Asia - FSU onshore 66'#$00b0'E to 72'#$00b0'E',
           66.00, 36.67, 72.00, 77.07
         );
  _cs.Add(   1802, 'Asia - FSU onshore 72'#$00b0'E to 78'#$00b0'E',
           72.00, 36.79, 78.00, 79.71
         );
  _cs.Add(   1803, 'Asia - FSU onshore 78'#$00b0'E to 84'#$00b0'E',
           78.00, 41.04, 84.01, 81.03
         );
  _cs.Add(   1804, 'Asia - FSU onshore 84'#$00b0'E to 90'#$00b0'E',
           84.00, 46.82, 90.00, 81.27
         );
  _cs.Add(   1852, 'Asia - Brunei and East Malaysia - 108'#$00b0'E to 114'#$00b0'E',
           109.31, 0.85, 114.00, 7.37
         );
  _cs.Add(   1853, 'Asia - Brunei and East Malaysia - 114'#$00b0'E to 120'#$00b0'E',
           114.00, 1.43, 119.61, 7.67
         );
  _cs.Add(   2292, 'Asia - Bangladesh; Myanmar - onshore 15'#$00b0'N to 21'#$00b0'N',
           92.10, 15.00, 100.65, 21.01
         );
  _cs.Add(   2294, 'Asia - Middle East - Iraq zone',
           38.79, 25.02, 63.34, 37.39
         );
  _cs.Add(   2314, 'Asia - FSU - Caspian states',
           26.00, 35.15, 90.00, 81.85
         )         .MarkDeprecated( True );
  _cs.Add(   2325, 'Argentina - Neuquen province Auca Mahuida area',
           -69.50, -38.75, -68.25, -37.50
         );
  _cs.Add(   2345, 'Asia - Middle East - Iraq; Israel; Jordan; Lebanon; Kuwait; Saudi Arabia; Syria',
           34.17, 16.37, 55.67, 37.39
         );
  _cs.Add(   2357, 'Argentina - Tierra del Fuego onshore',
           -68.64, -55.11, -63.73, -52.59
         );
  _cs.Add(   2409, 'Asia - Japan and South Korea',
           122.83, 20.37, 145.87, 45.54
         );
  _cs.Add(   2411, 'Asia - India mainland and Nepal',
           68.13, 8.02, 97.42, 35.51
         );
  _cs.Add(   2593, 'Asia - FSU - Azerbaijan and Georgia',
           39.99, 37.89, 51.73, 43.59
         );
  _cs.Add(   2596, 'Argentina - Tierra del Fuego offshore west of 66'#$00b0'W',
           -68.62, -54.61, -66.00, -51.65
         );
  _cs.Add(   2597, 'Argentina - Tierra del Fuego offshore east of 66'#$00b0'W',
           -66.00, -54.93, -61.49, -51.36
         );
  _cs.Add(   2603, 'Asia - Middle East - Israel and Palestine Territory onshore',
           34.17, 29.45, 35.69, 33.28
         );
  _cs.Add(   3533, 'Channel Islands - Guernsey',
           -2.75, 49.40, -2.13, 49.75
         )         .MarkDeprecated( True );
  _cs.Add(   3634, 'Caribbean - Puerto Rico and US Virgin Islands - onshore',
           -67.97, 17.62, -64.51, 18.57
         );
  _cs.Add(   3810, 'Caribbean - St Maarten, St Eustatius and Saba - onshore',
           -63.31, 17.41, -62.88, 18.07
         );
  _cs.Add(   3811, 'Chile - 72'#$00b0'W to 66'#$00b0'W',
           -72.00, -59.87, -66.00, -17.50
         );
  _cs.Add(   3825, 'Caribbean - French Antilles west of 60'#$00b0'W',
           -63.66, 14.08, -60.00, 18.31
         );
end;

procedure prepare_37;
begin
  _cs.Add(   3829, 'Chile - 78'#$00b0'W to 72'#$00b0'W',
           -78.00, -59.36, -71.99, -18.35
         );
  _cs.Add(   3876, 'Central America - Guatemala to Costa Rica',
           -92.29, 7.98, -82.53, 17.83
         );
  _cs.Add(   3944, 'China - 102'#$00b0'E to 108'#$00b0'E',
           102.00, 17.75, 108.00, 42.47
         );
  _cs.Add(   3945, 'China - 108'#$00b0'E to 114'#$00b0'E',
           108.00, 16.70, 114.00, 45.11
         );
  _cs.Add(   3946, 'China - 114'#$00b0'E to 120'#$00b0'E',
           114.00, 19.02, 120.00, 51.52
         );
  _cs.Add(   4121, 'Cayman Islands - Little Cayman',
           -80.14, 19.63, -79.93, 19.74
         );
  _cs.Add(   4221, 'Chile - onshore 36'#$00b0'S to 43.5'#$00b0'S',
           -74.48, -43.50, -70.39, -35.99
         );
  _cs.Add(   4222, 'Chile - onshore 26'#$00b0'S to 36'#$00b0'S',
           -72.87, -36.00, -68.28, -26.00
         );
  _cs.Add(   4224, 'Chile - onshore 32'#$00b0'S to 36'#$00b0'S',
           -72.87, -36.00, -69.77, -31.99
         );
  _cs.Add(   4231, 'Chile - onshore north of 26'#$00b0'S',
           -70.79, -26.00, -67.00, -17.50
         );
  _cs.Add(   4232, 'Chile - onshore north of 32'#$00b0'S',
           -71.77, -32.00, -67.00, -17.50
         );
  _cs.Add(   4536, 'Canada - Ontario - Toronto',
           -79.64, 43.58, -79.11, 43.86
         );
  _cs.Add(   4537, 'Canada - Ontario ex. Toronto',
           -95.16, 41.67, -74.35, 56.90
         );
  _cs.Add(   4612, 'Canada - Newfoundland',
           -59.48, 46.56, -52.54, 51.68
         );
  _cs.Add(   1068, 'Christmas Island',
           102.14, -13.92, 109.03, -8.87
         );
  _cs.Add(   3246, 'Estonia - onshore',
           21.74, 57.52, 28.20, 59.75
         );
  _cs.Add(   3576, 'Europe - 12'#$00b0'E to 18'#$00b0'E onshore and S-42(83) by country',
           12.00, 45.78, 18.01, 54.74
         );
  _cs.Add(   3577, 'Europe - 18'#$00b0'E to 24'#$00b0'E onshore and S-42(58) by country',
           18.00, 39.64, 24.00, 54.89
         );
  _cs.Add(   3578, 'Europe - 18'#$00b0'E to 24'#$00b0'E onshore and S-42(83) by country',
           18.00, 45.74, 22.90, 50.06
         );
  _cs.Add(   3579, 'Europe - 24'#$00b0'E to 30'#$00b0'E onshore and S-42(58) by country',
           24.00, 41.24, 29.74, 50.93
         );
  _cs.Add(   3580, 'Europe - 13.5'#$00b0'E to 16.5'#$00b0'E onshore and S-42(58) by country',
           13.50, 46.54, 16.50, 54.72
         );
  _cs.Add(   3581, 'Europe - 16.5'#$00b0'E to 19.5'#$00b0'E onshore and S-42(58) by country',
           16.50, 40.14, 19.50, 54.89
         );
  _cs.Add(   3582, 'Europe - 16.5'#$00b0'E to 19.5'#$00b0'E onshore and S-42(83) by country',
           16.50, 45.74, 19.50, 50.45
         );
  _cs.Add(   3583, 'Europe - 19.5'#$00b0'E to 22.5'#$00b0'E onshore and S-42(58) by country',
           19.50, 39.64, 22.50, 54.51
         );
  _cs.Add(   3584, 'Europe - 19.5'#$00b0'E to 22.5'#$00b0'E onshore and S-42(83) by country',
           19.50, 46.10, 22.50, 49.59
         );
  _cs.Add(   3585, 'Europe - 22.5'#$00b0'E to 25.5'#$00b0'E onshore and S-42(58) by country',
           22.50, 41.24, 25.50, 54.41
         );
  _cs.Add(   3586, 'Europe - 22.5'#$00b0'E to 25.5'#$00b0'E onshore and S-42(83) by country',
           22.50, 47.76, 22.90, 49.10
         );
  _cs.Add(   3587, 'Europe - 25.5'#$00b0'E to 28.5'#$00b0'E onshore and S-42(58) by country',
           25.50, 41.28, 28.50, 48.27
         );
  _cs.Add(   3588, 'Europe - 28.5'#$00b0'E to 31.5'#$00b0'E onshore and S-42(58) by country',
           28.50, 43.34, 29.74, 45.44
         );
  _cs.Add(   3594, 'Europe - EVRF2007',
           -9.56, 35.95, 31.59, 71.21
         );
  _cs.Add(   3889, 'Europe - Fehmarnbelt outer',
           10.66, 54.33, 12.01, 54.83
         );
  _cs.Add(   3890, 'Europe - Fehmarnbelt inner',
           11.17, 54.42, 11.51, 54.76
         );
  _cs.Add(   4205, 'enter here applicable extent',
           179.99, 89.99, 180.00, 90.00
         );
  _cs.Add(   4220, 'Equatorial Guinea - Bioko',
           8.37, 3.14, 9.02, 3.82
         );
  _cs.Add(   4606, 'Europe - British Isles - UK and Ireland onshore, UKCS',
           -9.00, 49.75, 2.01, 61.01
         );
end;

begin
  prepare_1;
  prepare_2;
  prepare_3;
  prepare_4;
  prepare_5;
  prepare_6;
  prepare_7;
  prepare_8;
  prepare_9;
  prepare_10;
  prepare_11;
  prepare_12;
  prepare_13;
  prepare_14;
  prepare_15;
  prepare_16;
  prepare_17;
  prepare_18;
  prepare_19;
  prepare_20;
  prepare_21;
  prepare_22;
  prepare_23;
  prepare_24;
  prepare_25;
  prepare_26;
  prepare_27;
  prepare_28;
  prepare_29;
  prepare_30;
  prepare_31;
  prepare_32;
  prepare_33;
  prepare_34;
  prepare_35;
  prepare_36;
  prepare_37;
end;

//==================================== END ====================================


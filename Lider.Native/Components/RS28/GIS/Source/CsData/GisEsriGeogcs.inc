//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  List of Geographic Coordinate Systems.
  Matching ESRI names and EPSG codes.
  Generated automatically.
}


procedure Init_ESRIGeogcsList(
  const _cs : T_esriNamesMap
) ;

procedure prepare_1;
begin
  _cs.Add     (    3819, 'GCS_HD1909'
              );
  _cs.Add     (    3821, 'GCS_TWD_1967'
              );
  _cs.Add     (    3824, 'GCS_TWD_1997'
              );
  _cs.Add     (    3889, 'GCS_IGRS'
              );
  _cs.Add     (    3906, 'GCS_MGI_1901'
              );
  _cs.Add     (    4001, 'GCS_Airy_1830'
              );
  _cs.Add     (    4002, 'GCS_Airy_Modified'
              );
  _cs.Add     (    4003, 'GCS_Australian'
              );
  _cs.Add     (    4004, 'GCS_Bessel_1841'
              );
  _cs.Add     (    4005, 'GCS_Bessel_Modified'
              );
  _cs.Add     (    4006, 'GCS_Bessel_Namibia'
              );
  _cs.Add     (    4007, 'GCS_Clarke_1858'
              );
  _cs.Add     (    4008, 'GCS_Clarke_1866'
              );
  _cs.Add     (    4009, 'GCS_Clarke_1866_Michigan'
              );
  _cs.Add     (    4010, 'GCS_Clarke_1880_Benoit'
              );
  _cs.Add     (    4011, 'GCS_Clarke_1880_IGN'
              );
  _cs.Add     (    4012, 'GCS_Clarke_1880_RGS'
              );
  _cs.Add     (    4013, 'GCS_Clarke_1880_Arc'
              );
  _cs.Add     (    4014, 'GCS_Clarke_1880_SGA'
              );
  _cs.Add     (    4015, 'GCS_Everest_Adj_1937'
              );
  _cs.Add     (    4016, 'GCS_Everest_def_1967'
              );
  _cs.Add     (    4018, 'GCS_Everest_Modified'
              );
  _cs.Add     (    4019, 'GCS_GRS_1980'
              );
  _cs.Add     (    4020, 'GCS_Helmert_1906'
              );
  _cs.Add     (    4021, 'GCS_Indonesian'
              );
  _cs.Add     (    4022, 'GCS_International_1924'
              );
  _cs.Add     (    4023, 'GCS_MOLDREF99'
              );
  _cs.Add     (    4024, 'GCS_Krasovsky_1940'
              );
  _cs.Add     (    4025, 'GCS_NWL_9D'
              );
  _cs.Add     (    4027, 'GCS_Plessis_1817'
              );
  _cs.Add     (    4028, 'GCS_Struve_1860'
              );
  _cs.Add     (    4029, 'GCS_War_Office'
              );
  _cs.Add     (    4031, 'GCS_GEM_10C'
              );
  _cs.Add     (    4032, 'GCS_OSU_86F'
              );
  _cs.Add     (    4033, 'GCS_OSU_91A'
              );
  _cs.Add     (    4034, 'GCS_Clarke_1880'
              );
  _cs.Add     (    4035, 'GCS_Sphere'
              );
  _cs.Add     (    4036, 'GCS_GRS_1967'
              );
  _cs.Add     (    4042, 'GCS_Everest_1830'
              );
  _cs.Add     (    4044, 'GCS_Everest_def_1962'
              );
  _cs.Add     (    4045, 'GCS_Everest_def_1975'
              );
  _cs.Add     (    4046, 'GCS_RGRDC_2005'
              );
  _cs.Add     (    4047, 'GCS_Sphere_GRS_1980_Authalic'
              );
  _cs.Add     (    4052, 'GCS_Sphere_Clarke_1866_Authalic'
              );
  _cs.Add     (    4053, 'GCS_Sphere_International_1924_Authalic'
              );
  _cs.Add     (    4054, 'GCS_Hughes_1980'
              );
  _cs.Add     (    4075, 'GCS_SREF98'
              );
  _cs.Add     (    4081, 'GCS_REGCAN95'
              );
  _cs.Add     (    4120, 'GCS_Greek'
              );
  _cs.Add     (    4121, 'GCS_GGRS_1987'
              );
  _cs.Add     (    4122, 'GCS_ATS_1977'
              );
  _cs.Add     (    4123, 'GCS_KKJ'
              );
  _cs.Add     (    4124, 'GCS_RT_1990'
              );
  _cs.Add     (    4125, 'GCS_Samboja'
              );
  _cs.Add     (    4126, 'GCS_LKS_1994'
              );
  _cs.Add     (    4127, 'GCS_Tete'
              );
  _cs.Add     (    4128, 'GCS_Madzansua'
              );
  _cs.Add     (    4129, 'GCS_Observatario'
              );
  _cs.Add     (    4130, 'GCS_Moznet'
              );
  _cs.Add     (    4131, 'GCS_Indian_1960'
              );
  _cs.Add     (    4132, 'GCS_FD_1958'
              );
  _cs.Add     (    4133, 'GCS_Estonia_1992'
              );
  _cs.Add     (    4134, 'GCS_PDO_1993'
              );
  _cs.Add     (    4135, 'GCS_Old_Hawaiian'
              );
  _cs.Add     (    4136, 'GCS_St_Lawrence_Island'
              );
  _cs.Add     (    4137, 'GCS_St_Paul_Island'
              );
  _cs.Add     (    4138, 'GCS_St_George_Island'
              );
  _cs.Add     (    4139, 'GCS_Puerto_Rico'
              );
  _cs.Add     (    4140, 'GCS_North_American_1983_CSRS'
              );
  _cs.Add     (    4141, 'GCS_Israel'
              );
  _cs.Add     (    4142, 'GCS_Locodjo_1965'
              );
  _cs.Add     (    4143, 'GCS_Abidjan_1987'
              );
  _cs.Add     (    4144, 'GCS_Kalianpur_1937'
              );
  _cs.Add     (    4145, 'GCS_Kalianpur_1962'
              );
  _cs.Add     (    4146, 'GCS_Kalianpur_1975'
              );
  _cs.Add     (    4147, 'GCS_Hanoi_1972'
              );
  _cs.Add     (    4148, 'GCS_Hartebeesthoek_1994'
              );
  _cs.Add     (    4149, 'GCS_CH1903'
              );
  _cs.Add     (    4150, 'GCS_CH1903+'
              );
  _cs.Add     (    4151, 'GCS_Swiss_TRF_1995'
              );
  _cs.Add     (    4152, 'GCS_North_American_1983_HARN'
              );
  _cs.Add     (    4153, 'GCS_Rassadiran'
              );
  _cs.Add     (    4154, 'GCS_European_1950_ED77'
              );
  _cs.Add     (    4155, 'GCS_Dabola_1981'
              );
  _cs.Add     (    4156, 'GCS_S_JTSK'
              );
  _cs.Add     (    4157, 'GCS_Mount_Dillon'
              );
  _cs.Add     (    4158, 'GCS_Naparima_1955'
              );
  _cs.Add     (    4159, 'GCS_European_Libyan_Datum_1979'
              );
  _cs.Add     (    4160, 'GCS_Chos_Malal_1914'
              );
  _cs.Add     (    4161, 'GCS_Pampa_del_Castillo'
              );
  _cs.Add     (    4162, 'GCS_Korean_Datum_1985'
              );
  _cs.Add     (    4163, 'GCS_Yemen_NGN_1996'
              );
  _cs.Add     (    4164, 'GCS_South_Yemen'
              );
  _cs.Add     (    4165, 'GCS_Bissau'
              );
  _cs.Add     (    4166, 'GCS_Korean_Datum_1995'
              );
  _cs.Add     (    4167, 'GCS_NZGD_2000'
              );
  _cs.Add     (    4168, 'GCS_Accra'
              );
  _cs.Add     (    4169, 'GCS_American_Samoa_1962'
              );
  _cs.Add     (    4170, 'GCS_SIRGAS'
              );
  _cs.Add     (    4171, 'GCS_RGF_1993'
              );
end;

procedure prepare_2;
begin
  _cs.Add     (    4172, 'GCS_POSGAR'
              );
  _cs.Add     (    4173, 'GCS_IRENET95'
              );
  _cs.Add     (    4174, 'GCS_Sierra_Leone_1924'
              );
  _cs.Add     (    4175, 'GCS_Sierra_Leone_1968'
              );
  _cs.Add     (    4176, 'GCS_Australian_Antarctic_1998'
              );
  _cs.Add     (    4178, 'GCS_Pulkovo_1942_Adj_1983'
              );
  _cs.Add     (    4179, 'GCS_Pulkovo_1942_Adj_1958'
              );
  _cs.Add     (    4180, 'GCS_Estonia_1997'
              );
  _cs.Add     (    4181, 'GCS_Luxembourg_1930'
              );
  _cs.Add     (    4182, 'GCS_Azores_Occidental_1939'
              );
  _cs.Add     (    4183, 'GCS_Azores_Central_1948'
              );
  _cs.Add     (    4184, 'GCS_Azores_Oriental_1940'
              );
  _cs.Add     (    4185, 'GCS_Madeira_1936'
              );
  _cs.Add     (    4188, 'GCS_OSNI_1952'
              );
  _cs.Add     (    4189, 'GCS_REGVEN'
              );
  _cs.Add     (    4190, 'GCS_POSGAR_1998'
              );
  _cs.Add     (    4191, 'GCS_Albanian_1987'
              );
  _cs.Add     (    4192, 'GCS_Douala_1948'
              );
  _cs.Add     (    4193, 'GCS_Manoca_1962'
              );
  _cs.Add     (    4194, 'GCS_Qornoq_1927'
              );
  _cs.Add     (    4195, 'GCS_Scoresbysund_1952'
              );
  _cs.Add     (    4196, 'GCS_Ammassalik_1958'
              );
  _cs.Add     (    4198, 'GCS_Kousseri'
              );
  _cs.Add     (    4199, 'GCS_Egypt_1930'
              );
  _cs.Add     (    4200, 'GCS_Pulkovo_1995'
              );
  _cs.Add     (    4201, 'GCS_Adindan'
              );
  _cs.Add     (    4202, 'GCS_Australian_1966'
              );
  _cs.Add     (    4203, 'GCS_Australian_1984'
              );
  _cs.Add     (    4204, 'GCS_Ain_el_Abd_1970'
              );
  _cs.Add     (    4205, 'GCS_Afgooye'
              );
  _cs.Add     (    4206, 'GCS_Agadez'
              );
  _cs.Add     (    4207, 'GCS_Lisbon'
              );
  _cs.Add     (    4208, 'GCS_Aratu'
              );
  _cs.Add     (    4209, 'GCS_Arc_1950'
              );
  _cs.Add     (    4210, 'GCS_Arc_1960'
              );
  _cs.Add     (    4211, 'GCS_Batavia'
              );
  _cs.Add     (    4212, 'GCS_Barbados_1938'
              );
  _cs.Add     (    4213, 'GCS_Beduaram'
              );
  _cs.Add     (    4214, 'GCS_Beijing_1954'
              );
  _cs.Add     (    4215, 'GCS_Belge_1950'
              );
  _cs.Add     (    4216, 'GCS_Bermuda_1957'
              );
  _cs.Add     (    4218, 'GCS_Bogota'
              );
  _cs.Add     (    4219, 'GCS_Bukit_Rimpah'
              );
  _cs.Add     (    4220, 'GCS_Camacupa'
              );
  _cs.Add     (    4221, 'GCS_Campo_Inchauspe'
              );
  _cs.Add     (    4222, 'GCS_Cape'
              );
  _cs.Add     (    4223, 'GCS_Carthage'
              );
  _cs.Add     (    4224, 'GCS_Chua'
              );
  _cs.Add     (    4225, 'GCS_Corrego_Alegre'
              );
  _cs.Add     (    4226, 'GCS_Cote_d_Ivoire'
              );
  _cs.Add     (    4227, 'GCS_Deir_ez_Zor'
              );
  _cs.Add     (    4228, 'GCS_Douala'
              );
  _cs.Add     (    4229, 'GCS_Egypt_1907'
              );
  _cs.Add     (    4230, 'GCS_European_1950'
              );
  _cs.Add     (    4231, 'GCS_European_1987'
              );
  _cs.Add     (    4232, 'GCS_Fahud'
              );
  _cs.Add     (    4233, 'GCS_Gandajika_1970'
              );
  _cs.Add     (    4234, 'GCS_Garoua'
              );
  _cs.Add     (    4235, 'GCS_Guyane_Francaise'
              );
  _cs.Add     (    4236, 'GCS_Hu_Tzu_Shan'
              );
  _cs.Add     (    4237, 'GCS_Hungarian_1972'
              );
  _cs.Add     (    4238, 'GCS_Indonesian_1974'
              );
  _cs.Add     (    4239, 'GCS_Indian_1954'
              );
  _cs.Add     (    4240, 'GCS_Indian_1975'
              );
  _cs.Add     (    4241, 'GCS_Jamaica_1875'
              );
  _cs.Add     (    4242, 'GCS_Jamaica_1969'
              );
  _cs.Add     (    4243, 'GCS_Kalianpur_1880'
              );
  _cs.Add     (    4244, 'GCS_Kandawala'
              );
  _cs.Add     (    4245, 'GCS_Kertau'
              );
  _cs.Add     (    4246, 'GCS_Kuwait_Oil_Company'
              );
  _cs.Add     (    4247, 'GCS_La_Canoa'
              );
  _cs.Add     (    4248, 'GCS_Provisional_S_American_1956'
              );
  _cs.Add     (    4249, 'GCS_Lake'
              );
  _cs.Add     (    4250, 'GCS_Leigon'
              );
  _cs.Add     (    4251, 'GCS_Liberia_1964'
              );
  _cs.Add     (    4252, 'GCS_Lome'
              );
  _cs.Add     (    4253, 'GCS_Luzon_1911'
              );
  _cs.Add     (    4254, 'GCS_Hito_XVIII_1963'
              );
  _cs.Add     (    4255, 'GCS_Herat_North'
              );
  _cs.Add     (    4256, 'GCS_Mahe_1971'
              );
  _cs.Add     (    4257, 'GCS_Makassar'
              );
  _cs.Add     (    4258, 'GCS_ETRS_1989'
              );
  _cs.Add     (    4259, 'GCS_Malongo_1987'
              );
  _cs.Add     (    4260, 'GCS_Manoca'
              );
  _cs.Add     (    4261, 'GCS_Merchich'
              );
  _cs.Add     (    4262, 'GCS_Massawa'
              );
  _cs.Add     (    4263, 'GCS_Minna'
              );
  _cs.Add     (    4264, 'GCS_Mhast'
              );
  _cs.Add     (    4265, 'GCS_Monte_Mario'
              );
  _cs.Add     (    4266, 'GCS_Mporaloko'
              );
  _cs.Add     (    4267, 'GCS_North_American_1927'
              );
  _cs.Add     (    4268, 'GCS_North_American_Michigan'
              );
  _cs.Add     (    4269, 'GCS_North_American_1983'
              );
  _cs.Add     (    4270, 'GCS_Nahrwan_1967'
              );
  _cs.Add     (    4271, 'GCS_Naparima_1972'
              );
  _cs.Add     (    4272, 'GCS_New_Zealand_1949'
              );
  _cs.Add     (    4273, 'GCS_NGO_1948'
              );
  _cs.Add     (    4274, 'GCS_Datum_73'
              );
  _cs.Add     (    4275, 'GCS_NTF'
              );
  _cs.Add     (    4276, 'GCS_NSWC_9Z_2'
              );
end;

procedure prepare_3;
begin
  _cs.Add     (    4277, 'GCS_OSGB_1936'
              );
  _cs.Add     (    4278, 'GCS_OSGB_1970_SN'
              );
  _cs.Add     (    4279, 'GCS_OS_SN_1980'
              );
  _cs.Add     (    4280, 'GCS_Padang_1884'
              );
  _cs.Add     (    4281, 'GCS_Palestine_1923'
              );
  _cs.Add     (    4282, 'GCS_Pointe_Noire'
              );
  _cs.Add     (    4283, 'GCS_GDA_1994'
              );
  _cs.Add     (    4284, 'GCS_Pulkovo_1942'
              );
  _cs.Add     (    4285, 'GCS_Qatar_1974'
              );
  _cs.Add     (    4286, 'GCS_Qatar_1948'
              );
  _cs.Add     (    4287, 'GCS_Qornoq'
              );
  _cs.Add     (    4288, 'GCS_Loma_Quintana'
              );
  _cs.Add     (    4289, 'GCS_Amersfoort'
              );
  _cs.Add     (    4291, 'GCS_South_American_1969'
              );
  _cs.Add     (    4292, 'GCS_Sapper_Hill_1943'
              );
  _cs.Add     (    4293, 'GCS_Schwarzeck'
              );
  _cs.Add     (    4294, 'GCS_Segora'
              );
  _cs.Add     (    4295, 'GCS_Serindung'
              );
  _cs.Add     (    4296, 'GCS_Sudan'
              );
  _cs.Add     (    4297, 'GCS_Tananarive_1925'
              );
  _cs.Add     (    4298, 'GCS_Timbalai_1948'
              );
  _cs.Add     (    4299, 'GCS_TM65'
              );
  _cs.Add     (    4300, 'GCS_TM75'
              );
  _cs.Add     (    4301, 'GCS_Tokyo'
              );
  _cs.Add     (    4302, 'GCS_Trinidad_1903'
              );
  _cs.Add     (    4303, 'GCS_Trucial_Coast_1948'
              );
  _cs.Add     (    4304, 'GCS_Voirol_1875'
              );
  _cs.Add     (    4306, 'GCS_Bern_1938'
              );
  _cs.Add     (    4307, 'GCS_Nord_Sahara_1959'
              );
  _cs.Add     (    4308, 'GCS_RT38'
              );
  _cs.Add     (    4309, 'GCS_Yacare'
              );
  _cs.Add     (    4310, 'GCS_Yoff'
              );
  _cs.Add     (    4311, 'GCS_Zanderij'
              );
  _cs.Add     (    4312, 'GCS_MGI'
              );
  _cs.Add     (    4313, 'GCS_Belge_1972'
              );
  _cs.Add     (    4314, 'GCS_Deutsches_Hauptdreiecksnetz'
              );
  _cs.Add     (    4315, 'GCS_Conakry_1905'
              );
  _cs.Add     (    4316, 'GCS_Dealul_Piscului_1933'
              );
  _cs.Add     (    4317, 'GCS_Dealul_Piscului_1970'
              );
  _cs.Add     (    4318, 'GCS_NGN'
              );
  _cs.Add     (    4319, 'GCS_KUDAMS'
              );
  _cs.Add     (    4322, 'GCS_WGS_1972'
              );
  _cs.Add     (    4324, 'GCS_WGS_1972_BE'
              );
  _cs.Add     (    4326, 'GCS_WGS_1984'
              );
  _cs.Add     (    4463, 'GCS_RGSPM_2006'
              );
  _cs.Add     (    4470, 'GCS_RGM_2004'
              );
  _cs.Add     (    4475, 'GCS_Cadastre_1997'
              );
  _cs.Add     (    4483, 'GCS_Mexican_Datum_of_1993'
              );
  _cs.Add     (    4490, 'GCS_China_Geodetic_Coordinate_System_2000'
              );
  _cs.Add     (    4555, 'GCS_New_Beijing'
              );
  _cs.Add     (    4558, 'GCS_RRAF_1991'
              );
  _cs.Add     (    4600, 'GCS_Anguilla_1957'
              );
  _cs.Add     (    4601, 'GCS_Antigua_1943'
              );
  _cs.Add     (    4602, 'GCS_Dominica_1945'
              );
  _cs.Add     (    4603, 'GCS_Grenada_1953'
              );
  _cs.Add     (    4604, 'GCS_Montserrat_1958'
              );
  _cs.Add     (    4605, 'GCS_St_Kitts_1955'
              );
  _cs.Add     (    4606, 'GCS_St_Lucia_1955'
              );
  _cs.Add     (    4607, 'GCS_St_Vincent_1945'
              );
  _cs.Add     (    4608, 'GCS_NAD_1927_Definition_1976'
              );
  _cs.Add     (    4609, 'GCS_NAD_1927_CGQ77'
              );
  _cs.Add     (    4610, 'GCS_Xian_1980'
              );
  _cs.Add     (    4611, 'GCS_Hong_Kong_1980'
              );
  _cs.Add     (    4612, 'GCS_JGD_2000'
              );
  _cs.Add     (    4613, 'GCS_Gunung_Segara'
              );
  _cs.Add     (    4614, 'GCS_QND_1995'
              );
  _cs.Add     (    4615, 'GCS_Porto_Santo_1936'
              );
  _cs.Add     (    4616, 'GCS_Selvagem_Grande_1938'
              );
  _cs.Add     (    4617, 'GCS_North_American_1983_CSRS'
              );
  _cs.Add     (    4618, 'GCS_South_American_1969'
              );
  _cs.Add     (    4619, 'GCS_SWEREF99'
              );
  _cs.Add     (    4620, 'GCS_Point_58'
              );
  _cs.Add     (    4621, 'GCS_Fort_Marigot'
              );
  _cs.Add     (    4622, 'GCS_Sainte_Anne'
              );
  _cs.Add     (    4623, 'GCS_CSG_1967'
              );
  _cs.Add     (    4624, 'GCS_RGFG_1995'
              );
  _cs.Add     (    4625, 'GCS_Fort_Desaix'
              );
  _cs.Add     (    4626, 'GCS_Reunion_1947'
              );
  _cs.Add     (    4627, 'GCS_RGR_1992'
              );
  _cs.Add     (    4628, 'GCS_Tahiti_1952'
              );
  _cs.Add     (    4629, 'GCS_Tahaa_1954'
              );
  _cs.Add     (    4630, 'GCS_IGN72_Nuku_Hiva'
              );
  _cs.Add     (    4631, 'GCS_Kerguelen_Island_1949'
              );
  _cs.Add     (    4632, 'GCS_Combani_1950'
              );
  _cs.Add     (    4633, 'GCS_IGN56_Lifou'
              );
  _cs.Add     (    4636, 'GCS_Petrels_1972'
              );
  _cs.Add     (    4637, 'GCS_Pointe_Geologie_Perroud_1950'
              );
  _cs.Add     (    4638, 'GCS_Saint_Pierre_et_Miquelon_1950'
              );
  _cs.Add     (    4639, 'GCS_MOP78'
              );
  _cs.Add     (    4641, 'GCS_IGN53_Mare'
              );
  _cs.Add     (    4642, 'GCS_ST84_Ile_des_Pins'
              );
  _cs.Add     (    4643, 'GCS_ST71_Belep'
              );
  _cs.Add     (    4644, 'GCS_NEA74_Noumea'
              );
  _cs.Add     (    4645, 'GCS_RGNC_1991'
              );
  _cs.Add     (    4646, 'GCS_Grand_Comoros'
              );
  _cs.Add     (    4657, 'GCS_Reykjavik_1900'
              );
  _cs.Add     (    4658, 'GCS_Hjorsey_1955'
              );
  _cs.Add     (    4659, 'GCS_ISN_1993'
              );
  _cs.Add     (    4660, 'GCS_Helle_1954'
              );
  _cs.Add     (    4661, 'GCS_LKS_1992'
              );
end;

procedure prepare_4;
begin
  _cs.Add     (    4662, 'GCS_IGN72_Grande_Terre'
              );
  _cs.Add     (    4663, 'GCS_Porto_Santo_1995'
              );
  _cs.Add     (    4664, 'GCS_Azores_Oriental_1995'
              );
  _cs.Add     (    4665, 'GCS_Azores_Central_1995'
              );
  _cs.Add     (    4666, 'GCS_Lisbon_1890'
              );
  _cs.Add     (    4667, 'GCS_IKBD_1992'
              );
  _cs.Add     (    4668, 'GCS_European_1979'
              );
  _cs.Add     (    4669, 'GCS_LKS_1994'
              );
  _cs.Add     (    4670, 'GCS_IGM_1995'
              );
  _cs.Add     (    4671, 'GCS_Voirol_1879'
              );
  _cs.Add     (    4672, 'GCS_Chatham_Island_1971'
              );
  _cs.Add     (    4673, 'GCS_Chatham_Islands_1979'
              );
  _cs.Add     (    4674, 'GCS_SIRGAS_2000'
              );
  _cs.Add     (    4675, 'GCS_Guam_1963'
              );
  _cs.Add     (    4676, 'GCS_Vientiane_1982'
              );
  _cs.Add     (    4677, 'GCS_Lao_1993'
              );
  _cs.Add     (    4678, 'GCS_Lao_1997'
              );
  _cs.Add     (    4679, 'GCS_Jouik_1961'
              );
  _cs.Add     (    4680, 'GCS_Nouakchott_1965'
              );
  _cs.Add     (    4682, 'GCS_Gulshan_303'
              );
  _cs.Add     (    4683, 'GCS_PRS_1992'
              );
  _cs.Add     (    4684, 'GCS_Gan_1970'
              );
  _cs.Add     (    4686, 'GCS_MAGNA'
              );
  _cs.Add     (    4687, 'GCS_RGPF'
              );
  _cs.Add     (    4688, 'GCS_Fatu_Iva_1972'
              );
  _cs.Add     (    4689, 'GCS_IGN63_Hiva_Oa'
              );
  _cs.Add     (    4690, 'GCS_Tahiti_1979'
              );
  _cs.Add     (    4691, 'GCS_Moorea_1987'
              );
  _cs.Add     (    4692, 'GCS_Maupiti_1983'
              );
  _cs.Add     (    4693, 'GCS_Nakhl_e_Ghanem'
              );
  _cs.Add     (    4694, 'GCS_POSGAR_1994'
              );
  _cs.Add     (    4695, 'GCS_Katanga_1955'
              );
  _cs.Add     (    4696, 'GCS_Kasai_1953'
              );
  _cs.Add     (    4697, 'GCS_IGC_1962_6th_Parallel_South'
              );
  _cs.Add     (    4698, 'GCS_Kerguelen_Island_1949'
              );
  _cs.Add     (    4699, 'GCS_Le_Pouce_1934'
              );
  _cs.Add     (    4700, 'GCS_IGN_Astro_1960'
              );
  _cs.Add     (    4701, 'GCS_IGCB_1955'
              );
  _cs.Add     (    4702, 'GCS_Mauritania_1999'
              );
  _cs.Add     (    4703, 'GCS_Mhast_1951'
              );
  _cs.Add     (    4704, 'GCS_Mhast_Onshore'
              );
  _cs.Add     (    4705, 'GCS_Mhast_Offshore'
              );
  _cs.Add     (    4706, 'GCS_Egypt_Gulf_of_Suez_S_650_TL'
              );
  _cs.Add     (    4707, 'GCS_Tern_Island_1961'
              );
  _cs.Add     (    4708, 'GCS_Anna_1_1965'
              );
  _cs.Add     (    4709, 'GCS_Beacon_E_1945'
              );
  _cs.Add     (    4710, 'GCS_DOS_71_4'
              );
  _cs.Add     (    4711, 'GCS_Astro_1952'
              );
  _cs.Add     (    4712, 'GCS_Ascension_Island_1958'
              );
  _cs.Add     (    4713, 'GCS_Ayabelle'
              );
  _cs.Add     (    4714, 'GCS_Bellevue_IGN'
              );
  _cs.Add     (    4715, 'GCS_Camp_Area'
              );
  _cs.Add     (    4716, 'GCS_Canton_1966'
              );
  _cs.Add     (    4717, 'GCS_Cape_Canaveral'
              );
  _cs.Add     (    4718, 'GCS_Solomon_1968'
              );
  _cs.Add     (    4719, 'GCS_Easter_Island_1967'
              );
  _cs.Add     (    4720, 'GCS_Fiji_1986'
              );
  _cs.Add     (    4721, 'GCS_Fiji_1956'
              );
  _cs.Add     (    4722, 'GCS_ISTS_061_1968'
              );
  _cs.Add     (    4723, 'GCS_Grand_Cayman_1959'
              );
  _cs.Add     (    4724, 'GCS_ISTS_073_1969'
              );
  _cs.Add     (    4725, 'GCS_Johnston_Island_1961'
              );
  _cs.Add     (    4726, 'GCS_Little_Cayman_1961'
              );
  _cs.Add     (    4727, 'GCS_Midway_1961'
              );
  _cs.Add     (    4728, 'GCS_Pico_de_Las_Nieves'
              );
  _cs.Add     (    4729, 'GCS_Pitcairn_1967'
              );
  _cs.Add     (    4730, 'GCS_Santo_DOS_1965'
              );
  _cs.Add     (    4731, 'GCS_Viti_Levu_1916'
              );
  _cs.Add     (    4732, 'GCS_Wake_Eniwetok_1960'
              );
  _cs.Add     (    4733, 'GCS_Wake_Island_1952'
              );
  _cs.Add     (    4734, 'GCS_Tristan_1968'
              );
  _cs.Add     (    4735, 'GCS_Kusaie_1951'
              );
  _cs.Add     (    4736, 'GCS_Deception_Island'
              );
  _cs.Add     (    4737, 'GCS_Korea_2000'
              );
  _cs.Add     (    4738, 'GCS_Hong_Kong_1963'
              );
  _cs.Add     (    4739, 'GCS_Hong_Kong_1963_67'
              );
  _cs.Add     (    4740, 'GCS_PZ_1990'
              );
  _cs.Add     (    4741, 'GCS_FD_1954'
              );
  _cs.Add     (    4742, 'GCS_GDM_2000'
              );
  _cs.Add     (    4743, 'GCS_Karbala_1979_Polservice'
              );
  _cs.Add     (    4744, 'GCS_Nahrwan_1934'
              );
  _cs.Add     (    4745, 'GCS_RD_83'
              );
  _cs.Add     (    4746, 'GCS_PD_83'
              );
  _cs.Add     (    4747, 'GCS_Greenland_1996'
              );
  _cs.Add     (    4748, 'GCS_Vanua_Levu_1915'
              );
  _cs.Add     (    4749, 'GCS_RGNC_1991_93'
              );
  _cs.Add     (    4750, 'GCS_ST87_Ouvea'
              );
  _cs.Add     (    4751, 'GCS_Kertau_RSO'
              );
  _cs.Add     (    4752, 'GCS_Viti_Levu_1912'
              );
  _cs.Add     (    4753, 'GCS_fk89'
              );
  _cs.Add     (    4754, 'GCS_LGD2006'
              );
  _cs.Add     (    4755, 'GCS_DGN_1995'
              );
  _cs.Add     (    4756, 'GCS_VN_2000'
              );
  _cs.Add     (    4757, 'GCS_SVY21'
              );
  _cs.Add     (    4758, 'GCS_JAD_2001'
              );
  _cs.Add     (    4759, 'GCS_NAD_1983_NSRS2007'
              );
  _cs.Add     (    4760, 'GCS_WGS_1966'
              );
  _cs.Add     (    4761, 'GCS_HTRS96'
              );
  _cs.Add     (    4762, 'GCS_Bermuda_2000'
              );
  _cs.Add     (    4763, 'GCS_Pitcairn_2006'
              );
end;

procedure prepare_5;
begin
  _cs.Add     (    4764, 'GCS_RSRGD2000'
              );
  _cs.Add     (    4765, 'GCS_Slovenia_1996'
              );
  _cs.Add     (    4801, 'GCS_Bern_1898_Bern'
              );
  _cs.Add     (    4802, 'GCS_Bogota_Bogota'
              );
  _cs.Add     (    4803, 'GCS_Lisbon_Lisbon'
              );
  _cs.Add     (    4804, 'GCS_Makassar_Jakarta'
              );
  _cs.Add     (    4805, 'GCS_MGI_Ferro'
              );
  _cs.Add     (    4806, 'GCS_Monte_Mario_Rome'
              );
  _cs.Add     (    4807, 'GCS_NTF_Paris'
              );
  _cs.Add     (    4808, 'GCS_Padang_1884_Jakarta'
              );
  _cs.Add     (    4809, 'GCS_Belge_1950_Brussels'
              );
  _cs.Add     (    4810, 'GCS_Tananarive_1925_Paris'
              );
  _cs.Add     (    4811, 'GCS_Voirol_1875_Paris'
              );
  _cs.Add     (    4813, 'GCS_Batavia_Jakarta'
              );
  _cs.Add     (    4814, 'GCS_RT38_Stockholm'
              );
  _cs.Add     (    4815, 'GCS_Greek_Athens'
              );
  _cs.Add     (    4816, 'GCS_Carthage_Paris'
              );
  _cs.Add     (    4817, 'GCS_NGO_1948_Oslo'
              );
  _cs.Add     (    4818, 'GCS_S_JTSK_Ferro'
              );
  _cs.Add     (    4819, 'GCS_Nord_Sahara_1959_Paris'
              );
  _cs.Add     (    4820, 'GCS_Gunung_Segara_Jakarta'
              );
  _cs.Add     (    4821, 'GCS_Voirol_1879_Paris'
              );
  _cs.Add     (    4823, 'GCS_Sao_Tome'
              );
  _cs.Add     (    4824, 'GCS_Principe'
              );
  _cs.Add     (    4901, 'GCS_ATF_Paris'
              );
  _cs.Add     (    4902, 'GCS_Nord_de_Guerre_Paris'
              );
  _cs.Add     (    4903, 'GCS_Madrid_1870_Madrid'
              );
  _cs.Add     (    4904, 'GCS_Lisbon_1890_Lisbon'
              );
  _cs.Add     (    5013, 'GCS_PTRA08'
              );
  _cs.Add     (    5228, 'GCS_S_JTSK_05'
              );
  _cs.Add     (    5229, 'GCS_S_JTSK_05_Ferro'
              );
  _cs.Add     (    5233, 'GCS_SLD99'
              );
  _cs.Add     (    5246, 'GCS_GDBD2009'
              );
  _cs.Add     (    5252, 'GCS_TUREF'
              );
  _cs.Add     (    5264, 'GCS_DRUKREF_03'
              );
  _cs.Add     (    5324, 'GCS_ISN_2004'
              );
  _cs.Add     (    5340, 'GCS_POSGAR_2007'
              );
  _cs.Add     (    5354, 'GCS_MARGEN'
              );
  _cs.Add     (    5360, 'GCS_SIRGAS_Chile'
              );
  _cs.Add     (    5365, 'GCS_CR05'
              );
  _cs.Add     (    5371, 'GCS_MARCARIO_SOLIS'
              );
  _cs.Add     (    5373, 'GCS_Peru96'
              );
  _cs.Add     (    5381, 'GCS_SIRGAS_ROU98'
              );
  _cs.Add     (    5393, 'GCS_SIRGAS_ES2007_8'
              );
  _cs.Add     (    5451, 'GCS_Ocotepeque_1935'
              );
  _cs.Add     (    5464, 'GCS_Sibun_Gorge_1922'
              );
  _cs.Add     (    5467, 'GCS_Panama_Colon_1911'
              );
  _cs.Add     (    5489, 'GCS_RGAF09'
              );
  _cs.Add     (    5524, 'GCS_Corrego_Alegre_1961'
              );
  _cs.Add     (    5527, 'GCS_SAD_1969_96'
              );
  _cs.Add     (    5546, 'GCS_PNG94'
              );
  _cs.Add     (    5561, 'GCS_Ukraine_2000'
              );
  _cs.Add     (   37001, 'GCS_WGS_1966'
              );
  _cs.Add     (   37002, 'GCS_Fischer_1960'
              );
  _cs.Add     (   37003, 'GCS_Fischer_1968'
              );
  _cs.Add     (   37004, 'GCS_Fischer_Modified'
              );
  _cs.Add     (   37005, 'GCS_Hough_1960'
              );
  _cs.Add     (   37006, 'GCS_Everest_Modified_1969'
              );
  _cs.Add     (   37007, 'GCS_Walbeck'
              );
  _cs.Add     (   37201, 'GCS_European_1979'
              );
  _cs.Add     (   37202, 'GCS_Everest_Bangladesh'
              );
  _cs.Add     (   37203, 'GCS_Everest_India_Nepal'
              );
  _cs.Add     (   37204, 'GCS_Hjorsey_1955'
              );
  _cs.Add     (   37205, 'GCS_Hong_Kong_1963_67'
              );
  _cs.Add     (   37206, 'GCS_Oman'
              );
  _cs.Add     (   37207, 'GCS_South_Asia_Singapore'
              );
  _cs.Add     (   37208, 'GCS_Ayabelle'
              );
  _cs.Add     (   37211, 'GCS_Point_58'
              );
  _cs.Add     (   37212, 'GCS_Beacon_E_1945'
              );
  _cs.Add     (   37213, 'GCS_Tern_Island_1961'
              );
  _cs.Add     (   37214, 'GCS_Astro_1952'
              );
  _cs.Add     (   37215, 'GCS_Bellevue_IGN'
              );
  _cs.Add     (   37216, 'GCS_Canton_1966'
              );
  _cs.Add     (   37217, 'GCS_Chatham_Island_1971'
              );
  _cs.Add     (   37218, 'GCS_DOS_1968'
              );
  _cs.Add     (   37219, 'GCS_Easter_Island_1967'
              );
  _cs.Add     (   37220, 'GCS_Guam_1963'
              );
  _cs.Add     (   37221, 'GCS_GUX_1'
              );
  _cs.Add     (   37222, 'GCS_Johnston_Island_1961'
              );
  _cs.Add     (   37223, 'GCS_Carthage'
              );
  _cs.Add     (   37224, 'GCS_Midway_1961'
              );
  _cs.Add     (   37226, 'GCS_Pitcairn_1967'
              );
  _cs.Add     (   37227, 'GCS_Santo_DOS_1965'
              );
  _cs.Add     (   37228, 'GCS_Viti_Levu_1916'
              );
  _cs.Add     (   37229, 'GCS_Wake_Eniwetok_1960'
              );
  _cs.Add     (   37230, 'GCS_Wake_Island_1952'
              );
  _cs.Add     (   37231, 'GCS_Anna_1_1965'
              );
  _cs.Add     (   37232, 'GCS_Gan_1970'
              );
  _cs.Add     (   37233, 'GCS_ISTS_073_1969'
              );
  _cs.Add     (   37234, 'GCS_Kerguelen_Island_1949'
              );
  _cs.Add     (   37235, 'GCS_Reunion_1947'
              );
  _cs.Add     (   37237, 'GCS_Ascension_Island_1958'
              );
  _cs.Add     (   37238, 'GCS_DOS_71_4'
              );
  _cs.Add     (   37239, 'GCS_Cape_Canaveral'
              );
  _cs.Add     (   37240, 'GCS_Fort_Thomas_1955'
              );
  _cs.Add     (   37241, 'GCS_Graciosa_Base_SW_1948'
              );
  _cs.Add     (   37242, 'GCS_ISTS_061_1968'
              );
  _cs.Add     (   37243, 'GCS_LC5_1961'
              );
  _cs.Add     (   37245, 'GCS_Observatorio_Meteorologico_1939'
              );
  _cs.Add     (   37246, 'GCS_Pico_de_Las_Nieves'
              );
end;

procedure prepare_6;
begin
  _cs.Add     (   37247, 'GCS_Porto_Santo_1936'
              );
  _cs.Add     (   37249, 'GCS_Sao_Braz'
              );
  _cs.Add     (   37250, 'GCS_Selvagem_Grande_1938'
              );
  _cs.Add     (   37251, 'GCS_Tristan_1968'
              );
  _cs.Add     (   37252, 'GCS_American_Samoa_1962'
              );
  _cs.Add     (   37253, 'GCS_Camp_Area'
              );
  _cs.Add     (   37254, 'GCS_Deception_Island'
              );
  _cs.Add     (   37255, 'GCS_Gunung_Segara'
              );
  _cs.Add     (   37257, 'GCS_S42_Hungary'
              );
  _cs.Add     (   37259, 'GCS_Kusaie_1951'
              );
  _cs.Add     (   37260, 'GCS_Alaskan_Islands'
              );
  _cs.Add     (  104000, 'GCS_Assumed_Geographic_1'
              );
  _cs.Add     (  104101, 'GCS_Estonia_1937'
              );
  _cs.Add     (  104102, 'GCS_Hermannskogel'
              );
  _cs.Add     (  104103, 'GCS_Sierra_Leone_1960'
              );
  _cs.Add     (  104104, 'GCS_Hong_Kong_1980'
              );
  _cs.Add     (  104105, 'GCS_Datum_Lisboa_Bessel'
              );
  _cs.Add     (  104106, 'GCS_Datum_Lisboa_Hayford'
              );
  _cs.Add     (  104107, 'GCS_RGF_1993'
              );
  _cs.Add     (  104108, 'GCS_NZGD_2000'
              );
  _cs.Add     (  104261, 'GCS_Merchich_Degree'
              );
  _cs.Add     (  104304, 'GCS_Voirol_1875'
              );
  _cs.Add     (  104305, 'GCS_Voirol_Unifie_1960_Degree'
              );
end;

begin
  prepare_1;
  prepare_2;
  prepare_3;
  prepare_4;
  prepare_5;
  prepare_6;
end;

//==================================== END ====================================


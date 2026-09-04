# Belcher runner behavior is fixed across modes and profiles

    Code
      cat(belcher_test__snapshot_json(lapply(results, belcher_test__result_behavior)))
    Output
      {
        "absolute_legacy": {
          "backend": "belcher_absolute",
          "profile": "legacy",
          "policy": "legacy_independent_fields",
          "data": {
            "dimensions": "8760x40",
            "schema": "datetime:POSIXct/POSIXt[tz=UTC],year:integer,month:integer,day:integer,hour:integer,minute:integer,data_source:character,dry_bulb_temperature:numeric,dew_point_temperature:numeric,relative_humidity:numeric,atmospheric_pressure:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,global_horizontal_radiation:numeric,direct_normal_radiation:numeric,diffuse_horizontal_radiation:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric,wind_direction:numeric,wind_speed:numeric,total_sky_cover:integer,opaque_sky_cover:integer,visibility:numeric,ceiling_height:numeric,present_weather_observation:integer,present_weather_codes:character,precipitable_water:numeric,aerosol_optical_depth:numeric,snow_depth:numeric,days_since_last_snow:integer,albedo:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future]",
            "digest": "a31099be8bbeed16ad6f0dffec9cdc3234ecaae8fa8da3195fe4207fe33e1852"
          },
          "parts": {
            "tdb": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dry_bulb_temperature:numeric,delta:numeric,alpha:numeric",
              "digest": "d29f9d029ac694d6efe7da16e99f0b04e0c540a73d765926fbb0d16ee74ff483"
            },
            "tdew": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dew_point_temperature:numeric,delta:numeric,alpha:numeric",
              "digest": "8ec6105384d2047bf036dc93d2fc185ffe65151871dcd72567339d49aa232ae1"
            },
            "rh": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,relative_humidity:numeric,delta:numeric,alpha:numeric",
              "digest": "46bfebf20eb54004021a698b68452581ee90d1e5395f7a88e46a2fb83d7089f7"
            },
            "p": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,atmospheric_pressure:numeric,delta:numeric,alpha:numeric",
              "digest": "ea45af38a5fff16063d6f38313087bfcda64df501a30b9b93a83ba5409bd44c8"
            },
            "hor_ir": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,delta:numeric,alpha:numeric",
              "digest": "7c70cbedd68c769a65e624bdba65e93ba721469337eff539e011f72250e15abd"
            },
            "solar": {
              "dimensions": "0x0",
              "schema": "",
              "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
            },
            "glob_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,global_horizontal_radiation:numeric,delta:numeric,alpha:numeric",
              "digest": "ceb20ade4f7ceb619da58f2937d173a5b0ce00d3aa725de04ea959e73211b43d"
            },
            "norm_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,direct_normal_radiation:numeric",
              "digest": "03b874eac023f8a4fe3e88fd4f4411af3d025e613767aea57d457c5fa2a1a5ac"
            },
            "diff_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,diffuse_horizontal_radiation:numeric",
              "digest": "355e3f879d318bfd1c2ffd292ee010db3b185ec2f482a3dfabd4a0bebfcc1b54"
            },
            "illuminance": {
              "dimensions": "0x0",
              "schema": "",
              "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
            },
            "wind": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,wind_speed:numeric,delta:numeric,alpha:numeric",
              "digest": "0f5e7fc2daf2b7b9ac3e58eb0390346264fe61007a5a8a1e3cb3b168ce2f3935"
            },
            "total_cover": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,total_sky_cover:numeric,delta:numeric,alpha:numeric",
              "digest": "9f75984fb1c8773f65884df7c73fdc9db00733bc3e7c9f35b604df0ffb750ed0"
            },
            "opaque_cover": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,opaque_sky_cover:numeric,delta:numeric,alpha:numeric",
              "digest": "88bb2ae5d582d1f49a316f73f03a936a67caf578bcb1a08120e98c8b2121b067"
            },
            "snow_depth": {
              "dimensions": "0x0",
              "schema": "",
              "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
            },
            "precip": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,delta:numeric,alpha:numeric",
              "digest": "ce3a7f3af8ee05c594570cef3fb4d2b16ca59240706cc8328db7dfe5f97e4b14"
            }
          },
          "factors": {
            "dimensions": "0x0",
            "schema": "",
            "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
          },
          "diagnostics": {
            "dimensions": "0x14",
            "schema": "stage:character,severity:character,code:character,message:character,plan_id:character,summary_id:character,baseline_id:character,morph_id:character,case_id:character,variable_id:character,epw_field:character,period:character,month:integer,action:character",
            "digest": "460f9e97b866482316a7e19405881b5ba651e3053379505e1724c341b3409117"
          }
        },
        "absolute_enhanced": {
          "backend": "belcher_absolute",
          "profile": "enhanced",
          "policy": "monthly_harmonized",
          "data": {
            "dimensions": "8760x40",
            "schema": "datetime:POSIXct/POSIXt[tz=UTC],year:integer,month:integer,day:integer,hour:integer,minute:integer,data_source:character,dry_bulb_temperature:numeric,dew_point_temperature:numeric,relative_humidity:numeric,atmospheric_pressure:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,global_horizontal_radiation:numeric,direct_normal_radiation:numeric,diffuse_horizontal_radiation:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric,wind_direction:numeric,wind_speed:numeric,total_sky_cover:integer,opaque_sky_cover:integer,visibility:numeric,ceiling_height:numeric,present_weather_observation:integer,present_weather_codes:character,precipitable_water:numeric,aerosol_optical_depth:numeric,snow_depth:numeric,days_since_last_snow:integer,albedo:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future]",
            "digest": "dd806203fdab9e1bfb5915d392166237872203a78cd72df0c4c46a2346a87e44"
          },
          "parts": {
            "tdb": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dry_bulb_temperature:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "957f5ab66735ca20c283c39b2bd2a289840fc2db2b03801608cfc937b94b249b"
            },
            "tdew": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,method_applied:character,factor_status:character,dew_point_temperature:numeric,delta:numeric,alpha:numeric",
              "digest": "2f827f7369c2798ab566717b31da84fe2396e11a13d2ef4ccc3b2beab1ffa3ba"
            },
            "rh": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,relative_humidity:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "00c6f8c78f8d1a1327aa3ab98601cd9392d6d5b201175eb419da9c94371fd72b"
            },
            "p": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,atmospheric_pressure:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "b160029724ccd0401a28638733e2acf53083a3671b2044555d011e6d2bd4880e"
            },
            "hor_ir": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "46afd6c1246026e1eb460b055f6c51ca0fbdcd6c29fb52cbebdf4b36fda9d437"
            },
            "solar": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric",
              "digest": "f32d8ca94e231850e91eec8cae4436709f7c5a9e696fecbb2dcc609a2fbb03e3"
            },
            "glob_rad": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,global_horizontal_radiation:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "ed01eeb4d5255a45cea92143d3e324b08c50674743836f13af07b011df56b4cf"
            },
            "norm_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,direct_normal_radiation:numeric",
              "digest": "c795282568a9865dfc1060fc01f7652c4ffd17be8823f65e42952569bcd9cd8f"
            },
            "diff_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,diffuse_horizontal_radiation:numeric",
              "digest": "e52a4a55e71e07981ecd040ee36dcc8f85e3862c64cf383bd144a03dab972bb2"
            },
            "illuminance": {
              "dimensions": "8760x21",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric",
              "digest": "fa089e1dd3b15aca88faa1da25927bf71feb4c5a183da5b7453e881779e5edfe"
            },
            "wind": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,wind_speed:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "b8610c40d346f73764807b9d0c8decab68e227cad9c4165886b8e633e25d1aa6"
            },
            "total_cover": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,total_sky_cover:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "2b7653e86536fbd0d6b71f9b5286f09109cd09793e86ef3b51cf89736e8ffd2e"
            },
            "opaque_cover": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,opaque_sky_cover:numeric,delta:numeric,alpha:numeric",
              "digest": "417c98ecb71dbb414eff38f94d0540ccb359047af468a4b54376cae672a71fb5"
            },
            "snow_depth": {
              "dimensions": "0x0",
              "schema": "",
              "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
            },
            "precip": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,delta:numeric,alpha:numeric",
              "digest": "ce3a7f3af8ee05c594570cef3fb4d2b16ca59240706cc8328db7dfe5f97e4b14"
            }
          },
          "factors": {
            "dimensions": "96x10",
            "schema": "source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future],month:numeric,factor_status:character,method_applied:character,delta:numeric,alpha:numeric,step:character",
            "digest": "674377cd7d7e495d7bb1c5d8ca1813eff585d47831304dcd548d2a00852741b6"
          },
          "diagnostics": {
            "dimensions": "1x14",
            "schema": "stage:character,severity:character,code:character,message:character,plan_id:character,summary_id:character,baseline_id:character,morph_id:character,case_id:character,variable_id:character,epw_field:character,period:character,month:integer,action:character",
            "digest": "bb750ecc58592432cf5353fd306bf12d3fedc7b6da89f108eddee7e8f7c66226"
          }
        },
        "change_legacy": {
          "backend": "belcher",
          "profile": "legacy",
          "policy": "legacy_independent_fields",
          "data": {
            "dimensions": "8760x40",
            "schema": "datetime:POSIXct/POSIXt[tz=UTC],year:integer,month:integer,day:integer,hour:integer,minute:integer,data_source:character,dry_bulb_temperature:numeric,dew_point_temperature:numeric,relative_humidity:numeric,atmospheric_pressure:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,global_horizontal_radiation:numeric,direct_normal_radiation:numeric,diffuse_horizontal_radiation:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric,wind_direction:numeric,wind_speed:numeric,total_sky_cover:integer,opaque_sky_cover:integer,visibility:numeric,ceiling_height:numeric,present_weather_observation:integer,present_weather_codes:character,precipitable_water:numeric,aerosol_optical_depth:numeric,snow_depth:numeric,days_since_last_snow:integer,albedo:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future]",
            "digest": "d812d436692c4a5af1d722597e2b255fb8b10a82fb95818dbd305e60ae300e22"
          },
          "parts": {
            "tdb": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dry_bulb_temperature:numeric,delta:numeric,alpha:numeric",
              "digest": "42b1ea524340d4921bd992c53c1cc1cdf9eb9cd3229ac16dc18ec5526875a850"
            },
            "tdew": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dew_point_temperature:numeric,delta:numeric,alpha:numeric",
              "digest": "ec03df18767d0cf26dc0211aceb427c0565f7e07470bbfdb4dfb7d47c9fd1865"
            },
            "rh": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,relative_humidity:numeric,delta:numeric,alpha:numeric",
              "digest": "f609b70ac6b6af205ddb42e3071eb587ded4aa01f861065ea21a867d0ceb446a"
            },
            "p": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,atmospheric_pressure:numeric,delta:numeric,alpha:numeric",
              "digest": "73a4c2c734acdd0bd7394af1ca86f6e978d048f38166f7870805b88099947be8"
            },
            "hor_ir": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,delta:numeric,alpha:numeric",
              "digest": "340b20209be512c3bde4394766cea523250ece144adb0cb9f695c8c37e82e25d"
            },
            "solar": {
              "dimensions": "0x0",
              "schema": "",
              "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
            },
            "glob_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,global_horizontal_radiation:numeric,delta:numeric,alpha:numeric",
              "digest": "52d1e05553ee6a3e7b95e837a0cfef698ad319ba2fe764a407cbd9da64948933"
            },
            "norm_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,direct_normal_radiation:numeric",
              "digest": "3a818dbbe9388f9cc7d3227fdd04aefcddcd87c867653eca06829082e99f77b7"
            },
            "diff_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,diffuse_horizontal_radiation:numeric",
              "digest": "aae7135fd7eac88000dde7b344ef5ee2b2466d3da2bad961534fdd7145281a55"
            },
            "illuminance": {
              "dimensions": "0x0",
              "schema": "",
              "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
            },
            "wind": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,wind_speed:numeric,delta:numeric,alpha:numeric",
              "digest": "1f8fb2bb7a6709316b623937c4c4db98a0abdea66fcbef19eefca7c721b69bbb"
            },
            "total_cover": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,total_sky_cover:numeric,delta:numeric,alpha:numeric",
              "digest": "b12739f83d12004c72cc3104d48589630ce3260d4ad0e03660bfe5d1ce9824ef"
            },
            "opaque_cover": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,opaque_sky_cover:numeric,delta:numeric,alpha:numeric",
              "digest": "0a5be1c870c6f47c4b720f3d66c7a0738db7ca0e050748d084d9dd5bdebc2a20"
            },
            "snow_depth": {
              "dimensions": "0x0",
              "schema": "",
              "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
            },
            "precip": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,delta:numeric,alpha:numeric",
              "digest": "c6a59b73b7dec1095258e8886b975185a174443652c8b378814b5c3a12238172"
            }
          },
          "factors": {
            "dimensions": "0x0",
            "schema": "",
            "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
          },
          "diagnostics": {
            "dimensions": "0x14",
            "schema": "stage:character,severity:character,code:character,message:character,plan_id:character,summary_id:character,baseline_id:character,morph_id:character,case_id:character,variable_id:character,epw_field:character,period:character,month:integer,action:character",
            "digest": "460f9e97b866482316a7e19405881b5ba651e3053379505e1724c341b3409117"
          }
        },
        "change_enhanced": {
          "backend": "belcher",
          "profile": "enhanced",
          "policy": "monthly_harmonized",
          "data": {
            "dimensions": "8760x40",
            "schema": "datetime:POSIXct/POSIXt[tz=UTC],year:integer,month:integer,day:integer,hour:integer,minute:integer,data_source:character,dry_bulb_temperature:numeric,dew_point_temperature:numeric,relative_humidity:numeric,atmospheric_pressure:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,global_horizontal_radiation:numeric,direct_normal_radiation:numeric,diffuse_horizontal_radiation:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric,wind_direction:numeric,wind_speed:numeric,total_sky_cover:integer,opaque_sky_cover:integer,visibility:numeric,ceiling_height:numeric,present_weather_observation:integer,present_weather_codes:character,precipitable_water:numeric,aerosol_optical_depth:numeric,snow_depth:numeric,days_since_last_snow:integer,albedo:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future]",
            "digest": "cc0543fdc9fb13f4af8dfcdb801f8d3d68128b1c6474d62055b605032303c0d8"
          },
          "parts": {
            "tdb": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dry_bulb_temperature:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "c901f3912f3c18a0ab4cc8363412458300426f983b02628b65a814016924ad8b"
            },
            "tdew": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dew_point_temperature:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "919abb7059ff0d7e86c4417783026f0658a1e49cfd9a11eb7a879868bce0ba76"
            },
            "rh": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,relative_humidity:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "5ef2a81a44f85b8611f2afc67f535b0388b575be1b24a828e2a2edb14899e5d0"
            },
            "p": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,atmospheric_pressure:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "8ed68331cf2d2f2891a1b0d294434acf947b92d4055c5418c555e380dea3d8be"
            },
            "hor_ir": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "5560b4d82dd5539ca0ea63fb9cf6c7651b178592d859e812c0a6a8afa471a5cd"
            },
            "solar": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric",
              "digest": "9e4383f515f45e8f9d81751f7bc7281d7165a275507af7eb4b438ffe7690f624"
            },
            "glob_rad": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,global_horizontal_radiation:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "f7fa91e94c18abe56c40d484a9bf6eab298921ae9ce2f82ddf0acb54f1c9e48e"
            },
            "norm_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,direct_normal_radiation:numeric",
              "digest": "acd24fadc3d21ebb9233d2455ce7183036c7593c304dfff3a27753940e705e33"
            },
            "diff_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,diffuse_horizontal_radiation:numeric",
              "digest": "e410247dc4d2eb44b997646591317eccc07d28fb96c6296c3da4edbb50641d4f"
            },
            "illuminance": {
              "dimensions": "8760x21",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric",
              "digest": "13b04eaea68310c7f8689ac528bb36ca98b0c620788ba64a468de4734d1d73fb"
            },
            "wind": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,wind_speed:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "0dd05df36b86a489cda147910d48af445c23a939bee5d8f03b77f1c5ed096fc8"
            },
            "total_cover": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,total_sky_cover:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "6c4ee9c9cc5fba55ac89089fd0d7a9da8538c1b35cff3ff6fdaa12bd9fa7bafd"
            },
            "opaque_cover": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,opaque_sky_cover:numeric,delta:numeric,alpha:numeric",
              "digest": "8578e07154069dc07bc4d3ec887290c74a462b061cda210ebf84f969549f50b6"
            },
            "snow_depth": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,snow_depth:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "cb77f3dd3f47e2f44c25fc87b888fe5319ccc3e249e98c75bc3212e27da7a604"
            },
            "precip": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,delta:numeric,alpha:numeric",
              "digest": "c6a59b73b7dec1095258e8886b975185a174443652c8b378814b5c3a12238172"
            }
          },
          "factors": {
            "dimensions": "108x10",
            "schema": "source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future],month:numeric,factor_status:character,method_applied:character,delta:numeric,alpha:numeric,step:character",
            "digest": "38aca90379f8efe2b96fec0fa2f5d5d83e90fa70d18a65f3f4d4a405749bd4bc"
          },
          "diagnostics": {
            "dimensions": "36x14",
            "schema": "stage:character,severity:character,code:character,message:character,plan_id:character,summary_id:character,baseline_id:character,morph_id:character,case_id:character,variable_id:character,epw_field:character,period:character,month:integer,action:character",
            "digest": "bbe017b95d9188807403a53195d8b826b5174dd7c4d73f2c28a45c0528dbeeee"
          }
        },
        "baseline_fallback": {
          "backend": "belcher",
          "profile": "enhanced",
          "policy": "monthly_harmonized",
          "data": {
            "dimensions": "8760x40",
            "schema": "datetime:POSIXct/POSIXt[tz=UTC],year:integer,month:integer,day:integer,hour:integer,minute:integer,data_source:character,dry_bulb_temperature:numeric,dew_point_temperature:numeric,relative_humidity:numeric,atmospheric_pressure:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,global_horizontal_radiation:numeric,direct_normal_radiation:numeric,diffuse_horizontal_radiation:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric,wind_direction:numeric,wind_speed:numeric,total_sky_cover:integer,opaque_sky_cover:integer,visibility:numeric,ceiling_height:numeric,present_weather_observation:integer,present_weather_codes:character,precipitable_water:numeric,aerosol_optical_depth:numeric,snow_depth:numeric,days_since_last_snow:integer,albedo:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future]",
            "digest": "f29965aeeb2a32484999a1f6e5e9390735069458f3e2d05ce0e3deb020f95c80"
          },
          "parts": {
            "tdb": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dry_bulb_temperature:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "957f5ab66735ca20c283c39b2bd2a289840fc2db2b03801608cfc937b94b249b"
            },
            "tdew": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dew_point_temperature:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "ea9cb31a9b4928c83a3d44679ca68a4e612854da70beeb8712a11be5ac952caf"
            },
            "rh": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,relative_humidity:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "321b2ef9e2911dd0674b1a3a88f1536925f2fd6c90843701b32ac8c63ce40752"
            },
            "p": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,atmospheric_pressure:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "bf2bd0870e5237bfc8b836525431057f2b44aa5bf305c77fb6c36ea2650fdd8c"
            },
            "hor_ir": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "46afd6c1246026e1eb460b055f6c51ca0fbdcd6c29fb52cbebdf4b36fda9d437"
            },
            "solar": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric",
              "digest": "f32d8ca94e231850e91eec8cae4436709f7c5a9e696fecbb2dcc609a2fbb03e3"
            },
            "glob_rad": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,global_horizontal_radiation:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "ed01eeb4d5255a45cea92143d3e324b08c50674743836f13af07b011df56b4cf"
            },
            "norm_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,direct_normal_radiation:numeric",
              "digest": "c795282568a9865dfc1060fc01f7652c4ffd17be8823f65e42952569bcd9cd8f"
            },
            "diff_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,diffuse_horizontal_radiation:numeric",
              "digest": "e52a4a55e71e07981ecd040ee36dcc8f85e3862c64cf383bd144a03dab972bb2"
            },
            "illuminance": {
              "dimensions": "8760x21",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric",
              "digest": "6a9adf7da7011d5365d8ba0d8e5d510ab7cd39ebaec6592715f27804a289fe51"
            },
            "wind": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,wind_speed:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "b8610c40d346f73764807b9d0c8decab68e227cad9c4165886b8e633e25d1aa6"
            },
            "total_cover": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,total_sky_cover:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "2b7653e86536fbd0d6b71f9b5286f09109cd09793e86ef3b51cf89736e8ffd2e"
            },
            "opaque_cover": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,opaque_sky_cover:numeric,delta:numeric,alpha:numeric",
              "digest": "417c98ecb71dbb414eff38f94d0540ccb359047af468a4b54376cae672a71fb5"
            },
            "snow_depth": {
              "dimensions": "0x0",
              "schema": "",
              "digest": "f3cf74ac2b202ab9c47cf8d839891c3ab4add7b1c64b2d1e5566d2b1617e561c"
            },
            "precip": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,delta:numeric,alpha:numeric",
              "digest": "ce3a7f3af8ee05c594570cef3fb4d2b16ca59240706cc8328db7dfe5f97e4b14"
            }
          },
          "factors": {
            "dimensions": "100x10",
            "schema": "source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future],month:numeric,factor_status:character,method_applied:character,delta:numeric,alpha:numeric,step:character",
            "digest": "23b9ba15dfe5b252327d0e5758d51c45bc762fa9ef6990a0536ef020e1ca8004"
          },
          "diagnostics": {
            "dimensions": "25x14",
            "schema": "stage:character,severity:character,code:character,message:character,plan_id:character,summary_id:character,baseline_id:character,morph_id:character,case_id:character,variable_id:character,epw_field:character,period:character,month:integer,action:character",
            "digest": "733e230b190163e4e6eeab0565f295cd64fa2ca8010cd3beb09809c20df5e564"
          }
        }
      }
# Belcher production case contexts preserve identity and isolation

    Code
      cat(belcher_test__snapshot_json(lapply(results, belcher_test__result_behavior)))
    Output
      {
        "model_a": {
          "backend": "belcher",
          "profile": "enhanced",
          "policy": "monthly_harmonized",
          "data": {
            "dimensions": "8760x40",
            "schema": "datetime:POSIXct/POSIXt[tz=UTC],year:integer,month:integer,day:integer,hour:integer,minute:integer,data_source:character,dry_bulb_temperature:numeric,dew_point_temperature:numeric,relative_humidity:numeric,atmospheric_pressure:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,global_horizontal_radiation:numeric,direct_normal_radiation:numeric,diffuse_horizontal_radiation:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric,wind_direction:numeric,wind_speed:numeric,total_sky_cover:integer,opaque_sky_cover:integer,visibility:numeric,ceiling_height:numeric,present_weather_observation:integer,present_weather_codes:character,precipitable_water:numeric,aerosol_optical_depth:numeric,snow_depth:numeric,days_since_last_snow:integer,albedo:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future]",
            "digest": "cc0543fdc9fb13f4af8dfcdb801f8d3d68128b1c6474d62055b605032303c0d8"
          },
          "parts": {
            "tdb": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dry_bulb_temperature:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "c901f3912f3c18a0ab4cc8363412458300426f983b02628b65a814016924ad8b"
            },
            "tdew": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dew_point_temperature:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "919abb7059ff0d7e86c4417783026f0658a1e49cfd9a11eb7a879868bce0ba76"
            },
            "rh": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,relative_humidity:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "5ef2a81a44f85b8611f2afc67f535b0388b575be1b24a828e2a2edb14899e5d0"
            },
            "p": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,atmospheric_pressure:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "8ed68331cf2d2f2891a1b0d294434acf947b92d4055c5418c555e380dea3d8be"
            },
            "hor_ir": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "5560b4d82dd5539ca0ea63fb9cf6c7651b178592d859e812c0a6a8afa471a5cd"
            },
            "solar": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric",
              "digest": "9e4383f515f45e8f9d81751f7bc7281d7165a275507af7eb4b438ffe7690f624"
            },
            "glob_rad": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,global_horizontal_radiation:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "f7fa91e94c18abe56c40d484a9bf6eab298921ae9ce2f82ddf0acb54f1c9e48e"
            },
            "norm_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,direct_normal_radiation:numeric",
              "digest": "acd24fadc3d21ebb9233d2455ce7183036c7593c304dfff3a27753940e705e33"
            },
            "diff_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,diffuse_horizontal_radiation:numeric",
              "digest": "e410247dc4d2eb44b997646591317eccc07d28fb96c6296c3da4edbb50641d4f"
            },
            "illuminance": {
              "dimensions": "8760x21",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric",
              "digest": "13b04eaea68310c7f8689ac528bb36ca98b0c620788ba64a468de4734d1d73fb"
            },
            "wind": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,wind_speed:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "0dd05df36b86a489cda147910d48af445c23a939bee5d8f03b77f1c5ed096fc8"
            },
            "total_cover": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,total_sky_cover:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "6c4ee9c9cc5fba55ac89089fd0d7a9da8538c1b35cff3ff6fdaa12bd9fa7bafd"
            },
            "opaque_cover": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,opaque_sky_cover:numeric,delta:numeric,alpha:numeric",
              "digest": "8578e07154069dc07bc4d3ec887290c74a462b061cda210ebf84f969549f50b6"
            },
            "snow_depth": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,snow_depth:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "cb77f3dd3f47e2f44c25fc87b888fe5319ccc3e249e98c75bc3212e27da7a604"
            },
            "precip": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,delta:numeric,alpha:numeric",
              "digest": "c6a59b73b7dec1095258e8886b975185a174443652c8b378814b5c3a12238172"
            }
          },
          "factors": {
            "dimensions": "108x10",
            "schema": "source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future],month:numeric,factor_status:character,method_applied:character,delta:numeric,alpha:numeric,step:character",
            "digest": "38aca90379f8efe2b96fec0fa2f5d5d83e90fa70d18a65f3f4d4a405749bd4bc"
          },
          "diagnostics": {
            "dimensions": "36x14",
            "schema": "stage:character,severity:character,code:character,message:character,plan_id:character,summary_id:character,baseline_id:character,morph_id:character,case_id:character,variable_id:character,epw_field:character,period:character,month:integer,action:character",
            "digest": "bbe017b95d9188807403a53195d8b826b5174dd7c4d73f2c28a45c0528dbeeee"
          }
        },
        "model_b": {
          "backend": "belcher",
          "profile": "enhanced",
          "policy": "monthly_harmonized",
          "data": {
            "dimensions": "8760x40",
            "schema": "datetime:POSIXct/POSIXt[tz=UTC],year:integer,month:integer,day:integer,hour:integer,minute:integer,data_source:character,dry_bulb_temperature:numeric,dew_point_temperature:numeric,relative_humidity:numeric,atmospheric_pressure:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,global_horizontal_radiation:numeric,direct_normal_radiation:numeric,diffuse_horizontal_radiation:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric,wind_direction:numeric,wind_speed:numeric,total_sky_cover:integer,opaque_sky_cover:integer,visibility:numeric,ceiling_height:numeric,present_weather_observation:integer,present_weather_codes:character,precipitable_water:numeric,aerosol_optical_depth:numeric,snow_depth:numeric,days_since_last_snow:integer,albedo:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future]",
            "digest": "3bf55a26aca3b7e3c98ba590ad4c2e63af753a61509f2e830f073c54292f75b5"
          },
          "parts": {
            "tdb": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dry_bulb_temperature:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "3b68bdc66751d2e1fc9b8b8345e34d0bb88b6d35bf40f2a9f95b312a1846f9b4"
            },
            "tdew": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,dew_point_temperature:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "cb0474646d07bce2fee0d7b9d5d0d94dbfa7d41b12e412c6ba441d5a7ae200ff"
            },
            "rh": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,relative_humidity:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "38dd94434bea74cfc2ffe776c1aacbbfccf55cd89eba716cc416a764b8688e0b"
            },
            "p": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,atmospheric_pressure:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "62838aacf014f8728ae42f09b10b03f375a7774897b3b907785a9b1701fd4a9f"
            },
            "hor_ir": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,horizontal_infrared_radiation_intensity_from_sky:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "e0a8f76f9bd746696c16bfd82c2e69722516c677e087f1e5daeaac495b337cb0"
            },
            "solar": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,extraterrestrial_horizontal_radiation:numeric,extraterrestrial_direct_normal_radiation:numeric",
              "digest": "60dddb842122a5c04fd1c757ce590be3bca59aed2e1578be0b0f2ddcc1472a0c"
            },
            "glob_rad": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,global_horizontal_radiation:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "4d5ab1c5503555fbfe69aa1310c4b25ac7e217d389f5f3f99a9651d1e0b0d6da"
            },
            "norm_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,direct_normal_radiation:numeric",
              "digest": "091fe4c8030226f6fae314065da9676731756e07a9259144685af2912ab8845f"
            },
            "diff_rad": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,diffuse_horizontal_radiation:numeric",
              "digest": "61fb057b64c709c869692811f55c8607b4443482d685251094e4d8dec50fd890"
            },
            "illuminance": {
              "dimensions": "8760x21",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,delta:numeric,alpha:numeric,global_horizontal_illuminance:numeric,direct_normal_illuminance:numeric,diffuse_horizontal_illuminance:numeric,zenith_luminance:numeric",
              "digest": "feceb60b05c5e23f289a3ba5993f2ffaff2d7deb1870c2adb61bef19c0ee53aa"
            },
            "wind": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,wind_speed:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "d4a3f9c8ca90c92419cfb4a4ffd9a8fad68d0e316276ca14b03d04965a1a11be"
            },
            "total_cover": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,total_sky_cover:numeric,delta:numeric,alpha:numeric,factor_status:character",
              "digest": "0547a79bf237bd2a7d5be19854d00442f4647b525d7d7e9d4373f4939479454d"
            },
            "opaque_cover": {
              "dimensions": "8760x18",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,opaque_sky_cover:numeric,delta:numeric,alpha:numeric",
              "digest": "e17712ed0b080cf19c16f4cc48a9423482b737bfb975544fcf33f66f10300603"
            },
            "snow_depth": {
              "dimensions": "8760x20",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,snow_depth:numeric,delta:numeric,alpha:numeric,method_applied:character,factor_status:character",
              "digest": "be80dc9a0974eeb2931d7c544ad654e50960132d9f25ad3090ee8c63f0d0b4d1"
            },
            "precip": {
              "dimensions": "8760x19",
              "schema": "activity_drs:character,institution_id:character,source_id:character,experiment_id:character,member_id:character,table_id:character,lon:numeric,lat:numeric,interval:factor[levels=future],datetime:numeric,year:numeric,month:numeric,day:numeric,hour:numeric,minute:numeric,liquid_precip_depth:numeric,liquid_precip_rate:numeric,delta:numeric,alpha:numeric",
              "digest": "f9107971e198c3f49e1039f34c0e79b543aff28333d1b2385bdd36e6a00359e7"
            }
          },
          "factors": {
            "dimensions": "108x10",
            "schema": "source_id:character,experiment_id:character,member_id:character,interval:factor[levels=future],month:numeric,factor_status:character,method_applied:character,delta:numeric,alpha:numeric,step:character",
            "digest": "34df2b7a03f09d1337995dcc07e04a0b3e4bd9b1c1e0e7f3aa435a35c0845c57"
          },
          "diagnostics": {
            "dimensions": "36x14",
            "schema": "stage:character,severity:character,code:character,message:character,plan_id:character,summary_id:character,baseline_id:character,morph_id:character,case_id:character,variable_id:character,epw_field:character,period:character,month:integer,action:character",
            "digest": "bbe017b95d9188807403a53195d8b826b5174dd7c4d73f2c28a45c0528dbeeee"
          }
        }
      }

# Get the PXWEB API catalogue

Get the PXWEB API catalogue

## Usage

``` r
pxweb_api_catalogue()

pxweb_api_catalogue_from_json(json)

pxweb_api_catalogue_from_github(branch = "master")

pxweb_api_catalogue_path()
```

## Details

A list with implemented API:s.

## Examples

``` r
pxweb_api_catalogue()
#> $api.scb.se
#> Api: api.scb.se
#>      Statistics Sweden 
#>      ('scb')
#> Version(s)   : v1 
#> Language(s)  : en, sv 
#> Url template :
#>  https://api.scb.se/OV0104/[version]/doris/[lang] 
#> 
#> $statfin.stat.fi
#> Api: statfin.stat.fi
#>      Statistics Finland 
#>      ('statfi', 'statfin')
#> Version(s)   : v1 
#> Language(s)  : en, fi, sv 
#> Url template :
#>  https://statfin.stat.fi/PXWeb/api/[version]/[lang] 
#> 
#> $statistik.sjv.se
#> Api: statistik.sjv.se
#>      The Swedish Agricultural Agency 
#>      ('jordbruksverket')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Url template :
#>  https://statistik.sjv.se/PXWeb/api/[version]/[lang] 
#> 
#> $`fohm-app.folkhalsomyndigheten.se`
#> Api: fohm-app.folkhalsomyndigheten.se
#>      The Public Health Agency of Sweden 
#>      ('fohm')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Url template :
#>  https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/[version]/[lang] 
#> 
#> $statistik.konj.se
#> Api: statistik.konj.se
#>      The Swedish National Institute of Economic Research 
#>      ('konj')
#> Version(s)   : v1 
#> Language(s)  : en, sv 
#> Url template :
#>  https://statistik.konj.se/PXWeb/api/[version]/[lang] 
#> 
#> $prognos.konj.se
#> Api: prognos.konj.se
#>      The Swedish national institute of economic research, forecast database 
#>      ('konjforcast')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Url template :
#>  https://prognos.konj.se/PXWeb/api/[version]/[lang] 
#> 
#> $statdb.luke.fi
#> Api: statdb.luke.fi
#>      LUKE Natural Resources Institute Finland 
#>      ('luke')
#> Version(s)   : v1 
#> Language(s)  : en, fi, sv 
#> Url template :
#>  https://statdb.luke.fi/PXWeb/api/[version]/[lang] 
#> 
#> $vero2.stat.fi
#> Api: vero2.stat.fi
#>      Verohallinto - Finnish Tax Administration 
#>      ('vero')
#> Version(s)   : v1 
#> Language(s)  : en, fi, sv 
#> Url template :
#>  https://vero2.stat.fi/PXWeb/api/[version]/[lang] 
#> 
#> $px.hagstofa.is
#> Api: px.hagstofa.is
#>      Statistics Iceland 
#>      ('statice')
#> Version(s)   : v1 
#> Language(s)  : en, is 
#> Url template :
#>  https://px.hagstofa.is/px[lang]/api/[version]/[lang] 
#> 
#> $statistik.linkoping.se
#> Api: statistik.linkoping.se
#>      Linköping municipality in Sweden 
#>      ('linkoping')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Url template :
#>  https://statistik.linkoping.se/PXWeb/api/[version]/[lang] 
#> 
#> $pxweb2022.vgregion.se
#> Api: pxweb2022.vgregion.se
#>      Vastra Gotaland Region in Sweden 
#>      ('vgregion')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Url template :
#>  https://pxweb2022.vgregion.se/Pxwebb/api/[version]/[lang] 
#> 
#> $bank.stat.gl
#> Api: bank.stat.gl
#>      Statbank Greenland 
#>      ('greenland')
#> Version(s)   : v1 
#> Language(s)  : en, kl, da 
#> Url template :
#>  https://bank.stat.gl/api/[version]/[lang] 
#> 
#> $statbank.hagstova.fo
#> Api: statbank.hagstova.fo
#>      Statistics Faroe Islands 
#>      ('hagstovan')
#> Version(s)   : v1 
#> Language(s)  : en, fo 
#> Url template :
#>  https://statbank.hagstova.fo:443/api/[version]/[lang] 
#> 
#> $pxweb.asub.ax
#> Api: pxweb.asub.ax
#>      Statistics Aland 
#>      ('asub')
#> Version(s)   : v1 
#> Language(s)  : en, sv 
#> Url template :
#>  https://pxweb.asub.ax/PXWeb/api/[version]/[lang]/ 
#> 
#> $makstat.stat.gov.mk
#> Api: makstat.stat.gov.mk
#>      State Statistical Office of the Republic of Macedonia 
#>      ('makstat')
#> Version(s)   : v1 
#> Language(s)  : en, mk 
#> Url template :
#>  https://makstat.stat.gov.mk/PXWeb/api/[version]/[lang] 
#> 
#> $data.stat.gov.lv
#> Api: data.stat.gov.lv
#>      Latvia - official statistics 
#>      ('csb_lv')
#> Version(s)   : v1 
#> Language(s)  : en, lv 
#> Url template :
#>  https://data.stat.gov.lv/api/[version]/[lang]/ 
#> 
#> $statbank.statistica.md
#> Api: statbank.statistica.md
#>      Statistics Moldova 
#>      ('statistica_md')
#> Version(s)   : v1 
#> Language(s)  : en, ro 
#> Url template :
#>  https://statbank.statistica.md/pxweb/api/[version]/[lang]/ 
#> 
#> $www.pxweb.bfs.admin.ch
#> Api: www.pxweb.bfs.admin.ch
#>      Statistics Switzerland 
#>      ('switzerland')
#> Version(s)   : v1 
#> Language(s)  : en, de, fr 
#> Url template :
#>  https://www.pxweb.bfs.admin.ch/api/[version]/[lang] 
#> 
#> $`askdata.rks-gov.net`
#> Api: askdata.rks-gov.net
#>      Statistics Kosovo 
#>      ('askdata')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://askdata.rks-gov.net/api/[version]/[lang]/ 
#> 
#> $trafi2.stat.fi
#> Api: trafi2.stat.fi
#>      Finnish Transport Safety Agency 
#>      ('trafi2')
#> Version(s)   : v1 
#> Language(s)  : en, sv, fi 
#> Url template :
#>  https://trafi2.stat.fi/PXWeb/api/[version]/[lang]/ 
#> 
#> $w3.unece.org
#> Api: w3.unece.org
#>      United Nations Economic Commission for Europe 
#>      ('unece')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://w3.unece.org/PXWeb2015/api/[version]/[lang]/ 
#> 
#> $`www.grande-region.lu`
#> Api: www.grande-region.lu
#>      Portail statistique de la Grande Région 
#>      ('grande-region')
#> Version(s)   : v1 
#> Language(s)  : fr, de 
#> Url template :
#>  https://www.grande-region.lu/pxweb/api/[version]/[lang] 
#> 
#> $visitfinland.stat.fi
#> Api: visitfinland.stat.fi
#>      Visit Finland (Rudolf service) 
#> Version(s)   : v1 
#> Language(s)  : fi 
#> Url template :
#>  https://visitfinland.stat.fi/PXWeb/api/[version]/[lang] 
#> 
#> $stat.hel.fi
#> Api: stat.hel.fi
#>      Helsingin seudun aluesarjat -tilastotietokanta 
#> Version(s)   : v1 
#> Language(s)  : fi 
#> Url template :
#>  https://stat.hel.fi/api/[version]/[lang] 
#> 
#> $andmed.stat.ee
#> Api: andmed.stat.ee
#>      Estonia - official statistics 
#>      ('stat_ee')
#> Version(s)   : v1 
#> Language(s)  : en, et 
#> Url template :
#>  https://andmed.stat.ee/api/[version]/[lang] 
#> 
#> $pxweb.nordicstatistics.org
#> Api: pxweb.nordicstatistics.org
#>      Nordic Statistics Database 
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://pxweb.nordicstatistics.org/api/[version]/[lang]/ 
#> 
#> $pxweb.stat.si
#> Api: pxweb.stat.si
#>      SiStat Database 
#> Version(s)   : v1 
#> Language(s)  : sl 
#> Url template :
#>  https://pxweb.stat.si/SiStatData/api/[version]/[lang] 
#> 
#> $statistik.csn.se
#> Api: statistik.csn.se
#>      Swedish Board of Student Finance 
#>      ('csn')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Url template :
#>  https://statistik.csn.se/PXWeb/api/[version]/[lang] 
#> 
#> $`m02-http-pxwebb.login.sundsvall.se`
#> Api: m02-http-pxwebb.login.sundsvall.se
#>      Sundsvall municipality in Sweden 
#>      ('sundsvall')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Url template :
#>  https://m02-http-pxwebb.login.sundsvall.se/PXWeb_Ext/api/[version]/[lang] 
#> 
#> $statistik.vasteras.se
#> Api: statistik.vasteras.se
#>      Vasteras municipality in Sweden 
#>      ('vasteras')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Url template :
#>  https://statistik.vasteras.se/api/[version]/[lang] 
#> 
#> $pxweb.nhwstat.org
#> Api: pxweb.nhwstat.org
#>      Nordic Health and Welfare Statistics 
#>      ('nhwstat')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://pxweb.nhwstat.org/Prod/api/[version]/[lang] 
#> 
#> $web.dzs.hr
#> Api: web.dzs.hr
#>      Croatian Bureau of Statistics 
#>      ('croatia', 'dzs')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://web.dzs.hr/PXWeb/api/[version]/[lang] 
#> 
#> $etab.llv.li
#> Api: etab.llv.li
#>      Statistics Liechtenstein 
#>      ('liechtenstein')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://etab.llv.li/PXWeb/api/[version]/[lang]/eTab 
#> 
#> $www6.poderjudicial.es
#> Api: www6.poderjudicial.es
#>      Judicial statistics, Spain 
#>      ('poderjudicial')
#> Version(s)   : v1 
#> Language(s)  : es 
#> Url template :
#>  https://www6.poderjudicial.es/PxWeb-20252-v1/api/[version]/[lang] 
#> 
#> $openstat.psa.gov.ph
#> Api: openstat.psa.gov.ph
#>      Philippine Statistics Authority OpenSTAT 
#>      ('psa', 'openstat')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://openstat.psa.gov.ph/PXWeb/api/[version]/[lang] 
#> 
#> $px.web.ined.fr
#> Api: px.web.ined.fr
#>      Generations and Gender Contextual Database 
#>      ('ggp', 'ined')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://px.web.ined.fr/GGP/api/[version]/[lang] 
#> 
#> $statistika.spkc.gov.lv
#> Api: statistika.spkc.gov.lv
#>      Latvian Health Statistics Database 
#>      ('spkc', 'latvia_health')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://statistika.spkc.gov.lv/api/[version]/[lang]/Health 
#> 
#> $pxweb.irena.org
#> Api: pxweb.irena.org
#>      International Renewable Energy Agency 
#>      ('irena')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://pxweb.irena.org/api/[version]/[lang] 
#> 
#> $tilastot.etk.fi
#> Api: tilastot.etk.fi
#>      Finnish Centre for Pensions 
#>      ('etk')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://tilastot.etk.fi/api/[version]/[lang]/ETK 
#> 
#> $`pc-axis.geostat.ge`
#> Api: pc-axis.geostat.ge
#>      Geostat Statistics Database 
#>      ('geostat')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  http://pc-axis.geostat.ge/PXweb/api/[version]/[lang]/Database 
#> 
#> $statistika.tai.ee
#> Api: statistika.tai.ee
#>      Estonian Health Statistics and Health Research Database 
#>      ('tai', 'estonia_health')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://statistika.tai.ee/api/[version]/[lang] 
#> 
#> $pxexternal.energimyndigheten.se
#> Api: pxexternal.energimyndigheten.se
#>      Swedish Energy Agency 
#>      ('energimyndigheten')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://pxexternal.energimyndigheten.se/api/[version]/[lang] 
#> 
#> $skogsstatistik.slu.se
#> Api: skogsstatistik.slu.se
#>      Swedish University of Agricultural Sciences forest statistics 
#>      ('slu', 'riksskogstaxeringen')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://skogsstatistik.slu.se/api/[version]/[lang] 
#> 
#> $pxweb.skogsstyrelsen.se
#> Api: pxweb.skogsstyrelsen.se
#>      The Swedish Forest Agency 
#>      ('skogsstyrelsen')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://pxweb.skogsstyrelsen.se/api/[version]/[lang] 
#> 
#> $tilastot.tela.fi
#> Api: tilastot.tela.fi
#>      Finnish Pension Alliance statistics 
#>      ('tela')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Url template :
#>  https://tilastot.tela.fi/api/[version]/[lang] 
#> 
#> $statistik.tillvaxtanalys.se
#> Api: statistik.tillvaxtanalys.se
#>      Swedish Agency for Growth Policy Analysis (Tillväxtanalys) 
#>      ('tva', 'tillvaxtanalys')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Url template :
#>  https://statistik.tillvaxtanalys.se/PxWeb/api/[version]/[lang] 
#> 
#> attr(,"class")
#> [1] "pxweb_api_catalogue" "list"               
```

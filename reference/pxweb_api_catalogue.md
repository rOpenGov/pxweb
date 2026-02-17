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
#> Limit(s)     : 30 calls per 10 sec.
#>                110000  values per call.
#> Url template :
#>  https://api.scb.se/OV0104/[version]/doris/[lang] 
#> 
#> $statfin.stat.fi
#> Api: statfin.stat.fi
#>      Statistics Finland 
#>      ('statfi', 'statfin')
#> Version(s)   : v1 
#> Language(s)  : en, fi, sv 
#> Limit(s)     : 30 calls per 10 sec.
#>                120000  values per call.
#> Url template :
#>  https://statfin.stat.fi/PXWeb/api/[version]/[lang] 
#> 
#> $pxwebapi2.stat.fi
#> Api: pxwebapi2.stat.fi
#>      Statistics Finland (old version) 
#> Version(s)   : v1 
#> Language(s)  : fi 
#> Limit(s)     : 1000 calls per 10 sec.
#>                110000  values per call.
#> Url template :
#>  https://pxwebapi2.stat.fi/PXWeb/api/[version]/[lang] 
#> 
#> $statistik.sjv.se
#> Api: statistik.sjv.se
#>      The Swedish Agricultural Agency 
#>      ('jordbruksverket')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Limit(s)     : 1000 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  https://statistik.sjv.se/PXWeb/api/[version]/[lang] 
#> 
#> $`fohm-app.folkhalsomyndigheten.se`
#> Api: fohm-app.folkhalsomyndigheten.se
#>      The Public Health Agency of Sweden 
#>      ('fohm')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Limit(s)     : 1000 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/[version]/[lang] 
#> 
#> $statistik.konj.se
#> Api: statistik.konj.se
#>      The Swedish National Institute of Economic Research 
#>      ('konj')
#> Version(s)   : v1 
#> Language(s)  : en, sv 
#> Limit(s)     : 10 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  http://statistik.konj.se/PXWeb/api/[version]/[lang] 
#> 
#> $prognos.konj.se
#> Api: prognos.konj.se
#>      The Swedish national institute of economic research, forecast database 
#>      ('konjforcast')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Limit(s)     : 10 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  http://prognos.konj.se/PXWeb/api/[version]/[lang] 
#> 
#> $statdb.luke.fi
#> Api: statdb.luke.fi
#>      LUKE Natural Resources Institute Finland 
#>      ('luke')
#> Version(s)   : v1 
#> Language(s)  : en, fi, sv 
#> Limit(s)     : 10 calls per 10 sec.
#>                10000  values per call.
#> Url template :
#>  http://statdb.luke.fi/PXWeb/api/[version]/[lang] 
#> 
#> $vero2.stat.fi
#> Api: vero2.stat.fi
#>      Verohallinto - Finnish Tax Administration 
#>      ('vero')
#> Version(s)   : v1 
#> Language(s)  : en, fi, sv 
#> Limit(s)     : 100 calls per 10 sec.
#>                150000  values per call.
#> Url template :
#>  http://vero2.stat.fi/PXWeb/api/[version]/[lang] 
#> 
#> $px.hagstofa.is
#> Api: px.hagstofa.is
#>      Statistics Iceland 
#>      ('statice')
#> Version(s)   : v1 
#> Language(s)  : en, is 
#> Limit(s)     : 30 calls per 1 sec.
#>                10000  values per call.
#> Url template :
#>  http://px.hagstofa.is/px[lang]/api/[version]/[lang] 
#> 
#> $statistik.linkoping.se
#> Api: statistik.linkoping.se
#>      Linköping municipality in Sweden 
#>      ('linkoping')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Limit(s)     : 1000 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  http://statistik.linkoping.se/PXWeb/api/[version]/[lang] 
#> 
#> $pxwebb2017.vgregion.se
#> Api: pxwebb2017.vgregion.se
#>      Vastra Gotaland Region in Sweden 
#>      ('vgregion')
#> Version(s)   : v1 
#> Language(s)  : sv 
#> Limit(s)     : 10 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  http://pxwebb2017.vgregion.se/pxweb/api/[version]/[lang] 
#> 
#> $bank.stat.gl
#> Api: bank.stat.gl
#>      Statbank Greenland 
#>      ('greenland')
#> Version(s)   : v1 
#> Language(s)  : en, kl, da 
#> Limit(s)     : 10000 calls per 10 sec.
#>                1000000  values per call.
#> Url template :
#>  https://bank.stat.gl/api/[version]/[lang] 
#> 
#> $px.rsv.is
#> Api: px.rsv.is
#>      Icelandic Centre for Retail Studies 
#>      ('rsv')
#> Version(s)   : v1 
#> Language(s)  : en, is 
#> Limit(s)     : 30 calls per 1 sec.
#>                10000  values per call.
#> Url template :
#>  http://px.rsv.is/PXWeb/api/[version]/[lang] 
#> 
#> $statbank.hagstova.fo
#> Api: statbank.hagstova.fo
#>      Statistics Faroe Islands 
#>      ('hagstovan')
#> Version(s)   : v1 
#> Language(s)  : en, fo 
#> Limit(s)     : 1000000 calls per 1000000 sec.
#>                100000  values per call.
#> Url template :
#>  https://statbank.hagstova.fo:443/api/[version]/[lang] 
#> 
#> $data.ssb.no
#> Api: data.ssb.no
#>      Statistics Norway 
#>      ('ssb')
#> Version(s)   : v0 
#> Language(s)  : en, no 
#> Limit(s)     : 30 calls per 60 sec.
#>                30000  values per call.
#> Url template :
#>  http://data.ssb.no/api/[version]/[lang] 
#> 
#> $pxweb.asub.ax
#> Api: pxweb.asub.ax
#>      Statistics Aland 
#>      ('asub')
#> Version(s)   : v1 
#> Language(s)  : en, sv 
#> Limit(s)     : 10 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  https://pxweb.asub.ax/PXWeb/api/[version]/[lang]/ 
#> 
#> $makstat.stat.gov.mk
#> Api: makstat.stat.gov.mk
#>      State Statistical Office of the Republic of Macedonia 
#>      ('makstat')
#> Version(s)   : v1 
#> Language(s)  : en, mk 
#> Limit(s)     : 30 calls per 1 sec.
#>                10000  values per call.
#> Url template :
#>  https://makstat.stat.gov.mk/PXWeb/api/[version]/[lang]/MakStat/ 
#> 
#> $data.stat.gov.lv
#> Api: data.stat.gov.lv
#>      Latvia - official statistics 
#>      ('csb_lv')
#> Version(s)   : v1 
#> Language(s)  : en, lv 
#> Limit(s)     : 100 calls per 10 sec.
#>                3800  values per call.
#> Url template :
#>  https://data.stat.gov.lv/api/[version]/[lang]/ 
#> 
#> $statbank.statistica.md
#> Api: statbank.statistica.md
#>      Statistics Moldova 
#>      ('statistica_md')
#> Version(s)   : v1 
#> Language(s)  : en, ro 
#> Limit(s)     : 10 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  https://statbank.statistica.md/pxweb/api/[version]/[lang]/ 
#> 
#> $www.pxweb.bfs.admin.ch
#> Api: www.pxweb.bfs.admin.ch
#>      Statistics Switzerland 
#>      ('switzerland')
#> Version(s)   : v1 
#> Language(s)  : en, de, fr 
#> Limit(s)     : 10 calls per 10 sec.
#>                5000  values per call.
#> Url template :
#>  https://www.pxweb.bfs.admin.ch/api/[version]/[lang] 
#> 
#> $`askdata.rks-gov.net`
#> Api: askdata.rks-gov.net
#>      Statistics Kosovo 
#>      ('askdata')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Limit(s)     : 10 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  https://askdata.rks-gov.net/PXWeb/api/[version]/[lang]/ 
#> 
#> $trafi2.stat.fi
#> Api: trafi2.stat.fi
#>      Finnish Transport Safety Agency 
#>      ('trafi2')
#> Version(s)   : v1 
#> Language(s)  : en, sv, fi 
#> Limit(s)     : 100 calls per 10 sec.
#>                110000  values per call.
#> Url template :
#>  https://trafi2.stat.fi/PXWeb/api/[version]/[lang]/ 
#> 
#> $w3.unece.org
#> Api: w3.unece.org
#>      United Nations Economic Commission for Europe 
#>      ('unece')
#> Version(s)   : v1 
#> Language(s)  : en 
#> Limit(s)     : 10 calls per 10 sec.
#>                100000  values per call.
#> Url template :
#>  https://w3.unece.org/PXWeb2015/api/[version]/[lang]/ 
#> 
#> $`www.grande-region.lu`
#> Api: www.grande-region.lu
#>      Portail statistique de la Grande Région 
#>      ('grande-region')
#> Version(s)   : v1 
#> Language(s)  : fr, de 
#> Limit(s)     : 10 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  https://www.grande-region.lu/pxweb/api/[version]/[lang] 
#> 
#> $visitfinland.stat.fi
#> Api: visitfinland.stat.fi
#>      Visit Finland (Rudolf service) 
#> Version(s)   : v1 
#> Language(s)  : fi 
#> Limit(s)     : 20 calls per 10 sec.
#>                110000  values per call.
#> Url template :
#>  http://visitfinland.stat.fi/PXWeb/api/[version]/[lang]/VisitFinland 
#> 
#> $stat.hel.fi
#> Api: stat.hel.fi
#>      Helsingin seudun aluesarjat -tilastotietokanta 
#> Version(s)   : v1 
#> Language(s)  : fi 
#> Limit(s)     : 1000 calls per 10 sec.
#>                100000  values per call.
#> Url template :
#>  https://stat.hel.fi/api/[version]/[lang] 
#> 
#> $andmed.stat.ee
#> Api: andmed.stat.ee
#>      Estonia - official statistics 
#>      ('stat_ee')
#> Version(s)   : v1 
#> Language(s)  : en, et 
#> Limit(s)     : 1000 calls per 10 sec.
#>                1000000  values per call.
#> Url template :
#>  https://andmed.stat.ee/api/[version]/[lang]/stat 
#> 
#> $pxweb.nordicstatistics.org
#> Api: pxweb.nordicstatistics.org
#>      Nordic Statistics Database 
#> Version(s)   : v1 
#> Language(s)  : en 
#> Limit(s)     : 10 calls per 10 sec.
#>                1000  values per call.
#> Url template :
#>  https://pxweb.nordicstatistics.org/api/[version]/[lang]/ 
#> 
#> $pxweb.stat.si
#> Api: pxweb.stat.si
#>      SiStat Database 
#> Version(s)   : v1 
#> Language(s)  : sl 
#> Limit(s)     : 100 calls per 10 sec.
#>                10000000  values per call.
#> Url template :
#>  https://pxweb.stat.si/SiStatData/api/[version]/[lang]/Data 
#> 
#> attr(,"class")
#> [1] "pxweb_api_catalogue" "list"               
```

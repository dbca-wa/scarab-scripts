library("rjson")

# https://tsc.dbca.wa.gov.au/api/1/occ-taxon-points/
taxonpoints = rjson::fromJSON('[{"taxon": 25755, "code": "Condingup", "description": "Pine plantations off Fisheries Rd east of Condinup\nOpportunistic sighting\nDay sighting\nHeard/Call\nSaw a flock of approximately 200 birds in the pine plantation\nObserved by Abby Thomas, DBCA Technical Officer, Albany",    "source": 10,    "source_id": "94654",    "encountered_on": "2018-04-16T15:00:00+08:00",    "encountered_by": 1,    "area_type": 30,    "accuracy": 1000.0,    "point":"POINT(119.7322, -32.35756)"},{"taxon": 12905, "code": "7", "description": "Location: UCL, [ca. 7km NNW of Forrestania crossroads]. Shire of Kondinin.\nSource: TFL, sheet number 35719.\nLocated by Paul Armstrong for Lionore Australia.",    "source": 11, "source_id": "35719", "encountered_on": "2004-01-07T12:00:00+08:00", "encountered_by": 1, "area_type": 21, "accuracy": 10.0, "point":"POINT(119.7322 -32.35756)"}]')
taxonpoint = rjson::fromJSON('[{"taxon": 25755, "code": "Condingup", "description": "Pine plantations off Fisheries Rd east of Condinup\nOpportunistic sighting\nDay sighting\nHeard/Call\nSaw a flock of approximately 200 birds in the pine plantation\nObserved by Abby Thomas, DBCA Technical Officer, Albany",    "source": 10,    "source_id": "94654",    "encountered_on": "2018-04-16T15:00:00+08:00",    "encountered_by": 1,    "area_type": 30,    "accuracy": 1000.0,    "point":"SRID=4326;POINT (119.732234 -32.35756)"}]')
wastdr::wastd_POST(taxonpoint, serializer = "occ-taxon-points", verbose = T, api_url = "http://0.0.0.0:32770/api/1/")


j = rjson::fromJSON('{"community":148,"code":"828","description":"TEC Occurrence ID 1978\nBoundary 828 (to be pasted from shapefile)\nCircular basin wetland in NE portion of Drummond Nature Reserve clearly visible on orthophotos\nBuffered 500m\nNE Basin Wetland in Drummond Nature Reserve (42808)\nClay based wetland\nSeasonally inundated wetland in a sandy depression with no clear drainage lines indicating good internal drainage thought the sandy soil. Recorded as containing freshwater to 20cm in October but drying out in summer","source":12,"source_id":"1978","encountered_on":"2006-01-16T15:11:00+08:00","encountered_by":1,"area_type":0,"accuracy":500.0, "geom":{"type": "Polygon","coordinates": [[[117.294895,-27.136717],[122.7013,-23.523029],[126.964888,-27.019332],[123.800163,-29.189894],[120.020075,-29.419823],[117.294895,-27.136717]]]}}')
wastdr::wastd_POST(j, serializer = "occ-community-areas", verbose = T, api_url = dev)
jj <- wastdr::wastd_GET(serializer = "occ-community-areas", api_url = dev)
jj$features

# api/1/occ-community-points/
{
    "community": 148,
    "code": "828",
    "label": "Encounter of A.huegeliana and L.tuberculatum at [Ephemeral Site] (828) None on 2006-01-16 15:11:00+08:00 by Florian Mayer (Admin)",
    "description": "TEC Occurrence ID 1978\nBoundary 828 (to be pasted from shapefile)\nCircular basin wetland in NE portion of Drummond Nature Reserve clearly visible on orthophotos\nBuffered 500m\nNE Basin Wetland in Drummond Nature Reserve (42808)\nClay based wetland\nSeasonally inundated wetland in a sandy depression with no clear drainage lines indicating good internal drainage thought the sandy soil. Recorded as containing freshwater to 20cm in October but drying out in summer",
    "source": 12,
    "source_id": "1978",
    "encountered_on": "2006-01-16T15:11:00+08:00",
    "encountered_by": 1,
    "area_type": 0,
    "accuracy": 500.0,
    "geom":  [
            [
                117.294895,
                -27.136717
                ],
            [
                122.7013,
                -23.523029
                ],
            [
                126.964888,
                -27.019332
                ],
            [
                123.800163,
                -29.189894
                ],
            [
                120.020075,
                -29.419823
                ],
            [
                117.294895,
                -27.136717
                ]
            ]
}


# https://github.com/tomwenseleers/export
install.packages("officer")
install.packages("rvg")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("flextable")
install.packages("xtable")
install.packages("rgl")
install.packages("stargazer")
install.packages("tikzDevice")
install.packages("xml2")
install.packages("broom")
install.packages("devtools")
library(devtools)
devtools::install_github("tomwenseleers/export")

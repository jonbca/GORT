%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains reasoning for calculating populations, and population-related
%% guesstimation.


:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- ensure_loaded('fetch').
:- ensure_loaded('conversion').
:- ensure_loaded('calculations').

/** distance(+PointA, +PointB, -Distance) is det.
Computes the great-circle distance between PointA and PointB, and places
the result in Distance (in km). This is an implementation of the
Haversine formula for computing distances between points on a sphere.
PointA and PointB must be specified as: point(Latitude, Longitude)

From:
Wikipedia contributors, "Haversine formula," Wikipedia, The Free Encyclopedia,
http://en.wikipedia.org/w/index.php?title=Haversine_formula&oldid=302125655 (accessed July 14, 2009). 

@param PointA The start point
@param PointB The end point
@param Distance the distance, in km
*/
distance(point(LatitudeA, LongitudeA), point(LatitudeB, LongitudeB), km(Distance)) :-
    LatARad is LatitudeA * pi / 180,
    LongARad is LongitudeA * pi / 180,
    LatBRad is LatitudeB * pi / 180,
    LongBRad is LongitudeB * pi / 180,
    Distance is 2 * asin( 
    	sqrt( 
    		sin((LatARad - LatBRad) / 2) ** 2 +
    		cos(LatARad) * cos(LatBRad) *
    		(
    			sin((LongARad - LongBRad) / 2) ** 2
    		)
    	)) * 6371.

/** distance_between(+StartFeature, +EndFeature, Distance) is semidet.
Computes the distance between two features on the Earth's surface. Each feature
is identified by a URI. If lat/long is available for both, the distance will be computed.

@param StartFeature the starting location
@param EndFeature the ending location
@param Distance the distance from the start point to the end point.
*/
distance_between(StartFeature, EndFeature, Distance) :-
	feature_latitude(StartFeature, StartLat),
	feature_longitude(StartFeature, StartLong),
	feature_latitude(EndFeature, EndLat),
	feature_longitude(EndFeature, EndLong),
	distance(point(StartLat, StartLong), point(EndLat, EndLong), Distance).
    
feature_latitude(Feature, Latitude) :-
    solve(Feature, 'http://www.w3.org/2003/01/geo/wgs84_pos#lat', [literal(type(xsd:float, LatitudeA)) | _], _),
    atom_number(LatitudeA, Latitude).

feature_longitude(Feature, Longitude) :-
    solve(Feature, 'http://www.w3.org/2003/01/geo/wgs84_pos#long', [literal(type(xsd:float, LongitudeA)) | _], _),
    atom_number(LongitudeA, Longitude).

/**********************************************************************************************
RETURN TABLE WITH LINES BROKEN AT INTERSECTIONS

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	25/04/2014

Description:	Takes a table ("inputLines") with line geometries as an input and finds all 
		lines that need to be broken to create proper intersection nodes. Then breaks
		the lines accordingly and creates a table "newlines" with the broken lines.

Note:		Due to robustness issues in PostGIS, breaking lines at intersections may some-
		times produce residual lines of almost zero length. These are removed in the
		last step.
**********************************************************************************************/

DROP INDEX IF EXISTS inputlines_gix_SCHEMANAME;
CREATE INDEX inputlines_gix_SCHEMANAME ON SCHEMANAME.inputlines USING GIST (the_geom);


-- Compute all intersections between lines in inputLines table
-- Since the same two lines may cross several times, we use the ST_DUMP command to divide resulting multipoints
DROP TABLE IF EXISTS SCHEMANAME.intersectionPoints_0;
CREATE TABLE SCHEMANAME.intersectionPoints_0 AS
SELECT
	l.lid,
	l2.lid AS lid2,
	ST_STARTPOINT(l.the_geom) AS startpoint_geom,
	ST_ENDPOINT(l.the_geom) AS endpoint_geom,
	(ST_DUMP(ST_INTERSECTION(l.the_geom, l2.the_geom))).geom AS intersection_geom
FROM	SCHEMANAME.inputlines l
	INNER JOIN SCHEMANAME.inputlines l2
	ON ST_INTERSECTS(l.the_geom, l2.the_geom)
	AND l.lid <> l2.lid;
DROP INDEX IF EXISTS startpoint_gix_SCHEMANAME;
CREATE INDEX startpoint_gix_SCHEMANAME ON SCHEMANAME.intersectionPoints_0 USING GIST (startpoint_geom);

DROP INDEX IF EXISTS endpoint_gix_SCHEMANAME;
CREATE INDEX endpoint_gix_SCHEMANAME ON SCHEMANAME.intersectionPoints_0 USING GIST (endpoint_geom);

DROP INDEX IF EXISTS intersection_gix_SCHEMANAME;
CREATE INDEX intersection_gix_SCHEMANAME ON SCHEMANAME.intersectionPoints_0 USING GIST (intersection_geom);



-- Since the above query may also produce lines (if two input lines lie ontop of eachother), we use the
-- start and end points of these line intersections as node break points
DROP TABLE IF EXISTS SCHEMANAME.intersectionPoints_1;
CREATE TABLE SCHEMANAME.intersectionPoints_1 AS
SELECT
	ip.lid,
	ip.lid2,
	ip.startpoint_geom,
	ip.endpoint_geom,
	CASE
		WHEN GEOMETRYTYPE(ip.intersection_geom) = 'LINESTRING'
			THEN (ST_DUMP(ST_COLLECT(ST_STARTPOINT(ip.intersection_geom), ST_ENDPOINT(ip.intersection_geom)))).geom
		ELSE	intersection_geom
	END AS intersection_points
FROM	SCHEMANAME.intersectionPoints_0 ip;

DROP INDEX IF EXISTS intersection2_gix_SCHEMANAME;
CREATE INDEX intersection2_gix_SCHEMANAME ON SCHEMANAME.intersectionPoints_1 USING GIST (intersection_points);

-- From the above table, remove node break points that already overlap with the start or end point of the original
-- line, since in these cases the original line doesn't need to be broken
DROP TABLE IF EXISTS SCHEMANAME.intersectionPoints_2;
CREATE TABLE SCHEMANAME.intersectionPoints_2 AS
SELECT
	ip.lid,
	ip.lid2,
	ip.intersection_points AS the_geom
FROM	SCHEMANAME.intersectionPoints_1 ip
WHERE	NOT ST_DISTANCE(ip.intersection_points, ip.startpoint_geom) = 0
	AND NOT ST_DISTANCE(ip.intersection_points, ip.endpoint_geom) = 0;

DROP INDEX IF EXISTS intersectionpoints2_gix_SCHEMANAME;
CREATE INDEX intersectionpoints2_gix_SCHEMANAME ON SCHEMANAME.intersectionPoints_2 USING GIST (the_geom);



-- Find the length-fractions on the input lines where they need to be broken
DROP TABLE IF EXISTS SCHEMANAME.splitfractions;
CREATE TABLE SCHEMANAME.splitfractions AS
SELECT
	ip.lid,
	ip.lid2,
	ST_Line_Locate_Point(il.the_geom, ip.the_geom) AS splitfrac
FROM	SCHEMANAME.intersectionPoints_2 ip
	LEFT JOIN SCHEMANAME.inputlines il
	ON il.lid = ip.lid;


-- Create start and end fractions on which input lines need to be broken to generate new split lines
DROP TABLE IF EXISTS SCHEMANAME.splitfractions_all;
CREATE TABLE SCHEMANAME.splitfractions_all AS
SELECT DISTINCT
	u.lid,
	u.splitfrac1,
	u.splitfrac2
FROM (
SELECT
	sf.lid,
	sf.lid2,
	sf.splitfrac AS splitfrac1,
	CASE
		WHEN MIN(sf2.splitfrac) IS NOT NULL THEN MIN(sf2.splitfrac)
		ELSE 1
	END AS splitfrac2
FROM	SCHEMANAME.splitfractions sf
	LEFT JOIN SCHEMANAME.splitfractions sf2
	ON sf.lid = sf2.lid
	AND sf.splitfrac < sf2.splitfrac
GROUP BY	sf.lid,
		sf.lid2,
		sf.splitfrac
UNION ALL
SELECT
	sf.lid,
	sf.lid2,
	CASE
		WHEN MAX(sf2.splitfrac) IS NOT NULL THEN MAX(sf2.splitfrac)
		ELSE 0
	END AS splitfrac1,
	sf.splitfrac AS splitfrac2
FROM	SCHEMANAME.splitfractions sf
	LEFT JOIN SCHEMANAME.splitfractions sf2
	ON sf.lid = sf2.lid
	AND sf.splitfrac > sf2.splitfrac
GROUP BY	sf.lid,
		sf.lid2,
		sf.splitfrac) u
ORDER BY	u.lid,
		u.splitfrac1;


-- Finally, create the new splitlines
DROP TABLE IF EXISTS SCHEMANAME.newlines;
CREATE TABLE SCHEMANAME.newlines AS
SELECT
	'S-' || sf.lid || '-' || c.count AS lid,	-- Create new unique line IDs
	sf.lid AS lid_original,
	ST_SimplifyPreserveTopology(ST_Line_Substring(il.the_geom, sf.splitfrac1, sf.splitfrac2),0.000001) AS the_geom
FROM	SCHEMANAME.splitfractions_all sf
	LEFT JOIN SCHEMANAME.inputlines il
	ON il.lid = sf.lid
		LEFT JOIN (
		SELECT 
			sf.lid, 
			sf.splitfrac1, 
			COUNT(sf2.lid) AS count 
		FROM SCHEMANAME.splitfractions_all sf
		LEFT JOIN SCHEMANAME.splitfractions_all sf2 
		ON sf2.splitfrac1 < sf.splitfrac2 AND sf.lid = sf2.lid
		GROUP BY 	sf.lid, 
				sf.splitfrac1) c
		ON c.lid = sf.lid
		AND c.splitfrac1 = sf.splitfrac1
WHERE	GEOMETRYTYPE(ST_Line_Substring(il.the_geom, sf.splitfrac1, sf.splitfrac2)) = 'LINESTRING'
AND	ST_LENGTH(ST_Line_Substring(il.the_geom, sf.splitfrac1, sf.splitfrac2)) >= 1e-005;




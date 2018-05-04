/**********************************************************************************************
RETURN REAL END POINTS

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	16/05/2013

Description:	Takes a table ("inputLines") with line geometries as an input and creates a new
		table ("outputPoints") with real end points ''loose end points'' (i.e., end points
		that don't intersect with other lines).

Notes:		Line geometries in inputlines table must have unique ID named "lid"	

Run time:	~ 6500ms (if run locally)
**********************************************************************************************/


-- Create a list of end point candidates, i.e. all start and end points of all line geometries
DROP TABLE IF EXISTS SCHEMANAME.endpointcandidates;
CREATE TABLE SCHEMANAME.endpointcandidates AS
SELECT
	u.*
FROM	(SELECT
		lid,
		1 AS start,
		ST_STARTPOINT(the_geom) AS the_geom
	FROM	SCHEMANAME.inputlines
	WHERE	GeometryType(the_geom) = 'LINESTRING'
	UNION ALL
	SELECT
		lid,
		0 AS start,
		ST_ENDPOINT(the_geom) AS the_geom
	FROM	SCHEMANAME.inputLines
	WHERE	GeometryType(the_geom) = 'LINESTRING') u;
DROP SEQUENCE IF EXISTS SCHEMANAME.ep_seq;
CREATE SEQUENCE SCHEMANAME.ep_seq;
ALTER TABLE SCHEMANAME.endpointcandidates ADD COLUMN pid INTEGER DEFAULT nextval('SCHEMANAME.ep_seq');
DROP INDEX IF EXISTS endpointcands_gix_SCHEMANAME;
CREATE INDEX endpointcands_gix_SCHEMANAME ON SCHEMANAME.endpointcandidates USING GIST (the_geom);

-- Identify intersection end points, i.e., end points that already intersect with another line
CREATE TEMP TABLE intersectionEndpoints ON COMMIT DROP AS
SELECT
	epc.pid,
	epc.lid,
	epc.start,
	ARRAY_AGG(l.lid) AS intersect_gid_a
FROM	SCHEMANAME.endpointcandidates epc
	INNER JOIN SCHEMANAME.inputLines l
	ON ST_INTERSECTS(l.the_geom, epc.the_geom)
	AND l.lid <> epc.lid
GROUP BY	epc.pid,
		epc.lid,
		epc.start;

-- Create a real end point list, i.e., end points that do not intersect with another line
CREATE TEMP TABLE realEndpoints ON COMMIT DROP AS
SELECT
	epc.*
FROM	SCHEMANAME.endpointcandidates epc
	LEFT JOIN intersectionEndpoints f
	ON f.pid = epc.pid
WHERE	f.pid IS NULL;

-- Create output table
DROP TABLE IF EXISTS SCHEMANAME.outputpoints;
CREATE TABLE SCHEMANAME.outputpoints AS SELECT * FROM realendpoints;

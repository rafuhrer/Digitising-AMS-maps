/**********************************************************************************************
CREATE A TABLE OF POSSIBLE SNAP-LINES BASED ON A TABLE WITH LINE POINT GEOMETRIES

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	10/06/2013

Description:	Snaps points that are within specified buffer size of each other; 
		buffersize given by variable 'BUFFERSIZE'. Returns the according point
		combinations.

Notes:		Requires ''apoints'' table on DB
**********************************************************************************************/

-- Buffer the points
DROP TABLE IF EXISTS SCHEMANAME.apoints_b;
CREATE TABLE SCHEMANAME.apoints_b AS
SELECT 
	pid,
	eid,
	lid,
	ST_BUFFER(the_geom, BUFFERSIZE) AS the_geom
FROM SCHEMANAME.apoints;
DROP INDEX IF EXISTS idx_apoints_geom_SCHEMANAME;
CREATE INDEX idx_apoints_geom_SCHEMANAME ON SCHEMANAME.apoints_b USING gist(the_geom);
ALTER TABLE SCHEMANAME.apoints_b CLUSTER ON idx_apoints_geom_SCHEMANAME;


-- Get point combinations that are not on the same line, and within buffersize distance of each other
-- Also, create a lid-combination ID called ulid that is unique across line combinations
CREATE TEMP TABLE snap_candidates ON COMMIT DROP AS
SELECT
	ab.pid,
	ab.eid,
	ab.lid,
	a.pid AS pid2,
	a.eid AS eid2,
	a.lid AS lid2,
	ST_DISTANCE(a.the_geom, a2.the_geom) AS dist,
	CASE 
		WHEN ab.lid < a.lid THEN ab.lid || '_' || a.lid
		ELSE a.lid || '_' || ab.lid
	END AS ulid
FROM	SCHEMANAME.apoints_b ab
	INNER JOIN SCHEMANAME.apoints a
	ON ST_INTERSECTS(a.the_geom, ab.the_geom)
	AND a.pid <> ab.pid
	AND a.eid <> ab.eid
	AND a.lid <> ab.lid
		LEFT JOIN SCHEMANAME.apoints a2
		ON a2.pid = ab.pid
ORDER BY	ab.pid::int,
		a.pid::int;

-- Get the shortest snapline per line combination
-- Also, create a pid-combination ID called upid that is unique across pid-combinations
CREATE TEMP TABLE snap_candidates2 ON COMMIT DROP AS
SELECT DISTINCT ON (ulid)
	sc.*,
	CASE 
		WHEN sc.pid < sc.pid2 THEN sc.pid || '_' || sc.pid2
		ELSE sc.pid2 || '_' || sc.pid
	END AS upid
FROM	snap_candidates sc
ORDER BY	sc.ulid,
		sc.dist;


-- Get unique pid combinations (eliminates redundant combinations due to reverse matches, e.g. A-B and B-A)
SELECT DISTINCT ON (sc.upid)
	sc.pid,
	sc.eid,
	sc.lid,
	sc.pid2,
	sc.eid2,
	sc.lid2,
	sc.dist
FROM	snap_candidates2 sc
ORDER BY	sc.upid;





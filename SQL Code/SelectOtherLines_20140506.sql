/**********************************************************************************************
GET OTHER LINES INTERSECTING WHERE LID1-LID2 DYAD MEET

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	06/05/2014
**********************************************************************************************/

DROP INDEX IF EXISTS inputlines_gix_SCHEMANAME;
CREATE INDEX inputlines_gix_SCHEMANAME ON SCHEMANAME.inputlines USING GIST (the_geom);

-- Points where lid1 and lid2 intersect
DROP TABLE IF EXISTS SCHEMANAME.ipoint;
CREATE TABLE SCHEMANAME.ipoint AS
SELECT DISTINCT ON (e.lid1, e.lid2)
	e.lid1,
	e.lid2,
	(ST_DUMP(ST_INTERSECTION(s1.the_geom, s2.the_geom))).geom AS ipoint
FROM	SCHEMANAME.edges e
	LEFT JOIN SCHEMANAME.inputlines s1
	ON s1.lid = e.lid1
		LEFT JOIN SCHEMANAME.inputlines s2
		ON s2.lid = e.lid2
ORDER BY	e.lid1, e.lid2;
DROP INDEX IF EXISTS ipointgix_SCHEMANAME;
CREATE INDEX ipointgix_SCHEMANAME ON SCHEMANAME.ipoint USING GIST (ipoint);

-- Identify other lines that intersect with lid1 and lid2 at the same intersection point
SELECT
	ip.lid1,
	ip.lid2,
	il.lid AS otherlid
FROM	SCHEMANAME.ipoint ip
	INNER JOIN SCHEMANAME.inputlines il
	ON il.lid <> ip.lid1
	AND il.lid <> ip.lid2
	AND ST_INTERSECTS(il.the_geom, ip.ipoint);

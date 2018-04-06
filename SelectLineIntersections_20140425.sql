/**********************************************************************************************
GET LINE INTERSECTION POINTS

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	25/04/2014

Description:	Create a table with all intersection points and according line names.

Notes:		Line geometries in inputlines table must have unique ID named ''lid''
**********************************************************************************************/

DROP TABLE IF EXISTS SCHEMANAME.points;
CREATE TABLE SCHEMANAME.points AS
SELECT DISTINCT
	u.*
FROM (
SELECT
	ST_STARTPOINT(il.the_geom) AS the_geom
FROM	SCHEMANAME.inputlines il
UNION ALL 
SELECT 
	ST_ENDPOINT(il.the_geom) AS the_geom
FROM  	SCHEMANAME.inputlines il) u;
DROP SEQUENCE IF EXISTS SCHEMANAME.pid_seq;
CREATE SEQUENCE SCHEMANAME.pid_seq;
ALTER TABLE SCHEMANAME.points ADD COLUMN pid INTEGER DEFAULT nextval('SCHEMANAME.pid_seq');


SELECT
	p.pid,
	il.lid
FROM	SCHEMANAME.points p
	LEFT JOIN SCHEMANAME.inputlines il
	ON (ST_INTERSECTS(ST_STARTPOINT(il.the_geom), p.the_geom)
	OR ST_INTERSECTS(ST_ENDPOINT(il.the_geom), p.the_geom));
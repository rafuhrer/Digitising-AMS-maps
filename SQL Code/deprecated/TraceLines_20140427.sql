/**********************************************************************************************
TRACE LINES

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	27/04/2014

Description:	Trace line algorithm

Notes:		Requires ''inputpoints'' table on DB
**********************************************************************************************/

DROP TABLE IF EXISTS SCHEMANAME.pixels;
CREATE TABLE SCHEMANAME.pixels AS SELECT pid, the_geom, ST_BUFFER(the_geom, COMPLENGTH) AS buffer FROM SCHEMANAME.inputpoints;

DROP INDEX IF EXISTS pixels_gix_SCHEMANAME;
CREATE INDEX pixels_gix_SCHEMANAME ON SCHEMANAME.pixels USING GIST (the_geom);
DROP INDEX IF EXISTS buffers_gix_SCHEMANAME;
CREATE INDEX buffers_gix_SCHEMANAME ON SCHEMANAME.pixels USING GIST (buffer);



CREATE TEMP TABLE minilines_0 ON COMMIT DROP AS
SELECT
	ip.pid,
	ip2.pid AS pid2, 
	CASE WHEN ip.pid < ip2.pid THEN ip.pid || '_' || ip2.pid
	ELSE ip2.pid || '_' || ip.pid
	END AS upid,
	ST_MAKELINE(ip.the_geom, ip2.the_geom) AS the_geom
FROM	SCHEMANAME.pixels ip
	LEFT JOIN SCHEMANAME.pixels ip2
	ON ip.pid <> ip2.pid
	AND ST_INTERSECTS(ip.buffer, ip2.the_geom)
WHERE	ip2.pid IS NOT NULL;


DROP TABLE IF EXISTS SCHEMANAME.edgelines;
CREATE TABLE SCHEMANAME.edgelines AS
SELECT DISTINCT ON (upid) 
	upid AS lid,
	the_geom
FROM	minilines_0;
DROP INDEX IF EXISTS edge_gix_SCHEMANAME;
CREATE INDEX edge_gix_SCHEMANAME ON SCHEMANAME.edgelines USING GIST (the_geom);


DROP TABLE IF EXISTS SCHEMANAME.diagpoints;
CREATE TABLE SCHEMANAME.diagpoints AS
SELECT
	t.pid,
	t.the_geom,
	ST_BUFFER(t.the_geom, DIAGLENGTH) AS buffer
FROM	SCHEMANAME.pixels t
	LEFT JOIN SCHEMANAME.edgelines e
	ON ST_INTERSECTS(e.the_geom, t.the_geom)
WHERE	e.lid IS NULL;
DROP INDEX IF EXISTS diag_gix_SCHEMANAME;
CREATE INDEX diag_gix_SCHEMANAME ON SCHEMANAME.diagpoints USING GIST (the_geom);
DROP INDEX IF EXISTS bufferdiag_gix_SCHEMANAME;
CREATE INDEX bufferdiag_gix_SCHEMANAME ON SCHEMANAME.diagpoints USING GIST (buffer);


DROP TABLE IF EXISTS SCHEMANAME.edgepoints;
CREATE TABLE SCHEMANAME.edgepoints AS
SELECT
	t.pid,
	t.the_geom
FROM	SCHEMANAME.pixels t
	LEFT JOIN SCHEMANAME.edgelines e
	ON ST_INTERSECTS(e.the_geom, t.the_geom)
WHERE	e.lid IS NOT NULL;
DROP INDEX IF EXISTS edgep_gix_SCHEMANAME;
CREATE INDEX edgep_gix_SCHEMANAME ON SCHEMANAME.edgepoints USING GIST (the_geom);


CREATE TEMP TABLE diaglines_0 ON COMMIT DROP AS
SELECT
	d.pid,
	e.pid AS pid2, 
	CASE WHEN d.pid < e.pid THEN d.pid || '_' || e.pid
	ELSE e.pid || '_' || d.pid
	END AS upid,
	ST_MAKELINE(d.the_geom, e.the_geom) AS the_geom
FROM	SCHEMANAME.diagpoints d
	LEFT JOIN (SELECT u.* FROM (SELECT * FROM SCHEMANAME.edgepoints UNION ALL SELECT pid, the_geom FROM SCHEMANAME.diagpoints) u) e
	ON ST_INTERSECTS(d.buffer, e.the_geom)
	AND e.pid <> d.pid
WHERE	e.pid IS NOT NULL;


DROP TABLE IF EXISTS SCHEMANAME.diaglines;
CREATE TABLE SCHEMANAME.diaglines AS
SELECT DISTINCT ON (upid) 
	upid AS lid,
	the_geom
FROM	diaglines_0;


DROP TABLE IF EXISTS SCHEMANAME.candidatelines;
CREATE TABLE SCHEMANAME.candidatelines AS
SELECT 
	a.*,
	FALSE AS done
FROM (
(SELECT * FROM SCHEMANAME.diaglines)
UNION ALL
(SELECT * FROM SCHEMANAME.edgelines)) a;


-- Create function merging lines (but not at intersections)
DROP FUNCTION IF EXISTS SCHEMANAME.my_union();
create function SCHEMANAME.my_union() returns void
as $$
declare
	lid_i text;
	mergeline geometry;
	the_geom_i geometry;
	startpoint_i geometry;
	endpoint_i geometry;
	startcount int;
	endcount int;
	newline geometry;
	startmatch_lid text;
	startmatch_geom geometry;
	endmatch_lid text;
	endmatch_geom geometry;
	startdone boolean;
	enddone boolean;
	nrows int;
	multiline_i boolean;
begin
	loop

	--select count(lid) into nrows from SCHEMANAME.candidatelines where not done;
	--RAISE NOTICE 'rows left %', nrows;
	
	-- Fetch row from the table which is not done
		select lid, the_geom
		into lid_i, the_geom_i
		from SCHEMANAME.candidatelines where not done limit 1;

		-- If no such rows, algorithm is complete
		exit when not found;

		-- process startpoints
		startdone := TRUE;
		startpoint_i := ST_STARTPOINT(the_geom_i);
		SELECT count(distinct lid) FROM SCHEMANAME.candidatelines INTO startcount WHERE lid != lid_i AND ST_INTERSECTS(startpoint_i, the_geom);
		if (startcount = 1) then 
			SELECT lid, the_geom FROM SCHEMANAME.candidatelines INTO startmatch_lid, mergeline WHERE lid != lid_i AND ST_INTERSECTS(startpoint_i, the_geom);
			multiline_i := GEOMETRYTYPE(ST_LINEMERGE(ST_UNION(mergeline, the_geom_i))) = 'MULTILINESTRING';
			if (not multiline_i) then
				newline := ST_LINEMERGE(ST_UNION(mergeline, the_geom_i));
				UPDATE SCHEMANAME.candidatelines SET the_geom = newline WHERE lid = lid_i;
				the_geom_i := newline;
				DELETE FROM SCHEMANAME.candidatelines WHERE lid = startmatch_lid;
				startdone := FALSE;
			end if;
		end if;
		
		-- process endpoints 
		enddone := TRUE;
		endpoint_i := ST_ENDPOINT(the_geom_i);
		SELECT count(distinct lid) FROM SCHEMANAME.candidatelines INTO endcount WHERE lid != lid_i AND ST_INTERSECTS(endpoint_i, the_geom);
		if (endcount = 1) then 
			SELECT lid, the_geom FROM SCHEMANAME.candidatelines INTO endmatch_lid, mergeline WHERE lid != lid_i AND ST_INTERSECTS(endpoint_i, the_geom);
			multiline_i := GEOMETRYTYPE(ST_LINEMERGE(ST_UNION(mergeline, the_geom_i))) = 'MULTILINESTRING';
			if (not multiline_i) then
				newline := ST_LINEMERGE(ST_UNION(mergeline, the_geom_i));
				UPDATE SCHEMANAME.candidatelines SET the_geom = newline WHERE lid = lid_i;
				the_geom_i := newline;
				DELETE FROM SCHEMANAME.candidatelines WHERE lid = endmatch_lid;
				enddone := FALSE;
			end if;
		end if;

		-- set row to done if no more additional contiguous lines
		if (startdone and enddone) then
			update SCHEMANAME.candidatelines set done = True where lid = lid_i;
		end if;
	end loop;
end;
$$ language plpgsql; 

SELECT SCHEMANAME.my_union();

DROP TABLE IF EXISTS SCHEMANAME.outputlines;
CREATE TABLE SCHEMANAME.outputlines AS SELECT lid, the_geom FROM SCHEMANAME.candidatelines;


DROP TABLE IF EXISTS SCHEMANAME.inputlines;
DROP TABLE IF EXISTS SCHEMANAME.candidatelines;
DROP TABLE IF EXISTS SCHEMANAME.diaglines;
DROP TABLE IF EXISTS SCHEMANAME.edgelines;
DROP TABLE IF EXISTS SCHEMANAME.diagpoints;
DROP TABLE IF EXISTS SCHEMANAME.edgepoints;
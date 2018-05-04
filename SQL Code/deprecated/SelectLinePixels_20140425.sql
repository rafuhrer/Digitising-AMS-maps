/**********************************************************************************************
SEGMENTIZE LINES INTO SMALLER LINES OF EQUAL LENGTH, AND OVERLAP SEGMENTIZED LINES WITH 
GIVEN PIXELS; THEN REAGGREGATE TO ORIGINAL LINE LENGTH.

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	25/04/2014

Description:	Takes a table with lines (''inputlines'') and segmentizes said lines into 
		smaller lines of approximately equal length. Then overlaps buffered versions
		of these segments with a given table of points (or pixels, ''inputpoints'') 
		and calculates aggregate pixel statistics per line. E.g., we calculate the
		average number of pixels falling into the line buffer per line-length-unit.

Notes:		Line geometries in inputlines table must have unique ID named ''lid''
**********************************************************************************************/


-- Segmentize inputlines into small, straight lines (of variable length, depending on where coordinates are placed)
CREATE TEMP TABLE segmentlines ON COMMIT DROP AS
SELECT 
	il.lid,
	ST_Length(ST_MakeLine(il.sp, il.ep)) AS linelength,
	2 /ST_Length(ST_MakeLine(il.sp, il.ep)) AS segmentfrac,
	ST_MakeLine(il.sp, il.ep) AS the_geom
FROM (	SELECT 
		lid,
		st_pointn(the_geom, generate_series(1, st_npoints(the_geom)-1)) as sp,
		st_pointn(the_geom, generate_series(2, st_npoints(the_geom)  )) as ep
	FROM SCHEMANAME.inputlines ) il 
	WHERE ST_Length(ST_MakeLine(il.sp, il.ep)) > 0; -- Because some lines contain errors in the form of duplicate coordinates;
DROP SEQUENCE IF EXISTS SCHEMANAME.sid_seq;
CREATE SEQUENCE SCHEMANAME.sid_seq;
ALTER TABLE segmentlines ADD COLUMN sid INTEGER DEFAULT nextval('SCHEMANAME.sid_seq');



-- Calculate start and end fractions of segmentized lines with equal length
CREATE TEMP TABLE segmentfracs ON COMMIT DROP AS
SELECT
	sl.*,
	num,
	((sl.segmentfrac * num) - sl.segmentfrac) AS startfrac,
	CASE
		WHEN (sl.segmentfrac * num) > 1 THEN 1
		ELSE (sl.segmentfrac * num)
	END AS endfrac
FROM 	segmentlines sl,
	generate_series(1, 100) AS num
WHERE	num <= ceiling(1/sl.segmentfrac);


-- Generate new buffered line segments with unique id ''ssid''
DROP TABLE IF EXISTS SCHEMANAME.equallengthlines;
CREATE TABLE SCHEMANAME.equallengthlines AS
SELECT
	(sf.sid || '_' || sf.num) AS ssid,
	sf.sid,
	sf.lid,
	sf.num,
	ST_LENGTH(ST_Line_Substring(sf.the_geom, sf.startfrac, sf.endfrac))/PIXELSIZE AS ssid_length,
	ST_Line_Substring(sf.the_geom, sf.startfrac, sf.endfrac) AS the_geom,
	ST_BUFFER(ST_Line_Substring(sf.the_geom, sf.startfrac, sf.endfrac), PIXELSIZE*3) AS buffer
FROM	segmentfracs sf
WHERE	ST_LENGTH(ST_Line_Substring(sf.the_geom, sf.startfrac, sf.endfrac)) <> 0
ORDER BY	sf.sid,
		sf.num;
DROP INDEX IF EXISTS equallengthlines_gix_SCHEMANAME;
CREATE INDEX equallengthlines_gix_SCHEMANAME ON SCHEMANAME.equallengthlines USING GIST (buffer);

-- Create G-index over inputpoints table
DROP INDEX IF EXISTS inputpoints_gix_SCHEMANAME;
CREATE INDEX inputpoints_gix_SCHEMANAME ON SCHEMANAME.inputpoints USING GIST (the_geom);

-- Intersect buffered equal-length-lines with pixels from inputpoints table and group by ssid
CREATE TEMP TABLE ssidpixels ON COMMIT DROP AS
SELECT
	ell.ssid,
	COUNT(ip.the_geom) AS npixels
FROM	SCHEMANAME.equallengthlines ell
	LEFT JOIN SCHEMANAME.inputpoints ip
	ON ST_INTERSECTS(ell.buffer, ip.the_geom)
GROUP BY	ell.ssid;

-- Calculate aggregate pixel statistics per line in the inputlines table
SELECT
	ell.lid,
	ST_LENGTH(il.the_geom) AS length,
	2 AS sectionlength,
	COUNT(ssp.npixels) AS sectioncount,
	SUM(ssp.npixels) / (ST_LENGTH(il.the_geom)/PIXELSIZE) AS ppl,
	STDDEV_POP(ssp.npixels / ell.ssid_length) AS ppl_sd,
	MIN(ssp.npixels / ell.ssid_length) AS ppl_min,
	MAX(ssp.npixels / ell.ssid_length) AS ppl_max
FROM	SCHEMANAME.equallengthlines ell
	LEFT JOIN SCHEMANAME.inputlines il
	ON il.lid = ell.lid
		LEFT JOIN ssidpixels ssp
		ON ssp.ssid = ell.ssid
GROUP BY	ell.lid,
		il.the_geom;